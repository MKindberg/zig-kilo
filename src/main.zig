const std = @import("std");

// Constants
const KILO_VERSION = "0.0.1";
const KILO_TAB_STOP = 8;
const KILO_QUIT_TIMES = 3;
const STDOUT = std.io.getStdOut();

const HL_HIGHLIGHT_NUMBERS = (1 << 0);
const HL_HIGHLIGHT_STRINGS = (1 << 1);

const escape = struct {
    const esc = "\x1b";
    const clear_screen = esc ++ "[2J";
    const clear_line = esc ++ "[K";
    const cursor_to_top = esc ++ "[H";
    const cursor_to_right = esc ++ "[999C";
    const cursor_to_bot = esc ++ "[999B";
    const get_cursor_pos = esc ++ "[6n";
    const hide_cursor = esc ++ "[?25l";
    const show_cursor = esc ++ "[?25h";
    const invert_colors = esc ++ "[7m";
    const normal_text = esc ++ "[m";
    const color = struct {
        const red = esc ++ "[31m";
        const green = esc ++ "[32m";
        const yellow = esc ++ "[33m";
        const blue = esc ++ "[34m";
        const magenta = esc ++ "[35m";
        const cyan = esc ++ "[36m";
        const reset = esc ++ "[39m";
    };
};
const TermError = error{
    get_win_size_failed,
};

// Structs
const WindowSize = struct {
    rows: usize,
    cols: usize,

    fn init() !WindowSize {
        var ws: std.os.linux.winsize = undefined;
        if (std.os.linux.ioctl(STDOUT.handle, std.os.linux.T.IOCGWINSZ, @intFromPtr(&ws)) != 0 or ws.ws_col == 0) {
            return get_window_size_backup();
        }
        return WindowSize{ .rows = ws.ws_row, .cols = ws.ws_col };
    }

    fn get_window_size_backup() !WindowSize {
        if (try STDOUT.write(escape.cursor_to_bot ++ escape.cursor_to_right) != 12) return TermError.get_win_size_failed;
        if (try STDOUT.write(escape.get_cursor_pos) != 4) return TermError.get_win_size_failed;
        try STDOUT.writer().print("\r\n", .{});

        var i: usize = 0;
        var buf: [32]u8 = undefined;
        while (i < buf.len) : (i += 1) {
            if (try std.io.getStdIn().read(buf[i .. i + 1]) != 1) break;
            if (buf[i] == 'R') break;
        }

        if (buf[0] != escape.esc[0] or buf[1] != '[') return TermError.get_win_size_failed;
        if (std.mem.indexOf(u8, buf[2..i], ";")) |delimiter| {
            const d = 2 + delimiter;
            return WindowSize{ .rows = try std.fmt.parseInt(usize, buf[2..d], 10), .cols = try std.fmt.parseInt(usize, buf[(d + 1)..i], 10) };
        }

        return TermError.get_win_size_failed;
    }
};
const CursorPos = struct { x: usize, y: usize, rx: usize };
const EditorSyntax = struct {
    filetype: []const u8,
    filematch: []const []const u8,
    keywords: []const []const u8,
    singleline_comment_start: ?[]const u8 = null,
    multiline_comment_start: ?[]const u8 = null,
    multiline_comment_end: ?[]const u8 = null,
    flags: u32,
};
const EditorHighlight = enum(u8) {
    NORMAL,
    KEYWORD1,
    KEYWORD2,
    COMMENT,
    ML_COMMENT,
    STRING,
    NUMBER,
    MATCH,

    fn toColor(self: EditorHighlight) []const u8 {
        switch (self) {
            .NORMAL => return escape.color.reset,
            .KEYWORD1 => return escape.color.yellow,
            .KEYWORD2 => return escape.color.green,
            .COMMENT, .ML_COMMENT => return escape.color.cyan,
            .STRING => return escape.color.magenta,
            .NUMBER => return escape.color.red,
            .MATCH => return escape.color.blue,
        }
    }
};
const Row = struct {
    idx: usize,
    chars: std.ArrayList(u8),
    render: std.ArrayList(u8),
    hl: std.ArrayList(EditorHighlight),
    hl_open_comment: bool = false,

    const Self = @This();
    fn init(allocator: std.mem.Allocator, idx: usize) Self {
        return Self{
            .idx = idx,
            .chars = std.ArrayList(u8).init(allocator),
            .render = std.ArrayList(u8).init(allocator),
            .hl = std.ArrayList(EditorHighlight).init(allocator),
        };
    }
    fn deinit(self: *Self) void {
        self.chars.deinit();
        self.render.deinit();
        self.hl.deinit();
    }

    fn update(row: *Row) !void {
        const tabs: usize = std.mem.count(u8, row.chars.items, "\t");
        try row.render.ensureTotalCapacity(row.chars.items.len + tabs * (KILO_TAB_STOP - 1));
        row.render.clearRetainingCapacity();
        for (row.chars.items) |c| {
            if (c == '\t') {
                row.render.appendSliceAssumeCapacity(" " ** KILO_TAB_STOP);
            } else {
                row.render.appendAssumeCapacity(c);
            }
        }
        try editorUpdateSyntax(row);
    }

    fn cxToRx(row: Row, cx: usize) usize {
        var rx: usize = 0;
        var j: usize = 0;
        while (j < cx) : (j += 1) {
            if (row.chars.items[j] == '\t')
                rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
            rx += 1;
        }
        return rx;
    }

    fn rxToCx(row: Row, rx: usize) usize {
        var cur_rx: usize = 0;
        for (row.chars.items, 0..) |c, cx| {
            if (c == '\t') cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
            cur_rx += 1;
            if (cur_rx > rx) return cx;
        }
        return row.chars.items.len;
    }
};

const StatusMessage = struct {
    msg: []u8,
    buf: [80]u8,
    time: i64,

    const Self = @This();
    fn set(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        self.msg = try std.fmt.bufPrint(&self.buf, fmt, args);
        self.time = std.time.milliTimestamp();
    }

    fn draw(self: Self, buf: *std.ArrayList(u8)) !void {
        try buf.appendSlice(escape.clear_line);
        const len = @min(self.msg.len, E.win_size.cols);
        if (len > 0 and self.time > std.time.milliTimestamp() - 5000) {
            try buf.appendSlice(self.msg[0..len]);
        }
    }
};

const EditorConfig = struct {
    orig_termios: std.os.termios,
    win_size: WindowSize,
    c: CursorPos,
    rows: std.ArrayList(Row),
    row_offset: usize,
    col_offset: usize,
    filename: std.ArrayList(u8),
    statusmsg: StatusMessage,
    dirty: bool,
    syntax: ?EditorSyntax,

    const Self = @This();
    fn init(self: *Self, allocator: std.mem.Allocator) !void {
        self.c = CursorPos{ .x = 0, .y = 0, .rx = 0 };
        self.win_size = try WindowSize.init();
        self.rows = @TypeOf(E.rows).init(allocator);
        self.row_offset = 0;
        self.col_offset = 0;
        self.filename = @TypeOf(E.filename).init(allocator);
        self.statusmsg.msg = self.statusmsg.buf[0..0];
        self.statusmsg.time = 0;
        self.dirty = false;
        self.syntax = null;

        self.win_size.rows -= 2;
    }
};

const EditorKey = enum(u32) {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    HOME_KEY,
    END_KEY,
    DEL_KEY,
    PAGE_UP,
    PAGE_DOWN,

    fn int(comptime self: EditorKey) u32 {
        return @intFromEnum(self);
    }
};

// Globals

var E: EditorConfig = undefined;

// Constants

const HLDB = [_]EditorSyntax{ .{
    .filetype = "c",
    .filematch = &[_][]const u8{ ".c", ".h", ".cpp" },
    .keywords = &[_][]const u8{
        "switch", "if",    "while",   "for",    "break", "continue",  "return",  "else",
        "struct", "union", "typedef", "static", "enum",  "class",     "case",    "const",
        "int|",   "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|",
    },
    .singleline_comment_start = "//",
    .multiline_comment_start = "/*",
    .multiline_comment_end = "*/",
    .flags = HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
}, .{
    .filetype = "zig",
    .filematch = &[_][]const u8{".zig"},
    .keywords = &[_][]const u8{
        "switch", "if",    "while",   "for",  "break", "continue", "return", "else",
        "struct", "union", "typedef", "enum", "case",  "const",    "i8|",    "i16|",
        "i32|",   "i64|",  "isize|",  "u8|",  "u16|",  "u32|",     "u64|",   "usize|",
        "f32|",   "f64|",  "void|",
    },
    .singleline_comment_start = "//",
    .flags = HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
} };

// Main

fn ctrlKey(c: u8) u8 {
    return c & 0x1f;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();
    defer {
        _ = STDOUT.write(escape.clear_screen) catch {};
        _ = STDOUT.write(escape.cursor_to_top) catch {};
    }
    try enableRawMode();
    defer disableRawMode();
    try E.init(allocator);
    var args = std.process.args();
    _ = args.skip();
    if (args.next()) |filename| {
        try editorOpen(allocator, filename);
    }

    try E.statusmsg.set("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find", .{});

    while (true) {
        try editorRefreshScreen(allocator);
        if (!try editorProcessKeyPress(allocator)) break;
    }
}

// Terminal
fn enableRawMode() !void {
    E.orig_termios = try std.os.tcgetattr(std.io.getStdIn().handle);
    var raw = E.orig_termios;

    const f = std.os.linux;
    raw.iflag &= ~(f.BRKINT | f.ICRNL | f.INPCK | f.ISTRIP | f.IXON);
    raw.oflag &= ~(f.OPOST);
    raw.cflag |= (f.CS8);
    raw.lflag &= ~(f.ECHO | f.ICANON | f.IEXTEN | f.ISIG);
    raw.cc[f.V.MIN] = 0;
    raw.cc[f.V.TIME] = 1;

    try std.os.tcsetattr(std.io.getStdIn().handle, std.os.linux.TCSA.FLUSH, raw);
}
fn disableRawMode() void {
    std.os.tcsetattr(std.io.getStdIn().handle, std.os.linux.TCSA.FLUSH, E.orig_termios) catch std.debug.print("Could not reset terminal\r\n", .{});
}

fn editorReadKey() !u32 {
    var buf: [1]u8 = undefined;
    while (try std.io.getStdIn().read(buf[0..]) != 1) {}
    if (buf[0] == escape.esc[0]) {
        var seq: [3]u8 = undefined;
        if (try std.io.getStdIn().read(seq[0..2]) != 2) return buf[0];

        if (seq[0] == '[') {
            if (seq[1] >= '0' and seq[1] <= '9') {
                if (try std.io.getStdIn().read(seq[2..3]) != 1) return buf[0];
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        '1' => return @intFromEnum(EditorKey.HOME_KEY),
                        '3' => return @intFromEnum(EditorKey.DEL_KEY),
                        '4' => return @intFromEnum(EditorKey.END_KEY),
                        '5' => return @intFromEnum(EditorKey.PAGE_UP),
                        '6' => return @intFromEnum(EditorKey.PAGE_DOWN),
                        '7' => return @intFromEnum(EditorKey.HOME_KEY),
                        '8' => return @intFromEnum(EditorKey.END_KEY),
                        else => {},
                    }
                }
            }
            switch (seq[1]) {
                'A' => return @intFromEnum(EditorKey.ARROW_UP),
                'B' => return @intFromEnum(EditorKey.ARROW_DOWN),
                'C' => return @intFromEnum(EditorKey.ARROW_RIGHT),
                'D' => return @intFromEnum(EditorKey.ARROW_LEFT),
                'H' => return @intFromEnum(EditorKey.HOME_KEY),
                'F' => return @intFromEnum(EditorKey.END_KEY),
                else => {},
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                'H' => return @intFromEnum(EditorKey.HOME_KEY),
                'F' => return @intFromEnum(EditorKey.END_KEY),
                else => {},
            }
        }
    }
    return buf[0];
}

// Syntax highlighting

fn isSeparator(c: u8) bool {
    return std.ascii.isWhitespace(c) or std.mem.indexOfScalar(u8, ",.()+-/*=~%<>[];", c) != null;
}

fn editorUpdateSyntax(row: *Row) !void {
    row.hl.clearRetainingCapacity();
    try row.hl.appendNTimes(EditorHighlight.NORMAL, row.render.items.len);

    const syntax = E.syntax orelse return;

    const keyowrds = syntax.keywords;

    const scs = syntax.singleline_comment_start;
    const mcs = syntax.multiline_comment_start;
    const mce = syntax.multiline_comment_start;

    var prev_sep = true;
    var in_string: u8 = 0;
    var in_comment = (row.idx > 0 and E.rows.items[row.idx - 1].hl_open_comment);
    var i: usize = 0;
    outer: while (i < row.render.items.len) : (i += 1) {
        var prev_hl = if (i > 0) row.hl.items[i - 1] else EditorHighlight.NORMAL;
        const c = row.render.items[i];
        if (scs != null and in_string == 0 and !in_comment) {
            if (std.mem.startsWith(u8, row.render.items[i..], scs.?)) {
                for (i..row.render.items.len) |j| {
                    row.hl.items[j] = .COMMENT;
                }
                break;
            }
        }
        if (mcs != null and mce != null and in_string == 0) {
            if (in_comment) {
                row.hl.items[i] = .ML_COMMENT;
                if (std.mem.startsWith(u8, row.render.items[i..], mce.?)) {
                    for (i..i + mce.?.len) |j| {
                        row.hl.items[j] = .ML_COMMENT;
                    }
                    i += mce.?.len - 1;
                    in_comment = false;
                    prev_sep = true;
                }
                continue;
            } else if (std.mem.startsWith(u8, row.render.items[i..], mcs.?)) {
                for (i..i + mcs.?.len) |j| {
                    row.hl.items[j] = .ML_COMMENT;
                }
                i += mcs.?.len - 1;
                in_comment = true;
                continue;
            }
        }
        if (syntax.flags & HL_HIGHLIGHT_STRINGS != 0) {
            if (in_string != 0) {
                row.hl.items[i] = .STRING;
                if (c == '\\' and i + 1 < row.render.items.len) {
                    row.hl.items[i + 1] = .STRING;
                    i += 1;
                    continue;
                }
                if (c == in_string) in_string = 0;
                prev_sep = true;
                continue;
            } else {
                if (c == '"' or c == '\'') {
                    in_string = c;
                    row.hl.items[i] = .STRING;
                    continue;
                }
            }
        }
        if (syntax.flags & HL_HIGHLIGHT_NUMBERS != 0) {
            if (std.ascii.isDigit(c) and (prev_sep or prev_hl == EditorHighlight.NUMBER) or
                (c == '.' and prev_hl == EditorHighlight.NUMBER))
            {
                row.hl.items[i] = .NUMBER;
                prev_sep = false;
                continue;
            }
        }
        if (prev_sep) {
            for (keyowrds) |kw| {
                const len = if (kw[kw.len - 1] == '|') kw.len - 1 else kw.len;
                if (std.mem.startsWith(u8, row.render.items[i..], kw[0..len]) and
                    (i + len < row.render.items.len and isSeparator(row.render.items[i + len])))
                {
                    for (i..i + len) |j| {
                        row.hl.items[j] = if (kw.len == len) .KEYWORD1 else .KEYWORD2;
                    }
                    i += len - 1;
                    prev_sep = false;
                    continue :outer;
                }
            }
        }
        prev_sep = isSeparator(c);
    }
    const changed = (row.hl_open_comment != in_comment);
    row.hl_open_comment = in_comment;
    if (changed and row.idx + 1 < E.rows.items.len)
        try editorUpdateSyntax(&E.rows.items[row.idx + 1]);
}

fn editorSelectSyntaxHighlight() void {
    E.syntax = null;
    if (E.filename.items.len == 0) return;

    const ext = if (std.mem.lastIndexOfScalar(u8, E.filename.items, '.')) |i|
        E.filename.items[i..]
    else
        "";

    for (HLDB) |entry| {
        for (entry.filematch) |pattern| {
            const is_ext = pattern[0] == '.';
            if (is_ext and ext.len > 0 and std.mem.eql(u8, ext, pattern) or
                !is_ext and std.mem.eql(u8, E.filename.items, pattern))
            {
                E.syntax = entry;
                return;
            }
        }
    }
}

// Editor operations

fn editorInsertChar(allocator: std.mem.Allocator, c: u8) !void {
    if (E.c.y == E.rows.items.len) {
        try E.rows.append(Row.init(allocator, E.c.y));
    }
    try E.rows.items[E.c.y].chars.insert(E.c.x, c);
    try E.rows.items[E.c.y].update();
    E.c.x += 1;
    E.dirty = true;
}

fn editorInsertNewline(allocator: std.mem.Allocator) !void {
    if (E.c.x == 0) {
        try E.rows.insert(E.c.y, Row.init(allocator, E.c.y));
    } else {
        var row = Row.init(allocator, E.c.y + 1);
        try row.chars.appendSlice(E.rows.items[E.c.y].chars.items[E.c.x..]);
        try E.rows.insert(E.c.y + 1, row);
        try E.rows.items[E.c.y + 1].update();
        try E.rows.items[E.c.y].chars.resize(E.c.x);
        try E.rows.items[E.c.y].update();
    }
    for (E.c.y + 1..E.rows.items.len) |i| {
        E.rows.items[i].idx += i;
    }
    E.c.y += 1;
    E.c.x = 0;
    E.dirty = true;
}

fn editorDelChar() !void {
    if (E.c.y == E.rows.items.len) return;

    if (E.c.x > 0) {
        _ = E.rows.items[E.c.y].chars.orderedRemove(E.c.x - 1);
        E.dirty = true;
        E.c.x -= 1;
        try E.rows.items[E.c.y].update();
    } else if (E.c.y > 0) {
        try E.rows.items[E.c.y - 1].chars.appendSlice(E.rows.items[E.c.y].chars.items);
        E.rows.items[E.c.y].deinit();
        _ = E.rows.orderedRemove(E.c.y);
        for (E.c.y..E.rows.items.len) |i| {
            E.rows.items[i].idx = i;
        }
        E.dirty = true;
        E.c.y -= 1;
        try E.rows.items[E.c.y].update();
    }
}

// File IO

fn editorOpen(allocator: std.mem.Allocator, filename: []const u8) !void {
    E.filename.clearRetainingCapacity();
    try E.filename.appendSlice(filename);
    editorSelectSyntaxHighlight();
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    var buf: [1024]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var row = Row.init(allocator, E.rows.items.len);
        try row.chars.appendSlice(line);
        try row.update();
        try E.rows.append(row);
    }
}

fn editorSave(allocator: std.mem.Allocator) !void {
    if (E.filename.items.len == 0) {
        E.filename.deinit();
        E.filename = try editorPrompt(allocator, "Save as: {s} (ESC to cancel)", null);
        if (E.filename.items.len == 0) {
            try E.statusmsg.set("Save aborted", .{});
            return;
        }
    }
    editorSelectSyntaxHighlight();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    for (E.rows.items) |row| {
        try buf.writer().print("{s}\n", .{row.chars.items});
    }

    var file = std.fs.cwd().createFile(E.filename.items, .{ .read = true, .truncate = true, .mode = 0o664 }) catch |e| {
        try E.statusmsg.set("Failed to open file: {}", .{e});
        return;
    };
    defer file.close();

    file.writer().print("{s}", .{buf.items}) catch |e| {
        try E.statusmsg.set("Failed to write to file: {}", .{e});
        return;
    };
    E.dirty = false;
    try E.statusmsg.set("{} bytes written to disk", .{buf.items.len});
}

// Find

fn editorFindCallback(query: []u8, key: u32) !void {
    const Static = struct {
        var last_match: i32 = -1;
        var direction: i32 = 1;
        var saved_hl_line: usize = undefined;
        var saved_hl: std.ArrayList(EditorHighlight) = undefined;
    };

    if (Static.saved_hl.items.len > 0) {
        std.mem.swap(std.ArrayList(EditorHighlight), &E.rows.items[Static.saved_hl_line].hl, &Static.saved_hl);
        Static.saved_hl.clearAndFree();
    }
    if (key == '\r' or key == escape.esc[0]) {
        Static.last_match = -1;
        Static.direction = 1;
        E.statusmsg.set("", .{}) catch {};
        return;
    } else if (key == asInt(.ARROW_RIGHT) or key == asInt(.ARROW_DOWN)) {
        Static.direction = 1;
    } else if (key == asInt(.ARROW_LEFT) or key == asInt(.ARROW_UP)) {
        Static.direction = -1;
    } else {
        Static.last_match = -1;
        Static.direction = 1;
    }

    if (Static.last_match == -1) Static.direction = 1;
    var current = Static.last_match;

    for (0..E.rows.items.len) |_| {
        current += Static.direction;
        if (current == -1) current = @intCast(E.rows.items.len - 1) else if (current == E.rows.items.len) current = 0;

        var row = E.rows.items[@as(usize, @intCast(current))];
        if (std.mem.indexOf(u8, row.render.items, query)) |pos| {
            Static.last_match = current;
            E.c.y = @intCast(current);
            E.c.x = row.rxToCx(pos);
            E.row_offset = E.rows.items.len;

            Static.saved_hl_line = @intCast(current);
            Static.saved_hl = try row.hl.clone();
            for (pos..pos + query.len) |i| {
                row.hl.items[i] = .MATCH;
            }

            break;
        }
    }
}

fn editorFind(allocator: std.mem.Allocator) !void {
    const saved_cx = E.c.x;
    const saved_cy = E.c.y;
    const saved_coloff = E.col_offset;
    const saved_rowoff = E.row_offset;
    const query = try editorPrompt(allocator, "Search: {s} (Use ESC/Arrows/Enter)", editorFindCallback);
    defer query.deinit();

    if (query.items.len == 0) {
        E.c.x = saved_cx;
        E.c.y = saved_cy;
        E.col_offset = saved_coloff;
        E.row_offset = saved_rowoff;
    }
}

// Output

fn editorRefreshScreen(allocator: std.mem.Allocator) !void {
    editorScroll();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    try buf.appendSlice(escape.hide_cursor);
    try buf.appendSlice(escape.cursor_to_top);
    try editorDrawRows(&buf);
    try editorDrawStatusBar(&buf);
    try E.statusmsg.draw(&buf);
    try buf.writer().print(escape.esc ++ "[{};{}H", .{ (E.c.y - E.row_offset) + 1, (E.c.rx - E.col_offset) + 1 });
    try buf.appendSlice(escape.show_cursor);
    _ = try STDOUT.write(buf.items);
}

fn editorDrawRows(buf: *std.ArrayList(u8)) !void {
    for (0..E.win_size.rows) |y| {
        var file_row = y + E.row_offset;
        if (file_row >= E.rows.items.len) {
            if (E.rows.items.len == 0 and y == E.win_size.rows / 3) {
                const welcome = "Kilo editor -- version " ++ KILO_VERSION;
                var padding = (E.win_size.cols - welcome.len) / 2;
                if (padding > 0) {
                    try buf.append('~');
                    padding -= 1;
                }
                try buf.appendNTimes(' ', padding);
                try buf.appendSlice(welcome[0..@min(welcome.len, E.win_size.cols)]);
            } else {
                try buf.append('~');
            }
        } else {
            const hl = E.rows.items[file_row].hl.items;
            var len = E.rows.items[file_row].render.items.len;
            if (len > E.col_offset) len -= E.col_offset else len = 0;
            var current_color = EditorHighlight.NORMAL;
            for (E.col_offset..(E.col_offset + @min(len, E.win_size.cols))) |i| {
                const c = E.rows.items[file_row].render.items[i];
                if (std.ascii.isControl(c)) {
                    const sym = if (c <= 26) '@' + c else '?';
                    try buf.appendSlice(escape.invert_colors);
                    try buf.append(sym);
                    try buf.appendSlice(escape.normal_text);
                    if (current_color != .NORMAL) {
                        try buf.appendSlice(current_color.toColor());
                    }
                }
                if (hl[i] == .NORMAL and current_color != .NORMAL) {
                    try buf.appendSlice(escape.color.reset);
                    current_color = .NORMAL;
                    try buf.append(c);
                } else {
                    if (hl[i] != current_color) {
                        try buf.appendSlice(hl[i].toColor());
                        current_color = hl[i];
                    }
                    try buf.append(c);
                }
            }
            try buf.appendSlice(escape.color.reset);
        }
        try buf.appendSlice(escape.clear_line);
        try buf.appendSlice("\r\n");
    }
}

fn editorDrawStatusBar(buf: *std.ArrayList(u8)) !void {
    try buf.appendSlice(escape.invert_colors);

    var status: [80]u8 = undefined;
    var rstatus: [80]u8 = undefined;
    const f = if (E.filename.items.len == 0) "[No Name]" else E.filename.items;
    const dirty = if (E.dirty) "(modified)" else "";
    const len = (try std.fmt.bufPrint(&status, "{s} - {} lines {s}", .{ f[0..@min(20, f.len)], E.rows.items.len, dirty })).len;
    const rlen = (try std.fmt.bufPrint(&rstatus, "{s} | {}/{}", .{ if (E.syntax) |s| s.filetype else "Unknown ft", E.c.y + 1, E.rows.items.len })).len;

    try buf.appendSlice(status[0..@min(len, E.win_size.cols)]);
    if (E.win_size.cols > len + rlen) {
        try buf.appendNTimes(' ', E.win_size.cols - len - rlen);
        try buf.appendSlice(rstatus[0..rlen]);
    } else if (E.win_size.cols > len) {
        try buf.appendNTimes(' ', E.win_size.cols - len);
    }

    try buf.appendSlice(escape.normal_text);
    try buf.appendSlice("\r\n");
}

// Input
fn asInt(comptime k: EditorKey) u32 {
    return @intFromEnum(k);
}
fn editorPrompt(allocator: std.mem.Allocator, comptime prompt: []const u8, comptime callback: ?fn ([]u8, u32) std.mem.Allocator.Error!void) !std.ArrayList(u8) {
    var buf = std.ArrayList(u8).init(allocator);
    errdefer buf.deinit();

    var c: u32 = undefined;
    defer if (callback) |func| func(buf.items, c) catch {};

    while (true) {
        try E.statusmsg.set(prompt, .{buf.items});
        try editorRefreshScreen(allocator);

        c = try editorReadKey();

        if (c == asInt(.DEL_KEY) or c == asInt(.BACKSPACE) or c == ctrlKey('h')) {
            _ = buf.popOrNull();
        } else if (c == escape.esc[0]) {
            buf.clearAndFree();
            return buf;
        } else if (c == '\r') {
            if (buf.items.len != 0) {
                try E.statusmsg.set("", .{});
                return buf;
            }
        } else if (c < 128 and !std.ascii.isControl(@intCast(c))) {
            try buf.append(@intCast(c));
        }
        if (callback) |func| try func(buf.items, c);
    }
}

fn editorProcessKeyPress(allocator: std.mem.Allocator) !bool {
    const Static = struct {
        var quit_times: usize = KILO_QUIT_TIMES;
    };
    const c = try editorReadKey();

    switch (c) {
        '\r' => {
            try editorInsertNewline(allocator);
        },
        ctrlKey('q') => {
            if (E.dirty) {
                if (Static.quit_times > 0) {
                    try E.statusmsg.set("WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.", .{Static.quit_times});
                    Static.quit_times -= 1;
                    return true;
                }
            }
            return false;
        },
        ctrlKey('s') => try editorSave(allocator),
        ctrlKey('f') => try editorFind(allocator),
        asInt(.DEL_KEY), asInt(.BACKSPACE), ctrlKey('h') => {
            if (c == asInt(.DEL_KEY)) {
                editorMoveCursor(.ARROW_RIGHT);
            }
            try editorDelChar();
        },
        ctrlKey('l'), escape.esc[0] => {},
        asInt(.ARROW_UP), asInt(.ARROW_LEFT), asInt(.ARROW_DOWN), asInt(.ARROW_RIGHT) => editorMoveCursor(@enumFromInt(c)),
        asInt(.PAGE_UP), asInt(.PAGE_DOWN) => {
            const k: EditorKey = @enumFromInt(c);
            if (k == .PAGE_UP) {
                E.c.y = E.row_offset;
            } else {
                E.c.y = E.row_offset + E.win_size.rows - 1;
                if (E.c.y > E.rows.items.len) E.c.y = E.rows.items.len;
            }
            var times = E.win_size.rows;
            while (times > 0) : (times -= 1) {
                editorMoveCursor(if (k == EditorKey.PAGE_UP) EditorKey.ARROW_UP else EditorKey.ARROW_DOWN);
            }
        },
        asInt(.HOME_KEY) => E.c.x = 0,
        asInt(.END_KEY) => E.c.x = if (E.c.y < E.rows.items.len) E.rows.items[E.c.y].chars.items.len else 0,
        else => {
            try editorInsertChar(allocator, @intCast(c));
        },
    }
    Static.quit_times = KILO_QUIT_TIMES;
    return true;
}

fn editorScroll() void {
    E.c.rx = 0;
    if (E.c.y < E.rows.items.len) {
        E.c.rx = E.rows.items[E.c.y].cxToRx(E.c.x);
    }
    if (E.c.y < E.row_offset) {
        E.row_offset = E.c.y;
    }
    if (E.c.y >= E.row_offset + E.win_size.rows) {
        E.row_offset = E.c.y - E.win_size.rows + 1;
    }
    if (E.c.rx < E.col_offset) {
        E.col_offset = E.c.rx;
    }
    if (E.c.rx >= E.col_offset + E.win_size.cols) {
        E.col_offset = E.c.rx - E.win_size.cols + 1;
    }
}

fn editorMoveCursor(key: EditorKey) void {
    var row: ?Row = if (E.c.y >= E.rows.items.len) null else E.rows.items[E.c.y];
    switch (key) {
        .ARROW_LEFT => {
            if (E.c.x != 0) {
                E.c.x -= 1;
            } else if (E.c.y > 0) {
                E.c.y -= 1;
                E.c.x = E.rows.items[E.c.y].chars.items.len;
            }
        },
        .ARROW_RIGHT => {
            if (row != null and E.c.x < row.?.chars.items.len) {
                E.c.x += 1;
            } else if (row != null and E.c.x == row.?.chars.items.len) {
                E.c.y += 1;
                E.c.x = 0;
            }
        },
        .ARROW_UP => {
            if (E.c.y != 0) {
                E.c.y -= 1;
            }
        },
        .ARROW_DOWN => {
            if (E.c.y < E.rows.items.len) {
                E.c.y += 1;
            }
        },
        else => {},
    }
    row = if (E.c.y >= E.rows.items.len) null else E.rows.items[E.c.y];
    var rowlen: usize = if (row != null) row.?.chars.items.len else 0;
    if (E.c.x > rowlen) {
        E.c.x = rowlen;
    }
}
