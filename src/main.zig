const std = @import("std");
const cLibs = @cImport({
    @cInclude("stdlib.h");
    @cInclude("termios.h");
});

// Constants
const KILO_VERSION = "0.0.1";
const KILO_TAB_STOP = 8;
const STDOUT = std.io.getStdOut();
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
};
const TermError = error{
    get_win_size_failed,
};

// Structs
const WindowSize = struct { rows: usize, cols: usize };
const CursorPos = struct { x: usize, y: usize, rx: usize };
const Row = struct {
    row: std.ArrayList(u8),
    render: std.ArrayList(u8),

    const Self = @This();
    fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .row = std.ArrayList(u8).init(allocator),
            .render = std.ArrayList(u8).init(allocator),
        };
    }
};
const EditorConfig = struct {
    orig_termios: std.os.termios,
    win_size: WindowSize,
    c: CursorPos,
    rows: std.ArrayList(Row),
    row_offset: usize,
    col_offset: usize,

    const Self = @This();
    fn init(self: *Self, allocator: std.mem.Allocator) !void {
        self.c = CursorPos{ .x = 0, .y = 0, .rx = 0 };
        self.win_size = try getWindowSize();
        self.rows = @TypeOf(E.rows).init(allocator);
        self.row_offset = 0;
        self.col_offset = 0;
    }
};

const EditorKey = enum(u32) {
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    HOME_KEY,
    END_KEY,
    DEL_KEY,
    PAGE_UP,
    PAGE_DOWN,
};

// Globals
var E: EditorConfig = undefined;

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
    try E.init(allocator);
    var args = std.process.args();
    _ = args.skip();
    if (args.next()) |filename| {
        try editorOpen(allocator, filename);
    }

    while (true) {
        try editorRefreshScreen(allocator);
        if (!try editorProcessKeyPress()) break;
    }
}

// Terminal
fn enableRawMode() !void {
    E.orig_termios = try std.os.tcgetattr(std.io.getStdIn().handle);
    _ = cLibs.atexit(disableRawMode);
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
fn disableRawMode() callconv(.C) void {
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

fn getWindowSize() !WindowSize {
    var ws: std.os.linux.winsize = undefined;
    if (std.os.linux.ioctl(STDOUT.handle, std.os.linux.T.IOCGWINSZ, @intFromPtr(&ws)) != 0 or ws.ws_col == 0) {
        if (try STDOUT.write(escape.cursor_to_bot ++ escape.cursor_to_right) != 12) return TermError.get_win_size_failed;
        return getCursorPosition();
    }
    return WindowSize{ .rows = ws.ws_row, .cols = ws.ws_col };
}

fn getCursorPosition() !WindowSize {
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

// File IO

fn editorUpdateRow(row: *Row) !void {
    const tabs: usize = std.mem.count(u8, row.row.items, "\t");
    try row.render.ensureTotalCapacity(row.row.items.len + tabs * (KILO_TAB_STOP - 1));
    for (row.row.items) |c| {
        if (c == '\t') {
            row.render.appendSliceAssumeCapacity(" " ** KILO_TAB_STOP);
        } else {
            row.render.appendAssumeCapacity(c);
        }
    }
}

fn editorOpen(allocator: std.mem.Allocator, filename: []const u8) !void {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    var buf: [1024]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var row = Row.init(allocator);
        try row.row.appendSlice(line);
        try editorUpdateRow(&row);
        try E.rows.append(row);
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
            var len = E.rows.items[file_row].render.items.len;
            if (len > E.col_offset) len -= E.col_offset else len = 0;
            if (len > 0)
                try buf.appendSlice(E.rows.items[file_row].render.items[E.col_offset..(E.col_offset + @min(len, E.win_size.cols))]);
        }
        try buf.appendSlice(escape.clear_line);
        if (y < E.win_size.rows - 1)
            try buf.appendSlice("\r\n");
    }
}

// Input

fn editorProcessKeyPress() !bool {
    const c = try editorReadKey();

    switch (c) {
        ctrlKey('q') => return false,
        1000...1000 + std.enums.values(EditorKey).len => {
            const k: EditorKey = @enumFromInt(c);
            switch (k) {
                .ARROW_UP, .ARROW_LEFT, .ARROW_DOWN, .ARROW_RIGHT => editorMoveCursor(k),
                .PAGE_UP, .PAGE_DOWN => {
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
                .HOME_KEY => E.c.x = 0,
                .END_KEY => E.c.x = if (E.c.y < E.rows.items.len) E.rows.items[E.c.y].row.items.len else 0,
                .DEL_KEY => {},
            }
        },
        else => {},
    }
    return true;
}

fn editorRowCxToRx(row: *Row, cx: usize) usize {
    var rx: usize = 0;
    var j: usize = 0;
    while (j < cx) : (j += 1) {
        if (row.row.items[j] == '\t')
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        rx += 1;
    }
    return rx;
}

fn editorScroll() void {
    E.c.rx = 0;
    if (E.c.y < E.rows.items.len) {
        E.c.rx = editorRowCxToRx(&E.rows.items[E.c.y], E.c.x);
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
                E.c.x = E.rows.items[E.c.y].row.items.len;
            }
        },
        .ARROW_RIGHT => {
            if (row != null and E.c.x < row.?.row.items.len) {
                E.c.x += 1;
            } else if (row != null and E.c.x == row.?.row.items.len) {
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
    var rowlen: usize = if (row != null) row.?.row.items.len else 0;
    if (E.c.x > rowlen) {
        E.c.x = rowlen;
    }
}
