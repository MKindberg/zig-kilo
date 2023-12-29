const std = @import("std");
const cLibs = @cImport({
    @cInclude("stdlib.h");
    @cInclude("termios.h");
});

// Constants
const KILO_VERSION = "0.0.1";
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
const CursorPos = struct { x: usize, y: usize };
// const EditorRow = struct {
//     size: usize,
//     chars: *u8,
// };
const EditorConfig = struct {
    orig_termios: std.os.termios,
    win_size: WindowSize,
    c: CursorPos,
    num_rows: usize,
    row: std.ArrayList(u8),
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
    try initEditor();
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

fn initEditor() !void {
    E.c = CursorPos{ .x = 0, .y = 0 };
    E.win_size = try getWindowSize();
    E.num_rows = 0;
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

fn editorOpen(allocator: std.mem.Allocator, filename: []const u8) !void {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    var buf: [1024]u8 = undefined;
    var line = try file.reader().readUntilDelimiter(&buf, '\n');

    E.row = @TypeOf(E.row).init(allocator);
    try E.row.appendSlice(line);
    E.num_rows = 1;
}

// Output

fn editorRefreshScreen(allocator: std.mem.Allocator) !void {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    try buf.appendSlice(escape.hide_cursor);
    try buf.appendSlice(escape.cursor_to_top);
    try editorDrawRows(&buf);
    try buf.writer().print(escape.esc ++ "[{};{}H", .{ E.c.y + 1, E.c.x + 1 });
    try buf.appendSlice(escape.show_cursor);
    _ = try STDOUT.write(buf.items);
}
fn editorDrawRows(buf: *std.ArrayList(u8)) !void {
    for (0..E.win_size.rows) |y| {
        if (y >= E.num_rows) {
            if (E.num_rows == 0 and y == E.win_size.rows / 3) {
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
            try buf.appendSlice(E.row.items[0..@min(E.row.items.len, E.win_size.cols)]);
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
                EditorKey.PAGE_UP, EditorKey.PAGE_DOWN => {
                    var times = E.win_size.rows;
                    while (times > 0) : (times -= 1) {
                        editorMoveCursor(if (k == EditorKey.PAGE_UP) EditorKey.ARROW_UP else EditorKey.ARROW_DOWN);
                    }
                },
                .HOME_KEY => E.c.x = 0,
                .END_KEY => E.c.x = E.win_size.cols - 1,
                .DEL_KEY => {},
            }
        },
        else => {},
    }
    return true;
}

fn editorMoveCursor(key: EditorKey) void {
    switch (key) {
        .ARROW_UP => E.c.y -|= 1,
        .ARROW_LEFT => E.c.x -|= 1,
        .ARROW_DOWN => {
            if (E.c.y < E.win_size.rows) E.c.y += 1;
        },
        .ARROW_RIGHT => {
            if (E.c.x < E.win_size.cols) E.c.x += 1;
        },
        else => {},
    }
}
