const std = @import("std");
const cLibs = @cImport({@cInclude("stdlib.h"); @cInclude("termios.h");});

pub fn main() !void {
    try enableRawMode();
    _ = cLibs.atexit(disableRawMode);

    var buf: [1]u8 = undefined;
    while (try std.io.getStdIn().read(buf[0..]) == 1 and buf[0] != 'q') {
        const c = buf[0];
        if (std.ascii.isControl(c)) {
            std.debug.print("{}\r\n", .{c});
        } else {
            std.debug.print("{} ('{c}')\r\n", .{ c, c });
        }
    }
}

var orig_termios: std.os.termios = undefined;
fn enableRawMode() !void {
    orig_termios = try std.os.tcgetattr(std.io.getStdIn().handle);
    var raw = orig_termios;

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
    std.os.tcsetattr(std.io.getStdIn().handle, std.os.linux.TCSA.FLUSH, orig_termios) catch std.debug.print("Could not reset terminal\r\n", .{});
}
