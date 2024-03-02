const std = @import("std");

pub const Match = struct {
    start: usize,
    len: usize,
};

pub const Regex = struct {
    pub fn indexOf(haystack: []const u8, needle: []const u8) ?Match {
        for (0..haystack.len) |i| {
            const m = matches(haystack[i..], needle);
            const match = m[0];
            const len = m[1];

            if (match) {
                return Match{ .start = i, .len = len };
            }
        }
        return null;
    }

    fn matches(string: []const u8, pattern: []const u8) struct { bool, usize } {
        var i: usize = 0;
        var j: usize = 0;
        while (i < pattern.len and j < string.len) : ({
            i += 1;
            j += 1;
        }) {
            switch (pattern[i]) {
                '.' => continue,
                '\\' => {
                    i += 1;
                    if (i == pattern.len) return .{ false, 0 };
                    switch (pattern[i]) {
                        'c' => if (!std.ascii.isControl(string[j])) return .{ false, 0 },
                        'd' => if (!std.ascii.isDigit(string[j])) return .{ false, 0 },
                        'D' => if (std.ascii.isDigit(string[j])) return .{ false, 0 },
                        's' => if (!std.ascii.isWhitespace(string[j])) return .{ false, 0 },
                        'S' => if (std.ascii.isWhitespace(string[j])) return .{ false, 0 },
                        'w' => if (!std.ascii.isAlphanumeric(string[j])) return .{ false, 0 },
                        'W' => if (std.ascii.isAlphanumeric(string[j])) return .{ false, 0 },
                        'x' => if (!std.ascii.isHex(string[j])) return .{ false, 0 },
                        'O' => if (!(string[j] >= '0' and string[j] <= '7')) return .{ false, 0 },
                        '\\' => if (string[j] != '\\') return .{ false, 0 },
                        '.' => if (string[j] != '.') return .{ false, 0 },
                        else => return .{ false, 0 },
                    }
                },
                else => if (pattern[i] != string[j]) return .{ false, 0 },
            }
        }
        return .{ true, j };
    }
};

test "match literal" {
    try std.testing.expect(Regex.matches("world", "world")[0]);
}

test "match ." {
    try std.testing.expect(Regex.matches("world", "w.rld")[0]);
    try std.testing.expect(Regex.matches("world", ".o.ld")[0]);
    try std.testing.expect(Regex.matches("world", "wor..")[0]);
}

test "match character classes" {
    try std.testing.expect(Regex.matches("world", "w\\Drld")[0]);
    try std.testing.expect(!Regex.matches("world", "w\\drld")[0]);
    try std.testing.expect(Regex.matches("world", "w\\wrld")[0]);
}
