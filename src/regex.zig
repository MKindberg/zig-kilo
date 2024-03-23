const std = @import("std");

pub const Regex = struct {
    const Token = union(enum) {
        Literal: u8,
        Any: void,
        CharacterGroup: []const u8,
        NegCharacterGroup: []const u8,
        CharacterClass: u8,
        Start: void,
        End: void,
    };

    const RegexError = error{
        InvalidPattern,
    };

    pub fn parse(allocator: std.mem.Allocator, pattern: []const u8) RegexError!std.ArrayList(Token) {
        var tokens = std.ArrayList(Token).init(allocator);
        errdefer tokens.deinit();
        var i: usize = 0;
        while (i < pattern.len) : (i += 1) {
            switch (pattern[i]) {
                '\\' => {
                    i += 1;
                    if (i == pattern.len) return RegexError.InvalidPattern;
                    const character_classes = "cdDsSwWxO";
                    const escaped_characters = "\\^$.|?*+()[]{}";
                    if (std.mem.indexOfScalar(u8, character_classes, pattern[i]) != null) {
                        _ = tokens.append(Token{ .CharacterClass = pattern[i] }) catch unreachable;
                    } else if (std.mem.indexOfScalar(u8, escaped_characters, pattern[i]) != null) {
                        _ = tokens.append(Token{ .Literal = pattern[i] }) catch unreachable;
                    } else {
                        return RegexError.InvalidPattern;
                    }
                },
                '.' => _ = tokens.append(Token.Any) catch unreachable,
                '^' => _ = {
                    if (i == 0) tokens.append(Token.Start) catch unreachable else tokens.append(Token{ .Literal = pattern[i] }) catch unreachable;
                },
                '$' => _ = {
                    if (i == pattern.len - 1) tokens.append(Token.End) catch unreachable else tokens.append(Token{ .Literal = pattern[i] }) catch unreachable;
                },
                '[' => {
                    if (std.mem.indexOfScalar(u8, pattern[i..], ']')) |end| {
                        if (pattern[i + 1] == '^') {
                            _ = tokens.append(Token{ .NegCharacterGroup = pattern[i + 2 .. i + end] }) catch unreachable;
                        } else {
                            _ = tokens.append(Token{ .CharacterGroup = pattern[i + 1 .. i + end] }) catch unreachable;
                        }
                        i += end;
                    } else return RegexError.InvalidPattern;
                },
                else => _ = tokens.append(Token{ .Literal = pattern[i] }) catch unreachable,
            }
        }
        return tokens;
    }

    pub fn find(haystack: []const u8, pattern: []const u8) ?Match {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const allocator = gpa.allocator();

        const tokens = parse(allocator, pattern) catch return null;
        defer tokens.deinit();

        return matches(tokens.items, haystack);
    }

    pub fn matches(tokens: []const Token, string: []const u8) ?Match {
        if (tokens.len == 0) return null;
        if (tokens[0] == .Start) {
            if (matches_sub(tokens[1..], string, 0)) |l| {
                return Match{ .start = 0, .len = l };
            }
            return null;
        }
        for (0..string.len) |i| {
            const match_len = matches_sub(tokens, string[i..], 0);
            if (match_len != null) {
                return Match{ .start = i, .len = match_len.? };
            }
        }
        return null;
    }

    fn matches_sub(tokens: []const Token, string: []const u8, len: usize) ?usize {
        if (tokens.len == 0 or (tokens.len == 1 and tokens[0] == .End and string.len == 0)) {
            return len;
        } else if (string.len == 0) {
            return null;
        }
        switch (tokens[0]) {
            .Literal => |c| if (string[0] == c) return matches_sub(tokens[1..], string[1..], len + 1),
            .Any => return matches_sub(tokens[1..], string[1..], len + 1),
            .CharacterGroup => |group| {
                if (std.mem.indexOfScalar(u8, group, string[0]) != null) return matches_sub(tokens[1..], string[1..], len + 1);
            },
            .NegCharacterGroup => |group| {
                if (std.mem.indexOfScalar(u8, group, string[0]) == null) return matches_sub(tokens[1..], string[1..], len + 1);
            },
            .CharacterClass => |c| if (matches_class(c, string[0])) return matches_sub(tokens[1..], string[1..], len + 1),
            .End => return null,
            .Start => unreachable,
        }
        return null;
    }

    fn matches_class(class: u8, c: u8) bool {
        switch (class) {
            'c' => return std.ascii.isControl(c),
            'd' => return std.ascii.isDigit(c),
            'D' => return !std.ascii.isDigit(c),
            's' => return std.ascii.isWhitespace(c),
            'S' => return !std.ascii.isWhitespace(c),
            'w' => return std.ascii.isAlphanumeric(c),
            'W' => return !std.ascii.isAlphanumeric(c),
            'x' => return std.ascii.isHex(c),
            'O' => return c >= '0' and c <= '7',
            else => unreachable,
        }
        return false;
    }
};

pub const Match = struct {
    start: usize,
    len: usize,
};

test "match literal" {
    try std.testing.expect(Regex.find("world", "world") != null);
}

test "match ." {
    try std.testing.expect(Regex.find("world", "w.rld") != null);
    try std.testing.expect(Regex.find("world", ".o.ld") != null);
    try std.testing.expect(Regex.find("world", "wor..") != null);
}

test "match class" {
    try std.testing.expect(Regex.matches_class('s', ' ') == true);
}

test "match character classes" {
    try std.testing.expect(Regex.find("world", "w\\Drld") != null);
    try std.testing.expect(Regex.find("world", "w\\drld") == null);
    try std.testing.expect(Regex.find("world", "w\\wrld") != null);
    try std.testing.expect(Regex.find("hello world", "hello\\sworld") != null);
}

test "find match start end" {
    try std.testing.expect(Regex.find("world", "^wo").?.start == 0);
    try std.testing.expect(Regex.find("wororld", "^or") == null);
    try std.testing.expect(Regex.find("worldld", "ld$").?.start == 5);
    try std.testing.expect(Regex.find("world", "or$") == null);
}

test "character group" {
    try std.testing.expect(Regex.find("world", "[w]orld") != null);
    try std.testing.expect(Regex.find("world", "w[fewoihw]rld") != null);
    try std.testing.expect(Regex.find("world", "worl[fewdoihw]") != null);
    try std.testing.expect(Regex.find("world", "[^feoih]orld") != null);
}
