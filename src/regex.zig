const std = @import("std");

pub const Regex = struct {
    const Token = union(enum) {
        Literal: u8,
        Any: void,
        CharacterGroup: []const u8,
        NegCharacterGroup: []const u8,
        CharacterClass: u8,
        Quantifier: QuantifierData,
        Start: void,
        End: void,
    };

    const QuantifierData = struct {
        min: usize,
        max: usize,
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
                '*', '?', '+' => {
                    if (tokens.items.len == 0) return RegexError.InvalidPattern;
                    const last = tokens.getLast();
                    if (last == .Start or last == .End or last == .Quantifier) return RegexError.InvalidPattern;
                    const quantifier: QuantifierData = switch (pattern[i]) {
                        '*' => .{ .min = 0, .max = std.math.maxInt(usize) },
                        '?' => .{ .min = 0, .max = 1 },
                        '+' => .{ .min = 1, .max = std.math.maxInt(usize) },
                        else => unreachable,
                    };
                    tokens.insert(tokens.items.len - 1, Token{ .Quantifier = quantifier }) catch unreachable;
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
        if (tokens[0] == .Quantifier) {
            const q = tokens[0].Quantifier;
            const min = q.min;
            const max: usize = for (0..@min(string.len, q.max)) |i| {
                if (!eq(tokens[1], string[i])) {
                    break @intCast(i);
                }
            } else blk: {
                break :blk @min(string.len, q.max);
            };

            if (min > max) return null;
            var capture = max;
            while (capture > min) : (capture -= 1) {
                if (matches_sub(tokens[2..], string[capture..], len + capture)) |res| {
                    return res;
                }
            }
            return matches_sub(tokens[2..], string[min..], len + min);
        } else if (eq(tokens[0], string[0])) {
            return matches_sub(tokens[1..], string[1..], len + 1);
        }
        return null;
    }

    fn eq(token: Token, c: u8) bool {
        switch (token) {
            .Literal => return c == token.Literal,
            .Any => return true,
            .CharacterGroup => return std.mem.indexOfScalar(u8, token.CharacterGroup, c) != null,
            .NegCharacterGroup => return std.mem.indexOfScalar(u8, token.NegCharacterGroup, c) == null,
            .CharacterClass => return matches_class(token.CharacterClass, c),
            .Start => return false,
            .End => return false,
            .Quantifier => return false,
        }
        return false;
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

test "star" {
    try std.testing.expect(Regex.find("world", "wo.*") != null);
    try std.testing.expect(Regex.find("world", "wo.*d") != null);
    try std.testing.expect(Regex.find("wooooooooooorld", "wo*rld") != null);
    try std.testing.expect(Regex.find("wrld", "wo*rld") != null);
    try std.testing.expect(Regex.find("wrld", "wo*rd") == null);
}

test "plus" {
    try std.testing.expect(Regex.find("world", "wo.+") != null);
    try std.testing.expect(Regex.find("world", "wo.+d") != null);
    try std.testing.expect(Regex.find("wooooooooooorld", "wo+rld") != null);
    try std.testing.expect(Regex.find("wrld", "wo+rld") == null);
    try std.testing.expect(Regex.find("wrld", "wo+rd") == null);
}

test "question mark" {
    try std.testing.expect(Regex.find("world", "wo.?rld") != null);
    try std.testing.expect(Regex.find("world", "wor?ld") != null);
    try std.testing.expect(Regex.find("world", "wo.?ld") != null);
    try std.testing.expect(Regex.find("world", "wo.?d") == null);
}
