const std = @import("std");
const mem = std.mem;

/// A Lox Scanner to find Token's in source code
pub const Scanner = struct {
    /// Lox Token
    pub const Token = struct {
        /// Lox Token Types
        pub const Type = enum {
            // zig fmt: off
            token_left_paren, token_right_paren, token_left_brace, token_right_brace, token_comma, token_dot, token_minus, token_plus, token_semicolon, token_slash, token_star,

            // one or two character tokens.
            token_bang, token_bang_equal, token_equal, token_equal_equal, token_greater, token_greater_equal, token_less, token_less_equal,

            // literals.
            token_identifier, token_string, token_number,

            // keywords.
            token_and, token_class, token_else, token_false, token_for, token_fun, token_if, token_nil, token_or, token_print, token_return, token_super, token_this, token_true, token_var, token_while,

            // other.
            token_error, token_eof,
            // zig fmt: on
        };

        type: Type,
        slice: []const u8,
        line: u16,
    };

    const Self = @This();

    source: []const u8,
    start: usize,
    current: usize, // 1 past token_start
    line: u16,

    /// Initialize new Scanner
    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    /// Scan next token
    pub fn next(self: *Self) Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isEnd()) {
            return self.token(.token_eof);
        }

        const c = self.advance();
        if (self.isAlpha(c)) {
            return self.identifier();
        }
        if (self.isDigit(c)) {
            return self.number();
        }
        return switch (c) {
            '(' => self.token(.token_left_paren),
            ')' => self.token(.token_right_paren),
            '{' => self.token(.token_left_brace),
            '}' => self.token(.token_right_brace),
            ';' => self.token(.token_semicolon),
            ',' => self.token(.token_comma),
            '.' => self.token(.token_dot),
            '-' => self.token(.token_minus),
            '+' => self.token(.token_plus),
            '/' => self.token(.token_slash),
            '*' => self.token(.token_star),
            '!' => self.token(if (self.isMatch('=')) .token_bang_equal else .token_bang),
            '=' => self.token(if (self.isMatch('=')) .token_equal_equal else .token_equal),
            '<' => self.token(if (self.isMatch('=')) .token_less_equal else .token_less),
            '>' => self.token(if (self.isMatch('=')) .token_greater_equal else .token_greater),
            '"' => self.string(),
            else => self.tokenError("Unexpected character."),
        };
    }

    /// Create new token
    fn token(self: Self, type_: Token.Type) Token {
        return Token{
            .type = type_,
            .slice = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    /// Create new error token
    fn tokenError(self: Self, message: []const u8) Token {
        return .{
            .type = .token_error,
            .slice = message,
            .line = self.line,
        };
    }

    /// Create identifier token
    /// REPLACES: identifier and identifierType (scanner.c)
    fn identifier(self: *Self) Token {
        while (self.isAlpha(self.peek()) or self.isDigit(self.peek())) {
            _ = self.advance();
        }
        const type_ = switch (self.source[self.start]) {
            'a' => self.tryMatchIdentifier(1, 2, "nd", .token_and),
            'c' => self.tryMatchIdentifier(1, 4, "lass", .token_class),
            'e' => self.tryMatchIdentifier(1, 3, "lse", .token_else),
            'f' => switch (self.source[self.start + 1]) {
                'a' => self.tryMatchIdentifier(2, 3, "lse", .token_false),
                'o' => self.tryMatchIdentifier(2, 1, "r", .token_for),
                'u' => self.tryMatchIdentifier(2, 1, "n", .token_fun),
                else => .token_identifier,
            },
            'i' => self.tryMatchIdentifier(1, 1, "f", .token_if),
            'n' => self.tryMatchIdentifier(1, 2, "il", .token_nil),
            'o' => self.tryMatchIdentifier(1, 1, "r", .token_or),
            'p' => self.tryMatchIdentifier(1, 4, "rint", .token_print),
            'r' => self.tryMatchIdentifier(1, 5, "eturn", .token_return),
            's' => self.tryMatchIdentifier(1, 4, "uper", .token_super),
            't' => switch (self.source[self.start + 1]) {
                'h' => self.tryMatchIdentifier(2, 2, "is", .token_this),
                'r' => self.tryMatchIdentifier(2, 2, "ue", .token_true),
                else => .token_identifier,
            },
            'v' => self.tryMatchIdentifier(1, 2, "ar", .token_var),
            'w' => self.tryMatchIdentifier(1, 4, "hile", .token_while),
            else => .token_identifier,
        };
        return self.token(type_);
    }

    /// Create number token
    fn number(self: *Self) Token {
        while (self.isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and self.isDigit(self.peekNext())) {
            _ = self.advance();
            while (self.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        return self.token(.token_number);
    }

    /// Create string token
    fn string(self: *Self) Token {
        while (self.peek() != '"' and !self.isEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }
        if (self.isEnd()) {
            return self.tokenError("Unteriminated token_string.");
        }
        _ = self.advance();
        return self.token(.token_string);
    }

    /// Advance current position
    fn advance(self: *Self) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    /// Check if character is alpha
    fn isAlpha(_: Self, char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or (char == '_');
    }

    /// Check if character is a digit
    fn isDigit(_: Self, char: u8) bool {
        return char >= '0' and char <= '9';
    }

    /// Check if end of source
    fn isEnd(self: *Self) bool {
        return self.current == self.source.len;
    }

    /// Check if current position matches char (advance if true)
    fn isMatch(self: *Self, char: u8) bool {
        if (self.isEnd() or self.source[self.current] != char) {
            return false;
        }
        _ = self.advance();
        return true;
    }

    /// Get current character
    fn peek(self: *Self) u8 {
        if (self.isEnd()) {
            return 0;
        }
        return self.source[self.current];
    }

    /// Get next character
    fn peekNext(self: *Self) u8 {
        if (self.isEnd()) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    /// Skip whitespace in source
    fn skipWhitespace(self: *Self) void {
        while (true) {
            if (self.isEnd()) {
                return;
            }

            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    /// Test matching string and return defined Token.Type else Token.Type.identifier
    /// REPLACES: checkKeyword (scanner.c)
    fn tryMatchIdentifier(self: Self, skip: usize, length: usize, match: []const u8, type_: Token.Type) Token.Type {
        const check_length = self.current - self.start - skip;
        if (check_length != length) {
            return .token_identifier;
        }
        const check = self.source[(self.start + skip)..(self.start + skip + length)];
        if (mem.eql(u8, check, match)) {
            return type_;
        }
        return .token_identifier;
    }
};

const testing = std.testing;

test "token" {
    var s = Scanner.init("return;");

    for (0..6) |_| {
        _ = s.advance();
    }
    const token = s.token(.token_return);

    // NOTE: don't think we need token_start/length
    try testing.expectEqual(.token_return, token.type);
    try testing.expectEqual(1, token.line);
    try testing.expectEqual(s.source[0..6], token.slice);
    try testing.expectEqualStrings("return", token.slice);
}

test "whitespace" {
    const source =
        \\ // comment
        \\;
    ;
    var s = Scanner.init(source);

    s.skipWhitespace();

    try testing.expectEqual(source.len - 1, s.current);
}

test "expected token" {
    const Test = struct {
        source: []const u8,
        exp_token: Scanner.Token.Type,
        exp_slice: []const u8,
    };

    const tests = [_]Test{
        .{
            .source = "\n",
            .exp_token = .token_eof,
            .exp_slice = "",
        },
        .{
            .source = "{",
            .exp_token = .token_left_brace,
            .exp_slice = "{",
        },
        .{
            .source = "100.00",
            .exp_token = .token_number,
            .exp_slice = "100.00",
        },
        .{
            .source = " for;",
            .exp_token = .token_for,
            .exp_slice = "for",
        },
        .{
            .source = " \"string\";",
            .exp_token = .token_string,
            .exp_slice = "\"string\"",
        },
        .{
            .source = " while (true) {};",
            .exp_token = .token_while,
            .exp_slice = "while",
        },
    };
    for (tests) |t| {
        var s = Scanner.init(t.source);
        const token = s.next();

        try testing.expectEqual(t.exp_token, token.type);
        try testing.expectEqualStrings(t.exp_slice, token.slice);
    }
}
