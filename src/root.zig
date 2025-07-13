//! A lexical analyzer class for simple shell-like syntaxes.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

// ===================== Error Types =====================

pub const ShlexError = error{
    NoClosingQuotation,
    NoEscapedCharacter,
    InvalidArgument,
    OutOfMemory,
    EndOfStream,
};

// ======================= Options =======================

pub const ShlexOptions = struct {
    /// Whether to use POSIX mode
    posix: bool = false,

    /// Punctuation character handling configuration
    punctuation_chars: union(enum) {
        /// Disable punctuation character handling
        disabled: void,
        /// Use default punctuation characters: ();<>|&
        enabled: void,
        /// Use custom punctuation characters
        custom: []const u8,
    } = .disabled,

    /// Debug level (0 = no debug, 1 = basic, 2 = verbose, 3 = very verbose)
    debug: u8 = 0,

    /// Whether to split at whitespace characters
    whitespace_split: bool = false,

    /// Comment characters
    commenters: []const u8 = "#",

    /// Quote characters
    quotes: []const u8 = "'\"",

    /// Escape characters
    escape: []const u8 = "\\",

    /// Characters that can be escaped within quotes
    escapedquotes: []const u8 = "\"",
};

// ========================= Constants =========================

// https://github.com/python/cpython/blob/9e5cebd56d06e35faeca166813215d72f2f8906a/Lib/shlex.py#L35
const DEFAULT_WORDCHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";
const POSIX_WORDCHARS_EXTRA = "ßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ";
const PUNCTUATION_WORDCHARS_EXTRA = "~-./*?=";
const DEFAULT_PUNCTUATION_CHARS = "();<>|&";
const DEFAULT_WHITESPACE = " \t\r\n";
const QUOTE_SAFE_CHARS = "%+,-./0123456789:=@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";

// ===================== State Machine States =====================

const TokenState = enum(u8) {
    /// Initial state, handling whitespace
    initial,
    /// Building word token
    word,
    /// Building punctuation token
    punctuation,
    /// Inside single quotes
    single_quoted,
    /// Inside double quotes
    double_quoted,
    /// Processing escape sequence
    escape,
    /// End of input
    eof,
};

// ====================== Lexer State =====================

const LexerState = struct {
    state: TokenState = .initial,
    escaped_state: TokenState = .initial,
    lineno: u32 = 1,
    input_pos: usize = 0,

    fn reset(self: *LexerState) void {
        self.state = .initial;
        self.escaped_state = .initial;
        self.lineno = 1;
        self.input_pos = 0;
    }
};

const StaticCharSet = struct {
    bitset: std.bit_set.StaticBitSet(256) = std.bit_set.StaticBitSet(256).initEmpty(),

    const Self = @This();

    fn init(comptime chars: []const u8) Self {
        var set = Self{};
        inline for (chars) |char| {
            set.add(char);
        }
        return set;
    }

    fn add(self: *Self, comptime char: u8) void {
        self.bitset.set(char);
    }

    fn contains(self: Self, char: u8) bool {
        return self.bitset.isSet(char);
    }

    fn isEmpty(self: Self) bool {
        return self.bitset.count() == 0;
    }

    fn count(self: Self) usize {
        return self.bitset.count();
    }

    fn addRange(self: *Self, comptime start: u8, comptime end: u8) void {
        self.bitset.setRangeValue(.{ .start = start, .end = end + 1 }, true);
    }

    fn merge(self: *Self, comptime other: Self) void {
        self.bitset.setUnion(other.bitset);
    }

    fn subtract(self: *Self, comptime other: Self) void {
        var tmp = other.bitset;
        tmp.toggleAll();
        self.bitset.setIntersection(tmp);
    }
};

pub fn Shlex(comptime options: ShlexOptions) type {
    return struct {
        const Self = @This();

        const CharSets = struct {
            wordchars: StaticCharSet,
            punctuation_chars: StaticCharSet,
            whitespace: StaticCharSet,
            commenters: StaticCharSet,
            quotes: StaticCharSet,
            escape: StaticCharSet,
            escapedquotes: StaticCharSet,
        };

        const char_sets = buildCharSets(options);

        allocator: Allocator,
        input_buffer: []const u8,
        lexer_state: LexerState,

        // Buffers
        token_buffer: ArrayList(u8),
        pushback_tokens: ArrayList([]const u8),
        pushback_chars: ArrayList(u8),

        fn buildCharSets(opts: ShlexOptions) CharSets {
            @setEvalBranchQuota(5000);

            var wordchars = StaticCharSet.init(DEFAULT_WORDCHARS);

            if (opts.posix) {
                inline for (POSIX_WORDCHARS_EXTRA) |char| {
                    wordchars.add(char);
                }
            }

            const punct_chars = switch (opts.punctuation_chars) {
                .disabled => "",
                .enabled => DEFAULT_PUNCTUATION_CHARS,
                .custom => |chars| chars,
            };

            const punctuation_chars = StaticCharSet.init(punct_chars);

            if (punct_chars.len > 0) {
                inline for (PUNCTUATION_WORDCHARS_EXTRA) |char| {
                    wordchars.add(char);
                }

                wordchars.subtract(punctuation_chars);
            }

            return CharSets{
                .wordchars = wordchars,
                .punctuation_chars = punctuation_chars,
                .whitespace = StaticCharSet.init(DEFAULT_WHITESPACE),
                .commenters = StaticCharSet.init(opts.commenters),
                .quotes = StaticCharSet.init(opts.quotes),
                .escape = StaticCharSet.init(opts.escape),
                .escapedquotes = StaticCharSet.init(opts.escapedquotes),
            };
        }

        pub fn init(allocator: Allocator, input: []const u8) !Self {
            return Self{
                .allocator = allocator,
                .input_buffer = input,
                .lexer_state = .{},
                .token_buffer = ArrayList(u8).init(allocator),
                .pushback_tokens = ArrayList([]const u8).init(allocator),
                .pushback_chars = ArrayList(u8).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.pushback_tokens.items) |token| {
                self.allocator.free(token);
            }
            self.pushback_tokens.deinit();
            self.pushback_chars.deinit();
            self.token_buffer.deinit();
        }

        /// Reset lexer state to parse new input
        pub fn reset(self: *Self, input: []const u8) !void {
            for (self.pushback_tokens.items) |token| {
                self.allocator.free(token);
            }
            self.pushback_tokens.clearRetainingCapacity();
            self.pushback_chars.clearRetainingCapacity();
            self.token_buffer.clearRetainingCapacity();

            // Reset state
            self.input_buffer = input;
            self.lexer_state.reset();
        }

        fn peekChar(self: *Self) ?u8 {
            if (self.lexer_state.input_pos >= self.input_buffer.len) {
                return null;
            }
            return self.input_buffer[self.lexer_state.input_pos];
        }

        fn readChar(self: *Self) ?u8 {
            if (self.lexer_state.input_pos >= self.input_buffer.len) {
                return null;
            }
            const char = self.input_buffer[self.lexer_state.input_pos];
            self.lexer_state.input_pos += 1;
            return char;
        }

        fn readLine(self: *Self) void {
            while (self.peekChar() != null and self.peekChar().? != '\n') {
                _ = self.readChar();
            }
        }

        fn pushToken(self: *Self, token: []const u8) !void {
            if (options.debug >= 1) {
                std.debug.print("shlex: push token {s}\n", .{token});
            }
            try self.pushback_tokens.insert(0, token);
        }

        fn popToken(self: *Self) ?[]const u8 {
            if (self.pushback_tokens.items.len == 0) {
                return null;
            }
            return self.pushback_tokens.orderedRemove(0);
        }

        fn pushChar(self: *Self, char: u8) !void {
            try self.pushback_chars.insert(0, char);
        }

        fn popChar(self: *Self) ?u8 {
            if (self.pushback_chars.items.len == 0) {
                return null;
            }
            return self.pushback_chars.orderedRemove(0);
        }

        fn getNextChar(self: *Self) ?u8 {
            if (!char_sets.punctuation_chars.isEmpty() and self.pushback_chars.items.len > 0) {
                return self.popChar();
            }
            return self.readChar();
        }

        // ==================== State Handling Functions ====================

        fn handleInitialState(self: *Self, char: ?u8) !?TokenAction {
            if (char == null) {
                self.lexer_state.state = .eof;
                return .finish_token;
            }

            const c = char.?;

            if (char_sets.whitespace.contains(c)) {
                if (options.debug >= 2) {
                    std.debug.print("shlex: whitespace seen in initial state\n", .{});
                }
                if (self.token_buffer.items.len > 0) {
                    return .finish_token;
                }
                return .continue_parsing;
            }

            if (char_sets.commenters.contains(c)) {
                self.readLine();
                self.lexer_state.lineno += 1;
                return .continue_parsing;
            }

            if (options.posix and char_sets.escape.contains(c)) {
                self.lexer_state.escaped_state = .word;
                self.lexer_state.state = .escape;
                return .continue_parsing;
            }

            if (char_sets.wordchars.contains(c)) {
                self.token_buffer.clearRetainingCapacity();
                try self.token_buffer.append(c);
                self.lexer_state.state = .word;
                return .continue_parsing;
            }

            if (char_sets.punctuation_chars.contains(c)) {
                self.token_buffer.clearRetainingCapacity();
                try self.token_buffer.append(c);
                self.lexer_state.state = .punctuation;
                return .continue_parsing;
            }

            if (c == '\'') {
                if (!options.posix) {
                    self.token_buffer.clearRetainingCapacity();
                    try self.token_buffer.append(c);
                }
                self.lexer_state.state = .single_quoted;
                return .continue_parsing;
            }

            if (c == '"') {
                if (!options.posix) {
                    self.token_buffer.clearRetainingCapacity();
                    try self.token_buffer.append(c);
                }
                self.lexer_state.state = .double_quoted;
                return .continue_parsing;
            }

            if (options.whitespace_split) {
                self.token_buffer.clearRetainingCapacity();
                try self.token_buffer.append(c);
                self.lexer_state.state = .word;
                return .continue_parsing;
            }

            // default case
            self.token_buffer.clearRetainingCapacity();
            try self.token_buffer.append(c);
            if (self.token_buffer.items.len > 0) {
                return .finish_token;
            }
            return .continue_parsing;
        }

        fn handleQuotedState(self: *Self, char: ?u8, quote_char: u8) !?TokenAction {
            if (char == null) {
                if (options.debug >= 2) {
                    std.debug.print("shlex: EOF seen in quoted state\n", .{});
                }
                return ShlexError.NoClosingQuotation;
            }

            const c = char.?;

            if (c == quote_char) {
                if (!options.posix) {
                    try self.token_buffer.append(c);
                    self.lexer_state.state = .initial;
                    return .finish_token;
                } else {
                    self.lexer_state.state = .word;
                    return .continue_parsing;
                }
            }

            // Only double quotes can be escaped in POSIX mode
            if (options.posix and char_sets.escape.contains(c) and quote_char == '"') {
                self.lexer_state.escaped_state = self.lexer_state.state;
                self.lexer_state.state = .escape;
                return .continue_parsing;
            }

            try self.token_buffer.append(c);
            return .continue_parsing;
        }

        fn handleEscapeState(self: *Self, char: ?u8) !?TokenAction {
            if (char == null) {
                if (options.debug >= 2) {
                    std.debug.print("shlex: EOF seen in escape state\n", .{});
                }
                return ShlexError.NoEscapedCharacter;
            }

            const c = char.?;

            // In POSIX shell, only quotes themselves or escape characters can be escaped within quotes
            switch (self.lexer_state.escaped_state) {
                .single_quoted => {
                    if (c != '\\' and c != '\'') {
                        try self.token_buffer.append('\\');
                    }
                },
                .double_quoted => {
                    if (c != '\\' and c != '"') {
                        try self.token_buffer.append('\\');
                    }
                },
                else => {},
            }

            try self.token_buffer.append(c);
            self.lexer_state.state = self.lexer_state.escaped_state;
            return .continue_parsing;
        }

        fn handleWordState(self: *Self, char: ?u8) !?TokenAction {
            if (char == null) {
                self.lexer_state.state = .eof;
                return .finish_token;
            }

            const c = char.?;

            if (char_sets.whitespace.contains(c)) {
                if (options.debug >= 2) {
                    std.debug.print("shlex: whitespace seen in word state\n", .{});
                }
                self.lexer_state.state = .initial;
                return .finish_token;
            }

            if (char_sets.commenters.contains(c)) {
                self.readLine();
                self.lexer_state.lineno += 1;
                if (options.posix) {
                    self.lexer_state.state = .initial;
                    return .finish_token;
                }
                return .continue_parsing;
            }

            if (options.posix and c == '\'') {
                self.lexer_state.state = .single_quoted;
                return .continue_parsing;
            }

            if (options.posix and c == '"') {
                self.lexer_state.state = .double_quoted;
                return .continue_parsing;
            }

            if (options.posix and char_sets.escape.contains(c)) {
                self.lexer_state.escaped_state = .word;
                self.lexer_state.state = .escape;
                return .continue_parsing;
            }

            if (char_sets.wordchars.contains(c) or
                char_sets.quotes.contains(c) or
                (options.whitespace_split and !char_sets.punctuation_chars.contains(c)))
            {
                try self.token_buffer.append(c);
                return .continue_parsing;
            }

            // Handle punctuation
            if (!char_sets.punctuation_chars.isEmpty()) {
                try self.pushChar(c);
            } else {
                const char_buf = [1]u8{c};
                try self.pushToken(&char_buf);
            }

            if (options.debug >= 2) {
                std.debug.print("shlex: punctuation seen in word state\n", .{});
            }

            self.lexer_state.state = .initial;
            return .finish_token;
        }

        fn handlePunctuationState(self: *Self, char: ?u8) !?TokenAction {
            if (char == null) {
                self.lexer_state.state = .eof;
                return .finish_token;
            }

            const c = char.?;

            if (char_sets.punctuation_chars.contains(c)) {
                try self.token_buffer.append(c);
                return .continue_parsing;
            }

            if (!char_sets.whitespace.contains(c)) {
                try self.pushChar(c);
            }

            self.lexer_state.state = .initial;
            return .finish_token;
        }

        const TokenAction = enum {
            continue_parsing,
            finish_token,
        };

        pub fn getToken(self: *Self) !?[]const u8 {
            if (self.pushback_tokens.items.len > 0) {
                const token = self.popToken();
                if (options.debug >= 1 and token != null) {
                    std.debug.print("shlex: pop token {s}\n", .{token.?});
                }
                return token;
            }

            const raw_token = try self.readToken();

            if (options.debug >= 1) {
                if (raw_token != null) {
                    std.debug.print("shlex: token={s}\n", .{raw_token.?});
                } else {
                    std.debug.print("shlex: token=EOF\n", .{});
                }
            }

            return raw_token;
        }

        pub fn readToken(self: *Self) !?[]const u8 {
            var quoted = false;
            self.token_buffer.clearRetainingCapacity();

            while (true) {
                const nextchar = self.getNextChar();

                if (nextchar != null and nextchar.? == '\n') {
                    self.lexer_state.lineno += 1;
                }

                if (options.debug >= 3) {
                    std.debug.print("shlex: character seen in state {any}: {any}\n", .{ self.lexer_state.state, nextchar });
                }

                const action = switch (self.lexer_state.state) {
                    .eof => {
                        self.token_buffer.clearRetainingCapacity();
                        break;
                    },
                    .initial => try self.handleInitialState(nextchar),
                    .single_quoted => blk: {
                        quoted = true;
                        break :blk try self.handleQuotedState(nextchar, '\'');
                    },
                    .double_quoted => blk: {
                        quoted = true;
                        break :blk try self.handleQuotedState(nextchar, '"');
                    },
                    .escape => try self.handleEscapeState(nextchar),
                    .word => try self.handleWordState(nextchar),
                    .punctuation => try self.handlePunctuationState(nextchar),
                };

                if (action) |act| {
                    switch (act) {
                        .continue_parsing => continue,
                        .finish_token => {
                            if (self.token_buffer.items.len > 0 or (options.posix and quoted)) {
                                break;
                            }
                            continue;
                        },
                    }
                }
            }

            // In POSIX mode, empty unquoted tokens should be null
            const should_return_null = options.posix and !quoted and self.token_buffer.items.len == 0;

            const final_result = if (should_return_null)
                null
            else if (self.token_buffer.items.len > 0)
                try self.allocator.dupe(u8, self.token_buffer.items)
            else if (quoted)
                try self.allocator.dupe(u8, "")
            else
                null;

            self.token_buffer.clearRetainingCapacity();

            if (options.debug > 1) {
                if (final_result != null) {
                    std.debug.print("shlex: raw token={s}\n", .{final_result.?});
                } else {
                    std.debug.print("shlex: raw token=EOF\n", .{});
                }
            }

            return final_result;
        }
    };
}

pub fn init(
    allocator: Allocator,
    input: []const u8,
    comptime options: ShlexOptions,
) !Shlex(options) {
    const ShlexType = Shlex(options);
    return ShlexType.init(allocator, input);
}

pub fn split(
    allocator: Allocator,
    s: []const u8,
    comptime comments: bool,
    comptime posix: bool,
) ![][]const u8 {
    const commenters = if (comments) "#" else "";

    const ShlexType = Shlex(.{
        .posix = posix,
        .commenters = commenters,
        .whitespace_split = true,
    });
    var lex = try ShlexType.init(allocator, s);
    defer lex.deinit();

    var result = ArrayList([]const u8).init(allocator);
    while (true) {
        const token = try lex.getToken();
        if (token == null) break;
        try result.append(token.?);
    }

    return result.toOwnedSlice();
}

pub fn quote(allocator: Allocator, s: []const u8) ![]const u8 {
    if (s.len == 0) {
        return try allocator.dupe(u8, "''");
    }

    for (s) |char| {
        if (char >= 128 or !std.mem.containsAtLeast(u8, QUOTE_SAFE_CHARS, 1, &[_]u8{char})) {
            var result = ArrayList(u8).init(allocator);
            defer result.deinit();

            try result.append('\'');
            for (s) |c| {
                if (c == '\'') {
                    try result.appendSlice("'\"'\"'");
                } else {
                    try result.append(c);
                }
            }
            try result.append('\'');

            return result.toOwnedSlice();
        }
    }

    return try allocator.dupe(u8, s);
}

pub fn join(allocator: Allocator, split_command: []const []const u8) ![]const u8 {
    var result = ArrayList(u8).init(allocator);
    defer result.deinit();

    for (split_command, 0..) |arg, i| {
        if (i > 0) {
            try result.append(' ');
        }
        const quoted_arg = try quote(allocator, arg);
        defer allocator.free(quoted_arg);
        try result.appendSlice(quoted_arg);
    }

    return result.toOwnedSlice();
}

