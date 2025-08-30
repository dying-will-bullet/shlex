//! Tests for the shlex library
//! A comprehensive test suite for shell-like lexical analysis functionality

const std = @import("std");
const testing = std.testing;
const ArrayList = std.ArrayList;

const shlex = @import("root.zig");

// Test data structures
const TestData = struct {
    input: []const u8,
    expected: []const []const u8,
};

// Standard mode test data - based on Python actual behavior
const standard_test_data = [_]TestData{
    .{ .input = "x", .expected = &[_][]const u8{"x"} },
    .{ .input = "foo bar", .expected = &[_][]const u8{ "foo", "bar" } },
    .{ .input = " foo bar", .expected = &[_][]const u8{ "foo", "bar" } },
    .{ .input = " foo bar ", .expected = &[_][]const u8{ "foo", "bar" } },
    .{ .input = "foo   bar    bla     fasel", .expected = &[_][]const u8{ "foo", "bar", "bla", "fasel" } },
    .{ .input = "x y  z              xxxx", .expected = &[_][]const u8{ "x", "y", "z", "xxxx" } },
    .{ .input = "\\x bar", .expected = &[_][]const u8{ "\\x", "bar" } },
    .{ .input = "\\ x bar", .expected = &[_][]const u8{ "\\", "x", "bar" } },
    .{ .input = "\\ bar", .expected = &[_][]const u8{ "\\", "bar" } },
    .{ .input = "foo \\x bar", .expected = &[_][]const u8{ "foo", "\\x", "bar" } },
    .{ .input = "foo \\ x bar", .expected = &[_][]const u8{ "foo", "\\", "x", "bar" } },
    .{ .input = "foo \\ bar", .expected = &[_][]const u8{ "foo", "\\", "bar" } },
    .{ .input = "foo \"bar\" bla", .expected = &[_][]const u8{ "foo", "\"bar\"", "bla" } },
    .{ .input = "\"foo\" \"bar\" \"bla\"", .expected = &[_][]const u8{ "\"foo\"", "\"bar\"", "\"bla\"" } },
    .{ .input = "\"foo\" bar \"bla\"", .expected = &[_][]const u8{ "\"foo\"", "bar", "\"bla\"" } },
    .{ .input = "\"foo\" bar bla", .expected = &[_][]const u8{ "\"foo\"", "bar", "bla" } },
    .{ .input = "foo 'bar' bla", .expected = &[_][]const u8{ "foo", "'bar'", "bla" } },
    .{ .input = "'foo' 'bar' 'bla'", .expected = &[_][]const u8{ "'foo'", "'bar'", "'bla'" } },
    .{ .input = "'foo' bar 'bla'", .expected = &[_][]const u8{ "'foo'", "bar", "'bla'" } },
    .{ .input = "'foo' bar bla", .expected = &[_][]const u8{ "'foo'", "bar", "bla" } },
    .{ .input = "blurb foo\"bar\"bar\"fasel\" baz", .expected = &[_][]const u8{ "blurb", "foo\"bar\"bar\"fasel\"", "baz" } },
    .{ .input = "blurb foo'bar'bar'fasel' baz", .expected = &[_][]const u8{ "blurb", "foo'bar'bar'fasel'", "baz" } },
    .{ .input = "\"\"", .expected = &[_][]const u8{"\"\""} },
    .{ .input = "''", .expected = &[_][]const u8{"''"} },
    .{ .input = "foo \"\" bar", .expected = &[_][]const u8{ "foo", "\"\"", "bar" } },
    .{ .input = "foo '' bar", .expected = &[_][]const u8{ "foo", "''", "bar" } },
    .{ .input = "foo \"\" \"\" \"\" bar", .expected = &[_][]const u8{ "foo", "\"\"", "\"\"", "\"\"", "bar" } },
    .{ .input = "foo '' '' '' bar", .expected = &[_][]const u8{ "foo", "''", "''", "''", "bar" } },
    .{ .input = "\\\"\"", .expected = &[_][]const u8{"\\\"\""} },
    .{ .input = "\"\\\"", .expected = &[_][]const u8{"\"\\\""} },
    .{ .input = "\"foo\\ bar\"", .expected = &[_][]const u8{"\"foo\\ bar\""} },
    .{ .input = "\"foo\\\\ bar\"", .expected = &[_][]const u8{"\"foo\\\\ bar\""} },
    .{ .input = "\"foo\\\\ bar\\\"", .expected = &[_][]const u8{"\"foo\\\\ bar\\\""} },
    .{ .input = "\"foo\\\\\" bar\\\"\"", .expected = &[_][]const u8{ "\"foo\\\\\"", "bar\\\"\"" } },
    .{ .input = "\"foo\\\\ bar\\\" dfadf\"", .expected = &[_][]const u8{ "\"foo\\\\ bar\\\"", "dfadf\"" } },
    .{ .input = "\"foo\\\\\\ bar\\\" dfadf\"", .expected = &[_][]const u8{ "\"foo\\\\\\ bar\\\"", "dfadf\"" } },
    .{ .input = "\"foo\\\\\\x bar\\\" dfadf\"", .expected = &[_][]const u8{ "\"foo\\\\\\x bar\\\"", "dfadf\"" } },
    .{ .input = "\"foo\\x bar\\\" dfadf\"", .expected = &[_][]const u8{ "\"foo\\x bar\\\"", "dfadf\"" } },
    .{ .input = "\\''", .expected = &[_][]const u8{"\\''"} },
    .{ .input = "'foo\\ bar'", .expected = &[_][]const u8{"'foo\\ bar'"} },
    .{ .input = "'foo\\\\ bar'", .expected = &[_][]const u8{"'foo\\\\ bar'"} },
    .{ .input = "\"foo\\\\\\x bar\\\" df'a\\ 'df'", .expected = &[_][]const u8{ "\"foo\\\\\\x bar\\\"", "df'a\\", "'df'" } },
    .{ .input = "\\\"foo\"", .expected = &[_][]const u8{"\\\"foo\""} },
    .{ .input = "\\\"foo\"\\x", .expected = &[_][]const u8{"\\\"foo\"\\x"} },
    .{ .input = "\"foo\\x\"", .expected = &[_][]const u8{"\"foo\\x\""} },
    .{ .input = "\"foo\\ \"", .expected = &[_][]const u8{"\"foo\\ \""} },
    .{ .input = "foo\\ xx", .expected = &[_][]const u8{ "foo\\", "xx" } },
    .{ .input = "foo\\ x\\x", .expected = &[_][]const u8{ "foo\\", "x\\x" } },
    .{ .input = "foo\\ x\\x\\\"\"", .expected = &[_][]const u8{ "foo\\", "x\\x\\\"\"" } },
    .{ .input = "\"foo\\ x\\x\"", .expected = &[_][]const u8{"\"foo\\ x\\x\""} },
    .{ .input = "\"foo\\ x\\x\\\\\"", .expected = &[_][]const u8{"\"foo\\ x\\x\\\\\""} },
    .{ .input = "\"foo\\ x\\x\\\\\"\"foobar\"", .expected = &[_][]const u8{ "\"foo\\ x\\x\\\\\"", "\"foobar\"" } },
    .{ .input = "\"foo\\ x\\x\\\\\"\\''\"foobar\"", .expected = &[_][]const u8{ "\"foo\\ x\\x\\\\\"", "\\''\"foobar\"" } },
    .{ .input = "\"foo\\ x\\x\\\\\"\\'\"fo'obar\"", .expected = &[_][]const u8{ "\"foo\\ x\\x\\\\\"", "\\'\"fo'obar\"" } },
    .{ .input = "\"foo\\ x\\x\\\\\"\\'\"fo'obar\" 'don'\\''t'", .expected = &[_][]const u8{ "\"foo\\ x\\x\\\\\"", "\\'\"fo'obar\"", "'don'", "\\''t'" } },
    .{ .input = "'foo\\ bar'", .expected = &[_][]const u8{"'foo\\ bar'"} },
    .{ .input = "'foo\\\\ bar'", .expected = &[_][]const u8{"'foo\\\\ bar'"} },
    .{ .input = "foo\\ bar", .expected = &[_][]const u8{ "foo\\", "bar" } },
    .{ .input = "foo#bar\nbaz", .expected = &[_][]const u8{ "foo#bar", "baz" } },
    .{ .input = ":-) ;-)", .expected = &[_][]const u8{ ":-)", ";-)" } },
    .{ .input = "áéíóú", .expected = &[_][]const u8{"áéíóú"} },
};

// POSIX mode test data - based on Python actual behavior
const posix_test_data = [_]TestData{
    .{ .input = "x", .expected = &[_][]const u8{"x"} },
    .{ .input = "foo bar", .expected = &[_][]const u8{ "foo", "bar" } },
    .{ .input = " foo bar", .expected = &[_][]const u8{ "foo", "bar" } },
    .{ .input = " foo bar ", .expected = &[_][]const u8{ "foo", "bar" } },
    .{ .input = "foo   bar    bla     fasel", .expected = &[_][]const u8{ "foo", "bar", "bla", "fasel" } },
    .{ .input = "x y  z              xxxx", .expected = &[_][]const u8{ "x", "y", "z", "xxxx" } },
    .{ .input = "\\x bar", .expected = &[_][]const u8{ "x", "bar" } },
    .{ .input = "\\ x bar", .expected = &[_][]const u8{ " x", "bar" } },
    .{ .input = "\\ bar", .expected = &[_][]const u8{" bar"} },
    .{ .input = "foo \\x bar", .expected = &[_][]const u8{ "foo", "x", "bar" } },
    .{ .input = "foo \\ x bar", .expected = &[_][]const u8{ "foo", " x", "bar" } },
    .{ .input = "foo \\ bar", .expected = &[_][]const u8{ "foo", " bar" } },
    .{ .input = "foo \"bar\" bla", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "\"foo\" \"bar\" \"bla\"", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "\"foo\" bar \"bla\"", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "\"foo\" bar bla", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "foo 'bar' bla", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "'foo' 'bar' 'bla'", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "'foo' bar 'bla'", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "'foo' bar bla", .expected = &[_][]const u8{ "foo", "bar", "bla" } },
    .{ .input = "blurb foo\"bar\"bar\"fasel\" baz", .expected = &[_][]const u8{ "blurb", "foobarbarfasel", "baz" } },
    .{ .input = "blurb foo'bar'bar'fasel' baz", .expected = &[_][]const u8{ "blurb", "foobarbarfasel", "baz" } },
    .{ .input = "\"\"", .expected = &[_][]const u8{""} },
    .{ .input = "''", .expected = &[_][]const u8{""} },
    .{ .input = "foo \"\" bar", .expected = &[_][]const u8{ "foo", "", "bar" } },
    .{ .input = "foo '' bar", .expected = &[_][]const u8{ "foo", "", "bar" } },
    .{ .input = "foo \"\" \"\" \"\" bar", .expected = &[_][]const u8{ "foo", "", "", "", "bar" } },
    .{ .input = "foo '' '' '' bar", .expected = &[_][]const u8{ "foo", "", "", "", "bar" } },
    .{ .input = "\\\"", .expected = &[_][]const u8{"\""} },
    .{ .input = "\"\\\"\"", .expected = &[_][]const u8{"\""} },
    .{ .input = "\"foo\\ bar\"", .expected = &[_][]const u8{"foo\\ bar"} },
    .{ .input = "\"foo\\\\ bar\"", .expected = &[_][]const u8{"foo\\ bar"} },
    .{ .input = "\"foo\\\\ bar\\\"\"", .expected = &[_][]const u8{"foo\\ bar\""} },
    .{ .input = "\"foo\\\\\" bar\\\"", .expected = &[_][]const u8{ "foo\\", "bar\"" } },
    .{ .input = "\"foo\\\\ bar\\\" dfadf\"", .expected = &[_][]const u8{"foo\\ bar\" dfadf"} },
    .{ .input = "\"foo\\\\\\ bar\\\" dfadf\"", .expected = &[_][]const u8{"foo\\\\ bar\" dfadf"} },
    .{ .input = "\"foo\\\\\\x bar\\\" dfadf\"", .expected = &[_][]const u8{"foo\\\\x bar\" dfadf"} },
    .{ .input = "\"foo\\x bar\\\" dfadf\"", .expected = &[_][]const u8{"foo\\x bar\" dfadf"} },
    .{ .input = "\\'", .expected = &[_][]const u8{"'"} },
    .{ .input = "'foo\\ bar'", .expected = &[_][]const u8{"foo\\ bar"} },
    .{ .input = "'foo\\\\ bar'", .expected = &[_][]const u8{"foo\\\\ bar"} },
    .{ .input = "\"foo\\\\\\x bar\\\" df'a\\ 'df\"", .expected = &[_][]const u8{"foo\\\\x bar\" df'a\\ 'df"} },
    .{ .input = "\\\"foo", .expected = &[_][]const u8{"\"foo"} },
    .{ .input = "\\\"foo\\x", .expected = &[_][]const u8{"\"foox"} },
    .{ .input = "\"foo\\x\"", .expected = &[_][]const u8{"foo\\x"} },
    .{ .input = "\"foo\\ \"", .expected = &[_][]const u8{"foo\\ "} },
    .{ .input = "foo\\ xx", .expected = &[_][]const u8{"foo xx"} },
    .{ .input = "foo\\ x\\x", .expected = &[_][]const u8{"foo xx"} },
    .{ .input = "foo\\ x\\x\\\"", .expected = &[_][]const u8{"foo xx\""} },
    .{ .input = "\"foo\\ x\\x\"", .expected = &[_][]const u8{"foo\\ x\\x"} },
    .{ .input = "\"foo\\ x\\x\\\\\"", .expected = &[_][]const u8{"foo\\ x\\x\\"} },
    .{ .input = "\"foo\\ x\\x\\\\\"\"foobar\"", .expected = &[_][]const u8{"foo\\ x\\x\\foobar"} },
    .{ .input = "\"foo\\ x\\x\\\\\"\\'\"foobar\"", .expected = &[_][]const u8{"foo\\ x\\x\\'foobar"} },
    .{ .input = "\"foo\\ x\\x\\\\\"\\'\"fo'obar\"", .expected = &[_][]const u8{"foo\\ x\\x\\'fo'obar"} },
    .{ .input = "\"foo\\ x\\x\\\\\"\\'\"fo'obar\" 'don'\\''t'", .expected = &[_][]const u8{ "foo\\ x\\x\\'fo'obar", "don't" } },
    .{ .input = "\"foo\\ x\\x\\\\\"\\'\"fo'obar\" 'don'\\''t' \\\\", .expected = &[_][]const u8{ "foo\\ x\\x\\'fo'obar", "don't", "\\" } },
    .{ .input = "'foo\\ bar'", .expected = &[_][]const u8{"foo\\ bar"} },
    .{ .input = "'foo\\\\ bar'", .expected = &[_][]const u8{"foo\\\\ bar"} },
    .{ .input = "foo\\ bar", .expected = &[_][]const u8{"foo bar"} },
    .{ .input = "foo#bar\nbaz", .expected = &[_][]const u8{ "foo", "baz" } },
    .{ .input = ":-) ;-)", .expected = &[_][]const u8{ ":-)", ";-)" } },
    .{ .input = "áéíóú", .expected = &[_][]const u8{"áéíóú"} },
};

test "standard mode splitting" {
    const allocator = testing.allocator;

    for (standard_test_data, 0..) |test_case, i| {
        const result = shlex.split(allocator, test_case.input, false, false) catch |err| {
            std.debug.print("Test {}: Error splitting '{s}' - {}\n", .{ i, test_case.input, err });
            return err;
        };
        defer {
            for (result) |token| {
                allocator.free(token);
            }
            allocator.free(result);
        }

        if (result.len != test_case.expected.len) {
            std.debug.print("Test {}: Length mismatch for '{s}'\n", .{ i, test_case.input });
            std.debug.print("  Expected {} tokens: ", .{test_case.expected.len});
            for (test_case.expected) |token| {
                std.debug.print("'{s}' ", .{token});
            }
            std.debug.print("\n  Got {} tokens: ", .{result.len});
            for (result) |token| {
                std.debug.print("'{s}' ", .{token});
            }
            std.debug.print("\n", .{});
            return error.TestFailed;
        }

        for (result, test_case.expected) |got, expected| {
            if (!std.mem.eql(u8, got, expected)) {
                std.debug.print("Test {}: Token mismatch for '{s}'\n", .{ i, test_case.input });
                std.debug.print("  Expected: '{s}', Got: '{s}'\n", .{ expected, got });
                return error.TestFailed;
            }
        }
    }
}

test "posix mode splitting" {
    const allocator = testing.allocator;

    for (posix_test_data, 0..) |test_case, i| {
        const result = shlex.split(allocator, test_case.input, true, true) catch |err| {
            std.debug.print("POSIX Test {}: Error splitting '{s}' - {}\n", .{ i, test_case.input, err });
            return err;
        };
        defer {
            for (result) |token| {
                allocator.free(token);
            }
            allocator.free(result);
        }

        if (result.len != test_case.expected.len) {
            std.debug.print("POSIX Test {}: Length mismatch for '{s}'\n", .{ i, test_case.input });
            std.debug.print("  Expected {} tokens: ", .{test_case.expected.len});
            for (test_case.expected) |token| {
                std.debug.print("'{s}' ", .{token});
            }
            std.debug.print("\n  Got {} tokens: ", .{result.len});
            for (result) |token| {
                std.debug.print("'{s}' ", .{token});
            }
            std.debug.print("\n", .{});
            return error.TestFailed;
        }

        for (result, test_case.expected) |got, expected| {
            if (!std.mem.eql(u8, got, expected)) {
                std.debug.print("POSIX Test {}: Token mismatch for '{s}'\n", .{ i, test_case.input });
                std.debug.print("  Expected: '{s}', Got: '{s}'\n", .{ expected, got });
                return error.TestFailed;
            }
        }
    }
}

test "punctuation chars splitting" {
    const allocator = testing.allocator;

    // Test ampersand and pipe
    const amp_pipe_tests = [_]struct {
        input: []const u8,
        expected: []const []const u8,
    }{
        .{ .input = "echo hi && echo bye", .expected = &[_][]const u8{ "echo", "hi", "&&", "echo", "bye" } },
        .{ .input = "echo hi||echo bye", .expected = &[_][]const u8{ "echo", "hi", "||", "echo", "bye" } },
        .{ .input = "echo hi & echo bye", .expected = &[_][]const u8{ "echo", "hi", "&", "echo", "bye" } },
        .{ .input = "echo hi | echo bye", .expected = &[_][]const u8{ "echo", "hi", "|", "echo", "bye" } },
    };

    for (amp_pipe_tests, 0..) |test_case, i| {
        var lex = try shlex.init(allocator, test_case.input, .{ .punctuation_chars = .enabled });
        defer lex.deinit();

        var result = ArrayList([]const u8){};
        defer {
            for (result.items) |token| {
                allocator.free(token);
            }
            result.deinit(allocator);
        }

        while (true) {
            const token = try lex.getToken();
            if (token == null) break;
            try result.append(allocator, token.?);
        }

        if (result.items.len != test_case.expected.len) {
            std.debug.print("Punctuation Test {}: Length mismatch for '{s}'\n", .{ i, test_case.input });
            std.debug.print("  Expected {} tokens, got {}\n", .{ test_case.expected.len, result.items.len });
            return error.TestFailed;
        }

        for (result.items, test_case.expected) |got, expected| {
            if (!std.mem.eql(u8, got, expected)) {
                std.debug.print("Punctuation Test {}: Token mismatch for '{s}'\n", .{ i, test_case.input });
                std.debug.print("  Expected: '{s}', Got: '{s}'\n", .{ expected, got });
                return error.TestFailed;
            }
        }
    }
}

test "quote function comprehensive" {
    const allocator = testing.allocator;

    const quote_tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{ .input = "", .expected = "''" },
        .{ .input = "hello", .expected = "hello" },
        .{ .input = "test file name", .expected = "'test file name'" },
        .{ .input = "can't", .expected = "'can'\"'\"'t'" },
        .{ .input = "hello world", .expected = "'hello world'" },
        .{ .input = "safe123_-+=:,./", .expected = "safe123_-+=:,./" },
    };

    for (quote_tests, 0..) |test_case, i| {
        const result = try shlex.quote(allocator, test_case.input);
        defer allocator.free(result);

        if (!std.mem.eql(u8, result, test_case.expected)) {
            std.debug.print("Quote Test {}: Expected '{s}', Got '{s}'\n", .{ i, test_case.expected, result });
            return error.TestFailed;
        }
    }
}

test "join function comprehensive" {
    const allocator = testing.allocator;

    const join_tests = [_]struct {
        input: []const []const u8,
        expected: []const u8,
    }{
        .{ .input = &[_][]const u8{ "a ", "b" }, .expected = "'a ' b" },
        .{ .input = &[_][]const u8{ "a", " b" }, .expected = "a ' b'" },
        .{ .input = &[_][]const u8{ "a", " ", "b" }, .expected = "a ' ' b" },
        .{ .input = &[_][]const u8{ "hello", "world" }, .expected = "hello world" },
        .{ .input = &[_][]const u8{ "hello", "world", "with spaces" }, .expected = "hello world 'with spaces'" },
    };

    for (join_tests, 0..) |test_case, i| {
        const result = try shlex.join(allocator, test_case.input);
        defer allocator.free(result);

        if (!std.mem.eql(u8, result, test_case.expected)) {
            std.debug.print("Join Test {}: Expected '{s}', Got '{s}'\n", .{ i, test_case.expected, result });
            return error.TestFailed;
        }
    }
}

test "roundtrip consistency" {
    const allocator = testing.allocator;

    const roundtrip_tests = [_][]const u8{
        "hello world",
        "foo bar baz",
        "test with spaces",
        "safe test",
        "multiple  spaces",
        "echo hi bye",
    };

    for (roundtrip_tests, 0..) |test_input, i| {
        // Split first
        const split_result = try shlex.split(allocator, test_input, false, true);
        defer {
            for (split_result) |token| {
                allocator.free(token);
            }
            allocator.free(split_result);
        }

        // Join back
        const joined = try shlex.join(allocator, split_result);
        defer allocator.free(joined);

        // Split again
        const split_again = try shlex.split(allocator, joined, false, true);
        defer {
            for (split_again) |token| {
                allocator.free(token);
            }
            allocator.free(split_again);
        }

        // Compare
        if (split_result.len != split_again.len) {
            std.debug.print("Roundtrip Test {}: Length mismatch\n", .{i});
            std.debug.print("  Original: {}\n", .{split_result.len});
            std.debug.print("  Roundtrip: {}\n", .{split_again.len});
            return error.TestFailed;
        }

        for (split_result, split_again) |orig, roundtrip| {
            if (!std.mem.eql(u8, orig, roundtrip)) {
                std.debug.print("Roundtrip Test {}: Token mismatch\n", .{i});
                std.debug.print("  Original: '{s}'\n", .{orig});
                std.debug.print("  Roundtrip: '{s}'\n", .{roundtrip});
                return error.TestFailed;
            }
        }
    }
}

test "empty string handling" {
    const allocator = testing.allocator;

    // Test empty string handling in POSIX mode
    const empty_tests = [_]struct {
        input: []const u8,
        posix: bool,
        expected: []const []const u8,
    }{
        .{ .input = "'')abc", .posix = true, .expected = &[_][]const u8{")abc"} },
        .{ .input = "'')abc", .posix = false, .expected = &[_][]const u8{ "''", ")abc" } },
        .{ .input = "\"\"", .posix = true, .expected = &[_][]const u8{""} },
        .{ .input = "\"\"", .posix = false, .expected = &[_][]const u8{"\"\""} },
    };

    for (empty_tests, 0..) |test_case, i| {
        if (test_case.posix) {
            var lex = try shlex.init(allocator, test_case.input, .{ .posix = true, .whitespace_split = true });
            defer lex.deinit();

            var result = ArrayList([]const u8){};
            defer {
                for (result.items) |token| {
                    allocator.free(token);
                }
                result.deinit(allocator);
            }

            while (true) {
                const token = try lex.getToken();
                if (token == null) break;
                try result.append(allocator, token.?);
            }

            if (result.items.len != test_case.expected.len) {
                std.debug.print("Empty Test {}: Length mismatch for '{s}' (posix={})\n", .{ i, test_case.input, test_case.posix });
                std.debug.print("  Expected {} tokens, got {}\n", .{ test_case.expected.len, result.items.len });
                return error.TestFailed;
            }

            for (result.items, test_case.expected) |got, expected| {
                if (!std.mem.eql(u8, got, expected)) {
                    std.debug.print("Empty Test {}: Token mismatch for '{s}' (posix={})\n", .{ i, test_case.input, test_case.posix });
                    std.debug.print("  Expected: '{s}', Got: '{s}'\n", .{ expected, got });
                    return error.TestFailed;
                }
            }
        } else {
            var lex = try shlex.init(allocator, test_case.input, .{ .posix = false, .whitespace_split = true });
            defer lex.deinit();

            var result = ArrayList([]const u8){};
            defer {
                for (result.items) |token| {
                    allocator.free(token);
                }
                result.deinit(allocator);
            }

            while (true) {
                const token = try lex.getToken();
                if (token == null) break;
                try result.append(allocator, token.?);
            }

            if (result.items.len != test_case.expected.len) {
                std.debug.print("Empty Test {}: Length mismatch for '{s}' (posix={})\n", .{ i, test_case.input, test_case.posix });
                std.debug.print("  Expected {} tokens, got {}\n", .{ test_case.expected.len, result.items.len });
                return error.TestFailed;
            }

            for (result.items, test_case.expected) |got, expected| {
                if (!std.mem.eql(u8, got, expected)) {
                    std.debug.print("Empty Test {}: Token mismatch for '{s}' (posix={})\n", .{ i, test_case.input, test_case.posix });
                    std.debug.print("  Expected: '{s}', Got: '{s}'\n", .{ expected, got });
                    return error.TestFailed;
                }
            }
        }
    }
}

test "basic shlex functionality" {
    const allocator = testing.allocator;

    // Test split function
    const test_input = "hello world 'quoted string' \"double quoted\"";
    const result = try shlex.split(allocator, test_input, false, true);
    defer {
        for (result) |token| {
            allocator.free(token);
        }
        allocator.free(result);
    }

    try testing.expect(result.len == 4);
    try testing.expectEqualStrings("hello", result[0]);
    try testing.expectEqualStrings("world", result[1]);
    try testing.expectEqualStrings("quoted string", result[2]);
    try testing.expectEqualStrings("double quoted", result[3]);
}
