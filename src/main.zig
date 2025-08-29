const std = @import("std");
const shlex = @import("root.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "split")) {
        if (args.len < 3) {
            std.debug.print("Usage: {s} split <string>\n", .{args[0]});
            return;
        }
        try split(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "quote")) {
        if (args.len < 3) {
            std.debug.print("Usage: {s} quote <string>\n", .{args[0]});
            return;
        }
        try quote(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "join")) {
        if (args.len < 3) {
            std.debug.print("Usage: {s} join <arg1> [arg2] ...\n", .{args[0]});
            return;
        }
        try join(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "tokenize")) {
        if (args.len < 3) {
            std.debug.print("Usage: {s} tokenize <string>\n", .{args[0]});
            return;
        }
        try tokenize(allocator, args[2]);
    } else {
        std.debug.print("Unknown command: {s}\n", .{command});
        try printUsage();
    }
}

fn printUsage() !void {
    std.debug.print("shlex - A lexical analyzer for simple shell-like syntaxes\n\n", .{});
    std.debug.print("Usage:\n", .{});
    std.debug.print("  shlex split <string>       - Split string using shell-like syntax\n", .{});
    std.debug.print("  shlex quote <string>       - Quote string for shell usage\n", .{});
    std.debug.print("  shlex join <arg1> [arg2]... - Join arguments with proper quoting\n", .{});
    std.debug.print("  shlex tokenize <string>    - Tokenize string step by step\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  shlex split \"hello world 'quoted string'\"\n", .{});
    std.debug.print("  shlex quote \"can't do it\"\n", .{});
    std.debug.print("  shlex join hello world \"with spaces\"\n", .{});
}

fn split(allocator: std.mem.Allocator, input: []const u8) !void {
    std.debug.print("Splitting: {s}\n", .{input});

    const tokens = try shlex.split(allocator, input, false, true);
    defer {
        for (tokens) |token| {
            allocator.free(token);
        }
        allocator.free(tokens);
    }

    std.debug.print("Result ({d} tokens):\n", .{tokens.len});
    for (tokens, 0..) |token, i| {
        std.debug.print("  [{d}]: \"{s}\"\n", .{ i, token });
    }
}

fn quote(allocator: std.mem.Allocator, input: []const u8) !void {
    std.debug.print("Quoting: {s}\n", .{input});

    const quoted = try shlex.quote(allocator, input);
    defer allocator.free(quoted);

    std.debug.print("Result: {s}\n", .{quoted});
}

fn join(allocator: std.mem.Allocator, args: []const []const u8) !void {
    std.debug.print("Joining {d} arguments:\n", .{args.len});
    for (args, 0..) |arg, i| {
        std.debug.print("  [{d}]: \"{s}\"\n", .{ i, arg });
    }

    const joined = try shlex.join(allocator, args);
    defer allocator.free(joined);

    std.debug.print("Result: {s}\n", .{joined});
}

fn tokenize(allocator: std.mem.Allocator, input: []const u8) !void {
    std.debug.print("Tokenizing: {s}\n", .{input});

    var lexer = try shlex.init(allocator, input, .{ .posix = true, .whitespace_split = true, .debug = 1 });
    defer lexer.deinit();

    var token_count: usize = 0;
    while (true) {
        const token = try lexer.getToken();
        if (token == null) break;

        std.debug.print("Token {d}: \"{s}\"\n", .{ token_count, token.? });
        allocator.free(token.?);
        token_count += 1;
    }

    std.debug.print("Total tokens: {d}\n", .{token_count});
}

test "main functionality" {
    const allocator = std.testing.allocator;

    const test_input = "hello world 'quoted string'";
    const tokens = try shlex.split(allocator, test_input, false, true);
    defer {
        for (tokens) |token| {
            allocator.free(token);
        }
        allocator.free(tokens);
    }

    try std.testing.expect(tokens.len == 3);
    try std.testing.expectEqualStrings("hello", tokens[0]);
    try std.testing.expectEqualStrings("world", tokens[1]);
    try std.testing.expectEqualStrings("quoted string", tokens[2]);
}
