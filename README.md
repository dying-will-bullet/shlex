# shlex

[![CI](https://github.com/dying-will-bullet/shlex/actions/workflows/ci.yaml/badge.svg)](https://github.com/dying-will-bullet/shlex/actions/workflows/ci.yaml)
![](https://img.shields.io/badge/language-zig-%23ec915c)

A lexical analyzer for simple shell-like syntaxes implemented in Zig. This library provides functions to split, join, and quote strings using shell-like parsing rules.

Ported from the Python standard library. https://github.com/python/cpython/blob/main/Lib/shlex.py

## Features

- Split strings using shell-like tokenization rules
- Join string arrays with proper quoting
- Quote strings for safe shell usage
- Support for both POSIX and standard modes

## Usage

### split

Split a string into tokens using shell-like rules.

```zig
const std = @import("std");
const shlex = @import("shlex");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = "hello world 'quoted string'";
    const tokens = try shlex.split(allocator, input, false, true);
    defer {
        for (tokens) |token| {
            allocator.free(token);
        }
        allocator.free(tokens);
    }

    // tokens[0] = "hello"
    // tokens[1] = "world"
    // tokens[2] = "quoted string"
}
```

Parameters:
- `allocator`: Memory allocator
- `s`: Input string to split
- `comments`: Whether to handle comment characters (# by default)
- `posix`: Whether to use POSIX-compliant parsing rules

### quote

Quote a string for safe shell usage.

```zig
const std = @import("std");
const shlex = @import("shlex");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = "can't do it";
    const quoted = try shlex.quote(allocator, input);
    defer allocator.free(quoted);

    // quoted = "'can'\"'\"'t do it'"
}
```

Parameters:
- `allocator`: Memory allocator
- `s`: Input string to quote

### join

Join an array of strings with proper quoting.

```zig
const std = @import("std");
const shlex = @import("shlex");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = [_][]const u8{ "echo", "hello world", "can't" };
    const joined = try shlex.join(allocator, &args);
    defer allocator.free(joined);

    // joined = "echo 'hello world' 'can'\"'\"'t'"
}
```

Parameters:
- `allocator`: Memory allocator
- `split_command`: Array of strings to join


## Advanced Usage

For more advanced usage, you can create a lexer instance with custom options:

```zig
const lexer = try shlex.init(allocator, input, .{
    .posix = true,
    .punctuation_chars = .enabled,
    .whitespace_split = true,
    .debug = 1,
});
defer lexer.deinit();

while (true) {
    const token = try lexer.getToken();
    if (token == null) break;
    // Process token
    allocator.free(token.?);
}
```

## License

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

