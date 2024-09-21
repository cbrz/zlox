const std = @import("std");
const process = std.process;
const Allocator = std.mem.Allocator;

const VM = @import("vm.zig").VM;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    const allocator = std.heap.page_allocator;

    var vm = VM.init(allocator, stdout, stderr);
    defer vm.deinit();
    try vm.applyGarbageCollector();
    vm.applyParser(); // not sure if this should live in VM.interpret
    try vm.applyAllocator();
    try vm.applyNative();

    const args = try std.process.argsAlloc(allocator);

    switch (args.len) {
        1 => try runRepl(&vm),
        2 => try runFile(allocator, &vm, args[1]),
        else => {
            try stderr.print("Error: Too many arguments to zlox.", .{});
            std.process.exit(64);
        },
    }
}

pub fn runRepl(vm: *VM) !void {
    var line = [_]u8{0} ** 1024;
    const reader = std.io.getStdIn().reader();
    while (true) {
        try vm.writer_out.print("> ", .{});
        const source = try reader.readUntilDelimiterOrEof(&line, '\n') orelse return;
        try vm.interpret(source);
    }
}

pub fn runFile(allocator: Allocator, vm: *VM, path: []const u8) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(source);
    try vm.interpret(source);
}
