// Add files to be use tested by zig
comptime {
    _ = @import("main.zig");
    _ = @import("chunk.zig");
    _ = @import("compiler.zig");
    _ = @import("memory.zig");
    _ = @import("object.zig");
    _ = @import("scanner.zig");
    _ = @import("table.zig");
    _ = @import("value.zig");
    _ = @import("vm.zig");
}

const std = @import("std");
const testing = std.testing;

const VM = @import("vm.zig").VM;
const GC = @import("memory.zig").GarbageCollector;

/// Testing Fixture for VM because it's needed everywhere and adding tests that's driving me insane
/// Sets up a VM (in memory) with GC applied for testing
pub const FixtureVM = struct {
    vm: VM,
    gc: GC,
    test_dir: testing.TmpDir,
    stdout: std.fs.File,
    stderr: std.fs.File,

    // Could potentially just move this to setup
    pub fn init() !*FixtureVM {
        const self = try testing.allocator.create(FixtureVM);
        self.* = .{
            .test_dir = testing.tmpDir(.{}),
            .vm = undefined,
            .gc = undefined,
            .stdout = undefined,
            .stderr = undefined,
        };
        return self;
    }

    pub fn setup(self: *FixtureVM) !void {
        self.stdout = try self.test_dir.dir.createFile("stdout", .{ .read = true });
        self.stderr = try self.test_dir.dir.createFile("stderr", .{ .read = true });
        self.vm = VM.init(testing.allocator, self.stdout.writer(), self.stderr.writer());
        try self.vm.applyGarbageCollector();
        self.vm.applyParser();
        try self.vm.applyAllocator();
        try self.vm.applyNative();
    }

    pub fn teardown(self: *FixtureVM) void {
        self.vm.deinit();
        testing.allocator.destroy(self);
    }
};
