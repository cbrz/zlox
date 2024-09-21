const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const SinglyLinkedList = std.SinglyLinkedList;
const Writer = std.fs.File.Writer;

const common = @import("common.zig");
const Object = @import("object.zig").Object;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

const STRESS_GC = common.DEBUG_STRESS_GC;
const GC_HEAP_GROW_FACTOR: usize = 2;

pub const GarbageCollector = struct {
    const Error = Allocator.Error || Writer.Error;

    const Self = @This();

    vm: *VM,
    child_allocator: Allocator,
    writer: Writer,
    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(vm: *VM, child_allocator: Allocator) Self {
        return .{
            .vm = vm,
            .child_allocator = child_allocator,
            .writer = std.io.getStdErr().writer(),
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
        };
    }

    pub fn allocator(self: *Self) Allocator {
        return .{ .ptr = self, .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        } };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.bytes_allocated += len;
        if (self.bytes_allocated > self.next_gc or STRESS_GC) {
            self.collect() catch {
                // NOTE: Could try to free memory here
                @panic("Error in garbage collector.");
            };
        }
        return self.child_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        if (buf.len > new_len) {
            self.bytes_allocated += (new_len - buf.len);
            if (self.bytes_allocated > self.next_gc or STRESS_GC) {
                self.collect() catch {
                    // NOTE: Could try to free memory here
                    @panic("Error in garbage collector.");
                };
            }
        }
        return self.child_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        return self.child_allocator.rawFree(buf, log2_buf_align, ret_addr);
    }

    const GrayStack = ArrayList(*Object.Obj);

    fn collect(self: *Self) !void {
        if (self.bytes_allocated > self.next_gc) {
            self.next_gc *= GC_HEAP_GROW_FACTOR;
        }

        var gray_stack = GrayStack.init(self.child_allocator);
        defer gray_stack.deinit();

        if (common.DEBUG_TRACE_GC) {
            try self.writer.print(
                \\[
                \\  'action': 'collect garbage',
                \\  'mark': [
                \\
            , .{});
        }
        try self.mark(&gray_stack);
        if (common.DEBUG_TRACE_GC) {
            try self.writer.print("  ]\n", .{});
        }

        if (common.DEBUG_TRACE_GC) {
            try self.writer.print("  'trace': [\n", .{});
        }
        try self.trace(&gray_stack);
        if (common.DEBUG_TRACE_GC) {
            try self.writer.print("  ]\n", .{});
        }

        if (common.DEBUG_TRACE_GC) {
            try self.writer.print("  'sweep': [\n", .{});
        }
        try self.sweep();
        if (common.DEBUG_TRACE_GC) {
            try self.writer.print(
                \\  ]
                \\]
                \\
            , .{});
        }
    }

    fn mark(self: *Self, gray_stack: *GrayStack) Error!void {
        for (self.vm.stack.items) |v| {
            try self.markValue(gray_stack, v);
        }

        for (self.vm.frames.items) |cf| {
            try self.markObject(gray_stack, &cf.closure.obj);
        }

        for (self.vm.open_upvalues.items) |uv| {
            try self.markObject(gray_stack, &uv.obj);
        }

        try self.markCompiler(gray_stack);

        try self.markTable(gray_stack, &self.vm.globals);

        if (self.vm.init_string) |init_string| {
            try self.markObject(gray_stack, &init_string.obj);
        }
    }

    fn trace(self: *Self, gray_stack: *GrayStack) Error!void {
        while (gray_stack.items.len > 0) {
            const obj = gray_stack.pop();
            try self.blacken(gray_stack, obj);
        }
    }

    fn sweep(self: Self) Error!void {
        var previous: ?*Object.Obj = null;
        var object: ?*Object.Obj = self.vm.objects;
        while (object) |o| {
            if (o.marked) {
                o.marked = false;
                previous = o;
                object = o.next;
            } else {
                const unreached = o;
                object = o.next;
                if (previous) |p| {
                    p.next = object;
                } else {
                    self.vm.objects = object;
                }
                if (common.DEBUG_TRACE_GC) {
                    try self.writer.print("    [ 'addr': '{*}', 'type': '{}', 'value': '", .{ unreached, unreached.type });
                    try unreached.print(self.writer);
                    try self.writer.print("' ]\n", .{});
                }
                unreached.destroy(self.vm);
            }
        }
    }

    fn blacken(self: *Self, gray_stack: *GrayStack, obj: *Object.Obj) Error!void {
        switch (obj.type) {
            .bound_method => {
                const bound_method = obj.boundMethod();
                try self.markObject(gray_stack, &bound_method.method.obj);
            },
            .class => {
                const class = obj.class();
                try self.markObject(gray_stack, &class.name.obj);
                try self.markTable(gray_stack, &class.methods);
            },
            .closure => {
                const closure = obj.closure();
                try self.markObject(gray_stack, &closure.function.obj);
                for (closure.upvalues) |nullable_upvalue| {
                    if (nullable_upvalue) |upvalue| {
                        try self.markObject(gray_stack, &upvalue.obj);
                    }
                }
            },
            .function => {
                const function = obj.function();
                try self.markValueArrayList(gray_stack, function.chunk.constants);
                try self.markObject(gray_stack, &function.name.obj);
            },
            .instance => {
                const instance = obj.instance();
                try self.markObject(gray_stack, &instance.class.obj);
                try self.markTable(gray_stack, &instance.fields);
            },
            .native => {},
            .string => {},
            .upvalue => {
                const upvalue = obj.upvalue();
                try self.markValue(gray_stack, upvalue.closed);
            },
        }
    }

    fn markCompiler(self: *Self, gray_stack: *GrayStack) Error!void {
        if (self.vm.parser) |parser| {
            var opt_compiler = parser.compiler;
            while (opt_compiler) |compiler| {
                try self.markObject(gray_stack, &compiler.function.obj);
                opt_compiler = compiler.enclosing;
            }
        }
    }

    fn markTable(self: *Self, gray_stack: *GrayStack, table: *Table) Error!void {
        var iter = table.hashmap.iterator();
        while (iter.next()) |entry| {
            try self.markObject(gray_stack, &entry.key_ptr.*.obj);
            try self.markValue(gray_stack, entry.value_ptr.*);
        }
    }

    fn markObject(self: *Self, gray_stack: *GrayStack, obj: *Object.Obj) Error!void {
        if (obj.marked) {
            return;
        }
        obj.marked = true;
        try gray_stack.append(obj);
        if (common.DEBUG_TRACE_GC) {
            try self.writer.print("    [ 'addr': '{*}', 'type': '{}', 'value': '", .{ obj, obj.type });
            try obj.print(self.writer);
            try self.writer.print("' ]\n", .{});
        }
    }

    fn markValue(self: *Self, gray_stack: *GrayStack, value: Value) Error!void {
        switch (value) {
            .object => |o| try self.markObject(gray_stack, o),
            else => {},
        }
    }

    fn markValueArrayList(self: *Self, gray_stack: *GrayStack, arr: ArrayList(Value)) Error!void {
        for (arr.items) |v| {
            try self.markValue(gray_stack, v);
        }
    }
};

const testing = std.testing;
const FixtureVM = @import("test.zig").FixtureVM;

// Not possible to test this well because the string won't be appended to
// vm.parser.compiler.function.chunk.constants. If collect happens after
// string is created and before it's in place it can be marked, it will
// be freed
test "collect" {
    const fixture = try FixtureVM.init();
    try fixture.setup();
    defer fixture.teardown();
    var vm = &fixture.vm;

    // TODO: error here when allocating data onto vm.stack
    // need to add capacity, which requires allocating memory
    const e1 = try Object.String.copy(vm, "1");
    try vm.push(Value{ .object = &e1.obj });
    const e2 = try Object.String.copy(vm, "2");
    try vm.push(Value{ .object = &e2.obj });

    const s2 = vm.pop().object.string();
    const s1 = vm.pop().object.string();

    try testing.expectEqual(e1, s1);
    try testing.expectEqual(e2, s2);

    //try fixture.vm.gc.collect();
}
