const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const BufPrintError = std.fmt.BufPrintError;
const ArrayList = std.ArrayList;
const Writer = std.fs.File.Writer;

const common = @import("common.zig");
const Chunk = @import("chunk.zig").Chunk;
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Object = @import("object.zig").Object;
const BoundMethod = Object.BoundMethod;
const Class = Object.Class;
const Obj = Object.Obj;
const Closure = Object.Closure;
const Function = Object.Function;
const Instance = Object.Instance;
const String = Object.String;
const Parser = @import("compiler.zig").Parser;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const Upvalue = Object.Upvalue;

const Byte = common.Byte;
const Short = common.Short;
const FRAME_MAX = common.FRAME_MAX;
const STACK_MAX = common.STACK_MAX;

pub const InterpreterError = error{
    RuntimeError,
};

/// Lox bytecode VM
/// - can handle memory via garbage collector
pub const VM = struct {
    pub const Error = (InterpreterError || Parser.Error || Allocator.Error || Writer.Error || BufPrintError);

    const CallFrame = struct {
        closure: *Closure,
        ip: usize,
        offset: usize, // TODO: kinda weird, book used a pointer... basically it's where to start the stack
    };

    const Self = @This();

    allocator: Allocator,
    writer_out: Writer,
    writer_err: Writer,

    frames: ArrayList(CallFrame),
    globals: Table,
    stack: ArrayList(Value),
    strings: Table,

    gc: GarbageCollector,
    init_string: ?*String,
    parser: ?Parser,
    objects: ?*Obj, // QUESTION: replace with DoublyLinkedList?
    open_upvalues: ArrayList(*Upvalue),

    pub fn init(allocator: Allocator, writer_out: Writer, writer_err: Writer) Self {
        return .{
            .allocator = allocator,
            .writer_out = writer_out,
            .writer_err = writer_err,

            .frames = ArrayList(CallFrame).init(allocator),
            .globals = Table.init(allocator),
            .stack = ArrayList(Value).init(allocator),
            .strings = Table.init(allocator),
            .open_upvalues = ArrayList(*Upvalue).init(allocator),

            .gc = undefined,
            // NOTE: pass in later
            .init_string = null,
            .parser = null,
            .objects = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.frames.deinit();
        self.globals.deinit();
        self.open_upvalues.deinit();
        self.stack.deinit();
        self.strings.deinit();
        self.free();
    }

    /// REPLACES: freeObjects (memory.c)
    fn free(self: *Self) void {
        var obj = self.objects;
        while (obj) |o| {
            const next = o.next;
            o.destroy(self);
            obj = next;
        }
    }

    pub fn applyGarbageCollector(self: *Self) Allocator.Error!void {
        self.gc = GarbageCollector.init(self, self.allocator);
        self.allocator = self.gc.allocator();
    }

    pub fn applyNative(self: *Self) Error!void {
        try self.defineNative("timestamp", timestampNative);
        try self.defineNative("clock", timestampNative);
    }

    pub fn applyParser(self: *Self) void {
        self.parser = Parser.init(self);
    }

    pub fn applyAllocator(self: *Self) Allocator.Error!void {
        self.frames.deinit();
        self.globals.deinit();
        self.open_upvalues.deinit();
        self.stack.deinit();
        self.strings.deinit();

        self.globals = Table.init(self.allocator);
        self.strings = Table.init(self.allocator);
        self.open_upvalues = ArrayList(*Upvalue).init(self.allocator);
        self.frames = try ArrayList(CallFrame).initCapacity(self.allocator, FRAME_MAX);
        self.stack = try ArrayList(Value).initCapacity(self.allocator, STACK_MAX);
        // NOTE: stack needs to be recreated
        self.init_string = try String.copy(self, "init");
        try self.reset();
    }

    pub fn interpret(self: *Self, source: []const u8) Error!void {
        if (self.parser.?.compile(source)) |function| {
            if (common.DEBUG_PRINT_CODE) {
                try function.chunk.disassemble("code");
            }
            try self.push(Value{ .object = &function.obj });
            const closure = try Closure.create(self, function);
            _ = self.pop();
            try self.push(Value{ .object = &closure.obj });
            try self.call(closure, 0);
            return self.run();
        } else |e| {
            std.debug.print("Error during interpret: {any}\n", .{e});
            return;
        }
    }

    // REGION: stack manipulation

    fn peek(self: *Self, distance: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - distance];
    }

    fn peekAddress(self: *Self, distance: usize) *Value {
        return &self.stack.items[self.stack.items.len - 1 - distance];
    }

    pub fn pop(self: *Self) Value {
        return self.stack.pop();
    }

    pub fn push(self: *Self, value: Value) Allocator.Error!void {
        assert(self.stack.items.len < STACK_MAX);
        try self.stack.append(value);
    }

    /// REPLACES: resetStack (vm.cs)
    fn reset(self: *Self) Allocator.Error!void {
        try self.stack.resize(0);
    }

    // REGION: closure/function

    fn call(self: *Self, callee: *Closure, arg_count: Byte) Error!void {
        if (arg_count != callee.function.arity) {
            return self.throwRuntimeError("Expected {d} arguments but got {d}.", .{ callee.function.arity, arg_count });
        }

        if (self.frames.items.len >= FRAME_MAX) {
            return self.throwRuntimeError("Stack overflow.", .{});
        }
        try self.frames.append(.{
            .closure = callee,
            .ip = 0,
            .offset = self.stack.items.len - arg_count - 1,
        });
    }

    fn callValue(self: *Self, callee: Value, arg_count: Byte) Error!void {
        assert(callee.isObject());
        return switch (callee.object.type) {
            .bound_method => {
                const bound = callee.object.boundMethod();
                self.stack.items[self.stack.items.len - arg_count - 1] = bound.receiver;
                try self.call(bound.method, arg_count);
            },
            .closure => self.call(callee.object.closure(), arg_count),
            //.function => self.call(callee.object.function(), arg_count),
            .class => {
                const class = callee.object.class();
                const instance = try Instance.create(self, class);
                self.stack.items[self.stack.items.len - arg_count - 1] = Value{ .object = &instance.obj };
                // NOTE: shouldn't be null at this point
                if (class.methods.get(self.init_string.?)) |initializer| {
                    return self.call(initializer.object.closure(), arg_count);
                } else if (arg_count != 0) {
                    return try self.throwRuntimeError("Expected 0 arguments but got {d}.", .{arg_count});
                }
            },
            .native => {
                const native = callee.object.native().function;
                const stack_size = self.stack.items.len;
                // NOTE: passing a slice into the stack instead of pointer to Value
                const result = native(self.stack.items[0..]);
                const stack_resize = stack_size - arg_count - 1;
                try self.stack.resize(stack_resize);
                try self.push(result);
            },
            else => try self.throwRuntimeError("Can only call functions and classes", .{}),
        };
    }

    fn invoke(self: *Self, name: *String, arg_count: Byte) Error!void {
        const receiver = self.peek(arg_count);
        // TODO: error check receiver? currently not since instance has an assert
        const instance = receiver.object.instance();
        if (instance.fields.get(name)) |closure| {
            self.stack.items[self.stack.items.len - arg_count - 1] = closure;
            return try self.callValue(closure, arg_count);
        }
        return try self.invokeFromClass(instance.class, name, arg_count);
    }

    fn invokeFromClass(self: *Self, class: *Class, name: *String, arg_count: Byte) Error!void {
        if (class.methods.get(name)) |method| {
            return self.call(method.object.closure(), arg_count);
        }
        return try self.throwRuntimeError("Undefined property '{s}'.", .{name.chars});
    }

    // REGION: upvalues
    fn captureUpvalue(self: *Self, local: *Value) Error!*Upvalue {
        var i: usize = 0;
        while (i < self.open_upvalues.items.len) : (i += 1) {
            const idx = self.open_upvalues.items.len - 1 - i;
            const uv = self.open_upvalues.items[idx];
            if (@intFromPtr(uv.location) < @intFromPtr(local)) {
                break;
            }
            if (@intFromPtr(uv.location) == @intFromPtr(local)) {
                return uv;
            }
        }

        const upvalue = try Upvalue.create(self, local);
        // GC, probably because using ArrayList instead of linkedlist
        try self.push(Value{ .object = &upvalue.obj });
        try self.open_upvalues.append(upvalue);
        _ = self.pop();
        return upvalue;
    }

    fn closeUpvalues(self: *Self, last: *const Value) void {
        var i: usize = 0;
        while (i < self.open_upvalues.items.len) : (i += 1) {
            const idx = self.open_upvalues.items.len - 1 - i;
            const uv = self.open_upvalues.items[idx];
            if (@intFromPtr(uv.location) < @intFromPtr(last)) {
                break;
            }
            uv.closed = uv.location.*;
            uv.location = &uv.closed;
        }
    }

    // REGION: classes/instances/methods
    fn bindMethod(self: *Self, class: *Class, name: *String) Error!void {
        if (class.methods.get(name)) |method| {
            const bound_method = try BoundMethod.create(self, self.peek(0), method.object.closure());
            _ = self.pop();
            try self.push(Value{ .object = &bound_method.obj });
        }
        // TODO: need error?
    }

    fn defineMethod(self: *Self, name: *String) Error!void {
        const method = self.peek(0);
        const class = self.peek(1).object.class();
        try class.methods.set(name, method);
        _ = self.pop();
    }

    // REGION: native

    fn defineNative(self: *Self, name: []const u8, function: Object.NativeFn) Error!void {
        const key = try String.copy(self, name);
        try self.push(Value{ .object = &key.obj });
        const value = try Object.Native.create(self, function);
        try self.push(Value{ .object = &value.obj });
        try self.globals.set(key, Value{ .object = &value.obj });
        _ = self.pop();
        _ = self.pop();
    }

    fn timestampNative(_: []Value) Value {
        // Using timestamp... time.h clock can be called after using @cImport, however I'm not sure how (or if possible) to cast the return from c_long to f64
        const ts = @as(f64, @floatFromInt(std.time.timestamp()));
        return Value{ .number = ts };
    }

    // REGION: error handling

    fn throwRuntimeError(self: *Self, comptime fmt: []const u8, args: anytype) Error!void {
        var buf = [_]u8{0} ** 1024;
        const message = try std.fmt.bufPrint(&buf, fmt, args);
        const frame = getCurrentFrame(self);
        const line = frame.closure.function.chunk.lines.items[frame.ip];
        try self.writer_err.print("[line {d}] runtime error: {s}\n", .{ line, message });

        var i: usize = 0;
        while (i < self.frames.items.len) : (i += 1) {
            const idx = self.frames.items.len - 1 - i;
            const stack_frame = self.frames.items[idx];
            std.debug.print("frame.ip = {d}, frame.function.chunk.count = {d}\n", .{ stack_frame.ip, stack_frame.closure.function.chunk.count() });
            const stack_line = stack_frame.closure.function.chunk.lines.items[stack_frame.ip - 1];
            if (idx != 0) {
                try self.writer_err.print("[line {d}] in {s}()\n", .{ stack_line, stack_frame.closure.function.name.chars });
            } else {
                try self.writer_err.print("[line {d}] in {s}\n", .{ stack_line, stack_frame.closure.function.name.chars });
            }
        }

        try self.reset();
        return InterpreterError.RuntimeError;
    }

    // REGION: run bytecode

    fn getCurrentFrame(self: *Self) *CallFrame {
        return &self.frames.items[self.frames.items.len - 1];
    }

    // could be on frame?
    fn readByte(self: *Self) Byte {
        var frame = self.getCurrentFrame();
        const byte = frame.closure.function.chunk.code.items[frame.ip];
        frame.ip += 1;
        return byte;
    }

    // could be on frame?
    fn readConstant(self: *Self) Value {
        const frame = self.getCurrentFrame();
        return frame.closure.function.chunk.constants.items[self.readByte()];
    }

    // could be on frame?
    fn readString(self: *Self) *String {
        const value = self.readConstant();
        return value.object.string();
    }

    fn readShort(self: *Self) Short {
        var frame = self.getCurrentFrame();
        frame.ip += 2;
        const short = @as(Short, frame.closure.function.chunk.code.items[frame.ip - 2]) << 8 | frame.closure.function.chunk.code.items[frame.ip - 1];
        return short;
    }

    fn binary(self: *Self, op: Chunk.OpCode) Error!void {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            return self.throwRuntimeError("Operands must be numbers.", .{});
        }
        const rhs = self.pop();
        const lhs = self.pop();
        switch (op) {
            .op_greater => try self.push(Value{ .boolean = lhs.number > rhs.number }),
            .op_less => try self.push(Value{ .boolean = lhs.number < rhs.number }),
            .op_subtract => try self.push(Value{ .number = lhs.number - rhs.number }),
            .op_multiply => try self.push(Value{ .number = lhs.number * rhs.number }),
            .op_divide => try self.push(Value{ .number = lhs.number / rhs.number }),
            else => return,
        }
    }

    fn concatenate(self: *Self) Error!void {
        const rhs = self.peek(0);
        const lhs = self.peek(1);
        assert(rhs.isObject());
        assert(lhs.isObject());
        switch (lhs.object.type) {
            .string => switch (rhs.object.type) {
                .string => {
                    const rhs_chars = rhs.object.string().chars;
                    const lhs_chars = lhs.object.string().chars;
                    const chars = try self.allocator.alloc(u8, rhs_chars.len + lhs_chars.len);
                    @memcpy(chars[0..lhs_chars.len], lhs_chars[0..lhs_chars.len]);
                    @memcpy(chars[lhs_chars.len..], rhs_chars[0..rhs_chars.len]);
                    _ = self.pop();
                    _ = self.pop();
                    const string_concat = try String.create(self, chars);
                    try self.push(Value{ .object = &string_concat.obj });
                    return;
                },
                else => {},
            },
            else => {},
        }
        try self.throwRuntimeError("Operands must be two strings", .{});
    }

    fn isFalsey(value: Value) bool {
        return value.isNil() or (value.isBoolean() and !value.boolean);
    }

    fn run(self: *Self) Error!void {
        assert(self.frames.items.len > 0);

        const writer = std.io.getStdErr().writer();

        if (common.DEBUG_TRACE_EXECUTION) {
            try writer.print(
                \\[
                \\  'action': 'trace'
                \\  'name': 'run'
                \\  'op': [
                \\
            , .{});
        }

        while (true) {
            const op: Chunk.OpCode = @enumFromInt(self.readByte());
            if (common.DEBUG_TRACE_EXECUTION) {
                // print stack
                try writer.print("    [ 'stack': [", .{});
                for (self.stack.items, 0..) |v, i| {
                    if (i == 0) {
                        try writer.print(" ", .{});
                        try v.print(writer);
                    } else {
                        try writer.print(", ", .{});
                        try v.print(writer);
                    }
                }
                try writer.print(" ] ]\n", .{});

                try writer.print("    [ 'open_upvalues': [", .{});
                for (self.open_upvalues.items, 0..) |v, i| {
                    if (i == 0) {
                        try writer.print(" ", .{});
                        try v.location.*.print(writer);
                    } else {
                        try writer.print(", ", .{});
                        try v.location.*.print(writer);
                    }
                }
                try writer.print(" ] ]\n", .{});

                // print op
                const frame = self.getCurrentFrame();
                // NOTE: frame.ip always one past current
                _ = try frame.closure.function.chunk.disassembleOp(frame.ip - 1, writer);
            }
            switch (op) {
                .op_constant => {
                    const constant = self.readConstant();
                    try self.push(constant);
                },
                .op_nil => try self.push(Value.nil()),
                .op_true => try self.push(Value{ .boolean = true }),
                .op_false => try self.push(Value{ .boolean = false }),
                .op_pop => {
                    _ = self.pop();
                },
                .op_get_local => {
                    const slot = self.readByte();
                    const frame = self.getCurrentFrame();
                    try self.push(self.stack.items[slot + frame.offset]);
                },
                .op_set_local => {
                    const slot = self.readByte();
                    const frame = self.getCurrentFrame();
                    self.stack.items[slot + frame.offset] = self.peek(0);
                },
                .op_get_global => {
                    const key = self.readString();
                    const value = self.globals.get(key);
                    if (value) |v| {
                        try self.push(v);
                    } else {
                        return try self.throwRuntimeError("Undefined variable '{s}'.", .{key.chars});
                    }
                },
                .op_set_global => {
                    const key = self.readString();
                    const value = self.peek(0);
                    self.globals.set(key, value) catch |e| {
                        std.debug.print("error? => {}", .{e});
                        return try self.throwRuntimeError("Undefined variable '{s}'.", .{key.chars});
                    };
                },
                .op_define_global => {
                    const key = self.readString();
                    try self.globals.set(key, self.peek(0));
                    _ = self.pop();
                },
                .op_get_upvalue => {
                    const frame = self.getCurrentFrame();
                    const slot = self.readByte();
                    assert(frame.closure.upvalues[slot] != null);
                    if (frame.closure.upvalues[slot]) |uv| {
                        const value = uv.location;
                        try self.push(value.*);
                    }
                },
                .op_set_upvalue => {
                    const frame = self.getCurrentFrame();
                    const slot = self.readByte();
                    frame.closure.upvalues[slot].?.location.* = self.peek(0);
                },
                .op_get_property => {
                    const instance = (self.peek(0)).object.instance();
                    const name = self.readString();
                    if (instance.fields.get(name)) |value| {
                        _ = self.pop();
                        try self.push(value);
                        continue;
                    }
                    try self.bindMethod(instance.class, name);
                },
                .op_set_property => {
                    const instance = self.peek(1).object.instance(); // NOTE: Will error on assert here
                    const name = self.readString();
                    try instance.fields.set(name, self.peek(0));
                    const value = self.pop();
                    _ = self.pop();
                    try self.push(value);
                },
                .op_get_super => {
                    const name = self.readString();
                    const superclass = self.pop().object.class();
                    try self.bindMethod(superclass, name);
                },
                .op_equal => {
                    const b = self.pop();
                    const a = self.pop();
                    try self.push(Value{ .boolean = Value.eql(a, b) });
                },
                .op_add => {
                    if (self.peek(0).isObject() and self.peek(1).isObject()) {
                        try self.concatenate();
                    } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        const rhs = self.pop();
                        const lhs = self.pop();
                        try self.push(Value{ .number = lhs.number + rhs.number });
                    } else {
                        return self.throwRuntimeError("Operands must be two numbers or two strings.", .{});
                    }
                },
                .op_greater, .op_less, .op_subtract, .op_multiply, .op_divide => try self.binary(op),
                .op_not => try self.push(Value{ .boolean = isFalsey(self.pop()) }),
                .op_negate => {
                    if (!self.peek(0).isNumber()) {
                        return InterpreterError.RuntimeError;
                    }
                    try self.push(Value{ .number = -self.pop().number });
                },
                .op_print => {
                    try self.pop().print(self.writer_out);
                    try self.writer_out.print("\n", .{});
                },
                .op_jump => {
                    const frame = self.getCurrentFrame();
                    const offset = self.readShort();
                    frame.ip += offset;
                },
                .op_jump_if_false => {
                    const offset = self.readShort();
                    if (isFalsey(self.peek(0))) {
                        const frame = self.getCurrentFrame();
                        frame.ip += offset;
                    }
                },
                .op_loop => {
                    const offset = self.readShort();
                    const frame = self.getCurrentFrame();
                    frame.ip -= offset;
                },
                .op_call => {
                    const arg_count = self.readByte();
                    try self.callValue(self.peek(arg_count), arg_count);
                    // frame popped in op_return
                },
                .op_invoke => {
                    const method = self.readString();
                    const arg_count = self.readByte();
                    try self.invoke(method, arg_count);
                    // frame popped in op_return
                },
                .op_super_invoke => {
                    const method = self.readString();
                    const arg_count = self.readByte();
                    const superclass = self.pop().object.class();
                    try self.invokeFromClass(superclass, method, arg_count);
                    // frame popped in op_return
                },
                .op_closure => {
                    const constant = self.readConstant();
                    assert(constant.isObject());
                    const func = constant.object.function();
                    const closure = try Closure.create(self, func);
                    try self.push(Value{ .object = &closure.obj });

                    const frame = self.getCurrentFrame();
                    var i: usize = 0;
                    while (i < func.upvalue_count) : (i += 1) {
                        const is_local = self.readByte();
                        const index = self.readByte();
                        if (is_local == 1) {
                            // NOTE: pointer can point to garbage if stack is resized... found when not using GC
                            const value_ptr = &self.stack.items[frame.offset + index];
                            closure.upvalues[i] = try self.captureUpvalue(value_ptr);
                        } else {
                            closure.upvalues[i] = frame.closure.upvalues[i];
                        }
                    }
                },
                .op_close_upvalue => {
                    // NOTE: seems like self.peek returned a copy of the Value
                    self.closeUpvalues(self.peekAddress(0));
                    _ = self.pop();
                },
                .op_return => {
                    const result = self.pop();
                    const frame = self.frames.pop();
                    // Should be start of frame
                    self.closeUpvalues(&self.stack.items[frame.offset]);
                    if (self.frames.items.len == 0) {
                        if (common.DEBUG_TRACE_EXECUTION) {
                            try writer.print(
                                \\  ]
                                \\]
                                \\
                            , .{});
                        }
                        return;
                    }
                    // not sure what to do with this... pop the stack or add stack top as a ptr?
                    try self.stack.resize(frame.offset);
                    try self.push(result);
                },
                .op_class => {
                    const name = self.readString();
                    const klass = try Class.create(self, name);
                    try self.push(Value{ .object = &klass.obj });
                },
                .op_inherit => {
                    // TODO: should assert class type, class has assert
                    const superclass = self.peek(1).object.class();
                    const class = self.peek(0).object.class();
                    var iter = superclass.methods.hashmap.iterator();
                    while (iter.next()) |kv| {
                        try class.methods.set(kv.key_ptr.*, kv.value_ptr.*);
                    }
                    _ = self.pop();
                },
                .op_method => {
                    try self.defineMethod(self.readString());
                },
                //else => unreachable,
            }
        }
    }
};

const testing = std.testing;
const FixtureVM = @import("test.zig").FixtureVM;

const InterpreterTest = struct {
    source: []const u8,
    expected: []const u8,
    expected_err: ?[]const u8 = null,
};

fn testInterpreter(test_: InterpreterTest) !void {
    const fixture = try FixtureVM.init();
    try fixture.setup();
    defer fixture.teardown();

    std.debug.print("- source\n{s}\n", .{test_.source});
    if (fixture.vm.interpret(test_.source)) |_| {} else |e| {
        std.debug.print("- interpret failed with error\n{any}\n", .{e});
    }

    try fixture.stdout.seekTo(0);
    const out = try fixture.stdout.readToEndAlloc(testing.allocator, std.math.maxInt(usize));
    defer testing.allocator.free(out);

    try fixture.stderr.seekTo(0);
    const err = try fixture.stderr.readToEndAlloc(testing.allocator, std.math.maxInt(usize));
    defer testing.allocator.free(err);
    if (err.len > 0) {
        std.debug.print("- errors detected\n{s}\n", .{err});
    }

    try testing.expectEqualStrings(test_.expected, out);
    if (test_.expected_err) |expected_err| {
        try testing.expectEqualStrings(expected_err, err);
    }
}

test "interpreter" {
    const tests_ = [_]InterpreterTest{
        // runtime error
        .{
            .source = (
                \\ "a" + 1;
            ),
            .expected = "",
            .expected_err = "[line 1] runtime error: Operands must be two numbers or two strings.\n[line 1] in __script__\n",
        },
        // runtime error with format
        .{
            .source = (
                \\ fun f() {
                \\   print x;
                \\ }
                \\ f();
            ),
            .expected = "",
            .expected_err = "[line 2] runtime error: Undefined variable 'x'.\n[line 2] in f()\n[line 4] in __script__\n",
        },
        // nil
        .{
            .source = "print nil;",
            .expected = "nil\n",
        },
        // eql
        .{
            .source = "print \"123\" == \"123\";",
            .expected = "true\n",
        },
        // concatenate
        .{
            .source = "print \"a\" + \"a\";",
            .expected = "aa\n",
        },
        // add
        .{
            .source = "print 1 + 2;",
            .expected = "3\n",
        },
        // subtract
        .{
            .source = "print 2 - -1;",
            .expected = "3\n",
        },
        // var (global)
        .{
            .source = "var a = (6 * 10) + 9; a = 420; print a;\n",
            .expected = "420\n",
        },
        // block
        .{
            .source = "{ print 1; }",
            .expected = "1\n",
        },
        // local
        .{
            .source = (
                \\ var x = 9;
                \\ {
                \\   var x = 6;
                \\   print x;
                \\ }
                \\ print x;
                \\
            ),
            .expected = "6\n9\n",
        },
        // local - compiler error
        .{
            .source = (
                \\ {
                \\   var x;
                \\   var x;
                \\ }
                \\
            ),
            .expected = "",
            .expected_err = "[line 3] Error: Already a variable with this name in this scope.\n",
        },
        // if/else
        .{
            .source = (
                \\ if (false) {
                \\   print true;
                \\ } else {
                \\   print false;
                \\ }
                \\
            ),
            .expected = "false\n",
        },
        // and
        .{
            .source = (
                \\ if (true and true) {
                \\   print true;
                \\ } else {
                \\   print false;
                \\ }
                \\
            ),
            .expected = "true\n",
        },
        // or
        .{
            .source = (
                \\ if (false or true) {
                \\   print true;
                \\ } else {
                \\   print false;
                \\ }
                \\
            ),
            .expected = "true\n",
        },
        // while
        .{ .source = (
            \\ var i = 0;
            \\ while (i < 3) {
            \\   i = i + 1;
            \\ }
            \\ print i;
        ), .expected = "3\n" },
        // for
        .{ .source = (
            \\ for (var i = 0; i < 3; i = i + 1) {
            \\   print i;
            \\ }
        ), .expected = "0\n1\n2\n" },
        // function
        .{ .source = (
            \\ print "start";
            \\ var a = 1;
            \\ fun x(y, z) {
            \\   print "6";
            \\   return y - z - 1;
            \\ }
            \\ print x;
            \\ print "end";
            \\ print x(11, a);
        ), .expected = "start\n<fn x>\nend\n6\n9\n" },
        // native
        .{
            .source = (
                \\ var ts = timestamp();
                \\ print ts - ts;
            ),
            .expected = "0\n",
        },
        // closure
        .{
            .source = (
                \\ var x = "global";
                \\ fun outer() {
                \\   var x = "outer";
                \\   fun inner() {
                \\     print x;
                \\   }
                \\   inner();
                \\ }
                \\ outer();
            ),
            .expected = "outer\n",
        },
        // closure - counter
        .{
            .source = (
                \\ fun counter() {
                \\   var n = 0;
                \\   fun inner() {
                \\     n = n + 1;
                \\     print n;
                \\   }
                \\   inner();
                \\ }
                \\ var c = counter;
                \\ counter();
            ),
            .expected = "1\n",
        },
        // closure - close over 1
        .{
            .source = (
                \\ var globalSet;
                \\ var globalGet;
                \\ fun main() {
                \\   var a = "initial";
                \\   fun set() { a = "updated"; }
                \\   fun get() { print a; }
                \\   globalSet = set;
                \\   globalGet = get;
                \\ }
                \\ main();
                \\ globalSet();
                \\ globalGet();
            ),
            .expected = "updated\n",
        },
        // closure - close over 2
        .{
            .source = (
                \\ {
                \\   var a = 1;
                \\   fun f() { print a; }
                \\   var b = 2;
                \\   fun g() { print b; }
                \\   var c = 3;
                \\   fun h() { print c; }
                \\   f(); g(); h();
                \\   var d = 3;
                \\   fun i() { print d; }
                \\   i();
                \\   fun inc() { c = c + 1; }
                \\   inc(); h(); i();
                \\ }
                \\
            ),
            .expected = "1\n2\n3\n3\n4\n3\n",
        },
        // class - declaration
        .{
            .source = (
                \\ class Brioche {}
                \\ print Brioche;
                \\
            ),
            .expected = "<class Brioche>\n",
        },
        // instance - declaration
        .{
            .source = (
                \\ class Brioche {}
                \\ print Brioche();
                \\
            ),
            .expected = "<instance Brioche>\n",
        },
        // instance - property
        .{
            .source = (
                \\ class Toast {}
                \\ var toast = Toast();
                \\ toast.jam = "grape";
                \\ print toast.jam;
            ),
            .expected = "grape\n",
        },
        // method
        .{
            .source = (
                \\ class Person {
                \\   init(name) {this.name = name;}
                \\   sayHello() {
                \\      print "hello, " + this.name;
                \\   }
                \\ }
                \\ var p = Person("bob");
                \\ p.sayHello();
            ),
            .expected = "hello, bob\n",
        },
        // subclass
        .{
            .source = (
                \\ class S {
                \\   init(name) {this.name = name;}
                \\   hi() {
                \\      print "hello, " + this.name;
                \\   }
                \\ }
                \\ class C < S {}
                \\ var p = C("bob");
                \\ p.hi();
            ),
            .expected = "hello, bob\n",
        },
        // subclass - super
        .{
            .source = (
                \\ class A {
                \\   method() {
                \\     print "A method";
                \\   }
                \\ }
                \\ class B < A {
                \\   method() {
                \\     print "B method";
                \\   }
                \\   test() {
                \\     super.method();
                \\   }
                \\ }
                \\ class C < B {}
                \\ C().test();
            ),
            .expected = "A method\n",
        },
    };
    for (tests_) |t| {
        std.debug.print("== new test ==\n", .{});
        try testInterpreter(t);
    }
}
