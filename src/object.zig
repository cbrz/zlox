const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Chunk = @import("chunk.zig").Chunk;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Object = struct {
    pub const Error = Allocator.Error;

    pub const Type = enum {
        bound_method,
        class,
        closure,
        function,
        instance,
        native,
        string,
        upvalue,
    };

    /// Obj containing pointer to each parent
    pub const Obj = struct {
        type: Type,
        ctx: *anyopaque,
        marked: bool,
        next: ?*Obj,

        pub fn init(ctx: *anyopaque, type_: Type) Obj {
            return .{
                .type = type_,
                .ctx = ctx,
                .marked = false,
                .next = null,
            };
        }

        fn appendToVm(self: *Obj, vm: *VM) void {
            self.next = vm.objects;
            vm.objects = self;
        }

        /// REPLACES: freeObject (object.c)
        pub fn destroy(self: Obj, vm: *VM) void {
            switch (self.type) {
                .bound_method => self.boundMethod().destroy(vm),
                .class => self.class().destroy(vm),
                .closure => self.closure().destroy(vm),
                .function => self.function().destroy(vm),
                .instance => self.instance().destroy(vm),
                .native => self.native().destroy(vm),
                .string => self.string().destroy(vm),
                .upvalue => self.upvalue().destroy(vm),
            }
        }

        pub fn eql(a: *Obj, b: *Obj) bool {
            return a == b;
        }

        pub fn print(self: Obj, writer: anytype) !void {
            switch (self.type) {
                .bound_method => try self.boundMethod().print(writer),
                .class => try self.class().print(writer),
                .closure => try self.closure().print(writer),
                .function => try self.function().print(writer),
                .instance => try self.instance().print(writer),
                .native => try self.native().print(writer),
                .string => try self.string().print(writer),
                .upvalue => try self.upvalue().print(writer),
            }
        }

        // Cast Type

        pub fn boundMethod(self: Obj) *BoundMethod {
            assert(@as(Type, self.type) == .bound_method);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn class(self: Obj) *Class {
            assert(@as(Type, self.type) == .class);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn closure(self: Obj) *Closure {
            assert(@as(Type, self.type) == .closure);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn function(self: Obj) *Function {
            assert(@as(Type, self.type) == .function);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn instance(self: Obj) *Instance {
            assert(@as(Type, self.type) == .instance);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn native(self: Obj) *Native {
            assert(@as(Type, self.type) == .native);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn string(self: Obj) *String {
            assert(@as(Type, self.type) == .string);
            return @ptrCast(@alignCast(self.ctx));
        }

        pub fn upvalue(self: Obj) *Upvalue {
            assert(@as(Type, self.type) == .upvalue);
            return @ptrCast(@alignCast(self.ctx));
        }
    };

    pub const BoundMethod = struct {
        obj: Obj,
        receiver: Value,
        method: *Closure,

        pub fn create(vm: *VM, receiver: Value, method: *Closure) Error!*BoundMethod {
            const self = try vm.allocator.create(BoundMethod);
            self.* = .{
                .obj = Obj.init(self, .bound_method),
                .receiver = receiver,
                .method = method,
            };
            self.obj.appendToVm(vm);
            return self;
        }
        pub fn destroy(self: *BoundMethod, vm: *VM) void {
            vm.allocator.destroy(self);
        }
        pub fn print(self: *BoundMethod, writer: anytype) !void {
            return self.method.function.print(writer);
        }
    };

    pub const Class = struct {
        obj: Obj,
        methods: Table,
        name: *String,

        pub fn create(vm: *VM, name: *String) Error!*Class {
            const self = try vm.allocator.create(Class);
            self.* = .{
                .obj = Obj.init(self, Type.class),
                .methods = Table.init(vm.allocator),
                .name = name,
            };
            self.obj.appendToVm(vm);
            return self;
        }

        pub fn destroy(self: *Class, vm: *VM) void {
            self.methods.deinit();
            vm.allocator.destroy(self);
        }

        pub fn print(self: *Class, writer: anytype) !void {
            try writer.print("<class {s}>", .{self.name.chars});
        }
    };

    pub const Closure = struct {
        obj: Obj,
        function: *Function,
        // TODO: try replace with ArrayList
        upvalues: []?*Upvalue, // are these safe as pointers?

        /// REPLACES: newClosure (object.c)
        pub fn create(vm: *VM, func: *Function) Error!*Closure {
            const upvalues = try vm.allocator.alloc(?*Upvalue, func.upvalue_count);
            for (upvalues) |*uv| uv.* = null;

            const self = try vm.allocator.create(Closure);
            self.* = .{
                .obj = Obj.init(self, Type.closure),
                .function = func,
                .upvalues = upvalues,
            };
            self.obj.appendToVm(vm);
            return self;
        }

        pub fn destroy(self: *Closure, vm: *VM) void {
            // NOTE: function not owned
            vm.allocator.free(self.upvalues);
            vm.allocator.destroy(self);
        }

        pub fn print(self: *Closure, writer: anytype) !void {
            try self.function.print(writer);
        }
    };

    pub const Function = struct {
        obj: Obj,
        arity: u32,
        upvalue_count: u32, // probably should be limited to BYTE_MAX
        chunk: Chunk,
        name: *String,

        /// REPLACES: newFunction (object.c)
        pub fn create(vm: *VM, name: *String) Error!*Function {
            const self = try vm.allocator.create(Function);
            self.* = .{
                .obj = Obj.init(self, .function),
                .chunk = Chunk.init(vm.allocator),
                .arity = 0,
                .upvalue_count = 0,
                .name = name,
            };
            self.obj.appendToVm(vm);
            return self;
        }

        pub fn destroy(self: *Function, vm: *VM) void {
            // NOTE: name not owned
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }

        pub fn print(self: *Function, writer: anytype) !void {
            try writer.print("<fn {s}>", .{self.name.chars});
        }
    };

    pub const Instance = struct {
        obj: Obj,
        class: *Class,
        fields: Table,

        pub fn create(vm: *VM, class: *Class) Error!*Instance {
            const self = try vm.allocator.create(Instance);
            self.* = .{
                .obj = Obj.init(self, .instance),
                .class = class,
                .fields = Table.init(vm.allocator),
            };
            self.obj.appendToVm(vm);
            return self;
        }

        pub fn destroy(self: *Instance, vm: *VM) void {
            self.fields.deinit();
            vm.allocator.destroy(self);
        }

        pub fn print(self: *Instance, writer: anytype) !void {
            try writer.print("<instance {s}>", .{self.class.name.chars});
        }
    };

    pub const NativeFn = *const fn ([]Value) Value;

    pub const Native = struct {
        obj: Obj,
        function: NativeFn,

        /// REPLACES: newNative (object.c)
        pub fn create(vm: *VM, function: NativeFn) Error!*Native {
            const self = try vm.allocator.create(Native);
            self.* = .{
                .obj = Obj.init(self, .native),
                .function = function,
            };
            self.obj.appendToVm(vm);
            return self;
        }

        pub fn destroy(self: *Native, vm: *VM) void {
            vm.allocator.destroy(self);
        }

        pub fn print(_: *Native, writer: anytype) !void {
            try writer.print("<native fn>", .{});
        }
    };

    pub const String = struct {
        obj: Obj,
        chars: []const u8,

        /// REPLACES: takeString (object.c)
        pub fn create(vm: *VM, chars: []const u8) Error!*String {
            // assert vm is initialized?
            if (vm.strings.getKey(chars)) |interned| {
                vm.allocator.free(chars);
                return interned;
            }
            const self = try vm.allocator.create(String);
            self.* = .{
                .obj = Obj.init(self, .string),
                .chars = chars,
            };
            self.obj.appendToVm(vm);
            // GC
            try vm.push(Value{ .object = &self.obj });
            try vm.strings.set(self, Value.nil());
            _ = vm.pop();

            return self;
        }

        /// REPLACES: copyString (object.c)
        pub fn copy(vm: *VM, chars: []const u8) Error!*String {
            const chars_copy = try vm.allocator.alloc(u8, chars.len);
            @memcpy(chars_copy, chars);
            return create(vm, chars_copy);
        }

        pub fn destroy(self: *String, vm: *VM) void {
            vm.allocator.free(self.chars);
            vm.allocator.destroy(self);
        }

        pub fn print(self: *String, writer: anytype) !void {
            try writer.print("{s}", .{self.chars});
        }
    };

    pub const Upvalue = struct {
        obj: Obj,
        closed: Value,
        location: *Value,
        next: ?*Upvalue,

        /// REPLACES: newUpvalue (object.c)
        pub fn create(vm: *VM, slot: *Value) Error!*Upvalue {
            const self = try vm.allocator.create(Upvalue);
            self.* = .{
                .obj = Obj.init(self, .upvalue),
                .location = slot,
                .closed = Value.nil(),
                .next = null,
            };
            self.obj.appendToVm(vm);
            return self;
        }

        pub fn destroy(self: *Upvalue, vm: *VM) void {
            vm.allocator.destroy(self);
        }

        pub fn print(_: Upvalue, writer: anytype) !void {
            try writer.print("<upvalue>", .{});
        }
    };
};

const testing = std.testing;
const FixtureVM = @import("test.zig").FixtureVM;

test "Obj accesses parent" {
    const fixture = try FixtureVM.init();
    try fixture.setup();
    defer fixture.teardown();
    var vm = &fixture.vm;

    const s1 = try Object.String.copy(vm, "parent");
    try testing.expectEqual(Object.Type.string, s1.obj.type);
    try testing.expectEqual(&s1.obj, vm.objects.?);

    const s2 = vm.objects.?.string();
    try testing.expectEqualStrings(s1.chars, s2.chars);
    try testing.expectEqual(s1.chars, s2.chars);

    const f1 = try Object.Function.create(vm, s1);
    const f2 = vm.objects.?.function();
    try testing.expectEqual(f1, f2);
}
