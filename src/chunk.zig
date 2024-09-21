const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const common = @import("common.zig");
const Byte = common.Byte;
const BYTE_MAX = common.BYTE_MAX;
const Line = common.Line;
const Value = @import("value.zig").Value;

pub const Chunk = struct {
    const Error = Allocator.Error;

    pub const OpCode = enum(Byte) {
        op_constant,
        op_nil,
        op_true,
        op_false,

        op_pop,
        op_get_local,
        op_set_local,
        op_get_global,
        op_set_global,
        op_get_upvalue,
        op_set_upvalue,
        op_get_property,
        op_set_property,
        op_get_super,
        op_define_global,

        op_equal,
        op_greater,
        op_less,

        op_add,
        op_subtract,
        op_multiply,
        op_divide,
        op_not,

        op_negate,

        op_print,

        op_jump,
        op_jump_if_false,
        op_loop,
        op_call,
        op_invoke,
        op_super_invoke,
        op_closure,
        op_close_upvalue,
        op_return,

        op_class,
        op_inherit,
        op_method,
    };

    const Self = @This();

    code: ArrayList(Byte),
    lines: ArrayList(Line),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Self {
        return .{
            .code = ArrayList(Byte).init(allocator),
            .lines = ArrayList(Line).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn count(self: Self) usize {
        assert(self.code.items.len == self.lines.items.len);
        return self.code.items.len;
    }

    /// REPLACES: writeChunk (chunk.c)
    pub fn write(self: *Self, byte: Byte, line: Line) Error!void {
        assert(self.code.items.len == self.lines.items.len);
        try self.code.append(byte);
        try self.lines.append(line);
    }

    pub fn writeOp(self: *Self, op: OpCode, line: Line) Error!void {
        return try self.write(@intFromEnum(op), line);
    }

    /// REPLACES: addConstant (chunk.c)
    pub fn appendConstant(self: *Self, value: Value) Error!Byte {
        assert(self.constants.items.len < BYTE_MAX - 1); // cannot exceed byte size or we won't be able to get the value by index
        try self.constants.append(value);
        return @intCast(self.constants.items.len - 1);
    }

    /// REPLACES: disassembleChunk (debug.c)
    pub fn disassemble(self: Self, comptime name: []const u8) !void {
        const writer = std.io.getStdErr().writer();

        try writer.print(
            \\[
            \\  'action': 'disassemble'
            \\  'name': '{s}'
            \\  'code': [
            \\
        , .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = try self.disassembleOp(offset, writer);
        }

        try writer.print("    [ 'constants': [", .{});
        for (self.constants.items, 0..) |v, i| {
            if (i == 0) {
                try writer.print(" ", .{});
                try v.print(writer);
            } else {
                try writer.print(", ", .{});
                try v.print(writer);
            }
        }
        try writer.print(" ] ]\n", .{});

        try writer.print(
            \\  ]
            \\]
            \\
        , .{});
    }

    /// REPLACES: disassembleOp (debug.c)
    pub fn disassembleOp(self: Self, offset: usize, writer: anytype) !usize {
        const op: OpCode = @enumFromInt(self.code.items[offset]);
        return switch (op) {
            .op_nil, .op_true, .op_false, .op_pop, .op_equal, .op_greater, .op_less, .op_add, .op_subtract, .op_multiply, .op_divide, .op_not, .op_negate, .op_print, .op_close_upvalue, .op_return, .op_inherit => try self.disassembleSimple(op, offset, writer),

            .op_constant, .op_get_global, .op_set_global, .op_define_global, .op_get_property, .op_set_property, .op_get_super, .op_class, .op_method => try self.disassembleConstant(op, offset, writer),

            .op_get_local, .op_set_local, .op_get_upvalue, .op_set_upvalue, .op_call => try self.disassembleByte(op, offset, writer),

            .op_invoke, .op_super_invoke => try self.disassembleInvoke(op, offset, writer),

            .op_jump, .op_jump_if_false => try self.disassembleJump(op, offset, 1, writer),
            .op_loop => try self.disassembleJump(op, offset, -1, writer),

            .op_closure => {
                var offset_closure = offset;
                offset_closure += 1;
                const index = self.code.items[offset_closure];
                try writer.print("    [ 'op': '{}', 'offset': {d}, 'value': '", .{ op, offset_closure });
                const value = self.constants.items[index];
                try value.print(writer);

                try writer.print("', 'upvalues': [ ", .{});
                const func = value.object.function();
                var i: usize = 0;
                while (i < func.upvalue_count) : (i += 1) {
                    const uv_is_local = self.code.items[offset_closure + 1];
                    const uv_index = self.code.items[offset_closure + 2];
                    switch (i) {
                        0 => try writer.print("[ 'offset': {}, 'is_local': {}, 'index': {d} ]", .{ offset_closure + 1, uv_is_local, uv_index }),
                        else => try writer.print(", [ 'offset': {}, 'is_local': {}, 'index': {d} ]", .{ offset_closure + 1, uv_is_local, uv_index }),
                    }
                    offset_closure += 2;
                }
                try writer.print(" ] ]\n", .{});
                return offset_closure + 1;
            },

            // else => unreachable,
        };
    }

    /// REPLACES: byteInstruction
    fn disassembleByte(self: Self, op: OpCode, offset: usize, writer: anytype) !usize {
        const slot = self.code.items[offset + 1];
        try writer.print("    [ 'op': '{}', 'offset': {d}, 'byte': {d} ]\n", .{ op, offset, slot });
        return offset + 2;
    }

    /// REPLACES: constantInstruction
    fn disassembleConstant(self: Self, op: OpCode, offset: usize, writer: anytype) !usize {
        const index = self.code.items[offset + 1];
        const value = self.constants.items[index];
        try writer.print("    [ 'op': '{}', 'offset': {d}, 'index', {d}, 'value': '", .{ op, offset, index });
        try value.print(writer);
        try writer.print("' ]\n", .{});
        return offset + 2;
    }

    fn disassembleInvoke(self: Self, op: OpCode, offset: usize, writer: anytype) !usize {
        const index = self.code.items[offset + 1];
        const value = self.constants.items[index];
        const arg_count = self.code.items[offset + 2];
        try writer.print("    [ 'op': '{}', 'offset': {d}, 'index', {d}, 'value': '", .{ op, offset, index });
        try value.print(writer);
        try writer.print("', 'arg_count': {d} ]\n", .{arg_count});
        return offset + 3;
    }

    fn disassembleJump(self: Self, op: OpCode, offset: usize, sign: isize, writer: anytype) !usize {
        const jump: common.Short = @as(common.Short, self.code.items[offset + 1]) << 8 | self.code.items[offset + 2];
        try writer.print("    [ 'op': '{}', 'offset': {d}, 'jump': {d} ]\n", .{ op, offset, @as(isize, @intCast(offset + 3)) + (sign * jump) });
        return offset + 3;
    }

    /// REPLACES: simpleInstruction
    fn disassembleSimple(_: Self, op: OpCode, offset: usize, writer: anytype) !usize {
        try writer.print("    [ 'op': '{}', 'offset': {d} ]\n", .{ op, offset });
        return offset + 1;
    }
};

const testing = std.testing;

test {
    const chunk = Chunk.init(testing.allocator);
    defer chunk.deinit();
}
