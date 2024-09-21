const std = @import("std");
const assert = std.debug.assert;

const Obj = @import("object.zig").Object.Obj;

pub const Value = union(enum) {
    const Self = @This();

    boolean: bool,
    nil: void,
    number: f64,
    object: *Obj,

    pub fn nil() Self {
        return .{ .nil = undefined };
    }

    pub fn isBoolean(self: Self) bool {
        return switch (self) {
            .boolean => true,
            else => false,
        };
    }

    pub fn isNil(self: Self) bool {
        return switch (self) {
            .nil => true,
            else => false,
        };
    }

    pub fn isNumber(self: Self) bool {
        return switch (self) {
            .number => true,
            else => false,
        };
    }

    pub fn isObject(self: Self) bool {
        return switch (self) {
            .object => true,
            else => false,
        };
    }

    pub fn eql(a: Self, b: Self) bool {
        return switch (a) {
            .boolean => switch (b) {
                .boolean => a.boolean == b.boolean,
                else => false,
            },
            .nil => switch (b) {
                .nil => true,
                else => false,
            },
            .number => switch (b) {
                .number => a.number == b.number,
                else => false,
            },
            .object => switch (b) {
                .object => Obj.eql(a.object, b.object),
                else => false,
            },
        };
    }

    pub fn print(self: Self, writer: anytype) !void {
        // using anytype... curious if this is a valid usecase instead of passing std.fs.File.Writer
        switch (self) {
            .boolean => |v| try writer.print("{}", .{v}),
            .number => |v| try writer.print("{d}", .{v}),
            .nil => try writer.print("nil", .{}),
            .object => |o| try o.print(writer),
        }
    }
};

const testing = std.testing;

test "eql" {
    const a = Value{ .number = 1 };
    const b = Value{ .number = 1 };
    const c = Value{ .number = 2 };

    try testing.expectEqual(true, Value.eql(a, b));
    try testing.expectEqual(false, Value.eql(a, c));
}
