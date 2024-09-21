const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const HashMap = std.HashMap;

const String = @import("object.zig").Object.String;
const Value = @import("value.zig").Value;

/// Table using Zig's HashMap as the backend. Expect for getKey(), could directly use HashMap to replace everything
/// Going to keep the style of the HashMap API's since it seems more intuitive for me
pub const Table = struct {
    const Self = @This();

    const Error = Allocator.Error;

    const Context = struct {
        /// FNV1a hash returning u64 for *Object.String
        /// Could just use std.hash.Fnv1a_64.hash
        /// REPLACES: hashString (object.c)
        pub fn hash(_: Context, key: *String) u64 {
            var hash_: u64 = 14695981039346656037;
            for (key.chars) |c| {
                hash_ ^= c;
                hash_ = @mulWithOverflow(hash_, 1099511628211)[0];
            }
            return hash_;
        }

        pub fn eql(_: Context, a: *String, b: *String) bool {
            return mem.eql(u8, a.chars, b.chars);
        }
    };

    const BackingHashMap = HashMap(*String, Value, Context, 75);
    const BackingEntry = BackingHashMap.Entry;

    hashmap: BackingHashMap,

    pub fn init(allocator: Allocator) Self {
        return .{
            .hashmap = HashMap(*String, Value, Context, 75).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.hashmap.deinit();
    }

    /// REPLACES: tableDelete (table.c)
    pub fn delete(self: *Self, key: *String) bool {
        return self.hashmap.remove(key);
    }

    /// REPLACES: tableFindString (table.c)
    pub fn getKey(self: Self, chars: []const u8) ?*String {
        var iter = self.hashmap.keyIterator();
        while (iter.next()) |key| {
            if (mem.eql(u8, key.*.chars, chars)) {
                return key.*;
            }
        }
        return null;
    }

    /// REPLACES: tableGet (table.c)
    pub fn get(self: Self, key: *String) ?Value {
        return self.hashmap.get(key);
    }

    /// REPLACES: tableSet (table.c)
    pub fn set(self: *Self, key: *String, value: Value) Error!void {
        return self.hashmap.put(key, value);
    }
};

const testing = std.testing;
const FixtureVM = @import("test.zig").FixtureVM;

test "fnv1a hash" {
    const fixture = try FixtureVM.init();
    try fixture.setup();
    defer fixture.teardown();
    const vm = &fixture.vm;

    const s = try String.copy(vm, "test");

    try testing.expectEqual(std.hash.Fnv1a_64.hash("test"), (Table.Context{}).hash(s));
}

test "table" {
    const fixture = try FixtureVM.init();
    try fixture.setup();
    defer fixture.teardown();
    var table = Table.init(testing.allocator);
    defer table.deinit();
    const vm = &fixture.vm;

    // set
    const k = try String.copy(vm, "test");
    const v = Value{ .object = &k.obj };
    try table.set(k, v);

    // getkey
    if (table.getKey("test")) |getk| {
        try testing.expectEqual(k, getk);
        try testing.expectEqualStrings(k.chars, getk.chars);
    } else {
        try testing.expect(false);
    }

    // get
    const getv = table.get(k) orelse Value.nil();
    switch (getv) {
        .object => try testing.expectEqual(v, getv),
        else => try testing.expect(false),
    }

    // delete
    const is_deleted = table.delete(k);
    try testing.expect(is_deleted);
}
