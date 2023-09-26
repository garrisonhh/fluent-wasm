const std = @import("std");
const RawOpcode = @import("bytecode.zig").RawOpcode;

pub const Type = enum {
    i32,
    i64,
    u32,
    u64,
    f32,
    f64,
};

/// signedness in types doesn't technically exist in wasm, but encoding them as
/// such drastically improves the readability of opcodes
pub const IntType = enum {
    i32,
    i64,
    u32,
    u64,

    pub fn into(it: IntType) Type {
        return switch (it) {
            inline else => |tag| std.enums.nameCast(Type, tag),
        };
    }
};

pub const FloatType = enum {
    i32,
    i64,

    pub fn into(ft: FloatType) Type {
        return switch (ft) {
            inline else => |tag| std.enums.nameCast(Type, tag),
        };
    }
};

// TODO must encode as higher level:
// i32.clz
// i32.ctz
// i32.popcnt
// i32.shl
// i32.shr_s
// i32.shr_u
// i32.rotl
// i32.rotr
// i64.clz
// i64.ctz
// i64.popcnt
// i64.shl
// i64.shr_s
// i64.shr_u
// i64.rotl
// i64.rotr
// f32.abs
// f32.neg
// f32.ceil
// f32.floor
// f32.nearest
// f32.sqrt
// f32.min
// f32.max
// f32.copysign
// f64.abs
// f64.neg
// f64.ceil
// f64.floor
// f64.nearest
// f64.sqrt
// f64.min
// f64.max
// f64.copysign

pub const Op = union(enum) {
    const Self = @This();

    pub const Bytes = enum { @"8", @"16", @"32", @"64" };

    pub const ILoad = struct {
        type: IntType,
        bytes: Bytes,
    };

    pub const IStore = struct {
        type: IntType,
        bytes: Bytes,
    };

    pub const Convert = struct {
        type: Type,
        into: Type,
    };

    pub const Reinterpret = struct {
        type: Type,
        into: Type,
    };

    // basic
    @"unreachable",
    nop,
    block,
    loop,
    @"if",
    @"else",
    end,
    br,
    br_if,
    br_table,
    @"return",
    call,
    call_indirect,
    drop,
    select,

    // intrinsic
    @"memory.size",
    @"memory.grow",

    // vars
    @"local.get",
    @"local.set",
    @"local.tee",
    @"global.get",
    @"global.set",

    // value manipulation
    @"const": Type,
    load: Type,
    store: Type,
    /// partial int load
    iload: ILoad,
    /// partial int store
    istore: IStore,

    // comparison operators
    eqz: IntType,
    eq: Type,
    ne: Type,
    lt: Type,
    gt: Type,
    le: Type,
    ge: Type,

    // arithmetic operators
    add: Type,
    sub: Type,
    mul: Type,
    div: Type,
    rem: IntType,

    // logic
    @"and": IntType,
    @"or": IntType,
    @"xor": IntType,

    // conversions
    /// as lossless as possible
    convert: Convert,
    reinterpret: Reinterpret,

    pub fn into(self: Self,) RawOpcode {
        return switch (self) {
            inline .@"unreachable",
            .nop,
            .block,
            .loop,
            .@"if",
            .@"else",
            .end,
            .br,
            .br_if,
            .br_table,
            .@"return",
            .call,
            .call_indirect,
            .drop,
            .select,
            .@"memory.size",
            .@"memory.grow",
            .@"local.get",
            .@"local.set",
            .@"local.tee",
            .@"global.get",
            .@"global.set",
            => |tag| std.enums.nameCast(RawOpcode, tag),

            else => |tag| std.debug.panic("TODO into() for {}\n", .{tag}),
        };
    }
};