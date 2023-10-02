const std = @import("std");
const com = @import("common");
const wasm3 = @import("wasm3");

pub const Global = com.Ref(.wasm_global, 32);
pub const Local = com.Ref(.wasm_local, 32);
pub const FuncRef = com.Ref(.wasm_funcref, 32);
pub const FlowRef = com.Ref(.wasm_controlflow, 32);

pub const Type = wasm3.ValueType;
pub const Value = wasm3.Value;

const IntBackingType = @typeInfo(Type).Enum.tag_type;

pub const IntType = enum(IntBackingType) {
    i32 = @intFromEnum(Type.i32),
    i64 = @intFromEnum(Type.i64),

    pub fn from(t: Type) ?IntType {
        return switch (t) {
            .i32 => .i32,
            .i64 => .i64,
            else => null,
        };
    }

    pub fn into(self: IntType) Type {
        return @enumFromInt(@intFromEnum(self));
    }
};

pub const FloatType = enum(IntBackingType) {
    f32 = @intFromEnum(Type.f32),
    f64 = @intFromEnum(Type.f64),

    pub fn from(t: Type) ?FloatType {
        return switch (t) {
            .f32 => .f32,
            .f64 => .f64,
            else => null,
        };
    }

    pub fn into(self: FloatType) Type {
        return @enumFromInt(@intFromEnum(self));
    }
};

pub const Bytes = enum {
    @"8",
    @"16",
    @"32",
    @"64",

    pub fn sizeOf(t: Type) Bytes {
        return switch (t) {
            .i32, .f32 => .@"32",
            .i64, .f64 => .@"64",
        };
    }

    pub fn format(
        self: Bytes,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}", .{@tagName(self)});
    }
};

pub const Op = union(enum) {
    const Self = @This();
    pub const Code = std.meta.Tag(Self);

    pub const IntOp = struct {
        signed: std.builtin.Signedness,
        type: Type,
    };

    pub const If = struct {
        /// TODO represent function type sigs?
        type: ?Type,
        if_true: FlowRef,
        if_false: ?FlowRef,
    };

    // basic
    @"unreachable",
    nop,
    @"if": If,
    // block,
    // loop,
    // br,
    // br_if,
    // br_table,
    @"return",
    call: FuncRef,
    // call_indirect,
    // drop,
    // select,

    // intrinsic
    // memory_size,
    // memory_grow: u32,

    // vars
    local_get: Local,
    local_set: Local,
    local_tee: Local,
    global_get: Global,
    global_set: Global,

    // value manipulation
    @"const": Value,
    // load,
    // store,

    // comparisons
    eq: Type,
    ne: Type,
    lt: Type,
    gt: Type,
    le: Type,
    ge: Type,

    // arithmetic
    add: Type,
    sub: Type,
    mul: Type,

    // logic
    @"and": IntType,
    @"or": IntType,
    xor: IntType,

    // bit twiddling
    // ctz: Local,
    // clz: Local,
    // popcnt: Local,
    // shl: Local,
    // shr: Local,
    // rotl: Local,
    // rotr: Local,

    // int-specific
    eqz: IntType,
    // idiv: IntOp,
    // irem: IntOp,

    // float-specific
    // fdiv: FloatType,
    // frem: FloatType,
    // abs: FloatType,
    // neg: FloatType,
    // ceil: FloatType,
    // floor: FloatType,
    // nearest: FloatType,
    // sqrt: FloatType,
    // min: FloatType,
    // max: FloatType,
    // copysign: FloatType,

    // conversions
    // /// as lossless as possible between reprs
    // convert,
    // /// bitcast
    // reinterpret,
};

