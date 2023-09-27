const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const write_wasm = @import("write_wasm.zig");

pub const Global = com.Ref(.wasm_global, 32);
pub const Local = com.Ref(.wasm_local, 32);
pub const FuncRef = com.Ref(.wasm_funcref, 32);
pub const FlowRef = com.Ref(.wasm_controlflow, 32);

pub const Bytes = enum {
    @"8",
    @"16",
    @"32",
    @"64",

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}", .{@tagName(self)});
    }
};

pub const Type = enum {
    i32,
    i64,
    u32,
    u64,
    f32,
    f64,
    funcref,

    pub fn bits(self: @This()) Bytes {
        return switch (self) {
            .i32, .u32, .f32 => .@"32",
            .i64, .u64, .f64, .funcref => .@"64",
        };
    }
};

pub const Value = union(Type) {
    i32: i32,
    i64: i64,
    u32: u32,
    u64: u64,
    f32: f32,
    f64: f64,
    funcref: FuncRef,
};

pub const Op = union(enum) {
    const Self = @This();

    pub const Load = struct {
        dst: Local,
        bytes: Bytes,
    };

    pub const Store = struct {
        src: Local,
        bytes: Bytes,
    };

    pub const Oneway = struct {
        dst: Local,
        src: Local,
    };

    // basic
    @"unreachable",
    nop,
    block,
    // loop,
    @"if",
    @"else",
    end,
    // br,
    // br_if,
    // br_table,
    @"return",
    // call,
    // call_indirect,
    // drop,
    // select,

    // intrinsic
    @"memory.size",
    @"memory.grow": u32,

    // vars
    @"local.get": Local,
    @"local.set": Local,
    @"local.tee": Local,
    @"global.get": Global,
    @"global.set": Global,

    // value manipulation
    @"const": Value,
    load: Load,
    store: Store,

    // comparison operators
    eqz: Type,
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
    rem: Type,

    // logic
    @"and",
    @"or",
    xor,

    // bit twiddling
    // ctz: Local,
    // clz: Local,
    // popcnt: Local,
    // shl: Local,
    // shr: Local,
    // rotl: Local,
    // rotr: Local,

    // float-specific
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
    /// as lossless as possible
    convert: Oneway,
    reinterpret: Oneway,
};

pub const Module = struct {
    const Self = @This();
    const FunctionMap = com.RefMap(FuncRef, Function);

    functions: FunctionMap = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        var func_iter = self.functions.iterator();
        while (func_iter.next()) |func| func.deinit(ally);
        self.functions.deinit(ally);
    }

    pub const writeWasm = write_wasm.write;

    /// create a new function
    /// supply a name if this function should be exported
    pub fn function(
        self: *Self,
        ally: Allocator,
        name: ?[]const u8,
        params: []const Type,
        returns: []const Type,
    ) Allocator.Error!*Function {
        const ref = try self.functions.new(ally);

        var locals = Function.LocalMap{};

        const param_locals = try ally.alloc(Local, params.len);
        for (param_locals, params) |*slot, param_type| {
            slot.* = try locals.put(ally, param_type);
        }

        try self.functions.set(ally, ref, Function{
            .ref = ref,
            .name = if (name) |got| try ally.dupe(u8, got) else null,
            .params = param_locals,
            .returns = try ally.dupe(Type, returns),
            .locals = locals,
        });

        return self.functions.get(ref);
    }
};

/// initialized with Module
pub const Function = struct {
    const Self = @This();
    const LocalMap = com.RefList(Local, Type);

    ref: FuncRef,
    /// if this isn't null, this function will be exported with this name
    name: ?[]const u8,
    params: []const Local,
    returns: []const Type,
    locals: LocalMap,
    ops: std.ArrayListUnmanaged(Op) = .{},

    fn deinit(self: *Self, ally: Allocator) void {
        if (self.name) |name| ally.free(name);
        ally.free(self.params);
        ally.free(self.returns);
        self.locals.deinit(ally);
        self.ops.deinit(ally);
    }

    pub fn param(self: *const Self, index: usize) Local {
        return self.params[index];
    }

    pub fn local(self: *Self, ally: Allocator, t: Type) Allocator.Error!Local {
        return try self.locals.put(ally, t);
    }

    pub fn typeOf(self: *const Self, ref: Local) Type {
        return self.locals.get(ref).*;
    }

    pub fn op(self: *Self, ally: Allocator, o: Op) Allocator.Error!void {
        try self.ops.append(ally, o);
    }
};
