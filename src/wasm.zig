const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const wasm3 = @import("wasm3");
const write_wasm = @import("write_wasm.zig");

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

    /// compile a module to an allocated array of bytes
    pub fn compile(
        self: *const Self,
        ally: Allocator,
    ) Allocator.Error![]const u8 {
        var code = std.ArrayList(u8).init(ally);
        defer code.deinit();

        try self.writeWasm(code.writer());

        return try code.toOwnedSlice();
    }

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

        // local params
        var locals = Function.LocalMap{};

        const param_locals = try ally.alloc(Local, params.len);
        for (param_locals, params) |*slot, param_type| {
            slot.* = try locals.put(ally, param_type);
        }

        // name
        const owned_name: ?[:0]const u8 = if (name) |got| x: {
            const slice = try ally.allocSentinel(u8, got.len, 0);
            @memcpy(slice, got);
            slice[slice.len] = 0;
            break :x slice;
        } else null;

        // entry flow
        var flows = Function.FlowMap{};

        const entry_ref = try flows.new(ally);
        flows.set(entry_ref, .{ .ref = entry_ref });

        try self.functions.set(ally, ref, Function{
            .ref = ref,
            .name = owned_name,
            .params = param_locals,
            .returns = try ally.dupe(Type, returns),
            .locals = locals,
            .flows = flows,
        });

        return self.functions.get(ref);
    }
};

/// initialized with Module
pub const Function = struct {
    const Self = @This();
    const LocalMap = com.RefList(Local, Type);
    const FlowMap = com.RefList(FlowRef, Flow);

    ref: FuncRef,
    /// if this isn't null, this function will be exported with this name
    name: ?[:0]const u8,
    params: []const Local,
    returns: []const Type,
    locals: LocalMap,
    flows: FlowMap,

    fn deinit(self: *Self, ally: Allocator) void {
        if (self.name) |name| ally.free(name);
        ally.free(self.params);
        ally.free(self.returns);
        self.locals.deinit(ally);
        var flow_iter = self.flows.iterator();
        while (flow_iter.next()) |fl| fl.deinit(ally);
        self.flows.deinit(ally);
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

    /// retrieve the entry flow
    pub fn entry(self: *const Self) *Flow {
        return self.flows.get(.{ .index = 0 });
    }

    /// start a new flow
    pub fn flow(self: *Self, ally: Allocator) Allocator.Error!*Flow {
        const ref = try self.flows.new(ally);
        self.flows.set(ref, .{ .ref = ref });
        return self.flows.get(ref);
    }
};

/// use this to add ops to a function
pub const Flow = struct {
    const Self = @This();

    ref: FlowRef,
    ops: std.ArrayListUnmanaged(Op) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.ops.deinit(ally);
    }

    pub fn op(self: *Self, ally: Allocator, o: Op) Allocator.Error!void {
        try self.ops.append(ally, o);
    }
};
