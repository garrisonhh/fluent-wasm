const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const wasm = @import("wasm.zig");
const Local = wasm.Local;
const Type = wasm.Type;
const Value = wasm.Value;
const FuncRef = wasm.FuncRef;
const FlowRef = wasm.FlowRef;
const Op = wasm.Op;
const write_wasm = @import("write_wasm.zig");

fn addSentinel(ally: Allocator, str: []const u8) Allocator.Error![:0]const u8 {
    const slice = try ally.allocSentinel(u8, str.len, 0);
    @memcpy(slice, str);
    slice[slice.len] = 0;
    return slice;
}

pub const Import = struct {
    const Self = @This();

    ref: FuncRef,
    module: [:0]const u8,
    name: [:0]const u8,
    params: []const Type,
    returns: []const Type,

    fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.module);
        ally.free(self.name);
        ally.free(self.params);
        ally.free(self.returns);
    }
};

pub const Export = struct {
    const Self = @This();

    ref: FuncRef,
    name: [:0]const u8,

    fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.name);
    }
};

pub const Module = struct {
    const Self = @This();

    const FuncEntry = union(enum) {
        function: Function,
        import: Import,
    };

    const FuncRegistry = com.RefMap(FuncRef, FuncEntry);

    /// owns the function/import data
    registry: FuncRegistry = .{},
    functions: std.AutoHashMapUnmanaged(FuncRef, *Function) = .{},
    imports: std.AutoHashMapUnmanaged(FuncRef, *const Import) = .{},
    exports: std.AutoHashMapUnmanaged(FuncRef, Export) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        var entries = self.registry.iterator();
        while (entries.next()) |entry| {
            switch (entry.*) {
                .function => |*func| func.deinit(ally),
                .import => |imp| imp.deinit(ally),
            }
        }

        self.registry.deinit(ally);
        self.functions.deinit(ally);
        self.imports.deinit(ally);

        var export_iter = self.exports.valueIterator();
        while (export_iter.next()) |exp| exp.deinit(ally);
        self.exports.deinit(ally);
    }

    /// compile a module to an allocated array of bytes
    pub fn compile(
        self: *const Self,
        ally: Allocator,
    ) Allocator.Error![]const u8 {
        return try write_wasm.write(self, ally);
    }

    /// supply a native function definition to be imported from your zig code
    pub fn import(
        self: *Self,
        ally: Allocator,
        module: []const u8,
        name: []const u8,
        params: []const Type,
        returns: []const Type,
    ) Allocator.Error!*const Import {
        const ref = try self.registry.new(ally);

        const final = Import{
            .ref = ref,
            .module = try addSentinel(ally, module),
            .name = try addSentinel(ally, name),
            .params = try ally.dupe(Type, params),
            .returns = try ally.dupe(Type, returns),
        };

        try self.registry.set(ally, ref, .{ .import = final });

        const ptr = &self.registry.get(ref).import;
        try self.imports.put(ally, ref, ptr);

        return ptr;
    }

    /// create a new function
    /// supply a name if this function should be exported
    pub fn function(
        self: *Self,
        ally: Allocator,
        exported_name: ?[]const u8,
        params: []const Type,
        returns: []const Type,
    ) Allocator.Error!*Function {
        // local params
        var locals = Function.LocalMap{};

        const param_locals = try ally.alloc(Local, params.len);
        for (param_locals, params) |*slot, param_type| {
            slot.* = try locals.put(ally, param_type);
        }

        // entry flow
        var flows = Function.FlowMap{};

        const entry_ref = try flows.new(ally);
        flows.set(entry_ref, .{ .ref = entry_ref });

        const ref = try self.registry.new(ally);

        const final = Function{
            .ref = ref,
            .params = param_locals,
            .returns = try ally.dupe(Type, returns),
            .locals = locals,
            .flows = flows,
        };

        try self.registry.set(ally, ref, .{ .function = final });

        const ptr = &self.registry.get(ref).function;
        try self.functions.put(ally, ref, ptr);

        // export
        if (exported_name) |name| {
            const owned = try addSentinel(ally, name);
            try self.exports.put(ally, ref, Export{
                .ref = ref,
                .name = owned,
            });
        }

        return ptr;
    }
};

/// initialized with Module
pub const Function = struct {
    const Self = @This();
    const LocalMap = com.RefList(Local, Type);
    const FlowMap = com.RefList(FlowRef, Flow);

    ref: FuncRef,
    params: []const Local,
    returns: []const Type,
    locals: LocalMap,
    flows: FlowMap,

    fn deinit(self: *Self, ally: Allocator) void {
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