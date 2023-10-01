const std = @import("std");
const Allocator = std.mem.Allocator;
const wasm = @import("wasm.zig");
const wasm3 = @import("wasm3");

pub const NativeFunction = wasm3.RawFunction;

pub const NativeImport = struct {
    meta: *const wasm.Module.Import,
    ptr: *const NativeFunction,
};

pub const WasmError = wasm3.Error;
/// context for loading and running wasm code
pub const Hooker = struct {
    const Self = @This();

    env: wasm3.Environment,
    runtime: wasm3.Runtime,
    module: wasm3.Module,

    pub fn init(
        bytecode: []const u8,
        imports: []const NativeImport,
    ) WasmError!Self {
        const env = wasm3.Environment.init();
        const runtime = wasm3.Runtime.init(env, .{});
        var module = try wasm3.Module.parse(env, bytecode);

        try runtime.load(&module);

        for (imports) |import| {
            const meta = import.meta;
            try module.linkRawFunction(
                meta.module,
                meta.name,
                meta.params,
                meta.returns,
                import.ptr,
            );
        }

        return Self{
            .env = env,
            .runtime = runtime,
            .module = module,
        };
    }

    pub fn deinit(self: *Self) void {
        self.module.deinit();
        self.runtime.deinit();
        self.env.deinit();
    }

    pub const CallError = wasm3.Function.CallAllocError;

    /// call an exported function
    pub fn call(
        self: Self,
        ally: Allocator,
        name: [:0]const u8,
        params: []const wasm.Value,
    ) CallError![]const wasm.Value {
        const func = try self.runtime.findFunction(name);
        return try func.callAlloc(ally, params);
    }
};
