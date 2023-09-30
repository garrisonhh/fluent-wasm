const std = @import("std");
const Allocator = std.mem.Allocator;
const wasm = @import("wasm.zig");
const wasm3 = @import("wasm3");

pub const CallExportError = wasm3.Function.CallAllocError;

/// load some wasm bytecode and call an exported function
pub fn callExport(
    ally: Allocator,
    bytecode: []const u8,
    name: [:0]const u8,
    params: []const wasm.Value,
) CallExportError![]const wasm.Value {
    const env = wasm3.Environment.init();
    defer env.deinit();

    const runtime = wasm3.Runtime.init(env, .{});
    defer runtime.deinit();

    var module = try wasm3.Module.parse(env, bytecode);
    defer module.deinit();

    try runtime.load(&module);

    const func = try runtime.findFunction(name);
    return try func.callAlloc(ally, params);
}
