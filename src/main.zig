pub usingnamespace @import("raw_opcodes.zig");
pub usingnamespace @import("wasm.zig");
pub usingnamespace @import("hooks.zig");

// tests =======================================================================

const std = @import("std");
const wasm = @This();

test "nop" {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const func_name = "nop";
    const func = try module.function(ally, func_name, &.{}, &.{});
    try func.op(ally, .nop);

    const bytecode = try module.compile(ally);
    defer ally.free(bytecode);

    const results = try wasm.callExport(ally, bytecode, func_name, &.{});
    defer ally.free(results);

    try std.testing.expectEqualSlices(wasm.Value, &.{}, results);
}

test "sum" {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const func_name = "sum";
    const func = try module.function(
        ally,
        func_name,
        &.{ .i32, .i32 },
        &.{.i32},
    );
    try func.op(ally, .{ .@"local.get" = func.param(0) });
    try func.op(ally, .{ .@"local.get" = func.param(1) });
    try func.op(ally, .{ .add = .i32 });

    const bytecode = try module.compile(ally);
    defer ally.free(bytecode);

    const params = [_]wasm.Value{
        .{ .i32 = 42 },
        .{ .i32 = 69 },
    };

    const expected = [_]wasm.Value{
        .{ .i32 = 111 },
    };

    const results = try wasm.callExport(ally, bytecode, func_name, &params);
    defer ally.free(results);

    try std.testing.expectEqualSlices(wasm.Value, &expected, results);
}