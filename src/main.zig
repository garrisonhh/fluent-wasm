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

    const entry = func.entry();
    try entry.op(ally, .nop);

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

    const entry = func.entry();
    try entry.op(ally, .{ .@"local.get" = func.param(0) });
    try entry.op(ally, .{ .@"local.get" = func.param(1) });
    try entry.op(ally, .{ .add = .i32 });

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

test "if-else" {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const func_name = "testfn";
    const func = try module.function(
        ally,
        func_name,
        &.{ .i32, .i32, .i32 },
        &.{.i32},
    );

    const if_true = try func.flow(ally);
    try if_true.op(ally, .{ .@"local.get" = func.param(1) });

    const if_false = try func.flow(ally);
    try if_false.op(ally, .{ .@"local.get" = func.param(2) });

    const entry = func.entry();
    try entry.op(ally, .{ .@"local.get" = func.param(0) });
    try entry.op(ally, .{
        .@"if" = .{
            .type = .i32,
            .if_true = if_true.ref,
            .if_false = if_false.ref,
        },
    });

    const bytecode = try module.compile(ally);
    defer ally.free(bytecode);

    {
        const params = [_]wasm.Value{
            .{ .i32 = 0 },
            .{ .i32 = 82 },
            .{ .i32 = 420 },
        };

        const expected = [_]wasm.Value{
            .{ .i32 = 420 },
        };

        const results = try wasm.callExport(ally, bytecode, func_name, &params);
        defer ally.free(results);

        try std.testing.expectEqualSlices(wasm.Value, &expected, results);
    }

    {
        const params = [_]wasm.Value{
            .{ .i32 = 1 },
            .{ .i32 = 82 },
            .{ .i32 = 420 },
        };

        const expected = [_]wasm.Value{
            .{ .i32 = 82 },
        };

        const results = try wasm.callExport(ally, bytecode, func_name, &params);
        defer ally.free(results);

        try std.testing.expectEqualSlices(wasm.Value, &expected, results);
    }
}