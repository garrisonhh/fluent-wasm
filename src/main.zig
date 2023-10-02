pub usingnamespace @import("wasm.zig");
pub usingnamespace @import("hooks.zig");
pub usingnamespace @import("builders.zig");

// tests =======================================================================

const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const wasm = @This();

const save_dir = "compiled_tests";

fn saveBytecode(
    bytecode: []const u8,
    comptime name_fmt: []const u8,
    name_args: anytype,
) !void {
    const ally = std.testing.allocator;
    const name = try std.fmt.allocPrint(ally, name_fmt ++ ".wasm", name_args);
    defer ally.free(name);

    const cwd = std.fs.cwd();
    try cwd.makePath(save_dir);
    const dir = try cwd.openDir(save_dir, .{});
    try dir.writeFile(name, bytecode);
}

/// compile module, save bytecode with name, and return bytecode
fn compileAndSave(
    ally: Allocator,
    module: *const wasm.Module,
    comptime name_fmt: []const u8,
    name_args: anytype,
) ![]const u8 {
    const bytecode = try module.compile(ally);
    try saveBytecode(bytecode, name_fmt, name_args);

    return bytecode;
}

fn valuesEql(a: []const wasm.Value, b: []const wasm.Value) bool {
    if (a.len != b.len) return false;

    for (a, b) |ea, eb| {
        if (!ea.eql(eb)) return false;
    }

    return true;
}

const FormattableValues = struct {
    values: []const wasm.Value,

    pub fn format(
        self: @This(),
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void{
        try writer.writeByte('[');

        for (self.values, 0..) |value, i| {
            if (i > 0) try writer.writeAll(", ");
            try value.format(&.{}, .{}, writer);
        }

        try writer.writeByte(']');
    }
};

fn valuesFmt(values: []const wasm.Value) FormattableValues {
    return .{ .values = values };
}

fn expectWasmResults(
    func_name: [:0]const u8,
    params: []const wasm.Value,
    actual: []const wasm.Value,
    expected: []const wasm.Value,
) !void {
    if (valuesEql(expected, actual)) return;

    try stderr.print(
        \\[error in test case]
        \\called:   {s} {}
        \\actual:   {}
        \\expected: {}
        \\
        \\
    ,
        .{
            func_name,
            valuesFmt(params),
            valuesFmt(actual),
            valuesFmt(expected),
        },
    );
}

/// loads bytecode, calls the function, and expects that the results will be the
/// same as provided
fn expectCall(
    ally: Allocator,
    hooker: wasm.Hooker,
    func_name: [:0]const u8,
    params: []const wasm.Value,
    expected: []const wasm.Value,
) !void {
    const results = try hooker.call(ally, func_name, params);
    defer ally.free(results);

    try expectWasmResults(func_name, params, results, expected);
}

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

    var hooker = try wasm.Hooker.init(bytecode, &.{});
    defer hooker.deinit();

    try expectCall(ally, hooker, func_name, &.{}, &.{});
}

const HomogenousBinaryOpTest = struct {
    const Self = @This();

    const Case = struct {
        in: [2]wasm.Value,
        out: wasm.Value,

        fn init(
            comptime t: wasm.Type,
            in0: t.zigType(),
            in1: t.zigType(),
            out: t.zigType(),
        ) Case {
            return Case{
                .in = .{
                    @unionInit(wasm.Value, @tagName(t), in0),
                    @unionInit(wasm.Value, @tagName(t), in1),
                },
                .out = @unionInit(wasm.Value, @tagName(t), out),
            };
        }
    };

    opcode: wasm.Op.Code,
    cases: []const Case,

    fn init(opcode: wasm.Op.Code, cases: []const Case) Self {
        return Self{
            .opcode = opcode,
            .cases = cases,
        };
    }

    fn run(self: Self, ally: Allocator) !void {
        var module = wasm.Module{};
        defer module.deinit(ally);

        // make function
        const func_name = @tagName(self.opcode);
        const t = @as(wasm.Type, self.cases[0].out);
        const func = try module.function(ally, func_name, &.{ t, t }, &.{t});

        const entry = func.entry();
        try entry.op(ally, .{ .local_get = func.param(0) });
        try entry.op(ally, .{ .local_get = func.param(1) });

        const bin_op = switch (self.opcode) {
            inline else => |tag| o: {
                const OpFieldEnum = std.meta.FieldEnum(wasm.Op);
                const field_tag = comptime std.enums.nameCast(OpFieldEnum, tag);
                const field_name = @tagName(tag);

                break :o switch (std.meta.FieldType(wasm.Op, field_tag)) {
                    wasm.Type => @unionInit(wasm.Op, field_name, t),

                    wasm.IntType,
                    wasm.FloatType,
                    => |T| @unionInit(wasm.Op, field_name, T.from(t).?),

                    else => unreachable,
                };
            },
        };
        try entry.op(ally, bin_op);

        const bytecode = try compileAndSave(ally, &module, "{s}_{s}", .{
            @tagName(self.opcode),
            @tagName(self.cases[0].out),
        });
        defer ally.free(bytecode);

        var hooker = try wasm.Hooker.init(bytecode, &.{});
        defer hooker.deinit();

        for (self.cases) |case| {
            try expectCall(ally, hooker, func_name, &case.in, &.{ case.out });
        }
    }
};

test HomogenousBinaryOpTest {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const suite = HomogenousBinaryOpTest.init;
    const case = HomogenousBinaryOpTest.Case.init;

    const max_i32 = std.math.maxInt(i32);
    const min_i32 = std.math.minInt(i32);
    const max_i64 = std.math.maxInt(i64);
    const min_i64 = std.math.minInt(i64);

    // NOTE wasm3 executes an illegal instruction on integer overflow. I suppose
    // this is undefined behavior in the c language, maybe I can zig translate-c
    // the offending function in wasm3.zig?

    const suites = [_]HomogenousBinaryOpTest{
        suite(.add, &.{
            case(.i32, 0, 0, 0),
            case(.i32, 1, 2, 3),
            case(.i32, max_i32, 0, max_i32),
            case(.i32, max_i32 - 1, 1, max_i32),
            case(.i32, max_i32, min_i32, -1),
            case(.i32, min_i32, 0, min_i32),
        }),
        suite(.sub, &.{
            case(.i32, 0, 0, 0),
            case(.i32, 1, 2, -1),
            case(.i32, max_i32, max_i32, 0),
            case(.i32, min_i32, min_i32, 0),
        }),
        suite(.mul, &.{
            case(.i32, 0, 0, 0),
            case(.i32, 3, 4, 12),
            case(.i32, 5, -2, -10),
            case(.i32, max_i32, 1, max_i32),
            case(.i32, 1, min_i32, min_i32),
            case(.i32, min_i32 / 2, 2, min_i32),
        }),
        suite(.@"and", &.{
            case(.i32, 0, 0, 0),
            case(.i32, 0, 1, 0),
            case(.i32, 1, 0, 0),
            case(.i32, 1, 1, 1),
            case(.i32, 0b01, 0b10, 0b00),
            case(.i32, 0b11, 0b10, 0b10),
        }),
        suite(.@"or", &.{
            case(.i32, 0, 0, 0),
            case(.i32, 0, 1, 1),
            case(.i32, 1, 0, 1),
            case(.i32, 1, 1, 1),
            case(.i32, 0b01, 0b10, 0b11),
            case(.i32, 0b00, 0b10, 0b10),
        }),
        suite(.xor, &.{
            case(.i32, 0, 0, 0),
            case(.i32, 0, 1, 1),
            case(.i32, 1, 0, 1),
            case(.i32, 1, 1, 0),
            case(.i32, 0b01, 0b10, 0b11),
            case(.i32, 0b00, 0b10, 0b10),
            case(.i32, 0b11, 0b10, 0b01),
        }),

        suite(.add, &.{
            case(.i64, 0, 0, 0),
            case(.i64, 1, 2, 3),
            case(.i64, max_i64, 0, max_i64),
            case(.i64, max_i64 - 1, 1, max_i64),
            case(.i64, max_i64, min_i64, -1),
            case(.i64, min_i64, 0, min_i64),
        }),
        suite(.sub, &.{
            case(.i64, 0, 0, 0),
            case(.i64, 1, 2, -1),
            case(.i64, max_i64, max_i64, 0),
            case(.i64, min_i64, min_i64, 0),
        }),
        suite(.mul, &.{
            case(.i64, 0, 0, 0),
            case(.i64, 3, 4, 12),
            case(.i64, 5, -2, -10),
            case(.i64, max_i64, 1, max_i64),
            case(.i64, 1, min_i64, min_i64),
            case(.i64, min_i64 / 2, 2, min_i64),
        }),
        suite(.@"and", &.{
            case(.i64, 0, 0, 0),
            case(.i64, 0, 1, 0),
            case(.i64, 1, 0, 0),
            case(.i64, 1, 1, 1),
            case(.i64, 0b01, 0b10, 0b00),
            case(.i64, 0b11, 0b10, 0b10),
        }),
        suite(.@"or", &.{
            case(.i64, 0, 0, 0),
            case(.i64, 0, 1, 1),
            case(.i64, 1, 0, 1),
            case(.i64, 1, 1, 1),
            case(.i64, 0b01, 0b10, 0b11),
            case(.i64, 0b00, 0b10, 0b10),
        }),
        suite(.xor, &.{
            case(.i64, 0, 0, 0),
            case(.i64, 0, 1, 1),
            case(.i64, 1, 0, 1),
            case(.i64, 1, 1, 0),
            case(.i64, 0b01, 0b10, 0b11),
            case(.i64, 0b00, 0b10, 0b10),
            case(.i64, 0b11, 0b10, 0b01),
        }),
    };

    for (suites) |s| try s.run(ally);
}

test "ifElse" {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const func_name = "if_else";
    const func = try module.function(
        ally,
        func_name,
        &.{ .i32, .i32, .i32 },
        &.{.i32},
    );

    const if_true = try func.flow(ally);
    try if_true.op(ally, .{ .local_get = func.param(1) });

    const if_false = try func.flow(ally);
    try if_false.op(ally, .{ .local_get = func.param(2) });

    const entry = func.entry();
    try entry.op(ally, .{ .local_get = func.param(0) });
    try entry.op(ally, .{
        .@"if" = .{
            .type = .i32,
            .if_true = if_true.ref,
            .if_false = if_false.ref,
        },
    });

    const bytecode = try compileAndSave(ally, &module, "{s}", .{func_name});
    defer ally.free(bytecode);

    var hooker = try wasm.Hooker.init(bytecode, &.{});
    defer hooker.deinit();

    try expectCall(
        ally,
        hooker,
        func_name,
        &[_]wasm.Value{
            .{ .i32 = 0 },
            .{ .i32 = 82 },
            .{ .i32 = 420 },
        },
        &[_]wasm.Value{
            .{ .i32 = 420 },
        },
    );

    try expectCall(
        ally,
        hooker,
        func_name,
        &[_]wasm.Value{
            .{ .i32 = 1 },
            .{ .i32 = 82 },
            .{ .i32 = 420 },
        },
        &[_]wasm.Value{
            .{ .i32 = 82 },
        },
    );
}

test "fibonacci" {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const func_name = "fibonacci";
    const func = try module.function(ally, func_name, &.{.i64}, &.{.i64});

    const entry = func.entry();

    // $0 == 0 || $0 == 1
    try entry.op(ally, .{ .local_get = func.param(0) });
    try entry.op(ally, .{ .eqz = .i64 });
    try entry.op(ally, .{ .local_get = func.param(0) });
    try entry.op(ally, .{ .@"const" = .{ .i64 = 1 } });
    try entry.op(ally, .{ .eq = .i64 });
    try entry.op(ally, .{ .@"or" = .i32 });

    // true => 1
    const if_true = try func.flow(ally);

    try if_true.op(ally, .{ .@"const" = .{ .i64 = 1 } });

    // false => fibonacci($0 - 1) + fibonacci($0 - 2)
    const if_false = try func.flow(ally);

    try if_false.op(ally, .{ .local_get = func.param(0) });
    try if_false.op(ally, .{ .@"const" = .{ .i64 = 1 } });
    try if_false.op(ally, .{ .sub = .i64 });
    try if_false.op(ally, .{ .call = func.ref });

    try if_false.op(ally, .{ .local_get = func.param(0) });
    try if_false.op(ally, .{ .@"const" = .{ .i64 = 2 } });
    try if_false.op(ally, .{ .sub = .i64 });
    try if_false.op(ally, .{ .call = func.ref });

    try if_false.op(ally, .{ .add = .i64 });

    try entry.op(ally, .{
        .@"if" = .{
            .type = .i64,
            .if_true = if_true.ref,
            .if_false = if_false.ref,
        },
    });

    const bytecode = try compileAndSave(ally, &module, "{s}", .{func_name});
    defer ally.free(bytecode);

    var hooker = try wasm.Hooker.init(bytecode, &.{});
    defer hooker.deinit();

    const fibonacci = [_]i64{ 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89 };

    for (fibonacci, 0..) |n, i| {
        try wasm.expectCall(
            ally,
            hooker,
            func_name,
            &[_]wasm.Value{.{ .i64 = @intCast(i) }},
            &[_]wasm.Value{.{ .i64 = n }},
        );
    }
}

var toggled: bool = false;

fn toggle(_: wasm.Runtime, _: []const wasm.Value, _: []wasm.Value) void {
    toggled = !toggled;
}

test "callNative" {
    const ally = std.testing.allocator;

    var module = wasm.Module{};
    defer module.deinit(ally);

    const native = try module.import(ally, "zig", "toggle", &.{}, &.{});

    const func_name = "call_native";
    const func = try module.function(ally, func_name, &.{}, &.{});

    const entry = func.entry();
    try entry.op(ally, .{ .call = native.ref });

    const bytecode = try compileAndSave(ally, &module, "{s}", .{func_name});
    defer ally.free(bytecode);

    var hooker = wasm.Hooker.init(bytecode, &.{
        .{ .meta = native, .ptr = &toggle },
    }) catch |e| std.debug.panic("err: {}\n", .{e});
    defer hooker.deinit();

    try std.testing.expect(!toggled);
    ally.free(try hooker.call(ally, func_name, &.{}));
    try std.testing.expect(toggled);
    ally.free(try hooker.call(ally, func_name, &.{}));
    try std.testing.expect(!toggled);
}
