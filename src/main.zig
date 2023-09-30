pub usingnamespace @import("wasm.zig");
pub usingnamespace @import("hooks.zig");

// tests =======================================================================

const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
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
                const tagname = @tagName(tag);
                var op = @unionInit(wasm.Op, tagname, undefined);

                if (@TypeOf(@field(op, tagname)) != wasm.Type) {
                    unreachable;
                }

                @field(op, tagname) = t;

                break :o op;
            },
        };
        try entry.op(ally, bin_op);

        // save bytecode for debugging
        const bytecode = try module.compile(ally);
        defer ally.free(bytecode);

        var buf: [64]u8 = undefined;
        const wasm_name = try std.fmt.bufPrint(
            &buf,
            "{s}_{s}.wasm",
            .{
                @tagName(self.opcode),
                @tagName(self.cases[0].out),
            },
        );

        const dir_name = "compiled_tests";

        try std.fs.cwd().makePath(dir_name);
        const out_dir = try std.fs.cwd().openDir(dir_name, .{});
        try out_dir.writeFile(wasm_name, bytecode);

        // run cases
        for (self.cases) |case| {
            const res = try wasm.callExport(
                ally,
                bytecode,
                func_name,
                &case.in,
            );
            defer ally.free(res);

            const actual = res[0];

            if (!actual.eql(case.out)) {
                try stderr.print(
                    \\[error in test case]
                    \\called:   {s}({}, {})
                    \\got:      {}
                    \\expected: {}
                    \\
                    \\
                    ,
                    .{
                        @tagName(self.opcode),
                        case.in[0],
                        case.in[1],
                        actual,
                        case.out,
                    },
                );

                return error.TestUnexpectedResult;
            }
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
    };

    for (suites) |s| try s.run(ally);
}

test "ifElse" {
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