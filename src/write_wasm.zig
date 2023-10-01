//! https://webassembly.github.io/spec/core/binary/index.html

const std = @import("std");
const builtin = @import("builtin");
const RawOpcode = std.wasm.Opcode;
const wasm = @import("wasm.zig");
const Module = wasm.Module;
const Function = wasm.Function;
const Op = wasm.Op;
const Local = wasm.Local;
const FlowRef = wasm.FlowRef;

const wasm_magic_bytes: [4]u8 = .{0} ++ "asm".*;
const wasm_version: u32 = 0x1;

const functype: u8 = 0x60;
const blocktype_empty: u8 = 0x40;

const Section = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    @"export" = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
};

const ValType = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
    v128 = 0x7B,
    funcref = 0x70,
    externref = 0x6F,

    fn ofWasmType(t: wasm.Type) ValType {
        return std.meta.stringToEnum(ValType, @tagName(t)).?;
    }
};

const LinkDesc = enum(u8) {
    func = 0,
    table = 1,
    mem = 2,
    global = 3,
};

// utils =======================================================================

/// determines impl by signedness
fn writeInt(
    comptime Int: type,
    value: Int,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const info = @typeInfo(Int).Int;
    switch (info.signedness) {
        .signed => try std.leb.writeILEB128(writer, value),
        .unsigned => try std.leb.writeULEB128(writer, value),
    }
}

/// write float in little-endian byte order
fn writeFloat(
    comptime F: type,
    value: F,
    writer: anytype,
) @TypeOf(writer).Error!void {
    std.debug.assert(@typeInfo(F) == .Float);

    const UInt = std.meta.Int(.unsigned, @bitSizeOf(F));
    var buf: [@sizeOf(F)]u8 = undefined;
    std.mem.writeIntLittle(UInt, &buf, @as(UInt, @bitCast(value)));

    try writer.writeAll(&buf);
}

fn writeEnum(
    comptime E: type,
    value: E,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.writeByte(@intFromEnum(value));
}

fn writeName(name: []const u8, writer: anytype) @TypeOf(writer).Error!void {
    try writeInt(u32, @as(u32, @intCast(name.len)), writer);
    try writer.writeAll(name);
}

fn writeSectionHeader(
    section: Section,
    size: u32,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writeEnum(Section, section, writer);
    try writeInt(u32, size, writer);
}

/// wraps a section impl in a function that handles the section header and the
/// vec count
fn writeVecSection(
    module: *const Module,
    count: u32,
    writer: anytype,
    section: Section,
    comptime wrapped: fn (
        *const Module,
        writer: anytype,
    ) @TypeOf(writer).Error!void,
) @TypeOf(writer).Error!void {
    if (count == 0) return;

    var counter = std.io.countingWriter(std.io.null_writer);
    try writeInt(u32, count, counter.writer());
    try wrapped(module, counter.writer());

    try writeSectionHeader(section, @intCast(counter.bytes_written), writer);
    try writeInt(u32, count, writer);
    try wrapped(module, writer);
}

/// calculates the number of functions that aren't imports
fn countFunctions(module: *const Module) u32 {
    var count: u32 = 0;
    var func_iter = module.functions.iterator();
    while (func_iter.next()) |anyfunc| {
        if (anyfunc.* == .function) {
            count += 1;
        }
    }

    return count;
}

fn countImports(module: *const Module) u32 {
    var count: u32 = 0;
    var func_iter = module.functions.iterator();
    while (func_iter.next()) |anyfunc| {
        if (anyfunc.* == .import) {
            count += 1;
        }
    }

    return count;
}

fn countExports(module: *const Module) u32 {
    var num_exports: u32 = 0;

    var func_iter = module.functions.iterator();
    while (func_iter.next()) |anyfunc| {
        switch (anyfunc.*) {
            .function => |func| {
                if (func.name) |_| {
                    num_exports += 1;
                }
            },
            else => {},
        }
    }

    return num_exports;
}

// =============================================================================

/// writes function types as their index
fn writeTypes(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var func_iter = module.functions.iterator();
    while (func_iter.next()) |anyfunc| {
        try writer.writeByte(functype);

        switch (anyfunc.*) {
            .function => |func| {
                try writeInt(u32, @as(u32, @intCast(func.params.len)), writer);
                for (func.params) |param| {
                    const vt = ValType.ofWasmType(func.typeOf(param));
                    try writeEnum(ValType, vt, writer);
                }
            },
            .import => |imp| {
                try writeInt(u32, @as(u32, @intCast(imp.params.len)), writer);
                for (imp.params) |param| {
                    try writeEnum(ValType, ValType.ofWasmType(param), writer);
                }
            },
        }

        const returns = switch (anyfunc.*) {
            inline else => |x| x.returns,
        };

        try writeInt(u32, @as(u32, @intCast(returns.len)), writer);
        for (returns) |t| {
            try writeEnum(ValType, ValType.ofWasmType(t), writer);
        }
    }
}

fn writeImports(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var funcs = module.functions.iterator();
    while (funcs.next()) |anyfunc| {
        switch (anyfunc.*) {
            .import => |meta| {
                try writeName(meta.module, writer);
                try writeName(meta.name, writer);
                try writeEnum(LinkDesc, .func, writer);
                try writeInt(u32, meta.ref.index, writer);
            },
            else => {},
        }
    }
}

/// just maps functions to their indexed type
fn writeFunctions(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var funcs = module.functions.iterator();
    while (funcs.next()) |anyfunc| {
        switch (anyfunc.*) {
            inline .function, .import => |meta| {
                try writeInt(u32, meta.ref.index, writer);
            },
        }
    }
}

fn writeExports(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var func_iter = module.functions.iterator();
    while (func_iter.next()) |anyfunc| {
        switch (anyfunc.*) {
            .function => |func| {
                if (func.name) |name| {
                    try writeName(name, writer);
                    try writeEnum(LinkDesc, .func, writer);
                    try writeInt(u32, func.ref.index, writer);
                }
            },
            else => {},
        }
    }
}

fn writeRawOp(ro: RawOpcode, writer: anytype) @TypeOf(writer).Error!void {
    try writeEnum(RawOpcode, ro, writer);
}

fn convertOpcode(opcode: wasm.Op.Code) RawOpcode {
    const name = @tagName(opcode);
    return std.meta.stringToEnum(RawOpcode, name) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("`{s}` is not a valid raw opcode", .{name});
        } else unreachable;
    };
}

fn convertTypedOpcode(t: wasm.Type, opcode: wasm.Op.Code) RawOpcode {
    // minimum required buffer for writing ops with std.fmt.bufPrint
    const buf_size = comptime sz: {
        var max_len = 0;
        for (std.enums.values(RawOpcode)) |tag| {
            max_len = @max(max_len, @tagName(tag).len);
        }
        break :sz max_len;
    };
    var buf: [buf_size]u8 = undefined;

    const name = std.fmt.bufPrint(
        &buf,
        "{s}_{s}",
        .{ @tagName(t), @tagName(opcode) },
    ) catch unreachable;

    return std.meta.stringToEnum(RawOpcode, name) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("`{s}` is not a valid raw opcode", .{name});
        } else unreachable;
    };
}

fn writeOp(
    function: *const Function,
    op: Op,
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (op) {
        // operators with no args
        inline .@"unreachable",
        .nop,
        .@"return",
        => |_, opcode| {
            try writeRawOp(convertOpcode(opcode), writer);
        },

        .@"if" => |meta| {
            try writeRawOp(.@"if", writer);

            if (meta.type) |t| {
                try writeEnum(ValType, ValType.ofWasmType(t), writer);
            } else {
                try writer.writeByte(blocktype_empty);
            }

            try writeFlowCode(function, meta.if_true, writer);

            if (meta.if_false) |if_false| {
                try writeRawOp(.@"else", writer);
                try writeFlowCode(function, if_false, writer);
            }

            try writeRawOp(.end, writer);
        },

        .call => |func_ref| {
            try writeRawOp(.call, writer);
            try writeInt(u32, func_ref.index, writer);
        },

        .local_get, .local_set => |local| {
            try writeRawOp(convertOpcode(op), writer);
            try writeInt(u32, local.index, writer);
        },

        .@"const" => |value| {
            const raw_op = convertTypedOpcode(value, .@"const");
            try writeRawOp(raw_op, writer);

            switch (value) {
                .i32 => |n| try writeInt(i32, n, writer),
                .i64 => |n| try writeInt(i64, n, writer),
                .f32 => |n| try writeFloat(f32, n, writer),
                .f64 => |n| try writeFloat(f64, n, writer),
            }
        },

        // operators which abstract over a type
        .eq,
        .ne,
        .lt,
        .gt,
        .le,
        .ge,
        .add,
        .sub,
        .mul,
        => |t| {
            const raw_op = convertTypedOpcode(t, op);
            try writeRawOp(raw_op, writer);
        },

        // int-only operators which abstract over a type
        .eqz,
        .@"and",
        .@"or",
        .xor,
        => |t| {
            const raw_op = convertTypedOpcode(t.into(), op);
            try writeRawOp(raw_op, writer);
        },

        else => |tag| {
            std.debug.panic("TODO write op {s}", .{@tagName(tag)});
        },
    }
}

fn writeFlowCode(
    function: *const Function,
    flow: FlowRef,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const ops = function.flows.get(flow).ops.items;
    for (ops) |op| {
        try writeOp(function, op, writer);
    }
}

fn writeFunctionCode(
    function: *const Function,
    writer: anytype,
) @TypeOf(writer).Error!void {
    // locals
    const total_locals = function.locals.count();
    const num_params = function.params.len;
    const real_locals = total_locals - num_params;

    var locals = function.locals.iterator();
    // discard param locals
    for (0..num_params) |_| _ = locals.next();

    try writeInt(u32, @as(u32, @intCast(real_locals)), writer);
    while (locals.next()) |t| {
        try writeInt(u32, 1, writer);
        try writeEnum(ValType, ValType.ofWasmType(t.*), writer);
    }

    // structured instrs
    try writeFlowCode(function, function.entry().ref, writer);
    try writeRawOp(.end, writer);
}

/// the actual code of each function
fn writeCode(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    var funcs = module.functions.iterator();
    while (funcs.next()) |anyfunc| {
        switch (anyfunc.*) {
            .function => |*func| {
                var counter = std.io.countingWriter(std.io.null_writer);
                try writeFunctionCode(func, counter.writer());

                try writeInt(u32, @as(u32, @intCast(counter.bytes_written)), writer);
                try writeFunctionCode(func, writer);
            },
            else => {},
        }
    }
}

pub fn write(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const total_functions: u32 = @intCast(module.functions.count());
    const num_functions = countFunctions(module);
    const num_exports = countExports(module);
    const num_imports = countImports(module);

    try writer.writeAll(&wasm_magic_bytes);
    try writer.writeIntLittle(u32, wasm_version);
    try writeVecSection(module, total_functions, writer, .type, writeTypes);
    try writeVecSection(module, num_imports, writer, .import, writeImports);
    try writeVecSection(module, num_functions, writer, .function, writeFunctions);
    try writeVecSection(module, num_exports, writer, .@"export", writeExports);
    try writeVecSection(module, num_functions, writer, .code, writeCode);
}
