//! https://webassembly.github.io/spec/core/binary/index.html

const std = @import("std");
const builtin = @import("builtin");
const repr = @import("repr.zig");
const Module = repr.Module;
const Function = repr.Function;
const Op = repr.Op;
const bytecode = @import("bytecode.zig");
const RawOpcode = bytecode.RawOpcode;

const wasm_magic_bytes: [4]u8 = .{0} ++ "asm".*;
const wasm_version: u32 = 0x1;

const functype: u8 = 0x60;

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

    fn ofReprType(t: repr.Type) ValType {
        return switch (t) {
            .u32 => .i32,
            .u64 => .i64,

            inline .i32,
            .i64,
            .f32,
            .f64,
            .funcref,
            => |tag| std.enums.nameCast(ValType, tag),
        };
    }
};

// utils =======================================================================

/// write unsigned LEB128 format
fn writeUIntLeb128(
    comptime bits: comptime_int,
    n: std.meta.Int(.unsigned, bits),
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (n == 0) {
        try writer.writeByte(0);
        return;
    }

    var x = n;
    while (x > 0) {
        var byte: u8 = @as(u7, @truncate(x));
        x >>= 7;

        if (x != 0) byte |= 0x80;
        try writer.writeByte(byte);
    }
}

/// write signed LEB128 format
fn writeIntLeb128(
    comptime bits: comptime_int,
    n: std.meta.Int(.signed, bits),
    writer: anytype,
) @TypeOf(writer).Error!void {
    var x = n;
    var done = false;
    while (!done) {
        var byte: u8 = @as(u7, @truncate(x));
        x >>= 7;

        const signed_bit = @intFromBool(byte ^ 0x40 == 0);
        if ((x == 0 and signed_bit) or (x == -1 and !signed_bit)) {
            done = true;
        } else {
            byte |= 0x80;
        }

        try writer.writeByte(byte);
    }
}

/// determines impl by signedness
fn writeInt(
    comptime Int: type,
    value: Int,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const info = @typeInfo(Int).Int;
    switch (info.signedness) {
        .signed => try writeIntLeb128(info.bits, value, writer),
        .unsigned => try writeUIntLeb128(info.bits, value, writer),
    }
}

/// write float in little-endian byte order
fn writeFloat(
    comptime F: type,
    value: F,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const UInt = std.meta.Int(.unsigned, @bitSizeOf(F));
    try std.mem.writeIntLittle(UInt, @bitCast(value));
}

fn writeEnum(
    comptime E: type,
    value: E,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.writeIntLittle(u8, @intFromEnum(value));
}

fn writeSectionHeader(
    section: Section,
    size_guesstimate: u32,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writeEnum(Section, section, writer);
    try writeInt(u32, size_guesstimate, writer);
}

/// wraps a section impl in a function that handles the section header
fn writeSection(
    module: *const Module,
    writer: anytype,
    section: Section,
    comptime wrapped: fn (
        *const Module,
        writer: anytype,
    ) @TypeOf(writer).Error!void,
) @TypeOf(writer).Error!void {
    const num_functions = module.functions.count();
    if (num_functions == 0) return;

    var counter = std.io.countingWriter(std.io.null_writer);
    try wrapped(module, counter.writer());

    try writeSectionHeader(section, @intCast(counter.bytes_written), writer);
    try wrapped(module, writer);
}

// =============================================================================

/// writes function types as their index
fn writeTypes(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const num_functions = module.functions.count();
    try writeInt(u32, @as(u32, @intCast(num_functions)), writer);

    var func_iter = module.functions.iterator();
    while (func_iter.next()) |func| {
        try writer.writeByte(functype);

        try writeInt(u32, @as(u32, @intCast(func.params.len)), writer);
        for (func.params) |param| {
            const vt = ValType.ofReprType(func.typeOf(param));
            try writeEnum(ValType, vt, writer);
        }

        try writeInt(u32, @as(u32, @intCast(func.returns.len)), writer);
        for (func.returns) |t| {
            try writeEnum(ValType, ValType.ofReprType(t), writer);
        }
    }
}

/// just maps functions to their indexed type
fn writeFunctions(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const num_functions = module.functions.count();
    try writeInt(u32, @as(u32, @intCast(num_functions)), writer);

    var funcs = module.functions.iterator();
    while (funcs.next()) |func| {
        const index = func.ref.index;
        try writeInt(u32, @as(u32, @intCast(index)), writer);
    }
}

fn writeRawOp(ro: RawOpcode, writer: anytype) @TypeOf(writer).Error!void {
    try writeEnum(RawOpcode, ro, writer);
}

fn writeOp(
    function: *const Function,
    op: Op,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = function;
    switch (op) {
        inline .@"unreachable",
        .nop,
        .@"return",
        => |_, tag| {
            try writeRawOp(std.enums.nameCast(RawOpcode, tag), writer);
        },

        else => |tag| {
            std.debug.panic("TODO write op {s}", .{@tagName(tag)});
        }
    }
}

fn writeFunctionCode(
    function: *const Function,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writeInt(u32, @as(u32, @intCast(function.locals.count())), writer);
    var locals = function.locals.iterator();
    while (locals.next()) |t| {
        try writeInt(u32, 1, writer);
        try writeEnum(ValType, ValType.ofReprType(t.*), writer);
    }

    for (function.ops.items) |op| {
        try writeOp(function, op, writer);
    }

    try writeRawOp(.end, writer);
}

/// the actual code of each function
fn writeCode(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    const num_functions = module.functions.count();
    try writeInt(u32, @as(u32, @intCast(num_functions)), writer);

    var funcs = module.functions.iterator();
    while (funcs.next()) |func| {
        var counter = std.io.countingWriter(std.io.null_writer);
        try writeFunctionCode(func, counter.writer());

        try writeInt(u32, @as(u32, @intCast(counter.bytes_written)), writer);
        try writeFunctionCode(func, writer);
    }
}

pub fn write(
    module: *const Module,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.writeAll(&wasm_magic_bytes);
    try writer.writeIntLittle(u32, wasm_version);
    try writeSection(module, writer, .type, writeTypes);
    try writeSection(module, writer, .function, writeFunctions);
    try writeSection(module, writer, .code, writeCode);
}
