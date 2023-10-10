//! https://webassembly.github.io/spec/core/binary/index.html

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const RawOpcode = std.wasm.Opcode;
const wasm = @import("main.zig");
const Module = wasm.Module;
const Function = wasm.Function;
const Op = wasm.Op;
const Local = wasm.Local;
const FlowRef = wasm.FlowRef;

pub const Error = Allocator.Error;

const wasm_magic_bytes: [4]u8 = .{0} ++ "asm".*;
const wasm_version: [4]u8 = .{1, 0, 0, 0};

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

    fn from(t: wasm.Type) ValType {
        return std.meta.stringToEnum(ValType, @tagName(t)).?;
    }
};

/// used for imports and exports
const LinkDesc = enum(u8) {
    func = 0,
    table = 1,
    mem = 2,
    global = 3,
};

// utils =======================================================================

/// determines impl by signedness
fn writeInt(comptime I: type, value: I, bl: *std.ArrayList(u8)) Error!void {
    const info = @typeInfo(I).Int;
    switch (info.signedness) {
        .signed => try std.leb.writeILEB128(bl.writer(), value),
        .unsigned => try std.leb.writeULEB128(bl.writer(), value),
    }
}

/// write float in little-endian byte order
fn writeFloat(comptime F: type, value: F, bl: *std.ArrayList(u8)) Error!void {
    std.debug.assert(@typeInfo(F) == .Float);

    const UInt = std.meta.Int(.unsigned, @bitSizeOf(F));
    var buf: [@sizeOf(F)]u8 = undefined;
    std.mem.writeIntLittle(UInt, &buf, @as(UInt, @bitCast(value)));

    try bl.appendSlice(&buf);
}

fn writeEnum(comptime E: type, value: E, bl: *std.ArrayList(u8)) Error!void {
    try bl.append(@intFromEnum(value));
}

fn writeName(name: []const u8, bl: *std.ArrayList(u8)) Error!void {
    try writeInt(u32, @as(u32, @intCast(name.len)), bl);
    try bl.appendSlice(name);
}

fn writeSectionHeader(
    section: Section,
    size: u32,
    bl: *std.ArrayList(u8),
) Error!void {
    try writeEnum(Section, section, bl);
    try writeInt(u32, size, bl);
}

/// wraps a section impl in a function that handles the section header and the
/// vec count
fn writeVecSection(
    module: *const Module,
    count: u32,
    bl: *std.ArrayList(u8),
    section: Section,
    comptime wrapped: fn (*const Module, *std.ArrayList(u8)) Error!void,
) Error!void {
    if (count == 0) return;

    var sub_bl = std.ArrayList(u8).init(bl.allocator);
    defer sub_bl.deinit();
    try writeInt(u32, count, &sub_bl);
    try wrapped(module, &sub_bl);

    try writeSectionHeader(section, @intCast(sub_bl.items.len), bl);
    try bl.appendSlice(sub_bl.items);
}

// =============================================================================

/// writes function types as their index
fn writeTypes(module: *const Module, bl: *std.ArrayList(u8)) Error!void {
    var entries = module.registry.iterator();
    while (entries.next()) |entry| {
        try bl.append(functype);

        switch (entry.*) {
            .function => |func| {
                try writeInt(u32, @as(u32, @intCast(func.params.len)), bl);
                for (func.params) |param| {
                    const vt = ValType.from(func.typeOf(param));
                    try writeEnum(ValType, vt, bl);
                }
            },
            .import => |imp| {
                try writeInt(u32, @as(u32, @intCast(imp.params.len)), bl);
                for (imp.params) |param| {
                    try writeEnum(ValType, ValType.from(param), bl);
                }
            },
        }

        const returns = switch (entry.*) {
            inline else => |x| x.returns,
        };

        try writeInt(u32, @as(u32, @intCast(returns.len)), bl);
        for (returns) |t| {
            try writeEnum(ValType, ValType.from(t), bl);
        }
    }
}

fn writeImports(module: *const Module, bl: *std.ArrayList(u8)) Error!void {
    var imports = module.imports.valueIterator();
    while (imports.next()) |ptr| {
        const imp = ptr.*;
        try writeName(imp.module, bl);
        try writeName(imp.name, bl);
        try writeEnum(LinkDesc, .func, bl);
        try writeInt(u32, imp.ref.index, bl);
    }
}

/// just maps functions to their indexed type
fn writeFunctions(module: *const Module, bl: *std.ArrayList(u8)) Error!void {
    var entries = module.registry.iterator();
    while (entries.next()) |entry| {
        const ref = switch (entry.*) {
            inline .function, .import => |meta| meta.ref,
        };
        try writeInt(u32, ref.index, bl);
    }
}

fn writeExports(module: *const Module, bl: *std.ArrayList(u8)) Error!void {
    var exports = module.exports.valueIterator();
    while (exports.next()) |exp| {
        try writeName(exp.name, bl);
        try writeEnum(LinkDesc, .func, bl);
        try writeInt(u32, exp.ref.index, bl);
    }
}

fn writeRawOp(ro: RawOpcode, bl: *std.ArrayList(u8)) Error!void {
    try writeEnum(RawOpcode, ro, bl);
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
    bl: *std.ArrayList(u8),
) Error!void {
    switch (op) {
        // operators with no args
        inline .@"unreachable",
        .nop,
        .@"return",
        => |_, opcode| {
            try writeRawOp(convertOpcode(opcode), bl);
        },

        .@"if" => |meta| {
            try writeRawOp(.@"if", bl);

            if (meta.type) |t| {
                try writeEnum(ValType, ValType.from(t), bl);
            } else {
                try bl.append(blocktype_empty);
            }

            try writeFlowCode(function, meta.if_true, bl);

            if (meta.if_false) |if_false| {
                try writeRawOp(.@"else", bl);
                try writeFlowCode(function, if_false, bl);
            }

            try writeRawOp(.end, bl);
        },

        .call => |func_ref| {
            try writeRawOp(.call, bl);
            try writeInt(u32, func_ref.index, bl);
        },

        .local_get, .local_set => |local| {
            try writeRawOp(convertOpcode(op), bl);
            try writeInt(u32, local.index, bl);
        },

        .@"const" => |value| {
            const raw_op = convertTypedOpcode(value, .@"const");
            try writeRawOp(raw_op, bl);

            switch (value) {
                .i32 => |n| try writeInt(i32, n, bl),
                .i64 => |n| try writeInt(i64, n, bl),
                .f32 => |n| try writeFloat(f32, n, bl),
                .f64 => |n| try writeFloat(f64, n, bl),
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
            try writeRawOp(raw_op, bl);
        },

        // int-only operators which abstract over a type
        .eqz,
        .@"and",
        .@"or",
        .xor,
        => |t| {
            const raw_op = convertTypedOpcode(t.into(), op);
            try writeRawOp(raw_op, bl);
        },

        else => |tag| {
            std.debug.panic("TODO write op {s}", .{@tagName(tag)});
        },
    }
}

fn writeFlowCode(
    function: *const Function,
    flow: FlowRef,
    bl: *std.ArrayList(u8),
) Error!void {
    const ops = function.flows.get(flow).ops.items;
    for (ops) |op| {
        try writeOp(function, op, bl);
    }
}

fn writeFunctionCode(
    function: *const Function,
    bl: *std.ArrayList(u8),
) Error!void {
    // locals
    const total_locals = function.locals.count();
    const num_params = function.params.len;
    const real_locals = total_locals - num_params;

    var locals = function.locals.iterator();
    // discard param locals
    for (0..num_params) |_| _ = locals.next();

    try writeInt(u32, @as(u32, @intCast(real_locals)), bl);
    while (locals.next()) |t| {
        try writeInt(u32, 1, bl);
        try writeEnum(ValType, ValType.from(t.*), bl);
    }

    // structured instrs
    try writeFlowCode(function, function.entry().ref, bl);
    try writeRawOp(.end, bl);
}

/// the actual code of each function
fn writeCode(module: *const Module, bl: *std.ArrayList(u8)) Error!void {
    var funcs = module.functions.valueIterator();
    while (funcs.next()) |ptr| {
        const func = ptr.*;

        var sub_bl = std.ArrayList(u8).init(bl.allocator);
        defer sub_bl.deinit();
        try writeFunctionCode(func, &sub_bl);

        try writeInt(u32, @as(u32, @intCast(sub_bl.items.len)), bl);
        try bl.appendSlice(sub_bl.items);
    }
}

pub fn write(module: *const Module, ally: Allocator) Error![]const u8 {
    const total_functions: u32 = @intCast(module.registry.count());
    const num_functions: u32 = @intCast(module.functions.count());
    const num_exports: u32 = @intCast(module.exports.count());
    const num_imports: u32 = @intCast(module.imports.count());

    var bl = std.ArrayList(u8).init(ally);
    defer bl.deinit();

    try bl.appendSlice(&wasm_magic_bytes);
    try bl.appendSlice(&wasm_version);
    try writeVecSection(module, total_functions, &bl, .type, writeTypes);
    try writeVecSection(module, num_imports, &bl, .import, writeImports);
    try writeVecSection(module, num_functions, &bl, .function, writeFunctions);
    try writeVecSection(module, num_exports, &bl, .@"export", writeExports);
    try writeVecSection(module, num_functions, &bl, .code, writeCode);

    return try bl.toOwnedSlice();
}
