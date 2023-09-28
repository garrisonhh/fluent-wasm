const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const common = b.dependency("zighh", .{}).module("common");
    const wasm3_dep = b.dependency("wasm3", .{});
    const wasm3_mod = wasm3_dep.module("wasm3");
    const wasm3_lib = wasm3_dep.artifact("wasm3");

    // mod + lib
    _ = b.addModule("fluent-wasm", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = &.{
            .{ .name = "common", .module = common },
            .{ .name = "wasm3", .module = wasm3_mod },
        },
    });

    const lib = b.addStaticLibrary(.{
        .name = "fluent-wasm",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    lib.linkLibC();
    lib.linkLibrary(wasm3_lib);
    try lib.include_dirs.appendSlice(wasm3_lib.include_dirs.items);

    b.installArtifact(lib);

    // tests
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    unit_tests.addModule("common", common);
    unit_tests.addModule("wasm3", wasm3_mod);

    unit_tests.linkLibC();
    unit_tests.linkLibrary(wasm3_lib);
    try unit_tests.include_dirs.appendSlice(wasm3_lib.include_dirs.items);

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
