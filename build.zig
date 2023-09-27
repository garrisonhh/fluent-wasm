const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const common = b.dependency("zighh", .{}).module("common");

    _ = b.addModule("wasm", .{
        .source_file = .{ .path = "src/main.zig" },
        .dependencies = &.{
            .{ .name = "common", .module = common },
        },
    });

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    unit_tests.addModule("common", common);

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
