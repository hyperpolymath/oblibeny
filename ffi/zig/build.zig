// SPDX-License-Identifier: PMPL-1.0-or-later
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ========================================================================
    // Crypto Library (liboblibeny_crypto.a)
    // ========================================================================
    const crypto_lib = b.addStaticLibrary(.{
        .name = "oblibeny_crypto",
        .root_source_file = b.path("src/crypto.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Link crypto libraries
    crypto_lib.linkLibC();
    crypto_lib.linkSystemLibrary("oqs");       // liboqs
    crypto_lib.linkSystemLibrary("sodium");    // libsodium

    // Add library search paths (adjust for your system)
    crypto_lib.addLibraryPath(b.path("/usr/local/lib"));
    crypto_lib.addIncludePath(b.path("/usr/local/include"));

    b.installArtifact(crypto_lib);

    // ========================================================================
    // obli-pkg Executable
    // ========================================================================
    const obli_pkg = b.addExecutable(.{
        .name = "obli-pkg",
        .root_source_file = b.path("src/obli-pkg.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Link with crypto library
    obli_pkg.linkLibrary(crypto_lib);
    obli_pkg.linkLibC();

    b.installArtifact(obli_pkg);

    // Run command
    const run_cmd = b.addRunArtifact(obli_pkg);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run obli-pkg");
    run_step.dependOn(&run_cmd.step);

    // ========================================================================
    // Tests
    // ========================================================================
    const crypto_tests = b.addTest(.{
        .root_source_file = b.path("src/crypto.zig"),
        .target = target,
        .optimize = optimize,
    });

    crypto_tests.linkLibC();
    crypto_tests.linkSystemLibrary("oqs");
    crypto_tests.linkSystemLibrary("sodium");
    crypto_tests.addLibraryPath(b.path("/usr/local/lib"));
    crypto_tests.addIncludePath(b.path("/usr/local/include"));

    const test_step = b.step("test", "Run crypto tests");
    test_step.dependOn(&b.addRunArtifact(crypto_tests).step);
}
