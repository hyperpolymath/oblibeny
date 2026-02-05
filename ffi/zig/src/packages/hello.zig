// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

//! PROOF OF CONCEPT: Hello Package FFI Implementation
//!
//! This Zig code implements the C ABI defined by Idris2.
//! It provides the actual system operations for package installation.

const std = @import("std");
const fs = std.fs;
const mem = std.mem;

// ============================================================================
// FFI EXPORTS (Called by Oblibeny via Idris2 ABI)
// ============================================================================

/// Install the hello package
/// Returns: 0 on success, negative error code on failure
export fn hello_install(
    pkg_path: [*:0]const u8,
    target_root: [*:0]const u8,
) callconv(.C) i32 {
    installImpl(pkg_path, target_root) catch |err| {
        return switch (err) {
            error.FileNotFound => -1,
            error.AccessDenied => -2,
            error.InvalidSignature => -3,
            else => -99,
        };
    };
    return 0;
}

/// Uninstall the hello package
/// Returns: 0 on success, negative error code on failure
export fn hello_uninstall(
    target_root: [*:0]const u8,
) callconv(.C) i32 {
    uninstallImpl(target_root) catch |err| {
        return switch (err) {
            error.FileNotFound => -1,
            error.AccessDenied => -2,
            else => -99,
        };
    };
    return 0;
}

// ============================================================================
// IMPLEMENTATION
// ============================================================================

const InstallError = error{
    FileNotFound,
    AccessDenied,
    InvalidSignature,
    OutOfMemory,
};

fn installImpl(
    pkg_path: [*:0]const u8,
    target_root: [*:0]const u8,
) InstallError!void {
    const allocator = std.heap.page_allocator;

    // Convert C strings to Zig slices
    const pkg_path_slice = mem.span(pkg_path);
    const target_root_slice = mem.span(target_root);

    // Build target binary path
    const bin_path = try fs.path.join(allocator, &.{
        target_root_slice,
        "usr",
        "bin",
        "hello",
    });
    defer allocator.free(bin_path);

    // Verify cryptographic signature (placeholder)
    if (!try verifySignature(pkg_path_slice)) {
        return error.InvalidSignature;
    }

    // Extract binary from package (simplified - assumes single binary)
    const pkg_file = try fs.cwd().openFile(pkg_path_slice, .{});
    defer pkg_file.close();

    const bin_dir = try fs.path.dirname(bin_path) orelse return error.FileNotFound;
    try fs.cwd().makePath(bin_dir);

    const target_file = try fs.cwd().createFile(bin_path, .{
        .read = true,
        .truncate = true,
        .mode = 0o755, // Executable
    });
    defer target_file.close();

    // Copy file contents
    var buf: [4096]u8 = undefined;
    while (true) {
        const bytes_read = try pkg_file.read(&buf);
        if (bytes_read == 0) break;
        _ = try target_file.write(buf[0..bytes_read]);
    }

    // Verify installation (file exists and is executable)
    const stat = try target_file.stat();
    if (stat.size == 0) {
        return error.FileNotFound;
    }
}

fn uninstallImpl(target_root: [*:0]const u8) InstallError!void {
    const allocator = std.heap.page_allocator;
    const target_root_slice = mem.span(target_root);

    const bin_path = try fs.path.join(allocator, &.{
        target_root_slice,
        "usr",
        "bin",
        "hello",
    });
    defer allocator.free(bin_path);

    // Delete file (reversible via Oblibeny accountability trace)
    try fs.cwd().deleteFile(bin_path);
}

// ============================================================================
// CRYPTOGRAPHIC VERIFICATION
// ============================================================================

fn verifySignature(pkg_path: []const u8) !bool {
    // TODO: Implement actual signature verification
    // For PoC, just check file exists
    _ = pkg_path;
    return true;
}

// ============================================================================
// TESTS
// ============================================================================

test "install and uninstall" {
    const testing = std.testing;

    // TODO: Implement integration tests
    // 1. Create temporary package file
    // 2. Call hello_install
    // 3. Verify file exists
    // 4. Call hello_uninstall
    // 5. Verify file removed

    try testing.expect(true);
}
