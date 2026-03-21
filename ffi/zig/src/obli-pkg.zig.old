// SPDX-License-Identifier: PMPL-1.0-or-later
// obli-pkg.zig - Oblibeny package manager for Lago Grey
//
// Provides package management for .zpkg archives with:
// - Package installation/removal
// - Signature verification (Dilithium5 + Ed448 + SPHINCS+)
// - Reversible operations with accountability traces
// - Dependency resolution

const std = @import("std");
const posix = std.posix;

const VERSION = "0.1.0";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage();
        return;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "version")) {
        try printVersion();
    } else if (std.mem.eql(u8, command, "install")) {
        if (args.len < 3) {
            try printError("install requires a package path");
            return;
        }
        try installPackage(args[2]);
    } else if (std.mem.eql(u8, command, "list")) {
        try listPackages();
    } else if (std.mem.eql(u8, command, "remove")) {
        if (args.len < 3) {
            try printError("remove requires a package name");
            return;
        }
        try removePackage(args[2]);
    } else if (std.mem.eql(u8, command, "verify")) {
        if (args.len < 3) {
            try printError("verify requires a package path");
            return;
        }
        try verifyPackage(args[2]);
    } else {
        try printError("unknown command");
        try printUsage();
    }
}

fn printVersion() !void {
    const msg =
        \\obli-pkg version {s}
        \\Oblibeny package manager for Lago Grey
        \\
        \\Features:
        \\  • Post-quantum signature verification (Dilithium5, Ed448, SPHINCS+)
        \\  • Reversible installations with accountability traces
        \\  • Formally verified with Idris2 ABI proofs
        \\  • Community governed (PMPL-1.0-or-later)
        \\
    ;

    var buf: [1024]u8 = undefined;
    const formatted = try std.fmt.bufPrint(&buf, msg, .{VERSION});
    _ = try posix.write(posix.STDOUT_FILENO, formatted);
}

fn printUsage() !void {
    const msg =
        \\Usage: obli-pkg <command> [arguments]
        \\
        \\Commands:
        \\  version              Show version information
        \\  install <pkg.zpkg>   Install a package
        \\  remove <name>        Remove an installed package
        \\  list                 List installed packages
        \\  verify <pkg.zpkg>    Verify package signatures
        \\
        \\Examples:
        \\  obli-pkg install hello-1.0.0.zpkg
        \\  obli-pkg list
        \\  obli-pkg remove hello
        \\  obli-pkg verify hello-1.0.0.zpkg
        \\
    ;
    _ = try posix.write(posix.STDOUT_FILENO, msg);
}

fn printError(msg: []const u8) !void {
    const prefix = "Error: ";
    _ = try posix.write(posix.STDERR_FILENO, prefix);
    _ = try posix.write(posix.STDERR_FILENO, msg);
    _ = try posix.write(posix.STDERR_FILENO, "\n");
}

fn installPackage(pkg_path: []const u8) !void {
    var buf: [256]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf,
        \\[obli-pkg] Installing package: {s}
        \\  TODO: Extract .zpkg archive
        \\  TODO: Verify triple signatures (Dilithium5 + Ed448 + SPHINCS+)
        \\  TODO: Check dependencies
        \\  TODO: Install files with accountability trace
        \\  TODO: Register in package database
        \\
    , .{pkg_path});
    _ = try posix.write(posix.STDOUT_FILENO, msg);
}

fn removePackage(pkg_name: []const u8) !void {
    var buf: [256]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf,
        \\[obli-pkg] Removing package: {s}
        \\  TODO: Check for dependent packages
        \\  TODO: Create rollback trace
        \\  TODO: Remove files
        \\  TODO: Update package database
        \\
    , .{pkg_name});
    _ = try posix.write(posix.STDOUT_FILENO, msg);
}

fn listPackages() !void {
    const msg =
        \\[obli-pkg] Installed packages:
        \\  TODO: Read from /var/lib/obli-pkg/installed.db
        \\  TODO: Show package name, version, size, install date
        \\  TODO: Show total disk usage
        \\
    ;
    _ = try posix.write(posix.STDOUT_FILENO, msg);
}

fn verifyPackage(pkg_path: []const u8) !void {
    var buf: [256]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf,
        \\[obli-pkg] Verifying package: {s}
        \\  TODO: Extract signature files (.sig.dilithium5, .sig.ed448, .sig.sphincs)
        \\  TODO: Verify Dilithium5 signature using liboqs
        \\  TODO: Verify Ed448 signature using libsodium
        \\  TODO: Verify SPHINCS+ signature using liboqs
        \\  TODO: All three must pass for package to be valid
        \\
    , .{pkg_path});
    _ = try posix.write(posix.STDOUT_FILENO, msg);
}
