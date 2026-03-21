// SPDX-License-Identifier: PMPL-1.0-or-later
// hello.zig - Simple hello binary for Lago Grey proof-of-concept

const std = @import("std");
const posix = std.posix;

pub fn main() !void {
    const msg =
        \\🌊 Hello from Lago Grey!
        \\Minimal Linux distribution:
        \\  • 17.5 MB base (Small Iceberg 🏔️)
        \\  • Post-quantum crypto (Dilithium5, Kyber-1024)
        \\  • Formally verified (Idris2 + Zig)
        \\  • Community governed (PMPL-1.0-or-later)
        \\
    ;

    _ = try posix.write(posix.STDOUT_FILENO, msg);
}
