// SPDX-License-Identifier: MPL-2.0
// installed_set.zig - the obli-pkg package database as a SET of package names.
//
// This module is the executable counterpart of the `installedPackages`
// component of `SystemState` in src/abi/Packages/Hello/Interface.idr. The
// Idris model proves two laws over that component:
//
//   installReversible       uninstall pkg (install pkg st) = st   (pkg fresh)
//   doubleInstallIdempotent install pkg (install pkg st) = install pkg st
//
// Both laws hold only under set semantics (`addUnique` / `remove`). The
// previous Zig implementation appended a row on every install and never
// removed anything, which falsified both. The functions here mirror the Idris
// primitives one-for-one and the tests at the bottom check the two laws on a
// real file.
//
// On-disk format (unchanged, tab-separated, one row per line):
//
//     <name>\t<status>\t<timestamp>
//
// The set element is the FIRST column (the package name), exactly as the
// Idris carrier is a `List String` of names. The remaining columns are opaque
// payload carried with the element; they play no part in equality.
//
// Scope: only `installedPackages` is modelled. The Idris `existingFiles`
// component has no counterpart here, because install does not record which
// files a package placed on disk.
//
// This file imports nothing but `std`, so its tests run without liboqs.

const std = @import("std");

/// Default location of the package database.
pub const default_db_path = "/var/lib/obli-pkg/installed.db";

/// Environment variable that overrides `default_db_path`.
pub const db_path_env = "OBLI_PKG_DB";

pub const Error = error{
    /// A name or row would corrupt the TSV format (empty name, or a tab or
    /// newline inside a field).
    InvalidRow,
};

/// The name field of a row: everything before the first tab. This is the
/// element the Idris `installedPackages : List String` carries.
pub fn rowName(row: []const u8) []const u8 {
    const end = std.mem.indexOfScalar(u8, row, '\t') orelse row.len;
    return row[0..end];
}

/// Derive a package name from an archive path: take the basename, drop a
/// `.zpkg` suffix, then drop a trailing `-<version>` where the version starts
/// with a digit. `/tmp/hello-1.0.0.zpkg` -> `hello`; `foo-bar-2.1.zpkg` ->
/// `foo-bar`. This is the naming policy of the database, not part of the
/// Idris model (which takes `pkg.name` as given).
pub fn packageNameFromPath(path: []const u8) []const u8 {
    var stem = std.fs.path.basename(path);
    if (std.mem.endsWith(u8, stem, ".zpkg")) stem = stem[0 .. stem.len - ".zpkg".len];
    if (std.mem.lastIndexOfScalar(u8, stem, '-')) |dash| {
        if (dash > 0 and dash + 1 < stem.len and std.ascii.isDigit(stem[dash + 1])) {
            return stem[0..dash];
        }
    }
    return stem;
}

/// Build a database row `<name>\tinstalled\t<timestamp>`; caller owns it.
/// Refuses names that would corrupt the TSV format.
pub fn formatRow(allocator: std.mem.Allocator, name: []const u8, timestamp: i64) ![]u8 {
    try validateName(name);
    return std.fmt.allocPrint(allocator, "{s}\tinstalled\t{d}", .{ name, timestamp });
}

/// Reject an empty name or one containing a tab or newline.
fn validateName(name: []const u8) Error!void {
    if (name.len == 0) return error.InvalidRow;
    if (std.mem.indexOfAny(u8, name, "\t\n\r") != null) return error.InvalidRow;
}

/// The set of installed packages, held as rows in file order.
pub const InstalledSet = struct {
    allocator: std.mem.Allocator,
    rows: std.ArrayList([]u8),

    /// An empty set: the Idris `[]`.
    pub fn init(allocator: std.mem.Allocator) InstalledSet {
        return .{ .allocator = allocator, .rows = std.ArrayList([]u8).init(allocator) };
    }

    /// Free every row and the backing list.
    pub fn deinit(self: *InstalledSet) void {
        for (self.rows.items) |row| self.allocator.free(row);
        self.rows.deinit();
    }

    /// Read the database at `sub_path` (relative to `dir`, or absolute). A
    /// missing file is the empty set. Blank lines are skipped; duplicate names
    /// already present on disk (as written by the old append-only code) are
    /// kept as-is so that `remove` can drop every one of them.
    pub fn load(allocator: std.mem.Allocator, dir: std.fs.Dir, sub_path: []const u8) !InstalledSet {
        var set = InstalledSet.init(allocator);
        errdefer set.deinit();

        const bytes = dir.readFileAlloc(allocator, sub_path, 16 * 1024 * 1024) catch |err| switch (err) {
            error.FileNotFound => return set,
            else => return err,
        };
        defer allocator.free(bytes);

        var it = std.mem.splitScalar(u8, bytes, '\n');
        while (it.next()) |line| {
            if (line.len == 0) continue;
            const row = try allocator.dupe(u8, line);
            errdefer allocator.free(row);
            try set.rows.append(row);
        }
        return set;
    }

    /// Serialise to the canonical on-disk form: each row followed by `\n`.
    /// Caller owns the returned bytes.
    pub fn serialize(self: *const InstalledSet, allocator: std.mem.Allocator) ![]u8 {
        var out = std.ArrayList(u8).init(allocator);
        errdefer out.deinit();
        for (self.rows.items) |row| {
            try out.appendSlice(row);
            try out.append('\n');
        }
        return out.toOwnedSlice();
    }

    /// Write the set to `sub_path` atomically (temp file + rename), so a crash
    /// mid-write never leaves a truncated database.
    pub fn save(self: *const InstalledSet, dir: std.fs.Dir, sub_path: []const u8) !void {
        const bytes = try self.serialize(self.allocator);
        defer self.allocator.free(bytes);
        var af = try dir.atomicFile(sub_path, .{});
        defer af.deinit();
        try af.file.writeAll(bytes);
        try af.finish();
    }

    /// Membership by name. Mirrors Idris `isInstalled pkg st = elem pkg
    /// st.installedPackages`.
    pub fn contains(self: *const InstalledSet, name: []const u8) bool {
        for (self.rows.items) |row| {
            if (std.mem.eql(u8, rowName(row), name)) return true;
        }
        return false;
    }

    /// Insert `row` only if no row with the same name is present. Mirrors
    /// Idris `addUnique x xs`: present -> `xs` unchanged (the EXISTING row,
    /// with its version/timestamp, is kept; nothing is replaced); absent ->
    /// the row is added. Idris conses at the front, this appends at the end to
    /// keep the file chronological; the proved laws do not depend on position.
    /// Returns true iff the row was inserted. The set takes a copy of `row`.
    pub fn add(self: *InstalledSet, row: []const u8) !bool {
        const name = rowName(row);
        try validateName(name);
        if (std.mem.indexOfAny(u8, row, "\n\r") != null) return error.InvalidRow;
        if (self.contains(name)) return false;
        const owned = try self.allocator.dupe(u8, row);
        errdefer self.allocator.free(owned);
        try self.rows.append(owned);
        return true;
    }

    /// Drop EVERY row whose name equals `name`, preserving the order of the
    /// rest. Mirrors Idris `remove x xs`; removing an absent name is a no-op
    /// (Idris lemma `removeNotElem`). Returns the number of rows dropped.
    pub fn remove(self: *InstalledSet, name: []const u8) usize {
        var kept: usize = 0;
        var dropped: usize = 0;
        for (self.rows.items) |row| {
            if (std.mem.eql(u8, rowName(row), name)) {
                self.allocator.free(row);
                dropped += 1;
            } else {
                self.rows.items[kept] = row;
                kept += 1;
            }
        }
        self.rows.shrinkRetainingCapacity(kept);
        return dropped;
    }
};

/// Load, `add` (Idris `addUnique`), and save only if something changed. This
/// is the database half of Idris `install`. Returns true iff the row was new;
/// when the name is already present the file is not touched at all.
pub fn registerInstall(allocator: std.mem.Allocator, dir: std.fs.Dir, sub_path: []const u8, row: []const u8) !bool {
    var set = try InstalledSet.load(allocator, dir, sub_path);
    defer set.deinit();
    const inserted = try set.add(row);
    if (inserted) try set.save(dir, sub_path);
    return inserted;
}

/// Load, `remove` (Idris `remove`), and save only if something changed. This
/// is the database half of Idris `uninstall`. Returns the number of rows
/// dropped; when it is 0 the file is not touched (and not created).
pub fn unregister(allocator: std.mem.Allocator, dir: std.fs.Dir, sub_path: []const u8, name: []const u8) !usize {
    var set = try InstalledSet.load(allocator, dir, sub_path);
    defer set.deinit();
    const dropped = set.remove(name);
    if (dropped > 0) try set.save(dir, sub_path);
    return dropped;
}

// ============================================================================
// Tests: the two Idris laws, checked on real files in a temp directory.
// ============================================================================

const testing = std.testing;
const db = "installed.db";

/// Read the whole db file, or "" when absent.
fn readDb(dir: std.fs.Dir) ![]u8 {
    return dir.readFileAlloc(testing.allocator, db, 1 << 20) catch |err| switch (err) {
        error.FileNotFound => testing.allocator.dupe(u8, ""),
        else => err,
    };
}

test "installReversible: install;remove = id on a fresh state (bytes equal)" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const seed = "alpha\tinstalled\t100\nbeta\tinstalled\t200\n";
    try tmp.dir.writeFile(.{ .sub_path = db, .data = seed });

    try testing.expect(try registerInstall(testing.allocator, tmp.dir, db, "hello\tinstalled\t300"));
    const mid = try readDb(tmp.dir);
    defer testing.allocator.free(mid);
    try testing.expect(!std.mem.eql(u8, mid, seed));

    try testing.expectEqual(@as(usize, 1), try unregister(testing.allocator, tmp.dir, db, "hello"));
    const after = try readDb(tmp.dir);
    defer testing.allocator.free(after);
    try testing.expectEqualStrings(seed, after);
}

test "installReversible: from an empty database the result is empty" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    _ = try registerInstall(testing.allocator, tmp.dir, db, "hello\tinstalled\t1");
    _ = try unregister(testing.allocator, tmp.dir, db, "hello");
    const after = try readDb(tmp.dir);
    defer testing.allocator.free(after);
    try testing.expectEqualStrings("", after);
}

test "doubleInstallIdempotent: install;install = install (bytes equal, first row kept)" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try testing.expect(try registerInstall(testing.allocator, tmp.dir, db, "hello\tinstalled\t1"));
    const once = try readDb(tmp.dir);
    defer testing.allocator.free(once);

    // Same name, later timestamp: addUnique keeps the existing element.
    try testing.expect(!try registerInstall(testing.allocator, tmp.dir, db, "hello\tinstalled\t2"));
    const twice = try readDb(tmp.dir);
    defer testing.allocator.free(twice);
    try testing.expectEqualStrings(once, twice);
    try testing.expectEqualStrings("hello\tinstalled\t1\n", twice);
}

test "remove of an absent name is a no-op and does not create the db" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try testing.expectEqual(@as(usize, 0), try unregister(testing.allocator, tmp.dir, db, "ghost"));
    try testing.expectError(error.FileNotFound, tmp.dir.access(db, .{}));

    const seed = "alpha\tinstalled\t100\n";
    try tmp.dir.writeFile(.{ .sub_path = db, .data = seed });
    try testing.expectEqual(@as(usize, 0), try unregister(testing.allocator, tmp.dir, db, "ghost"));
    const after = try readDb(tmp.dir);
    defer testing.allocator.free(after);
    try testing.expectEqualStrings(seed, after);
}

test "remove drops EVERY occurrence (legacy append-only duplicates)" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    // What the old append-only installer produced after install;install.
    const legacy = "hello\tinstalled\t1\nalpha\tinstalled\t5\nhello\tinstalled\t2\n";
    try tmp.dir.writeFile(.{ .sub_path = db, .data = legacy });
    try testing.expectEqual(@as(usize, 2), try unregister(testing.allocator, tmp.dir, db, "hello"));
    const after = try readDb(tmp.dir);
    defer testing.allocator.free(after);
    try testing.expectEqualStrings("alpha\tinstalled\t5\n", after);
}

test "load/save roundtrip is byte-identical on the canonical form" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const seed = "a\tinstalled\t1\nb\tinstalled\t2\nc\tinstalled\t3\n";
    try tmp.dir.writeFile(.{ .sub_path = db, .data = seed });
    var set = try InstalledSet.load(testing.allocator, tmp.dir, db);
    defer set.deinit();
    try testing.expectEqual(@as(usize, 3), set.rows.items.len);
    try set.save(tmp.dir, "copy.db");
    const copy = try tmp.dir.readFileAlloc(testing.allocator, "copy.db", 1 << 20);
    defer testing.allocator.free(copy);
    try testing.expectEqualStrings(seed, copy);
}

test "membership is by name only, not by version or timestamp" {
    var set = InstalledSet.init(testing.allocator);
    defer set.deinit();
    try testing.expect(try set.add("hello\tinstalled\t1"));
    try testing.expect(set.contains("hello"));
    try testing.expect(!set.contains("hell"));
    try testing.expect(!set.contains("hello\tinstalled\t1"));
    try testing.expect(!try set.add("hello\tinstalled\t99"));
    try testing.expectEqual(@as(usize, 1), set.rows.items.len);
}

test "rows that would corrupt the TSV are refused" {
    var set = InstalledSet.init(testing.allocator);
    defer set.deinit();
    try testing.expectError(error.InvalidRow, set.add("\tinstalled\t1"));
    try testing.expectError(error.InvalidRow, set.add("a\nb\tinstalled\t1"));
    try testing.expectError(error.InvalidRow, formatRow(testing.allocator, "bad\tname", 1));
}

test "packageNameFromPath strips directory, .zpkg and -<version>" {
    try testing.expectEqualStrings("hello", packageNameFromPath("/tmp/e2e/hello-1.0.0.zpkg"));
    try testing.expectEqualStrings("hello", packageNameFromPath("hello.zpkg"));
    try testing.expectEqualStrings("foo-bar", packageNameFromPath("foo-bar-2.1.zpkg"));
    try testing.expectEqualStrings("foo-bar", packageNameFromPath("foo-bar.zpkg"));
    try testing.expectEqualStrings("hello-signed", packageNameFromPath("hello-signed.zpkg"));
}
