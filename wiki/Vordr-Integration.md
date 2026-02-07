# Vordr Integration

**Vordr** + **Lago Grey** = Accountability-driven monitoring and defense

## Overview

Vordr integrates with Lago Grey to gain:
- **Accountability traces** from all package operations
- **Minimal attack surface** (14.6 MB = fewer CVEs)
- **Reversible operations** for forensic analysis and recovery
- **Structured audit logs** in machine-readable format

---

## What Vordr Gets from Lago Grey

### 1. Accountability Trace Database
**Location:** `/var/lib/obli-pkg/traces/`

Every `obli-pkg` operation creates a trace entry:
```scheme
;; Example trace entry
(trace
  (id "550e8400-e29b-41d4-a716-446655440000")
  (timestamp "2026-02-05T10:30:15Z")
  (operation "install")
  (package "hello-1.0.0.zpkg")
  (user "root")
  (files-added (
    "/usr/bin/hello"
    "/usr/share/doc/hello/README.md"))
  (files-modified ())
  (files-removed ())
  (signature-verification (
    (dilithium5 "VALID")
    (ed448 "VALID")
    (sphincs "VALID")))
  (reversible true)
  (rollback-id "550e8400-e29b-41d4-a716-446655440000"))
```

### 2. Package Database
**Location:** `/var/lib/obli-pkg/installed.db`

Tracks all installed packages:
```scheme
;; Example package entry
(package
  (name "hello")
  (version "1.0.0")
  (install-date "2026-02-05T10:30:15Z")
  (size 29696)  ; bytes
  (files (
    ("/usr/bin/hello" "9a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d")
    ("/usr/share/doc/hello/README.md" "1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d")))
  (dependencies ())
  (trace-id "550e8400-e29b-41d4-a716-446655440000"))
```

### 3. File-Level Change Tracking
**Location:** `/var/lib/obli-pkg/checksums/`

SHA-256 checksums of all managed files:
```
9a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d  /usr/bin/hello
1a2b3c4d5e6f7a8b9c0d1e2f3a4b5c6d  /usr/share/doc/hello/README.md
```

### 4. Minimal Attack Surface
**Lago Grey base:** 14.6 MB, ~50 files

Fewer files = fewer CVEs to monitor:
- Alpine: ~5,000 files, ~14,000 packages
- Lago Grey: ~50 files, ~10 packages (target)
- **99% reduction in monitoring overhead**

---

## Integration Patterns

### Pattern 1: Real-Time Trace Monitoring

Vordr watches the trace directory for new operations:

```zig
const std = @import("std");
const posix = std.posix;

pub const TraceMonitor = struct {
    trace_dir: []const u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !TraceMonitor {
        return TraceMonitor{
            .trace_dir = "/var/lib/obli-pkg/traces",
            .allocator = allocator,
        };
    }

    pub fn watch(self: *TraceMonitor) !void {
        // Use inotify to watch for new trace files
        const fd = try posix.inotify_init1(posix.linux.IN.CLOEXEC);
        defer posix.close(fd);

        const wd = try posix.inotify_add_watch(
            fd,
            self.trace_dir,
            posix.linux.IN.CREATE | posix.linux.IN.MODIFY,
        );
        defer _ = posix.inotify_rm_watch(fd, wd);

        std.debug.print("[Vordr] Monitoring traces at {s}\\n", .{self.trace_dir});

        while (true) {
            var buf: [4096]u8 = undefined;
            const n = try posix.read(fd, &buf);

            // Process inotify events
            try self.processTraces(buf[0..n]);
        }
    }

    fn processTraces(self: *TraceMonitor, events: []const u8) !void {
        // Parse trace files and check for anomalies
        // ...
    }
};
```

### Pattern 2: Periodic Integrity Checks

Vordr validates file checksums against database:

```zig
pub const IntegrityChecker = struct {
    pub fn verifySystem(self: *IntegrityChecker) !Report {
        const db = try self.loadPackageDatabase();
        var anomalies = std.ArrayList(Anomaly).init(self.allocator);

        for (db.packages) |pkg| {
            for (pkg.files) |file| {
                const actual_hash = try self.hashFile(file.path);
                if (!std.mem.eql(u8, actual_hash, file.expected_hash)) {
                    try anomalies.append(Anomaly{
                        .file = file.path,
                        .expected = file.expected_hash,
                        .actual = actual_hash,
                        .package = pkg.name,
                    });
                }
            }
        }

        return Report{ .anomalies = anomalies.toOwnedSlice() };
    }
};
```

### Pattern 3: Automatic Rollback

When Vordr detects unauthorized changes, trigger rollback:

```zig
pub fn handleAnomaly(anomaly: Anomaly) !void {
    std.debug.print(
        "[Vordr] ⚠️  Unauthorized change detected: {s}\\n",
        .{anomaly.file}
    );

    // Find the trace that modified this file
    const trace = try findTraceForFile(anomaly.file);

    if (trace.reversible) {
        std.debug.print(
            "[Vordr] 🔄 Initiating automatic rollback (trace: {s})\\n",
            .{trace.id}
        );

        // Call obli-pkg to rollback
        const result = try std.ChildProcess.exec(.{
            .allocator = allocator,
            .argv = &[_][]const u8{
                "/usr/bin/obli-pkg",
                "rollback",
                trace.id,
            },
        });

        if (result.term.Exited == 0) {
            std.debug.print("[Vordr] ✅ Rollback successful\\n", .{});
        }
    } else {
        std.debug.print("[Vordr] ❌ Cannot rollback - operation not reversible\\n", .{});
        // Alert human operator
        try sendAlert(anomaly);
    }
}
```

---

## Vordr-Specific Features

### CVE Monitoring
Track CVEs for minimal package set:

```zig
pub const CVEMonitor = struct {
    pub fn checkPackage(pkg: Package) ![]const CVE {
        // Query CVE database for this package
        // With only ~10 packages, this is fast!
        const cves = try queryCVEDatabase(pkg.name, pkg.version);
        return cves;
    }

    pub fn assessRisk(pkg: Package) !RiskLevel {
        const cves = try self.checkPackage(pkg);

        if (cves.len == 0) return .None;

        // Check severity
        for (cves) |cve| {
            if (cve.severity == .Critical) return .Critical;
        }

        return .Medium;
    }
};
```

### Behavioral Analysis
Detect anomalous package operations:

```zig
pub const BehaviorAnalyzer = struct {
    baseline: Baseline,

    pub fn analyze(self: *BehaviorAnalyzer, trace: Trace) !Verdict {
        // Check against baseline behavior
        if (trace.files_added.len > self.baseline.max_files_per_op) {
            return .Suspicious;  // Too many files added at once
        }

        if (trace.timestamp.hour < 6 or trace.timestamp.hour > 22) {
            return .Suspicious;  // Installation outside business hours
        }

        if (!trace.signature_verification.all_valid()) {
            return .Malicious;  // Failed signature check
        }

        return .Normal;
    }
};
```

### Compliance Reporting
Generate audit reports from traces:

```zig
pub fn generateComplianceReport(start: Date, end: Date) !Report {
    const traces = try loadTracesInRange(start, end);

    var report = Report.init(allocator);

    for (traces) |trace| {
        try report.addEntry(.{
            .timestamp = trace.timestamp,
            .operation = trace.operation,
            .package = trace.package,
            .user = trace.user,
            .verified = trace.signature_verification.all_valid(),
            .reversible = trace.reversible,
        });
    }

    return report;
}
```

---

## Accountability Trace Format

### Trace File Structure
**Path:** `/var/lib/obli-pkg/traces/<uuid>.scm`

```scheme
;; Full trace example
(trace
  ;; Identity
  (id "550e8400-e29b-41d4-a716-446655440000")
  (timestamp "2026-02-05T10:30:15Z")
  (operation "install")  ; or "remove", "upgrade"

  ;; Package information
  (package (
    (name "hello")
    (version "1.0.0")
    (file "hello-1.0.0.zpkg")
    (hash "9a8b7c6d...")))

  ;; Context
  (user "root")
  (hostname "production-01")
  (pid 1234)

  ;; Changes
  (files-added (
    ("/usr/bin/hello" (
      (size 29696)
      (mode 0755)
      (hash "9a8b7c6d5e4f3a2b1c0d9e8f7a6b5c4d")))))

  (files-modified ())
  (files-removed ())

  ;; Security
  (signature-verification (
    (dilithium5 (
      (status "VALID")
      (key-id "0x123456")))
    (ed448 (
      (status "VALID")
      (key-id "0xabcdef")))
    (sphincs (
      (status "VALID")
      (key-id "0x789abc")))))

  ;; Reversibility
  (reversible true)
  (rollback-script "/var/lib/obli-pkg/rollback/550e8400.obl")
  (dependencies-before ())
  (dependencies-after ()))
```

---

## Monitoring Dashboard

### Key Metrics for Vordr

```zig
pub const Metrics = struct {
    // System health
    total_packages: u32,
    total_files: u32,
    disk_usage: u64,  // bytes

    // Security
    last_integrity_check: Timestamp,
    anomalies_detected_24h: u32,
    failed_signature_verifications: u32,

    // Operations
    installs_24h: u32,
    removes_24h: u32,
    rollbacks_24h: u32,

    // Compliance
    audit_trail_complete: bool,
    all_operations_verified: bool,
};
```

### Sample Dashboard Output
```
┌─────────────────────────────────────────┐
│  Vordr Monitoring - Lago Grey System   │
├─────────────────────────────────────────┤
│ System Health                           │
│   Packages:      10                     │
│   Files:         47                     │
│   Disk Usage:    14.6 MB                │
│                                          │
│ Security (24h)                          │
│   Integrity:     ✅ PASS (1h ago)      │
│   Anomalies:     0                      │
│   Sig Failures:  0                      │
│                                          │
│ Operations (24h)                        │
│   Installs:      3                      │
│   Removes:       1                      │
│   Rollbacks:     0                      │
│                                          │
│ Compliance                              │
│   Audit Trail:   ✅ Complete           │
│   All Verified:  ✅ Yes                │
└─────────────────────────────────────────┘
```

---

## Alert Configuration

### Example Alert Rules
```yaml
# vordr-alerts.yaml
alerts:
  - name: UnauthorizedFileChange
    condition: file_modified AND signature_invalid
    severity: critical
    action: rollback

  - name: SuspiciousInstallTime
    condition: operation=install AND hour NOT IN [6..22]
    severity: warning
    action: notify

  - name: HighFileCount
    condition: files_added > 100
    severity: warning
    action: review

  - name: CVEDetected
    condition: cve_severity=critical
    severity: critical
    action: quarantine
```

---

## Example: Full Vordr Integration

```zig
// vordr-with-lago-grey.zig
const std = @import("std");

pub const Vordr = struct {
    monitor: TraceMonitor,
    integrity: IntegrityChecker,
    cve: CVEMonitor,
    behavior: BehaviorAnalyzer,

    pub fn init(allocator: std.mem.Allocator) !Vordr {
        return Vordr{
            .monitor = try TraceMonitor.init(allocator),
            .integrity = try IntegrityChecker.init(allocator),
            .cve = try CVEMonitor.init(allocator),
            .behavior = try BehaviorAnalyzer.init(allocator),
        };
    }

    pub fn run(self: *Vordr) !void {
        std.debug.print("[Vordr] Starting monitoring...\\n", .{});

        // Spawn monitoring threads
        const trace_thread = try std.Thread.spawn(.{},
            TraceMonitor.watch, .{&self.monitor});

        const integrity_thread = try std.Thread.spawn(.{},
            IntegrityChecker.periodicCheck, .{&self.integrity});

        const cve_thread = try std.Thread.spawn(.{},
            CVEMonitor.periodicScan, .{&self.cve});

        // Wait for threads
        trace_thread.join();
        integrity_thread.join();
        cve_thread.join();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var vordr = try Vordr.init(gpa.allocator());
    try vordr.run();
}
```

---

## Testing Integration

```bash
# Test trace monitoring
podman run --rm \
  -v /var/lib/obli-pkg:/var/lib/obli-pkg:ro \
  ghcr.io/hyperpolymath/vordr:latest \
  test-traces

# Test integrity checking
podman run --rm \
  -v /:/host:ro \
  ghcr.io/hyperpolymath/vordr:latest \
  check-integrity

# Simulate rollback
podman run --rm \
  -v /var/lib/obli-pkg:/var/lib/obli-pkg \
  ghcr.io/hyperpolymath/lago-grey:minimal \
  /usr/bin/obli-pkg rollback <trace-id>
```

---

## Next Steps

1. ✅ Read this integration guide
2. [ ] Set up trace directory monitoring
3. [ ] Configure integrity check schedule
4. [ ] Define alert rules
5. [ ] Test automatic rollback
6. [ ] Deploy Vordr to staging

---

**Back to:** [[Ecosystem Integration]]
