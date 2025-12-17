# Capability Model

Oblíbený implements a capability-based security model for all I/O operations in deploy-time code. No syscalls are permitted; all external interactions require explicit capability tokens.

---

## Table of Contents

1. [Overview](#overview)
2. [Capability Tokens](#capability-tokens)
3. [Capability Types](#capability-types)
4. [Granting Capabilities](#granting-capabilities)
5. [Using Capabilities](#using-capabilities)
6. [Budgets and Limits](#budgets-and-limits)
7. [Capability Composition](#capability-composition)
8. [Security Properties](#security-properties)

---

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    DEPLOYMENT MANIFEST                       │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ Granted Capabilities:                                    ││
│  │   • sensor:read (budget: 1000/hour)                      ││
│  │   • actuator:write (budget: 100/hour)                    ││
│  │   • log:write (budget: unlimited)                        ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    RUNTIME ENFORCEMENT                       │
│                                                              │
│  ┌─────────┐    ┌──────────────┐    ┌─────────────┐        │
│  │  Code   │───▶│  Capability  │───▶│  Hardware/  │        │
│  │         │    │   Checker    │    │   Driver    │        │
│  └─────────┘    └──────────────┘    └─────────────┘        │
│                        │                                     │
│                        ▼                                     │
│                 Budget Tracking                              │
└─────────────────────────────────────────────────────────────┘
```

### Why Capabilities?

1. **Explicit Authority**: All permissions are visible in code
2. **Principle of Least Privilege**: Only grant what's needed
3. **No Ambient Authority**: No implicit access to resources
4. **Auditable**: Easy to review what a program can do
5. **Composable**: Capabilities can be attenuated and delegated

---

## Capability Tokens

Capability tokens are unforgeable references to system resources.

### Token Structure

```lisp
;; Capability token type (opaque to user code)
(deftype capability-token
  :resource-type keyword     ; :sensor, :actuator, :log, etc.
  :resource-id   u64         ; Specific resource instance
  :permissions   permission-set
  :budget        (option budget-spec)
  :expiry        (option timestamp)
  :signature     bytes)      ; Cryptographic signature
```

### Token Properties

| Property | Description |
|----------|-------------|
| Unforgeable | Cannot be created by user code |
| Attenuable | Can be restricted but not elevated |
| Revocable | Can be invalidated by issuer |
| Auditable | All uses are logged |

---

## Capability Types

### Sensor Capabilities

```lisp
;; Read from sensor
(defcap sensor-read
  :permissions (:read)
  :resources (temperature humidity pressure)
  :rate-limit (100 :per :second))

;; Usage
(defun read-temperature (cap :type (cap sensor-read))
  #:deploy
  (invoke-capability :read cap))
```

### Actuator Capabilities

```lisp
;; Write to actuator
(defcap actuator-write
  :permissions (:write)
  :resources (motor valve led)
  :rate-limit (10 :per :second)
  :value-range (0 255))

;; Usage
(defun set-motor-speed (cap :type (cap actuator-write)
                        speed :type u8)
  #:deploy
  (invoke-capability :write cap speed))
```

### Communication Capabilities

```lisp
;; Network send
(defcap net-send
  :permissions (:send)
  :destinations ("192.168.1.0/24")
  :protocols (:udp)
  :max-packet-size 256
  :rate-limit (10 :per :second))

;; Network receive
(defcap net-recv
  :permissions (:receive)
  :sources ("192.168.1.0/24")
  :protocols (:udp)
  :buffer-size 256)
```

### Storage Capabilities

```lisp
;; Persistent storage
(defcap storage-rw
  :permissions (:read :write)
  :region (0x1000 0x2000)  ; Address range
  :max-write-size 64)

;; Usage
(defun save-calibration (cap :type (cap storage-rw)
                         data :type (array u8 32))
  #:deploy
  (invoke-capability :write cap 0x1000 data))
```

### Logging Capabilities

```lisp
;; Log output
(defcap log-write
  :permissions (:write)
  :levels (:debug :info :warn :error)
  :max-message-size 128
  :rate-limit (100 :per :second))
```

---

## Granting Capabilities

Capabilities are granted in the deployment manifest.

### Package Manifest

```lisp
(package sensor-node
  :version "1.0.0"

  :capabilities
  (;; Request capabilities needed
   (request sensor-read
     :resources (temperature)
     :justification "Read temperature for monitoring")

   (request actuator-write
     :resources (heater)
     :justification "Control heater based on temperature")

   (request log-write
     :levels (:info :warn :error)
     :justification "Log operational status")))
```

### Deployment Manifest

```lisp
(deployment sensor-node
  :target "edge-device-001"

  :grant-capabilities
  ((sensor-read
     :resource-id 0x01
     :budget (:calls 1000 :period :hour))

   (actuator-write
     :resource-id 0x02
     :budget (:calls 100 :period :hour)
     :value-range (0 100))  ; Restrict range further

   (log-write
     :budget :unlimited)))
```

### Programmatic Granting (Compile-Time)

```lisp
(compile-time
  ;; Generate capability grants based on configuration
  (defun generate-grants (config)
    (for (sensor (config :sensors))
      (grant-capability
        :type sensor-read
        :resource-id (sensor :id)
        :to (sensor :handler-fn)))))
```

---

## Using Capabilities

### Invoking Capabilities

```lisp
;; Basic invocation
(invoke-capability :read sensor-cap)

;; With arguments
(invoke-capability :write actuator-cap value)

;; With options
(invoke-capability :send net-cap message
  :timeout 100
  :retry false)
```

### Capability Checking

```lisp
;; Check if capability is valid
(when (capability-valid? sensor-cap)
  (invoke-capability :read sensor-cap))

;; Check remaining budget
(let ((remaining (capability-budget-remaining sensor-cap)))
  (when (> remaining 0)
    (invoke-capability :read sensor-cap)))
```

### Capability Parameters

```lisp
;; Functions that require capabilities
(defun process-sensor (cap :type (cap sensor-read))
  #:deploy
  (let ((reading (invoke-capability :read cap)))
    (process reading)))

;; Calling with capability
(process-sensor my-sensor-cap)
```

---

## Budgets and Limits

### Budget Types

```lisp
;; Call count budget
(budget :calls 1000 :period :hour)

;; Data volume budget
(budget :bytes 1048576 :period :day)

;; Combined budget
(budget :calls 100 :bytes 10240 :period :minute)

;; Unlimited
(budget :unlimited)
```

### Budget Tracking

```lisp
;; Query budget status
(capability-budget-remaining cap)     ; Remaining calls/bytes
(capability-budget-period-end cap)    ; When budget resets
(capability-budget-used cap)          ; Used in current period

;; Example
(defun safe-read (cap)
  #:deploy
  (if (> (capability-budget-remaining cap) 0)
      (Some (invoke-capability :read cap))
      None))
```

### Rate Limiting

```lisp
;; Rate-limited capability
(defcap rate-limited-sensor
  :permissions (:read)
  :rate-limit (10 :per :second)    ; Max 10 calls/sec
  :burst 20)                        ; Allow burst of 20

;; Enforcement is automatic
(invoke-capability :read rate-limited-cap)
;; Blocks if rate limit exceeded
```

---

## Capability Composition

### Attenuation

```lisp
;; Reduce capability permissions
(defun attenuate-for-child (parent-cap)
  #:deploy
  (attenuate parent-cap
    :reduce-budget-by 0.5    ; Half the budget
    :restrict-range (10 50)  ; Narrower value range
    :add-expiry (+ (now) 3600)))  ; 1 hour expiry
```

### Delegation

```lisp
;; Delegate capability to sub-component
(defun delegate-sensor-access (master-cap component-id)
  #:deploy
  (let ((child-cap (attenuate master-cap
                     :reduce-budget-by 0.1)))
    (register-component-cap component-id child-cap)))
```

### Revocation

```lisp
;; Revoke a delegated capability
(revoke-capability delegated-cap)

;; Check if revoked
(when (not (capability-revoked? cap))
  (invoke-capability :read cap))
```

---

## Security Properties

### Formal Guarantees

1. **Confinement**: Code cannot access resources without capability
2. **No Escalation**: Capabilities cannot be elevated
3. **Transitivity**: Delegation cannot exceed original permissions
4. **Accountability**: All capability uses are traceable

### Verification

```lisp
;; Compile-time capability analysis
(defun verified-function (sensor-cap :type (cap sensor-read)
                          actuator-cap :type (cap actuator-write))
  #:deploy
  #:capabilities (sensor-read actuator-write)  ; Declare required

  ;; Compiler verifies:
  ;; 1. Only declared capabilities are used
  ;; 2. No capability forgery
  ;; 3. No capability leakage
  ;; 4. Budget constraints respected

  (let ((reading (invoke-capability :read sensor-cap)))
    (when (> reading threshold)
      (invoke-capability :write actuator-cap 0))))
```

### Audit Trail

```lisp
;; All capability invocations are logged
;; Format: (timestamp capability-id operation result)

;; Query audit log (compile-time only)
(compile-time
  (defun analyze-capability-usage (cap-id time-range)
    (let ((log (get-capability-audit-log cap-id time-range)))
      (summarize-usage log))))
```

---

## Built-in Capabilities

| Capability | Operations | Typical Budget |
|------------|------------|----------------|
| `sensor-read` | `:read` | 1000/hour |
| `actuator-write` | `:write` | 100/hour |
| `log-write` | `:write` | unlimited |
| `storage-read` | `:read` | 10000/hour |
| `storage-write` | `:write` | 1000/hour |
| `net-send` | `:send` | 100/min |
| `net-recv` | `:receive` | 100/min |
| `time-read` | `:read` | unlimited |
| `random` | `:generate` | 1000/sec |

---

## Example: Complete Sensor Node

```lisp
(package temperature-monitor
  :version "1.0.0"

  :capabilities
  ((request sensor-read :resources (temp-sensor))
   (request actuator-write :resources (heater fan))
   (request log-write)))

(defmodule main
  (import std.core)

  (defconst TEMP_LOW 18.0)
  (defconst TEMP_HIGH 25.0)

  (defun control-loop (sensor-cap heater-cap fan-cap log-cap)
    #:deploy
    #:capabilities (sensor-read actuator-write log-write)

    (let ((temp (invoke-capability :read sensor-cap)))
      (cond
        ((< temp TEMP_LOW)
         (invoke-capability :write heater-cap 255)
         (invoke-capability :write fan-cap 0)
         (invoke-capability :write log-cap "Heating"))

        ((> temp TEMP_HIGH)
         (invoke-capability :write heater-cap 0)
         (invoke-capability :write fan-cap 255)
         (invoke-capability :write log-cap "Cooling"))

        (true
         (invoke-capability :write heater-cap 0)
         (invoke-capability :write fan-cap 0))))))
```

---

## Next Steps

- [Resource Bounds](Resource-Bounds.md)
- [Security Constraints](Security-Constraints.md)
- [Deployment Specification](Deployment-Specification.md)
