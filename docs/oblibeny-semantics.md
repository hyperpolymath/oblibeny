# Oblíbený Formal Semantics v0.6

<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
<!-- SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell -->

> Operational semantics for the Dual-Language Paradigm: compile-time
> Turing-complete Master Language → deploy-time Turing-incomplete
> Deployment Subset.

---

## 1. Semantic Domains

### 1.1 Values

```
v ∈ Value ::= n                     -- integers (i8, i16, i32, i64, u8, u16, u32, u64)
            | f                     -- floats (f32, f64)
            | b                     -- booleans (true, false)
            | s                     -- strings
            | (v₁, v₂, ..., vₙ)     -- tuples/structs
            | [v₁, v₂, ..., vₙ]     -- arrays (fixed size)
            | ptr(addr)             -- pointers
            | cap(token, budget)    -- capabilities
            | unit                  -- unit value
            | ⊥                     -- undefined/error
```

### 1.2 Types

```
τ ∈ Type ::= u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64
           | f32 | f64 | bool | string
           | (array τ n)            -- n is compile-time constant
           | (ptr τ)
           | (struct (f₁:τ₁) (f₂:τ₂) ...)
           | (enum id₁ id₂ ...)
           | (τ₁ × τ₂ × ... → τᵣ)   -- function type
           | cap                    -- capability token
```

### 1.3 Environments

```
Γ ∈ TypeEnv    = Identifier → Type
ρ ∈ ValueEnv   = Identifier → Value
σ ∈ Store      = Address → Value
κ ∈ CapEnv     = CapToken → (Budget × Permissions)
μ ∈ Memory     = { store: Store, next_addr: Address, used: Nat }
```

### 1.4 Execution Context

```
Ctx = {
  phase    : Phase,           -- compile-time | deploy-time
  env      : ValueEnv,        -- variable bindings
  types    : TypeEnv,         -- type bindings
  memory   : Memory,          -- heap state
  caps     : CapEnv,          -- capability budgets
  stack    : List<Frame>,     -- call stack
  bounds   : ResourceBounds,  -- deployment limits
  callgraph: CallGraph        -- for cycle detection
}

Phase ::= CompileTime | DeployTime

ResourceBounds = {
  max_iterations    : Nat,
  max_stack_depth   : Nat,
  max_memory        : Nat,
  max_call_depth    : Nat,
  max_execution_time: Nat
}
```

---

## 2. Operational Semantics

### 2.1 Notation

We use small-step operational semantics:

```
⟨e, ctx⟩ ⟶ ⟨e', ctx'⟩     -- expression e in context ctx steps to e' in ctx'
⟨s, ctx⟩ ⟶ₛ ctx'          -- statement s in context ctx produces ctx'
```

### 2.2 Expression Evaluation

#### Literals

```
─────────────────────────────
⟨n, ctx⟩ ⟶ ⟨n, ctx⟩         [E-Int]

─────────────────────────────
⟨true, ctx⟩ ⟶ ⟨true, ctx⟩   [E-True]

─────────────────────────────
⟨false, ctx⟩ ⟶ ⟨false, ctx⟩ [E-False]
```

#### Variables

```
ρ(x) = v
─────────────────────────────
⟨x, ctx[env=ρ]⟩ ⟶ ⟨v, ctx⟩  [E-Var]
```

#### Arithmetic

```
⟨e₁, ctx⟩ ⟶ ⟨e₁', ctx'⟩
───────────────────────────────────────────
⟨(op e₁ e₂), ctx⟩ ⟶ ⟨(op e₁' e₂), ctx'⟩    [E-ArithL]

⟨e₂, ctx⟩ ⟶ ⟨e₂', ctx'⟩
───────────────────────────────────────────
⟨(op v₁ e₂), ctx⟩ ⟶ ⟨(op v₁ e₂'), ctx'⟩    [E-ArithR]

op ∈ {+, -, *, /, %}    v = v₁ op v₂
───────────────────────────────────────────
⟨(op v₁ v₂), ctx⟩ ⟶ ⟨v, ctx⟩               [E-ArithOp]
```

#### Comparison

```
op ∈ {=, !=, <, >, <=, >=}    b = (v₁ op v₂)
──────────────────────────────────────────────
⟨(op v₁ v₂), ctx⟩ ⟶ ⟨b, ctx⟩                  [E-Compare]
```

#### Logical

```
⟨(and true e), ctx⟩ ⟶ ⟨e, ctx⟩              [E-AndTrue]
⟨(and false e), ctx⟩ ⟶ ⟨false, ctx⟩         [E-AndFalse]
⟨(or true e), ctx⟩ ⟶ ⟨true, ctx⟩            [E-OrTrue]
⟨(or false e), ctx⟩ ⟶ ⟨e, ctx⟩              [E-OrFalse]
⟨(not true), ctx⟩ ⟶ ⟨false, ctx⟩            [E-NotTrue]
⟨(not false), ctx⟩ ⟶ ⟨true, ctx⟩            [E-NotFalse]
```

#### Memory Access

```
σ(addr) = v
─────────────────────────────────────────────
⟨(mem addr), ctx[memory.store=σ]⟩ ⟶ ⟨v, ctx⟩ [E-MemRead]

addr' = addr + offset
─────────────────────────────────────────────────────
⟨(mem addr + offset), ctx⟩ ⟶ ⟨(mem addr'), ctx⟩      [E-MemOffset]
```

#### Capability Invocation

```
κ(token) = (budget, perms)    budget > 0
κ' = κ[token ↦ (budget - 1, perms)]
result = invoke_capability(token, args, perms)
──────────────────────────────────────────────────────
⟨(capability token args), ctx[caps=κ]⟩
    ⟶ ⟨result, ctx[caps=κ']⟩                          [E-CapInvoke]

κ(token) = (0, perms)
──────────────────────────────────────────────────────
⟨(capability token args), ctx[caps=κ]⟩ ⟶ ⟨⊥, ctx⟩    [E-CapExhausted]
```

### 2.3 Statement Evaluation

#### Variable Binding

```
⟨e, ctx⟩ ⟶* ⟨v, ctx'⟩    ρ' = ρ[x ↦ v]
────────────────────────────────────────────
⟨(let x e), ctx[env=ρ]⟩ ⟶ₛ ctx'[env=ρ']     [S-Let]
```

#### With Type Annotation (Deploy-Time Required)

```
ctx.phase = DeployTime    ⟨e, ctx⟩ ⟶* ⟨v, ctx'⟩
Γ ⊢ v : τ    ρ' = ρ[x ↦ v]    Γ' = Γ[x ↦ τ]
──────────────────────────────────────────────────
⟨(let x : τ e), ctx[env=ρ,types=Γ]⟩
    ⟶ₛ ctx'[env=ρ',types=Γ']                        [S-LetTyped]
```

#### Data Transfer

```
⟨e, ctx⟩ ⟶* ⟨v, ctx'⟩    ρ' = ρ[reg ↦ v]
──────────────────────────────────────────
⟨(mov reg e), ctx[env=ρ]⟩ ⟶ₛ ctx'[env=ρ'] [S-Mov]
```

#### Stack Operations

```
stack' = v :: ctx.stack
depth(stack') ≤ ctx.bounds.max_stack_depth
──────────────────────────────────────────────────
⟨(push v), ctx⟩ ⟶ₛ ctx[stack=stack']              [S-Push]

ctx.stack = v :: rest
─────────────────────────────────────────
⟨(pop reg), ctx⟩ ⟶ₛ ctx[stack=rest, env=ρ[reg↦v]] [S-Pop]
```

#### Memory Allocation (Deploy-Time: Constant Size Only)

```
ctx.phase = DeployTime    n is compile-time constant
ctx.memory.used + n ≤ ctx.bounds.max_memory
addr = ctx.memory.next_addr
μ' = { store=σ, next_addr=addr+n, used=ctx.memory.used+n }
ρ' = ρ[reg ↦ ptr(addr)]
────────────────────────────────────────────────────────────
⟨(alloc reg n), ctx[memory=μ,env=ρ]⟩ ⟶ₛ ctx[memory=μ',env=ρ'] [S-Alloc]
```

---

## 3. Type System

### 3.1 Typing Judgment

```
Γ ⊢ e : τ    -- expression e has type τ in environment Γ
```

### 3.2 Typing Rules

#### Literals

```
──────────────────
Γ ⊢ n : infer_int_type(n)    [T-Int]

─────────────
Γ ⊢ true : bool              [T-True]

──────────────
Γ ⊢ false : bool             [T-False]

─────────────
Γ ⊢ s : string               [T-String]
```

#### Variables

```
Γ(x) = τ
───────────
Γ ⊢ x : τ                    [T-Var]
```

#### Arithmetic

```
Γ ⊢ e₁ : τ    Γ ⊢ e₂ : τ    τ ∈ NumericTypes
─────────────────────────────────────────────
Γ ⊢ (op e₁ e₂) : τ    where op ∈ {+,-,*,/,%} [T-Arith]
```

#### Functions

```
Γ, x₁:τ₁, ..., xₙ:τₙ ⊢ body : τᵣ
────────────────────────────────────────────────────────
Γ ⊢ (defun f ((x₁:τ₁) ... (xₙ:τₙ)) : τᵣ body)
    : (τ₁ × ... × τₙ → τᵣ)                              [T-Fun]
```

#### Function Call

```
Γ ⊢ f : (τ₁ × ... × τₙ → τᵣ)    Γ ⊢ eᵢ : τᵢ for all i
──────────────────────────────────────────────────────
Γ ⊢ (call f (e₁ ... eₙ)) : τᵣ                          [T-Call]
```

#### Arrays

```
Γ ⊢ e : τ    n is compile-time constant
───────────────────────────────────────
Γ ⊢ (array τ n) : Type                  [T-ArrayType]

Γ ⊢ arr : (array τ n)    Γ ⊢ i : IntType    0 ≤ i < n
──────────────────────────────────────────────────────
Γ ⊢ arr[i] : τ                                         [T-ArrayAccess]
```

#### Memory

```
Γ ⊢ addr : (ptr τ)
──────────────────────
Γ ⊢ (mem addr) : τ     [T-MemRead]
```

#### Capability

```
Γ ⊢ token : cap    token grants permission P
──────────────────────────────────────────────
Γ ⊢ (capability token args) : result_type(P)  [T-Cap]
```

---

## 4. Termination Analysis

### 4.1 Call Graph Construction

```
CG : Program → CallGraph
CG(prog) = {
  nodes = all function definitions in prog,
  edges = { (f, g) | f calls g }
}
```

### 4.2 Cycle Detection (Tarjan's SCC)

```
has_cycle : CallGraph → Bool
has_cycle(cg) = ∃ scc ∈ tarjan_scc(cg). |scc| > 1 ∨ self_loop(scc)

-- DEPLOY-TIME CONSTRAINT
deploy_safe(prog) ⟹ ¬has_cycle(CG(prog))
```

### 4.3 Bounded Iteration Counting

```
count_iterations : Statement → Nat ∪ {∞}

count_iterations(bounded-for x a b body) =
  (b - a) × count_iterations(body)
  where a, b must be compile-time constants

count_iterations(while e body) = ∞   -- Compile-time only!

count_iterations(for x e body) = ∞   -- Compile-time only!

count_iterations(s₁; s₂) =
  count_iterations(s₁) + count_iterations(s₂)

count_iterations(if e s₁ s₂) =
  max(count_iterations(s₁), count_iterations(s₂))
```

### 4.4 Termination Proof Verification

```
verify_terminates : Function × Strategy → Bool

verify_terminates(f, bounded-loop) =
  all_loops_bounded(f.body) ∧ count_iterations(f.body) < ∞

verify_terminates(f, structural-recursion) =
  -- Compile-time only: verify decreasing argument
  has_decreasing_measure(f)

verify_terminates(f, well-founded-order) =
  -- Requires explicit ranking function
  ∃ rank. well_founded(rank) ∧ decreases_on_call(f, rank)
```

---

## 5. Phase Separation Semantics

### 5.1 Phase Classification

```
Phase ::= CompileTime | DeployTime

phase_of : Construct → Phase

-- Compile-Time Only Constructs
phase_of(while _ _) = CompileTime
phase_of(for _ _ _) = CompileTime
phase_of(syscall _) = CompileTime
phase_of(transaction _ _) = CompileTime
phase_of(metamorphic-if _ _ _) = CompileTime
phase_of(try _ _ _) = CompileTime

-- Deploy-Time Allowed Constructs
phase_of(bounded-for _ _ _ _) = DeployTime
phase_of(capability _ _) = DeployTime
phase_of(mov _ _) = DeployTime
phase_of(add _ _) = DeployTime
-- ... other bounded operations
```

### 5.2 Phase Checking Rules

```
ctx.phase = CompileTime
──────────────────────────────────
⟨(compile-time body), ctx⟩ ⟶ₛ eval(body, ctx)    [P-CompileBlock]

ctx.phase = DeployTime    all_deploy_safe(body)
──────────────────────────────────────────────────
⟨(deploy-time body), ctx⟩ ⟶ₛ eval(body, ctx)     [P-DeployBlock]

ctx.phase = DeployTime    phase_of(s) = CompileTime
──────────────────────────────────────────────────────
⟨s, ctx⟩ ⟶ₛ ERROR("compile-only construct in deploy-time") [P-PhaseError]
```

### 5.3 Function Deployment Tags

```
deploy_callable : Function → Bool

deploy_callable(f) =
  f.tag ∈ {runtime-only, both} ∧
  verify_terminates(f, f.strategy) ∧
  all_deploy_safe(f.body)
```

---

## 6. Resource Semantics

### 6.1 Static Resource Analysis

```
Resources = {
  iterations : Nat,
  stack_depth : Int,
  memory : Nat,
  call_depth : Nat
}

analyze : Statement → Resources

analyze(bounded-for x a b body) = {
  iterations = (b - a) × analyze(body).iterations,
  stack_depth = analyze(body).stack_depth,
  memory = analyze(body).memory,
  call_depth = analyze(body).call_depth
}

analyze(push _) = { stack_depth = +1, ... }
analyze(pop _) = { stack_depth = -1, ... }
analyze(alloc _ n) = { memory = n, ... }
analyze(call f _) = { call_depth = +1, ... } ⊕ analyze(f.body)
```

### 6.2 Bound Verification

```
verify_bounds : Program × ResourceBounds → Bool

verify_bounds(prog, bounds) =
  let r = analyze(prog.main) in
  r.iterations ≤ bounds.max_iterations ∧
  max_stack(r) ≤ bounds.max_stack_depth ∧
  r.memory ≤ bounds.max_memory ∧
  max_call_depth(prog) ≤ bounds.max_call_depth
```

### 6.3 Maximal Principle Reduction (MPR)

The core insight: for any deploy-time function, we can compute the **maximal** resource usage statically because:

1. All loops are bounded with compile-time constant bounds
2. All memory allocations are compile-time constant size
3. Call graph is acyclic (finite call depth)
4. No recursion (no stack explosion)

```
MPR : DeployFunction → ResourceCertificate

MPR(f) = {
  max_iterations = Π (b - a) for all bounded-for a b in f,
  max_stack = max_stack_path(f),
  max_memory = Σ alloc_size for all alloc in f,
  max_calls = longest_path(callgraph(f)),
  terminates = true  -- By construction!
}
```

---

## 7. Obfuscation Semantics

### 7.1 Semantic Equivalence

Two programs P₁ and P₂ are semantically equivalent iff:

```
P₁ ≈ P₂  ⟺  ∀ input. eval(P₁, input) = eval(P₂, input)
```

### 7.2 Obfuscation Transformation

```
Obfuscate : Program × Level × Seed → Program

-- Correctness requirement
∀ P, level, seed. P ≈ Obfuscate(P, level, seed)

-- Diversity requirement
∀ P, level, s₁ ≠ s₂.
  syntactic_distance(Obfuscate(P, level, s₁),
                     Obfuscate(P, level, s₂)) > threshold(level)
```

### 7.3 Transformation Rules (Semantic-Preserving)

```
-- Arithmetic Substitution
(+ a b) ≈ (- a (- 0 b))
(* a 2) ≈ (<< a 1)
(/ a 2) ≈ (>> a 1)

-- Logic Substitution
(and a b) ≈ (not (or (not a) (not b)))
(or a b) ≈ (not (and (not a) (not b)))

-- Control Flow
(if c t e) ≈ (if (not c) e t)

-- Dead Code Insertion (opaque predicate)
body ≈ (if (= (* 7 11) 77) body unreachable_code)

-- Instruction Reordering (when independent)
(s₁; s₂) ≈ (s₂; s₁)  when independent(s₁, s₂)
```

### 7.4 Metamorphic Transformation

```
Metamorph : Program × Seed → Program

Metamorph(P, seed) =
  let rng = init_rng(seed) in
  let transforms = select_transforms(rng, P) in
  fold(apply_transform, P, transforms)

-- Each deployment gets different seed → different binary
-- Same semantics, different syntax
```

---

## 8. Error Semantics

### 8.1 Compile-Time Errors

```
CompileError ::=
  | PhaseViolation(construct, expected_phase)
  | RecursionDetected(function, cycle_path)
  | UnboundedLoop(location)
  | TypeMismatch(expected, actual, location)
  | ResourceExceeded(resource, limit, actual)
  | UndefinedVariable(name, location)
  | TerminationUnprovable(function)
```

### 8.2 Deploy-Time Errors

```
DeployError ::=
  | CapabilityExhausted(token)
  | MemoryExhausted
  | StackOverflow           -- Should be statically prevented
  | ChecksumFailed          -- Anti-tampering triggered
  | HardwareBindingFailed   -- Wrong device
```

---

## 9. Deployment Semantics

### 9.1 Deployment Pipeline

```
Deploy : Source × DeploymentSpec → Binary

Deploy(src, spec) =
  let ast = parse(src) in
  let expanded = expand_macros(ast) in           -- Stage 0
  let typed = typecheck(expanded) in
  let verified = verify_all(typed, spec) in      -- Stage 1
  let optimized = optimize(verified) in
  let obfuscated = obfuscate(optimized, spec.obfuscation) in
  let binary = codegen(obfuscated, spec.target) in  -- Stage 2
  embed_limits(binary, spec.bounds)
```

### 9.2 Verification Requirements

```
verify_all : TypedAST × DeploymentSpec → VerifiedAST

verify_all(ast, spec) =
  assert(phase_correct(ast));
  assert(no_recursion(ast));
  assert(all_loops_bounded(ast));
  assert(all_termination_proofs_valid(ast));
  assert(resource_bounds_satisfied(ast, spec.bounds));
  assert(capabilities_available(ast, spec.manifest));
  return ast with proofs
```

---

## 10. Formal Properties

### 10.1 Type Safety

**Theorem (Progress):** If `Γ ⊢ e : τ` and `e` is not a value, then `∃ e'. ⟨e, ctx⟩ ⟶ ⟨e', ctx'⟩`.

**Theorem (Preservation):** If `Γ ⊢ e : τ` and `⟨e, ctx⟩ ⟶ ⟨e', ctx'⟩`, then `Γ' ⊢ e' : τ` for some `Γ' ⊇ Γ`.

### 10.2 Termination Guarantee

**Theorem (Deploy-Time Termination):** For any deploy-time program P that passes verification:

```
∀ input. ∃ n ∈ ℕ. eval(P, input) terminates within n steps
```

where `n ≤ MPR(P).max_iterations`.

**Proof Sketch:** By structural induction on the restricted grammar. The Deployment Subset excludes all sources of non-termination (unbounded loops, recursion, while). The bounded-for construct has compile-time constant bounds, and the call graph is acyclic.

### 10.3 Resource Boundedness

**Theorem (Static Resource Bounds):** For any verified deploy-time program P with bounds B:

```
∀ execution of P:
  iterations_used ≤ B.max_iterations
  stack_depth ≤ B.max_stack_depth
  memory_used ≤ B.max_memory
  call_depth ≤ B.max_call_depth
```

### 10.4 Obfuscation Soundness

**Theorem (Semantic Preservation):** For all obfuscation levels and seeds:

```
∀ P, level, seed, input.
  eval(P, input) = eval(Obfuscate(P, level, seed), input)
```

---

## Appendix A: Grammar to Semantics Mapping

| Grammar Construct | Semantic Rule | Section |
|-------------------|---------------|---------|
| `literal_expr` | E-Int, E-True, E-False | §2.2 |
| `variable_expr` | E-Var | §2.2 |
| `arithmetic_expr` | E-ArithL, E-ArithR, E-ArithOp | §2.2 |
| `comparison_expr` | E-Compare | §2.2 |
| `logical_expr` | E-And*, E-Or*, E-Not* | §2.2 |
| `memory_expr` | E-MemRead, E-MemOffset | §2.2 |
| `capability_invoke` | E-CapInvoke, E-CapExhausted | §2.2 |
| `local_data_def` | S-Let, S-LetTyped | §2.3 |
| `data_transfer_op` | S-Mov | §2.3 |
| `stack_op` | S-Push, S-Pop | §2.3 |
| `bounded_memory_op` | S-Alloc | §2.3 |
| `bounded_loop` | count_iterations | §4.3 |
| `function_def` | T-Fun | §3.2 |
| `function_call` | T-Call | §3.2 |
| `compile_time_block` | P-CompileBlock | §5.2 |
| `deploy_time_block` | P-DeployBlock | §5.2 |
| `deployment_spec` | Deploy | §9.1 |
| `obfuscation_level` | Obfuscate | §7.2 |

---

## Appendix B: Rust Implementation Mapping

| Semantic Domain | Rust Module | Primary Types |
|-----------------|-------------|---------------|
| Value | `value.rs` | `enum Value { Int, Float, Bool, ... }` |
| Type | `types.rs` | `enum Type { U8, I32, Array, Ptr, ... }` |
| TypeEnv | `env.rs` | `HashMap<Ident, Type>` |
| ValueEnv | `env.rs` | `HashMap<Ident, Value>` |
| Store | `memory.rs` | `HashMap<Address, Value>` |
| CapEnv | `capability.rs` | `HashMap<CapToken, CapState>` |
| Context | `context.rs` | `struct Context { ... }` |
| CallGraph | `callgraph.rs` | `petgraph::DiGraph` |
| ResourceBounds | `bounds.rs` | `struct ResourceBounds { ... }` |

---

*End of Formal Semantics*
