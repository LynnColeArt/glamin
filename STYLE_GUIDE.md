# GLAMIN Coding Standards & Style Guide

**Geometric Logical Application Meta-Instruction Network**

> "Code is read far more often than it is written." — Guido van Rossum
> "Yeah, but does it have to be boring?" — Lynn Cole

---

## 🎯 Core Principles

1. **Strong Typing Always** — Every variable knows exactly what it is
2. **Explicit is Better than Implicit** — No guessing games, ever
3. **Elegance Without Elitism** — Beautiful code that a grad student can understand
4. **Consistency Without Conformity** — Same patterns, but room to breathe
5. **Self-Documenting** — If you need a decoder ring, rewrite it
6. **Geometrically Honest** — Logic has shape. Respect the manifold.

---

## 📐 Module Structure

Every module follows this layout. Deviations require a comment explaining why.

```fortran
module glamin_example
  ! 1. Use statements (like a civilized person)
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding,   only: c_ptr, c_int
  use glamin_types
  use glamin_memory

  ! 2. implicit none ALWAYS (because we are not psychics)
  implicit none

  ! 3. Access control (private by default, we are not exhibitionists)
  private
  public :: public_type, public_function

  ! 4. Module parameters (constants that actually make sense)
  integer,      parameter :: CACHE_LINE_SIZE = 64
  real(real64), parameter :: EPSILON         = 1.0e-12_real64  ! Not just "small number"

  ! 5. Type definitions (structs, but cooler)
  type :: example_type
    integer(int64) :: count    = 0              ! Always initialize
    real(real64)   :: value    = 0.0_real64     ! Seriously, always
    logical        :: is_valid = .false.         ! No uninitialized surprises
  contains
    procedure :: init    => example_init
    procedure :: compute => example_compute
  end type example_type

contains
  ! Implementation here (the fun part)
end module glamin_example
```

---

## 🔤 Naming Conventions

> If the name would make sense in a CRUD app, it is not specific enough for Glamin.

| CRUD (bad) | Glamin (good) |
|---|---|
| `get_record` | `resolve_nearest_mint` |
| `update_value` | `collapse_corridor_to_decision` |
| `delete_item` | `evict_mint_from_manifold` |
| `process_data` | `trace_input_through_manifold` |
| `result` | `conf_auth_decision` |

### Types

- **PascalCase** for derived types: `DeviceHandle`, `MemoryPool`, `ManifoldContext`
- No cute abbreviations: `MessageQueue` not `MsgQ`
- `_t` suffix for enums only (tradition)
- `_behavior` suffix for behavioral intent types
- `_mint` suffix for meta-instruction types (Meta-INstruction Type)

### Variables

- **snake_case** always: `device_count`, `memory_size`, `dist_to_boundary`
- Full words: `buffer_size` not `buf_sz`
- Booleans read like English: `is_ready`, `has_data`, `can_proceed`
- Distance variables say what they measure FROM: `dist_to_boundary`, `dist_to_nearest_mint`
- Confidence values prefixed `conf_`: `conf_auth_decision`, `conf_input_valid`

### Functions & Subroutines

- **snake_case** that tells a story: `allocate_memory`, `resolve_nearest_mint`
- Verb-first like you're giving orders: `create_device`, `collapse_corridor`, `evict_mint`
- Questions get question names: `is_valid()`, `has_capability()`, `should_escalate()`

### Constants

- **SCREAMING_SNAKE_CASE** because they're important: `MAX_DEVICES`, `DEFAULT_TIMEOUT_MS`
- Include units or you're fired: `TIMEOUT_MS` not `TIMEOUT`
- No magic numbers lurking: `GIGABYTE` not `1073741824`
- Corridor widths always annotated with risk rationale (see Corridors section)

### Module Names

- **snake_case** with `glamin_` prefix: `glamin_memory`, `glamin_manifold`, `glamin_scheduler`
- Say what it does: `glamin_scheduler` not `glamin_utils` (everything is utils)

---

## 💪 Strong Typing Rules

### Always Specify Kind

```fortran
! Good - we know exactly what we're getting
integer(int32) :: device_count       ! 32-bit integer
integer(int64) :: memory_size        ! 64-bit for big allocations
real(real32)   :: performance_score  ! Single precision is fine here
real(real64)   :: precision_value    ! Double for when it matters
real(real64)   :: conf_auth_decision = 0.0_real64  ! Confidence: always 0.0-1.0

! Bad - playing precision roulette
integer :: count  ! 16-bit? 32-bit? 64-bit? Who knows!
real    :: value  ! Single? Double? Roll the dice!
```

### Use Typed Constants

```fortran
! Good - constants that explain themselves
integer(int64), parameter :: GIGABYTE = 1024_int64**3
real(real64),   parameter :: PI       = 3.14159265359_real64

! Bad - mystery numbers from the void
size = 1073741824  ! Is this bytes? Megabytes? Bananas?
```

### Enumerations: Because Magic Numbers Are Evil

```fortran
enum, bind(c)
  enumerator :: DEVICE_READY   = 0
  enumerator :: DEVICE_BUSY    = 1
  enumerator :: DEVICE_ON_FIRE = 2  ! It happens

  ! Confidence tiers - humans think in bands, not raw floats
  enumerator :: CONF_CERTAIN  = 0   ! > 0.95    - act deterministically
  enumerator :: CONF_HIGH     = 1   ! 0.80-0.95 - act, but log it
  enumerator :: CONF_MODERATE = 2   ! 0.60-0.80 - act with caution
  enumerator :: CONF_LOW      = 3   ! 0.40-0.60 - escalate or interpolate
  enumerator :: CONF_LOST     = 4   ! < 0.40    - we are genuinely in the fog
end enum
```

---

## 🧠 Behavioral & Meta-Instruction Conventions

Glamin introduces concepts no coding standard has ever needed to cover. These aren't data structures. They aren't functions. They're something new — so we name them like something new.

### The Core Vocabulary

- A **behavior** is a discrete unit of logical intent. Not a function — a function is an implementation. A behavior is what the function *means*.
- A **meta-instruction** (`mint`) is a behavior embedded into vector space. It has coordinates, neighbors, and distance from other mints. It is alive in the manifold.
- A **corridor** is the confidence region around a decision boundary. Wide = uncertainty. Narrow = conviction.
- A **trace** is the path an input takes through the manifold. Not a call stack. A trajectory.

### Naming Behavioral Types

```fortran
! Behaviors use _behavior suffix - describe intent, not implementation
type :: validate_input_behavior
type :: retry_with_backoff_behavior
type :: fanout_collect_behavior

! Meta-instructions use _mint suffix (Meta-INstruction Type)
! because life is short and mint is fresh
type :: validate_input_mint
type :: auth_flow_mint
type :: rate_limit_mint
```

### Naming Corridors and Boundaries

```fortran
! Corridors named for what they decide, annotated with risk rationale
real(real64), parameter :: AUTH_CORRIDOR_WIDTH      = 0.15_real64  ! Tight - auth matters
real(real64), parameter :: RECOMMEND_CORRIDOR_WIDTH  = 0.45_real64  ! Loose - suggestions are fine
real(real64), parameter :: PAYMENT_CORRIDOR_WIDTH    = 0.05_real64  ! Near zero - no gambling with money

! Boundaries named for what they separate: boundary_[below]_[above]
real(real64) :: boundary_deny_allow
real(real64) :: boundary_cache_fetch
real(real64) :: boundary_sync_async
```

### Naming Distance Variables

```fortran
! Distance always says what it is distance FROM
! dist_to_[thing]
real(real64) :: dist_to_boundary      ! How close to a decision edge
real(real64) :: dist_to_nearest_mint  ! Closest behavioral neighbor
real(real64) :: dist_to_origin        ! Absolute position in manifold

! Never:
real(real64) :: distance  ! Distance to WHAT? We don't play this game.
real(real64) :: d         ! You're fired.
```

### Naming Traces and Trajectories

```fortran
! Traces: verb-noun describing the journey
type :: auth_resolution_trace
type :: input_validation_trace
type :: fallback_escalation_trace

! Trajectory vectors: traj_[from]_[toward]
real(real64) :: traj_uncertain_toward_confident(MANIFOLD_DIM)
real(real64) :: traj_valid_toward_invalid(MANIFOLD_DIM)
```

### The Manifold Context

```fortran
integer(int32), parameter :: MANIFOLD_DIM         = 1536
integer(int32), parameter :: MANIFOLD_SHARD_COUNT  = 16
real(real64),   parameter :: MANIFOLD_BOUNDARY_EPS = 1.0e-6_real64  ! Below this, we're ON the boundary

! Name your manifold like a ship - it deserves a name
type :: manifold_context
  integer(int64)                :: version   = 0
  integer(int32)                :: dim       = MANIFOLD_DIM
  logical                       :: is_frozen = .false.  ! Frozen = read only, no exceptions
  character(len=:), allocatable :: name                 ! Name this thing
end type manifold_context
```

> **The Golden Rule:** If the name would make sense in a CRUD app, it's not specific enough for Glamin. Names must describe position, distance, confidence, behavior, or trajectory — not just data.

---

## 🎨 Code Layout

- 2 spaces per level (tabs are for monsters)
- 100 characters max — we have wide screens now, it's not 1980
- One blank line between procedures (they need personal space)
- Two blank lines between major sections
- Align related declarations — it's a satisfaction video

```fortran
type :: compute_kernel
  integer(int64)                :: work_items    = 0
  integer(int32)                :: block_size    = 256
  real(real64)                  :: elapsed_time  = 0.0_real64
  character(len=:), allocatable :: name
  logical                       :: is_optimized  = .false.
end type
```

---

## 🛡️ Defensive Programming

The system knows what it doesn't know. Surface uncertainty as a first-class value. A traditional if-statement will confidently take the wrong branch on an edge case and never tell you. Glamin never does this.

### Always Check Allocations

```fortran
allocate(buffer(n), stat=ierr)
if (ierr /= 0) then
  error stop "Failed to allocate buffer - probably need more RAM"
end if
```

### Validate Inputs Like You're Paranoid

```fortran
pure function safe_divide(a, b) result(c)
  real(real64), intent(in) :: a, b
  real(real64) :: c

  if (abs(b) < epsilon(b)) then
    c = 0.0_real64  ! Better than infinity
  else
    c = a / b
  end if
end function
```

### Confidence Corridors: Know Your Certainty

```fortran
! At every decision boundary, make certainty explicit
if (dist_to_boundary > CORRIDOR_WIDTH) then
  ! Deterministic - classical behavior, far from the edge
  call handle_auth_deterministic(request, status)

else if (dist_to_boundary < MANIFOLD_BOUNDARY_EPS) then
  ! On the boundary - escalate, never guess
  call escalate_to_human(request, status)

else
  ! Inside corridor - interpolate, log, proceed with caution
  conf_auth_decision = compute_confidence(dist_to_boundary, CORRIDOR_WIDTH)
  call handle_auth_probabilistic(request, conf_auth_decision, status)
end if
```

### Initialize Everything (Seriously, Everything)

```fortran
type :: device_stats
  integer(int64) :: total_memory    = 0           ! Not garbage
  integer(int64) :: used_memory     = 0           ! Not random
  real(real32)   :: temperature     = 0.0_real32  ! Not on fire
  logical        :: is_initialized  = .false.     ! Explicitly not ready
end type
```

---

## 📝 Documentation

Comments explain WHY, not WHAT. The code already shows what.

```fortran
! BAD: Increment i by 1
i = i + 1

! GOOD: Skip header row in CSV
i = i + 1

! BETTER: Skip header row (parser expects data starting at row 2)
i = i + 1

! GLAMIN LEVEL: Advance manifold cursor past calibration vectors
! (first CALIBRATION_COUNT rows are reference embeddings, not query data)
i = i + CALIBRATION_COUNT
```

### Module Documentation

```fortran
!> @brief High-performance mint resolver for behavioral queries
!>
!> Resolves nearest behavioral neighbors in the manifold with
!> corridor-aware confidence scoring. Deterministic outside corridors,
!> probabilistic within. Always tells you which one you got.
!>
!> Example:
!>   result = resolve_nearest_mint(manifold, query_vec, AUTH_CORRIDOR_WIDTH)
!>   if (result%confidence_tier == CONF_LOST) call escalate()
module glamin_resolver
```

---

## ⚡ Performance Patterns

### Let Fortran Be Fortran

```fortran
! Good - compiler fuses, vectorizes, does all the goodies
result = a * b + c

! Bad - you outsmarted yourself out of SIMD
do i = 1, n
  result(i) = a(i) * b(i) + c(i)  ! Why did you do this?
end do
```

### Allocate Once, Compute Many

```fortran
! Good - reusable workspace, allocation is a one-time cost
type :: manifold_query_context
  real(real64), allocatable :: workspace(:)
  real(real64), allocatable :: distance_buffer(:)
  logical :: workspace_ready = .false.
contains
  procedure :: ensure_workspace
  procedure :: query_nearest_mint
end type

! Bad - malloc/free party, every query pays the full cost
function query_mint(manifold, vec)
  real(real64), allocatable :: temp(:)  ! Born every call, dies every call
  allocate(temp(MANIFOLD_DIM))          ! The allocator weeps
end function
```

---

## 🔍 Common Patterns

### Factory Functions

```fortran
function create_mint(behavior_type, source_vec) result(mint)
  integer,      intent(in) :: behavior_type
  real(real64), intent(in) :: source_vec(:)
  class(base_mint), allocatable :: mint

  select case (behavior_type)
  case (MINT_VALIDATE)
    allocate(validate_input_mint :: mint)
  case (MINT_AUTH)
    allocate(auth_flow_mint :: mint)
  case (MINT_RATE_LIMIT)
    allocate(rate_limit_mint :: mint)
  case default
    error stop "Unknown mint type - did you invent new behavior?"
  end select

  call mint%embed(source_vec)
end function
```

### Builder Pattern (Not Ridiculous)

```fortran
! Clean composition of complex mints
query = mint_query_builder()           &
  %with_vector(input_embedding)        &
  %with_corridor(AUTH_CORRIDOR_WIDTH)  &
  %with_fallback(MINT_DENY)            &
  %build()

! Much cleaner than:
! query = create_mint_query(input_embedding, AUTH_CORRIDOR_WIDTH, MINT_DENY, 0, .true., null)
```

---

## ✅ Code Review Checklist

### Core Fortran

- [ ] All variables have explicit types with kinds (no `integer :: i`)
- [ ] `implicit none` in every module — no exceptions, ever
- [ ] No magic numbers — name your constants
- [ ] Functions declare intent (`in`, `out`, `inout` — pick one)
- [ ] Errors handled, not ignored
- [ ] Memory freed — what goes up must come down
- [ ] Names make sense to humans, not just you
- [ ] Comments explain why — code already shows what
- [ ] Initialized all the things — no garbage values
- [ ] Did not try to outsmart the compiler — it's smarter than you

### Glamin-Specific

- [ ] Behavioral types use `_behavior` or `_mint` suffix
- [ ] Distance variables say what they are distance *from*
- [ ] Confidence values are `0.0–1.0`, never raw distance
- [ ] Corridor widths annotated with risk tolerance rationale
- [ ] No manifold context passed by value — always `intent(inout)` or pointer
- [ ] Confidence tier evaluated at every decision boundary
- [ ] Names would confuse a Django developer *(this is a compliment)*

---

*We're not just writing code. We're building a compute revolution.*
*Make it beautiful. Make it fast. Make it so clean that even physicists complain it's too readable.*

**GLAMIN • Geometric Logical Application Meta-Instruction Network**