---
layout: doc
docType: api
unit: GR32_LowLevel
entity: StackAlloc
aliases: [StackFree]
kind: Function
declaration: "function StackAlloc(Size: Integer): Pointer; register;\nprocedure StackFree(P: Pointer); register;"
summary: "Allocates a temporary block of memory on the call stack and releases it."
parameters:
  - name: Size
    type: Integer
    description: "The number of bytes to allocate on the stack."
  - name: P
    type: Pointer
    description: "Pointer to the stack-allocated memory block to free."
returns:
  - type: Pointer
    description: "Returns a pointer to the stack-allocated memory buffer."
---

## Description

`StackAlloc` provides rapid allocation of small temporary memory blocks directly from the call stack by adjusting the stack pointer. This achieves allocation performance equivalent to local variables while allowing dynamic runtime buffer sizing.

`StackFree` releases the memory allocated by `StackAlloc`.

### Remarks

- `StackFree` must be called in the exact same stack context as `StackAlloc` (not inside a nested subroutine or `finally` block).
- Multiple `StackFree` calls must occur in reverse order of their corresponding `StackAlloc` calls.
- If `USESTACKALLOC` is disabled or pure Pascal mode is active, `StackAlloc` transparently falls back to `GetMem` / `FreeMem` heap allocation.
