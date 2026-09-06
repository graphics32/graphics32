---
layout: doc
docType: api
unit: GR32_System
entity: CPU
kind: Variable
summary: "Global TCPU record instance containing hardware CPU detection and feature flags."
declaration: "var CPU: TCPU;"
seealso:
  - "[[GR32.CPUID]]"
  - "[[TCPU]]"
---

## Description

`CPU` is the global [[TCPU]] record instance initialized during the initialization section of `GR32_System`.

It provides global access to host CPU vendor identification, topology, cache sizes, and hardware SIMD instruction set capabilities (`CPU.InstructionSupport`), enabling CPU dispatch bindings to select optimized Pascal, SSE, SSE2, or AVX routines at runtime.
