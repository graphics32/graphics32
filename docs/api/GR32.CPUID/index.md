---
layout: doc
docType: api
unit: GR32.CPUID
entity: GR32.CPUID
kind: Unit
summary: "Provides hardware CPU feature detection, vendor identification, cache queries, and instruction set introspection."
---

## Description

The `GR32.CPUID` unit provides comprehensive CPU detection capabilities for x86 and x64 architectures. It queries CPU vendor identification, instruction set extensions (such as MMX, SSE, SSE2..SSE4.2, AVX, AVX2, AVX-512, BMI1/2, FMA), processor topology, cache sizes, and operating system support for extended register sets (XMM/YMM).

Key capabilities provided by this unit include:

- **Vendor Identification**: Enumerates CPU vendors and hypervisor virtual machines via [[TCPUVendor]].
- **Instruction Set Feature Flags**: Detects hardware SIMD and vector instruction extensions via [[TCPUInstructionSet]] and [[TInstructionSupport]].
- **CPU Details & Cache Info**: Queries CPU family, model, stepping, and L1/L2/L3 cache sizes via the [[TCPU]] record structure.

---

[members]
