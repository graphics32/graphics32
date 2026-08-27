---
layout: doc
docType: api
unit: GR32_Backends_Generic
entity: GR32_Backends_Generic
kind: Unit
summary: "Provides platform-independent heap memory and memory-mapped file surface backends."
---

# Unit GR32_Backends_Generic

The `GR32_Backends_Generic` unit provides generic surface backend implementations for Graphics32 that do not depend on platform GUI subsystem handles (such as GDI or LCL handles).

---

## Classes

- [[TMemoryBackend]]: Heap memory backend storing pixel buffers in dynamically allocated RAM.
- [[TMMFBackend]]: Memory-mapped file backend storing pixel buffers in swap space or disk files (Windows).
