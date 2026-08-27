---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
entity: GR32_Backends_LCL_Win
kind: Unit
summary: "Provides Lazarus LCL Win32/Win64 surface backend implementations utilizing LCLIntf and Windows GDI."
---

# Unit GR32_Backends_LCL_Win

The `GR32_Backends_LCL_Win` unit provides surface backend implementations for Graphics32 when compiled under Lazarus LCL for Windows targets (`LCLWin32` / `LCLWin64`).

---

## Classes

- [[TLCLBackend]]: Windows LCL device context surface backend allocating DIB sections via `LCLIntf.CreateDIBSection`.
- [[TLCLMMFBackend]]: Windows LCL surface backend backed by memory-mapped files or swap space.
- [[TLCLMemoryBackend]]: Lightweight LCL memory backend rendering directly to LCL `TCanvas` controls.
