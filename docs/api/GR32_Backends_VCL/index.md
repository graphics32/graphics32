---
layout: doc
docType: api
unit: GR32_Backends_VCL
entity: GR32_Backends_VCL
kind: Unit
summary: "Provides Windows GDI device context, memory-mapped file GDI, and lightweight GDI memory backends for Delphi VCL."
---

# Unit GR32_Backends_VCL

The `GR32_Backends_VCL` unit provides Windows GDI surface backend implementations for Graphics32 when compiled under Delphi VCL.

---

## Classes

- [[TGDIBackend]]: Windows GDI device context surface backend allocating DIB sections and DC handles.
- [[TGDIMMFBackend]]: Windows GDI surface backend backed by memory-mapped files or swap space.
- [[TGDIMemoryBackend]]: Lightweight GDI memory backend rendering directly to `TCanvas` without allocating GDI handles.
