---
layout: doc
docType: api
unit: GR32_OrdinalMaps
entity: TBooleanMap
kind: Class
declaration: "TBooleanMap = class(TCustomMap)"
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TBooleanMap
summary: "Bit-packed 2D boolean map providing memory-efficient 1-bit per pixel storage."
---

## Description

`TBooleanMap` is a specialized 2D data map derived from [[TCustomMap]] that stores boolean values using 1 bit per element ($1/8$ byte per pixel).

It is used for binary selection masks, clip bitmasks, and boolean flag grids.

### Key Features & Operations

- **Bit Packing**: Access individual bit flags via `Value[X, Y]` or direct byte pointer `Bits`.
- **`ToggleBit(X, Y)`**: Inverts the bit value at coordinate `(X, Y)`.
- **`Clear` Overloads**: Supports clearing to `False` (0x00), `True` (0xFF), or an arbitrary byte pattern.

[members]
