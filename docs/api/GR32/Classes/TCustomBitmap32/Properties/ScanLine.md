---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.ScanLine
kind: Property
scope: Public
declaration: "property ScanLine[Y: Integer]: PColor32Array read GetScanLine;"
summary: "Pointer to the starting pixel memory location for scanline row Y."
---

## Description

`ScanLine` returns a pointer (`PColor32Array`) to row `Y` of the bitmap buffer.
