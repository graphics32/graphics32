---
layout: doc
docType: api
unit: GR32
entity: TPalette32
aliases: [PPalette32]
kind: Type
declaration: |
  TPalette32 = array[Byte] of TColor32;
  PPalette32 = ^TPalette32;
summary: "256-element array of 32-bit ARGB colors representing an indexed color palette."
seealso:
  - "[[TColor32]]"
  - "[[TByteMap]]"
  - "[[TColor32Gradient.SetColors]]"
  - "[[WinPalette]]"
  - "[[Color32]]"
---

## Description

`TPalette32` is a 256-element array of [[TColor32]] values representing an indexed 8-bit color palette.

`TPalette32` is used for palette-based color operations, converting indexed bitmaps or LUTs to 32-bit ARGB colors, and Windows GDI palette conversions.
