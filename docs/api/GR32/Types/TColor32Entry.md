---
layout: doc
docType: api
unit: GR32
entity: TColor32Entry
kind: Type
summary: "32-bit ARGB color record."
declaration: |
  type
    TColor32Entry = packed record
      case Integer of
  {$IFNDEF RGBA_FORMAT}
        0: (B, G, R, A: Byte);
  {$ELSE}
        0: (R, G, B, A: Byte);
  {$ENDIF}
        1: (ARGB: TColor32);
        2: (Planes: array[0..3] of Byte);
        3: (Components: array[TColor32Component] of Byte);
    end;
    PColor32Entry = ^TColor32Entry;
aliases: [PColor32Entry]
---

## Description

`TColor32Entry` is a packed variant record providing multi-view field access to individual 8-bit color channels, the combined 32-bit [[TColor32]] ARGB value, raw byte planes, or enum-indexed color components.

## Variant Fields

**case 0**

| Field | Type | Description |
| --- | --- | --- |
| `B` | `Byte` | Blue channel byte (0..255). |
| `G` | `Byte` | Green channel byte (0..255). |
| `R` | `Byte` | Red channel byte (0..255). |
| `A` | `Byte` | Alpha channel byte (0..255). |

**case 1**

| Field | Type | Description |
| --- | --- | --- |
| `ARGB` | `TColor32` | Combined 32-bit ARGB color integer. |

**case 2**

| Field | Type | Description |
| --- | --- | --- |
| `Planes` | `array[0..3] of Byte` | Array view of 4 byte planes. |

**case 3**

| Field | Type | Description |
| --- | --- | --- |
| `Components` | `array[TColor32Component] of Byte` | Indexed access by `TColor32Component` enum (`ccBlue`, `ccGreen`, `ccRed`, `ccAlpha`). |
