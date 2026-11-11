---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Line
kind: Method
scope: Public
aliases: [LineS, LineT, LineTS, LineA, LineAS, LineX, LineXS, LineF, LineFS, LineXP, LineXSP, LineFP, LineFSP]
declaration: |
  procedure Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
  procedure LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
  procedure LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
  procedure LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
  procedure LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
  procedure LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
  procedure LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
  procedure LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
  procedure LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
  procedure LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
  procedure LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
  procedure LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
  procedure LineFP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;
  procedure LineFSP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;
parameters:
  - name: X1, Y1, X2, Y2
    type: "-"
    description: "Start and end pixel coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
  - name: L
    type: Boolean
    description: "If True, includes the last pixel."
summary: "Draws 1-pixel wide line segments between specified coordinates using integer, fixed-point, or floating-point positioning with optional clipping, alpha blending, anti-aliasing, and stippling."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[LineTo]]"
---

## Description

The `Line` methods draw 1-pixel wide straight line segments between starting coordinates `(X1, Y1)` and ending coordinates `(X2, Y2)`. Method variants provide specific combinations of coordinate types (`Integer`, `TFixed`, `Single`), boundary clipping (`S`), alpha transparency (`T`), anti-aliasing (`A`, `X`, `F`), and pattern stippling (`P`).

When parameter `L` (Last pixel) is `True`, the final end pixel `(X2, Y2)` is rendered; when `False`, the endpoint pixel is omitted (useful for drawing contiguous polyline segments without double-rendering vertices).

## Variants

| Variant | Coordinate Type | Modifiers | Description |
| --- | --- | --- | --- |
| `Line` | `Integer` | Direct | Fast integer line rendering without clipping or blending. |
| `LineS` | `Integer` | Safe | Integer line rendering clipped against `ClipRect`. |
| `LineT` | `Integer` | Transparent | Unclipped integer line rendering with alpha blending. |
| `LineTS` | `Integer` | Transparent + Safe | Clipped integer line rendering with alpha blending. |
| `LineA` | `Integer` | Anti-aliased | Unclipped anti-aliased integer line rendering using Wu's algorithm. |
| `LineAS` | `Integer` | Anti-aliased + Safe | Clipped anti-aliased integer line rendering using Wu's algorithm. |
| `LineX` | `TFixed` | Fixed Sub-pixel | Unclipped 16.16 fixed-point anti-aliased line rendering. |
| `LineXS` | `TFixed` | Fixed Sub-pixel + Safe | Clipped 16.16 fixed-point anti-aliased line rendering. |
| `LineF` | `Single` | Float Sub-pixel | Unclipped floating-point anti-aliased line rendering. |
| `LineFS` | `Single` | Float Sub-pixel + Safe | Clipped floating-point anti-aliased line rendering. |
| `LineXP` | `TFixed` | Fixed + Stipple | Unclipped 16.16 fixed-point stippled line rendering using active stipple pattern. |
| `LineXSP` | `TFixed` | Fixed + Stipple + Safe | Clipped 16.16 fixed-point stippled line rendering using active stipple pattern. |
| `LineFP` | `Single` | Float + Stipple | Unclipped floating-point stippled line rendering using active stipple pattern. |
| `LineFSP` | `Single` | Float + Stipple + Safe | Clipped floating-point stippled line rendering using active stipple pattern. |

## Example

```pascal
// Direct integer line
Bitmap.Line(0, 0, 100, 100, clRed32);

// Clipped anti-aliased floating-point line
Bitmap.LineFS(10.5, 10.5, 200.25, 150.75, clBlue32);
```
