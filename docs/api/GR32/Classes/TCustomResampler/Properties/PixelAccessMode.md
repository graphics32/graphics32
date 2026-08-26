---
layout: doc
docType: api
unit: GR32
parent: TCustomResampler
entity: TCustomResampler.PixelAccessMode
kind: Property
scope: Published
declaration: "property PixelAccessMode: TPixelAccessMode read FPixelAccessMode write SetPixelAccessMode default pamSafe;"
summary: "Controls out-of-bounds coordinate handling and boundary checking during pixel sampling."
---

## Description

`PixelAccessMode` determines how coordinates outside the source bitmap boundaries are handled during sampling operations.

### Pixel Access Modes (`TPixelAccessMode`)

| Value | Description |
| --- | --- |
| `pamUnsafe` | Performs direct buffer memory access without boundary checks. Fastest, but sampling outside bitmap dimensions results in access violations or memory corruption. |
| `pamSafe` | Clamps out-of-bounds coordinates to valid bitmap boundary pixels. Safe default mode. |
| `pamWrap` | Wraps coordinates outside bitmap boundaries tiled/repeating infinitely in both X and Y directions. |
| `pamTransparentEdge` | Samples outside bitmap boundaries as fully transparent pixels (`clNone32`). |

## Example

```pascal
// Enable tiled wrapping mode for pattern sampling
Resampler.PixelAccessMode := pamWrap;
```
