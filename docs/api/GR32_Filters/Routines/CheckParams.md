---
layout: doc
docType: api
unit: GR32_Filters
entity: CheckParams
kind: Function
declaration: "function CheckParams(Dst, Src: TCustomBitmap32; ResizeDst: Boolean = True; ClearDst: boolean = True): boolean;"
summary: "Validates bitmap parameters and optionally resizes destination bitmap to match source dimensions."
parameters:
  - name: Dst
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Src
    type: TCustomBitmap32
    description: "Source bitmap."
  - name: ResizeDst
    type: Boolean
    description: "If True (default) and dimensions differ, resizes Dst to match Src."
  - name: ClearDst
    type: Boolean
    description: "If True (default) and Dst is resized, clears Dst buffer."
returns:
  - type: Boolean
    description: "Returns `True` if the destination bitmap was resized to match source dimensions; otherwise `False`."
---

## Description

`CheckParams` validates that `Dst` and `Src` are non-nil (raising an exception if nil). If `ResizeDst` is `True` and `Dst` dimensions differ from `Src`, `Dst.SetSize` is called to match `Src`.

Returns `True` if `Dst` was resized.

## Example

```pascal
CheckParams(Dst, Src, True, False);
```
