---
layout: doc
docType: api
unit: GR32_Filters
entity: IntensityToAlpha
kind: Function
declaration: "procedure IntensityToAlpha(Dst, Src: TCustomBitmap32);"
summary: "Maps weighted pixel luminance (intensity) of source pixels to the alpha channel of destination pixels."
parameters:
  - name: Dst
    type: TCustomBitmap32
    description: "Destination bitmap whose alpha channel is updated."
  - name: Src
    type: TCustomBitmap32
    description: "Source bitmap."
---

## Description

`IntensityToAlpha` calculates luminance for each `Src` pixel using the integer formula:

$$\text{Alpha} = (61 \cdot R + 174 \cdot G + 21 \cdot B) \gg 8$$

and assigns the calculated intensity value to the `A` channel of `Dst`.

## Example

```pascal
IntensityToAlpha(TargetBitmap, SourceBitmap);
```
