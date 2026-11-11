---
layout: doc
docType: api
unit: GR32_Image
parent: TBackgroundOptions
entity: TBackgroundOptions.FillStyle
kind: Property
aliases: [TBackgroundFillStyle, bfsColor, bfsCheckers, bfsPattern]
declaration: |
  type
    TBackgroundFillStyle = (bfsColor, bfsCheckers, bfsPattern);

  property FillStyle: TBackgroundFillStyle read FFillStyle write SetFillStyle default bfsColor;
summary: "Specifies background fill style surrounding the bitmap."
seealso:
  - "[[PatternBitmap]]"
  - "[[CheckersColors]]"
---

## Description

`FillStyle` specifies how the background area surrounding the bitmap is filled.

| Value | Description |
| --- | --- |
| `bfsColor` | Fills the background area with a solid color specified by the control's `Color` property. |
| `bfsCheckers` | Fills the background with a checkerboard pattern configured via [[CheckersStyle]]. |
| `bfsPattern` | Fills the background by tiling a custom [[PatternBitmap]]. |
