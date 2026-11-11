---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.ScaleMode
kind: Property
aliases: [TScaleMode, smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled]
declaration: |
  type
    TScaleMode = (smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled);

  property ScaleMode: TScaleMode read FScaleMode write SetScaleMode;
summary: "Sizing and scaling mode for bitmap rendering."
seealso:
  - "[[Scale]]"
---

## Description

`ScaleMode` controls how the bitmap is scaled, stretched, or resized within the image control.

| Value | Description |
| --- | --- |
| `smNormal` | Renders the bitmap at its original unscaled dimensions (1:1 scale ratio). |
| `smStretch` | Stretches the bitmap to fill the entire client area of the control, ignoring aspect ratio. |
| `smScale` | Scales the bitmap using explicit horizontal ([[ScaleX]]) and vertical ([[ScaleY]]) scaling factors. |
| `smResize` | Automatically resizes the control to match the bitmap size. |
| `smOptimal` | Automatically scales the bitmap down to fit inside the control while maintaining aspect ratio if the bitmap is larger than the control, or renders unscaled if smaller. |
| `smOptimalScaled` | Fits the bitmap inside the control while maintaining aspect ratio, scaling both up or down as required. |
