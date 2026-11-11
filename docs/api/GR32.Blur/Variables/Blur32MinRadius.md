---
layout: doc
docType: api
unit: GR32.Blur
entity: Blur32MinRadius
kind: Variable
declaration: "var Blur32MinRadius: TFloat = 0.5;"
summary: "Global threshold minimum radius for blur operations."
seealso:
  - "[[Blur32]]"
  - "[[GammaBlur32]]"
---

## Description

`Blur32MinRadius` specifies the global lower threshold for the blur radius parameter across all `GR32.Blur` routines. The default value is `0.5` pixels.

When calling [[Blur32]] or [[GammaBlur32]] with a `Radius` parameter less than `Blur32MinRadius`:
* **Out-of-place overloads** (`ASource`, `ADest`) perform a fast direct copy from `ASource` to `ADest` using `CopyMapTo` without invoking the recursive Gaussian filter.
* **In-place overloads** (`Bitmap`) immediately return without altering pixel contents.
