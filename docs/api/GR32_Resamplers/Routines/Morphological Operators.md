---
layout: doc
docType: api
unit: GR32_Resamplers
entity: Morphological Operators
kind: Function
aliases: [Convolve, Dilate, Erode, Expand, Contract]
declaration: |
  procedure Convolve(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
  procedure Dilate(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
  procedure Erode(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
  procedure Expand(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
  procedure Contract(Src, Dst: TCustomBitmap32; Kernel: TIntegerMap; CenterX, CenterY: Integer);
summary: "Convenience routines for applying 2D spatial convolution or morphological filtering to a bitmap."
parameters:
  - name: Src
    type: TCustomBitmap32
    description: "Source bitmap containing the image to be processed."
  - name: Dst
    type: TCustomBitmap32
    description: "Destination bitmap where the filtered result will be stored."
  - name: Kernel
    type: TIntegerMap
    description: "Integer map representing the 2D neighborhood kernel structure."
  - name: CenterX, CenterY
    type: Integer
    description: "Coordinates of the kernel origin/center point."
---

## Description

These convenience routines internally uses neighborhood kernel samplers ([[TConvolver]], [[TDilater]], [[TEroder]], [[TExpander]], [[TContracter]]) and rasterize the resulting filtered output from `Src` into `Dst`.
