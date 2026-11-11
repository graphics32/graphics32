---
layout: doc
docType: api
unit: GR32.Blend.Modes
entity: TGraphics32BlenderNormal
kind: Class
declaration: |
  TGraphics32BlenderNormal = class(TCustomGraphics32Blender)
inheritance:
  - TObject
  - TCustomGraphics32Blender
  - TGraphics32BlenderNormal
summary: "Default standard alpha blender implementation."
---

## Description

`TGraphics32BlenderNormal` implements standard alpha blending (Normal mode). It routes blending operations directly to Graphics32's optimized core routines (`MergeReg` and `MergeMemEx`).

[members]
