---
layout: doc
docType: api
unit: GR32_Blend
entity: BLEND_REG
aliases: [BLEND_MEM, BLEND_REG_EX, BLEND_MEM_EX, BLEND_LINE, BLEND_LINE_EX]
kind: Constant
declaration: |
  const BLEND_REG: TBlendRegCombineModeArray = ((@@BlendReg), (@@MergeReg));
  const BLEND_MEM: TBlendMemCombineModeArray = ((@@BlendMem), (@@MergeMem));
  const BLEND_REG_EX: TBlendRegExCombineModeArray = ((@@BlendRegEx), (@@MergeRegEx));
  const BLEND_MEM_EX: TBlendMemExCombineModeArray = ((@@BlendMemEx), (@@MergeMemEx));
  const BLEND_LINE: TBlendLineCombineModeArray = ((@@BlendLine), (@@MergeLine));
  const BLEND_LINE_EX: TBlendLineExCombineModeArray = ((@@BlendLineEx), (@@MergeLineEx));
summary: "Lookup arrays mapping TCombineMode enum values (cmBlend, cmMerge) to corresponding blending or merging function delegate pointers."
seealso:
  - "[[TCombineMode]]"
  - "[[Blend]]"
  - "[[Merge]]"
---

## Description

The `BLEND_*` array constants contain pointers to `Blend*` (index 0, `cmBlend`) and `Merge*` (index 1, `cmMerge`) function delegates.

These lookup arrays allow internal drawing algorithms and surface rasterizers to retrieve the appropriate blending or merging delegate pointer efficiently without conditional branch instructions:

```pascal
var
  MyBlendMem: TBlendMem;
  Pixel: PColor32;
begin
  MyBlendMem := BLEND_MEM[Bitmap.CombineMode]^;

  Pixel := Bitmap.PixelPtr[0, 0];

  // Blend or merge 50% red onto first pixel in bitmap
  MyBlendMem(clTrRed32, Pixel^);
end;
```
