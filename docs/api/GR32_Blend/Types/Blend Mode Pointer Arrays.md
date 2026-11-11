---
layout: doc
docType: api
unit: GR32_Blend
entity: Blend Mode Pointer Arrays
aliases: [PBlendReg, PBlendMem, PBlendRegEx, PBlendMemEx, PBlendLine, PBlendLineEx, TBlendRegCombineModeArray, TBlendMemCombineModeArray, TBlendRegExCombineModeArray, TBlendMemExCombineModeArray, TBlendLineCombineModeArray, TBlendLineExCombineModeArray]
kind: Type
declaration: |
  PBlendReg = ^TBlendReg;
  PBlendMem = ^TBlendMem;
  PBlendRegEx = ^TBlendRegEx;
  PBlendMemEx = ^TBlendMemEx;
  PBlendLine = ^TBlendLine;
  PBlendLineEx = ^TBlendLineEx;

  TBlendRegCombineModeArray = array[TCombineMode] of PBlendReg;
  TBlendMemCombineModeArray = array[TCombineMode] of PBlendMem;
  TBlendRegExCombineModeArray = array[TCombineMode] of PBlendRegEx;
  TBlendMemExCombineModeArray = array[TCombineMode] of PBlendMemEx;
  TBlendLineCombineModeArray = array[TCombineMode] of PBlendLine;
  TBlendLineExCombineModeArray = array[TCombineMode] of PBlendLineEx;
summary: "Pointers to function delegates and combine mode lookup arrays for selecting compositing functions by TCombineMode."
seealso:
  - "[[BLEND_REG]]"
  - "[[BLEND_MEM]]"
---

## Description

These pointer types and arrays allow Graphics32 drawing and bitmap routines to dynamically select the appropriate function delegate based on the active [[TCombineMode]] (`cmBlend` or `cmMerge`).
