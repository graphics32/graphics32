---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImgView32
entity: TCustomImgView32.SizeGrip
kind: Property
aliases: [TSizeGripStyle, sgAuto, sgNone, sgAlways]
declaration: |
  type
    TSizeGripStyle = (sgAuto, sgNone, sgAlways);

  property SizeGrip: TSizeGripStyle read FSizeGrip write SetSizeGrip default sgAuto;
summary: "Size grip rendering style in bottom-right corner."
---

## Description

`SizeGrip` controls visibility of the bottom-right corner resize grip.

| Value | Description |
| --- | --- |
| `sgAuto` | Size grip is displayed automatically when both horizontal and vertical scrollbars are visible. |
| `sgNone` | Size grip is never displayed. |
| `sgAlways` | Size grip is always visible. |
