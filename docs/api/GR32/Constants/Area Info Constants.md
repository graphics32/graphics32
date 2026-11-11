---
layout: doc
docType: api
unit: GR32
entity: Area Info
kind: Constant
aliases: [AREAINFO_RECT,AREAINFO_LINE,AREAINFO_ABSOLUTE,AREAINFO_MASK]
summary: "Bitmap area change bit masks"
---

## Description

Bitmask constants used in change notification calls (such as [[TCustomBitmap32.Changed]] and [OnAreaChanged] events) to describe the shape or characteristics of the updated region.

## Constants Table

| Constant | Value | Description |
| --- | --- | --- |
| `AREAINFO_RECT` | `$80000000` | Indicates that the changed area is a rectangular region. |
| `AREAINFO_LINE` | `$40000000` | Indicates that the changed area is a line segment. Lower 24 bits store line width in pixels. |
| `AREAINFO_ELLIPSE` | `$20000000` | Indicates that the changed area is an elliptical region. |
| `AREAINFO_ABSOLUTE` | `$10000000` | Indicates that coordinates in the area structure are absolute values rather than relative offsets. |
| `AREAINFO_MASK` | `$FF000000` | Bitmask covering high byte area flags. |

::: warning
`AREAINFO_ELLIPSE` has been deprecated and is no longer implemented.
:::