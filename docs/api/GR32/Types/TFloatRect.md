---
layout: doc
docType: api
unit: GR32
entity: TFloatRect
kind: Type
summary: "Rectangle structure using single-precision floating-point coordinates."
declaration: |
  type
    TFloatRect = record
      case Integer of
        0: (Left, Top, Right, Bottom: TFloat);
        1: (TopLeft, BottomRight: TFloatPoint);
    end;
    PFloatRect = ^TFloatRect;
aliases: [PFloatRect]
---

## Description

`TFloatRect` represents a rectangle using single-precision floating-point coordinates ([[TFloat]]).

## Variant Fields

**case 0**

| Field | Type | Description |
| --- | --- | --- |
| `Left` | `TFloat` | Single precision X-coordinate of left edge. |
| `Top` | `TFloat` | Single precision Y-coordinate of top edge. |
| `Right` | `TFloat` | Single precision X-coordinate of right edge. |
| `Bottom` | `TFloat` | Single precision Y-coordinate of bottom edge. |

**case 1**

| Field | Type | Description |
| --- | --- | --- |
| `TopLeft` | `TFloatPoint` | Top-left point of rectangle. |
| `BottomRight` | `TFloatPoint` | Bottom-right point of rectangle. |
