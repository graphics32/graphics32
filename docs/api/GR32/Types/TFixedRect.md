---
layout: doc
docType: api
unit: GR32
entity: TFixedRect
kind: Type
summary: "Rectangle structure using 16.16 fixed-point coordinates."
declaration: |
  type
    TFixedRect = record
      case Integer of
        0: (Left, Top, Right, Bottom: TFixed);
        1: (TopLeft, BottomRight: TFixedPoint);
    end;
    PFixedRect = ^TFixedRect;
aliases: [PFixedRect]
---

## Description

`TFixedRect` represents a rectangle using 16.16 fixed-point coordinates ([[TFixed]]).

## Variant Fields

**case 0**

| Field | Type | Description |
| --- | --- | --- |
| `Left` | `TFixed` | X-coordinate of left edge. |
| `Top` | `TFixed` | Y-coordinate of top edge. |
| `Right` | `TFixed` | X-coordinate of right edge. |
| `Bottom` | `TFixed` | Y-coordinate of bottom edge. |

**case 1**

| Field | Type | Description |
| --- | --- | --- |
| `TopLeft` | `TFixedPoint` | Top-left point of rectangle. |
| `BottomRight` | `TFixedPoint` | Bottom-right point of rectangle. |

## See also

- [[TFixedPoint]]
