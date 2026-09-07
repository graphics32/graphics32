---
layout: doc
docType: api
unit: GR32_Layers
entity: TRBHandles
kind: Type
declaration: "TRBHandles = set of (rhCenter, rhSides, rhCorners, rhFrame, rhNotLeftSide, rhNotRightSide, rhNotTopSide, rhNotBottomSide, rhNotTLCorner, rhNotTRCorner, rhNotBLCorner, rhNotBRCorner);"
summary: "Set of flags enabling or disabling specific sizing handles on a rubberband layer."
---

## Description

`TRBHandles` specifies which handles and frames are visible and interactive on a `TRubberbandLayer`.

## Values

| Value | Description |
| --- | --- |
| `rhCenter` | Enables body dragging (moving). |
| `rhSides` | Enables side sizing handles (L, T, R, B). |
| `rhCorners` | Enables corner sizing handles (TL, TR, BL, BR). |
| `rhFrame` | Displays the rubberband frame outline. |
| `rhNotLeftSide` | Disables left side sizing handle. |
| `rhNotRightSide` | Disables right side sizing handle. |
| `rhNotTopSide` | Disables top side sizing handle. |
| `rhNotBottomSide` | Disables bottom side sizing handle. |
| `rhNotTLCorner` | Disables top-left corner sizing handle. |
| `rhNotTRCorner` | Disables top-right corner sizing handle. |
| `rhNotBLCorner` | Disables bottom-left corner sizing handle. |
| `rhNotBRCorner` | Disables bottom-right corner sizing handle. |
