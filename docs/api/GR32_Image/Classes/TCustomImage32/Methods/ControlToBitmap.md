---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.ControlToBitmap
kind: Method
overloads:
  - signature: "function ControlToBitmap(const APoint: TPoint): TPoint; overload;"
    summary: "Converts integer point coordinates from control viewport space to bitmap space."
    parameters:
      - name: APoint
        type: TPoint
        description: "Point in control viewport coordinates."
    returns:
      - type: TPoint
        description: "Point transformed into bitmap coordinates."
  - signature: "function ControlToBitmap(const APoint: TFloatPoint): TFloatPoint; overload;"
    summary: "Converts floating-point coordinates from control viewport space to bitmap space."
    parameters:
      - name: APoint
        type: TFloatPoint
        description: "Sub-pixel point in control viewport coordinates."
    returns:
      - type: TFloatPoint
        description: "Point transformed into bitmap coordinates."
  - signature: "function ControlToBitmap(const ARect: TRect; Rounding: TRectRounding = rrOutside): TRect; overload;"
    summary: "Transforms rectangle from control viewport space to bitmap space."
    parameters:
      - name: ARect
        type: TRect
        description: "Rectangle in control viewport coordinates."
      - name: Rounding
        type: TRectRounding
        description: "Coordinate rounding mode (`rrOutside`, `rrInside`, `rrClosest`, `rrLine`)."
    returns:
      - type: TRect
        description: "Rectangle transformed into bitmap coordinates."
seealso:
  - "[[BitmapToControl]]"
---

## Description

`ControlToBitmap` converts client viewport coordinates to underlying `Bitmap` pixel coordinates, accounting for viewport scale and offset.
