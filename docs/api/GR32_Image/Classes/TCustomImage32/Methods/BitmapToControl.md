---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.BitmapToControl
kind: Method
overloads:
  - signature: "function BitmapToControl(const APoint: TPoint): TPoint; overload;"
    summary: "Converts integer point coordinates from bitmap space to control viewport space."
    parameters:
      - name: APoint
        type: TPoint
        description: "Point in bitmap coordinates."
    returns:
      - type: TPoint
        description: "Point transformed into control viewport coordinates."
  - signature: "function BitmapToControl(const APoint: TFloatPoint): TFloatPoint; overload;"
    summary: "Converts floating-point coordinates from bitmap space to control viewport space."
    parameters:
      - name: APoint
        type: TFloatPoint
        description: "Sub-pixel point in bitmap coordinates."
    returns:
      - type: TFloatPoint
        description: "Point transformed into control viewport coordinates."
  - signature: "function BitmapToControl(const ARect: TRect): TRect; overload;"
    summary: "Transforms rectangle from bitmap space to control viewport space."
    parameters:
      - name: ARect
        type: TRect
        description: "Rectangle in bitmap coordinates."
    returns:
      - type: TRect
        description: "Rectangle transformed into control viewport coordinates."
seealso:
  - "[[ControlToBitmap]]"
---

## Description

`BitmapToControl` maps points or rectangles from the `Bitmap` coordinate system to control client viewport space, applying current scale factors and offsets.
