---
layout: doc
docType: api
unit: GR32_Polygons
entity: DashLineFS
kind: Function
summary: "Renders dashed floating-point polylines with optional stroke outlines and custom span fillers."
overloads:
  - signature: "procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; Color: TColor32; Closed: Boolean = False; Width: TFloat = 1.0); overload;"
    summary: "Renders a dashed floating-point polyline using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: Dashes
        type: TArrayOfFloat
        description: "Array of dash/space pattern lengths in pixels."
      - name: Color
        type: TColor32
        description: "Dash fill color."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: Width
        type: TFloat
        description: "Dash line width in pixels."

  - signature: "procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32; Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;"
    summary: "Renders a dashed floating-point polyline with separate fill and stroke outline colors."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: Dashes
        type: TArrayOfFloat
        description: "Array of dash/space pattern lengths in pixels."
      - name: FillColor
        type: TColor32
        description: "Dash fill color."
      - name: StrokeColor
        type: TColor32
        description: "Dash outline stroke color."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: Width
        type: TFloat
        description: "Dash line width in pixels."
      - name: StrokeWidth
        type: TFloat
        description: "Outline stroke width in pixels."

  - signature: "procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; Closed: Boolean = False; Width: TFloat = 1.0); overload;"
    summary: "Renders a dashed floating-point polyline using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: Dashes
        type: TArrayOfFloat
        description: "Array of dash/space pattern lengths in pixels."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: Width
        type: TFloat
        description: "Dash line width in pixels."

  - signature: "procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32; Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;"
    summary: "Renders a dashed floating-point polyline using custom Filler and a separate stroke outline color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: Dashes
        type: TArrayOfFloat
        description: "Array of dash/space pattern lengths in pixels."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: StrokeColor
        type: TColor32
        description: "Dash outline stroke color."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: Width
        type: TFloat
        description: "Dash line width in pixels."
      - name: StrokeWidth
        type: TFloat
        description: "Outline stroke width in pixels."
---

## Description

`DashLineFS` breaks input polyline segments into dashed polyline segments using pattern lengths in `Dashes`, generates stroke polygons, and renders them onto `Bitmap`.
