---
layout: doc
docType: api
unit: GR32_Polygons
entity: DashLineXS
kind: Function
summary: "Renders dashed fixed-point polylines with optional stroke outlines and custom span fillers."
overloads:
  - signature: "procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; Color: TColor32; Closed: Boolean = False; Width: TFixed = $10000); overload;"
    summary: "Renders a dashed fixed-point polyline using solid Color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: Dashes
        type: TArrayOfFixed
        description: "Array of dash/space pattern lengths in fixed point format."
      - name: Color
        type: TColor32
        description: "Dash fill color."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: Width
        type: TFixed
        description: "Dash line width in fixed point format."

  - signature: "procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32; Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;"
    summary: "Renders a dashed fixed-point polyline with separate fill and stroke outline colors."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: Dashes
        type: TArrayOfFixed
        description: "Array of dash/space pattern lengths in fixed point format."
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
        type: TFixed
        description: "Dash line width in fixed point format."
      - name: StrokeWidth
        type: TFixed
        description: "Outline stroke width in fixed point format."

  - signature: "procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; Closed: Boolean = False; Width: TFixed = $10000); overload;"
    summary: "Renders a dashed fixed-point polyline using custom Filler."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: Dashes
        type: TArrayOfFixed
        description: "Array of dash/space pattern lengths in fixed point format."
      - name: Filler
        type: TCustomPolygonFiller
        description: "Custom span filler."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."
      - name: Width
        type: TFixed
        description: "Dash line width in fixed point format."

  - signature: "procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32; Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;"
    summary: "Renders a dashed fixed-point polyline using custom Filler and a separate stroke outline color."
    parameters:
      - name: Bitmap
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: Dashes
        type: TArrayOfFixed
        description: "Array of dash/space pattern lengths in fixed point format."
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
        type: TFixed
        description: "Dash line width in fixed point format."
      - name: StrokeWidth
        type: TFixed
        description: "Outline stroke width in fixed point format."
---

## Description

`DashLineXS` breaks fixed-point polyline segments into dashed polyline segments using pattern lengths in `Dashes`, generates stroke polygons, and renders them onto `Bitmap`.
