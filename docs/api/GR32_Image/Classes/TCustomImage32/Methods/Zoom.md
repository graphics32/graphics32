---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.Zoom
kind: Method
overloads:
  - signature: "procedure Zoom(AScale: TFloat; const APivot: TFloatPoint; AAnimate: Boolean = False); overload;"
    summary: "Zooms control scale factor around a specific bitmap pivot point."
    parameters:
      - name: AScale
        type: TFloat
        description: "Target scale factor."
      - name: APivot
        type: TFloatPoint
        description: "Bitmap pivot point held stationary under cursor."
      - name: AAnimate
        type: Boolean
        description: "If True, animates scale transition using cubic easing."
  - signature: "procedure Zoom(AScale: TFloat; AAnimate: Boolean = False); overload;"
    summary: "Zooms control scale factor."
    parameters:
      - name: AScale
        type: TFloat
        description: "Target scale factor."
      - name: AAnimate
        type: Boolean
        description: "If True, animates scale transition."
seealso:
  - "[[TMouseZoomOptions]]"
---

## Description

`Zoom` changes scale factor to `AScale`, optionally preserving cursor pivot point `APivot` and optionally animating the transition using cubic easing.
