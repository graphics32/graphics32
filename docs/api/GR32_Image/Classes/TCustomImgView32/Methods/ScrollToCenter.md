---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImgView32
entity: TCustomImgView32.ScrollToCenter
kind: Method
overloads:
  - signature: "procedure ScrollToCenter; overload;"
    summary: "Centers image inside viewport."
  - signature: "procedure ScrollToCenter(X, Y: Integer); overload; override;"
    summary: "Centers specified bitmap coordinate in viewport."
    parameters:
      - name: X, Y
        type: Integer
        description: "Bitmap coordinate to center."
---

## Description

`ScrollToCenter` updates scrollbars and viewport offsets to center specified image coordinates inside the viewport.
