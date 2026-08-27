---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIMemoryBackend
entity: TGDIMemoryBackend.Draw
kind: Method
scope: Public
declaration: "procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;"
summary: "IDeviceContextSupport method."
parameters:
  - name: DstRect, SrcRect
    type: TRect
    description: "Target and source rectangles."
  - name: hSrc
    type: HDC
    description: "Source device context handle."
---

# TGDIMemoryBackend.Draw

`Draw` triggers bitmap modification notifications.
