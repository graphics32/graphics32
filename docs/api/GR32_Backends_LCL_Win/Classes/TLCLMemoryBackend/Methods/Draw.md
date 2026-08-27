---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLMemoryBackend
entity: TLCLMemoryBackend.Draw
kind: Method
scope: Public
declaration: "procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;"
summary: "IDeviceContextSupport method."
parameters:
  - name: DstRect, SrcRect
    type: TRect
    description: "Target and source bounds."
  - name: hSrc
    type: HDC
    description: "Source device context."
---

# TLCLMemoryBackend.Draw

`Draw` triggers bitmap modification notifications.
