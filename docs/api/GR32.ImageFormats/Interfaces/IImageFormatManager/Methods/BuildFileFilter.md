---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormatManager
entity: IImageFormatManager.BuildFileFilter
kind: Method
scope: Public
declaration: "function BuildFileFilter(Intf: TGUID; IncludeAll: Boolean = False): String;"
summary: "Builds an open/save dialog filter string formatted for Windows/LCL open and save dialogs."
parameters:
  - name: Intf
    type: TGUID
    description: "Interface GUID filter (e.g. IImageFormatReader or IImageFormatWriter)."
  - name: IncludeAll
    type: Boolean
    description: "Whether to prepend an 'All supported files' filter entry."
---

## Description

`BuildFileFilter` queries registered image formats implementing `Intf` and [[IImageFormatFileInfo]] to construct a standard dialog filter string (e.g. `"PNG Images (*.png)|*.png|JPEG Images (*.jpg)|*.jpg"`).
