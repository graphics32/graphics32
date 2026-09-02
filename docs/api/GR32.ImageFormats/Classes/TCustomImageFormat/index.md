---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: TCustomImageFormat
kind: Class
scope: Public
abstract: true
declaration: "TCustomImageFormat = class abstract(TInterfacedObject, IImageFormat);"
inheritance:
  - TObject
  - TInterfacedObject
  - TCustomImageFormat
summary: "Abstract base class for custom image format implementations in Graphics32."
---

## Description

`TCustomImageFormat` serves as the abstract base class implementing [[IImageFormat]] and `IUnknown` via `TInterfacedObject`. Concrete image format implementations derive from `TCustomImageFormat` (or [[TCustomImageFormatAdapter]]) and implement specialized interfaces like [[IImageFormatReader]], [[IImageFormatWriter]], or [[IImageFormatFileInfo]].

[members]
