---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: IImageFormat
kind: Interface
declaration: "IImageFormat = interface"
summary: "Root base interface for all registered image format implementations."
---

## Description

`IImageFormat` is the base interface that every image format handler must implement to register with the global [[ImageFormatManager]]. Specific image format functionality (such as reading, writing, clipboard interop, or object assignment) is declared through specialized sub-interfaces like [[IImageFormatAdapter]], [[IImageFormatReader]], or [[IImageFormatWriter]].

[members]
