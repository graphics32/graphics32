---
layout: doc
docType: api
unit: GR32.ImageFormats
entity: GR32.ImageFormats
kind: Unit
summary: "Provides core image format interfaces, abstract base classes, manager singleton, and signature validation utilities."
---

## Description

The `GR32.ImageFormats` unit serves as the extensible image format framework for Graphics32.

Key capabilities provided by this unit include:

- **Image Format Management**: Centralized registration, unregistration, and priority ordering of image format handlers via [[ImageFormatManager]].
- **Stream, File, and Resource Operations**: Reading and writing graphics streams, files, and executable resources through [[IImageFormatReaders]] and [[IImageFormatWriters]].
- **Object Assignment & Interop**: Bidirectional conversion between [[TCustomBitmap32]] and external graphic types (such as `TPicture` or `TGraphic`) via [[IImageFormatAdapter]].
- **Clipboard Integration**: Format detection and pasting from system clipboards via [[IImageFormatClipboardFormats]].
- **Dialog File Filters**: Automatic construction of open/save dialog filter strings using [[IImageFormatFileInfo]].
- **Signature Utilities**: Stream header and magic byte validation helper functions ([[CheckFileSignature]]).

---

[members]
