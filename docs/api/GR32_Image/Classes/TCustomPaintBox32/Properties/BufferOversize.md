---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.BufferOversize
kind: Property
scope: Public
declaration: "property BufferOversize: Integer read FBufferOversize write SetBufferOversize;"
summary: "Specifies extra padding margin added to the buffer dimensions to reduce reallocation during resize."
---

## Description

`BufferOversize` defines an additional pixel margin allocated when resizing the internal buffer, minimizing frequent memory reallocations during active resizing.
