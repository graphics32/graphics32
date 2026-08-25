---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.SaveToStream
kind: Method
scope: Public
summary: "Saves bitmap image data to a stream in BMP/DIB format."
overloads:
  - signature: "procedure SaveToStream(Stream: TStream; SaveTopDown: Boolean = False); overload; virtual;"
    summary: "Saves bitmap to a stream with optional top-down scanline orientation."
    parameters:
      - name: Stream
        type: TStream
        description: "Target output stream."
      - name: SaveTopDown
        type: Boolean
        description: "If True, writes scanlines in top-down order instead of bottom-up."
  - signature: "procedure SaveToStream(Stream: TStream; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion); overload; virtual;"
    summary: "Saves bitmap to a stream specifying scanline orientation and header format version."
    parameters:
      - name: Stream
        type: TStream
        description: "Target output stream."
      - name: SaveTopDown
        type: Boolean
        description: "If True, writes scanlines in top-down order."
      - name: InfoHeaderVersion
        type: TInfoHeaderVersion
        description: "BMP header format version (InfoHeaderVersion1 to InfoHeaderVersion5)."
---

## Description

`SaveToStream` encodes and writes bitmap contents to `Stream`.

## Example

```pascal
Bitmap.SaveToStream(Stream, True);
```
