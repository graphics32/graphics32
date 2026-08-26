---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.SaveToFile
kind: Method
scope: Public
summary: "Saves bitmap image data to a file on disk in BMP/DIB format."
overloads:
  - signature: "procedure SaveToFile(const FileName: string); overload; virtual;"
    summary: "Saves bitmap to a file using default BMP header version."
    parameters:
      - name: FileName
        type: string
        description: "Output file path."
  - signature: "procedure SaveToFile(const FileName: string; SaveTopDown: Boolean); overload; virtual;"
    summary: "Saves bitmap to a file with specified scanline orientation."
    parameters:
      - name: FileName
        type: string
        description: "Output file path."
      - name: SaveTopDown
        type: Boolean
        description: "If True, writes scanlines top-down."
  - signature: "procedure SaveToFile(const FileName: string; SaveTopDown: Boolean; InfoHeaderVersion: TInfoHeaderVersion); overload; virtual;"
    summary: "Saves bitmap to a file specifying scanline orientation and BMP header version."
    parameters:
      - name: FileName
        type: string
        description: "Output file path."
      - name: SaveTopDown
        type: Boolean
        description: "If True, writes scanlines top-down."
      - name: InfoHeaderVersion
        type: TInfoHeaderVersion
        description: "BMP header format version."
---

## Description

`SaveToFile` writes the bitmap surface contents to disk.

## Example

```pascal
Bitmap.SaveToFile('Output.bmp');
```
