---
layout: doc
docType: api
unit: GR32.ImageFormats.BMP
entity: GR32.ImageFormats.BMP
kind: Unit
summary: "Provides bitmap image format adapter and reader support for standard BMP files."
seealso:
  - "[[GR32.ImageFormats.TBitmap]]"
---

## Description

The `GR32.ImageFormats.BMP` unit implements image format adapters and readers for standard Windows bitmap files (`.bmp`) using the native [[TBitmap32]] BMP support.

::: info
The native Graphics32 BMP implementation only supports writing BMP files in 32-bit ARGB format. By default using the V4 sub-format.

The default sub-format can be specified with the [[DefaultBitmapHeaderVersion]] global setting.
:::

::: warning BMP Version 5
Note that while Graphics32 fully support reading and writing the BMP V5 sub-format, most other applications and libraries either doesn't support this format, or claim to support it but has misinterpreted the specification.

In order to work around this problem, when copying a `TBitmap32` to the clipboard, if Graphics32's native PNG support is enabled ([[TImageFormatAdapterPNG32]]), then Graphics32 will additionally place a copy of the bitmap in PNG format onto the clipboard. Most applications will give precedence to the PNG format when copying from the clipboard.

See also:
- [Copy to clipboard, image got shifted - Graphics32, issue #257](https://github.com/graphics32/graphics32/issues/257)
- [Paint-NET - Pasting DIBv5 image](https://forums.paint.net/topic/122848-pasting-dibv5-image/)
- [1 px line on top of every image pasted into Firefox from paint.net](https://forums.paint.net/topic/124628-1-px-line-on-top-of-every-image-pasted-into-firefox-from-paintnet/)
- [Support “short” BI_BITFIELDS DIBv5 when pasting images (pixel data shifted when copy/pasting from Paint.NET) - Mozilla issue 1866655](https://bugzilla.mozilla.org/show_bug.cgi?id=1866655)
:::

::: info
By design, the native Graphics32 BMP implementation does not support reading the legacy indexed (palette), or RLE compressed BMP formats. For these formats it falls back to reading the BMP using the standard `TBitmap` class ([[GR32.ImageFormats.TBitmap]]).
:::

---

[members]
