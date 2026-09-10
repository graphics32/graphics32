---
layout: doc
docType: api
unit: GR32_Image
entity: TBackgroundOptions
kind: Class
declaration: "TBackgroundOptions = class(TNotifiablePersistent);"
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TBackgroundOptions
summary: "Internal background rendering options class used by TCustomImage32 for the Background property."
---

## Description

`TBackgroundOptions` is an internal helper class used by `TCustomImage32` to manage background rendering options for its `Background` property. It manages background fill styles (`bfsColor`, `bfsCheckers`, `bfsPattern`), checkerboard color themes, pattern bitmaps, borders, and drop shadow decorations surrounding or beneath the bitmap surface.

[members]

---

## See also

- "[[TCustomImage32]]"
