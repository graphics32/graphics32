---
layout: doc
docType: api
unit: GR32_Image
entity: TBitmap32Item
kind: Class
hidden: true
declaration: |
  TBitmap32Item = class(TCollectionItem);

  TBitmap32ItemClass = class of TBitmap32Item;
inheritance:
  - TCollectionItem
  - TBitmap32Item
summary: "Collection item holding an individual TBitmap32 instance inside TBitmap32Collection."
aliases: [TBitmap32ItemClass]
seealso:
  - "[[TBitmap32Collection]]"
  - "[[TBitmap32List]]"
---

## Description

`TBitmap32Item` represents a single item inside `TBitmap32Collection`, holding an owned `TBitmap32` instance.

[members]
