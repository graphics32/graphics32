---
layout: doc
docType: api
unit: GR32.ImageFormats
parent: IImageFormats
entity: IImageFormats.GetEnumerator
kind: Method
scope: Public
declaration: "function GetEnumerator: IImageFormatEnumerator;"
summary: "Creates and returns a new enumerator instance for registered formats."
returns:
  - type: IImageFormatEnumerator
    description: "The calculated [[IImageFormatEnumerator]] result."
---

## Description

`GetEnumerator` returns an [[IImageFormatEnumerator]] instance enabling `for..in` iteration over registered formats.
