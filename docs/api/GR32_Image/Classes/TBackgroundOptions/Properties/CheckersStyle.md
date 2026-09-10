---
layout: doc
docType: api
unit: GR32_Image
parent: TBackgroundOptions
entity: TBackgroundOptions.CheckersStyle
kind: Property
aliases: [TBackgroundCheckerStyle, bcsCustom, bcsNone, bcsLight, bcsMedium, bcsDark]
declaration: |
  type
    TBackgroundCheckerStyle = (bcsCustom, bcsNone, bcsLight, bcsMedium, bcsDark);

  property CheckersStyle: TBackgroundCheckerStyle read FCheckersStyle write SetCheckersStyle default bcsNone;
summary: "Background checkerboard pattern color scheme."
seealso:
  - "[[DefaultCheckersColors]]"
---

## Description

`CheckersStyle` selects the checkerboard color schemes preset (default `bcsNone`).

| Value | Description |
| --- | --- |
| `bcsCustom` | Custom checkerboard colors specified explicitly by [[CheckersColorOdd]] and [[CheckersColorEven]]. |
| `bcsNone` | Disables checkerboard background pattern rendering. |
| `bcsLight` | Light grey and white checkerboard pattern suitable for dark images. |
| `bcsMedium` | Medium grey checkerboard pattern. |
| `bcsDark` | Dark grey and black checkerboard pattern suitable for bright or light images. |
