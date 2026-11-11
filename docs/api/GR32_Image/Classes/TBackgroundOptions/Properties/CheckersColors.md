---
layout: doc
docType: api
unit: GR32_Image
parent: TBackgroundOptions
entity: TBackgroundOptions.CheckersColors
kind: Property
declaration: |
  type
    TCheckersColors = array[0..1] of TColor32;

  property CheckersColors: TCheckersColors read FCheckersColors;
summary: "Array of two TColor32 values representing odd/even square colors of the checkers pattern."
---

## Description

`CheckersColors` holds the pair of 32-bit colors used to draw odd and even squares in the background checkerboard pattern.
