---
layout: doc
docType: api
unit: GR32_Image
entity: DefaultCheckersColors
kind: Variable
declaration: |
  var DefaultCheckersColors: array[TBackgroundCheckerStyle] of TBackgroundOptions.TCheckersColors = (
    (clWhite, $00E0E0E0), // bcsCustom
    (clWhite, clWhite),   // bcsNone
    (clWhite, $00E0E0E0), // bcsLight
    ($00B0B0B0, $00606060), // bcsMedium
    ($00505050, clBlack)  // bcsDark
  );
summary: "Default color pairs for background checkerboard styles."
seealso:
  - "TBackgroundOptions.CheckersStyle"
---

## Description

`DefaultCheckersColors` defines default two-color lookup array indexed by [[TBackgroundCheckerStyle]] used in [[TBackgroundOptions]] to initialize background checkerboard pattern colors.
