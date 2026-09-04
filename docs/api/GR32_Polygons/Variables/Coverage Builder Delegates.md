---
layout: doc
docType: api
unit: GR32_Polygons
entity: MakeAlpha
kind: Variable
aliases: [MakeAlphaEvenOddUP, MakeAlphaNonZeroUP, MakeAlphaEvenOddUPF, MakeAlphaNonZeroUPF]
declaration: |
  var MakeAlphaEvenOddUP: TFillProc;
  var MakeAlphaNonZeroUP: TFillProc;
  var MakeAlphaEvenOddUPF: TFillProc;
  var MakeAlphaNonZeroUPF: TFillProc;
summary: "Global function pointers for scanline alpha coverage builder procedures."
---

## Description

These global procedural variables point to the active CPU-optimized coverage builder function implementations:

| Delegate | Description |
| --- | --- |
| **`MakeAlphaEvenOddUP`** | Computes EvenOdd coverage alpha values for solid color filling. |
| **`MakeAlphaNonZeroUP`** | Computes NonZero winding coverage alpha values for solid color filling. |
| **`MakeAlphaEvenOddUPF`** | Computes EvenOdd coverage alpha values for custom filler rendering. |
| **`MakeAlphaNonZeroUPF`** | Computes NonZero winding coverage alpha values for custom filler rendering. |
