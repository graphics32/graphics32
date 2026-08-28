---
layout: doc
docType: api
unit: GR32_Polygons
entity: MakeAlphaEvenOddUP
kind: Variable
aliases: [MakeAlphaNonZeroUP, MakeAlphaEvenOddUPF, MakeAlphaNonZeroUPF]
declaration: "var MakeAlphaEvenOddUP: TFillProc;\nvar MakeAlphaNonZeroUP: TFillProc;\nvar MakeAlphaEvenOddUPF: TFillProc;\nvar MakeAlphaNonZeroUPF: TFillProc;"
summary: "Global function pointers for scanline alpha coverage builder procedures."
---

## Description

These global procedural variables point to the active CPU-optimized coverage builder function implementations dispatched by `PolygonsRegistry`:

- **`MakeAlphaEvenOddUP`**: Computes EvenOdd coverage alpha values for solid color filling.
- **`MakeAlphaNonZeroUP`**: Computes NonZero winding coverage alpha values for solid color filling.
- **`MakeAlphaEvenOddUPF`**: Computes EvenOdd coverage alpha values for custom filler rendering.
- **`MakeAlphaNonZeroUPF`**: Computes NonZero winding coverage alpha values for custom filler rendering.
