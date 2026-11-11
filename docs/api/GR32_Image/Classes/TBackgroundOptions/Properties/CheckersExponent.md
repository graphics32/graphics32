---
layout: doc
docType: api
unit: GR32_Image
parent: TBackgroundOptions
entity: TBackgroundOptions.CheckersExponent
kind: Property
declaration: "property CheckersExponent: Integer read FCheckersExponent write SetCheckersExponent default 3;"
summary: "Exponent of 2 defining checkerboard square size in pixels."
---

## Description

`CheckersExponent` sets the size of individual checkerboard grid squares as $2^{\text{CheckersExponent}}$ pixels (e.g. `3` $\rightarrow 2^3 = 8$ pixels).
