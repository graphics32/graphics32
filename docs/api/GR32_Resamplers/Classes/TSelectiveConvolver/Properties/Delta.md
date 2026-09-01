---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TSelectiveConvolver
entity: TSelectiveConvolver.Delta
kind: Property
scope: Published
declaration: "property Delta: Integer read FDelta write FDelta;"
summary: "Specifies the maximum color difference threshold for selective convolution."
---

## Description

`Delta` sets the maximum allowable color distance threshold for neighboring pixels during convolution. Samples exceeding this color threshold are excluded, preserving sharp edges.
