---
layout: doc
docType: api
unit: GR32
entity: TCustomSampler
kind: Class
declaration: "TCustomSampler = class(TNotifiablePersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomSampler
summary: "Abstract base class representing color sampling engines capable of returning a `TColor32` at arbitrary sub-pixel floating-point coordinates (X, Y)."
---

## Description

`TCustomSampler` is the abstract base class for all sampling engines in Graphics32. It provides a generalized interface for retrieving 32-bit ARGB color values (`TColor32`) at coordinate locations specified in integer, 16.16 fixed-point (`TFixed`), or single-precision floating-point (`TFloat`) formats.

Sampler implementations can represent procedurally generated patterns, color gradients, coordinate transformations, nested sampler chains, or bitmap resamplers.

Before performing intensive sampling loops, callers must call `PrepareSampling` to allow the sampler (and any nested child samplers) to initialize internal caches or lookup tables. After sampling is complete, `FinalizeSampling` should be called to release temporary resources.

[members]
