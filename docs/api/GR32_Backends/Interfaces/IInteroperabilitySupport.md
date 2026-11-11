---
layout: doc
docType: api
unit: GR32_Backends
entity: IInteroperabilitySupport
kind: Interface
declaration: "IInteroperabilitySupport = interface(IUnknown)"
summary: "Interface for surface backends supporting direct pixel copying from TGraphic objects."
---

## Description

`IInteroperabilitySupport` enables direct copying of graphical images from VCL/LCL `TGraphic` objects into the backend surface buffer.

## Methods

### CopyFrom
```pascal
function CopyFrom(Graphic: TGraphic): Boolean; overload;
```
Copies pixel data from the specified `Graphic` instance onto the surface. Returns `True` if successfully copied.
