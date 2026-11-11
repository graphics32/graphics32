---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TProgressiveRasterizer
kind: Class
declaration: "TProgressiveRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TProgressiveRasterizer
summary: "Multi-pass progressive rasterizer that renders coarse pixel blocks first, refining resolution across successive passes."
---

## Description

`TProgressiveRasterizer` implements multi-pass progressive subsampling. It begins by evaluating coarse pixel blocks of size $2^{\text{Steps}} \times 2^{\text{Steps}}$, filling destination blocks with initial samples. In subsequent passes, it successively halves the step size until individual 1-pixel resolution is reached.

This approach provides fast initial visual feedback during time-consuming sampling or raytracing operations, rapidly presenting a full coarse preview before refining fine details.

[members]
