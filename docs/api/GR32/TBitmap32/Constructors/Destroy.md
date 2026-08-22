---
layout: doc
docType: api
unit: GR32
entity: TBitmap32.Destroy
kind: Destructor
declaration: "destructor Destroy; override;"
summary: "Disposes of the TBitmap32 instance, releasing allocated pixel buffers, backend memory surfaces, and attached resamplers."
---

## Remarks

Do not call `Destroy` directly. Use `Free` to safely check for `nil` before destroying the instance.
