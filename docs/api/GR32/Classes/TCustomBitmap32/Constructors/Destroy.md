---
layout: doc
docType: api
unit: GR32
entity: TCustomBitmap32.Destroy
kind: Destructor
declaration: "destructor Destroy; override;"
summary: "Disposes of the bitmap instance, releasing allocated pixel buffers, backend memory surfaces, and attached resamplers."
---

::: warning
Do not call `Destroy` directly. Use `Free` to safely check for `nil` before destroying the instance.
:::
