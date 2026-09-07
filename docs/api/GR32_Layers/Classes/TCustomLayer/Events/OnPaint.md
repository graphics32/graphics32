---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnPaint
kind: Event
scope: Published
declaration: |
  type
    TPaintLayerEvent = procedure(Sender: TObject; Buffer: TBitmap32) of object;
  
  property OnPaint: TPaintLayerEvent read ... write ...;
summary: "Fired during the layer paint cycle to perform custom rendering onto the target bitmap buffer."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance being painted."
  - name: Buffer
    type: TBitmap32
    description: "Target 32-bit bitmap buffer where rendering takes place."
---

## Description

`OnPaint` allows custom rendering logic to draw directly onto `Buffer`.