---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.OnPaintStage
kind: Event
aliases: [TPaintStageEvent]
declaration: |
  type
    TPaintStageEvent = procedure(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal) of object;

  property OnPaintStage: TPaintStageEvent read FOnPaintStage write FOnPaintStage;
parameters:
  - name: Sender
    type: TObject
    description: "The image control executing the paint stage."
  - name: Buffer
    type: TBitmap32
    description: "The destination 32-bit bitmap surface being rendered into."
  - name: StageNum
    type: Cardinal
    description: "The identifier or index number of the custom paint stage being executed."
summary: "Fired during execution of custom PST_CUSTOM paint stage."
seealso:
  - "[[Paint Stage Constants]]"
---

## Description

`OnPaintStage` is invoked when executing a paint stage with `PST_CUSTOM`, allowing user code to render custom graphics into the destination `Buffer`.

---
