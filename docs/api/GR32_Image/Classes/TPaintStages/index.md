---
layout: doc
docType: api
unit: GR32_Image
entity: TPaintStages
kind: Class
hidden: true
declaration: "TPaintStages = class;"
inheritance:
  - TObject
  - TPaintStages
summary: "Manages an ordered list of TPaintStage records controlling image rendering pipeline stages."
---

## Description

`TPaintStages` manages an ordered collection of `TPaintStage` records in [[TCustomImage32]]. Each stage defines a discrete rendering step and execution context mask (`psmDesignTime`, `psmRunTime`, `psmExport`).

[members]
