---
layout: doc
docType: api
unit: GR32_Image
parent: TPaintStages
entity: TPaintStages.Dirty
kind: Property
scope: Public
declaration: "property Dirty: boolean read FDirty write FDirty;"
summary: "Indicates whether stage lists or handlers require rebuilding or reinitialization."
---

## Description

`Dirty` flags whether the stages in [[TPaintStages]] have been modified and require processing before rendering.
