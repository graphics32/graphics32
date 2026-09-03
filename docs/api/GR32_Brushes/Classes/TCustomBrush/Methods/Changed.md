---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.Changed
kind: Method
declaration: "procedure Changed; override;"
summary: "Triggers change notification updates up to the parent brush collection."
---

## Description

`Changed` overrides `TNotifiablePersistent.Changed`. When unlocked, it notifies the parent [[BrushCollection]] that brush properties have changed.
