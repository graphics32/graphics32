---
layout: doc
docType: api
unit: GR32_Brushes
entity: TBrushCollection
kind: Class
declaration: "TBrushCollection = class(TNotifiablePersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TBrushCollection
summary: "Container class managing an ordered collection of TCustomBrush instances."
---

## Description

`TBrushCollection` manages an ordered list of [[TCustomBrush]] items. It provides methods to instantiate, insert, clear, delete, and iterate through brush instances owned by a parent persistent object.

Changes to brush items within the collection trigger change notification updates to the collection's owner.

[members]
