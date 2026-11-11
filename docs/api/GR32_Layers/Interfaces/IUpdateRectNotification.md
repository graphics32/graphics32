---
layout: doc
docType: api
unit: GR32_Layers
entity: IUpdateRectNotification
kind: Interface
declaration: "IUpdateRectNotification = interface(IUnknown)"
summary: "Interface for receiving notification when rectangular areas are invalidated within a layer collection."
---

## Description

`IUpdateRectNotification` allows external subscribers or controls to be notified whenever a rectangular region within a layer collection needs repainting.

## Methods

### AreaUpdated
```pascal
procedure AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
```
Fired when the specified rectangular area (`AArea`) is invalidated and requires redrawing.
