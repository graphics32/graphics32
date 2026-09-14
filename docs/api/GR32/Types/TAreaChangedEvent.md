---
layout: doc
docType: api
unit: GR32
entity: TAreaChangedEvent
kind: Type
declaration: "type TAreaChangedEvent = procedure(Sender: TObject; const Area: TRect; const Info: Cardinal) of object;"
summary: "Event type for bitmap area update and change notifications."
parameters:
  - name: Sender
    type: TObject
    description: "Source bitmap generating the change event."
  - name: Area
    type: TRect
    description: "Bounding rectangle of the modified pixel area."
  - name: Info
    type: Cardinal
    description: "Flags bitmask specifying update category or state information."
seealso:
  - "[[TCustomBitmap32.OnAreaChanged]]"
---

## Description

`TAreaChangedEvent` defines the callback event signature for bitmap area change notifications (`OnAreaChanged`).

It passes the `Sender` bitmap instance, the bounding rectangle `Area` enclosing modified pixels, and an `Info` bitmask containing [[GR32.Area Info|area change flags]].
