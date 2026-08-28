---
layout: doc
docType: api
unit: GR32_Backends
entity: ICanvasSupport
kind: Interface
declaration: "ICanvasSupport = interface(IUnknown)"
summary: "Interface for backends providing VCL/LCL TCanvas allocation and management."
---

## Description

`ICanvasSupport` provides access to a standard VCL/LCL `TCanvas` bound to the backend surface device context.

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `Canvas` | `TCanvas` | Read-only | Returns the `TCanvas` instance associated with the backend device context. |
| `OnCanvasChange` | `TNotifyEvent` | Read/Write | Event handler triggered when canvas state or handles change. |

## Methods

- `function GetCanvas: TCanvas;`
- `function CanvasAllocated: Boolean;`
- `procedure DeleteCanvas;`
- `function GetCanvasChange: TNotifyEvent;`
- `procedure SetCanvasChange(Handler: TNotifyEvent);`
