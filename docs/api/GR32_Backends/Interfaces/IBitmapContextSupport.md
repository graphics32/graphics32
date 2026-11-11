---
layout: doc
docType: api
unit: GR32_Backends
entity: IBitmapContextSupport
kind: Interface
declaration: "IBitmapContextSupport = interface(IUnknown)"
summary: "Interface for backends managing DIB bitmap headers and GDI bitmap handles."
---

## Description

`IBitmapContextSupport` exposes platform bitmap header metadata (`TBitmapInfo`) and GDI/LCL bitmap handles (`THandle`/`HBITMAP`).

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `BitmapInfo` | `TBitmapInfo` | Read-only | Windows DIB bitmap header information structure. |
| `BitmapHandle` | `THandle` | Read-only | Native OS GDI bitmap section handle. |

## Methods

- `function GetBitmapInfo: TBitmapInfo;`
- `function GetBitmapHandle: THandle;`
