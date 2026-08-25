---
layout: doc
docType: api
unit: GR32
entity: TCustomMap
kind: Class
declaration: "TCustomMap = class(TThreadPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
summary: "Abstract base class for two-dimensional data containers (bitmaps, vector maps, ordinal maps) with dimensions and resizing capabilities."
---

## Description

`TCustomMap` is the abstract base class for all two-dimensional mapped data structures in Graphics32, including `TCustomBitmap32`, ordinal maps (`TByteMap`, `TFloatMap`), and vector maps (`TVectorMap`). It establishes standard `Width` and `Height` management, buffer resizing routines, and dimension change notifications via `OnResize`.

## Constructors

| Name | Description |
| --- | --- |
| [Create](Constructors/Create.md) | Initializes a new `TCustomMap` instance, optionally setting initial dimensions. |

## Methods

| Name | Description |
| --- | --- |
| [Clear](Methods/Clear.md) | Clears the map content or resets buffer memory. |
| [Delete](Methods/Delete.md) | Sets map dimensions to zero and releases allocated buffer memory. |
| [Empty](Methods/Empty.md) | Returns `True` if either `Width` or `Height` is zero. |
| [Resized](Methods/Resized.md) | Triggers internal size change processing and fires `OnResize`. |
| [SetSize](Methods/SetSize.md) | Changes the map dimensions to specified width and height. |
| [SetSizeFrom](Methods/SetSizeFrom.md) | Copies dimensions from another map object or persistent container. |
| [ChangeSize](Methods/ChangeSize.md) | Protected virtual method that performs buffer allocation and dimension updates. |
| [SetHeight](Methods/SetHeight.md) | Protected setter method for the `Height` property. |
| [SetWidth](Methods/SetWidth.md) | Protected setter method for the `Width` property. |

## Properties

| Name | Type | Description |
| --- | --- | --- |
| [Height](Properties/Height.md) | `Integer` | Vertical dimension of the map in units or pixels. |
| [Width](Properties/Width.md) | `Integer` | Horizontal dimension of the map in units or pixels. |

## Events

| Name | Type | Description |
| --- | --- | --- |
| [OnResize](Events/OnResize.md) | `TNotifyEvent` | Fired whenever the map dimensions (`Width` or `Height`) change. |
