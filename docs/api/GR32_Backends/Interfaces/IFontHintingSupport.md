---
layout: doc
docType: api
unit: GR32_Backends
entity: IFontHintingSupport
kind: Interface
declaration: "IFontHintingSupport = interface(IUnknown)"
summary: "Interface for configuring text font hinting modes (deprecated)."
---

## Description

`IFontHintingSupport` provides text hinting options (`TTextHinting`).

::: warning Deprecated
Font hinting settings are deprecated and no longer active. See `IGNORE_HINTING_DEPRECATED` in `GR32.inc`.
:::

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `Hinting` | `TTextHinting` | Read/Write | Active font hinting mode (`thNone`, `thNoHorz`, `thHinting`). |

## Methods

- `function GetHinting: TTextHinting;`
- `procedure SetHinting(Value: TTextHinting);`
