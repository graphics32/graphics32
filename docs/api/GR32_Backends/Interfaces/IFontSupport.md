---
layout: doc
docType: api
unit: GR32_Backends
entity: IFontSupport
kind: Interface
declaration: "IFontSupport = interface(IUnknown)"
summary: "Interface for surface backends providing font object access and font change notification management."
---

# Interface IFontSupport

`IFontSupport` provides access to a `TFont` object owned by the backend, allowing callers to configure typeface, size, style, and color, and subscribe to font modification events.

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `Font` | `TFont` | Read/Write | Active font instance used for backend text operations. |
| `OnFontChange` | `TNotifyEvent` | Read/Write | Event handler triggered whenever font properties are modified. |

## Methods

- `function GetFont: TFont;`
- `procedure SetFont(const Font: TFont);`
- `function GetOnFontChange: TNotifyEvent;`
- `procedure SetOnFontChange(Handler: TNotifyEvent);`
- `procedure UpdateFont;`
