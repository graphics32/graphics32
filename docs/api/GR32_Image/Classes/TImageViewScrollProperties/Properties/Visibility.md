---
layout: doc
docType: api
unit: GR32_Image
parent: TImageViewScrollProperties
entity: TImageViewScrollProperties.Visibility
kind: Property
aliases: [TScrollBarVisibility, svAlways, svHidden, svAuto]
declaration: |
  type
    TScrollBarVisibility = (svAlways, svHidden, svAuto);

  property Visibility: TScrollBarVisibility read FVisibility write SetVisibility default svAlways;
summary: "Scrollbar display mode."
---

## Description

`Visibility` controls scrollbars display behavior.

| Value | Description |
| --- | --- |
| `svAlways` | Scrollbars are always visible regardless of bitmap or viewport dimensions. |
| `svHidden` | Scrollbars are permanently hidden. |
| `svAuto` | Scrollbars automatically appear when bitmap dimensions exceed the viewport area and hide when fitting inside. |
