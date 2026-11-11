---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.Options
kind: Property
aliases: [TPaintBoxOptions, pboWantArrowKeys, pboAutoFocus]
scope: Public
declaration: |
  type
    TPaintBoxOptions = set of (pboWantArrowKeys, pboAutoFocus);

  property Options: TPaintBoxOptions read FOptions write FOptions default [];
summary: "Set of behavioral flags for arrow key navigation and auto-focus."
---

## Description

`Options` configures keyboard focus and navigation behavior.

| Flag | Description |
| --- | --- |
| `pboWantArrowKeys` | Intercepts arrow keyboard events (`VK_LEFT`, `VK_RIGHT`, `VK_UP`, `VK_DOWN`) so the control processes them instead of the parent form's focus manager. |
| `pboAutoFocus` | Automatically sets input focus to the control when clicked with the mouse. |
