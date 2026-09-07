---
layout: doc
docType: api
unit: GR32_Layers
entity: Layer Option Bits
kind: Constant
aliases: [LOB_VISIBLE, LOB_GDI_OVERLAY, LOB_MOUSE_EVENTS, LOB_NO_UPDATE, LOB_NO_CAPTURE, LOB_INVALID, LOB_FORCE_UPDATE, LOB_RESERVED_24, LOB_RESERVED_MASK]
summary: "Bitmask flags controlling visibility, update behavior, mouse interaction, and repaint options for layers."
---

## Description

`Layer Option Bits` (`LOB_*`) define bitwise flags used in `TCustomLayer.LayerOptions` to control visibility, repainting, overlay drawing, and mouse message capture behavior.

## Constants Table

| Constant | Value | Description |
| --- | --- | --- |
| `LOB_VISIBLE` | `$80000000` | Controls layer visibility. When set, the layer is rendered during paint cycles. |
| `LOB_GDI_OVERLAY` | `$40000000` | Indicates that the layer performs drawing when its owner control draws GDI overlays. |
| `LOB_MOUSE_EVENTS` | `$20000000` | Specifies whether the layer responds to mouse messages and hit testing. |
| `LOB_NO_UPDATE` | `$10000000` | Disables automatic repainting when layer location or properties change. |
| `LOB_NO_CAPTURE` | `$08000000` | Overrides automatic mouse message capturing when the left mouse button is pressed over the layer. Has no effect if `LOB_MOUSE_EVENTS` is cleared. |
| `LOB_INVALID` | `$04000000` | Used internally by the repaint optimizer to mark invalidated layer areas. |
| `LOB_FORCE_UPDATE` | `$02000000` | Used internally to force an area repaint when a layer is being hidden. |
| `LOB_RESERVED_24` | `$01000000` | Reserved bit. |
| `LOB_RESERVED_MASK` | `$FF000000` | Bitmask covering all system and option bit flags (bits 24..31). |
