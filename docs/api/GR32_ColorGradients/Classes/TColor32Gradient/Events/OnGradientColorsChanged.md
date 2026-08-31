---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.OnGradientColorsChanged
kind: Event
declaration: "property OnGradientColorsChanged: TNotifyEvent read FOnGradientColorsChanged write FOnGradientColorsChanged;"
summary: "Occurs when color stops or gradient colors are added, cleared, or modified."
---

## Description

Fired whenever the gradient is altered. Attached samplers and fillers subscribe to `OnGradientColorsChanged` to invalidate lookup tables and trigger repaints.
