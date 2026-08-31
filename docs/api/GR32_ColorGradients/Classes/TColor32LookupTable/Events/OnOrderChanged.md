---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.OnOrderChanged
kind: Event
declaration: "property OnOrderChanged: TNotifyEvent read FOnOrderChanged write FOnOrderChanged;"
summary: "Occurs when Order property is modified and the table buffer is resized."
---

## Description

Fired immediately after `Order` is changed and the underlying buffer is resized to $2^{	ext{Order}}$ entries. Gradient samplers and fillers subscribe to `OnOrderChanged` to re-populate table entries automatically.
