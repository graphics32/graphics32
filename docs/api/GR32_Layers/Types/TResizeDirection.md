---
layout: doc
docType: api
unit: GR32_Layers
entity: TResizeDirection
kind: Type
declaration: "TResizeDirection = (ResizeDirectionE, ResizeDirectionNE, ResizeDirectionN, ResizeDirectionNW, ResizeDirectionW, ResizeDirectionSW, ResizeDirectionS, ResizeDirectionSE);"
summary: "Compass directions used to determine resize cursors for rubberband handles."
---

## Description

`TResizeDirection` identifies 8 compass directions ordered counter-clockwise in 45-degree increments from 0 to 360 degrees, used to map handles to standard resize cursors.

## Values

| Value | Description |
| --- | --- |
| `ResizeDirectionE` | East ($0^\circ$): Right side sizing handle. |
| `ResizeDirectionNE` | North-East ($45^\circ$): Top-right corner sizing handle. |
| `ResizeDirectionN` | North ($90^\circ$): Top side sizing handle. |
| `ResizeDirectionNW` | North-West ($135^\circ$): Top-left corner sizing handle. |
| `ResizeDirectionW` | West ($180^\circ$): Left side sizing handle. |
| `ResizeDirectionSW` | South-West ($225^\circ$): Bottom-left corner sizing handle. |
| `ResizeDirectionS` | South ($270^\circ$): Bottom side sizing handle. |
| `ResizeDirectionSE` | South-East ($315^\circ$): Bottom-right corner sizing handle. |
