---
layout: doc
docType: api
unit: GR32_Image
entity: ZoomAnimateTime
kind: Variable
aliases: [ZoomAnimateTime, ZoomAnimateDeltaTime]
declaration: |
  ZoomAnimateTime: Integer = 300;
  ZoomAnimateDeltaTime: Integer = 5;
summary: "Default timing values for smooth animated zoom transitions in image view controls."
---

## Description

The global `ZoomAnimateTime` and `ZoomAnimateDeltaTime` variables specify animation duration and frame interval parameters for smooth wheel and interactive zoom transitions in [[TMouseZoomOptions]].

| Variable | Default Value | Description |
| --- | --- | --- |
| `ZoomAnimateTime` | `300` | Total animation duration in milliseconds for completing a zoom step transition. |
| `ZoomAnimateDeltaTime` | `5` | Time delta in milliseconds between consecutive animation steps (defining the target animation frame rate). |
