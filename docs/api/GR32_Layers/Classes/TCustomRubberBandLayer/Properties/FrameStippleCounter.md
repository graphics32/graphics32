---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.FrameStippleCounter
kind: Property
scope: Public
declaration: "property FrameStippleCounter: TFloat read FFrameStippleCounter write SetFrameStippleCounter;"
summary: "Initial offset phase in the stipple color pattern."
seealso:
  - "[[line-patterns]]"
---

## Description

`FrameStippleCounter` shifts the phase offset of the frame stipple pattern.

::: tip
By using a timer (`TTimer`) and modifying the value of `FrameStippleCounter`, it is possible to animate the rubberband. This effect if often called [marching ants or marquee](https://en.wikipedia.org/wiki/Marching_ants).

```pascal
//
// The form contains a TTimer named "TimerMarchingAnts" with Interval=50
// and hooked up to the following event handler method.
//
// We are using a simple alternating black/white stipple:
//   FLayerRubberband.FrameStipple :=
//     [
//       clWhite32, clWhite32, clWhite32, clWhite32,
//       clBlack32, clBlack32, clBlack32, clBlack32
//     ];
//
procedure TFormMain.TimerMarchingAntsTimer(Sender: TObject);
begin
  // Only animate when our application is active
  if (not Application.Active) then
    exit;

  // Only animate when interaction is in progress
  if (FLayerRubberband.ActiveHitTest <> nil) then
    FLayerRubberband.FrameStippleCounter := FLayerRubberband.FrameStippleCounter + 1.5;
end;
```

![](/images/marching-ants.gif)
:::