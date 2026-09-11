---
title: "Working with Interactive Layers in TImage32"
---

# Working with Interactive Layers in TImage32

Graphics32 includes `TImage32`, a powerful visual component for VCL and Lazarus/LCL applications. One of `TImage32`'s best features is its built-in interactive layer management system. With layers, you can stack images, vector annotations, rubberband selection handles, and transparent overlays without manually re-rendering the entire canvas.

In this tutorial, you will learn how to create `TBitmapLayer` instances, control position and opacity, manage z-ordering, and attach interactive rubberband transformation handles.

---

## 1. What is a Layer in Graphics32?

All layers descend from `TCustomLayer` and reside inside `TImage32.Layers`. When `TImage32` repaints, it draws layers sequentially from lowest index (background) to highest index (foreground).

### Key Layer Classes
- **`TBitmapLayer`**: Holds a sub-bitmap that can be positioned, scaled, and blended over `TImage32`.
- **`TPositionedLayer`**: Base class for layers with spatial boundaries (`Location` rectangle) and mouse event handling.
- **`TRubberbandLayer`**: Interactive layer displaying sizing handles around a target layer for mouse resizing and rotation.

---

## 2. Creating and Adding a Bitmap Layer

Adding a `TBitmapLayer` to `TImage32` is straightforward:

```pascal
uses
  Classes, GR32, GR32_Image, GR32_Layers;

procedure AddWatermarkLayer(Image32: TImage32);
var
  Layer: TBitmapLayer;
begin
  // Create layer belonging to Image32.Layers collection
  Layer := TBitmapLayer.Create(Image32.Layers);
  try
    // Load bitmap graphic into layer
    Layer.Bitmap.LoadFromFile('watermark_logo.png');

    // Enable alpha blending
    Layer.Bitmap.DrawMode := dmBlend;

    // Set location rectangle (Left, Top, Right, Bottom)
    Layer.Location := FloatRect(20.0, 20.0, 220.0, 120.0);

    // Adjust layer translucency (50% opacity)
    Layer.MasterAlpha := 128;

    // Ensure layer responds to mouse clicks and drag events
    Layer.MouseEvents := True;
  except
    Layer.Free;
    raise;
  end;
end;
```

---

## 3. Interactive Dragging & Selection with TRubberbandLayer

To allow users to drag, resize, and rotate a layer interactively on screen with sizing handles, attach a `TRubberbandLayer`:

```pascal
procedure AttachRubberbandToLayer(Image32: TImage32; TargetLayer: TPositionedLayer);
var
  Rubberband: TRubberbandLayer;
begin
  // Create rubberband selection handles around TargetLayer
  Rubberband := TRubberbandLayer.Create(Image32.Layers);

  // Associate rubberband with target layer
  Rubberband.ChildLayer := TargetLayer;

  // Options: Allow moving, resizing, and keeping aspect ratio
  Rubberband.Options := [roProportional, roConstrained];

  // Custom handle appearance
  Rubberband.HandleFill := clWhite32;
  Rubberband.HandleFrame := clNavy32;
end;
```

---

## 4. Layer Z-Ordering and Visibility

You can reorder layers or toggle visibility at runtime easily using layer list properties:

```pascal
procedure ManipulateLayers(Layer: TCustomLayer);
begin
  // Bring layer to front of display stack
  Layer.BringToFront;

  // Send layer behind other layers
  Layer.SendToBack;

  // Move layer up or down by index position
  Layer.Index := Layer.Index + 1;

  // Hide or show layer
  Layer.Visible := False;
end;
```

---

## 5. Responding to Layer Mouse Events

`TPositionedLayer` provides event hooks for mouse interaction (`OnMouseDown`, `OnMouseMove`, `OnMouseUp`):

```pascal
procedure TFormMain.LayerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Layer: TPositionedLayer;
begin
  if Sender is TPositionedLayer then
  begin
    Layer := TPositionedLayer(Sender);
    ShowMessage(Format('Clicked on layer at local coordinates (%d, %d)', [X, Y]));
  end;
end;
```

---

## Summary

In this tutorial, you learned:
1. How `TImage32` manages interactive layer stacks.
2. How to create `TBitmapLayer` instances and set location and opacity (`MasterAlpha`).
3. How to attach `TRubberbandLayer` for interactive mouse dragging and resizing.
4. How to manage layer visibility, z-order (`BringToFront`, `SendToBack`), and mouse events.

Next, check out [Loading, Saving & Image Formats](./loading-and-saving) to learn how to load and save images in PNG, JPEG, BMP, and PSD formats!
