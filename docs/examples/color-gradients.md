---
title: "Color Gradients & Fill Samplers"
---

# Color Gradients & Fill Samplers

Color gradients blend multiple colors continuously across a surface. Graphics32 provides a versatile gradient system in `GR32_ColorGradients.pas` that allows filling arbitrary vector polygons, shapes, and paths with smooth linear, radial, and conic color gradients.

In this tutorial, you will learn how to configure gradient color stops, set up linear and radial polygon fillers, and render smooth gradient shapes.

---

## 1. Setting Up Color Stops

A gradient is defined by a series of color stops positioned along a normalized range $[0.0..1.0]$, where $0.0$ represents the start point and $1.0$ represents the end point.

```pascal
uses
  GR32, GR32_ColorGradients;

var
  Gradient: TColor32Gradient;
begin
  Gradient := TColor32Gradient.Create;
  try
    // Clear default stops
    Gradient.ClearStops;

    // Add color stops (Position [0..1], Color32)
    Gradient.AddColorStop(0.00, clRed32);                      // Start at Red
    Gradient.AddColorStop(0.50, Color32(255, 255, 0, 255));    // Middle Yellow
    Gradient.AddColorStop(1.00, clBlue32);                     // End at Blue

    // Color stops are interpolated smoothly across the gradient line
  finally
    Gradient.Free;
  end;
end;
```

---

## 2. Filling Shapes with Linear Gradients

A linear gradient evaluates color along a directed vector line segment defined by StartPoint $(X_1, Y_1)$ and EndPoint $(X_2, Y_2)$.

```pascal
uses
  GR32, GR32_Polygons, GR32_VectorUtils, GR32_ColorGradients;

var
  Bitmap: TBitmap32;
  Filler: TLinearGradientPolygonFiller;
  Points: TArrayOfFloatPoint;
begin
  Bitmap := TBitmap32.Create;
  Filler := TLinearGradientPolygonFiller.Create;
  try
    Bitmap.SetSize(600, 400);
    Bitmap.Clear(clWhite32);

    // 1. Configure gradient color stops
    Filler.Gradient.AddColorStop(0.0, Color32(255, 100, 50, 255)); // Warm Orange
    Filler.Gradient.AddColorStop(1.0, Color32(100, 50, 200, 255)); // Deep Purple

    // 2. Set linear gradient start and end coordinates
    Filler.StartPoint := FloatPoint(100.0, 100.0);
    Filler.EndPoint   := FloatPoint(500.0, 300.0);

    // 3. Define target polygon geometry (e.g. rounded rectangle)
    Points := Rectangle(FloatRect(50.0, 50.0, 550.0, 350.0));

    // 4. Render polygon filled with linear gradient filler
    PolygonFS(Bitmap, Points, Filler);

    Bitmap.SaveToFile('linear_gradient.png');
  finally
    Filler.Free;
    Bitmap.Free;
  end;
end;
```

---

## 3. Filling Shapes with Radial Gradients

Radial gradients emanate outward from a central focal point $(Cx, Cy)$ to an outer radius $Radius$.

```pascal
var
  Bitmap: TBitmap32;
  Filler: TRadialGradientPolygonFiller;
  CirclePoly: TArrayOfFloatPoint;
begin
  Bitmap := TBitmap32.Create;
  Filler := TRadialGradientPolygonFiller.Create;
  try
    Bitmap.SetSize(500, 500);
    Bitmap.Clear(clBlack32);

    // Configure radial color stops (White core glowing into Cyan and Dark Blue)
    Filler.Gradient.AddColorStop(0.0, clWhite32);
    Filler.Gradient.AddColorStop(0.4, Color32(0, 220, 255, 255));
    Filler.Gradient.AddColorStop(1.0, Color32(0, 20, 80, 0)); // Fade to transparent

    // Set center position and radius
    Filler.EllipseBounds := FloatRect(100.0, 100.0, 400.0, 400.0);

    // Define boundary polygon to receive the radial gradient fill
    CirclePoly := Circle(250.0, 250.0, 200.0, 64);

    // Fill polygon with radial gradient
    PolygonFS(Bitmap, CirclePoly, Filler);

    Bitmap.SaveToFile('radial_glow.png');
  finally
    Filler.Free;
    Bitmap.Free;
  end;
end;
```

---

## 4. Wrap Modes for Repeating Gradients

When the polygon boundary extends past the gradient start/end points, `WrapMode` dictates how colors repeat beyond the boundaries:

- **`wmClamp`**: Extends edge colors indefinitely (default).
- **`wmRepeat`**: Repeats the gradient pattern from start to end continuously.
- **`wmMirror`**: Alternates and flips the gradient direction back and forth seamlessly.

```pascal
// Enable repeating mirrored gradient fill
Filler.WrapMode := wmMirror;
```

---

## Summary

In this tutorial, you learned:
1. How to create color stops with `TColor32Gradient`.
2. How to set up `TLinearGradientPolygonFiller` to fill vector polygons along a vector direction.
3. How to set up `TRadialGradientPolygonFiller` for glow and spherical lighting effects.
4. How `WrapMode` controls clamp, repeat, and mirror gradient extensions.

Next, proceed to [Working with Interactive Layers in TImage32](./working-with-layers) to learn how to create interactive GUI applications with layers!
