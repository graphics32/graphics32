---
title: "Repaint Optimization & Custom Canvas Drawing"
---

# Repaint Optimization & Custom Canvas Drawing

When building graphical user interfaces, animations, audio visualizers, or CAD applications, redrawing screen controls efficiently is critical for smooth performance. Repeatedly invalidating screen controls during rapid drawing operations causes lag, stuttering, and flickering.

In this tutorial, you will learn how to optimize rendering performance in Graphics32 using `BeginUpdate`/`EndUpdate` batching, partial rectangle updates with `Changed()`, and custom paint stages on `TImage32`.

---

## 1. Batching Operations with BeginUpdate and EndUpdate

Whenever you modify multiple pixels or draw multiple shapes on a `TBitmap32` that is displayed inside a UI control (like `TImage32`), every drawing call triggers a repaint event by default.

To prevent unnecessary repaints, pause the bitmap update notification mechanism using `BeginUpdate` before drawing, and resume it with `EndUpdate` when finished. Each modification inside a `BeginUpdate`/`EndUpdate` block will queue an update but the updates will not be processed until the final `EndUpdate`.

```pascal
procedure DrawComplexScene(Bitmap: TBitmap32);
var
  I: Integer;
  X, Y: Integer;
begin
  // 1. Suspend update notifications
  Bitmap.BeginUpdate;
  try
    // Clear surface
    Bitmap.Clear(clWhite32); // Clear internally calls Changed

    // Perform hundreds of drawing operations
    for I := 0 to 500 do
    begin
      X := Random(Bitmap.Width - 50);
      Y := Random(Bitmap.Height - 50);
      Bitmap.Pixel[X, Y] := Color32(Random(255), Random(255), Random(255), 128);
      // Pixel does not call Changed internally, so we do it manually
      Bitmap.Changed;
    end;

  finally
    // 2. Resume and trigger a SINGLE composite repaint event
    Bitmap.EndUpdate;
  end;
end;
```

---

## 2. Partial Invalidations with Changed(TRect)

If you modify only a small section of a large bitmap (such as updating a mouse cursor overlay or a small animated sprite), calling `Bitmap.Changed` repaints the entire screen control.

Instead, pass a target bounding rectangle (`TRect`) to `Changed(Rect)` to invalidate and redraw only the updated region!

```pascal
procedure UpdateSpritePosition(Bitmap: TBitmap32; OldRect, NewRect: TRect);
begin
  Bitmap.BeginUpdate;
  try
    // Erase sprite at old position
    // Note: FillRectS internally calls Bitmap.Changed(OldRect);
    Bitmap.FillRectS(OldRect.Left, OldRect.Top, OldRect.Right, OldRect.Bottom, clWhite32);

    // Draw sprite at new position
    // Note: FillRectS internally calls Bitmap.Changed(NewRect);
    Bitmap.FillRectS(NewRect.Left, NewRect.Top, NewRect.Right, NewRect.Bottom, clRed32);
  finally
    Bitmap.EndUpdate; // Only the two modified rectangles are repainted
  end;
end;
```

---

## 3. Custom Painting with TImage32 Paint Stages
***TODO***
`TImage32` uses a modular paint stage pipeline (`OnPaintStage`). Instead of drawing permanently onto the underlying bitmap, you can intercept paint stages to render temporary visual overlays (like grid lines, crosshairs, selection rectangles, or HUD elements) directly onto the back-buffer without altering bitmap pixels!

```pascal
procedure TFormMain.Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
var
  Image32: TImage32;
  X, Y: Integer;
begin
  Image32 := TImage32(Sender);

  // Paint Stage 0: Custom background grid (drawn before bitmap)
  if StageNum = PST_CUSTOM_BACK then
  begin
    Buffer.Clear(Color32(240, 240, 245, 255));

    // Draw 20x20 pixel grid background
    for X := 0 to Buffer.Width div 20 do
      Buffer.VertLineS(X * 20, 0, Buffer.Height, Color32(210, 210, 220, 255));

    for Y := 0 to Buffer.Height div 20 do
      Buffer.HorzLineS(0, Buffer.Width, Y * 20, Color32(210, 210, 220, 255));
  end;

  // Paint Stage 1: Custom foreground overlay (drawn after bitmap)
  if StageNum = PST_CUSTOM_FORE then
  begin
    // Draw crosshair overlay centered on canvas
    Buffer.PenColor := clRed32;
    Buffer.VertLineS(Buffer.Width div 2, 0, Buffer.Height, clRed32);
    Buffer.HorzLineS(0, Buffer.Width, Buffer.Height div 2, clRed32);
  end;
end;
```

---

## 4. Animation Loops Without Flickering

For high-framerate animations or game loops, process frame updates inside a high-resolution timer or `Application.OnIdle` handler using `BeginUpdate`/`EndUpdate`:

```pascal
procedure TFormMain.RenderAnimationFrame(Sender: TObject; var Done: Boolean);
begin
  // Perform animation physics/math update...
  Inc(AnimationFrameCounter);

  // Render frame
  Image32.Bitmap.BeginUpdate;
  try
    Image32.Bitmap.Clear(clBlack32);

    // Draw animated objects
    Image32.Bitmap.FillRectTS(
      Round(200 + 100 * Sin(AnimationFrameCounter * 0.05)),
      150,
      Round(260 + 100 * Sin(AnimationFrameCounter * 0.05)),
      210,
      clYellow32
    );
  finally
    Image32.Bitmap.EndUpdate;
  end;

  // Continue running animation loop on next idle cycle
  Done := False;
end;
```

---

## Summary

In this tutorial, you learned:
1. How to use `BeginUpdate` and `EndUpdate` to batch multiple drawing commands into a single frame repaint.
2. How to invalidate partial bounding boxes with `Changed(TRect)` for fast sub-region updates.
3. How to use `TImage32.OnPaintStage` to render non-destructive background grids and foreground overlays.
4. How to build smooth, flicker-free animation loops.

Congratulations! You have completed the Graphics32 Examples & Tutorials series! Explore the [API Reference](/api/) to dive deep into individual classes and functions.
