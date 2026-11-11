---
title: "10. Repaint Optimization"
---

# Repaint Optimization

When building graphical user interfaces, animations, audio visualizers, or CAD applications, redrawing screen controls efficiently is critical for smooth performance. Repeatedly invalidating screen controls during rapid drawing operations causes lag, stuttering, and flickering.

In this tutorial, you will learn how to optimize rendering performance in Graphics32 using `BeginUpdate`/`EndUpdate` batching and partial rectangle updates with `Changed()`.

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
      // TBitmap32.Pixel does not call Changed internally, so we do it manually
      Bitmap.Changed;
    end;

  finally
    // 2. Resume and trigger a SINGLE composite repaint event
    Bitmap.EndUpdate;
  end;
end;
```

::: info
`TBitmap32.Pixel` is special, among all the different methods that modify the bitmap, in that it **doesn't** call `Changed` internally.

The reason is simply that it would incur far too much overhead if each single modification of a pixel caused an invalidation->repaint cycle.

Therefore it is up to you to call `Changed` when modifying the image via `Pixel`. Of course you only need to do this if the bitmap in question is that of a `TImage32`, `TImgView32`, a bitmap layer, etc. This also goes for any modifications that are made directly into pixel memory.
:::

---

## 2. Partial Invalidations with Changed(TRect)

Calling `Bitmap.Changed` repaints the entire screen control so this is wasteful if you modify only a small section of a large bitmap (such as updating a mouse cursor overlay or a small animated sprite).

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

## 3. Animation Loops Without Flickering

For high-framerate animations or game loops, process frame updates inside a high-resolution timer or `Application.OnIdle` handler using `BeginUpdate`/`EndUpdate`:

```pascal
// This is an Application.OnIdle event handler:
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

::: box-green

In this tutorial, you learned:
1. How to use `BeginUpdate` and `EndUpdate` to batch multiple drawing commands into a single frame repaint.
2. How to invalidate partial bounding boxes with `Changed(TRect)` for fast sub-region updates.
3. How to build smooth, flicker-free animation loops.

:::

Congratulations! You have completed the Graphics32 Examples & Tutorials series! Explore the [API Reference](/api/) to dive deep into individual classes and functions.
