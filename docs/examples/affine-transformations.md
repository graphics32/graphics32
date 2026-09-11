---
title: "Affine Transformations"
---

# Affine Transformations (Rotate, Scale, Skew)

Affine transformations allow you to rotate, scale, translate (move), and skew (shear) bitmaps while preserving parallel lines. Graphics32 provides the `TAffineTransformation` class in `GR32_Transforms.pas` to perform smooth matrix transformations on bitmap surfaces and vector paths.

In this tutorial, you will learn how to set up transformation matrices, rotate an image around its center point, and render transformed bitmaps.

---

## 1. Understanding TAffineTransformation

An affine transformation uses a 3x3 matrix to map points from source space $(X, Y)$ to destination space $(X', Y')$. In Graphics32, you configure transformations by calling fluent transformation methods:

- **`Translate(Dx, Dy)`**: Shifts coordinates by $(Dx, Dy)$.
- **`Scale(Sx, Sy)`**: Scales coordinates by horizontal factor $Sx$ and vertical factor $Sy$.
- **`Rotate(Cx, Cy, AngleDegrees)`**: Rotates coordinates around center point $(Cx, Cy)$ by specified degrees.
- **`Skew(Sx, Sy)`**: Shears coordinates horizontally and vertically.
- **`Clear`**: Resets matrix to identity.

---

## 2. Rotating a Bitmap Around Its Center

To rotate an image around its center without shifting it off-screen, follow this order of operations:
1. Translate origin to center: `Translate(-Width / 2, -Height / 2)`
2. Apply rotation: `Rotate(0, 0, Angle)`
3. Translate back to target destination center: `Translate(DstX, DstY)`

```pascal
uses
  GR32, GR32_Transforms, GR32_Resamplers;

var
  Src, Dst: TBitmap32;
  Transformer: TAffineTransformation;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  Transformer := TAffineTransformation.Create;
  try
    Src.LoadFromFile('logo.png');
    Dst.SetSize(600, 600);
    Dst.Clear(clWhite32); // Clear target canvas

    // Enable smooth resampling on source
    TKernelResampler.Create(Src).Kernel := TLanczosKernel.Create;

    // Build transformation chain
    Transformer.Clear;

    // 1. Move origin to bitmap center
    Transformer.Translate(-Src.Width * 0.5, -Src.Height * 0.5);

    // 2. Rotate by 45 degrees clockwise
    Transformer.Rotate(45);

    // 3. Move origin back to top-left corner
    Transformer.Translate(Src.Width * 0.5, Src.Height * 0.5);

    // 4. Move rotated result to center of destination canvas (300, 300)
    Transformer.Translate(300, 300);

    // Apply transformation blit from Src into Dst
    Transform(Dst, Src, Transformer);

    Dst.SaveToFile('rotated_logo.png');
  finally
    Transformer.Free;
    Dst.Free;
    Src.Free;
  end;
end;
```

This sequence of...
1. Moving the origin
2. Rotating about the origin
3. Moving the origin back

...is so common that it is built into `TAffineTransformation`. We just need to specify the center of rotation when we call the `Rotate` method and it will take care of doing it for us:

```pascal
uses
  GR32, GR32_Transforms, GR32_Resamplers;

var
  Src, Dst: TBitmap32;
  Transformer: TAffineTransformation;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  Transformer := TAffineTransformation.Create;
  try
    Src.LoadFromFile('logo.png');
    Dst.SetSize(600, 600);
    Dst.Clear(clWhite32); // Clear target canvas

    // Enable smooth resampling on source
    TKernelResampler.Create(Src).Kernel := TLanczosKernel.Create;

    // Build transformation chain
    Transformer.Clear;

    // 1. Rotate by 45 degrees clockwise about the center
    Transformer.Rotate(Src.Width * 0.5, Src.Height * 0.5, 45);

    // 2. Move rotated result to center of destination canvas (300, 300)
    Transformer.Translate(300, 300);

    // Apply transformation blit from Src into Dst
    Transform(Dst, Src, Transformer);

    Dst.SaveToFile('rotated_logo.png');
  finally
    Transformer.Free;
    Dst.Free;
    Src.Free;
  end;
end;
```

In the above, the size of the result bitmap is hardcoded and the position of the rotated image within it isn't quite perfect. We'll take care of that in just a moment. Hang on.

---

## 3. Combining Scaling, Skewing, and Rotation

Affine operations are cumulative; Order matters! Performing rotation before translation yields a completely different result than translation before rotation.

```pascal
var
  Transformer: TAffineTransformation;
begin
  Transformer := TAffineTransformation.Create;
  try
    Transformer.Clear;

    // Step 1: Scale to 150% horizontally, 80% vertically
    Transformer.Scale(1.5, 0.8);

    // Step 2: Skew horizontally by 15 degrees
    Transformer.Skew(15, 0);

    // Step 3: Rotate by 30 degrees
    Transformer.Rotate(30);

    // Step 4: Translate to canvas position (200, 150)
    Transformer.Translate(200, 150);

    // Perform transform
    Transform(DstBitmap, SrcBitmap, Transformer);
  finally
    Transformer.Free;
  end;
end;
```

---

## 4. Automatic Transformed Bounds Calculation

When rotating or skewing an image, its bounding box expands. `TAffineTransformation` can automatically calculate the required destination canvas dimensions using `GetTransformedBounds`.

```pascal
var
  Src, Dst: TBitmap32;
  Transformer: TAffineTransformation;
  DstRect: TFloatRect;
begin
  Src := TBitmap32.Create;
  Dst := TBitmap32.Create;
  Transformer := TAffineTransformation.Create;
  try
    Src.LoadFromFile('photo.jpg');

    // Setup rotation around source center
    Transformer.Clear;
    Transformer.Rotate(Src.Width * 0.5, Src.Height * 0.5, 33); // 33 degree tilt

    // Calculate exact bounding rectangle required for transformed image
    DstRect := Transformer.GetTransformedBounds(Src.BoundsRect);

    // Translate matrix so the transformation result is centered in the destination bitmap
    Transformer.Translate((DstRect.Width - Src.Width) * 0.5, (DstRect.Height - Src.Height) * 0.5);

    // Resize destination bitmap to fit transformed dimensions perfectly
    Dst.SetSize(Ceil(DstRect.Width), Ceil(DstRect.Height));

    // Transform without cropping edge pixels
    Transform(Dst, Src, Transformer);

    Dst.SaveToFile('fitted_rotation.png');
  finally
    Transformer.Free;
    Dst.Free;
    Src.Free;
  end;
end;
```

---

## Summary

In this tutorial, you learned:
1. How `TAffineTransformation` manages 2D matrix transformations.
2. The importance of matrix operation order (Translate -> Rotate/Scale -> Translate).
3. How to rotate bitmaps around custom center points.
4. How to use `GetTransformedBounds` to resize target canvases so transformed images are never cropped.

Next, proceed to [Vector Polygons & Path Drawing](./vector-polygons) to explore scalable vector graphics in Graphics32!
