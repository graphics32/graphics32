---
title: "Alpha Blending & Transparency"
---

# Alpha Blending & Transparency

One of the defining features of Graphics32 is native 32-bit alpha blending. In traditional 24-bit graphics, pixels only specify Red, Green, and Blue. In Graphics32, every pixel contains an **Alpha** channel ($00..$FF) that determines transparency and opacity.

This tutorial covers the `TColor32` structure, transparency constants, drawing and combine modes, and global alpha composition using `MasterAlpha`.

---

## 1. The TColor32 Color Structure

In Graphics32, colors are stored as 32-bit unsigned integers (`Cardinal`) formatted as **ARGB** (Alpha, Red, Green, Blue)[^1]:

[^1]: The actual color channel layout is platform dependent.  
On Windows it's ARGB, but on Android and Linux it's ABGR.

| Alpha | Red | Green | Blue |
| --- | --- | --- | --- |
| 8 bits | 8 bits | 8 bits | 8 bits |

- Alpha = `$FF` (255): Fully opaque pixel.
- Alpha = `$80` (128): 50% semi-transparent pixel.
- Alpha = `$00` (0): Fully transparent pixel.

### Creating and Unpacking TColor32

Graphics32 provides many helper functions to construct and decompose colors:

```pascal
uses
  GR32;

var
  OpaqueRed, SemiBlue, CustomColor: TColor32;
  R, G, B, A: Byte;
begin
  // 1. Opaque Red (Alpha = 255)
  OpaqueRed := clRed32; // Defined as $FFFF0000

  // 2. Translucent Blue (Alpha = 128, Red = 0, Green = 0, Blue = 255)
  SemiBlue := clTrBlue32; // Predefined 50% translucent blue

  // 3. Construct custom color using Color32(R, G, B, Alpha)
  CustomColor := Color32(255, 128, 0, 192); // Orange with 75% opacity

  // 4. Extract individual ARGB channel values with Color32ToRGBA
  Color32ToRGBA(CustomColor, R, G, B, A);
  // R = 255, G = 128, B = 0, A = 192

  // 5. Extract individual ARGB channel values with Component functions
  R := RedComponent(CustomColor);
  G := GreenComponent(CustomColor);
  B := BlueComponent(CustomColor);
  A := AlphaComponent(CustomColor);

  // 6. Extract individual ARGB channel values with TColor32Entry type cast
  R := TColor32Entry(CustomColor).R;
  G := TColor32Entry(CustomColor).G;
  B := TColor32Entry(CustomColor).B;
  A := TColor32Entry(CustomColor).A;
end;
```

---

## 2. Standard Alpha Blending with FillRectTS

To draw semi-transparent shapes over existing content, use routines with the `TS` (Transparent + Safe Clipping) suffix or configure `DrawMode`.

```pascal
var
  Bitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    Bitmap.SetSize(400, 300, False);

    // Fill background with solid yellow
    Bitmap.Clear(Color32(255, 230, 150, 255));

    // Draw overlapping semi-transparent colored squares
    // 50% opacity Red square
    Bitmap.FillRectTS(50, 50, 200, 200, Color32(255, 0, 0, 128));

    // 50% opacity Green square overlapping the red square
    Bitmap.FillRectTS(120, 100, 270, 250, Color32(0, 200, 0, 128));

    // 75% opacity Blue square
    Bitmap.FillRectTS(180, 40, 330, 190, Color32(0, 100, 255, 192));

    Bitmap.SaveToFile('alpha_blending_demo.png');
  finally
    Bitmap.Free;
  end;
end;
```

When drawing with `FillRectTS`, Graphics32 automatically blends the incoming source color ($C_{\text{src}}, A_{\text{src}}$) with the existing destination color ($C_{\text{dst}}$) according to standard alpha compositing:

$$C_{\text{out}} = \frac{C_{\text{src}} \times A_{\text{src}} + C_{\text{dst}} \times (255 - A_{\text{src}})}{255}$$

---

## 3. DrawMode and Custom Blending

When blitting (copying) one bitmap onto another using `Draw` or `BlockTransfer`, the blending behavior is controlled by `DrawMode`.

```pascal
procedure TForm1.MyPixelCombine(F: TColor32; var B: TColor32; M: Cardinal);
begin
  // Custom blend operation: B = F xor B (preserves alpha)
  F := (F and $FF000000) or ((F and $00FFFFFF) xor (B and $00FFFFFF));
  BlendMem(F, B);
end;

var
  Background, Foreground: TBitmap32;
begin
  Background := TBitmap32.Create;
  Foreground := TBitmap32.Create;
  try
    Background.SetSize(500, 400, False);
    Background.Clear(clWhite32);

    Foreground.SetSize(200, 200, False);
    Foreground.Clear(Color32(255, 0, 128, 180)); // Translucent magenta

    // --- Mode 1: dmOpaque ---
    // Ignores alpha channel and completely overwrites target pixels
    Foreground.DrawMode := dmOpaque;
    Background.Draw(50, 50, Foreground);

    // --- Mode 2: dmBlend ---
    // Standard alpha blending based on foreground per-pixel alpha
    Foreground.DrawMode := dmBlend;
    Background.Draw(250, 50, Foreground);

    // --- Mode 3: dmCustom ---
    // Custom pixel blending logic (e.g. xor inversion)
    Foreground.DrawMode := dmCustom;
    Foreground.OnPixelCombine := MyPixelCombine;
    Background.Draw(150, 180, Foreground);

  finally
    Foreground.Free;
    Background.Free;
  end;
end;
```

---

## 4. MasterAlpha Global Opacity Control

`MasterAlpha` allows you to adjust the overall opacity of an entire bitmap during drawing without modifying the alpha channel of individual pixels.

```pascal
var
  Bg, Sprite: TBitmap32;
begin
  Bg := TBitmap32.Create;
  Sprite := TBitmap32.Create;
  try
    Bg.SetSize(600, 300);
    Bg.Clear(clLightGray32);

    // Prepare foreground sprite
    Sprite.SetSize(100, 100);
    Sprite.Clear(clBlue32); // Fully opaque blue

    Sprite.DrawMode := dmBlend;

    // Draw full opacity (MasterAlpha = 255)
    Sprite.MasterAlpha := 255;
    Bg.Draw(50, 100, Sprite);

    // Draw 50% opacity (MasterAlpha = 128)
    Sprite.MasterAlpha := 128;
    Bg.Draw(200, 100, Sprite);

    // Draw 20% opacity (MasterAlpha = 51)
    Sprite.MasterAlpha := 51;
    Bg.Draw(350, 100, Sprite);

  finally
    Sprite.Free;
    Bg.Free;
  end;
end;
```

---

## Summary

In this tutorial, you learned:
1. How `TColor32` packages ARGB channels into a single 32-bit integer.
2. How to use `Color32`, `Color32ToRGBA`, and `TColor32Entry` to construct and inspect colors.
3. How `FillRectTS` and `dmBlend` perform standard alpha composition.
4. How to use `dmCustom` for special blending effects.
5. How `MasterAlpha` controls global fade-in and transparency effects.

Next, head to [Fast Pixel Access & Direct Manipulation](./pixel-manipulation) to explore how to read and write bitmap pixels directly in high-performance loops!
