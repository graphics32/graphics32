---
title: "Loading, Saving & Image Formats"
---

# Loading, Saving & Image Formats

Graphics32 supports loading and saving 32-bit images across common graphics formats including PNG, JPEG, BMP, and Photoshop PSD files.

In this tutorial, you will learn how to load images from disk files and in-memory streams, preserve alpha transparency, and exchange bitmaps with standard VCL / Lazarus components (`TBitmap`, `TPicture`, `TImage`).

---

## 1. File I/O with LoadFromFile and SaveToFile

`TBitmap32` includes `LoadFromFile` and `SaveToFile` methods. Graphics32 automatically registers image format adapters based on file extensions.

```pascal
uses
  Classes, SysUtils, GR32, GR32_PNG; // Include GR32_PNG for native PNG support

procedure LoadAndSaveExample;
var
  Bitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    // 1. Load 32-bit PNG image with alpha channel
    Bitmap.LoadFromFile('input_transparent.png');

    // Inspect loaded dimensions
    Writeln(Format('Loaded image dimensions: %dx%d', [Bitmap.Width, Bitmap.Height]));

    // Perform drawing modifications...
    Bitmap.FillRectTS(10, 10, 100, 100, clTrRed32);

    // 2. Save result as PNG preserving full alpha transparency
    Bitmap.SaveToFile('output_modified.png');

    // 3. Save result as standard BMP
    Bitmap.SaveToFile('output_backup.bmp');
  finally
    Bitmap.Free;
  end;
end;
```

---

## 2. Working with In-Memory Streams

When dealing with database BLOBs, network downloads, or embedded binary assets, you can load and save directly from `TStream` instances using `LoadFromStream` and `SaveToStream`.

```pascal
procedure LoadBitmapFromStream(Stream: TStream; TargetBitmap: TBitmap32);
begin
  Stream.Position := 0;

  // Load stream content into TBitmap32
  TargetBitmap.LoadFromStream(Stream);
end;

procedure SaveBitmapToStream(SourceBitmap: TBitmap32; Stream: TStream);
begin
  // Save as PNG stream
  SourceBitmap.SaveToStream(Stream);
end;
```

---

## 3. Converting Between Standard VCL/FCL TBitmap and TBitmap32

In Delphi and Lazarus applications, you frequently need to exchange image data between standard VCL/FCL controls (`TImage`, `TPicture`, `Graphics.TBitmap`) and `TBitmap32`.

### Converting VCL TBitmap -> TBitmap32

```pascal
uses
  Graphics, GR32;

procedure ConvertVclToGR32(VclBitmap: Graphics.TBitmap; GR32Bitmap: TBitmap32);
begin
  // Assign handles color conversion and pixel copy automatically
  GR32Bitmap.Assign(VclBitmap);

  // Ensure alpha channel is set to opaque ($FF) if VCL bitmap lacked alpha
***TODO***
  GR32Bitmap.ResetAlpha;
end;
```

### Converting TBitmap32 -> VCL TBitmap

```pascal
procedure ConvertGR32ToVcl(GR32Bitmap: TBitmap32; VclBitmap: Graphics.TBitmap);
begin
  // Assign copies pixels from TBitmap32 into standard VCL TBitmap
  VclBitmap.Assign(GR32Bitmap);
end;
```

---

## 4. Preserving Alpha Channel Transparency on Load

When loading 24-bit bitmaps or JPEGs that do not contain alpha channel information, `TBitmap32` initializes the alpha channel to $00 (transparent) or leaves it uninitialized depending on options.
***TODO***
To guarantee that non-transparent images render as fully opaque solid colors, call `ResetAlpha` or `SetAlpha`:

```pascal
var
  Bitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    Bitmap.LoadFromFile('photo.jpg');

    // Sets all pixel alpha channels to $FF (255 = 100% opaque)
    Bitmap.ResetAlpha;

    // Or set a global uniform alpha value (e.g. 200) across all pixels
    // Bitmap.SetAlpha(200);

  finally
    Bitmap.Free;
  end;
end;
```

---

## Summary

In this tutorial, you learned:
1. How to load and save PNG, JPEG, and BMP files using `LoadFromFile` and `SaveToFile`.
2. How to process image data in memory streams using `LoadFromStream` and `SaveToStream`.
3. How to convert pixels between VCL/FCL `TBitmap` and Graphics32 `TBitmap32` using `Assign`.
4. How to use `ResetAlpha` to ensure loaded photos remain fully opaque.

Next, proceed to [Repaint Optimization & Custom Canvas Drawing](./custom-painting-repaint) to learn how to paint efficiently without flickering!
