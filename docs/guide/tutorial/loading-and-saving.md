---
title: "9. Loading, Saving & Image Formats"
---

# Loading, Saving & Image Formats

Graphics32 supports loading and saving 32-bit images across common graphics formats including PNG, JPEG, BMP, and Photoshop PSD files.

In this tutorial, you will learn how to load images from disk files and in-memory streams, preserve alpha transparency, and exchange bitmaps with standard VCL / Lazarus components (`TBitmap`, `TPicture`, `TImage`).

---

## 1. File I/O with LoadFromFile and SaveToFile

`TBitmap32` includes `LoadFromFile` and `SaveToFile` methods. Graphics32 automatically registers image format adapters based on file extensions.

```pascal
uses
  Classes, SysUtils, 
  GR32, 
  GR32.ImageFormats.PNG32; // Include GR32.ImageFormats.PNG32 for native PNG support

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
  // Save as BMP stream
  SourceBitmap.SaveToStream(Stream);
end;
```

::: info
When loading from a file Graphics32 will use both the file type and the content of the file to determine the image format. When loading from a stream it will of course only use the content.

When saving to a file it will use the file type to determine the target image format, and if that fails, it will fall back to BMP. When saving to a stream it will always save as BMP.
:::

---

## 3. Converting Between Standard VCL/FCL TBitmap and TBitmap32

In Delphi and Lazarus applications, you frequently need to exchange image data between standard VCL/FCL controls (`TImage`, `TPicture`, `Graphics.TBitmap`) and `TBitmap32`.

`TBitmap32` supports the standard `Assign`/`AssignTo` pattern for easy conversion to and from all `TGraphic`-based image formats.

### Converting VCL TBitmap -> TBitmap32

```pascal
uses
  Graphics, GR32;

procedure ConvertVclToGR32(SourceBitmap: Graphics.TBitmap; DestBitmap: TBitmap32);
begin
  // Assign handles color conversion and pixel copy automatically
  //
  // Graphics32 takes care of setting the alpha channel to 
  // opaque ($FF) if the source bitmap lacked alpha.
  DestBitmap.Assign(SourceBitmap);
end;
```

### Converting TBitmap32 -> VCL TBitmap

```pascal
procedure ConvertGR32ToVcl(SourceBitmap: TBitmap32; DestBitmap: Graphics.TBitmap);
begin
  // Assign copies pixels from TBitmap32 into standard VCL TBitmap
  DestBitmap.Assign(SourceBitmap);
end;
```

::: info
The `Assign`/`AssignTo` pattern simply means that both the source and the destination get a chance at performing the conversion; First the destination (`Dest.Assign(Source)`) and then the source (`Source.AssignTo(Dest)`).

As a result it is possible to perform a conversion if just one of the participants knows how to do it.

In the case of a conversion *to* `TBitmap32` where neither parts can handle the conversion, Graphics32 falls back to simply drawing the source on the destination bitmap.
:::

---

## Summary

::: box-green

In this tutorial, you learned:
1. How to load and save PNG, JPEG, and BMP files using `LoadFromFile` and `SaveToFile`.
2. How to process image data in memory streams using `LoadFromStream` and `SaveToStream`.
3. How to convert pixels between VCL/FCL `TBitmap` and Graphics32 `TBitmap32` using `Assign`.

:::

Next, proceed to [Repaint Optimization & Custom Canvas Drawing](./custom-painting-repaint) to learn how to paint efficiently without flickering!
