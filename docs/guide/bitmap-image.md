# Bitmap image

Internally a TImage32 represents the bitmap image with a [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) object. This bitmap is surfaced by the [Bitmap](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/Bitmap.htm) property. The scale and location of the bitmap within the control is determined by the following properties:

[BitmapAlign](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/BitmapAlign.htm)| Specifies if the bitmap image is positioned at the top-left corner of the control (baTopLeft), centered (baCenter), tiled (baTile) or it its exact location is determined by [OffsetHorz](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/OffsetHorz.htm) and [OffsetVert](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/OffsetVert.htm) properties.
---|---
[BitmapAlign](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/BitmapAlign.htm)| Indicates if the bitmap image is displayed with its original size (smNormal), stretched to fit the control’s boundaries (smStretch), proportionally resized to fit the control’s boundaries(smResize) or proportionally scaled using its [Scale](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Properties/Scale.htm) property (smScale).

![](/images/img_015.gif)

The bitmap image is combined with the back-buffer according to its [DrawMode](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/Properties/DrawMode.htm) property. And the quality of its resampling is determined by the [StretchFilter](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/Properties/StretchFilter.htm) property. If its [DrawMode](https://graphics32.github.io/Docs/Units/GR32/Classes/TCustomBitmap32/Properties/DrawMode.htm) is dmCustom, the bitmap will fire a series of [OnPixelCombine](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Events/OnPixelCombine.htm) events.
