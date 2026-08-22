# Using timage32

This section describes behavior and properties of [TCustomImage32](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/_Body.htm) and [TImage32](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TImage32/_Body.htm) controls, as well as their realization of layers.

Since [TCustomImage32](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/_Body.htm) and [TImage32](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TImage32/_Body.htm) share the same behavior, I will denote both of them here as TImage32. Most of the description applies to the TImgView32 control as well.

## Overview

TImage32 is an image displaying visual component, which also holds a collection of layers.

The internal image of the control is a TBitmap32 object. The scale and position of the image within the control’s boundaries can be controlled with several properties. See [Bitmap Image](https://melander.dk/graphics32/using-timage32/bitmap-image/) for details.

A layers is, generally speaking, an entities which ‘knows’ how to combine itself with the back buffer of the control. TImage32 maintains a collection of layers. See ‘[Using Layers](https://graphics32.github.io/Docs/Additional%20Topics/Using%20TImage32/Using%20Layers.htm)‘ for details.

When the image paints itself, it runs through several [Paint Stages](https://graphics32.github.io/Docs/Additional%20Topics/Using%20TImage32/Paint%20Stages.htm) which determine the order in which layers and the bitmap image are painted. This order is completely customizeable.

To avoid flicker and to speed-up scaling and blending operations, each layer and the bitmap image is painted to a back-buffer of the control. See the reference of TImage32 ancestor [TCustomPaintBox32](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomPaintBox32/_Body.htm) for details of the back-buffer realization.

Finally, TImage32 supports change notification via the [OnChange](https://graphics32.github.io/Docs/Units/GR32_Image/Classes/TCustomImage32/Events/OnChange.htm) event, which is done similar to change notification in [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm). Basically, it redirects change notification from its layers and the bitmap image.
