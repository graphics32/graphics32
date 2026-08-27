# Overview

::: info
This section describes the behavior and properties of the [[TCustomImage32]] base class and its derived class, the [[TImage32]] control, as well as their realization of layers.

Since `TCustomImage32` and `TImage32` share the same behavior, we will denote both of them here as `TImage32`. Most of the description applies to the `TImgView32` control as well.
:::

[[TImage32]] is an image displaying visual control, which also holds a collection of layers.

The internal image of the control is a `TBitmap32` object. The scale and position of the image within the control’s boundaries can be controlled with several properties. See [Bitmap Image](bitmap-image) for details.

 `TImage32` maintains a collection of layers. A layer is, generally speaking, an entity which ‘knows’ how to combine itself with the back buffer of the control. See ‘[Using Layers](using-layers)‘ for details.

When the image paints itself, it runs through several [Paint Stages](paint-stages) which determine the order in which layers and the bitmap image are painted. This order is completely customizeable.

To avoid flicker and to speed-up scaling and blending operations, each layer and the bitmap image is painted to a back-buffer of the control. See the description of [Repaint Optimization](repaint-optimization) for details on this.

Finally, `TImage32` supports change notification via the [[TCustomImage32.OnChange|OnChange]] event, which is done similar to change notification in `TBitmap32`. Basically, it redirects change notification from its layers and the bitmap image.
