# Bitmap image

Internally a `TImage32` control represents the bitmap image with a [[TBitmap32]] object. This bitmap is surfaced by the [[TCustomImage32.Bitmap|Bitmap]] property. The scale and location of the bitmap within the control is determined by the following properties:

| Property | Description |
| --- | --- |
| [[TCustomImage32.BitmapAlign\|BitmapAlign]] | Specifies if the bitmap image is positioned at the top-left corner of the control (`baTopLeft`), centered (`baCenter`), tiled (`baTile`) or it its exact location is determined by [[TCustomImage32.OffsetHorz\|OffsetHorz]] and [[TCustomImage32.OffsetVert\|OffsetVert]] properties. |
| [[TCustomImage32.ScaleMode\|ScaleMode]] | Indicates if the bitmap image is displayed with its original size (`smNormal`), stretched to fit the control’s boundaries (`smStretch`), proportionally resized to fit the control’s boundaries(`smResize`) or proportionally scaled using its [[TCustomImage32.Scale\|Scale]] property (`smScale`). |

![](../images/img_015.gif)

The bitmap image is combined with the back-buffer according to its [[TCustomBitmap32.DrawMode|DrawMode]] property. If its `DrawMode` is `dmCustom`, the bitmap will fire a series of [[TCustomBitmap32.OnPixelCombine|OnPixelCombine]] events.
