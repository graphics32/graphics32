# Naming conventions

The [TBitmap32](https://graphics32.github.io/Docs/Units/GR32/Classes/TBitmap32/_Body.htm) class provides a large number of methods for single pixel access and single pixel width line drawing. While the prefix of these methods indicate _what_ they do (draw a line, access a pixel, etc.), the postfix denotes _how_ they behave. The methods can include one or more of the following postfixes:

Postfix| Name| Description| Sample (magnified x4)| Examples
---|---|---|---|---
(none)| | Methods without modifiers offer maximum performance.
Lines and pixels are rendered without anti-aliasing and without color blending.
No clipping is performed so coordinates are expected to be within bitmap boundaries. Out of bounds coordinates can cause access violations and memory overwrites.| ![](/images/Line.png)| Line
HorzLine
Pixel
FillRect
S| Safe| Clipping is performed when coordinates extend beyond bitmap boundaries.
This modifier can be combined with any **one** of the anti-aliasing or transparent modifiers below (T, A, X or F) and also with the stippling (P) modifier.| ![](/images/Line.png)| LineS
LineToS
PixelS
T| Transparent| Semi-transparent pixels are merged onto the background image (though still without anti-aliasing).| ![](/images/LineT.png)| LineTS
LineToTS
FillRectTS
A| Anti-aliased| Uses a modified version of [Bresenham’s algorithm](https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm) known as [Wu’s antialiasing](https://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm).| ![](/images/LineA.png)| LineA
LineAS
X| Fixed| Coordinates are in fixed point (`TFixed`) format.
These methods also employ a more sophisticated anti-aliasing algorithm than that used in the ‘A’ methods above.| ![](/images/LineF.png)| LineX
LineXS
PixelXS
F| Float| Coordinates are in floating point (`TFloat`) format.
These methods employ the same anti-aliasing algorithm used in the ‘X’ methods.| ![](/images/LineF.png)| LineFS
PixelFS
P| Stippling| Lines are drawn using a [stippling pattern](https://melander.dk/graphics32/line-patterns/).
This modifier must be combined with either the ‘X’ or ‘F’ modifiers above.| ![](/images/LineFP.png)| LineFSP
LineXSP
| | For lines, all the methods listed above draw line segments that are **a single pixel wide**.
To draw thicker lines, see the [Polyline](https://graphics32.github.io/Docs/Units/GR32_Polygons/Routines/Polyline.htm) and [PolyPolyline](https://graphics32.github.io/Docs/Units/GR32_Polygons/Routines/PolyPolyline.htm) functions in the [GR32_Polygons](https://graphics32.github.io/Docs/Units/GR32_Polygons/_Body.htm) unit. Those functions also produce better anti-aliased blending over fully transparent backgrounds (when compared with X & F modified methods above).| ![](/images/LineQ.png)|
