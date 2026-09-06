---
layout: doc
docType: api
unit: GR32.ImageFormats.GIF
entity: GR32.ImageFormats.GIF
kind: Unit
summary: "Provides image format adapter and writer support for GIF files via the TGIFImage class."
---

## Description

The `GR32.ImageFormats.GIF` unit implements image format adapters and readers for Graphics Interchange Format (`.gif`) files.

::: tip True Color GIF
The GIF image format only supports up to 256 simultaneous colors.

When converting to GIF from a 32-bit bitmap, which can potentially contain thousands of colors, Graphics32 will automatically produce a so-called *True Color GIF*.<br>
A True Color GIF is an animated GIF where each frame contains a maximum of 255 colors (plus transparent) which, when layered on top of each other, produce an image with more than 256 colors.
| PNG | GIF |
| --- | --- |
| ![PNG](/images/orb-water.png) | ![GIF](/images/orb-water.gif) |
| 1207 colors | 5 frames of 256 colors |
| 3917 bytes | 6864 bytes |

Be aware that such True Color GIFs can become *very* large and that not all applications are able to reconstruct the True color image.

**Note:** This feature is only supported when compiling with Delphi/VCL.

See: [GIF, True color - Wikipedia](https://en.wikipedia.org/wiki/GIF#True_color)
:::
---

[members]
