---
layout: doc
docType: api
unit: GR32
entity: TCustomBitmap32
kind: Class
abstract: true
declaration: |
  type
    TCustomBitmap32 = class(TCustomMap)
      ...
    TCustomBitmap32Class = class of TCustomBitmap32;
aliases: [TCustomBitmap32Class]
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TCustomBitmap32
summary: "Abstract base class for 32-bit ARGB pixel bitmaps in Graphics32, defining backend surface management, sub-pixel sampling, drawing primitives, and resampling."
---

## Description

`TCustomBitmap32` is the primary 2D image data container in Graphics32. It extends `TCustomMap` with 32-bit ARGB pixel buffer management, hardware-abstracted rendering backends (`TCustomBackend`), spatial clipping (`ClipRect`), drawing modes (`DrawMode`, `CombineMode`), alpha blending (`MasterAlpha`), and customizable pixel resamplers (`TCustomResampler`).

Pixel elements are represented as 32-bit ARGB colors (`TColor32`). `TCustomBitmap32` provides extensive accessor methods for reading and writing pixels at integer, fixed-point (`TFixed`), or single-precision floating-point (`TFloat`) sub-pixel coordinates.

Derived Class: [[TBitmap32]]

## Constructors

| Name | Description |
| --- | --- |
| [[Create]] | Initializes a new `TCustomBitmap32` instance with default or custom backend and dimensions. |

## Methods

### General & Lifecycle
| Name | Description |
| --- | --- |
| [[Assign]] | Copies pixel buffer and properties from another persistent object or bitmap. |
| [[BeginMeasuring]] | Begins area change measurement and bounding box calculation. |
| [[BoundsRect]] | Returns a `TRect` representing the bitmap dimensions `(0, 0, Width, Height)`. |
| [[Changed]] | Triggers update notifications for the entire bitmap or a specified sub-rectangle. |
| [[Clear]] | Clears the bitmap pixel buffer to zero (`clNone32`) or a specified `TColor32` fill color. |
| [[CopyMapTo]] | Copies raw map dimensions and pixel data to a target `TCustomBitmap32`. |
| [[Delete]] | Resets bitmap dimensions to zero and releases buffer memory. |
| [[Empty]] | Returns `True` if the bitmap has zero width or height. |
| [[EndMeasuring]] | Concludes area change measurement. |
| [[GetPlatformBackendClass]] | Class method returning the default platform backend class. |
| [[PropertyChanged]] | Notifies the bitmap and backend that a property has changed. |
| [[ReleaseBackend]] | Detaches and returns the active backend instance without destroying it. |
| [[ResetClipRect]] | Resets `ClipRect` to cover the full bitmap bounds `(0, 0, Width, Height)`. |

### Stream & File I/O
| Name | Description |
| --- | --- |
| [[LoadFromFile]] | Loads bitmap data from a file on disk. |
| [[LoadFromResourceID]] | Loads bitmap data from an embedded resource by integer ID. |
| [[LoadFromResourceName]] | Loads bitmap data from an embedded resource by string name. |
| [[LoadFromStream]] | Loads bitmap data from a stream. |
| [[SaveToFile]] | Saves bitmap data to a file on disk in BMP/DIB format. |
| [[SaveToStream]] | Saves bitmap data to a stream in BMP/DIB format. |

### Blitting & Alpha Operations
| Name | Description |
| --- | --- |
| [[Draw]] | Draws a source bitmap or sub-rectangle onto this bitmap. |
| [[DrawTo]] | Draws this bitmap or a sub-rectangle onto a target destination bitmap. |
| [[ResetAlpha]] | Resets alpha values of all pixels to `$FF` (opaque) or a specified alpha component. |
| [[SetPixelT]] | Sets a pixel with alpha blending (`DrawMode` / `CombineMode`). |
| [[SetPixelTS]] | Sets a pixel with alpha blending and boundary clipping. |

### Lines & Drawing Primitives
| Name | Description |
| --- | --- |
| [[HorzLine]] | Draws an unclipped horizontal line at integer coordinates. |
| [[HorzLineS]] | Draws a clipped horizontal line at integer coordinates. |
| [[HorzLineT]] | Draws an unclipped blended horizontal line at integer coordinates. |
| [[HorzLineTS]] | Draws a clipped blended horizontal line at integer coordinates. |
| [[HorzLineTSP]] | Draws a clipped blended horizontal line using current stipple pattern. |
| [[HorzLineX]] | Draws an unclipped horizontal line at fixed-point coordinates. |
| [[HorzLineXS]] | Draws a clipped horizontal line at fixed-point coordinates. |
| [[VertLine]] | Draws an unclipped vertical line at integer coordinates. |
| [[VertLineS]] | Draws a clipped vertical line at integer coordinates. |
| [[VertLineT]] | Draws an unclipped blended vertical line at integer coordinates. |
| [[VertLineTS]] | Draws a clipped blended vertical line at integer coordinates. |
| [[VertLineTSP]] | Draws a clipped blended vertical line using current stipple pattern. |
| [[VertLineX]] | Draws an unclipped vertical line at fixed-point coordinates. |
| [[VertLineXS]] | Draws a clipped vertical line at fixed-point coordinates. |
| [[Line]] | Draws 1-pixel wide line segments between specified coordinates using integer, fixed-point, or floating-point positioning with optional clipping, blending, anti-aliasing, and stippling. |
| [[MoveTo]] | Sets current pen position for subsequent `LineTo` drawing operations using integer, fixed-point, or floating-point coordinates. |
| [[LineTo]] | Draws 1-pixel wide line segments from current pen position to target coordinates using integer, fixed-point, or floating-point positioning, updating pen position afterwards. |

### Rectangles & Shapes
| Name | Description |
| --- | --- |
| [[FillRect]] | Fills an unclipped rectangular region with a specified color. |
| [[FillRectS]] | Fills a clipped rectangular region with a specified color. |
| [[FillRectT]] | Fills an unclipped rectangular region using alpha blending. |
| [[FillRectTS]] | Fills a clipped rectangular region using alpha blending. |
| [[FrameRectS]] | Draws a 1-pixel clipped rectangular frame. |
| [[FrameRectTS]] | Draws a 1-pixel clipped alpha-blended rectangular frame. |
| [[FrameRectTSP]] | Draws a 1-pixel clipped stippled rectangular frame. |
| [[RaiseRectTS]] | Draws a 3D bevelled button/panel edge. |

### Transformations
| Name | Description |
| --- | --- |
| [[Roll]] | Scrolls/shifts pixel contents horizontally and vertically with optional background fill. |
| [[FlipHorz]] | Flips pixel contents horizontally (left to right). |
| [[FlipVert]] | Flips pixel contents vertically (top to bottom). |
| [[Rotate90]] | Rotates bitmap contents 90 degrees clockwise. |
| [[Rotate180]] | Rotates bitmap contents 180 degrees. |
| [[Rotate270]] | Rotates bitmap contents 270 degrees clockwise (90 degrees counter-clockwise). |

### Stipple Methods
| Name | Description |
| --- | --- |
| [[SetStipple]] | Sets custom stipple color array pattern. |
| [[AdvanceStippleCounter]] | Advances stipple counter position by specified distance in pixels. |
| [[GetStippleColor]] | Evaluates and returns current stipple color. |

## Properties

### Pixel Accessors
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [[Pixel]] | `TColor32` | Public | Indexed pixel access properties for reading and writing 32-bit ARGB pixel values across integer, fixed-point, and floating-point coordinates. |

### Pen & Stipple Properties
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [[PenColor]] | `TColor32` | Public | Color used for pen drawing operations (`MoveTo`, `LineTo`). |
| [[PenPos]] | `TPoint` | Public | Current integer pen coordinate position. |
| [[PenPosF]] | `TFixedPoint` | Public | Current fixed-point pen coordinate position. |
| [[StippleCounter]] | `Single` | Public | Current phase position index along stipple pattern array. |
| [[StippleStep]] | `Single` | Public | Step distance increment for stipple pattern evaluation. |

### Buffer & Backend Properties
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [[Backend]] | `TCustomBackend` | Public | Active rendering surface backend instance. |
| [[Bits]] | `PColor32Array` | Public | Pointer to contiguous raw 32-bit ARGB pixel memory buffer. |
| [[ClipRect]] | `TRect` | Public | Clipping rectangle restricting pixel modifications. |
| [[Clipping]] | `Boolean` | Public | Read-only flag indicating if `ClipRect` active region is smaller than full bitmap dimensions. |
| [[MeasuringMode]] | `Boolean` | Public | Indicates whether area change measurement is active. |
| [[PixelPtr]] | `PColor32` | Public | Pointer to specific pixel at integer coordinates `[X, Y]`. |
| [[ScanLine]] | `PColor32Array` | Public | Pointer to starting pixel memory location for scanline row `[Y]`. |

### Drawing Modes & Resampling
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [[CombineMode]] | `TCombineMode` | Published | Pixel combination mode (`cmBlend`, `cmMerge`, `cmModulo`, etc.). |
| [[DrawMode]] | `TDrawMode` | Published | Draw mode (`dmOpaque`, `dmBlend`, `dmCustom`). |
| [[MasterAlpha]] | `Cardinal` | Published | Global alpha scaling factor (`0` to `255`) applied to draw operations. |
| [[OuterColor]] | `TColor32` | Published | Color returned when sampling outside bitmap bounds in safe/transparent mode. |
| [[Resampler]] | `TCustomResampler` | Published | Active resampler engine instance for sub-pixel stretching and sampling. |
| [[ResamplerClassName]] | `string` | Published | Class name string of current resampler engine. |
| [[WrapMode]] | `TWrapMode` | Published | Boundary wrapping mode (`wmClamp`, `wmRepeat`, `wmMirror`). |

## Events

| Name | Type | Description |
| --- | --- | --- |
| [[OnAreaChanged]] | `TAreaChangedEvent` | Fired when a specific sub-rectangle region of pixel memory is updated. |
| [[OnPixelCombine]] | `TPixelCombineEvent` | Custom pixel combination callback event when `DrawMode = dmCustom`. |
