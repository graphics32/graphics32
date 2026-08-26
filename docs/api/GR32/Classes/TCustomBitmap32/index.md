---
layout: doc
docType: api
unit: GR32
entity: TCustomBitmap32
kind: Class
declaration: "TCustomBitmap32 = class(TCustomMap)"
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
| [Create](Constructors/Create.md) | Initializes a new `TCustomBitmap32` instance with default or custom backend and dimensions. |

## Methods

### General & Lifecycle
| Name | Description |
| --- | --- |
| [Assign](Methods/Assign.md) | Copies pixel buffer and properties from another persistent object or bitmap. |
| [BeginMeasuring](Methods/BeginMeasuring.md) | Begins area change measurement and bounding box calculation. |
| [BoundsRect](Methods/BoundsRect.md) | Returns a `TRect` representing the bitmap dimensions `(0, 0, Width, Height)`. |
| [Changed](Methods/Changed.md) | Triggers update notifications for the entire bitmap or a specified sub-rectangle. |
| [Clear](Methods/Clear.md) | Clears the bitmap pixel buffer to zero (`clNone32`) or a specified `TColor32` fill color. |
| [CopyMapTo](Methods/CopyMapTo.md) | Copies raw map dimensions and pixel data to a target `TCustomBitmap32`. |
| [Delete](Methods/Delete.md) | Resets bitmap dimensions to zero and releases buffer memory. |
| [Empty](Methods/Empty.md) | Returns `True` if the bitmap has zero width or height. |
| [EndMeasuring](Methods/EndMeasuring.md) | Concludes area change measurement. |
| [GetPlatformBackendClass](Methods/GetPlatformBackendClass.md) | Class method returning the default platform backend class. |
| [PropertyChanged](Methods/PropertyChanged.md) | Notifies the bitmap and backend that a property has changed. |
| [ReleaseBackend](Methods/ReleaseBackend.md) | Detaches and returns the active backend instance without destroying it. |
| [ResetClipRect](Methods/ResetClipRect.md) | Resets `ClipRect` to cover the full bitmap bounds `(0, 0, Width, Height)`. |

### Stream & File I/O
| Name | Description |
| --- | --- |
| [LoadFromFile](Methods/LoadFromFile.md) | Loads bitmap data from a file on disk. |
| [LoadFromResourceID](Methods/LoadFromResourceID.md) | Loads bitmap data from an embedded resource by integer ID. |
| [LoadFromResourceName](Methods/LoadFromResourceName.md) | Loads bitmap data from an embedded resource by string name. |
| [LoadFromStream](Methods/LoadFromStream.md) | Loads bitmap data from a stream. |
| [SaveToFile](Methods/SaveToFile.md) | Saves bitmap data to a file on disk in BMP/DIB format. |
| [SaveToStream](Methods/SaveToStream.md) | Saves bitmap data to a stream in BMP/DIB format. |

### Blitting & Alpha Operations
| Name | Description |
| --- | --- |
| [Draw](Methods/Draw.md) | Draws a source bitmap or sub-rectangle onto this bitmap. |
| [DrawTo](Methods/DrawTo.md) | Draws this bitmap or a sub-rectangle onto a target destination bitmap. |
| [ResetAlpha](Methods/ResetAlpha.md) | Resets alpha values of all pixels to `$FF` (opaque) or a specified alpha component. |
| [SetPixelT](Methods/SetPixelT.md) | Sets a pixel with alpha blending (`DrawMode` / `CombineMode`). |
| [SetPixelTS](Methods/SetPixelTS.md) | Sets a pixel with alpha blending and boundary clipping. |

### Lines & Drawing Primitives
| Name | Description |
| --- | --- |
| [HorzLine](Methods/HorzLine.md) | Draws an unclipped horizontal line at integer coordinates. |
| [HorzLineS](Methods/HorzLineS.md) | Draws a clipped horizontal line at integer coordinates. |
| [HorzLineT](Methods/HorzLineT.md) | Draws an unclipped blended horizontal line at integer coordinates. |
| [HorzLineTS](Methods/HorzLineTS.md) | Draws a clipped blended horizontal line at integer coordinates. |
| [HorzLineTSP](Methods/HorzLineTSP.md) | Draws a clipped blended horizontal line using current stipple pattern. |
| [HorzLineX](Methods/HorzLineX.md) | Draws an unclipped horizontal line at fixed-point coordinates. |
| [HorzLineXS](Methods/HorzLineXS.md) | Draws a clipped horizontal line at fixed-point coordinates. |
| [VertLine](Methods/VertLine.md) | Draws an unclipped vertical line at integer coordinates. |
| [VertLineS](Methods/VertLineS.md) | Draws a clipped vertical line at integer coordinates. |
| [VertLineT](Methods/VertLineT.md) | Draws an unclipped blended vertical line at integer coordinates. |
| [VertLineTS](Methods/VertLineTS.md) | Draws a clipped blended vertical line at integer coordinates. |
| [VertLineTSP](Methods/VertLineTSP.md) | Draws a clipped blended vertical line using current stipple pattern. |
| [VertLineX](Methods/VertLineX.md) | Draws an unclipped vertical line at fixed-point coordinates. |
| [VertLineXS](Methods/VertLineXS.md) | Draws a clipped vertical line at fixed-point coordinates. |
| [Line](Methods/Line.md) | Draws an unclipped arbitrary line segment at integer coordinates. |
| [LineS](Methods/LineS.md) | Draws a clipped arbitrary line segment at integer coordinates. |
| [LineT](Methods/LineT.md) | Draws an unclipped blended line segment at integer coordinates. |
| [LineTS](Methods/LineTS.md) | Draws a clipped blended line segment at integer coordinates. |
| [LineA](Methods/LineA.md) | Draws an unclipped anti-aliased line segment at integer coordinates. |
| [LineAS](Methods/LineAS.md) | Draws a clipped anti-aliased line segment at integer coordinates. |
| [LineX](Methods/LineX.md) | Draws an unclipped line segment at fixed-point coordinates. |
| [LineF](Methods/LineF.md) | Draws an unclipped line segment at floating-point coordinates. |
| [LineXS](Methods/LineXS.md) | Draws a clipped line segment at fixed-point coordinates. |
| [LineFS](Methods/LineFS.md) | Draws a clipped line segment at floating-point coordinates. |
| [LineXP](Methods/LineXP.md) | Draws an unclipped line segment using stipple pattern at fixed-point coordinates. |
| [LineFP](Methods/LineFP.md) | Draws an unclipped line segment using stipple pattern at floating-point coordinates. |
| [LineXSP](Methods/LineXSP.md) | Draws a clipped line segment using stipple pattern at fixed-point coordinates. |
| [LineFSP](Methods/LineFSP.md) | Draws a clipped line segment using stipple pattern at floating-point coordinates. |
| [MoveTo](Methods/MoveTo.md) | Sets current pen position for subsequent `LineTo` drawing operations. |
| [MoveToX](Methods/MoveToX.md) | Sets current fixed-point pen position for subsequent `LineTo` drawing operations. |
| [MoveToF](Methods/MoveToF.md) | Sets current floating-point pen position for subsequent `LineTo` drawing operations. |
| [LineToS](Methods/LineToS.md) | Draws a clipped line from current pen position to target integer point. |
| [LineToTS](Methods/LineToTS.md) | Draws a clipped blended line from current pen position to target integer point. |
| [LineToAS](Methods/LineToAS.md) | Draws a clipped anti-aliased line from current pen position to target integer point. |
| [LineToXS](Methods/LineToXS.md) | Draws a clipped line from current fixed-point pen position to target point. |
| [LineToFS](Methods/LineToFS.md) | Draws a clipped line from current floating-point pen position to target point. |
| [LineToXSP](Methods/LineToXSP.md) | Draws a clipped stippled line from current fixed-point pen position to target point. |
| [LineToFSP](Methods/LineToFSP.md) | Draws a clipped stippled line from current floating-point pen position to target point. |

### Rectangles & Shapes
| Name | Description |
| --- | --- |
| [FillRect](Methods/FillRect.md) | Fills an unclipped rectangular region with a specified color. |
| [FillRectS](Methods/FillRectS.md) | Fills a clipped rectangular region with a specified color. |
| [FillRectT](Methods/FillRectT.md) | Fills an unclipped rectangular region using alpha blending. |
| [FillRectTS](Methods/FillRectTS.md) | Fills a clipped rectangular region using alpha blending. |
| [FrameRectS](Methods/FrameRectS.md) | Draws a 1-pixel clipped rectangular frame. |
| [FrameRectTS](Methods/FrameRectTS.md) | Draws a 1-pixel clipped alpha-blended rectangular frame. |
| [FrameRectTSP](Methods/FrameRectTSP.md) | Draws a 1-pixel clipped stippled rectangular frame. |
| [RaiseRectTS](Methods/RaiseRectTS.md) | Draws a 3D bevelled button/panel edge. |

### Transformations
| Name | Description |
| --- | --- |
| [Roll](Methods/Roll.md) | Scrolls/shifts pixel contents horizontally and vertically with optional background fill. |
| [FlipHorz](Methods/FlipHorz.md) | Flips pixel contents horizontally (left to right). |
| [FlipVert](Methods/FlipVert.md) | Flips pixel contents vertically (top to bottom). |
| [Rotate90](Methods/Rotate90.md) | Rotates bitmap contents 90 degrees clockwise. |
| [Rotate180](Methods/Rotate180.md) | Rotates bitmap contents 180 degrees. |
| [Rotate270](Methods/Rotate270.md) | Rotates bitmap contents 270 degrees clockwise (90 degrees counter-clockwise). |

### Stipple Methods
| Name | Description |
| --- | --- |
| [SetStipple](Methods/SetStipple.md) | Sets custom stipple color array pattern. |
| [AdvanceStippleCounter](Methods/AdvanceStippleCounter.md) | Advances stipple counter position by specified distance in pixels. |
| [GetStippleColor](Methods/GetStippleColor.md) | Evaluates and returns current stipple color. |

## Properties

### Pixel Accessors
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [Pixel](Properties/Pixel.md) | `TColor32` | Public | Default indexed accessor for reading and writing pixels at integer coordinates `[X, Y]`. |
| [PixelS](Properties/PixelS.md) | `TColor32` | Public | Boundary-clipped pixel accessor at integer coordinates `[X, Y]`. |
| [PixelW](Properties/PixelW.md) | `TColor32` | Public | Wrapped pixel accessor at integer coordinates `[X, Y]`. |
| [PixelX](Properties/PixelX.md) | `TColor32` | Public | Pixel accessor at fixed-point coordinates `[X, Y]`. |
| [PixelXS](Properties/PixelXS.md) | `TColor32` | Public | Boundary-clipped pixel accessor at fixed-point coordinates `[X, Y]`. |
| [PixelXW](Properties/PixelXW.md) | `TColor32` | Public | Wrapped pixel accessor at fixed-point coordinates `[X, Y]`. |
| [PixelF](Properties/PixelF.md) | `TColor32` | Public | Sub-pixel accessor at floating-point coordinates `[X, Y]`. |
| [PixelFS](Properties/PixelFS.md) | `TColor32` | Public | Boundary-clipped sub-pixel accessor at floating-point coordinates `[X, Y]`. |
| [PixelFW](Properties/PixelFW.md) | `TColor32` | Public | Wrapped sub-pixel accessor at floating-point coordinates `[X, Y]`. |
| [PixelFR](Properties/PixelFR.md) | `TColor32` | Public | Read-only resampled sub-pixel accessor at floating-point coordinates `[X, Y]`. |
| [PixelXR](Properties/PixelXR.md) | `TColor32` | Public | Read-only resampled sub-pixel accessor at fixed-point coordinates `[X, Y]`. |

### Pen & Stipple Properties
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [PenColor](Properties/PenColor.md) | `TColor32` | Public | Color used for pen drawing operations (`MoveTo`, `LineTo`). |
| [PenPos](Properties/PenPos.md) | `TPoint` | Public | Current integer pen coordinate position. |
| [PenPosF](Properties/PenPosF.md) | `TFixedPoint` | Public | Current fixed-point pen coordinate position. |
| [StippleCounter](Properties/StippleCounter.md) | `Single` | Public | Current phase position index along stipple pattern array. |
| [StippleStep](Properties/StippleStep.md) | `Single` | Public | Step distance increment for stipple pattern evaluation. |

### Buffer & Backend Properties
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [Backend](Properties/Backend.md) | `TCustomBackend` | Public | Active rendering surface backend instance. |
| [Bits](Properties/Bits.md) | `PColor32Array` | Public | Pointer to contiguous raw 32-bit ARGB pixel memory buffer. |
| [ClipRect](Properties/ClipRect.md) | `TRect` | Public | Clipping rectangle restricting pixel modifications. |
| [Clipping](Properties/Clipping.md) | `Boolean` | Public | Read-only flag indicating if `ClipRect` active region is smaller than full bitmap dimensions. |
| [MeasuringMode](Properties/MeasuringMode.md) | `Boolean` | Public | Indicates whether area change measurement is active. |
| [PixelPtr](Properties/PixelPtr.md) | `PColor32` | Public | Pointer to specific pixel at integer coordinates `[X, Y]`. |
| [ScanLine](Properties/ScanLine.md) | `PColor32Array` | Public | Pointer to starting pixel memory location for scanline row `[Y]`. |

### Drawing Modes & Resampling
| Name | Type | Scope | Description |
| --- | --- | --- | --- |
| [CombineMode](Properties/CombineMode.md) | `TCombineMode` | Published | Pixel combination mode (`cmBlend`, `cmMerge`, `cmModulo`, etc.). |
| [DrawMode](Properties/DrawMode.md) | `TDrawMode` | Published | Draw mode (`dmOpaque`, `dmBlend`, `dmCustom`). |
| [MasterAlpha](Properties/MasterAlpha.md) | `Cardinal` | Published | Global alpha scaling factor (`0` to `255`) applied to draw operations. |
| [OuterColor](Properties/OuterColor.md) | `TColor32` | Published | Color returned when sampling outside bitmap bounds in safe/transparent mode. |
| [Resampler](Properties/Resampler.md) | `TCustomResampler` | Published | Active resampler engine instance for sub-pixel stretching and sampling. |
| [ResamplerClassName](Properties/ResamplerClassName.md) | `string` | Published | Class name string of current resampler engine. |
| [WrapMode](Properties/WrapMode.md) | `TWrapMode` | Published | Boundary wrapping mode (`wmClamp`, `wmRepeat`, `wmMirror`). |

## Events

| Name | Type | Description |
| --- | --- | --- |
| [OnAreaChanged](Events/OnAreaChanged.md) | `TAreaChangedEvent` | Fired when a specific sub-rectangle region of pixel memory is updated. |
| [OnPixelCombine](Events/OnPixelCombine.md) | `TPixelCombineEvent` | Custom pixel combination callback event when `DrawMode = dmCustom`. |
