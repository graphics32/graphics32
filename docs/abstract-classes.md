# Abstract Classes Audit

This document provides a comprehensive report of abstract class architecture across all Pascal source units located directly in `Source/*.pas`.

The analyzed classes are categorized into three distinct groups:
1. **Category 1: Explicitly Declared as `abstract`**: Classes declared using the `abstract` class specifier (e.g. `type TMyClass = class abstract(TBase)`).
2. **Category 2: Undeclared Abstract Classes with Abstract Members**: Classes containing `virtual; abstract;` methods or inheriting unimplemented abstract methods from ancestor classes, but missing the explicit `abstract` class specifier.
3. **Category 3: Architectural Abstract Base Classes**: Base classes intended purely for subclassing/extension (e.g., `TCustom...` components or core framework base classes) that do not fall into Category 2, but should be declared `abstract` to prevent direct instantiation.

---

## Category 1: Explicitly Declared as `abstract`

Total count: **18**

| Unit | Class Name | Base Class | Abstract Members | Description / Architectural Purpose |
|---|---|---|---|---|
| `GR32.Blend.Modes.pas` | `TCustomGraphics32Blender` | `TObject` | ``GetName`, `Blend`` | Base abstract blender class providing pixel blending engine interfaces. |
| `GR32.Blend.Modes.pas` | `TGraphics32ComponentBlender` | `TCustomGraphics32ComponentBlender` | `None` | Base abstract component blender operating on individual RGBA channels. |
| `GR32.Blend.Modes.pas` | `TGraphics32SeparableBlender` | `TGraphics32ComponentBlender` | ``BlendComponent`` | Base abstract separable blender operating on independent color components. |
| `GR32.ImageFormats.PSD.Model.pas` | `TCustomPhotoshopBitmapLayer32` | `TCustomPhotoshopLayer` | ``GetBitmap`` | Base abstract 32-bit bitmap Photoshop layer containing pixel bitmap data. |
| `GR32.ImageFormats.PSD.Model.pas` | `TCustomPhotoshopLayer` | `TObject` | `None` | Base abstract Photoshop document layer representation. |
| `GR32.Paint.Tool.Brush.pas` | `TBitmap32PaintToolBrush` | `TObject` | ``CreateBrush`` | Base abstract tool brush class for painting on TBitmap32 surfaces. |
| `GR32.Paint.Tool.Pen.pas` | `TCustomBitmap32PaintToolPen` | `TCustomBitmap32PaintTool` | `None` | Base abstract pen tool class for vector stroke painting. |
| `GR32_Layers.pas` | `TCustomBitmapLayer` | `TCustomIndirectBitmapLayer` | ``GetBitmapClass`` | Base abstract bitmap layer class managing layer bitmap access. |
| `GR32_Polygons.pas` | `TCustomPolygonFiller` | `TObject` | ``GetFillLine`` | Base abstract polygon filler producing scanline fill routines. |
| `GR32_Polygons.pas` | `TCustomPolygonRenderer` | `TThreadPersistent` | ``PolyPolygonFS`` | Base abstract polygon renderer rendering filled and outlined paths. |
| `GR32_Polygons.pas` | `TPolygonRenderer32` | `TCustomPolygonRenderer` | ``PolyPolygonFS`` | Base abstract 32-bit software polygon renderer. |
| `GR32_PortableNetworkGraphic.Chunks.bKGD.pas` | `TCustomPngBackgroundColor` | `TPersistent` | ``GetChunkSize`, `ReadFromStream`, `WriteToStream`` | Base abstract PNG bKGD (background color) chunk handler. |
| `GR32_PortableNetworkGraphic.Chunks.sBIT.pas` | `TCustomPngSignificantBits` | `TPersistent` | ``GetChunkSize`, `Create`, `ReadFromStream`, `WriteToStream`` | Base abstract PNG sBIT (significant bits) chunk handler. |
| `GR32_PortableNetworkGraphic.Chunks.tRNS.pas` | `TCustomPngTransparency` | `TPersistent` | ``GetChunkSize`, `ReadFromStream`, `WriteToStream`` | Base abstract PNG tRNS (transparency) chunk handler. |
| `GR32_PortableNetworkGraphic.Encoding.pas` | `TCustomPngCoder` | `TObject` | ``EncodeFilterRow`, `DecodeFilterRow`` | Base abstract PNG image row coder (encoder/decoder base). |
| `GR32_PortableNetworkGraphic.Encoding.pas` | `TCustomPngDecoder` | `TCustomPngCoder` | ``DecodeToScanline`` | Base abstract PNG decoder converting encoded scanlines into bitmap pixels. |
| `GR32_PortableNetworkGraphic.Transcoding.pas` | `TCustomPngTranscoder` | `TCustomPngCoder` | ``Transcode`` | Base abstract PNG transcoder modifying PNG chunk streams. |
| `GR32_Transforms.pas` | `TTransformation` | `TNotifiablePersistent` | `None` | Abstract base class declaration. |

---

## Category 2: Undeclared Abstract Classes with Virtual Abstract Members

These classes contain `virtual; abstract;` methods or inherit unimplemented abstract methods from base classes, but are **not** currently declared with the `abstract` keyword. They should be updated to include `class abstract` in their declarations.

Total count: **21**

| Unit | Class Name | Base Class | Unimplemented Abstract Members | Description / Architectural Purpose |
|---|---|---|---|---|
| `Clipper.Engine.pas` | `TPolyPathBase` | `TObject` | `AddChild` | Base tree node structure for polygon clipping hierarchies in Clipper engine. |
| `GR32.Paint.Brush.pas` | `TCustomPaintBrush` | `TObject` | `GetHeight`, `GetWidth`, `Draw`, `DrawPreview` | Base paint brush class defining vector stroke and fill drawing contracts. |
| `GR32_ArrowHeads.pas` | `TArrowHeadAbstract` | `TObject` | `GetPointsInternal` | Base class for computing arrowhead geometry points at line endpoints. |
| `GR32_ColorGradients.pas` | `TCustomArbitrarySparsePointGradientPolygonFiller` | `TCustomSparsePointGradientPolygonFiller` | `GetFillLine` (from `TCustomPolygonFiller`) | Base filler for arbitrary sparse point set polygon fills. |
| `GR32_ColorGradients.pas` | `TCustomGradientLookupTablePolygonFiller` | `TCustomGradientPolygonFiller` | `GetFillLine` (from `TCustomPolygonFiller`) | Base polygon filler utilizing pre-calculated color LUT tables. |
| `GR32_ColorGradients.pas` | `TCustomGradientPolygonFiller` | `TCustomPolygonFiller` | `GetFillLine` (from `TCustomPolygonFiller`) | Base filler for gradient-filled polygon rasterization. |
| `GR32_ColorGradients.pas` | `TCustomGradientSampler` | `TCustomSampler` | `UpdateInternals` | Base color gradient sampler interface generating interpolated color ramps. |
| `GR32_ColorGradients.pas` | `TCustomLinearGradientPolygonFiller` | `TCustomGradientLookupTablePolygonFiller` | `GetFillLine` (from `TCustomPolygonFiller`) | Base polygon filler rasterizing linear color gradients. |
| `GR32_ColorGradients.pas` | `TCustomRadialGradientPolygonFiller` | `TCustomGradientLookupTablePolygonFiller` | `GetFillLine` (from `TCustomPolygonFiller`), `EllipseBoundsChanged` | Base polygon filler rasterizing radial color gradients. |
| `GR32_ColorGradients.pas` | `TCustomSparsePointGradientPolygonFiller` | `TCustomPolygonFiller` | `GetFillLine` (from `TCustomPolygonFiller`), `GetCount`, `GetColor`, `GetPoint`, `GetColorPoint`, `SetColor`, `SetColorPoint`, `SetPoint`, `SetPoints`, `SetColorPoints` | Base polygon filler using sparse point gradient interpolation. |
| `GR32_ColorGradients.pas` | `TCustomSparsePointGradientSampler` | `TCustomSampler` | `GetCount`, `GetColor`, `GetPoint`, `GetColorPoint`, `SetColor`, `SetColorPoint`, `SetPoint`, `SetPoints`, `SetColorPoints` | Base sampler for sparse point color interpolation across arbitrary point sets. |
| `GR32_ColorPicker.pas` | `TCustomColorPicker` | `TCustomControl` | `PaintColorPicker` | Base visual color picker control class for selecting colors interactively. |
| `GR32_Rasterizers.pas` | `TRasterizer` | `TThreadPersistent` | `DoRasterize` | Base spatial rasterizer class performing scanline polygon rendering. |
| `GR32_RepaintOpt.pas` | `TCustomRepaintOptimizer` | `TNoRefCountObject` | `Reset`, `UpdatesAvailable`, `PerformOptimization`, `BufferResizedHandler` | Base repaint optimizer managing dirty rectangular surface regions. |
| `GR32_Resamplers.pas` | `TCustomKernel` | `TPersistent` | `Filter`, `GetWidth` | Base resampling kernel defining spatial filter function and kernel width. |

---

## Category 3: Architectural Abstract Base Classes

These classes do not contain unimplemented abstract methods, but serve as abstract base classes designed for extension and subclassing (e.g. `TCustom...` framework classes). Direct instantiation of these classes is not intended. Declaring them as `abstract` ensures compile-time and design-time clarity.

Total count: **31**

| Unit | Class Name | Base Class | Reason / Architectural Purpose |
|---|---|---|---|
| `Clipper.Engine.pas` | `TClipperBase` | `TObject` | Base abstract architecture for Clipper engine; manages low-level polygon execution structures and state. |
| `GR32.pas` | `TCustomBackend` | `TThreadPersistent` | Abstract surface backend interface managing surface creation, destruction, memory allocation, and OS handle interop. |
| `GR32.Paint.Controller.pas` | `TCustomBitmap32PaintController` | `TObject` | Base controller class for managing paint operations and user interaction state. |
| `GR32.ImageFormats.TGraphic.pas` | `TCustomImageFormatAdapterTGraphic` | `TObject` | Base image format adapter binding Delphi standard TGraphic components to Graphics32. |
| `GR32.ImageFormats.PSD.Model.pas` | `TCustomPhotoshopLayerProperty` | `TObject` | Base property record handler for Photoshop layer metadata chunks. |
| `GR32.pas` | `TCustomMap` | `TThreadPersistent` | Base 2D spatial lookup map abstract class (ancestor of byte maps, vector maps, and bitmaps). |
| `GR32.pas` | `TCustomResampler` | `TCustomSampler` | Base 2D spatial resampler class providing fundamental sampling interface; requires derived implementation. |
| `GR32.pas` | `TCustomSampler` | `TNotifiablePersistent` | Base spatial data sampler interface providing point lookup methods (GetSampleInt/Float/Fixed). |
| `GR32_Brushes.pas` | `TCustomBrush` | `TNotifiablePersistent` | Base vector paint brush class managing stroke generation and style properties. |
| `GR32_ColorGradients.pas` | `TCustomArbitrarySparsePointGradientSampler` | `TCustomSparsePointGradientSampler` | Base sparse-point gradient sampler for arbitrary polygon point sets. |
| `GR32_ColorGradients.pas` | `TCustomCenterLutGradientSampler` | `TCustomGradientLookUpTableSampler` | Base radial/centered LUT gradient sampler managing center offset and LUT caching. |
| `GR32_ColorGradients.pas` | `TCustomCenterRadiusAngleLutGradientSampler` | `TCustomCenterRadiusLutGradientSampler` | Base angled radial LUT gradient sampler managing rotation angle and radial focus. |
| `GR32_ColorGradients.pas` | `TCustomCenterRadiusLutGradientSampler` | `TCustomCenterLutGradientSampler` | Base radial LUT gradient sampler managing focus radius and color transformations. |
| `GR32_ColorGradients.pas` | `TCustomGradientLookUpTableSampler` | `TCustomGradientSampler` | Base 1D color gradient lookup table sampler providing palette interpolation. |
| `GR32_ColorPicker.pas` | `TCustomColorPickerComponent` | `TCustomColorPicker` | Base visual color picker component handling user input and mouse interaction. |
| `GR32_ColorPicker.pas` | `TCustomColorPickerGTK` | `TCustomColorPicker` | Base GTK-style color picker control managing color selection sub-elements. |
| `GR32_ColorPicker.pas` | `TCustomColorPickerHS` | `TCustomColorPicker` | Base Hue/Saturation color picker control. |
| `GR32_ColorPicker.pas` | `TCustomColorPickerHSV` | `TCustomColorPicker` | Base Hue/Saturation/Value color picker control. |
| `GR32_ColorPicker.pas` | `TCustomColorPickerRGBA` | `TCustomColorPicker` | Base Red/Green/Blue/Alpha color picker slider control. |
| `GR32_ColorSwatch.pas` | `TCustomColorSwatch` | `TCustomControl` | Base color swatch control displaying active/selected palette colors. |
| `GR32_Image.pas` | `TCustomImage32` | `TObject` | Base interactive image display component handling scrollbars, zooming, and layers. |
| `GR32_Image.pas` | `TCustomImgView32` | `TCustomImage32` | Base image view control providing viewport and transformation management. |
| `GR32_Image.pas` | `TCustomPaintBox32` | `TGraphics32ControlBaseClass` | Base lightweight double-buffered drawing surface component. |
| `GR32_Layers.pas` | `TCustomIndirectBitmapLayer` | `TPositionedLayer` | Base layer class maintaining indirect bitmap reference and layer position/bounds. |
| `GR32_Layers.pas` | `TCustomRubberBandLayer` | `TPositionedLayer` | Base interactive rubberband layer providing sizing handles and user interaction. |
| `GR32_Polygons.GDIPlus.pas` | `TCustomPolygonRenderer32GDIPlus` | `TPolygonRenderer32` | Base GDI+ accelerated polygon renderer base class. |
| `GR32_PortableNetworkGraphic.Chunks.tEXt.pas` | `TCustomChunkPngText` | `TCustomDefinedChunkWithHeader` | Base PNG textual metadata chunk decoder/encoder. |
| `GR32_RangeBars.pas` | `TCustomGaugeBar` | `TArrowBar` | Base gauge bar control providing interactive range and progress visualization. |
| `GR32_RangeBars.pas` | `TCustomRangeBar` | `TArrowBar` | Base range bar control providing interactive min/max range selection. |
| `GR32_Resamplers.pas` | `TWindowedKernel` | `TObject` | Base windowed resampler kernel class requiring concrete windowing filter calculation in subclasses. |
| `GR32_Paths.pas` | `TCustomCanvas` | `TObject` | Base vector canvas drawing abstraction managing path creation and stroke/fill renderers. |
