program EllipseTests;

{$APPTYPE CONSOLE}

uses
  DUnitTestRunner,
  GR32 in '..\..\Source\GR32.pas',
  GR32_Blend in '..\..\Source\GR32_Blend.pas',
  GR32_Bindings in '..\..\Source\GR32_Bindings.pas',
  GR32_System in '..\..\Source\GR32_System.pas',
  GR32_LowLevel in '..\..\Source\GR32_LowLevel.pas',
  GR32_Math in '..\..\Source\GR32_Math.pas',
  GR32_BlendASM in '..\..\Source\GR32_BlendASM.pas',
  GR32_BlendMMX in '..\..\Source\GR32_BlendMMX.pas',
  GR32_BlendSSE2 in '..\..\Source\GR32_BlendSSE2.pas',
  GR32_Resamplers in '..\..\Source\GR32_Resamplers.pas',
  GR32_Transforms in '..\..\Source\GR32_Transforms.pas',
  GR32_VectorMaps in '..\..\Source\GR32_VectorMaps.pas',
  GR32_Rasterizers in '..\..\Source\GR32_Rasterizers.pas',
  GR32_Containers in '..\..\Source\GR32_Containers.pas',
  GR32_OrdinalMaps in '..\..\Source\GR32_OrdinalMaps.pas',
  GR32_Gamma in '..\..\Source\GR32_Gamma.pas',
  GR32_Geometry in '..\..\Source\GR32_Geometry.pas',
  GR32_Backends in '..\..\Source\GR32_Backends.pas',
  GR32_Image in '..\..\Source\GR32_Image.pas',
  GR32_Layers in '..\..\Source\GR32_Layers.pas',
  GR32_RangeBars in '..\..\Source\GR32_RangeBars.pas',
  GR32_XPThemes in '..\..\Source\GR32_XPThemes.pas',
  GR32_RepaintOpt in '..\..\Source\GR32_RepaintOpt.pas',
  GR32_MicroTiles in '..\..\Source\GR32_MicroTiles.pas',
  GR32_Paths in '..\..\Source\GR32_Paths.pas',
  GR32_Polygons in '..\..\Source\GR32_Polygons.pas',
  GR32_VPR in '..\..\Source\GR32_VPR.pas',
  GR32_VectorUtils in '..\..\Source\GR32_VectorUtils.pas',
  GR32_Brushes in '..\..\Source\GR32_Brushes.pas',
  GR32_Backends_Generic in '..\..\Source\GR32_Backends_Generic.pas',
  GR32_Backends_VCL in '..\..\Source\GR32_Backends_VCL.pas',
  GR32_Text_VCL in '..\..\Source\GR32_Text_VCL.pas',
  Bitmap32CompareDialogUnit in 'Bitmap32CompareDialogUnit.pas' {Bitmap32CompareDialog},
  TestEllipse in 'TestEllipse.pas';

begin
  DUnitTestRunner.RunRegisteredTests;
  Readln;

end.
