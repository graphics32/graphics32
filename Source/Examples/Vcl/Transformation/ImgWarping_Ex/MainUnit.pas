unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Image Warping Example
 *
 * The Initial Developers of the Original Code is:
 *
 * Michael Hansen <dyster_tid@hotmail.com>
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, ExtDlgs, Menus, ToolWin, Math, GR32, GR32_Image, GR32_Transforms,
  GR32_VectorMaps, GR32_Layers, GR32_Blend, GR32_RangeBars, GR32_Rasterizers,
  GR32_Resamplers, GR32_Math, BrushAuxiliaries;

const
  cAppName = 'Image Warping Example';

type
  TBrushTool = (btWarp, btZoom, btTwirl, btFlower);
  TBrushToolMode = (btmLeft, btmRight);
  TToolProc = procedure(var D, R: Single; Param: Single);

  { TMainForm }
  TMainForm = class(TForm)
    Bevel2: TBevel;
    BrushMeshPreview: TPaintBox32;
    BrushPanel: TPanel;
    DstImg: TImgView32;
    FeatherBar: TGaugeBar;
    FeatherLabel: TLabel;
    File1: TMenuItem;
    GeneralPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    m2x2: TMenuItem;
    m3x3: TMenuItem;
    m5x5: TMenuItem;
    m7x7: TMenuItem;
    MainMenu: TMainMenu;
    MainPanel: TPanel;
    mBilinearWarp: TMenuItem;
    mExit: TMenuItem;
    mKernelMode: TMenuItem;
    mKmDefault: TMenuItem;
    mKmTableLinear: TMenuItem;
    mKmTableNearest: TMenuItem;
    mOpenImage: TMenuItem;
    mOpenMesh: TMenuItem;
    mResetMesh: TMenuItem;
    mSamplingGrid: TMenuItem;
    mSamplingKernel: TMenuItem;
    mSaveImage: TMenuItem;
    mSaveMesh: TMenuItem;
    mSupersampleNow: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    OpenMeshDialog: TOpenDialog;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    ParamBar: TGaugeBar;
    ParamLabel: TLabel;
    PinchBar: TGaugeBar;
    PressureBar: TGaugeBar;
    RateBar: TGaugeBar;
    RateLabel: TLabel;
    Sampling1: TMenuItem;
    SaveMeshDialog: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    ScaleBar: TGaugeBar;
    SizeBar: TGaugeBar;
    ToolGroup: TRadioGroup;
    ToolPanel: TPanel;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DstImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure RateBarChange(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure DstImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure DstImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ToolGroupClick(Sender: TObject);
    procedure GaugeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScaleBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PressureBarChange(Sender: TObject);
    procedure mOpenImageClick(Sender: TObject);
    procedure mSaveImageClick(Sender: TObject);
    procedure mResetMeshClick(Sender: TObject);
    procedure mSaveMeshClick(Sender: TObject);
    procedure mOpenMeshClick(Sender: TObject);
    procedure mSupersampleNowClick(Sender: TObject);
    procedure Bi1Click(Sender: TObject);
    procedure m3x3Click(Sender: TObject);
    procedure BrushMeshPreviewResize(Sender: TObject);
    procedure SizeBarChange(Sender: TObject);
    procedure ImgButtonClick(Sender: TObject);
    procedure DstImgPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure mkmDefaultClick(Sender: TObject);
    procedure mExitClick(Sender: TObject);
  public
    Src: TBitmap32;
    Remapper: TRemapTransformation;
    GenericBrush: TGenericBrush;
    CurrentBrush: array [TBrushToolMode] of TVectorMap;
    BrushMode: TBrushToolMode;
    TempMap: TVectorMap;
    MouseDown: Boolean;
    LastPos: TPoint;
    LastDelta: TFixedPoint;
    BrushLayer: TBrushLayer;
    SampleClipRect: TRect;
    SamplingGridSize: Byte;
    KernelMode: TKernelMode;
    procedure DrawMappedBrush(Pos: TPoint);
    procedure PrecalcCurrentBrush;
    procedure UpdateBrush;
    procedure DrawBrushMeshPreview;
    function SetBrushMode(Shift: TShiftState): Boolean;
    function GetParam: Single;
    procedure KernelChanged(Sender: TObject);
  end;

var
  MainForm: TMainForm;

const
  // Provide two attractive fast resampling modes for the realtime brush warping
  RESAMPLERS: array [Boolean] of TCustomResamplerClass =(TNearestResampler,
    TLinearResampler);

  // Pick some attractive kernels for the antialiasing methods
  KERNELS: array [0..6] of TCustomKernelClass = (TBoxKernel, TLinearKernel,
    TSplineKernel, TMitchellKernel, TSinshKernel, TGaussianKernel, TCubicKernel);

var
  KernelIndex : 0..6 = 6; //TCubicKernel

implementation

uses
  {$IFNDEF FPC}
  JPEG,
  {$ELSE}
  LazJPG,
  {$ENDIF}
  GR32_LowLevel, GR32_MediaPathLocator;

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

procedure WarpDummy(var D, R: Single; Param: Single);
begin
  // do nothing
end;

procedure ZoomIn(var D, R: Single; Param: Single);
begin
  D := D - 0.1 * (1 - (1 - D) * Param);
end;

procedure ZoomOut(var D, R: Single; Param: Single);
begin
  D := D + 0.1 * (1 - (1 - D) * Param);
end;

procedure TwirlCW(var D, R: Single; Param: Single);
begin
  R := R + Param;
end;

procedure TwirlCCW(var D, R: Single; Param: Single);
begin
  R := R - Param;
end;

procedure FlowerOut(var D, R: Single; Param: Single);
begin
  D := D * Sin(R * Param);
end;

procedure FlowerIn(var D, R: Single; Param: Single);
begin
  D := D * Max(Sin(R * Param), 0);
end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  I : TBrushToolMode;
  J: Integer;
  Item: TMenuItem;
var
  MediaPath: TFileName;
begin
  Src := TBitmap32.Create;

  // load example file
  MediaPath := ExpandFileName(GetMediaPath);
  Assert(FileExists(MediaPath + 'monalisa.jpg'));
  Src.LoadFromFile(MediaPath + 'monalisa.jpg');

  Src.OuterColor := 0;
  Src.DrawMode := dmBlend;
  Src.CombineMode := cmMerge;
  SetBorderTransparent(Src, Src.BoundsRect);

  with DstImg do
  begin
    Bitmap.Assign(Src);
    Cursor := crNone;
    with PaintStages[0]^ do //Set up custom paintstage to draw checkerboard
    begin
      Stage := PST_CUSTOM;
      Parameter := 1; // use parameter to tag the stage, we inspect this in OnPaintStage
    end;
  end;

  Remapper := TRemapTransformation.Create;
  Remapper.VectorMap.SetSizeFrom(Src);
  Remapper.SrcRect := FloatRect(Src.BoundsRect);
  Remapper.MappingRect := FloatRect(DstImg.Bitmap.BoundsRect);

  for I := btmLeft to btmRight do
    CurrentBrush[I] := TVectorMap.Create;
  TempMap := TVectorMap.Create;

  for J := 0 to High(KERNELS) do
  begin
    Item := TMenuItem.Create(Self);
    Item.Caption := KERNELS[J].ClassName;
    Item.Tag := J;
    Item.OnClick := KernelChanged;
    {$IFDEF DELPHI6}
    Item.AutoCheck := True;
    {$ENDIF}
    Item.RadioItem := True;
    if J = KernelIndex then Item.Checked := True;
    mSamplingKernel.Add(Item);
  end;

  KernelMode := kmTableLinear;
  GenericBrush := TGenericBrush.Create;
  RESAMPLERS[mBilinearWarp.Checked].Create(Src);
  BrushLayer := TBrushLayer.Create(DstImg.Layers);
  SampleClipRect := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
  SamplingGridSize := 3;
  PressureBarChange(Self);
  UpdateBrush;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I : TBrushToolMode;
begin
  Src.Free;
  Remapper.Free;
  GenericBrush.Free;
  for I := btmLeft to btmRight do
    CurrentBrush[I].Free;
  TempMap.Free;
  BrushLayer.Free;
end;

function TMainForm.SetBrushMode(Shift: TShiftState): Boolean;
begin
  Result := False;
  if [ssRight, ssLeft] * Shift <> [] then
  begin
    Result := True;
    if ssLeft in Shift then
      BrushMode := btmLeft  //Higher priority
    else
      BrushMode := btmRight;
  end;
end;

procedure TMainForm.DstImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);

  function Color32ToStr(C: TColor32): string;
  begin
    Result := 'A: ' + IntToStr(C shr 24);
    Result := Result + ' R: ' + IntToStr(C shr 16 and $FF);
    Result := Result + ' G: ' + IntToStr(C shr 8 and $FF);
    Result := Result + ' B: ' + IntToStr(C and $FF);
  end;

begin
  BrushLayer.Center := Point(X, Y);
  with DstImg.ControlToBitmap(Point(X, Y)) do Caption := cAppName + ' [' + Color32ToStr(DstImg.Bitmap.PixelS[X,Y]) + ']';
  if SetBrushMode(Shift) then
    with DstImg.ControlToBitmap(Point(X, Y)) do
      DrawMappedBrush(Point(X - CurrentBrush[BrushMode].Width div 2,
        Y - CurrentBrush[BrushMode].Height div 2));
end;


procedure TMainForm.RateBarChange(Sender: TObject);
begin
  if RateBar.Position = 0 then
    UpdateTimer.Enabled := False
  else
  begin
    UpdateTimer.Enabled := True;
    UpdateTimer.Interval := Round(400 - RateBar.Position);
  end;
end;

procedure TMainForm.UpdateTimerTimer(Sender: TObject);
begin
  if MouseDown then
    DrawMappedBrush(LastPos);
end;

procedure TMainForm.DstImgMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  MouseDown := False;
end;

procedure TMainForm.DstImgMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P, Q : Integer;
begin
  MouseDown := True;
  if SetBrushMode(Shift) then
    with DstImg.ControlToBitmap(Point(X, Y)) do
    begin
      P := CurrentBrush[BrushMode].Width div 2;
      Q := CurrentBrush[BrushMode].Height div 2;
      LastPos := Point(X - P, Y - Q);
      LastDelta := FixedPoint(0,0);
      with LastPos do
        UnionRect(SampleClipRect, SampleClipRect, Rect(X, Y, X + P, Y + Q));
    end;
end;

procedure TMainForm.DrawMappedBrush(Pos: TPoint);
const
  PI2 = PI * 2;
var
  SrcR, SrcB, X, Y, I, J, ClipLeft, ClipTop, ClipRight, ClipBottom: Integer;
  Vertex: TFixedPoint;
  Dst: TVectorMap;
  DeltaX, DeltaY, Zx, Zy: TFixed;
  P: TFixedPoint;
  DstClip: TRect;
begin
  X := Pos.X;
  Y := Pos.Y;
  SrcR := CurrentBrush[BrushMode].Width - 1;
  SrcB := CurrentBrush[BrushMode].Height - 1;
  Dst := Remapper.VectorMap;
  ClipLeft := Abs(Min(0, X));
  ClipRight := Min(SrcR + X, Dst.Width - 1) - X;
  ClipTop := Abs(Min(0, Y));
  ClipBottom := Min(SrcB + Y, Dst.Height - 1) - Y;

  if ToolGroup.ItemIndex = 0 then
  begin
    DeltaX := Round((Fixed(Integer(LastPos.X - X)) + LastDelta.X) * 0.5);
    DeltaY := Round((Fixed(Integer(LastPos.Y - Y)) + LastDelta.Y) * 0.5);
    LastDelta.X := DeltaX;
    LastDelta.Y := DeltaY;
  end
  else
  begin
    DeltaX := FIXEDONE;
    DeltaY := FIXEDONE;
  end;

  for J := ClipTop to ClipBottom do
    for I := ClipLeft to ClipRight do
    begin
      Zx := Fixed(Integer(X + I));
      Zy := Fixed(Integer(Y + J));

      P := CurrentBrush[BrushMode].FixedVector[I, J];
      P.X := FixedMul(DeltaX, P.X);
      P.Y := FixedMul(DeltaY, P.Y);

      Vertex := Dst.FixedVectorXS[Zx + P.X, Zy + P.Y];
      Inc(Vertex.X, P.X);
      Inc(Vertex.Y, P.Y);
      TempMap.FixedVector[I, J] := Vertex;
    end;

  for J := ClipTop to ClipBottom do
    for I := ClipLeft to ClipRight do
      Dst.FixedVector[X + I, Y + J] := TempMap.FixedVector[I, J];

  DstClip := Rect(X, Y, X + CurrentBrush[BrushMode].Width,
    Y + CurrentBrush[BrushMode].Height);
  DstImg.Bitmap.FillRectS(DstClip, 0);
  Transform(DstImg.Bitmap, Src, Remapper, DstClip);
  DstImg.Repaint;
  UnionRect(SampleClipRect, SampleClipRect, DstClip);
  LastPos := Point(X, Y);
end;

procedure TMainForm.PrecalcCurrentBrush;
// Precalculate transformation factors and/or integrate brush weights
const
  TOOLPROCS: array [TBrushToolMode, TBrushTool] of TToolProc = ((nil, ZoomIn,
    TwirlCW, FlowerOut),(nil, ZoomOut, TwirlCCW, FlowerIn));
var
  I,J: Integer;
  w, rx, ry, nrx, nry, x, y, d, r, Param: Single;
  Tool: TBrushTool;
  ToolMode: TBrushToolMode;
  Proc: TToolProc;
begin
  Tool := TBrushTool(ToolGroup.ItemIndex);

  for ToolMode := btmLeft to btmRight do with CurrentBrush[ToolMode] do
  begin
    rx := Width - 1;
    ry := Height - 1;
    nrx := 2 / rx;
    nry := 2 / ry;

    Proc := TOOLPROCS[ToolMode, Tool];
    case Tool of
      btWarp:
        begin
          for J := 0 to Height - 1 do
            for I := 0 to Width - 1 do
            begin
              x := I * nrx - 1;
              y := J * nry - 1;
              w := GenericBrush.Weight(x, y);
              FixedVector[I, J] := FixedPoint(w, w);
            end;
          Exit;
        end;
    end;
    Param := GetParam;

    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        x := I * nrx - 1;
        y := J * nry - 1;
        w := GenericBrush.Weight(x, y);

        d := GR32_Math.Hypot(x, y);
        r := ArcTan2(y, x);
        Proc(d, r, Param);

        SinCos(r, d, y, x);
        x := (x + 1) * rx * 0.5 - I;
        y := (y + 1) * ry * 0.5 - J;

        FixedVector[I, J] := FixedPoint(x * w, y * w);
      end;
  end;
end;

procedure TMainForm.ToolGroupClick(Sender: TObject);
begin
  case ToolGroup.ItemIndex of
    0:
      begin
        ParamLabel.Enabled := False;
        ParamBar.Enabled := False;
        RateLabel.Enabled := False;
        RateBar.Enabled := False;
        UpdateTimer.Enabled := False;
      end;
    1,2,3:
      begin
        ParamLabel.Enabled := True;
        ParamBar.Enabled := True;
        RateLabel.Enabled := True;
        RateBar.Enabled := True;
        UpdateTimer.Enabled := True;
      end;
  end;

  case ToolGroup.ItemIndex of
    1: ParamLabel.Caption := 'Softness';
    2: ParamLabel.Caption := 'Strength';
    3: ParamLabel.Caption := 'Leaves Count';
  end;

  UpdateBrush;
end;

procedure TMainForm.GaugeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateBrush;
end;

procedure TMainForm.UpdateBrush;
var
  I: TBrushToolMode;
begin
  for I := btmLeft to btmRight do
    CurrentBrush[I].SetSize(SizeBar.Position, SizeBar.Position);

  TempMap.SetSizeFrom(CurrentBrush[btmLeft]);

  PrecalcCurrentBrush;
  BrushLayer.Radius := SizeBar.Position div 2;
  DrawBrushMeshPreview;
end;

procedure TMainForm.DrawBrushMeshPreview;
// Render some sort of preview of the brush mesh...
var
  I, J, rx, ry: Integer;
  Proc: TToolProc;
  Tool: TBrushTool;
  D, R, x, y, Param,Sx, Sy, w: Single;

const
  Colors: array [Boolean] of TColor32 = ($FFE0E0E0, $FF000000);
  TOOLPROCS: array [TBrushToolMode, TBrushTool] of TToolProc =
    ((WarpDummy, ZoomIn, TwirlCW, FlowerOut),
     (WarpDummy, ZoomOut, TwirlCCW, FlowerIn));

  GridSize = 8;

begin
  if BrushMeshPreview.Buffer.Empty then Exit;

  Tool := TBrushTool(ToolGroup.ItemIndex);
  Proc := TOOLPROCS[BrushMode, Tool];

  with BrushMeshPreview do
  begin
    Param := GetParam;
    Buffer.Clear($FF000000);

    rx := Width - 1;
    ry := (Height - 1) div GridSize;
    Sx := 2/rx;
    Sy := 2/((Height - 1) / GridSize);
    for J := 0 to ry do
    begin
      Buffer.MoveToF(0, J * GridSize);
      for I := 0 to rx do
      begin
        x := I * Sx - 1;
        y := J * Sy - 1;
        d := GR32_Math.Hypot(x, y);
        r := ArcTan2(y, x);
        Proc(d, r, Param);
        w := GenericBrush.Weight(x,y);
        SinCos(r, d, y, x);
        x := I - ((x + 1) * rx * 0.5 - I) * w;
        y := J - ((y + 1) * ry * 0.5 - J) * w;
        y := y * GridSize;
        Buffer.PenColor := Gray32(15 + Round(240 * (1 - Sqr(1 - w))));
        Buffer.LineToFS(x,y);
      end;
    end;

    rx := (Width - 1) div GridSize;
    ry := Height - 1;
    Sx := 2/((Width - 1) / GridSize);
    Sy := 2/ry;
    for I := 0 to rx do
    begin
      Buffer.MoveToF(I * GridSize, 0);
      for J := 0 to ry do
      begin
        x := I * Sx - 1;
        y := J * Sy - 1;
        d := GR32_Math.Hypot(x, y);
        r := ArcTan2(y, x);
        Proc(d, r, Param);
        w := GenericBrush.Weight(x,y);
        SinCos(r, d, y, x);
        x := I - ((x + 1) * rx * 0.5 - I) * w;
        x := x * GridSize;
        y := J - ((y + 1) * ry * 0.5 - J) * w;
        Buffer.PenColor := Gray32(15 + Round(240 * (1 - Sqr(1 - w))) );
        Buffer.LineToFS(x,y);
      end;
    end;
    Buffer.FrameRectS(GetViewPortRect , $FFFFFFFF); //Draw Frame
    Repaint;
  end;
end;


procedure TMainForm.ScaleBarMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  S: Single;
begin
  S := ScaleBar.Position / 100;
  Remapper.Scale(S, S);
  SampleClipRect := Remapper.VectorMap.GetTrimmedBounds;
  Transform(DstImg.Bitmap, Src, Remapper);
end;


function TMainForm.GetParam: Single;
begin
  case TBrushTool(ToolGroup.ItemIndex) of
    btFlower: Result := Round(ParamBar.Position / 100 * 16 + 1);
    btZoom: Result := 1 - ParamBar.Position / 100;
  else Result := ParamBar.Position / 100
  end
end;

procedure TMainForm.PressureBarChange(Sender: TObject);
begin
  GenericBrush.Pressure := PressureBar.Position / 100;
  GenericBrush.Pinch := PinchBar.Position / 100;
  GenericBrush.Feather := FeatherBar.Position / 100;
  UpdateBrush;
end;

procedure TMainForm.mOpenImageClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    Src.LoadFromFile(OpenPictureDialog.Filename);
    SetBorderTransparent(Src, Src.BoundsRect); //Fix against border issues
    DstImg.Bitmap.Assign(Src);
    Remapper.VectorMap.SetSizeFrom(Src);
    Remapper.SrcRect := FloatRect(Src.BoundsRect);
    Remapper.MappingRect := FloatRect(DstImg.Bitmap.BoundsRect);
  end;
end;

procedure TMainForm.mSaveImageClick(Sender: TObject);
begin
 with SavePictureDialog do if Execute then
 begin
   if Lowercase(ExtractFileExt(Filename)) <> '.bmp' then
     Filename:= Filename + '.bmp';
   DstImg.Bitmap.SaveToFile(Filename);
 end
end;

procedure TMainForm.mResetMeshClick(Sender: TObject);
begin
  Remapper.VectorMap.Clear;
  DstImg.Bitmap.Assign(Src);
  ScaleBar.Position := 100;
  Remapper.Scale(1,1);
end;

procedure TMainForm.mSaveMeshClick(Sender: TObject);
begin
 with SaveMeshDialog do if Execute then
 begin
   if Lowercase(ExtractFileExt(Filename)) <> '.msh' then
     Filename:= Filename + '.msh';
   Remapper.VectorMap.SaveToFile(Filename);
 end
end;

procedure TMainForm.mOpenMeshClick(Sender: TObject);
begin
 with OpenMeshDialog do if Execute then begin
   Remapper.VectorMap.LoadFromFile(Filename);
   Transform(DstImg.Bitmap, Src, Remapper);
   SampleClipRect := Remapper.VectorMap.GetTrimmedBounds;
   DstImg.Repaint;
 end;
end;

procedure TMainForm.mSupersampleNowClick(Sender: TObject);
var
  Rasterizer: TRasterizer;
  Transformer: TTransformer;
  SuperSampler: TSuperSampler;
  KernelResampler : TKernelResampler;
begin
  Screen.Cursor := crHourGlass;
  KernelResampler := TKernelResampler.Create(Src);
  KernelResampler.KernelMode := KernelMode;

  // Normally this should be set higher.
  // It is set low here to display perceptual consequences
  KernelResampler.TableSize := 4;

  KernelResampler.Kernel := KERNELS[KernelIndex].Create;

  Transformer := TTransformer.Create(Src.Resampler, Remapper);
  SuperSampler := TSuperSampler.Create(Transformer);
  Rasterizer := TRegularRasterizer.Create;
  try
    Rasterizer.Sampler := SuperSampler;
    SuperSampler.SamplingX := SamplingGridSize;
    SuperSampler.SamplingY := SamplingGridSize;
    DstImg.Bitmap.FillRectS(SampleClipRect, 0);
    Rasterizer.Rasterize(DstImg.Bitmap, SampleClipRect, Src);
    SampleClipRect := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
  finally
    Rasterizer.Free;
    SuperSampler.Free;
    Transformer.Free;
    RESAMPLERS[mBilinearWarp.Checked].Create(Src);
    Screen.Cursor := crDefault;
    DstImg.Repaint;
  end;
end;

procedure TMainForm.Bi1Click(Sender: TObject);
begin
  RESAMPLERS[mBilinearWarp.Checked].Create(Src);
  Transform(DstImg.Bitmap, Src, Remapper);
end;

procedure TMainForm.m3x3Click(Sender: TObject);
begin
  if Sender is TMenuItem then SamplingGridSize := TMenuItem(Sender).Tag;
end;

procedure TMainForm.KernelChanged(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    KernelIndex := TMenuItem(Sender).Tag;
    SampleClipRect := Remapper.VectorMap.GetTrimmedBounds;
    mSuperSampleNowClick(Self);
  end;
end;

procedure TMainForm.BrushMeshPreviewResize(Sender: TObject);
begin
  UpdateBrush;
end;

procedure TMainForm.SizeBarChange(Sender: TObject);
begin
  DstImg.Repaint;
  BrushLayer.Radius := SizeBar.Position div 2;
  BrushLayer.Center := Point(DstImg.Width div 2, DstImg.Height div 2);
end;

procedure TMainForm.ImgButtonClick(Sender: TObject);
begin
  if Sender is TToolbutton then
    TToolButton(Sender).DropdownMenu.Popup(Mouse.CursorPos.X,
       Mouse.CursorPos.Y);
end;

procedure TMainForm.DstImgPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0); 
var 
  R: TRect;
  I, J: Integer; 
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer; 
begin
  with TImgView32(Sender) do
  begin
    BeginUpdate;
    R := GetViewportRect;
    TileHeight := 8;
    TileWidth := 8;
    TilesHorz := (R.Right - R.Left) div TileWidth;
    TilesVert := (R.Bottom - R.Top) div TileHeight;
    TileY := 0;
    for J := 0 to TilesVert do
    begin
      TileX := 0;
      OddY := J and $1;
      for I := 0 to TilesHorz do
      begin
        Buffer.FillRectS(TileX, TileY, TileX + TileWidth, TileY + TileHeight,Colors[I and $1 = OddY]);
        Inc(TileX, TileWidth);
      end;
      Inc(TileY, TileHeight);
    end;
    EndUpdate;
  end;
end;

procedure TMainForm.mkmDefaultClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    KernelMode := TKernelMode(TMenuItem(Sender).Tag);
    SampleClipRect := Remapper.VectorMap.GetTrimmedBounds;
    mSuperSampleNowClick(Self);
  end;
end;

procedure TMainForm.mExitClick(Sender: TObject);
begin
  Close;
end;

{$IFDEF FPC}
initialization
{$ENDIF}

end.
