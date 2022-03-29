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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFNDEF FPC} Windows, {$ELSE} LResources, LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, Grids,
  ExtCtrls, StdCtrls, Buttons, GR32, GR32_Image, GR32_Transforms,
  GR32_Resamplers, GR32_Layers, GR32_RangeBars;

type
  TOpType = (opNone, opTranslate, opScale, opRotate, opSkew);
  TOpRec = record
    OpType: TOpType;
    Dx, Dy: Extended;        // shifts for opTranslate mode
    Sx, Sy: Extended;        // scale factors for opScale mode
    Cx, Cy, Alpha: Extended; // rotation center and angle (deg) for opRotate mode
    Fx, Fy: Extended;        // skew factors for opSkew mode
  end;
  TOpRecs = array[0..7] of TOpRec;

const
  OpTypes: array [0..4] of TOpType = (opNone, opTranslate, opScale, opRotate,
    opSkew);

type
  TTransformMode = (tmAffine, tmProjective, tmBilinear);

  { TFormTranformExample }

  TFormTranformExample = class(TForm)
    BtnClearAll: TButton;
    CbxRepeat: TCheckBox;
    CmbKernelClassNames: TComboBox;
    CmbResamplerClassNames: TComboBox;
    ComboBox: TComboBox;
    Dst: TImage32;
    EdtAlpha: TEdit;
    EdtCodeString: TEdit;
    EdtCx: TEdit;
    EdtCy: TEdit;
    EdtDx: TEdit;
    EdtDy: TEdit;
    EdtFx: TEdit;
    EdtFy: TEdit;
    EdtSx: TEdit;
    EdtSy: TEdit;
    GbrAlpha: TGaugeBar;
    GbrDx: TGaugeBar;
    GbrDy: TGaugeBar;
    GbrFx: TGaugeBar;
    GbrFy: TGaugeBar;
    GbrSx: TGaugeBar;
    GbrSy: TGaugeBar;
    LblAlpha: TLabel;
    LblCodeString: TLabel;
    LblCx: TLabel;
    LblCy: TLabel;
    LblDx: TLabel;
    LblDy: TLabel;
    LblFx: TLabel;
    LblFy: TLabel;
    LblInfoRotate: TLabel;
    LblInfoSkew: TLabel;
    LblInfoTranslate: TLabel;
    LblKernel: TLabel;
    LblNoOperation: TLabel;
    LblProjectiveNote: TLabel;
    LblResampler: TLabel;
    LblScale: TLabel;
    LblSx: TLabel;
    LblSy: TLabel;
    LblTransformationMatrix: TLabel;
    LblType: TLabel;
    ListBox: TListBox;
    Notebook: TNotebook;
    OpacityBar: TGaugeBar;
    PageControl: TPageControl;
    {$IFDEF FPC}
    PageNone: TPage;
    PageTranslate: TPage;
    PageScale: TPage;
    PageRotate: TPage;
    PageSkew: TPage;
    {$ENDIF}
    PnlOpacity: TPanel;
    PnlOperation: TPanel;
    PnlTransformationMatrix: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Src: TImage32;
    StringGrid: TStringGrid;
    TstAffine: TTabSheet;
    TstProjective: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnClearAllClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure OpacityChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure RotationChanged(Sender: TObject);
    procedure RotationScrolled(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure ScaleScrolled(Sender: TObject);
    procedure SkewChanged(Sender: TObject);
    procedure SkewScrolled(Sender: TObject);
    procedure TranslationChanged(Sender: TObject);
    procedure TranslationScrolled(Sender: TObject);

    procedure SrcRBResizingEvent(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState);

    procedure RubberLayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ResamplerClassNamesListClick(Sender: TObject);
    procedure CmbResamplerClassNamesChange(Sender: TObject);
    procedure CmbKernelClassNamesChange(Sender: TObject);
    procedure DstPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure CbxRepeatClick(Sender: TObject);
    procedure SrcDblClick(Sender: TObject);
  protected
    LoadingValues: Boolean;
    DraggedVertex: Integer;
    LastMousePos: TPoint;
    StippleStart: Single;
    procedure PaintHandles(Sender: TObject; BackBuffer: TBitmap32);
  public
    SrcRubberBandLayer: TRubberBandLayer;
    Operation: TOpRecs;
    Current: ^TOpRec;
    AffineTransformation: TAffineTransformation;
    ProjectiveTransformation: TProjectiveTransformation;
    Transformation: TTransformation;
    Vertices: array [0..3] of TPoint;
    Mode: TTransformMode;
    procedure ClearTransformations;
    procedure DoTransform;
    procedure GenTransform;
    procedure PrepareSource;
    procedure ShowSettings(OperationNum: Integer);
    procedure InitVertices; // for projective mapping
  end;

var
  FormTranformExample: TFormTranformExample;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Types,
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG;
{$ELSE}
  LazJPG;
{$ENDIF}

const
  CAccessMode: array [Boolean] of TPixelAccessMode = (pamSafe, pamWrap);

function GetVal(Src: string; var Dst: Extended): Boolean;
var
  Code: Integer;
begin
  Val(Src, Dst, Code);
  Result := Code = 0;
end;

procedure TFormTranformExample.FormCreate(Sender: TObject);
var
  ResStream: TResourceStream;
  JPEG: TJPEGImage;
begin
  // load example image
  JPEG := TJPEGImage.Create;
  try
    ResStream := TResourceStream.Create(HInstance, 'Delphi', RT_RCDATA);
    try
      JPEG.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    Src.Bitmap.Assign(JPEG);
  finally
    JPEG.Free;
  end;

  //Setup custom paintstages ("checkerboard" and border)
  with Dst do
  begin
    with PaintStages[0]^ do //Set up custom paintstage to draw checkerboard
    begin
      Stage := PST_CUSTOM;
      Parameter := 1; // use parameter to tag the stage, we inspect this in OnPaintStage
    end;
    with PaintStages.Add^ do  //Insert new paintstage on top of everything else, we use this to draw border
    begin
      Stage := PST_CUSTOM;
      Parameter := 2;
    end;
  end;

  with Src do
  begin
    with PaintStages[0]^ do
    begin
      Stage := PST_CUSTOM;
      Parameter := 1;
    end;
    with PaintStages.Add^ do
    begin
      Stage := PST_CUSTOM;
      Parameter := 2;
    end;
  end;

  ResamplerList.GetClassNames(CmbResamplerClassNames.Items);
  KernelList.GetClassNames(CmbKernelClassNames.Items);
  CmbResamplerClassNames.ItemIndex := 0;
  CmbKernelClassNames.ItemIndex := 0;

  SrcRubberBandLayer := TRubberBandLayer.Create(Src.Layers);
  SrcRubberBandLayer.OnResizing := SrcRBResizingEvent;
  SrcRubberBandLayer.Location := FloatRect(0, 0, Src.Bitmap.Width - 1, Src.Bitmap.Height - 1);
  with TCustomLayer.Create(Dst.Layers) do
  begin
    OnPaint := PaintHandles;
  end;

  DraggedVertex := -1;
  Dst.SetupBitmap; // set the destination bitmap size to match the image size
  PrepareSource;
  ClearTransformations;
  ShowSettings(0);
  AffineTransformation := TAffineTransformation.Create;
  ProjectiveTransformation := TProjectiveTransformation.Create;
  Transformation := AffineTransformation;
  DoTransform;

  Application.OnIdle := AppEventsIdle;
end;

procedure TFormTranformExample.ClearTransformations;
var
  I: Integer;
begin
  FillChar(Operation[0], SizeOf(TOpRecs), 0);
  for I := 0 to 7 do
  begin
    Operation[I].Sx := 1;
    Operation[I].Sy := 1;
    Operation[I].Cx := Src.Bitmap.Width * 0.5;
    Operation[I].Cy := Src.Bitmap.Height * 0.5;
  end;
end;

procedure TFormTranformExample.PrepareSource;
begin
  // make the border pixels transparent while keeping their RGB components
  if not CbxRepeat.Checked then
    SetBorderTransparent(Src.Bitmap, Src.Bitmap.BoundsRect);
end;

procedure TFormTranformExample.DoTransform;
var
  i, j: Integer;
begin
  GenTransform;
  Dst.BeginUpdate;

  Dst.Bitmap.Clear($00000000);
  Transform(Dst.Bitmap, Src.Bitmap, Transformation);

  Dst.EndUpdate;
  Dst.Invalidate;

  if Mode = tmAffine then
  begin
    // fill the string grid
    for j := 0 to 2 do
      for i := 0 to 2 do
        StringGrid.Cells[i, j] := Format('%.3g', [AffineTransformation.Matrix[i, j]]);
    StringGrid.Col := 3; // hide grid cursor
  end;
end;

procedure TFormTranformExample.GenTransform;
var
  I: Integer;
  Rec: TOpRec;
  S: string;
begin
  if Mode = tmProjective then
  begin
    ProjectiveTransformation.X0 := Vertices[0].X;
    ProjectiveTransformation.Y0 := Vertices[0].Y;
    ProjectiveTransformation.X1 := Vertices[1].X;
    ProjectiveTransformation.Y1 := Vertices[1].Y;
    ProjectiveTransformation.X2 := Vertices[2].X;
    ProjectiveTransformation.Y2 := Vertices[2].Y;
    ProjectiveTransformation.X3 := Vertices[3].X;
    ProjectiveTransformation.Y3 := Vertices[3].Y;
  end
  else
  begin
    // affine mode
    AffineTransformation.Clear;
    for I := 0 to 7 do
    begin
      Rec := Operation[I];
      case Rec.OpType of
        opTranslate:  AffineTransformation.Translate(Rec.Dx, Rec.Dy);
        opScale:      AffineTransformation.Scale(Rec.Sx, Rec.Sy);
        opRotate:     AffineTransformation.Rotate(Rec.Cx, Rec.Cy, Rec.Alpha);
        opSkew:       AffineTransformation.Skew(Rec.Fx, Rec.Fy);
      end;
      case Rec.OpType of
        opTranslate:  s := s + Format('Translate(%.3g, %.3g); ', [Rec.Dx, Rec.Dy]);
        opScale:      s := s + Format('Scale(%.3g, %.3g); ', [Rec.Sx, Rec.Sy]);
        opRotate:     s := s + Format('Rotate(%.3g, %.3g, %3g); ', [Rec.Cx, Rec.Cy, Rec.Alpha]);
        opSkew:       s := s + Format('Skew(%.3g, %.3g); ', [Rec.Fx, Rec.Fy]);
      end;
    end;
    if Length(s) = 0 then s := 'Clear;';
    EdtCodeString.Text := s;
  end;
  Transformation.SrcRect := SrcRubberBandLayer.Location;
end;

procedure TFormTranformExample.FormDestroy(Sender: TObject);
begin
  AffineTransformation.Free;
  ProjectiveTransformation.Free;
end;

procedure TFormTranformExample.BtnClearAllClick(Sender: TObject);
begin
  ClearTransformations;
  ShowSettings(Listbox.ItemIndex);
  DoTransform;
end;

procedure TFormTranformExample.ListBoxClick(Sender: TObject);
begin
  ShowSettings(ListBox.ItemIndex);
end;

procedure TFormTranformExample.ShowSettings(OperationNum: Integer);
begin
  LoadingValues := True;
  ListBox.ItemIndex := OperationNum;
  Current := @Operation[OperationNum];
  Combobox.ItemIndex := Ord(Current.OpType);
  NoteBook.PageIndex := Ord(Current.OpType);
  EdtDx.Text := Format('%.4g', [Current.Dx]);
  EdtDy.Text := Format('%.4g', [Current.Dy]);
  GbrDx.Position := Round(Current.Dx * 10);
  GbrDy.Position := Round(Current.Dy * 10);
  EdtSx.Text := Format('%.4g', [Current.Sx]);
  EdtSy.Text := Format('%.4g', [Current.Sy]);
  GbrSx.Position := Round(Current.Sx * 100);
  GbrSy.Position := Round(Current.Sy * 100);
  EdtCx.Text := Format('%.4g', [Current.Cx]);
  EdtCy.Text := Format('%.4g', [Current.Cy]);
  EdtAlpha.Text := Format('%.4g', [Current.Alpha]);
  GbrAlpha.Position := Round(Current.Alpha * 2);
  EdtFx.Text := Format('%.4g', [Current.Fx]);
  EdtFy.Text := Format('%.4g', [Current.Fy]);
  GbrFx.Position := Round(Current.Fx * 100);
  GbrFy.Position := Round(Current.Fy * 100);
  LoadingValues := False;
end;

procedure TFormTranformExample.ComboBoxChange(Sender: TObject);
begin
  Current.OpType := OpTypes[ComboBox.ItemIndex];
  ShowSettings(ListBox.ItemIndex);
  DoTransform;
end;

procedure TFormTranformExample.TranslationChanged(Sender: TObject);
var
  Tx, Ty: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(EdtDx.Text, Tx) and GetVal(EdtDy.Text, Ty) then
  begin
    Current.Dx := Tx;
    Current.Dy := Ty;
    DoTransform;
    LoadingValues := True;
    GbrDx.Position := Round(Current.Dx * 10);
    GbrDy.Position := Round(Current.Dy * 10);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.TranslationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Dx := GbrDx.Position * 0.1;
  Current.Dy := GbrDy.Position * 0.1;
  DoTransform;
  LoadingValues := True;
  EdtDx.Text := FloatToStr(Current.Dx);
  EdtDy.Text := FloatToStr(Current.Dy);
  LoadingValues := False;
end;

procedure TFormTranformExample.ScaleChanged(Sender: TObject);
var
  Sx, Sy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(EdtSx.Text, Sx) and GetVal(EdtSy.Text, Sy) then
  begin
    Current.Sx := Sx;
    Current.Sy := Sy;
    DoTransform;
    LoadingValues := True;
    GbrSx.Position := Round(Current.Sx * 100);
    GbrSy.Position := Round(Current.Sy * 100);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.ScaleScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Sx := GbrSx.Position * 0.01;
  Current.Sy := GbrSy.Position * 0.01;
  DoTransform;
  LoadingValues := True;
  EdtSx.Text := FloatToStr(Current.Sx);
  EdtSy.Text := FloatToStr(Current.Sy);
  LoadingValues := False;
end;

procedure TFormTranformExample.RotationChanged(Sender: TObject);
var
  Cx, Cy, Alpha: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(EdtCx.Text, Cx) and GetVal(EdtCy.Text, Cy) and
    GetVal(EdtAlpha.Text, Alpha) then
  begin
    Current.Cx := Cx;
    Current.Cy := Cy;
    Current.Alpha := Alpha;
    DoTransform;
    LoadingValues := True;
    GbrAlpha.Position := Round(Alpha * 2);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.RotationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Alpha := GbrAlpha.Position * 0.5;
  DoTransform;
  LoadingValues := True;
  EdtAlpha.Text := FloatToStr(Current.Alpha * 0.5);
  LoadingValues := False;
end;

procedure TFormTranformExample.SkewChanged(Sender: TObject);
var
  Fx, Fy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(EdtFx.Text, Fx) and GetVal(EdtFy.Text, Fy) then
  begin
    Current.Fx := Fx;
    Current.Fy := Fy;
    DoTransform;
    LoadingValues := True;
    GbrFx.Position := Round(Current.Fx * 10);
    GbrFy.Position := Round(Current.Fy * 10);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.SkewScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Fx := GbrFx.Position * 0.1;
  Current.Fy := GbrFy.Position * 0.1;
  DoTransform;
  LoadingValues := True;
  EdtFx.Text := FloatToStr(Current.Fx);
  EdtFy.Text := FloatToStr(Current.Fy);
  LoadingValues := False;
end;

procedure TFormTranformExample.OpacityChange(Sender: TObject);
begin
  OpacityBar.Update;
  Src.Bitmap.MasterAlpha := OpacityBar.Position;
  DoTransform;
end;

procedure TFormTranformExample.InitVertices;
begin
  Vertices[0].X := 0;
  Vertices[0].Y := 0;
  Vertices[1].X := Src.Bitmap.Width - 1;
  Vertices[1].Y := 0;
  Vertices[2].X := Src.Bitmap.Width - 1;
  Vertices[2].Y := Src.Bitmap.Height - 1;
  Vertices[3].X := 0;
  Vertices[3].Y := Src.Bitmap.Height - 1;
end;

procedure TFormTranformExample.PageControlChange(Sender: TObject);
begin
  if Src = nil then
    Exit;
  if PageControl.ActivePage = TstAffine then
  begin
    Mode := tmAffine;
    Transformation := AffineTransformation;
    CmbResamplerClassNames.Parent := TstAffine;
    LblResampler.Parent := TstAffine;
    CmbKernelClassNames.Parent := TstAffine;
    LblKernel.Parent := TstAffine;
  end
  else
  begin
    // set current transformation as projective
    Mode := tmProjective;
    Transformation := ProjectiveTransformation;
    InitVertices;
    CmbResamplerClassNames.Parent := TstProjective;
    LblResampler.Parent := TstProjective;
    CmbKernelClassNames.Parent := TstProjective;
    LblKernel.Parent := TstProjective;
  end;
  DoTransform;
end;

procedure TFormTranformExample.RubberLayerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  I: Integer;
begin
  if Mode = tmAffine then Exit;

  DraggedVertex := -1;

  // find the vertex to drag
  for I := 0 to 3 do
    if (Abs(Vertices[I].X - X) < 3) and (Abs(Vertices[I].Y - Y) < 3) then
    begin
      DraggedVertex := I;
      Break;
    end;

  // or drag all of them, (DragVertex = 4)
  if DraggedVertex = -1 then DraggedVertex := 4;

  // store current mouse position
  LastMousePos := Classes.Point(X, Y);
end;

procedure TFormTranformExample.RubberLayerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  Dx, Dy, I: Integer;
begin
  if Mode = tmAffine then Exit;
  
  if DraggedVertex = -1 then Exit; // mouse is not pressed

  Dx := X - LastMousePos.X;
  Dy := Y - LastMousePos.Y;
  LastMousePos := Classes.Point(X, Y);

  // update coords
  if DraggedVertex = 4 then
  begin
    for I := 0 to 3 do
    begin
      Inc(Vertices[I].X, Dx);
      Inc(Vertices[I].Y, Dy);
    end;
  end
  else
  begin
    Inc(Vertices[DraggedVertex].X, Dx);
    Inc(Vertices[DraggedVertex].Y, Dy);
  end;

  DoTransform;
end;

procedure TFormTranformExample.RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  DraggedVertex := -1;
end;

procedure TFormTranformExample.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if DraggedVertex >= 0 then Exit;
  StippleStart := StippleStart - 0.05;
  Dst.Invalidate;
end;

procedure TFormTranformExample.PaintHandles(Sender: TObject; BackBuffer: TBitmap32);
var
  I, X0, Y0, X1, Y1: Integer;

  procedure PaintVertex(X, Y: Integer);
  begin
    BackBuffer.FillRectS(X - 2, Y - 2, X + 2, Y + 2, clWhite32);
    BackBuffer.FrameRectS(X - 3, Y - 3, X + 3, Y + 3, clBlack32);
  end;

begin
  if PageControl.ActivePage = TstAffine then Exit;

  BackBuffer.SetStipple([clBlack32, clBlack32, clWhite32, clWhite32]);
  BackBuffer.StippleStep := 0.5;
  BackBuffer.StippleCounter := StippleStart;

  X0 := Vertices[3].X;
  Y0 := Vertices[3].Y;
  for I := 0 to 3 do
  begin
    X1 := Vertices[I].X;
    Y1 := Vertices[I].Y;
    BackBuffer.LineFSP(X0, Y0, X1, Y1);
    X0 := X1;
    Y0 := Y1;
  end;
  for I := 0 to 3 do PaintVertex(Vertices[I].X, Vertices[I].Y);
end;

procedure TFormTranformExample.ResamplerClassNamesListClick(Sender: TObject);
begin
  with CmbResamplerClassNames do
    if ItemIndex >= 0 then
      Src.Bitmap.ResamplerClassName:= Items[ ItemIndex ];
  DoTransform;
end;

procedure TFormTranformExample.SrcDblClick(Sender: TObject);
begin
  SrcRubberBandLayer.Location := FloatRect(0, 0, Src.Bitmap.Width - 1,
    Src.Bitmap.Height - 1);
end;

procedure TFormTranformExample.SrcRBResizingEvent(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TRBDragState; Shift: TShiftState);
begin
  Src.Invalidate;
  DoTransform;
end;

procedure TFormTranformExample.CmbResamplerClassNamesChange(Sender: TObject);
var
  R: TCustomResampler;
begin
  with CmbResamplerClassNames do
    if ItemIndex >= 0 then
    begin
      Src.Bitmap.BeginUpdate;
      R := TCustomResamplerClass(ResamplerList[ItemIndex]).Create(Src.Bitmap);

      if CbxRepeat.Checked then
      begin
        Src.Bitmap.WrapMode := wmRepeat;
        Src.Bitmap.Resampler.PixelAccessMode := CAccessMode[CbxRepeat.Checked];
      end;

      CmbKernelClassNamesChange(nil);
      Src.Bitmap.EndUpdate;
      Src.Bitmap.Changed;

      CmbKernelClassNames.Visible := R is TKernelResampler;
      LblKernel.Visible := CmbKernelClassNames.Visible;
    end;
end;

procedure TFormTranformExample.CmbKernelClassNamesChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := CmbKernelClassNames.ItemIndex;
  if Src.Bitmap.Resampler is TKernelResampler then
  begin
    TKernelResampler(Src.Bitmap.Resampler).Kernel := TCustomKernelClass(KernelList[Index]).Create;
  end;
  DoTransform;
end;

procedure TFormTranformExample.DstPaintStage(Sender: TObject; Buffer: TBitmap32;
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
  if Sender is TImage32 then with TImage32(Sender) do
  begin
    BeginUpdate;
    R := GetViewportRect;
    case PaintStages[StageNum].Parameter of
      1: begin //Draw Checkerboard
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
           end
         end;
      2: Buffer.FrameRectS(R , $FF000000); //Draw Frame
    end;
    EndUpdate;
  end;
end;

procedure TFormTranformExample.CbxRepeatClick(Sender: TObject);
begin
  Src.Bitmap.WrapMode := wmRepeat;
  Src.Bitmap.Resampler.PixelAccessMode := CAccessMode[CbxRepeat.Checked];
  DoTransform;
end;

end.
