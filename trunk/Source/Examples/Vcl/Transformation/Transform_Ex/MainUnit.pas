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
  {$IFDEF FPC} LResources, {$ENDIF}
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
    Src: TImage32;
    Dst: TImage32;
    OpacityBar: TGaugeBar;
    sbDx: TGaugeBar;
    sbDy: TGaugeBar;
    sbSx: TGaugeBar;
    sbSy: TGaugeBar;
    sbAlpha: TGaugeBar;
    sbFx: TGaugeBar;
    sbFy: TGaugeBar;
    PageControl1: TPageControl;
{$IFDEF FPC}
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Page5: TPage;
{$ENDIF}
    Button1: TButton;
    cbRepeat: TCheckBox;
    CodeString: TEdit;
    ComboBox: TComboBox;
    eAlpha: TEdit;
    eCx: TEdit;
    eCy: TEdit;
    eDx: TEdit;
    eDy: TEdit;
    eFx: TEdit;
    eFy: TEdit;
    eSx: TEdit;
    eSy: TEdit;
    KernelClassNamesList: TComboBox;
    KernelLabel: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox: TListBox;
    Notebook: TNotebook;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ResamplerClassNamesList: TComboBox;
    ResamplerLabel: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    StringGrid: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure OpacityChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure RotationChanged(Sender: TObject);
    procedure RotationScrolled(Sender: TObject);
    procedure ScaleChanged(Sender: TObject);
    procedure ScaleScrolled(Sender: TObject);
    procedure SkewChanged(Sender: TObject);
    procedure SkewScrolled(Sender: TObject);
    procedure TranslationChanged(Sender: TObject);
    procedure TranslationScrolled(Sender: TObject);

    procedure SrcRBResizingEvent(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);

    procedure RubberLayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure RubberLayerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure ResamplerClassNamesListClick(Sender: TObject);
    procedure ResamplerClassNamesListChange(Sender: TObject);
    procedure KernelClassNamesListChange(Sender: TObject);
    procedure DstPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure cbRepeatClick(Sender: TObject);
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
    Vertices: array[0..3] of TPoint;
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
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
{$IFNDEF FPC}
  JPEG,
{$ELSE}
  LazJPG,
{$ENDIF}
  GR32_MediaPathLocator;

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
  MediaPath: TFileName;
begin
  MediaPath := ExpandFileName(GetMediaPath);

  // load example image
  Assert(FileExists(MediaPath + 'delphi.jpg'));
  Src.Bitmap.LoadFromFile(MediaPath + 'delphi.jpg');

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

  ResamplerList.GetClassNames(ResamplerClassNamesList.Items);
  KernelList.GetClassNames(KernelClassNamesList.Items);
  ResamplerClassNamesList.ItemIndex := 0;
  KernelClassNamesList.ItemIndex := 0;

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
  if not cbRepeat.Checked then
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
    CodeString.Text := s;
  end;
  Transformation.SrcRect := SrcRubberBandLayer.Location;
end;

procedure TFormTranformExample.FormDestroy(Sender: TObject);
begin
  AffineTransformation.Free;
  ProjectiveTransformation.Free;
end;

procedure TFormTranformExample.Button1Click(Sender: TObject);
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
  eDx.Text := Format('%.4g', [Current.Dx]);
  eDy.Text := Format('%.4g', [Current.Dy]);
  sbDx.Position := Round(Current.Dx * 10);
  sbDy.Position := Round(Current.Dy * 10);
  eSx.Text := Format('%.4g', [Current.Sx]);
  eSy.Text := Format('%.4g', [Current.Sy]);
  sbSx.Position := Round(Current.Sx * 100);
  sbSy.Position := Round(Current.Sy * 100);
  eCx.Text := Format('%.4g', [Current.Cx]);
  eCy.Text := Format('%.4g', [Current.Cy]);
  eAlpha.Text := Format('%.4g', [Current.Alpha]);
  sbAlpha.Position := Round(Current.Alpha * 2);
  eFx.Text := Format('%.4g', [Current.Fx]);
  eFy.Text := Format('%.4g', [Current.Fy]);
  sbFx.Position := Round(Current.Fx * 100);
  sbFy.Position := Round(Current.Fy * 100);
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
  if GetVal(eDx.Text, Tx) and GetVal(eDy.Text, Ty) then
  begin
    Current.Dx := Tx;
    Current.Dy := Ty;
    DoTransform;
    LoadingValues := True;
    sbDx.Position := Round(Current.Dx * 10);
    sbDy.Position := Round(Current.Dy * 10);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.TranslationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Dx := sbDx.Position * 0.1;
  Current.Dy := sbDy.Position * 0.1;
  DoTransform;
  LoadingValues := True;
  eDx.Text := FloatToStr(Current.Dx);
  eDy.Text := FloatToStr(Current.Dy);
  LoadingValues := False;
end;

procedure TFormTranformExample.ScaleChanged(Sender: TObject);
var
  Sx, Sy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eSx.Text, Sx) and GetVal(eSy.Text, Sy) then
  begin
    Current.Sx := Sx;
    Current.Sy := Sy;
    DoTransform;
    LoadingValues := True;
    sbSx.Position := Round(Current.Sx * 100);
    sbSy.Position := Round(Current.Sy * 100);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.ScaleScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Sx := sbSx.Position * 0.01;
  Current.Sy := sbSy.Position * 0.01;
  DoTransform;
  LoadingValues := True;
  eSx.Text := FloatToStr(Current.Sx);
  eSy.Text := FloatToStr(Current.Sy);
  LoadingValues := False;
end;

procedure TFormTranformExample.RotationChanged(Sender: TObject);
var
  Cx, Cy, Alpha: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eCx.Text, Cx) and GetVal(eCy.Text, Cy) and
    GetVal(eAlpha.Text, Alpha) then
  begin
    Current.Cx := Cx;
    Current.Cy := Cy;
    Current.Alpha := Alpha;
    DoTransform;
    LoadingValues := True;
    sbAlpha.Position := Round(Alpha * 2);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.RotationScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Alpha := sbAlpha.Position * 0.5;
  DoTransform;
  LoadingValues := True;
  eAlpha.Text := FloatToStr(Current.Alpha * 0.5);
  LoadingValues := False;
end;

procedure TFormTranformExample.SkewChanged(Sender: TObject);
var
  Fx, Fy: Extended;
begin
  if LoadingValues then Exit;
  if GetVal(eFx.Text, Fx) and GetVal(eFy.Text, Fy) then
  begin
    Current.Fx := Fx;
    Current.Fy := Fy;
    DoTransform;
    LoadingValues := True;
    sbFx.Position := Round(Current.Fx * 10);
    sbFy.Position := Round(Current.Fy * 10);
    LoadingValues := False;
  end;
end;

procedure TFormTranformExample.SkewScrolled(Sender: TObject);
begin
  if LoadingValues then Exit;
  Current.Fx := sbFx.Position * 0.1;
  Current.Fy := sbFy.Position * 0.1;
  DoTransform;
  LoadingValues := True;
  eFx.Text := FloatToStr(Current.Fx);
  eFy.Text := FloatToStr(Current.Fy);
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

procedure TFormTranformExample.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = TabSheet1 then
  begin
    Mode := tmAffine;
    Transformation := AffineTransformation;
    ResamplerClassNamesList.Parent := TabSheet1;
    ResamplerLabel.Parent := TabSheet1;
    KernelClassNamesList.Parent := TabSheet1;
    KernelLabel.Parent := TabSheet1;
  end
  else
  begin
    // set current transformation as projective
    Mode := tmProjective;
    Transformation := ProjectiveTransformation;
    InitVertices;
    ResamplerClassNamesList.Parent := TabSheet2;
    ResamplerLabel.Parent := TabSheet2;
    KernelClassNamesList.Parent := TabSheet2;
    KernelLabel.Parent := TabSheet2;
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
  if PageControl1.ActivePage = TabSheet1 then Exit;

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
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
      Src.Bitmap.ResamplerClassName:= Items[ ItemIndex ];
  DoTransform;
end;

procedure TFormTranformExample.SrcRBResizingEvent(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TDragState; Shift: TShiftState);
begin
  Src.Invalidate;
  DoTransform;
end;

procedure TFormTranformExample.ResamplerClassNamesListChange(Sender: TObject);
var
  R: TCustomResampler;
begin
  with ResamplerClassNamesList do
    if ItemIndex >= 0 then
    begin
      Src.Bitmap.BeginUpdate;
      R := TCustomResamplerClass(ResamplerList[ItemIndex]).Create(Src.Bitmap);

      if cbRepeat.Checked then
      begin
        Src.Bitmap.WrapMode := wmRepeat;
        Src.Bitmap.Resampler.PixelAccessMode := CAccessMode[cbRepeat.Checked];
      end;

      KernelClassNamesListChange(nil);
      Src.Bitmap.EndUpdate;
      Src.Bitmap.Changed;

      KernelClassNamesList.Visible := R is TKernelResampler;
      KernelLabel.Visible := KernelClassNamesList.Visible;
    end;
end;

procedure TFormTranformExample.KernelClassNamesListChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := KernelClassNamesList.ItemIndex;
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

procedure TFormTranformExample.cbRepeatClick(Sender: TObject);
begin
  Src.Bitmap.WrapMode := wmRepeat;
  Src.Bitmap.Resampler.PixelAccessMode := CAccessMode[cbRepeat.Checked];
  DoTransform;
end;

end.
