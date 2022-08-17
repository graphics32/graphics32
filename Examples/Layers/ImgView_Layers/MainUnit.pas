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
 * The Original Code is Image View Layers Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf <andre@metaexception.de>
 * Christian-W. Budde <Christian@aixcoustic.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls,
  ExtDlgs, StdCtrls, Buttons, Types, Actions, ActnList,
  GR32, GR32_Image, GR32_Layers, GR32_RangeBars,
  GR32_Filters, GR32_Transforms, GR32_Resamplers;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnLayerRescale: TButton;
    BtnLayerResetScale: TButton;
    CbxCropped: TCheckBox;
    CbxImageInterpolate: TCheckBox;
    CbxLayerInterpolate: TCheckBox;
    CbxMagnInterpolate: TCheckBox;
    CbxOptRedraw: TCheckBox;
    GbrBorderRadius: TGaugeBar;
    GbrBorderWidth: TGaugeBar;
    GbrLayerOpacity: TGaugeBar;
    GbrMagnMagnification: TGaugeBar;
    GbrMagnOpacity: TGaugeBar;
    GbrMagnRotation: TGaugeBar;
    ImgView: TImgView32;
    LblBorderRadius: TLabel;
    LblBorderWidth: TLabel;
    LblMagifierOpacity: TLabel;
    LblMagnification: TLabel;
    LblOpacity: TLabel;
    LblRotation: TLabel;
    LblScale: TLabel;
    MainMenu: TMainMenu;
    MimArrange: TMenuItem;
    MnuBringFront: TMenuItem;
    MnuButtonMockup: TMenuItem;
    MnuDelete: TMenuItem;
    MnuFile: TMenuItem;
    MnuFileNew: TMenuItem;
    MnuFileOpen: TMenuItem;
    MnuFlatten: TMenuItem;
    MnuFlipHorz: TMenuItem;
    MnuFlipVert: TMenuItem;
    MnuLayers: TMenuItem;
    MnuLevelDown: TMenuItem;
    MnuLevelUp: TMenuItem;
    MnuMagnifier: TMenuItem;
    MnuNewBitmapLayer: TMenuItem;
    MnuNewBitmapRGBA: TMenuItem;
    MnuNewCustomLayer: TMenuItem;
    MnuPrint: TMenuItem;
    MnuRotate180: TMenuItem;
    MnuRotate270: TMenuItem;
    MnuRotate90: TMenuItem;
    MnuScaled: TMenuItem;
    MnuSendBack: TMenuItem;
    MnuSimpleDrawing: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    PnlBitmapLayer: TPanel;
    PnlBitmapLayerHeader: TPanel;
    PnlButtonMockup: TPanel;
    PnlButtonMockupHeader: TPanel;
    PnlControl: TPanel;
    PnlImage: TPanel;
    PnlImageHeader: TPanel;
    PnlMagnification: TPanel;
    PnlMagnificationHeader: TPanel;
    SaveDialog: TSaveDialog;
    ScaleCombo: TComboBox;
    N7: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemCopy: TMenuItem;
    ActionList: TActionList;
    ActionCopy: TAction;
    ActionPasteNew: TAction;
    MenuItemPasteNew: TMenuItem;
    ActionPasteInto: TAction;
    MenuItemPasteInto: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnLayerRescaleClick(Sender: TObject);
    procedure BtnLayerResetScaleClick(Sender: TObject);
    procedure CbxCroppedClick(Sender: TObject);
    procedure CbxImageInterpolateClick(Sender: TObject);
    procedure CbxLayerInterpolateClick(Sender: TObject);
    procedure CbxOptRedrawClick(Sender: TObject);
    procedure ImgViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewPaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure LayerOpacityChanged(Sender: TObject);
    procedure MimArrangeClick(Sender: TObject);
    procedure MnuButtonMockupClick(Sender: TObject);
    procedure MnuDeleteClick(Sender: TObject);
    procedure MnuFileClick(Sender: TObject);
    procedure MnuFileNewClick(Sender: TObject);
    procedure MnuFileOpenClick(Sender: TObject);
    procedure MnuFlattenClick(Sender: TObject);
    procedure MnuFlipHorzClick(Sender: TObject);
    procedure MnuFlipVertClick(Sender: TObject);
    procedure MnuLayersClick(Sender: TObject);
    procedure MnuMagnifierClick(Sender: TObject);
    procedure MnuNewBitmapLayerClick(Sender: TObject);
    procedure MnuNewBitmapRGBAClick(Sender: TObject);
    procedure MnuPrintClick(Sender: TObject);
    procedure MnuReorderClick(Sender: TObject);
    procedure MnuRotate180Click(Sender: TObject);
    procedure MnuRotate270Click(Sender: TObject);
    procedure MnuRotate90Click(Sender: TObject);
    procedure MnuScaledClick(Sender: TObject);
    procedure MnuSimpleDrawingClick(Sender: TObject);
    procedure PropertyChange(Sender: TObject);
    procedure ScaleComboChange(Sender: TObject);
    procedure ImgViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ActionCopyUpdate(Sender: TObject);
    procedure ActionPasteIntoUpdate(Sender: TObject);
    procedure ActionPasteNewUpdate(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteIntoExecute(Sender: TObject);
    procedure ActionPasteNewExecute(Sender: TObject);
  private
    FPanning: boolean;
    FStartPos: TPoint;
  private
    FLockZoom: integer;
    procedure SetScale(AScale: Double);
  private
    FSelection: TPositionedLayer;
    procedure SetSelection(Value: TPositionedLayer);
  protected
    RBLayer: TRubberbandLayer;
    function CreatePositionedLayer: TPositionedLayer;
    procedure LayerDblClick(Sender: TObject);
    procedure LayerMouseDown(Sender: TObject; Buttons: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RBResizing(Sender: TObject; const OldLocation: TFloatRect;
      var NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState);
    procedure PaintMagnifierHandler(Sender: TObject; Buffer: TBitmap32);
    procedure PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
    procedure PaintButtonMockupHandler(Sender: TObject; Buffer: TBitmap32);
  public
    procedure CreateNewImage(AWidth, AHeight: Integer; FillColor: TColor32);
    procedure OpenImage(const FileName: string);

    property Selection: TPositionedLayer read FSelection write SetSelection;
  end;

var
  MainForm: TMainForm;

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
  Math, Printers, ClipBrd,
  GR32_LowLevel, GR32_Paths, GR32_VectorUtils, GR32_Backends, GR32_Text_VCL,
  GR32_ColorGradients, GR32_Polygons, GR32_Geometry, GR32_Clipboard,
  NewImageUnit, RGBALoaderUnit;

const
  RESAMPLER: array [Boolean] of TCustomResamplerClass = (TNearestResampler, TDraftResampler);

const
  ZoomLevels: array[0..9] of Double = (0.1, 0.25, 0.50, 0.75, 1.0, 2.0, 3.0, 4.0, 8.0, 16.0);

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
  // to call the OnPaintStage event instead of performing default action.
  if (ImgView.PaintStages[0].Stage = PST_CLEAR_BACKGND) then
    ImgView.PaintStages[0].Stage := PST_CUSTOM;

  ImgView.RepaintMode := rmOptimizer;
  ImgView.Options := ImgView.Options + [pboWantArrowKeys];

  // Fill scale combobox with predefined zoom levels
  ScaleCombo.Items.Clear;
  for i := 0 to High(ZoomLevels) do
    ScaleCombo.Items.Add(Format('%.0n%%', [ZoomLevels[i] * 100]));

  SetScale(1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Selection := nil;
  RBLayer := nil;
end;

procedure TMainForm.CreateNewImage(AWidth, AHeight: Integer; FillColor: TColor32);
begin
  Selection := nil;
  RBLayer := nil;

  ImgView.Layers.Clear;
  ImgView.Scale := 1;
  ImgView.Bitmap.SetSize(AWidth, AHeight);
  ImgView.Bitmap.Clear(FillColor);

  pnlImage.Visible := not ImgView.Bitmap.Empty;
end;

function TMainForm.CreatePositionedLayer: TPositionedLayer;
var
  R: TRect;
  P: TPoint;
begin
  // get coordinates of the center of viewport
  R := ImgView.GetViewportRect;
  P := ImgView.ControlToBitmap(R.CenterPoint);

  Result := TPositionedLayer.Create(ImgView.Layers);
  Result.Location := FloatRect(P.X - 32, P.Y - 32, P.X + 32, P.Y + 32);
  Result.Scaled := True;
  Result.MouseEvents := True;
  Result.OnMouseDown := LayerMouseDown;
  Result.OnDblClick := LayerDblClick;
end;

procedure TMainForm.CbxCroppedClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Cropped := CbxCropped.Checked;
end;

procedure TMainForm.CbxImageInterpolateClick(Sender: TObject);
begin
  RESAMPLER[CbxImageInterpolate.Checked].Create(ImgView.Bitmap);
end;

procedure TMainForm.CbxLayerInterpolateClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    RESAMPLER[CbxLayerInterpolate.Checked].Create(TBitmapLayer(Selection).Bitmap);
end;

procedure TMainForm.LayerDblClick(Sender: TObject);
begin
  if Sender is TRubberbandLayer then
    TRubberbandLayer(Sender).Quantize;
end;

procedure TMainForm.LayerMouseDown(Sender: TObject; Buttons: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender <> nil then
    Selection := TPositionedLayer(Sender);
end;

procedure TMainForm.LayerOpacityChanged(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.MasterAlpha := GbrLayerOpacity.Position;
end;

procedure TMainForm.ActionCopyExecute(Sender: TObject);
begin
  if (Selection is TBitmapLayer) then
    Clipboard.Assign(TBitmapLayer(Selection).Bitmap)
  else
    Clipboard.Assign(ImgView.Bitmap);
end;

procedure TMainForm.ActionCopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Selection is TBitmapLayer) or (not ImgView.Bitmap.Empty);
end;

procedure TMainForm.ActionPasteIntoExecute(Sender: TObject);
begin
  if (Selection is TBitmapLayer) then
    TBitmapLayer(Selection).Bitmap.Assign(Clipboard)
  else
    ImgView.Bitmap.Assign(Clipboard);
end;

procedure TMainForm.ActionPasteIntoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := CanPasteBitmap32;
end;

procedure TMainForm.ActionPasteNewExecute(Sender: TObject);
var
  BitmapLayer: TBitmapLayer;
  R: TRect;
  P: TPoint;
  W, H: Single;
begin
  BitmapLayer := TBitmapLayer.Create(ImgView.Layers);
  try
    BitmapLayer.Bitmap.Assign(Clipboard);
    BitmapLayer.Bitmap.DrawMode := dmBlend;

    R := ImgView.GetViewportRect;
    P := ImgView.ControlToBitmap(R.CenterPoint);

    W := BitmapLayer.Bitmap.Width * 0.5;
    H := BitmapLayer.Bitmap.Height * 0.5;

    with ImgView.Bitmap do
      BitmapLayer.Location := GR32.FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

    BitmapLayer.Scaled := True;
    BitmapLayer.OnMouseDown := LayerMouseDown;
  except
    BitmapLayer.Free;
    raise;
  end;
  Selection := BitmapLayer;
end;

procedure TMainForm.ActionPasteNewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := CanPasteBitmap32;
end;

procedure TMainForm.BtnLayerRescaleClick(Sender: TObject);
var
  T: TBitmap32;
  BitmapLayer: TBitmapLayer;
  R: TRect;
begin
  // resize the layer's bitmap to the size of the layer
  if Selection is TBitmapLayer then
  begin
    BitmapLayer := TBitmapLayer(Selection);

    T := TBitmap32.Create;
    try
      T.Assign(BitmapLayer.Bitmap);

      R := MakeRect(BitmapLayer.Location);
      BitmapLayer.Bitmap.SetSize(R.Width, R.Height);

      T.Resampler := TNearestResampler.Create(T);
      T.DrawMode := dmOpaque;

      T.DrawTo(BitmapLayer.Bitmap, Classes.Rect(0, 0, BitmapLayer.Bitmap.Width, BitmapLayer.Bitmap.Height));
    finally
      T.Free;
    end;
    BtnLayerResetScaleClick(Self);
  end;

  ImgView.GetBitmapRect
end;

procedure TMainForm.BtnLayerResetScaleClick(Sender: TObject);
var
  L: TFloatRect;
begin
  // resize the layer to the size of its bitmap
  if Selection is TBitmapLayer then
  begin
    L := RBLayer.Location;
    L.Right := L.Left + TBitmapLayer(Selection).Bitmap.Width;
    L.Bottom := L.Top + TBitmapLayer(Selection).Bitmap.Height;
    RBLayer.Location := L;
    RBLayer.Changed;
  end;
end;

procedure TMainForm.PropertyChange(Sender: TObject);
begin
  ImgView.Invalidate;
end;

procedure TMainForm.MimArrangeClick(Sender: TObject);
var
  HasSelection: Boolean;
begin
  HasSelection := (Selection <> nil);

  MnuBringFront.Enabled := HasSelection and (Selection.Index < ImgView.Layers.Count - 2);
  MnuSendBack.Enabled := HasSelection and (Selection.Index > 0);
  MnuLevelUp.Enabled := HasSelection and (Selection.Index < ImgView.Layers.Count - 2);
  MnuLevelDown.Enabled := HasSelection and (Selection.Index > 0);
  MnuScaled.Enabled := HasSelection;
  MnuScaled.Checked := HasSelection and Selection.Scaled;
  MnuDelete.Enabled := HasSelection;
  HasSelection := HasSelection and (Selection is TBitmapLayer);
  MnuFlipHorz.Enabled := HasSelection;
  MnuFlipVert.Enabled := HasSelection;
  MnuRotate90.Enabled := HasSelection;
  MnuRotate180.Enabled := HasSelection;
  MnuRotate270.Enabled := HasSelection;
end;

procedure TMainForm.MnuButtonMockupClick(Sender: TObject);
var
  L: TPositionedLayer;
begin
  L := CreatePositionedLayer;
  L.OnPaint := PaintButtonMockupHandler;
  L.Tag := 2;
  Selection := L;
end;

procedure TMainForm.MnuDeleteClick(Sender: TObject);
var
  ALayer: TPositionedLayer;
begin
  if Selection <> nil then
  begin
    ALayer := Selection;
    Selection := nil;
    ALayer.Free;
  end;
end;

procedure TMainForm.MnuFileNewClick(Sender: TObject);
begin
  FrmNewImage.ShowModal;

  if FrmNewImage.ModalResult <> mrOK then
    exit;

  CreateNewImage(FrmNewImage.BtnUpDownWidth.Position, FrmNewImage.BtnUpDownHeight.Position,
    Color32(FrmNewImage.PnlColor.Color));
end;

procedure TMainForm.MnuFileOpenClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    OpenImage(OpenPictureDialog.FileName);
end;

procedure TMainForm.MnuLayersClick(Sender: TObject);
var
  HasBitmap: Boolean;
begin
  HasBitmap := not ImgView.Bitmap.Empty;

  MnuNewBitmapLayer.Enabled := HasBitmap;
  MnuNewBitmapRGBA.Enabled := HasBitmap;
  MnuNewCustomLayer.Enabled := HasBitmap;
  MnuFlatten.Enabled := HasBitmap and (ImgView.Layers.Count > 0);
end;

procedure TMainForm.MnuMagnifierClick(Sender: TObject);
var
  L: TPositionedLayer;
begin
  L := CreatePositionedLayer;
  L.OnPaint := PaintMagnifierHandler;
  L.Tag := 3;
  Selection := L;
end;

procedure TMainForm.MnuNewBitmapLayerClick(Sender: TObject);
var
  BitmapLayer: TBitmapLayer;
  R: TRect;
  P: TPoint;
  W, H: Single;
begin
  if not OpenPictureDialog.Execute then
    exit;

  BitmapLayer := TBitmapLayer.Create(ImgView.Layers);
  try
    BitmapLayer.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    BitmapLayer.Bitmap.DrawMode := dmBlend;

    R := ImgView.GetViewportRect;
    P := ImgView.ControlToBitmap(R.CenterPoint);

    W := BitmapLayer.Bitmap.Width * 0.5;
    H := BitmapLayer.Bitmap.Height * 0.5;

    with ImgView.Bitmap do
      BitmapLayer.Location := GR32.FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

    BitmapLayer.Scaled := True;
    BitmapLayer.OnMouseDown := LayerMouseDown;
  except
    BitmapLayer.Free;
    raise;
  end;
  Selection := BitmapLayer;
end;

procedure TMainForm.MnuNewBitmapRGBAClick(Sender: TObject);
var
  BitmapLayer: TBitmapLayer;
  R: TRect;
  P: TPoint;
  Tmp: TBitmap32;
  W, H: Single;
begin
  RGBALoaderForm.ImgRGB.Bitmap.Delete;
  RGBALoaderForm.ImgRGB.Scale := 1;
  RGBALoaderForm.ImgAlpha.Bitmap.Delete;
  RGBALoaderForm.ImgAlpha.Scale := 1;
  RGBALoaderForm.ShowModal;
  if (RGBALoaderForm.ModalResult <> mrOK) then
    exit;

  if (RGBALoaderForm.ImgRGB.Bitmap.Empty) then
    exit;

  BitmapLayer := TBitmapLayer.Create(ImgView.Layers);
  BitmapLayer.Bitmap := RGBALoaderForm.ImgRGB.Bitmap;
  BitmapLayer.Bitmap.DrawMode := dmBlend;

  if not RGBALoaderForm.ImgAlpha.Bitmap.Empty then
  begin
    Tmp := TBitmap32.Create;
    try
      Tmp.SetSize(BitmapLayer.Bitmap.Width, BitmapLayer.Bitmap.Height);
      RGBALoaderForm.ImgAlpha.Bitmap.DrawTo(Tmp, Classes.Rect(0, 0, Tmp.Width, Tmp.Height));

      // combine Alpha into already loaded RGB colors
      IntensityToAlpha(BitmapLayer.Bitmap, Tmp);
    finally
      Tmp.Free;
    end;
  end;

  R := ImgView.GetViewportRect;
  P := ImgView.ControlToBitmap(R.CenterPoint);

  W := BitmapLayer.Bitmap.Width * 0.5;
  H := BitmapLayer.Bitmap.Height * 0.5;

  BitmapLayer.Location := GR32.FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

  BitmapLayer.Scaled := True;
  BitmapLayer.OnMouseDown := LayerMouseDown;

  Selection := BitmapLayer;
end;

procedure TMainForm.MnuReorderClick(Sender: TObject);
begin
  // note that the top-most layer is occupied with the rubber-banding layer
  if Selection = nil then
    exit;

  case TMenuItem(Sender).Tag of
    1: // Bring to front, do not use BringToFront here, see note above
      Selection.Index := ImgView.Layers.Count - 2;
    2: Selection.SendToBack;
    3: Selection.Index := Selection.Index + 1; // up one level
    4: Selection.Index := Selection.Index - 1; // down one level
  end;
end;

procedure TMainForm.MnuSimpleDrawingClick(Sender: TObject);
var
  L: TPositionedLayer;
begin
  L := CreatePositionedLayer;
  L.OnPaint := PaintSimpleDrawingHandler;
  L.Tag := 1;
  Selection := L;
end;

procedure TMainForm.OpenImage(const FileName: string);
begin
  try
    Selection := nil;
    RBLayer := nil;
    ImgView.Layers.Clear;
    ImgView.Scale := 1;
    ImgView.Bitmap.LoadFromFile(FileName);
  finally
    pnlImage.Visible := not ImgView.Bitmap.Empty;
  end;
end;

procedure TMainForm.PaintButtonMockupHandler(Sender: TObject;
  Buffer: TBitmap32);
var
  Layer: TPositionedLayer;
  RoundPoly: TArrayOfFloatPoint;
  TextPoly: TArrayOfArrayOfFloatPoint;
  Bounds, Dst: TFloatRect;
  Path: TFlattenedPath;
  Intf: ITextToPathSupport;
  ColorGradient: TLinearGradientPolygonFiller;
const
  CScale = 1 / 200;
begin
  if not(Sender is TPositionedLayer) then
    exit;

  Layer := TPositionedLayer(Sender);

  Bounds := Layer.GetAdjustedLocation;
  InflateRect(Bounds, -1, -1);
  RoundPoly := RoundRect(Bounds, GbrBorderRadius.Position);

  ColorGradient := TLinearGradientPolygonFiller.Create;
  try
    ColorGradient.SetPoints(FloatPoint(0, Bounds.Top), FloatPoint(0, Bounds.Bottom));
    ColorGradient.Gradient.StartColor := $FFE2E2E2;
    ColorGradient.Gradient.AddColorStop(0.499, $FFD3D3D3);
    ColorGradient.Gradient.AddColorStop(0.501, $FFDBDBDB);
    ColorGradient.Gradient.EndColor := $FFFDFDFD;

    PolygonFS(Buffer, RoundPoly, ColorGradient, pfAlternate);
  finally
    ColorGradient.Free;
  end;
  PolyPolygonFS(Buffer, BuildPolyPolyLine(PolyPolygon(RoundPoly), True,
    0.1 * GbrBorderWidth.Position), clGray32, pfAlternate);

  Path := TFlattenedPath.Create;
  try
//    Buffer.Font.Assign(FFont);
    Buffer.Font.Size := 12;
    if Supports(Buffer.Backend, ITextToPathSupport, Intf) then
    begin
      Intf.TextToPath(Path, 0, 0, 'Button');
      TextPoly := Path.Path;
      if Length(TextPoly) > 0 then
      begin
        Dst := PolypolygonBounds(TextPoly);
        TextPoly := TranslatePolyPolygon(TextPoly,
          0.5 * (Bounds.Left + Bounds.Right - (Dst.Right - Dst.Left)),
          0.5 * (Bounds.Bottom + Bounds.Top - Dst.Bottom));

        PolyPolygonFS_LCD2(Buffer, TextPoly, clBlack32, pfAlternate);
      end;
    end;
  finally
    Path.Free;
  end;
end;

procedure TMainForm.PaintMagnifierHandler(Sender: TObject; Buffer: TBitmap32);
var
  Layer: TPositionedLayer;
  Magnification, Rotation: Single;
  SrcRect, DstRect: TFloatRect;
  R: TRect;
  T: TAffineTransformation;
  B: TBitmap32;
  W2, H2: Single;
  I: Integer;
begin
  if not(Sender is TPositionedLayer) then
    exit;

  Layer := TPositionedLayer(Sender);

  DstRect := Layer.GetAdjustedLocation;
  R := MakeRect(DstRect);

  if not Buffer.MeasuringMode then
  begin
    Magnification := Power(10, (GbrMagnMagnification.Position * 0.02));
    Rotation := -GbrMagnRotation.Position;

    B := TBitmap32.Create;
    try
      B.SetSize(R.Width, R.Height);
      W2 := R.Width * 0.5;
      H2 := R.Height * 0.5;

      SrcRect := DstRect;
      SrcRect.Left := SrcRect.Left - H2;
      SrcRect.Right := SrcRect.Right + H2;
      SrcRect.Top := SrcRect.Top - W2;
      SrcRect.Bottom := SrcRect.Bottom + W2;

      T := TAffineTransformation.Create;
      try
        T.SrcRect := SrcRect;
        T.Translate(-R.Left, -R.Top);

        T.Translate(-W2, -H2);
        T.Scale(Magnification, Magnification);
        T.Rotate(0, 0, Rotation);
        T.Translate(W2, H2);

        if CbxMagnInterpolate.Checked then
        begin
          TLinearResampler.Create(Buffer);
          Transform(B, Buffer, T);
        end
        else
        begin
          TNearestResampler.Create(Buffer);
          Transform(B, Buffer, T);
        end;

        B.ResetAlpha;
        B.DrawMode := dmBlend;
        B.MasterAlpha := GbrMagnOpacity.Position;
        B.DrawTo(Buffer, R);

        // draw frame
        for I := 0 to 4 do
        begin
          Buffer.RaiseRectTS(R, 35 - I * 8);
          GR32.InflateRect(R, -1, -1);
        end;
      finally
        T.Free;
      end;
    finally
      B.Free;
    end;
  end;
  Buffer.Changed;
end;

procedure TMainForm.PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
var
  Layer: TPositionedLayer;
  R: TFloatRect;
  Cx, Cy: Single;
  W2, H2: Single;
  I: Integer;
const
  CScale = 1 / 200;
begin
  if not(Sender is TPositionedLayer) then
    exit;

  Layer := TPositionedLayer(Sender);

  R := Layer.GetAdjustedLocation;

  W2 := (R.Right - R.Left) * 0.5;
  H2 := (R.Bottom - R.Top) * 0.5;
  Cx := R.Left + W2;
  Cy := R.Top + H2;
  W2 := W2 * CScale;
  H2 := H2 * CScale;
  Buffer.PenColor := clRed32;

  Buffer.MoveToF(Cx, Cy);
  for I := 0 to 240 do
    Buffer.LineToFS(
      Cx + W2 * I * Cos(I * 0.125),
      Cy + H2 * I * Sin(I * 0.125));
end;

procedure TMainForm.ScaleComboChange(Sender: TObject);
var
  NewScale: Double;
  ScaleStr: string;
  i: integer;
begin
  if (ScaleCombo.ItemIndex <> -1) then
  begin
    // Predefined scale selected
    NewScale := ZoomLevels[ScaleCombo.ItemIndex];
  end else
  begin
    // Custom zoom manually entered
    ScaleStr := ScaleCombo.Text;

    // Remove junk from start
    while (ScaleStr <> '') and ((ScaleStr[1] < '0') or (ScaleStr[1] > '9')) do
      Delete(ScaleStr, 1, 1);

    // Remove junk from end
    i := 1;
    while (i <= Length(ScaleStr)) and ((ScaleStr[i] >= '0') or (ScaleStr[i] <= '9')) do
      inc(i);
    SetLength(ScaleStr, i-1);

    NewScale := StrToFloatDef(ScaleStr, 100) / 100;
  end;
  SetScale(NewScale);
end;

procedure TMainForm.SetScale(AScale: Double);
begin
  if (FLockZoom > 0) then
    exit;

  if AScale < ZoomLevels[Low(ZoomLevels)] then
    AScale := ZoomLevels[Low(ZoomLevels)]
  else
  if AScale > ZoomLevels[High(ZoomLevels)] then
    AScale := ZoomLevels[High(ZoomLevels)];

  Inc(FLockZoom);
  try

    ImgView.Scale := AScale;

    ScaleCombo.Text := Format('%.0n%%', [ImgView.Scale * 100]);
    ScaleCombo.SelStart := Length(ScaleCombo.Text) - 1;

  finally
    Dec(FLockZoom);
  end;
end;

procedure TMainForm.SetSelection(Value: TPositionedLayer);
begin
  if Value <> FSelection then
  begin
    if RBLayer <> nil then
    begin
      RBLayer.ChildLayer := nil;
      RBLayer.LayerOptions := LOB_NO_UPDATE;
      pnlBitmapLayer.Visible := False;
      pnlButtonMockup.Visible := False;
      pnlMagnification.Visible := False;
      ImgView.Invalidate;
    end;

    FSelection := Value;

    if Value <> nil then
    begin
      if RBLayer = nil then
      begin
        RBLayer := TRubberBandLayer.Create(ImgView.Layers);
        RBLayer.MinHeight := 1;
        RBLayer.MinWidth := 1;
      end
      else
        RBLayer.BringToFront;
      RBLayer.ChildLayer := Value;
      RBLayer.LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS or LOB_NO_UPDATE;
      RBLayer.OnResizing := RBResizing;
      RBLayer.OnDblClick := LayerDblClick;

      if Value is TBitmapLayer then
      begin
        pnlBitmapLayer.Visible := True;
        GbrLayerOpacity.Position := TBitmapLayer(Value).Bitmap.MasterAlpha;
        CbxLayerInterpolate.Checked := (TBitmapLayer(Value).Bitmap.Resampler.ClassType = TDraftResampler);
      end else
      if Value.Tag = 2 then
      begin
        // tag = 2 for button mockup
        pnlButtonMockup.Visible := True;
      end else
      if Value.Tag = 3 then
      begin
        // tag = 3 for magnifiers
        pnlMagnification.Visible := True;
      end;
    end;
  end;
end;

procedure TMainForm.MnuScaledClick(Sender: TObject);
begin
  if Selection <> nil then
    Selection.Scaled := not Selection.Scaled;
  RBLayer.Scaled := Selection.Scaled;
end;

procedure TMainForm.ImgViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Location: TFloatRect;
begin
  if (FSelection = nil) then
    exit;

  case Key of
    VK_LEFT:
      begin
        Location := OffsetRect(FSelection.Location, -1, 0);
        FSelection.Location := Location;
        RBLayer.Location := Location;
      end;

    VK_RIGHT:
      begin
        Location := OffsetRect(FSelection.Location, 1, 0);
        FSelection.Location := Location;
        RBLayer.Location := Location;
      end;

    VK_UP:
      begin
        Location := OffsetRect(FSelection.Location, 0, -1);
        FSelection.Location := Location;
        RBLayer.Location := Location;
      end;

    VK_DOWN:
      begin
        Location := OffsetRect(FSelection.Location, 0, 1);
        FSelection.Location := Location;
        RBLayer.Location := Location;
      end;

    VK_DELETE:
      begin
        FreeAndNil(FSelection);
        RBLayer.ChildLayer := nil;
        RBLayer.LayerOptions := LOB_NO_UPDATE;
      end;
  end;
end;

procedure TMainForm.ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Layer = nil then
  begin
    Selection := nil;

    // Left mouse = Pan
    if (Button = mbLeft) then
    begin
      FPanning := True;
      ImgView.Cursor := crSizeAll;
      // Remember start point
      FStartPos := GR32.Point(X, Y);
    end else
    // Middle mouse = Reset zoom to 100%
    if (Button = mbMiddle) then
      SetScale(1);
  end;
end;

procedure TMainForm.ImgViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Delta: TPoint;
begin
  if (not FPanning) then
    Exit;

  // If we're panning then calculate how far mouse has moved since last and
  // scroll image the same amount.
  var NewPos := GR32.Point(X, Y);
  Delta := FStartPos - NewPos;

  FStartPos := NewPos; // Remember new start point

  if (Delta.X <> 0) or (Delta.Y <> 0) then
    ImgView.Scroll(Delta.X, Delta.Y);
end;

procedure TMainForm.ImgViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  ImgView.Cursor := crDefault;
  FPanning := False;
end;

procedure TMainForm.ImgViewPaintStage(Sender: TObject; Buffer: TBitmap32;
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
  TileHeight := 13;
  TileWidth := 13;

  TilesHorz := Buffer.Width div TileWidth;
  TilesVert := Buffer.Height div TileHeight;
  TileY := 0;

  for J := 0 to TilesVert do
  begin
    TileX := 0;
    OddY := J and $1;
    for I := 0 to TilesHorz do
    begin
      R.Left := TileX;
      R.Top := TileY;
      R.Right := TileX + TileWidth;
      R.Bottom := TileY + TileHeight;
      Buffer.FillRectS(R, Colors[I and $1 = OddY]);
      Inc(TileX, TileWidth);
    end;
    Inc(TileY, TileHeight);
  end;
end;

procedure TMainForm.RBResizing(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TRBDragState; Shift: TShiftState);
var
  w, h, cx, cy: Single;
  nw, nh: Single;

begin
  if DragState = dsMove then
    Exit; // we are interested only in scale operations
  if Shift = [] then
    Exit; // special processing is not required

  if ssCtrl in Shift then
  begin
    { make changes symmetrical }

    with OldLocation do
    begin
      cx := (Left + Right) / 2;
      cy := (Top + Bottom) / 2;
      w := Right - Left;
      h := Bottom - Top;
    end;

    with NewLocation do
    begin
      nw := w / 2;
      nh := h / 2;
      case DragState of
        dsSizeL: nw := cx - Left;
        dsSizeT: nh := cy - Top;
        dsSizeR: nw := Right - cx;
        dsSizeB: nh := Bottom - cy;
        dsSizeTL: begin nw := cx - Left; nh := cy - Top; end;
        dsSizeTR: begin nw := Right - cx; nh := cy - Top; end;
        dsSizeBL: begin nw := cx - Left; nh := Bottom - cy; end;
        dsSizeBR: begin nw := Right - cx; nh := Bottom - cy; end;
      end;
      if nw < 2 then nw := 2;
      if nh < 2 then nh := 2;

      Left := cx - nw;
      Right := cx + nw;
      Top := cy - nh;
      Bottom := cy + nh;
    end;
  end;
end;

procedure TMainForm.MnuFlattenClick(Sender: TObject);
var
  B: TBitmap32;
  W, H: Integer;
begin
  { deselect everything }
  Selection := nil;
  W := ImgView.Bitmap.Width;
  H := ImgView.Bitmap.Height;

  { Create a new bitmap to store a flattened image }
  B := TBitmap32.Create;
  try
    B.SetSize(W, H);
    ImgView.PaintTo(B, Classes.Rect(0, 0, W, H));

    { destroy all the layers of the original image... }
    ImgView.Layers.Clear;
    RBLayer := nil; // note that RBLayer reference is destroyed here as well.
                    // The rubber band will be recreated during the next
                    // SetSelection call. Alternatively, you can delete
                    // all the layers except the rubber band.

    { ...and overwrite it with the flattened one }
    ImgView.Bitmap := B;
  finally
    B.Free;
  end;
end;

procedure TMainForm.MnuPrintClick(Sender: TObject);
var
  B: TBitmap32;
  W, H: Integer;
  R: TRect;

  function GetCenteredRectToFit(const src, dst: TRect): TRect;
  var
    srcWidth, srcHeight, dstWidth, dstHeight, ScaledSide: Integer;
  begin
    with src do begin
      srcWidth := Right - Left;
      srcHeight := Bottom - Top;
    end;
    with dst do begin
      dstWidth := Right - Left;
      dstHeight := Bottom - Top;
    end;
    if (srcWidth = 0) or (srcHeight = 0) then exit;
    if srcWidth / srcHeight > dstWidth / dstHeight then begin
      ScaledSide := Round(dstWidth * srcHeight / srcWidth);
      with Result do begin
        Left := dst.Left;
        Top := dst.Top + (dstHeight - ScaledSide) div 2;
        Right := dst.Right;
        Bottom := Top + ScaledSide;
      end;
    end else begin
      ScaledSide := Round(dstHeight * srcWidth / srcHeight);
      with Result do begin
        Left := dst.Left + (dstWidth - ScaledSide) div 2;
        Top := dst.Top;
        Right := Left + ScaledSide;
        Bottom := dst.Bottom;
      end;
    end;
  end;

begin
  { deselect everything }
  Selection := nil;
  W := ImgView.Bitmap.Width;
  H := ImgView.Bitmap.Height;

  { Create a new bitmap to store a flattened image }
  B := TBitmap32.Create;
  Screen.Cursor := crHourGlass;
  try
    B.SetSize(W, H);
    ImgView.PaintTo(B, Classes.Rect(0, 0, W, H));
    Printer.BeginDoc;
    Printer.Title := 'Image View Layers Example';
    B.Resampler := TLinearResampler.Create(B);
    R := GetCenteredRectToFit(Classes.Rect(0, 0, W, H), Classes.Rect(0, 0, Printer.PageWidth, Printer.PageHeight));
    B.TileTo(Printer.Canvas.Handle, R, Classes.Rect(0, 0, W, H));
    Printer.EndDoc;
  finally
    B.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ImgViewMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  // -10%
  SetScale(ImgView.Scale / 1.1);
end;

procedure TMainForm.ImgViewMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  // +10%
  SetScale(ImgView.Scale * 1.1);
end;

procedure TMainForm.MnuFlipHorzClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.FlipHorz;
end;

procedure TMainForm.MnuFlipVertClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.FlipVert;
end;

procedure TMainForm.MnuRotate90Click(Sender: TObject);
var
  R: TFloatRect;
  Cx, Cy, W2, H2: Single;
begin
  if Selection is TBitmapLayer then
  begin
    R := Selection.Location;
    TBitmapLayer(Selection).Bitmap.Rotate90;
    Cx := (R.Left + R.Right) * 0.5;
    Cy := (R.Top + R.Bottom) * 0.5;
    W2 := (R.Right - R.Left) * 0.5;
    H2 := (R.Bottom - R.Top) * 0.5;
    RBLayer.Location := FloatRect(Cx - H2, Cy - W2, Cx + H2, Cy + W2);
  end;
end;

procedure TMainForm.MnuRotate180Click(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.Rotate180;
end;

procedure TMainForm.MnuRotate270Click(Sender: TObject);
var
  R: TFloatRect;
  Cx, Cy, W2, H2: Single;
begin
  if Selection is TBitmapLayer then
  begin
    R := Selection.Location;
    TBitmapLayer(Selection).Bitmap.Rotate270;
    Cx := (R.Left + R.Right) * 0.5;
    Cy := (R.Top + R.Bottom) * 0.5;
    W2 := (R.Right - R.Left) * 0.5;
    H2 := (R.Bottom - R.Top) * 0.5;
    RBLayer.Location := FloatRect(Cx - H2, Cy - W2, Cx + H2, Cy + W2);
  end;
end;

procedure TMainForm.MnuFileClick(Sender: TObject);
begin
  MnuPrint.Enabled := not ImgView.Bitmap.Empty;
end;

procedure TMainForm.CbxOptRedrawClick(Sender: TObject);
const
  RepaintMode: array[Boolean] of TRepaintMode = (rmFull, rmOptimizer);
begin
  ImgView.RepaintMode := RepaintMode[CbxOptRedraw.Checked];
end;

end.
