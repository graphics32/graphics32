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
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Windows, Actions, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls,
  ExtDlgs, StdCtrls, Buttons, Types, ActnList,
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
    OpenDialog: TOpenDialog;
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
    N7: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemCopy: TMenuItem;
    ActionList: TActionList;
    ActionCopy: TAction;
    ActionPasteNew: TAction;
    MenuItemPasteNew: TMenuItem;
    ActionPasteInto: TAction;
    MenuItemPasteInto: TMenuItem;
    ActionSave: TAction;
    Saveas1: TMenuItem;
    TimerMarchingAnts: TTimer;
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
    procedure ActionCopyUpdate(Sender: TObject);
    procedure ActionPasteIntoUpdate(Sender: TObject);
    procedure ActionPasteNewUpdate(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteIntoExecute(Sender: TObject);
    procedure ActionPasteNewExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure TimerMarchingAntsTimer(Sender: TObject);
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
    procedure PaintSimpleDrawingGetUpdateRectHandler(Sender: TObject; var UpdateRect: TRect);
    procedure PaintButtonMockupHandler(Sender: TObject; Buffer: TBitmap32);
  public
    procedure CreateNewImage(AWidth, AHeight: Integer; FillColor: TColor32);
    procedure OpenImage(const FileName: string);

    property Selection: TPositionedLayer read FSelection write SetSelection;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
{$IFDEF Darwin}
  MacOSAll,
{$ENDIF}
  Math, Printers, ClipBrd,
  GR32_LowLevel, GR32_Paths, GR32_VectorUtils, GR32_Backends, GR32_Text_VCL,
  GR32_ColorGradients, GR32_Polygons, GR32_Geometry, GR32_Clipboard,
  GR32.ImageFormats,
  GR32.ImageFormats.JPG,
  GR32.ImageFormats.GIF,
  GR32.ImageFormats.PSD,
  NewImageUnit,
  RGBALoaderUnit;

const
  RESAMPLER: array [Boolean] of TCustomResamplerClass = (TNearestResampler, TDraftResampler);

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Background.FillStyle := bfsCheckers;
  ImgView.MousePan.Enabled := True;
  ImgView.MousePan.PanCursor := crSizeAll;
  ImgView.MouseZoom.Enabled := True;
  ImgView.MouseZoom.Animate := True;
  ImgView.MouseZoom.MaintainPivot := True;
  ImgView.RepaintMode := rmOptimizer;;
  ImgView.Options := ImgView.Options + [pboWantArrowKeys];

  OpenDialog.Filter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True);
  SaveDialog.Filter := ImageFormatManager.BuildFileFilter(IImageFormatWriter, True);
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

procedure TMainForm.ActionSaveExecute(Sender: TObject);
var
  TempBitmap: TBitmap32;
  SaveBitmap: TBitmap32;
begin
  if not SaveDialog.Execute then
    exit;

  // Hide selection
  Selection := nil;

  Screen.Cursor := crHourGlass;
  try
    TempBitmap := nil;
    try
      if (ImgView.Layers.Count > 0) then
      begin
        // Save a flattened copy of the main bitmap
        TempBitmap := TBitmap32.Create;
        TempBitmap.SetSizeFrom(ImgView.Bitmap);

        ImgView.PaintTo(TempBitmap, ImgView.Bitmap.BoundsRect);

        SaveBitmap := TempBitmap;
      end else
        // Save the main bitmap directly
        SaveBitmap := ImgView.Bitmap;

      SaveBitmap.SaveToFile(SaveDialog.FileName);
    finally
      TempBitmap.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
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
  if OpenDialog.Execute then
    OpenImage(OpenDialog.FileName);
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
  if not OpenDialog.Execute then
    exit;

  BitmapLayer := TBitmapLayer.Create(ImgView.Layers);
  try
    BitmapLayer.Bitmap.LoadFromFile(OpenDialog.FileName);
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
  L.OnGetUpdateRect := PaintSimpleDrawingGetUpdateRectHandler;
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
  BorderWidth: Double;
begin
  if not(Sender is TPositionedLayer) then
    exit;

  Layer := TPositionedLayer(Sender);

  Bounds := Layer.GetAdjustedLocation;

  BorderWidth := 0.1 * GbrBorderWidth.Position;

  // Make sure that we stay within bounds.
  // The fill is done inside the bounds rect while the border is centered on the
  // bounds rect. I.e. half of the border is "outside" the bounds rect.
  GR32.InflateRect(Bounds, -(BorderWidth / 2), -(BorderWidth / 2));

  RoundPoly := RoundRect(Bounds, GbrBorderRadius.Position);

  // Button fill
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

  // Button border
  PolyPolygonFS(Buffer,
    BuildPolyPolyLine(PolyPolygon(RoundPoly), True, BorderWidth),
    clGray32, pfAlternate);

  // Button text
  Path := TFlattenedPath.Create;
  try
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

procedure TMainForm.PaintSimpleDrawingGetUpdateRectHandler(Sender: TObject;
  var UpdateRect: TRect);
begin
  // Since we're drawing outside the layer bounds rect, we need to expand the
  // update rect or else the layer repaint mechanism will not be able to update
  // the correct area when the layer is moved or resized.

  // Instead of trying to calculate the precise extent of the figure drawn, we
  // just increase the update rect about 15%.
  GR32.InflateRect(UpdateRect, UpdateRect.Width div 6, UpdateRect.Height div 6);
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
        RBLayer.SetFrameStipple([clWhite32, clWhite32, clWhite32, clWhite32, clBlack32, clBlack32, clBlack32, clBlack32]);
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

procedure TMainForm.TimerMarchingAntsTimer(Sender: TObject);
const
  StippleSize = 8; // The size of our stipple pattern. The default pattern has a size of 4
var
  NewStippleCounter: TFloat;
begin
  if (RBLayer = nil) or (not RBLayer.Visible) then
    exit;

  // Only animate when our application is active
  if (not Application.Active) then
    exit;

  NewStippleCounter := RBLayer.FrameStippleCounter+1.5;

  // Handle overflow
  if (NewStippleCounter >= StippleSize) then
    NewStippleCounter := NewStippleCounter - StippleSize;

  RBLayer.FrameStippleCounter := NewStippleCounter;
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
var
  Size: TSize;
begin
  if Layer = nil then
    Selection := nil;

  if (Button = mbMiddle) then
  begin
    ImgView.BeginUpdate;
    try
      // Reset Zoom...
      ImgView.Scale := 1;

      // ...and Center ImgView
      Size := ImgView.GetBitmapSize;
      ImgView.OffsetHorz := (ImgView.Width-Size.cx) div 2;
      ImgView.OffsetVert := (ImgView.Height-Size.cy) div 2;
    finally
      ImgView.EndUpdate;
    end;
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
