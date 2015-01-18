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
  ExtDlgs, StdCtrls, Buttons, GR32, GR32_Image, GR32_Layers, GR32_RangeBars,
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
  NewImageUnit, RGBALoaderUnit, Math, Printers, GR32_LowLevel, GR32_Paths,
  GR32_VectorUtils, GR32_Backends, GR32_Text_VCL, GR32_ColorGradients,
  GR32_Polygons, GR32_Geometry;

const
  RESAMPLER: array [Boolean] of TCustomResamplerClass = (TNearestResampler, TDraftResampler);

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // by default, PST_CLEAR_BACKGND is executed at this stage,
  // which, in turn, calls ExecClearBackgnd method of ImgView.
  // Here I substitute PST_CLEAR_BACKGND with PST_CUSTOM, so force ImgView
  // to call the OnPaintStage event instead of performing default action.
  with ImgView.PaintStages[0]^ do
  begin
    if Stage = PST_CLEAR_BACKGND then Stage := PST_CUSTOM;
  end;

  ImgView.RepaintMode := rmOptimizer;
  ImgView.Options := ImgView.Options + [pboWantArrowKeys];
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Selection := nil;
  RBLayer := nil;
end;

procedure TMainForm.CreateNewImage(AWidth, AHeight: Integer; FillColor: TColor32);
begin
  with ImgView do
  begin
    Selection := nil;
    RBLayer := nil;
    Layers.Clear;
    Scale := 1;
    Bitmap.SetSize(AWidth, AHeight);
    Bitmap.Clear(FillColor);
    pnlImage.Visible := not Bitmap.Empty;
  end;
end;

function TMainForm.CreatePositionedLayer: TPositionedLayer;
var
  P: TPoint;
begin
  // get coordinates of the center of viewport
  with ImgView.GetViewportRect do
    P := ImgView.ControlToBitmap(GR32.Point((Right + Left) div 2, (Top + Bottom) div 2));

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
  begin
    RESAMPLER[CbxLayerInterpolate.Checked].Create(TBitmapLayer(Selection).Bitmap);
  end;
end;

procedure TMainForm.LayerDblClick(Sender: TObject);
begin
  if Sender is TRubberbandLayer then
    TRubberbandLayer(Sender).Quantize;
end;

procedure TMainForm.LayerMouseDown(Sender: TObject; Buttons: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender <> nil then Selection := TPositionedLayer(Sender);
end;

procedure TMainForm.LayerOpacityChanged(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.MasterAlpha := GbrLayerOpacity.Position;
end;

procedure TMainForm.BtnLayerRescaleClick(Sender: TObject);
var
  T: TBitmap32;
begin
  // resize the layer's bitmap to the size of the layer
  if Selection is TBitmapLayer then
    with TBitmapLayer(Selection) do
    begin
      T := TBitmap32.Create;
      T.Assign(Bitmap);
      with MakeRect(Location) do
        Bitmap.SetSize(Right - Left, Bottom - Top);
      T.Resampler := TNearestResampler.Create(T);
      T.DrawMode := dmOpaque;
      T.DrawTo(Bitmap, Classes.Rect(0, 0, Bitmap.Width, Bitmap.Height));
      T.Free;
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
    with RBLayer, TBitmapLayer(Selection).Bitmap do
    begin
      L := Location;
      L.Right := L.Left + Width;
      L.Bottom := L.Top + Height;
      Location := L;
      Changed;
    end;
end;

procedure TMainForm.PropertyChange(Sender: TObject);
begin
  ImgView.Invalidate;
end;

procedure TMainForm.MimArrangeClick(Sender: TObject);
var
  B: Boolean;
begin
  B := Selection <> nil;
  MnuBringFront.Enabled := B and (Selection.Index < ImgView.Layers.Count - 2);
  MnuSendBack.Enabled := B and (Selection.Index > 0);
  MnuLevelUp.Enabled := B and (Selection.Index < ImgView.Layers.Count - 2);
  MnuLevelDown.Enabled := B and (Selection.Index > 0);
  MnuScaled.Enabled := B;
  MnuScaled.Checked := B and Selection.Scaled;
  MnuDelete.Enabled := B;
  B := B and (Selection is TBitmapLayer);
  MnuFlipHorz.Enabled := B;
  MnuFlipVert.Enabled := B;
  MnuRotate90.Enabled := B;
  MnuRotate180.Enabled := B;
  MnuRotate270.Enabled := B;
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
  with FrmNewImage do
  begin
    ShowModal;
    if ModalResult = mrOK then
      CreateNewImage(BtnUpDownWidth.Position, BtnUpDownHeight.Position,
        Color32(PnlColor.Color));
  end;
end;

procedure TMainForm.MnuFileOpenClick(Sender: TObject);
begin
  with OpenPictureDialog do
    if Execute then OpenImage(FileName);
end;

procedure TMainForm.MnuLayersClick(Sender: TObject);
var
  B: Boolean;
begin
  B := not ImgView.Bitmap.Empty;
  MnuNewBitmapLayer.Enabled := B;
  MnuNewBitmapRGBA.Enabled := B;
  MnuNewCustomLayer.Enabled := B;
  MnuFlatten.Enabled := B and (ImgView.Layers.Count > 0);
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
  B: TBitmapLayer;
  P: TPoint;
  W, H: Single;
begin
  with OpenPictureDialog do
    if Execute then
    begin
      B := TBitmapLayer.Create(ImgView.Layers);
      with B do
      try
        Bitmap.LoadFromFile(FileName);
        Bitmap.DrawMode := dmBlend;

        with ImgView.GetViewportRect do
          P := ImgView.ControlToBitmap(GR32.Point((Right + Left) div 2, (Top + Bottom) div 2));

        W := Bitmap.Width * 0.5;
        H := Bitmap.Height * 0.5;

        with ImgView.Bitmap do
          Location := GR32.FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

        Scaled := True;
        OnMouseDown := LayerMouseDown;
      except
        Free;
        raise;
      end;
      Selection := B;
    end;
end;

procedure TMainForm.MnuNewBitmapRGBAClick(Sender: TObject);
var
  B: TBitmapLayer;
  P: TPoint;
  Tmp: TBitmap32;
  W, H: Single;
begin
  with RGBALoaderForm do
  begin
    ImgRGB.Bitmap.Delete;
    ImgRGB.Scale := 1;
    ImgAlpha.Bitmap.Delete;
    ImgAlpha.Scale := 1;
    ShowModal;
    if (ModalResult = mrOK) and not ImgRGB.Bitmap.Empty then
    begin
      B := TBitmapLayer.Create(ImgView.Layers);
      B.Bitmap := ImgRGB.Bitmap;
      B.Bitmap.DrawMode := dmBlend; 

      if not ImgAlpha.Bitmap.Empty then
      begin
        Tmp := TBitmap32.Create;
        try
          Tmp.SetSize(B.Bitmap.Width, B.Bitmap.Height);
          ImgAlpha.Bitmap.DrawTo(Tmp, Classes.Rect(0, 0, Tmp.Width, Tmp.Height));

          // combine Alpha into already loaded RGB colors
          IntensityToAlpha(B.Bitmap, Tmp);
        finally
          Tmp.Free;
        end;
      end;

      with ImgView.GetViewportRect do
        P := ImgView.ControlToBitmap(GR32.Point((Right + Left) div 2, (Top + Bottom) div 2));

      with B do
      begin
        W := Bitmap.Width * 0.5;
        H := Bitmap.Height * 0.5;

        with ImgView.Bitmap do
          Location := GR32.FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

        Scaled := True;
        OnMouseDown := LayerMouseDown;
      end;
      Selection := B;
    end;
  end;
end;

procedure TMainForm.MnuReorderClick(Sender: TObject);
begin
  // note that the top-most layer is occupied with the rubber-banding layer
  if Selection <> nil then
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
  with ImgView do
  try
    Selection := nil;
    RBLayer := nil;
    Layers.Clear;
    Scale := 1;
    Bitmap.LoadFromFile(FileName);
  finally
    pnlImage.Visible := not Bitmap.Empty;
  end;
end;

procedure TMainForm.PaintButtonMockupHandler(Sender: TObject;
  Buffer: TBitmap32);
var
  RoundPoly: TArrayOfFloatPoint;
  TextPoly: TArrayOfArrayOfFloatPoint;
  Bounds, Dst: TFloatRect;
  Path: TFlattenedPath;
  Intf: ITextToPathSupport;
  ColorGradient: TLinearGradientPolygonFiller;
const
  CScale = 1 / 200;
begin
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender) do
    begin
      Bounds := GetAdjustedLocation;
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
end;

procedure TMainForm.PaintMagnifierHandler(Sender: TObject; Buffer: TBitmap32);
var
  Magnification, Rotation: Single;
  SrcRect, DstRect: TFloatRect;
  R: TRect;
  T: TAffineTransformation;
  B: TBitmap32;
  W2, H2: Single;
  I: Integer;
begin
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender) do
    begin
      DstRect := GetAdjustedLocation;
      R := MakeRect(DstRect);

      if not Buffer.MeasuringMode then
      begin
        Magnification := Power(10, (GbrMagnMagnification.Position * 0.02));
        Rotation := -GbrMagnRotation.Position;

        B := TBitmap32.Create;
        try
          with R do
          begin
            B.SetSize(Right - Left, Bottom - Top);
            W2 := (Right - Left) * 0.5;
            H2 := (Bottom - Top) * 0.5;
          end;

          SrcRect := DstRect;
          with SrcRect do
          begin
            Left := Left - H2;
            Right := Right + H2;
            Top := Top - W2;
            Bottom := Bottom + W2;
          end;

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
               with R do Buffer.RaiseRectTS(Left, Top, Right, Bottom, 35 - I * 8);
               InflateRect(R, -1, -1);
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
end;

procedure TMainForm.PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
var
  Cx, Cy: Single;
  W2, H2: Single;
  I: Integer;
const
  CScale = 1 / 200;
begin
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      W2 := (Right - Left) * 0.5;
      H2 := (Bottom - Top) * 0.5;
      Cx := Left + W2;
      Cy := Top + H2;
      W2 := W2 * CScale;
      H2 := H2 * CScale;
      Buffer.PenColor := clRed32;
      Buffer.MoveToF(Cx, Cy);
      for I := 0 to 240 do
        Buffer.LineToFS(
          Cx + W2 * I * Cos(I * 0.125),
          Cy + H2 * I * Sin(I * 0.125));
    end;
end;

procedure TMainForm.ScaleComboChange(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  S := ScaleCombo.Text;
  S := StringReplace(S, '%', '', [rfReplaceAll]);
  S := StringReplace(S, ' ', '', [rfReplaceAll]);
  if S = '' then Exit;
  I := StrToIntDef(S, -1);
  if (I < 1) or (I > 2000) then
    I := Round(ImgView.Scale * 100)
  else
    ImgView.Scale := I * 0.01;
  ScaleCombo.Text := IntToStr(I) + '%';
  ScaleCombo.SelStart := Length(ScaleCombo.Text) - 1;
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
        with TBitmapLayer(Value) do
        begin
          pnlBitmapLayer.Visible := True;
          GbrLayerOpacity.Position := Bitmap.MasterAlpha;
          CbxLayerInterpolate.Checked := Bitmap.Resampler.ClassType = TDraftResampler;
        end
      else if Value.Tag = 2 then
      begin
        // tag = 2 for button mockup
        pnlButtonMockup.Visible := True;
      end
      else if Value.Tag = 3 then
      begin
        // tag = 3 for magnifiers
        pnlMagnification.Visible := True;
      end;
    end;
  end;
end;

procedure TMainForm.MnuScaledClick(Sender: TObject);
begin
  if Selection <> nil then Selection.Scaled := not Selection.Scaled;
  RBLayer.Scaled := Selection.Scaled;
end;

procedure TMainForm.ImgViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Location: TFloatRect;
begin
  if Assigned(FSelection) then
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
    end;
end;

procedure TMainForm.ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Layer = nil then
  begin
    Selection := nil;
  end;
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
  if DragState = dsMove then Exit; // we are interested only in scale operations
  if Shift = [] then Exit; // special processing is not required

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
var
  s: Single;
begin
  s := ImgView.Scale / 1.1;
  if s < 0.2 then s := 0.2;
  ImgView.Scale := s;
  ScaleCombo.Text := IntToStr(Round(s * 100)) + '%';
end;

procedure TMainForm.ImgViewMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  s: Single;
begin
  s := ImgView.Scale * 1.1;
  if s > 20 then s := 20;
  ImgView.Scale := s;
  ScaleCombo.Text := IntToStr(Round(s * 100)) + '%';
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
