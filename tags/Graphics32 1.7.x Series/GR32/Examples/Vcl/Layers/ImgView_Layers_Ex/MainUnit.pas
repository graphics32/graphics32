unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, JPeg, ExtDlgs, StdCtrls, GR32, GR32_Image, GR32_Layers,
  GR32_RangeBars, GR32_Filters, GR32_Transforms;

type
  TMainForm = class(TForm)
    ImgView: TImgView32;
    SidePanel: TPanel;
    MainMenu: TMainMenu;
    mnFileNew: TMenuItem;
    mnFile: TMenuItem;
    mnFileOpen: TMenuItem;
    mnLayers: TMenuItem;
    mnNewBitmapLayer: TMenuItem;
    mnDelete: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlImage: TPanel;
    ScaleCombo: TComboBox;
    Label1: TLabel;
    Panel2: TPanel;
    ImageInterpolate: TCheckBox;
    pnlBitmapLayer: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    LayerOpacity: TGaugeBar;
    LayerInterpolate: TCheckBox;
    mnNewBitmapRGBA: TMenuItem;
    LayerRescale: TButton;
    LayerResetScale: TButton;
    Cropped: TCheckBox;
    mnNewCustomLayer: TMenuItem;
    mnMagnifier: TMenuItem;
    PnlMagn: TPanel;
    Label3: TLabel;
    Panel4: TPanel;
    MagnOpacity: TGaugeBar;
    SaveDialog1: TSaveDialog;
    Label4: TLabel;
    MagnMagnification: TGaugeBar;
    Label5: TLabel;
    MagnRotation: TGaugeBar;
    MagnInterpolate: TCheckBox;
    mnSimpleDrawing: TMenuItem;
    mnArrange: TMenuItem;
    mnBringFront: TMenuItem;
    mnSendBack: TMenuItem;
    N1: TMenuItem;
    mnLevelUp: TMenuItem;
    mnLevelDown: TMenuItem;
    N2: TMenuItem;
    mnScaled: TMenuItem;
    N4: TMenuItem;
    mnFlatten: TMenuItem;
    N3: TMenuItem;
    mnFlipHorz: TMenuItem;
    mnFlipVert: TMenuItem;
    N5: TMenuItem;
    mnRotate90: TMenuItem;
    mnRotate180: TMenuItem;
    mnRotate270: TMenuItem;
    N6: TMenuItem;
    mnPrint: TMenuItem;
    procedure mnFileNewClick(Sender: TObject);
    procedure mnFileOpenClick(Sender: TObject);
    procedure mnNewBitmapLayerClick(Sender: TObject);
    procedure mnDeleteClick(Sender: TObject);
    procedure mnLayersClick(Sender: TObject);
    procedure mnNewBitmapRGBAClick(Sender: TObject);
    procedure mnMagnifierClick(Sender: TObject);
    procedure mnSimpleDrawingClick(Sender: TObject);
    procedure mnArrangeClick(Sender: TObject);
    procedure mnScaledClick(Sender: TObject);
    procedure mnReorder(Sender: TObject);
    procedure ScaleComboChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageInterpolateClick(Sender: TObject);
    procedure LayerOpacityChange(Sender: TObject);
    procedure LayerInterpolateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LayerRescaleClick(Sender: TObject);
    procedure LayerResetScaleClick(Sender: TObject);
    procedure CroppedClick(Sender: TObject);
    procedure MagnChange(Sender: TObject);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewPaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure mnFlattenClick(Sender: TObject);
    procedure ImgViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ImgViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure mnFlipHorzClick(Sender: TObject);
    procedure mnFlipVertClick(Sender: TObject);
    procedure mnRotate90Click(Sender: TObject);
    procedure mnRotate180Click(Sender: TObject);
    procedure mnRotate270Click(Sender: TObject);
    procedure mnFileClick(Sender: TObject);
    procedure mnPrintClick(Sender: TObject);
  private
    FSelection: TPositionedLayer;
    procedure SetSelection(Value: TPositionedLayer);
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
  protected
    RBLayer: TRubberbandLayer;
    function CreatePositionedLayer: TPositionedLayer;
    procedure LayerMouseDown(Sender: TObject; Buttons: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RBResizing(Sender: TObject; const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);
    procedure PaintMagnifierHandler(Sender: TObject; Buffer: TBitmap32);
    procedure PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
  public
    procedure CreateNewImage(AWidth, AHeight: Integer; FillColor: TColor32);
    procedure OpenImage(const FileName: string);
    property Selection: TPositionedLayer read FSelection write SetSelection;
  end;

var
  MainForm: TMainForm;

implementation

uses NewImageUnit, RGBALoaderUnit, Math, GR32_LowLevel, Printers;

{$R *.DFM}

{ TForm1 }

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
    P := ImgView.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));

  Result := TPositionedLayer.Create(ImgView.Layers);
  Result.Location := FloatRect(P.X - 32, P.Y - 32, P.X + 32, P.Y + 32);
  Result.Scaled := True;
  Result.MouseEvents := True;
  Result.OnMouseDown := LayerMouseDown;
end;

procedure TMainForm.CroppedClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Cropped := Cropped.Checked;
end;

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
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Selection := nil;
  RBLayer := nil;
end;

procedure TMainForm.ImageInterpolateClick(Sender: TObject);
const
  STRETCH_FILTER: array [Boolean] of TStretchFilter = (sfNearest, sfLinear);
begin
  ImgView.Bitmap.StretchFilter := STRETCH_FILTER[ImageInterpolate.Checked];
end;

procedure TMainForm.LayerInterpolateClick(Sender: TObject);
const
  STRETCH_FILTER: array [Boolean] of TStretchFilter = (sfNearest, sfLinear);
begin
  if Selection is TBitmapLayer then
  begin
    TBitmapLayer(Selection).Bitmap.StretchFilter := STRETCH_FILTER[LayerInterpolate.Checked];
  end;
end;

procedure TMainForm.LayerMouseDown(Sender: TObject; Buttons: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender <> nil then Selection := TPositionedLayer(Sender);
end;

procedure TMainForm.LayerOpacityChange(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.MasterAlpha := LayerOpacity.Position;
end;

procedure TMainForm.LayerRescaleClick(Sender: TObject);
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
      T.StretchFilter := sfLinear;
      T.DrawMode := dmOpaque;
      T.DrawTo(Bitmap, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      T.Free;
      LayerResetScaleClick(Self);
    end;
end;

procedure TMainForm.LayerResetScaleClick(Sender: TObject);
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

procedure TMainForm.MagnChange(Sender: TObject);
begin
  ImgView.Invalidate;
end;

procedure TMainForm.mnArrangeClick(Sender: TObject);
var
  B: Boolean;
begin
  B := Selection <> nil;
  mnBringFront.Enabled := B and (Selection.Index < ImgView.Layers.Count - 2);
  mnSendBack.Enabled := B and (Selection.Index > 0);
  mnLevelUp.Enabled := B and (Selection.Index < ImgView.Layers.Count - 2);
  mnLevelDown.Enabled := B and (Selection.Index > 0);
  mnScaled.Enabled := B;
  mnScaled.Checked := B and Selection.Scaled;
  mnDelete.Enabled := B;
  B := B and (Selection is TBitmapLayer);
  mnFlipHorz.Enabled := B;
  mnFlipVert.Enabled := B;
  mnRotate90.Enabled := B;
  mnRotate180.Enabled := B;
  mnRotate270.Enabled := B;
end;

procedure TMainForm.mnDeleteClick(Sender: TObject);
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

procedure TMainForm.mnFileNewClick(Sender: TObject);
begin
  with NewImageForm do
  begin
    ShowModal;
    if ModalResult = mrOK then
      CreateNewImage(UpDown1.Position, UpDown2.Position, Color32(Panel1.Color));
  end;
end;

procedure TMainForm.mnFileOpenClick(Sender: TObject);
begin
  with OpenPictureDialog1 do
    if Execute then OpenImage(FileName);
end;

procedure TMainForm.mnLayersClick(Sender: TObject);
var
  B: Boolean;
begin
  B := not ImgView.Bitmap.Empty;
  mnNewBitmapLayer.Enabled := B;
  mnNewBitmapRGBA.Enabled := B;
  mnNewCustomLayer.Enabled := B;
  mnFlatten.Enabled := B and (ImgView.Layers.Count > 0);
end;

procedure TMainForm.mnMagnifierClick(Sender: TObject);
var
  L: TPositionedLayer;
begin
  L := CreatePositionedLayer;
  L.OnPaint := PaintMagnifierHandler;
  L.Tag := 2;
  Selection := L;
end;

procedure TMainForm.mnNewBitmapLayerClick(Sender: TObject);
var
  B: TBitmapLayer;
  P: TPoint;
  W, H: Single;
begin
  with OpenPictureDialog1 do
    if Execute then
    begin
      B := TBitmapLayer.Create(ImgView.Layers);
      with B do
      try
        Bitmap.LoadFromFile(FileName);
        Bitmap.DrawMode := dmBlend;

        with ImgView.GetViewportRect do
          P := ImgView.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));

        W := Bitmap.Width / 2;
        H := Bitmap.Height / 2;

        with ImgView.Bitmap do
          Location := FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

        Scaled := True;
        OnMouseDown := LayerMouseDown;
      except
        Free;
        raise;
      end;
      Selection := B;
    end;
end;

procedure TMainForm.mnNewBitmapRGBAClick(Sender: TObject);
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
          ImgAlpha.Bitmap.DrawTo(Tmp, Rect(0, 0, Tmp.Width, Tmp.Height));

          // combine Alpha into already loaded RGB colors
          IntensityToAlpha(B.Bitmap, Tmp);
        finally
          Tmp.Free;
        end;
      end;

      with ImgView.GetViewportRect do
        P := ImgView.ControlToBitmap(Point((Right + Left) div 2, (Top + Bottom) div 2));

      with B do
      begin
        W := Bitmap.Width / 2;
        H := Bitmap.Height / 2;

        with ImgView.Bitmap do
          Location := FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);

        Scaled := True;
        OnMouseDown := LayerMouseDown;
      end;
      Selection := B;
    end;
  end;
end;

procedure TMainForm.mnReorder(Sender: TObject);
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

procedure TMainForm.mnSimpleDrawingClick(Sender: TObject);
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
      Magnification := Power(10, (MagnMagnification.Position / 50));
      Rotation := -MagnRotation.Position;

      DstRect := GetAdjustedLocation;
      R := MakeRect(DstRect);
      B := TBitmap32.Create;
      try
        with R do
        begin
          B.SetSize(Right - Left, Bottom - Top);
          W2 := (Right - Left) / 2;
          H2 := (Bottom - Top) / 2;
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

          if MagnInterpolate.Checked then
          begin
            Buffer.BeginUpdate;
            Buffer.StretchFilter := sfLinear;
            Transform(B, Buffer, T);
            Buffer.StretchFilter := sfNearest;
            Buffer.EndUpdate;
          end
          else
             Transform(B, Buffer, T);

          B.ResetAlpha;
          B.DrawMode := dmBlend;
          B.MasterAlpha := MagnOpacity.Position;
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
end;

procedure TMainForm.PaintSimpleDrawingHandler(Sender: TObject; Buffer: TBitmap32);
var
  Cx, Cy: Single;
  W2, H2: Single;
  I: Integer;
begin
  if Sender is TPositionedLayer then
    with TPositionedLayer(Sender).GetAdjustedLocation do
    begin
      W2 := (Right - Left) / 2;
      H2 := (Bottom - Top) / 2;
      Cx := Left + W2;
      Cy := Top + H2;
      Buffer.PenColor := clRed32;
      Buffer.MoveToF(Cx,Cy);
      for I := 0 to 240 do
      begin
        Buffer.LineToFS(Cx + W2 * I / 200 * Cos(I / 8), Cy + H2 * I / 200 * Sin(I / 8));
      end;
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
  if (I < 1) or (I > 2000) then I := Round(ImgView.Scale * 100)
  else ImgView.Scale := I / 100;
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
      pnlMagn.Visible := False;
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
      else RBLayer.BringToFront;
      RBLayer.ChildLayer := Value;
      RBLayer.LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS or LOB_NO_UPDATE;
      RBLayer.OnResizing := RBResizing;

      if Value is TBitmapLayer then
        with TBitmapLayer(Value) do
        begin
          pnlBitmapLayer.Visible := True;
          LayerOpacity.Position := Bitmap.MasterAlpha;
          LayerInterpolate.Checked := Bitmap.StretchFilter = sfLinear;
        end
      else if Value.Tag = 2 then
      begin
        // tag = 2 for magnifiers
        pnlMagn.Visible := True;
      end;
    end;
  end;
end;

procedure TMainForm.WMEraseBkgnd(var Msg: TMessage);
begin
  // disable form background cleaning
  Msg.Result := 1;
end;

procedure TMainForm.mnScaledClick(Sender: TObject);
begin
  if Selection <> nil then Selection.Scaled := not Selection.Scaled;
  RBLayer.Scaled := Selection.Scaled;
end;

procedure TMainForm.ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Layer = nil then
  begin
    Selection := nil;
    ReleaseCapture;
  end;
end;

procedure TMainForm.ImgViewPaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
const
  Colors: array [0..1] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  W, I, J, Parity: Integer;
  Line1, Line2: TArrayOfColor32; // a buffer for a couple of scanlines
begin
  with ImgView.Buffer do
  begin
    W := Width;
    SetLength(Line1, W);
    SetLength(Line2, W);
    for I := 0 to W - 1 do
    begin
      Parity := I shr 3 and $1;
      Line1[I] := Colors[Parity];
      Line2[I] := Colors[1 - Parity];
    end;
    for J := 0 to Height - 1 do
    begin
      Parity := J shr 3 and $1;
      if Boolean(Parity) then MoveLongword(Line1[0], ScanLine[J]^, W)
      else MoveLongword(Line2[0], ScanLine[J]^, W);
    end;
  end;
end;

procedure TMainForm.RBResizing(Sender: TObject;
  const OldLocation: TFloatRect; var NewLocation: TFloatRect;
  DragState: TDragState; Shift: TShiftState);
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

procedure TMainForm.mnFlattenClick(Sender: TObject);
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
    ImgView.PaintTo(B, Rect(0, 0, W, H));

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

procedure TMainForm.mnPrintClick(Sender: TObject);
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
    ImgView.PaintTo(B, Rect(0, 0, W, H));
    Printer.BeginDoc;
    Printer.Title := 'Graphics32 Demo';
    B.StretchFilter := sfLinear;
    R := GetCenteredRectToFit(Rect(0, 0, W, H), Rect(0, 0, Printer.PageWidth, Printer.PageHeight));
    B.TileTo(Printer.Canvas.Handle, R, Rect(0, 0, W, H));
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

procedure TMainForm.mnFlipHorzClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.FlipHorz;
end;

procedure TMainForm.mnFlipVertClick(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.FlipVert;
end;

procedure TMainForm.mnRotate90Click(Sender: TObject);
var
  R: TFloatRect;
  Cx, Cy, W2, H2: Single;
begin
  if Selection is TBitmapLayer then
  begin
    R := Selection.Location;
    TBitmapLayer(Selection).Bitmap.Rotate90;
    Cx := (R.Left + R.Right) / 2;
    Cy := (R.Top + R.Bottom) / 2;
    W2 := (R.Right - R.Left) / 2;
    H2 := (R.Bottom - R.Top) / 2;
    RBLayer.Location := FloatRect(Cx - H2, Cy - W2, Cx + H2, Cy + W2);
  end;
end;

procedure TMainForm.mnRotate180Click(Sender: TObject);
begin
  if Selection is TBitmapLayer then
    TBitmapLayer(Selection).Bitmap.Rotate180;
end;

procedure TMainForm.mnRotate270Click(Sender: TObject);
var
  R: TFloatRect;
  Cx, Cy, W2, H2: Single;
begin
  if Selection is TBitmapLayer then
  begin
    R := Selection.Location;
    TBitmapLayer(Selection).Bitmap.Rotate270;
    Cx := (R.Left + R.Right) / 2;
    Cy := (R.Top + R.Bottom) / 2;
    W2 := (R.Right - R.Left) / 2;
    H2 := (R.Bottom - R.Top) / 2;
    RBLayer.Location := FloatRect(Cx - H2, Cy - W2, Cx + H2, Cy + W2);
  end;
end;

procedure TMainForm.mnFileClick(Sender: TObject);
begin
  mnPrint.Enabled := not ImgView.Bitmap.Empty;
end;

end.
