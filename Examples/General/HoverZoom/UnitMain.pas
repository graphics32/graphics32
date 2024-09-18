unit UnitMain;

interface

uses
  System.Types,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ComCtrls,

  GR32_Image,
  GR32_Layers;

type
  TZoomMode = (zmAuto, zmSmall, zmLarge);

type
  TFormMain = class(TForm)
    Image: TImage32;
    CheckBoxLayer: TCheckBox;
    RadioButtonSmall: TRadioButton;
    RadioButtonLarge: TRadioButton;
    ActionList: TActionList;
    ActionViewLayer: TAction;
    ActionImageSmall: TAction;
    ActionImageLarge: TAction;
    ActionImageCustom: TAction;
    RadioButtonCustom: TRadioButton;
    StatusBar: TStatusBar;
    CheckBoxAnimate: TCheckBox;
    ActionAnimate: TAction;
    procedure FormCreate(Sender: TObject);
    procedure ImageMouseLeave(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageResize(Sender: TObject);
    procedure ActionImageSmallExecute(Sender: TObject);
    procedure ActionImageLargeExecute(Sender: TObject);
    procedure ActionViewLayerExecute(Sender: TObject);
    procedure ActionViewLayerUpdate(Sender: TObject);
    procedure ActionImageCustomExecute(Sender: TObject);
    procedure RadioButtonCustomDblClick(Sender: TObject);
    procedure ActionAnimateExecute(Sender: TObject);
  private
    FNormalOffset: TPoint;
    FBitmapLayer: TBitmapLayer;
    FZoomed: boolean;
    FZoomMode: TZoomMode;
    FNormalScale: Double;
    FZoomScale: Double;
  private
    procedure LoadImage(const Filename: string; ZoomMode: TZoomMode = zmAuto);
    procedure CenterImage;
    procedure UpdateScale;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.Math,
  GR32.Examples,
  GR32.ImageFormats,
  GR32_PNG,
  GR32_PortableNetworkGraphic, // Required for inline expansion
  GR32;

procedure TFormMain.ActionAnimateExecute(Sender: TObject);
begin
  //
end;

procedure TFormMain.ActionImageCustomExecute(Sender: TObject);
var
  Filename: string;
  Filter: string;
begin
  Filename := TAction(Sender).Hint;
  Filter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True);
  if (PromptForFileName(Filename, Filter)) then
  begin
    LoadImage(Filename);
    TAction(Sender).Caption := '&Custom: ' + Filename;
    TAction(Sender).Hint := Filename;
  end;
end;

procedure TFormMain.ActionImageLargeExecute(Sender: TObject);
begin
  LoadImage(Graphics32Examples.MediaFolder+'\freetrainer5.jpg', zmLarge);
end;

procedure TFormMain.ActionImageSmallExecute(Sender: TObject);
begin
  LoadImage(Graphics32Examples.MediaFolder+'\coffee.png', zmSmall);
end;

procedure TFormMain.ActionViewLayerExecute(Sender: TObject);
begin
  //
end;

procedure TFormMain.ActionViewLayerUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FZoomMode = zmSmall);
end;

procedure TFormMain.CenterImage;
var
  r: TRect;
begin
  if (FBitmapLayer = nil) then
    exit;

  // Center main bitmap
  Image.OffsetHorz :=  Round((Image.ClientWidth - Image.Bitmap.Width * FNormalScale) * 0.5);
  Image.OffsetVert :=  Round((Image.ClientHeight - Image.Bitmap.Height * FNormalScale) * 0.5);

  // Center layer in control
  r := FBitmapLayer.Bitmap.BoundsRect;
  r.Offset((Image.ClientWidth-r.Width) div 2, (Image.ClientHeight-r.Height) div 2);
  FBitmapLayer.Location := FloatRect(r);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Semi-transparent, unscaled layer
  FBitmapLayer := Image.Layers.Add<TBitmapLayer>;
  FBitmapLayer.Scaled := False;
  FBitmapLayer.Visible := False;

  ActionImageSmall.Execute;
end;

procedure TFormMain.ImageMouseLeave(Sender: TObject);
var
  Pivot: TPoint;
begin
  if (ActionAnimate.Checked) then
  begin
    // Animate zoom to normal
    Pivot := Image.ScreenToClient(Mouse.CursorPos);
    Pivot := Image.ControlToBitmap(Pivot);
    Image.Zoom(FNormalScale, Pivot, True);

    Image.Scale := FNormalScale; // Work around for bug in animated Zoom
  end else
    Image.Scale := FNormalScale;

  FBitmapLayer.Visible := False;
  FZoomed := False;
  UpdateScale;

  CenterImage;
end;

procedure TFormMain.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  MousePos, BitmapPos, ViewportPos: TPoint;
begin
  if (not FZoomed) then
  begin
    FZoomed := True;
    FBitmapLayer.Visible := (FZoomMode = zmSmall) and ActionViewLayer.Checked;

    // Save offset of bitmap with "normal" scale
    FNormalOffset := Image.GetBitmapRect.TopLeft;

    Image.ForceFullInvalidate; // Work around for bug in repaint mechanism
  end;

  // Increase zoom while we move the mouse until we reach the desired level.
  // This is just an optional effect to make the zoom appear nicer.
  if (ActionAnimate.Checked) then
  begin
    if (Abs(FZoomScale - Image.Scale) > 0.1) then
      Image.Scale := Image.Scale + (FZoomScale - Image.Scale) / 10
    else
      Image.Scale := FZoomScale;
  end else
    Image.Scale := FZoomScale;
  UpdateScale;


  //
  // Pan so "position in bitmap" = "position in viewport".
  //
  // Looking at TCustomImage32.BitmapToControl we can see that the relationship
  // between bitmap and control position is:
  //
  //   ViewportPos = BitmapPos * Scale + Offset
  //
  // Solving the above for Offset, given ViewportPos and BitmapPos:
  //
  //   ViewportPos = BitmapPos * Scale + Offset
  //   Offset = ViewportPos - BitmapPos * Scale
  //


  MousePos := Point(X, Y);

  // What is the position relative to the "normal" scaled bitmap?
  BitmapPos.X := Round((MousePos.X - FNormalOffset.X) / FNormalScale);
  BitmapPos.Y := Round((MousePos.Y - FNormalOffset.Y) / FNormalScale);

  // What is the control (viewport) position we would like the BitmapPos to correspond to?
  ViewportPos := MousePos;

  Image.OffsetHorz := ViewportPos.X - BitmapPos.X * Image.Scale;
  Image.OffsetVert := ViewportPos.Y - BitmapPos.Y * Image.Scale;
end;

procedure TFormMain.ImageResize(Sender: TObject);
begin
  CenterImage;
end;

procedure TFormMain.LoadImage(const Filename: string; ZoomMode: TZoomMode);
var
  ResizeScaleX, ResizeScaleY: Double;
begin
  // Load image
  FBitmapLayer.Bitmap.Assign(nil);
  Image.Bitmap.LoadFromFile(Filename);

  // Calculate zoom factors
  ResizeScaleX := Image.ClientWidth / Image.Bitmap.Width;
  ResizeScaleY := Image.ClientHeight / Image.Bitmap.Height;

  if (ZoomMode = zmAuto) then
  begin
    if (ResizeScaleX < 0.75) or (ResizeScaleY < 0.75) then
      ZoomMode := zmLarge
    else
      ZoomMode := zmSmall;
  end;
  FZoomMode := ZoomMode;

  case FZoomMode of
    // Bitmap is larger than viewport; Zoom is 1:1, Normal is fit to viewport
    zmLarge:
      begin
        FNormalScale := Min(ResizeScaleX, ResizeScaleY);
        FZoomScale := 1.0;
      end;

    // Bitmap is smaller than viewport; Normal is 1:1, Zoom is no less than 3
    zmSmall:
      begin
        FNormalScale := 1.0;
        FZoomScale := Max(3, Min(ResizeScaleX, ResizeScaleY));
        FBitmapLayer.Bitmap.Assign(Image.Bitmap);
        FBitmapLayer.Bitmap.MasterAlpha := 128;
      end;
  end;

  Image.Scale := FNormalScale;
  UpdateScale;

  CenterImage;
end;

procedure TFormMain.RadioButtonCustomDblClick(Sender: TObject);
begin
  ActionImageCustom.Execute;
end;

procedure TFormMain.UpdateScale;
begin
  StatusBar.SimpleText := Format('Scale: %.3n', [Image.Scale]);
end;

end.
