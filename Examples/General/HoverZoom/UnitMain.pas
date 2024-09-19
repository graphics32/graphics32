unit UnitMain;

interface

uses
  System.Types, System.Diagnostics,
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
    TimerZoom: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseLeave(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure ActionImageSmallExecute(Sender: TObject);
    procedure ActionImageLargeExecute(Sender: TObject);
    procedure ActionViewLayerExecute(Sender: TObject);
    procedure ActionViewLayerUpdate(Sender: TObject);
    procedure ActionImageCustomExecute(Sender: TObject);
    procedure RadioButtonCustomDblClick(Sender: TObject);
    procedure ActionAnimateExecute(Sender: TObject);
    procedure ImageScaleChange(Sender: TObject);
    procedure TimerZoomTimer(Sender: TObject);
  private
    FNormalOffset: TPoint;
    FBitmapLayer: TBitmapLayer;
    FZoomed: boolean;
    FZoomMode: TZoomMode;
    FNormalScale: Double;
    FZoomScale: Double;
    FStopwatchAnimation: TStopwatch;

  private
    procedure LoadImage(const Filename: string; ZoomMode: TZoomMode = zmAuto);
    procedure CenterImage;
    procedure ZoomIn(const MousePos: TPoint);
    procedure ZoomOut(const MousePos: TPoint);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.Math,
  amEasing,
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
  ImageScaleChange(nil);

  ActionImageSmall.Execute;
end;

procedure TFormMain.ImageMouseLeave(Sender: TObject);
var
  MousePos: TPoint;
begin
  MousePos := Image.ScreenToClient(Mouse.CursorPos);
  ZoomOut(MousePos)
end;

procedure TFormMain.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  MousePos: TPoint;
begin
  MousePos := Point(X, Y);

  if (Image.GetBitmapRect.Contains(MousePos)) then
    ZoomIn(MousePos)
  else
    ZoomOut(MousePos)
end;

procedure TFormMain.ImageResize(Sender: TObject);
begin
  CenterImage;
end;

procedure TFormMain.ImageScaleChange(Sender: TObject);
begin
  StatusBar.SimpleText := Format('Scale: %.3n', [Image.Scale]);
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

  CenterImage;
end;

procedure TFormMain.RadioButtonCustomDblClick(Sender: TObject);
begin
  ActionImageCustom.Execute;
end;

procedure TFormMain.TimerZoomTimer(Sender: TObject);
var
  MousePos: TPoint;
begin
  if (FZoomed) and (Image.Scale <> FZoomScale) then
  begin
    MousePos := Image.ScreenToClient(Mouse.CursorPos);
    ZoomIn(MousePos)
  end else
    TTimer(Sender).Enabled := False;
end;

procedure TFormMain.ZoomIn(const MousePos: TPoint);
var
  BitmapPos: TPoint;
  Elapsed: int64;
begin
  if (not FZoomed) then
  begin
    FZoomed := True;
    FBitmapLayer.Visible := (FZoomMode = zmSmall) and ActionViewLayer.Checked;

    // Save offset of bitmap with "normal" scale
    FNormalOffset := Image.GetBitmapRect.TopLeft;

    FStopwatchAnimation := TStopwatch.StartNew;
    Image.ForceFullInvalidate; // Work around for bug in repaint mechanism
  end;


  if (Image.Scale <> FZoomScale) then
  begin
    if (ActionAnimate.Checked) then
    begin
      // Animate the zoom using a "tween"
      Elapsed := FStopwatchAnimation.ElapsedMilliseconds;
      if (Elapsed < ZoomAnimateTime) then
      begin
        Image.Scale := FNormalScale + TEaseCubic.EaseInOut(Elapsed / ZoomAnimateTime) * (FZoomScale - FNormalScale);
        // Start a timer so we animate until the desired scale is reached
        TimerZoom.Enabled := True;
      end else
        Image.Scale := FZoomScale
    end else
      Image.Scale := FZoomScale;
  end;


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


  // Translate the position to bitmap coordinates, using the "normal" scale
  BitmapPos.X := Round((MousePos.X - FNormalOffset.X) / FNormalScale);
  BitmapPos.Y := Round((MousePos.Y - FNormalOffset.Y) / FNormalScale);

  // Calculate the offset from bitmap coordinates using the "zoomed" scale
  Image.OffsetHorz := MousePos.X - BitmapPos.X * Image.Scale;
  Image.OffsetVert := MousePos.Y - BitmapPos.Y * Image.Scale;

end;

procedure TFormMain.ZoomOut(const MousePos: TPoint);
var
  Pivot: TPoint;
begin
  if (not FZoomed) then
    exit;

  if (ActionAnimate.Checked) then
  begin
    // Animate zoom to normal
    Pivot := Image.ControlToBitmap(MousePos);
    Image.Zoom(FNormalScale, Pivot, True);
  end else
    Image.Scale := FNormalScale;

  FBitmapLayer.Visible := False;
  FZoomed := False;

  CenterImage;
end;

end.
