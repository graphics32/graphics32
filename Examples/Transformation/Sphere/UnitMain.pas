unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  GR32,
  GR32_Transforms,
  GR32_Rasterizers,
  GR32_Image;

const
  MSG_AFTER_SHOW = WM_USER;
  MSG_AFTER_RESIZE = WM_USER+1;

type
  TFormMain = class(TForm)
    PaintBox32: TPaintBox32;
    TimerRotate: TTimer;
    procedure FormResize(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimerRotateTimer(Sender: TObject);
    procedure PaintBox32DblClick(Sender: TObject);
    procedure PaintBox32MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
  private
    FBitmap: TBitmap32;
    FTransformation: TSphereTransformation;
    FRasterizer: TRasterizer;
    FDraftRasterizer: TRasterizer;

    FCurrentRasterizer: TRasterizer;
    FLastMousePos: TPoint;

    procedure MsgAfterShow(var Msg: TMessage); message MSG_AFTER_SHOW;
    procedure MsgAfterResize(var Msg: TMessage); message MSG_AFTER_RESIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

{$R *.dfm}

{$if defined(DCC) and (CompilerVersion >= 26.0)} // XE5 and later?
  {-$define USE_GEOLOCATION}
{$ifend}

uses
  System.Math,
  System.Diagnostics,
{$ifdef USE_GEOLOCATION}
  System.Sensors,
{$endif}
  GR32_Backends_Generic,
  GR32.ImageFormats.JPG,
  GR32.Examples;

//------------------------------------------------------------------------------

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := TBitmap32.Create(TMemoryBackend);
  FBitmap.ResamplerClassName := 'TLinearResampler';
  FBitmap.Resampler.PixelAccessMode := pamTransparentEdge;

  FTransformation := TSphereTransformation.Create;
  FRasterizer := TMultithreadedRegularRasterizer.Create;
  FDraftRasterizer := TDraftRasterizer.Create;

  FCurrentRasterizer := FRasterizer;

  PaintBox32.Visible := False;
end;

destructor TFormMain.Destroy;
begin
  FBitmap.Free;
  FTransformation.Free;
  FRasterizer.Free;
  FDraftRasterizer.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TFormMain.FormResize(Sender: TObject);
begin
  PostMessage(Handle, MSG_AFTER_RESIZE, 0, 0);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  // Loading the source image can take a while so defer it until the form has actually been shown
  PostMessage(Handle, MSG_AFTER_SHOW, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TFormMain.MsgAfterResize(var Msg: TMessage);
begin
  FTransformation.Radius := Min(PaintBox32.Width, PaintBox32.Height) / 2;
  FTransformation.Center := FloatPoint(PaintBox32.Width / 2, PaintBox32.Height / 2);
end;

procedure TFormMain.MsgAfterShow(var Msg: TMessage);
{$ifdef USE_GEOLOCATION}
var
  SensorManager: TSensorManager;
  Sensors: TSensorArray;
  Sensor: TCustomSensor;
  WasStarted: boolean;
{$endif}
var
  s: string;
  Size: TSize;
begin
  Screen.Cursor := crAppStart;

  PaintBox32.Buffer.Clear(clBlack32);
  PaintBox32.Visible := True;

  s := 'Loading - Please wait...';
  Size := PaintBox32.Buffer.TextExtent(s);
  PaintBox32.Buffer.RenderText((PaintBox32.Width-Size.cx) div 2, (PaintBox32.Height-Size.cy) div 2, s, -1, clRed32);
  PaintBox32.Flush;

  FBitmap.LoadFromFile(Graphics32Examples.MediaFolder + '\Globe.jpg');

  FTransformation.SrcRect := MakeRect(FBitmap.BoundsRect);

  Screen.Cursor := crDefault;

  // Start with some random location, a bit more interesting than the Pacific Ocean
  FTransformation.Longitude := 55.29952826015878 / 90 * PI*2; // TODO : Something wrong here
  FTransformation.Lattitude := -12.052785473578014 / 90 * PI*2;

{$ifdef USE_GEOLOCATION}
  // Try to get current location. Unfortunately this doesn't seem to work without
  // an actual location device although Windows is able to provide a user-specified
  // default location.
  SensorManager := TSensorManager.Current;
  SensorManager.Activate;
  Sensors := SensorManager.GetSensorsByCategory(TSensorCategory.Location);
  for Sensor in Sensors do
  begin
    WasStarted := Sensor.Started;
    if (not WasStarted) then
      Sensor.Start;

    if (not Sensor.Started) then
      continue;

    FTransformation.Longitude := TCustomLocationSensor(Sensor).Longitude / 180 * PI;
    FTransformation.Lattitude := TCustomLocationSensor(Sensor).Latitude / 90 * PI*2;

    if (not WasStarted) then
      Sensor.Stop;

    break;
  end;
{$endif}
end;

//------------------------------------------------------------------------------

procedure TFormMain.PaintBox32DblClick(Sender: TObject);
begin
  TimerRotate.Enabled := not TimerRotate.Enabled;
end;

procedure TFormMain.PaintBox32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;

  if ([ssLeft, ssRight] * Shift = []) then
    Exit;

  // Store current mouse position and switch to the draft rasterizer
  FLastMousePos := Point(X, Y);
  FCurrentRasterizer := FDraftRasterizer;
end;

procedure TFormMain.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
begin
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;

  if ([ssLeft, ssRight] * Shift = []) then
    Exit;

  TimerRotate.Enabled := False;

  DeltaX := X - FLastMousePos.X;
  DeltaY := Y - FLastMousePos.Y;

  if (Abs(DeltaX) <= 5) and (Abs(DeltaY) <= 5) then
    Exit;

  FLastMousePos := Point(X, Y);

  if (ssLeft in Shift) then
  begin
    // Rotate
    FTransformation.Longitude := FTransformation.Longitude - (DeltaX / FTransformation.Radius) * (PI / 2);
    FTransformation.Lattitude := FTransformation.Lattitude - (DeltaY / FTransformation.Radius) * (PI / 2);
  end else
  if (ssRight in Shift) then
    // Pan
    FTransformation.Center := FloatPoint(FTransformation.Center.X + DeltaX, FTransformation.Center.Y + DeltaY);

  PaintBox32.Invalidate;
end;

procedure TFormMain.PaintBox32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;

  FCurrentRasterizer := FRasterizer;
  PaintBox32.Invalidate;
end;

procedure TFormMain.PaintBox32MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  // Zoom
  FTransformation.Radius := FTransformation.Radius + 50 * WheelDelta / 120;
  PaintBox32.Invalidate;
  Handled := True;
end;

//------------------------------------------------------------------------------

procedure TFormMain.PaintBox32PaintBuffer(Sender: TObject);
var
  StopWatch: TStopWatch;
begin
  PaintBox32.Buffer.Clear(clBlack32);

  StopWatch := TStopWatch.StartNew;

  if (FBitmap <> nil) and (not FBitmap.Empty) then
    Transform(PaintBox32.Buffer, FBitmap, FTransformation, FCurrentRasterizer);

  StopWatch.Stop;
  PaintBox32.Buffer.RenderText(0, 0, Format('Rasterized in %d mS', [StopWatch.ElapsedMilliseconds]), -1, clWhite32);

  // While manually panning or rotating, adjust the pixel size so we are able to maintain
  // a frame rate between 25-50 fps
  if (FCurrentRasterizer = FDraftRasterizer) then
  begin
    if (StopWatch.ElapsedMilliseconds < 20) then
      TDraftRasterizer(FDraftRasterizer).PixelSize := TDraftRasterizer(FDraftRasterizer).PixelSize - 1
    else
    if (StopWatch.ElapsedMilliseconds > 40) then
      TDraftRasterizer(FDraftRasterizer).PixelSize := TDraftRasterizer(FDraftRasterizer).PixelSize + 1;
    PaintBox32.Buffer.RenderText(0, 20, Format('Pixel size: %d', [TDraftRasterizer(FDraftRasterizer).PixelSize]), -1, clWhite32);
  end;
end;

//------------------------------------------------------------------------------

procedure TFormMain.TimerRotateTimer(Sender: TObject);
begin
  FTransformation.Longitude := FTransformation.Longitude - 0.001;

  PaintBox32.Invalidate;
end;

//------------------------------------------------------------------------------

end.
