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
    TimerMove: TTimer;
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
    procedure TimerMoveTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FBitmap: TBitmap32;
    FTransformation: TSphereTransformation;
    FRasterizer: TRasterizer;
    FDraftRasterizer: TRasterizer;

    FCurrentRasterizer: TRasterizer;
    FLastMousePos: TPoint;
    FShowStarfield: boolean;
    FRotationDelta: Single;

    procedure MsgAfterShow(var Msg: TMessage); message MSG_AFTER_SHOW;
    procedure MsgAfterResize(var Msg: TMessage); message MSG_AFTER_RESIZE;
    procedure Reset;
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
  {$define USE_GEOLOCATION}
{$ifend}

uses
  System.Types,
  System.Math,
  System.Diagnostics,
{$ifdef USE_GEOLOCATION}
  System.Sensors,
{$endif}
  GR32_Backends_Generic,
  GR32.ImageFormats.JPG,
  GR32.Examples;

const
  DefaultLongitude: Double =    12.052785473578014 / 180 * PI + PI;
  DefaultLatitude: Double =     55.29952826015878 / 180 * PI;

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
  FRotationDelta := 0.001;

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

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;

    Ord('S'), Ord('s'):
      begin
        FShowStarfield := not FShowStarfield;
        PaintBox32.Invalidate;
      end;

    Ord('R'), Ord('r'):
      Reset;

    VK_ADD:
      FRotationDelta := Min(0.1, FRotationDelta * 2);

    VK_SUBTRACT:
      FRotationDelta := Max(0.001, FRotationDelta / 2);
  end;
end;

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
  PaintBox32.Buffer.RenderText((PaintBox32.Width-Size.cx) div 2, (PaintBox32.Height-Size.cy) div 2, s, clRed32);
  PaintBox32.Flush;

  FBitmap.LoadFromFile(Graphics32Examples.MediaFolder + '\Globe.jpg');

  FTransformation.SrcRect := MakeRect(FBitmap.BoundsRect);

  Screen.Cursor := crDefault;

  // Start with some "random location", a bit more interesting than the Pacific Ocean
  FTransformation.Longitude := DefaultLongitude;
  FTransformation.Latitude := DefaultLatitude;

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

    FTransformation.Longitude := TCustomLocationSensor(Sensor).Longitude / 180 * PI + PI;
    FTransformation.Latitude := TCustomLocationSensor(Sensor).Latitude / 180 * PI;

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
  FLastMousePos := GR32.Point(X, Y);
  FCurrentRasterizer := FDraftRasterizer;
end;

procedure TFormMain.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
  P1, P2: TFloatPoint;
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

  TimerMove.Enabled := False;

  if (ssLeft in Shift) then
  begin
    // Rotate
    P1 := FTransformation.SphericalCoordinate(FLastMousePos.X, FLastMousePos.Y);
    P2 := FTransformation.SphericalCoordinate(X, Y);

    if FTransformation.IsInSphere(FLastMousePos.X, FLastMousePos.Y) and FTransformation.IsInSphere(X, Y) then
    begin
      FTransformation.Longitude := FTransformation.Longitude - (P2.X - P1.X);
      FTransformation.Latitude := FTransformation.Latitude - (P2.Y - P1.Y);
    end else
    begin
      FTransformation.Longitude := FTransformation.Longitude - (DeltaX / FTransformation.Radius);
      FTransformation.Latitude := FTransformation.Latitude + (DeltaY / FTransformation.Radius);
    end;
  end else
  if (ssRight in Shift) then
    // Pan
    FTransformation.Center := FloatPoint(FTransformation.Center.X + DeltaX, FTransformation.Center.Y + DeltaY);

  FLastMousePos := GR32.Point(X, Y);
  PaintBox32.Invalidate;

  // Start timer that detects if user pauses movement
  TimerMove.Enabled := True;
end;

procedure TFormMain.PaintBox32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;

  TimerMove.Enabled := False;
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
  i: integer;
  x, y: integer;
  r: TRect;
begin
  PaintBox32.Buffer.Clear(clBlack32);

  StopWatch := TStopWatch.StartNew;

  if (FBitmap <> nil) and (not FBitmap.Empty) then
    Transform(PaintBox32.Buffer, FBitmap, FTransformation, FCurrentRasterizer);

  StopWatch.Stop;
  PaintBox32.Buffer.RenderText(0, 0, Format('Rasterized in %d mS', [StopWatch.ElapsedMilliseconds]), clWhite32);

  // Paint a random starfield
  if (FShowStarfield) then
  begin
    RandSeed := 0;
    for i := 1 to 200 do
    begin
      x := Random(PaintBox32.Width);
      y := Random(PaintBox32.Height);
      if (FTransformation.IsInSphere(x, y)) then
        continue;
      PaintBox32.Buffer.Pixel[x, y] := clWhite32;
    end;
  end;

  // While manually panning or rotating, adjust the pixel size so we are able to maintain
  // a frame rate between 25-50 fps
  if (FCurrentRasterizer = FDraftRasterizer) then
  begin
    if (StopWatch.ElapsedMilliseconds < 20) then
      TDraftRasterizer(FDraftRasterizer).PixelSize := TDraftRasterizer(FDraftRasterizer).PixelSize - 1
    else
    if (StopWatch.ElapsedMilliseconds > 40) then
      TDraftRasterizer(FDraftRasterizer).PixelSize := TDraftRasterizer(FDraftRasterizer).PixelSize + 1;
    PaintBox32.Buffer.RenderText(0, 20, Format('Pixel size: %d', [TDraftRasterizer(FDraftRasterizer).PixelSize]), clWhite32);
  end;

  r := Rect(0, 8, ClientWidth-8, ClientHeight);
  PaintBox32.Buffer.Font.Color := clSkyBlue;
  PaintBox32.Buffer.Textout(r, DT_RIGHT or DT_TOP,
    'Mouse-left: Rotate'#13+
    'Mouse-right: Pan'#13+
    'Mouse-scroll: Zoom'#13+
    'Dbl-click: Animate'#13+
    'S: Start field'#13+
    'R: Reset to origin'#13+
    '+ / -: Faster/Slower animate'#13+
    '[Esc]: Quit'
    );
end;

//------------------------------------------------------------------------------

procedure TFormMain.Reset;
begin
  FTransformation.Longitude := DefaultLongitude;
  FTransformation.Latitude := DefaultLatitude;
  FTransformation.Center := FloatPoint(PaintBox32.Width / 2, PaintBox32.Height / 2);
  FTransformation.Radius := Min(PaintBox32.Width, PaintBox32.Height) / 2;
  FRotationDelta := 0.001;
  PaintBox32.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TFormMain.TimerMoveTimer(Sender: TObject);
begin
  TimerMove.Enabled := False;

  FCurrentRasterizer := FRasterizer;
  try

    PaintBox32.Repaint;

  finally
    FCurrentRasterizer := FDraftRasterizer;
  end;
end;

procedure TFormMain.TimerRotateTimer(Sender: TObject);
begin
  FTransformation.Longitude := FTransformation.Longitude - FRotationDelta;

  PaintBox32.Invalidate;
end;

//------------------------------------------------------------------------------

end.
