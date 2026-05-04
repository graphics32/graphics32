unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  GR32,
  GR32_Math,
  GR32_Transforms,
  GR32_Rasterizers,
  GR32_Image;

const
  MSG_AFTER_SHOW = WM_USER;
  MSG_AFTER_RESIZE = WM_USER+1;

//------------------------------------------------------------------------------
//
//      TQuaternion
//
//------------------------------------------------------------------------------
// A quaternion represents a rotation in 3D space.
// It consists of a scalar part (W) and a vector part (X, Y, Z).
// q = W + Xi + Yj + Zk
//------------------------------------------------------------------------------
type
  TQuaternion = record
    X, Y, Z, W: TFloat;

    class function Identity: TQuaternion; static;

    // Creates a quaternion representing a rotation of 'Angle' radians around 'Axis'
    class function FromAxisAngle(const Axis: TVector3f; Angle: TFloat): TQuaternion; static;

    // Creates the shortest arc rotation that transforms vector V1 into V2.
    // V1 and V2 must be normalized.
    class function FromTwoVectors(const V1, V2: TVector3f): TQuaternion; static;

    // Combines two rotations.
    // Resulting rotation is Q then Self (for local rotations) or Self then Q (for
    // world/screen rotations).
    function Multiply(const Q: TQuaternion): TQuaternion;

    // Converts the quaternion to a 3x3 rotation matrix (Local to World)
    function ToMatrix: TFloatMatrix;

    // Ensures the quaternion has unit length (prevents numerical drift)
    function Normalize: TQuaternion;
  end;


//------------------------------------------------------------------------------
//
//      TArcballSphereTransformation
//
//------------------------------------------------------------------------------
// This class extends TSphereTransformation to provide 3D orientation control
// using an Arcball (or Trackball) interface.
//
// Coordinate System:
// - World Space:
//   X-axis: Points towards the viewer (Screen Forward)
//   Y-axis: Points to the right (Screen Right)
//   Z-axis: Points up (Screen Up)
// - Sphere Local Space:
//   Calculated based on FRotationMatrix.
//
// Mapping:
// - Longitude 0: Boundary of the map image (Prime Meridian is shifted by PI)
// - Latitude 0: Equator. Positive Latitude is North Pole (Z+), Negative is South.
//
// Based on:
// - "ARCBALL: A User Interface for Specifying Three-Dimensional Orientation
//    Using a Mouse"
//   Ken Shoemake
//   Proceedings of the conference on Graphics interface '92, Sep 1992, pages 151 - 156
//
// - "Arcball rotation control"
//   Ken Shoemake
//   Graphics Gems IV, Aug 1994, pages 175 - 192
//
//------------------------------------------------------------------------------
type
  TArcballSphereTransformation = class(TSphereTransformation)
  private
    FRotation: TQuaternion;
    FRotationMatrix: TFloatMatrix;
    FInvRadius: TFloat;
    procedure UpdateMatrix;
  protected
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat); override;
  public
    constructor Create; override;
    procedure RotateScreen(const Delta: TQuaternion);
    procedure RotateLocal(const Delta: TQuaternion);
    procedure SyncFromAngles;
    procedure SyncToAngles;
    property Rotation: TQuaternion read FRotation write FRotation;
  end;


//------------------------------------------------------------------------------
//
//      TFormMain
//
//------------------------------------------------------------------------------
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
    FTransformation: TArcballSphereTransformation;
    FRasterizer: TRasterizer;
    FDraftRasterizer: TRasterizer;

    FCurrentRasterizer: TRasterizer;
    FLastMousePos: TPoint;
    FDownMousePos: TPoint;
    FShowStarfield: boolean;
    FRotationDelta: Single;
    FStartRotation: TQuaternion;

    function GetSpherePoint(X, Y: Integer): TVector3f;

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
//
//      TQuaternion
//
//------------------------------------------------------------------------------

class function TQuaternion.Identity: TQuaternion;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  Result.W := 1;
end;

class function TQuaternion.FromAxisAngle(const Axis: TVector3f; Angle: TFloat): TQuaternion;
var
  S, C: TFloat;
begin
  GR32_Math.SinCos(Angle * 0.5, S, C);
  Result.X := Axis[0] * S;
  Result.Y := Axis[1] * S;
  Result.Z := Axis[2] * S;
  Result.W := C;
end;

class function TQuaternion.FromTwoVectors(const V1, V2: TVector3f): TQuaternion;
var
  Axis: TVector3f;
  Dot, S: TFloat;
begin
  // V1 and V2 are assumed to be normalized
  Dot := V1[0] * V2[0] + V1[1] * V2[1] + V1[2] * V2[2];
  if (Dot > 0.999999) then
  begin
    Result := TQuaternion.Identity;
  end else
  if (Dot < -0.999999) then
  begin
    // Opposite vectors, rotate 180 degrees around any orthogonal axis
    if Abs(V1[0]) < 0.8 then Axis[0] := 1 else Axis[0] := 0;
    if Abs(V1[1]) < 0.8 then Axis[1] := 1 else Axis[1] := 0;
    if Abs(V1[2]) < 0.8 then Axis[2] := 1 else Axis[2] := 0;

    // Cross product to get orthogonal axis
    S := Axis[0] * V1[0] + Axis[1] * V1[1] + Axis[2] * V1[2];
    Axis[0] := Axis[0] - V1[0] * S;
    Axis[1] := Axis[1] - V1[1] * S;
    Axis[2] := Axis[2] - V1[2] * S;

    // Normalize
    S := 1.0 / Sqrt(Sqr(Axis[0]) + Sqr(Axis[1]) + Sqr(Axis[2]));
    Axis[0] := Axis[0] * S;
    Axis[1] := Axis[1] * S;
    Axis[2] := Axis[2] * S;

    Result := TQuaternion.FromAxisAngle(Axis, PI);
  end else
  begin
    Axis[0] := V1[1] * V2[2] - V1[2] * V2[1];
    Axis[1] := V1[2] * V2[0] - V1[0] * V2[2];
    Axis[2] := V1[0] * V2[1] - V1[1] * V2[0];

    S := Sqrt((1 + Dot) * 2);

    Result.W := S * 0.5;
    S := 1 / S;
    Result.X := Axis[0] * S;
    Result.Y := Axis[1] * S;
    Result.Z := Axis[2] * S;
  end;
end;

function TQuaternion.Multiply(const Q: TQuaternion): TQuaternion;
begin
  Result.X := (W * Q.X) + (X * Q.W) + (Y * Q.Z) - (Z * Q.Y);
  Result.Y := (W * Q.Y) + (Y * Q.W) + (Z * Q.X) - (X * Q.Z);
  Result.Z := (W * Q.Z) + (Z * Q.W) + (X * Q.Y) - (Y * Q.X);
  Result.W := (W * Q.W) - (X * Q.X) - (Y * Q.Y) - (Z * Q.Z);
end;

function TQuaternion.Normalize: TQuaternion;
var
  InvLen: TFloat;
begin
  InvLen := 1.0 / Sqrt(Sqr(X) + Sqr(Y) + Sqr(Z) + Sqr(W));
  Result.X := X * InvLen;
  Result.Y := Y * InvLen;
  Result.Z := Z * InvLen;
  Result.W := W * InvLen;
end;

function TQuaternion.ToMatrix: TFloatMatrix;
var
  XX, YY, ZZ, XY, XZ, YZ, WX, WY, WZ: TFloat;
begin
  XX := X * X; YY := Y * Y; ZZ := Z * Z;
  XY := X * Y; XZ := X * Z; YZ := Y * Z;
  WX := W * X; WY := W * Y; WZ := W * Z;

  // Local to World rotation matrix
  Result[0, 0] := 1 - 2 * (YY + ZZ);
  Result[0, 1] := 2 * (XY - WZ);
  Result[0, 2] := 2 * (XZ + WY);

  Result[1, 0] := 2 * (XY + WZ);
  Result[1, 1] := 1 - 2 * (XX + ZZ);
  Result[1, 2] := 2 * (YZ - WX);

  Result[2, 0] := 2 * (XZ - WY);
  Result[2, 1] := 2 * (YZ + WX);
  Result[2, 2] := 1 - 2 * (XX + YY);
end;


//------------------------------------------------------------------------------
//
//      TArcballSphereTransformation
//
//------------------------------------------------------------------------------

constructor TArcballSphereTransformation.Create;
begin
  inherited;
  FRotation := TQuaternion.Identity;
  UpdateMatrix;
end;

procedure TArcballSphereTransformation.UpdateMatrix;
begin
  FRotationMatrix := FRotation.ToMatrix;
end;

procedure TArcballSphereTransformation.RotateScreen(const Delta: TQuaternion);
begin
  FRotation := Delta.Multiply(FRotation).Normalize;
  UpdateMatrix;
  SyncToAngles;
end;

procedure TArcballSphereTransformation.RotateLocal(const Delta: TQuaternion);
begin
  FRotation := FRotation.Multiply(Delta).Normalize;
  UpdateMatrix;
  SyncToAngles;
end;

procedure TArcballSphereTransformation.SyncFromAngles;
var
  Qlat, Qlong: TQuaternion;
  Axis: TVector3f;
begin
  // Convert spherical coordinates (Latitude, Longitude) to a rotation quaternion.
  // Geographic convention:
  // 1. Longitude is rotation around the Z-axis (Up).
  //    We apply PI - Longitude because Longitude 0 in the source map is at the left boundary.
  Axis[0] := 0; Axis[1] := 0; Axis[2] := 1;
  Qlong := TQuaternion.FromAxisAngle(Axis, PI - Longitude);

  // 2. Latitude is rotation around the Y-axis (Right).
  //    Positive Latitude tilts the North Pole towards the viewer.
  Axis[0] := 0; Axis[1] := 1; Axis[2] := 0;
  Qlat := TQuaternion.FromAxisAngle(Axis, Latitude);

  // Combined rotation: rotate by Longitude then by Latitude.
  FRotation := Qlat.Multiply(Qlong);
  UpdateMatrix;
end;

procedure TArcballSphereTransformation.SyncToAngles;
var
  L: TVector3f;
  LocalLongitude: TFloat;
begin
  // Extract Latitude/Longitude from the rotation matrix.
  // The viewer is located at World X = [1, 0, 0].
  // To find the viewer's position in Sphere Local Space, we multiply by R^T.
  // L = R^T * [1, 0, 0] = First row of R.
  L[0] := FRotationMatrix[0,0];
  L[1] := FRotationMatrix[0,1];
  L[2] := FRotationMatrix[0,2];

  // Map local vector L back to spherical coordinates.
  Latitude := ArcSin(L[2]); // Z component determines Latitude
  LocalLongitude := PI - Arctan2(L[1], L[0]); // X, Y determine Longitude
  Modulo2Pi(LocalLongitude);
  Longitude := LocalLongitude;
end;

procedure TArcballSphereTransformation.PrepareTransform;
begin
  inherited PrepareTransform;

  if Radius <> 0 then
    FInvRadius := 1.0 / Radius
  else
    FInvRadius := 0;
end;

procedure TArcballSphereTransformation.ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat);
var
  Dist: TFloat;
  W, L: TVector3f;
const
  OneOverPI: TFloat = 1 / PI;
begin
  // This method maps a screen pixel (DstX, DstY) back to a source bitmap pixel (SrcX, SrcY).

  // 1. Projection: Convert Screen Coordinates to World Space Vector W.
  // Center is the screen center of the sphere. Radius is the sphere radius in pixels.
  DstX := DstX - Center.X; // Map to World Y (Screen Right)
  DstY := Center.Y - DstY; // Map to World Z (Screen Up)
  Dist := DstX * DstX + DstY * DstY;

  // Only pixels inside the sphere radius are projectable.
  if (Dist > Sqr(Radius)) then
  begin
    SrcX := -1;
    SrcY := -1;
    Exit;
  end;

  Dist := Sqrt(Sqr(Radius) - Dist); // Calculate World X (Depth/Screen Forward)

  // Normalize W to unit length
  W[0] := Dist * FInvRadius;
  W[1] := DstX * FInvRadius;
  W[2] := DstY * FInvRadius;

  // 2. Transformation: Map World Space W to Sphere Local Space L.
  // L = R^-1 * W. Since R is an orthogonal rotation matrix, R^-1 = R^T (Transpose).
  // This corresponds to dotting W with the columns of the Local-to-World matrix.
  L[0] := FRotationMatrix[0,0] * W[0] + FRotationMatrix[1,0] * W[1] + FRotationMatrix[2,0] * W[2];
  L[1] := FRotationMatrix[0,1] * W[0] + FRotationMatrix[1,1] * W[1] + FRotationMatrix[2,1] * W[2];
  L[2] := FRotationMatrix[0,2] * W[0] + FRotationMatrix[1,2] * W[1] + FRotationMatrix[2,2] * W[2];

  // 3. Mapping: Convert Local Space L to Source Bitmap Coordinates (Spherical Projection).
  // SrcX = Longitude [0, 2*PI]
  SrcX := Arctan2(L[1], L[0]) + PI;
  Modulo2Pi(SrcX);

  // SrcY = Latitude (Inverted) [0, PI]
  SrcY := ArcCos(L[2]);

  // Scale to source bitmap dimensions.
  SrcX := SrcRect.Left + SrcX * (SrcRect.Width - 1) * OneOverPI * 0.5;
  SrcY := SrcRect.Top + SrcY * (SrcRect.Height - 1) * OneOverPI;
end;


//------------------------------------------------------------------------------
//
//      TFormMain
//
//------------------------------------------------------------------------------
constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;

  FBitmap := TBitmap32.Create(TMemoryBackend);
  FBitmap.ResamplerClassName := 'TLinearResampler';
  FBitmap.Resampler.PixelAccessMode := pamTransparentEdge;

  FTransformation := TArcballSphereTransformation.Create;
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
  FTransformation.SyncFromAngles;
end;

//------------------------------------------------------------------------------

function TFormMain.GetSpherePoint(X, Y: Integer): TVector3f;
var
  DX, DY, Dist, R, d, Angle: TFloat;
  DeltaSqrDist: TFloat;
  OneOverR: TFloat;
  CosAngleOverDist: TFloat;
begin
  // This function maps a 2D mouse position to a 3D unit vector on (or around) the sphere.
  // It is used to calculate the rotation delta between mouse moves.

  R := FTransformation.Radius;
  DX := X - FTransformation.Center.X; // Mouse relative to center (Horizontal)
  DY := FTransformation.Center.Y - Y; // Mouse relative to center (Vertical, inverted)
  Dist := DX * DX + DY * DY;

  DeltaSqrDist := Sqr(R) - Dist;
  OneOverR := 1 / R;

  if (DeltaSqrDist < 0) then
  begin
    // Outside the sphere: Use a "polar trackball" projection.
    // As the mouse moves further away, the sphere continues to rotate at a constant
    // angular rate proportional to the distance from the edge.
    // We use a projection that starts from the edge (where Depth X = 0) to ensure
    // a smooth transition with no continuity jump.
    d := Sqrt(Dist);
    Angle := (d - R) * OneOverR; // Angular displacement from the edge
    GR32_Math.SinCos(Angle, Result[0], CosAngleOverDist);
    CosAngleOverDist := CosAngleOverDist / d;
    Result[0] := -Result[0]; // Depth component (starting from 0 and going negative)
    Result[1] := DX * CosAngleOverDist; // Horizontal component (starting from edge)
    Result[2] := DY * CosAngleOverDist; // Vertical component (starting from edge)
  end else
  begin
    // Inside the sphere: Standard orthographic projection.
    // The mouse point is projected vertically onto the front surface of the sphere.
    Result[0] := Sqrt(DeltaSqrDist) * OneOverR; // Z = Sqrt(1 - X^2 - Y^2)
    Result[1] := DX * OneOverR;
    Result[2] := DY * OneOverR;
  end;
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
  FDownMousePos := FLastMousePos;
  FStartRotation := FTransformation.Rotation;
  FCurrentRasterizer := FDraftRasterizer;
end;

procedure TFormMain.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  DeltaX, DeltaY: Integer;
  V1, V2: TVector3f;
begin
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;

  if ([ssLeft, ssRight] * Shift = []) then
    Exit;

  TimerRotate.Enabled := False;
  DeltaX := X - FLastMousePos.X;
  DeltaY := Y - FLastMousePos.Y;

  if (Abs(X - FDownMousePos.X) <= 2) and (Abs(Y - FDownMousePos.Y) <= 2) then
    Exit;

  TimerMove.Enabled := False;

  if (ssLeft in Shift) then
  begin
    // Arcball Rotate
    V1 := GetSpherePoint(FDownMousePos.X, FDownMousePos.Y);
    V2 := GetSpherePoint(X, Y);
    // Orient_new = dQ * Orient_start
    FTransformation.Rotation := TQuaternion.FromTwoVectors(V1, V2).Multiply(FStartRotation).Normalize;
    FTransformation.UpdateMatrix;
    FTransformation.SyncToAngles;
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
const
  sHelp =
    'Mouse-left: Rotate'#13+
    'Mouse-right: Pan'#13+
    'Mouse-scroll: Zoom'#13+
    'Dbl-click: Animate'#13+
    '+ / -: Animate faster/slower'#13+
    'S: Star field'#13+
    'R: Reset to origin'#13+
    '[Esc]: Quit';
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

  r := ClientRect;
  PaintBox32.Buffer.Font.Color := clSkyBlue;
  PaintBox32.Buffer.Textout(r, DT_RIGHT or DT_TOP or DT_CALCRECT, sHelp);
  // 4 pixel margin from Top/Right
  GR32.InflateRect(r, 4, 4);
  GR32.OffsetRect(r, ClientWidth - r.Left - r.Width - 4, -r.Top + 4);
  PaintBox32.Buffer.FillRectTS(r, $7F00008B);
  PaintBox32.Buffer.FrameRectS(r, clRed32);
  PaintBox32.Buffer.Textout(r, DT_CENTER or DT_VCENTER, sHelp);
end;

//------------------------------------------------------------------------------

procedure TFormMain.Reset;
begin
  FTransformation.Longitude := DefaultLongitude;
  FTransformation.Latitude := DefaultLatitude;
  FTransformation.SyncFromAngles;
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
var
  Q: TQuaternion;
  Axis: TVector3f;
begin
  // Rotate around globe's own axis (Z local)
  Axis[0] := 0; Axis[1] := 0; Axis[2] := 1;
  Q := TQuaternion.FromAxisAngle(Axis, -FRotationDelta);
  FTransformation.RotateLocal(Q);

  PaintBox32.Invalidate;
end;

//------------------------------------------------------------------------------

end.
