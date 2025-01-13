unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Diagnostics,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32.Paint.API,
  GR32.Paint.Tool,
  GR32.Paint.Tool.API;

//------------------------------------------------------------------------------

type
  // Interposer
  // Redirects WM_PAINT handling to UpdateLayeredWindow
  TImage32 = class(GR32_Image.TImage32)
  private
    FUpdateTimer: TStopwatch;
  protected
    procedure Paint; override;
    function GetNeedUpdate: boolean;
  public
    procedure Invalidate; override;

    // NeedUpdate: True if more than 50 mS has elapsed since image was invalidated
    property NeedUpdate: boolean read GetNeedUpdate;
  end;

//------------------------------------------------------------------------------
{$define BUFFER_IS_LAYER}

type
  TFormMain = class(TForm, IBitmap32PaintHost)
    Image32: TImage32;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    procedure PaintForm;
  private
    // Update batching
    FUpdateCount: integer;
    FLockCount: integer;
    FModified: boolean;

    procedure BeginUpdate;
    procedure Changed;
    procedure EndUpdate;
    procedure BeginLockUpdate;
    procedure EndLockUpdate;

    property Modified: boolean read FModified;
    property UpdateCount: integer read FUpdateCount;
    property LockCount: integer read FLockCount;
  private
    // Mouse state
    FMouseShift: TShiftState;
    FLastMousePos: TPoint;
    FLastMouseMessageTime: Cardinal;
  private
    FPaintLayer: TBitmapLayer;
  private
    // Bitmap editor stuff
    FColorPrimary: TColor32;
    FColorSecondary: TColor32;
    FPaintTool: IBitmap32PaintTool; // The selected tool
    FActivePaintTool: IBitmap32PaintTool; // The tool that is currently handling mouse messages. Nil isf none.
    FActivePaintToolContext: IBitmap32PaintToolContext;
  private
    procedure UpdateToolContext(const PaintToolContext: IBitmap32PaintToolContext; const ViewPortPos: TPoint; SnapMouse: boolean);
    function BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
    function ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
    procedure EndOperation(Complete: boolean);
    procedure UpdateToolCursor;
    procedure SetToolCursor(NewCursor: TCursor);
    procedure SetActivePaintTool(const Value: IBitmap32PaintTool);
    procedure SetPaintTool(const Value: IBitmap32PaintTool);
    property PaintTool: IBitmap32PaintTool read FPaintTool write SetPaintTool;
    property ActivePaintTool: IBitmap32PaintTool read FActivePaintTool write SetActivePaintTool;
  private
    // IBitmap32PaintHost
    function GetColorPrimary: TColor32;
    procedure SetColorPrimary(const Value: TColor32);
    function GetColorSecondary: TColor32;
    procedure SetColorSecondary(const Value: TColor32);

    function GetMagnification: Single;
    procedure SetMagnification(const Value: Single);

    function ViewPortToScreen(const APoint: TPoint): TPoint;
    function ScreenToViewPort(const APoint: TPoint): TPoint;
    function ViewPortToBitmap(const APoint: TPoint; SnapToNearest: boolean = True): TPoint;
    function BitmapToViewPort(const APoint: TPoint): TPoint;

    function GetToolSettings(const AToolKey: string): ISettingValues;

    function CreateToolContext(const ViewPortPos: TPoint; SnapMouse: boolean): IBitmap32PaintToolContext;

    function SetToolVectorCursor(const Polygon: TArrayOfFixedPoint; HotspotX, HotspotY: integer; Color: TColor32; const OutlinePattern: TArrayOfColor32): boolean;
    procedure PaintHostChanged(const Action: string);
    procedure IBitmap32PaintHost.Changed = PaintHostChanged;
  protected
    procedure CreateWindowHandle(const Context: TCreateParams); override;
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  GR32.Paint.Tool.Pen,
  GR32.Paint.Tool.Brush;

type
  TLUT8 = array[byte] of byte;
  PLUT8 = ^TLUT8;

var
  PremultiplyLUT: array[byte] of TLUT8;

type
  TImage32Cracker = class(TImage32);

//------------------------------------------------------------------------------

constructor TFormMain.Create(AOwner: TComponent);

  procedure SetupBitmapEditor;
  begin
    FColorPrimary := clWhite32;
    FColorSecondary := clBlack32;
//    FPaintTool := TBitmap32PaintToolPen.Create(Self);
    FPaintTool := TBitmap32PaintToolCircularBrush.Create(Self);
    TBitmap32PaintToolCircularBrush(FPaintTool).BrushSize := 50;
  end;

begin
  inherited;

  SetupBitmapEditor;

  SetBounds(0, 0, Monitor.Width, Monitor.Height);

  // We never resize the image so ensure that the buffer fits the output area exactly
  Image32.BufferOversize := 0;

  // Load a bitmap so we have something to look at. This is completely optional.
  Image32.Bitmap.LoadFromResourceName(HInstance, 'DICE', 'PNG');

  // Create a bitmap layer we can paint on.
  // We could also just have painted directly on the TImage32.Bitmap
  FPaintLayer := Image32.Layers.Add<TBitmapLayer>;
  FPaintLayer.Location := FloatRect(Image32.BoundsRect);
  FPaintLayer.Bitmap.SetSize(Image32.Width, Image32.Height);
  FPaintLayer.Bitmap.DrawMode := dmBlend;
  // Since we are blending onto a transparent bitmap, and we need that bitmap to
  // stay transparent, we must use the Merge combine mode.
  FPaintLayer.Bitmap.CombineMode := cmMerge;

  // Do not clear the TImage32 background; We need it transparent
  if (Image32.PaintStages[0].Stage = PST_CLEAR_BACKGND) then
    Image32.PaintStages[0].Stage := PST_CUSTOM;
end;

destructor TFormMain.Destroy;
begin
  SetPaintTool(nil);

  FPaintTool := nil;
  FActivePaintTool := nil;
  FActivePaintToolContext := nil;

  inherited;
end;

procedure TFormMain.CreateWindowHandle(const Context: TCreateParams);
var
  ExStyle: NativeInt;
begin
  inherited;

  // An alpha-blended, transparent windows must use the WS_EX_LAYERED
  // windows style.
  // https://learn.microsoft.com/en-us/windows/win32/winmsg/extended-window-styles#WS_EX_LAYERED
  // https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setlayeredwindowattributes

  // CreateWindowHandle removes WS_EX_LAYERED so we can't set it in CreateParams,
  // before the handle is created.
  // Instead we must set it here, after the handle has been created.
  ExStyle := GetWindowLongA(Handle, GWL_EXSTYLE);
  if (ExStyle and WS_EX_LAYERED = 0) then
    SetWindowLong(Handle, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);
end;

//------------------------------------------------------------------------------

procedure TFormMain.DoShow;
begin
  inherited;

  // Initial paint
  Changed;
end;

//------------------------------------------------------------------------------
// IBitmap32PaintHost
//------------------------------------------------------------------------------

function TFormMain.GetColorPrimary: TColor32;
begin
  Result := FColorPrimary;
end;

procedure TFormMain.SetColorPrimary(const Value: TColor32);
begin
  FColorPrimary := Value;
end;

function TFormMain.GetColorSecondary: TColor32;
begin
  Result := FColorSecondary;
end;

procedure TFormMain.SetColorSecondary(const Value: TColor32);
begin
  FColorSecondary := Value;
end;

function TFormMain.GetMagnification: Single;
begin
  Result := Image32.Scale;
end;

function TFormMain.GetToolSettings(const AToolKey: string): ISettingValues;
begin
  Result := nil;
end;

procedure TFormMain.SetMagnification(const Value: Single);
begin
  Image32.Scale := Value;
end;

function TFormMain.ViewPortToScreen(const APoint: TPoint): TPoint;
begin
  Result := Image32.ClientToScreen(APoint);
end;

function TFormMain.ScreenToViewPort(const APoint: TPoint): TPoint;
begin
  Result := Image32.ScreenToClient(APoint);
end;

function TFormMain.ViewPortToBitmap(const APoint: TPoint; SnapToNearest: boolean = True): TPoint;
var
  SnapThreshold: integer;
begin
  if (SnapToNearest) then
  begin
    SnapThreshold := Trunc((Image32.Scale-1) / 2);

    // Snap the coordinates to the nearest pixel
    Result := GR32.Point(APoint.X + SnapThreshold, APoint.Y + SnapThreshold);
  end else
    Result := APoint;

{$ifdef BUFFER_IS_LAYER}
  Result := FPaintLayer.ControlToLayer(Result);
{$else}
  Result := Image32.ControlToBitmap(Result);
{$endif}
end;

function TFormMain.BitmapToViewPort(const APoint: TPoint): TPoint;
begin
{$ifdef BUFFER_IS_LAYER}
  Result := FPaintLayer.LayerToControl(APoint);
{$else}
  Result := Image32.BitmapToControl(APoint);
{$endif}
end;

function TFormMain.CreateToolContext(const ViewPortPos: TPoint; SnapMouse: boolean): IBitmap32PaintToolContext;
begin
{$ifdef BUFFER_IS_LAYER}
  Result := TBitmap32PaintToolContext.Create(FPaintLayer.Bitmap);
{$else}
  Result := TBitmap32PaintToolContext.Create(Image32.Bitmap);
{$endif}

  UpdateToolContext(Result, ViewPortPos, SnapMouse);
end;

procedure TFormMain.PaintHostChanged(const Action: string);
begin
end;


//------------------------------------------------------------------------------

procedure TFormMain.UpdateToolContext(const PaintToolContext: IBitmap32PaintToolContext; const ViewPortPos: TPoint;
  SnapMouse: boolean);
var
  MouseParams: PBitmap32PaintToolMouseParams;
begin
  MouseParams := PaintToolContext.MouseParams;

  MouseParams.ViewPortPos := ViewPortPos;
  MouseParams.ScreenPos := ViewPortToScreen(MouseParams.ViewPortPos);
{$ifdef BUFFER_IS_LAYER}
  MouseParams.BitmapPosFloat := FPaintLayer.ControlToLayer(FloatPoint(MouseParams.ViewPortPos));
{$else}
  MouseParams.BitmapPosFloat := Image32.ControlToBitmap(FloatPoint(MouseParams.ViewPortPos));
{$endif}
  // If SnapMouse=True then we only return the snapped coordinates
  MouseParams.BitmapPos := ViewPortToBitmap(MouseParams.ViewPortPos, SnapMouse);
  MouseParams.BitmapPosSnap := ViewPortToBitmap(MouseParams.ViewPortPos, True);
end;

function TFormMain.BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
var
  ToolState: TBitmap32PaintToolState;
  Continue: boolean;
begin
  Assert(FPaintTool <> nil);
  Continue := True;

  FPaintTool.BeginTool(Continue);
  try
    if (not Continue) then
    begin
      ToolState := tsAbort;
      Exit;
    end;

    if (FActivePaintTool = nil) then
      BeginUpdate;
    try

      ToolState := tsContinue;
      try

        FPaintTool.BeginAction(Context, ToolState);

      except
        ToolState := tsAbort;
        FPaintTool.EndAction(Context, ToolState);
        raise;
      end;

      case ToolState of
        tsComplete:
          ;

        tsAbort:
          ;

        tsContinue:
          begin
            SetActivePaintTool(FPaintTool);
            FActivePaintToolContext := Context;

            // Reacquire capture in case tool did something stupid that caused us to lose it (e.g. Move Select tool)
            if (not (betfMouseCapture in FPaintTool.ToolFeatures)) and (GetCapture <> Image32.Handle) then
              SetCapture(Image32.Handle);
          end;
      end;

    finally
      if (ToolState <> tsContinue) then
        EndUpdate;
    end;
  finally
    Result := (ToolState = tsContinue);

    if (not Result) then
    begin
      FPaintTool.EndTool;
      FMouseShift := [];
      SetActivePaintTool(nil);
    end;
  end;

  Image32.Update;
end;

function TFormMain.ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
var
  ToolState: TBitmap32PaintToolState;
  Stopwatch: TStopwatch;
begin
  if (FActivePaintTool = nil) then
    raise Exception.Create('Operation is not in progress');

  Result := False;

  Stopwatch := TStopwatch.StartNew;

  ToolState := tsContinue;
  try

    try
      FActivePaintTool.ContinueAction(FActivePaintToolContext, ToolState);

      // Note: EndOperation may be called from Tool.ContinueAction so we must
      // not assume that the tool is still active when ContinueAction returns.
      if (FActivePaintTool = nil) then
        exit;

    except
      ToolState := tsAbort;
      raise;
    end;

    case ToolState of
      tsComplete:
        ;

      tsAbort:
        ;

      tsContinue:
        begin
          Result := True;

          // Reacquire capture in case tool did something stupid that caused us to lose it (e.g. Move Select tool)
          if (not (betfMouseCapture in FPaintTool.ToolFeatures)) and (GetCapture <> Image32.Handle) then
            SetCapture(Image32.Handle);
        end;
    end;

  finally
    if (ToolState <> tsContinue) and (FActivePaintTool <> nil) then
    begin
      EndUpdate;
      FActivePaintTool.EndTool;
      FMouseShift := [];
      SetActivePaintTool(nil);
    end;
  end;

  // Repaint immediately to avoid lag caused by continous mouse messages during the operation.
  // The WM_PAINT messages are only generated once the message queue is otherwise empty or
  // UpdateWindow is called.
  if (Image32.NeedUpdate) then
    Image32.Update;
end;

procedure TFormMain.EndOperation(Complete: boolean);
var
  ToolState: TBitmap32PaintToolState;
begin
  if (FActivePaintTool = nil) then
    exit; // TODO : Is this an error condition?

  if (Complete) then
    ToolState := tsComplete
  else
    ToolState := tsAbort;
  try

    try

      FActivePaintTool.EndAction(FActivePaintToolContext, ToolState);

      if (FActivePaintTool = nil) then
        exit;

    except
      ToolState := tsAbort;
      raise;
    end;

    case ToolState of
      tsComplete:
        ;

      tsAbort:
        ;
    end;

  finally
    if (ToolState <> tsContinue) and (FActivePaintTool <> nil) then
    begin
      EndUpdate;
      FActivePaintTool.EndTool;
      FMouseShift := [];
      SetActivePaintTool(nil);
    end;
  end;

  Image32.Changed;
end;

procedure TFormMain.SetToolCursor(NewCursor: TCursor);

  procedure UpdateCursor;
  var
    p: TPoint;
  begin
    GetCursorPos(p);
    SetCursorPos(p.X, p.Y);
  end;

begin
  if (Image32.Cursor <> NewCursor) then
  begin
    Image32.Cursor := NewCursor;

    // CM_CURSORCHANGED should force the cursor to update, but doesn't
    // ... so we have to resort to this ugly hack:
    UpdateCursor;
  end;
end;

function TFormMain.SetToolVectorCursor(const Polygon: TArrayOfFixedPoint; HotspotX, HotspotY: integer; Color: TColor32;
  const OutlinePattern: TArrayOfColor32): boolean;
begin
  // Not implemented in this example (yet)
  Result := False;
end;

procedure TFormMain.UpdateToolCursor;
var
  NewCursor: TCursor;
begin
  if (FPaintTool = nil) or (not FPaintTool.GetCursor(NewCursor)) then
    NewCursor := Cursor;

  SetToolCursor(NewCursor);
end;

procedure TFormMain.SetActivePaintTool(const Value: IBitmap32PaintTool);
begin
  FActivePaintToolContext := nil;
  FActivePaintTool := Value;
end;

procedure TFormMain.SetPaintTool(const Value: IBitmap32PaintTool);
var
  Continue: boolean;
begin

  if (Value = FPaintTool) then
    exit;

  if (FActivePaintTool <> nil) then
    EndOperation(False);

  // Only activate new tool if we managed to deactivate old tool - or if there was no old tool
  if (FActivePaintTool = nil) then
  begin
    // Hide old cursor
    ShowCursor(False);

    if (FPaintTool <> nil) then
    begin
      FPaintTool.Deactivate;
      FPaintTool := nil;
    end;

    if (Value <> nil) then
    begin
      Continue := True;
      Value.Activate(Continue);

      if (Continue) then
      begin
        FPaintTool := Value;

        // Display new cursor
        UpdateToolCursor;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormMain.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFormMain.Changed;
begin
  if (FLockCount > 0) then
    exit;
  BeginUpdate;
  FModified := True;
  EndUpdate;
end;

procedure TFormMain.EndUpdate;
begin
  if (FUpdateCount = 1) then
  begin
    PaintForm;

    FModified := False;
  end;
  Dec(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TFormMain.BeginLockUpdate;
begin
  Inc(FLockCount);
end;

procedure TFormMain.EndLockUpdate;
begin
  Dec(FLockCount);
end;

//------------------------------------------------------------------------------

procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TFormMain.Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Tool: IBitmap32PaintTool;
  ToolContext: IBitmap32PaintToolContext;
begin
  if (Layer <> FPaintLayer) then
    exit;

  if (FActivePaintTool = nil) then
    Tool := FPaintTool
  else
    Tool := FActivePaintTool;

  if (Tool = nil) then
    exit;

  // Save time of last mouse down message (for use in mouse movement history)
  FLastMouseMessageTime := Cardinal(GetMessageTime);

  ToolContext := CreateToolContext(GR32.Point(X, Y), Tool.SnapMouse);

  ToolContext.MouseParams.ShiftState := Shift;
  ToolContext.MouseParams.MouseMessageTime := FLastMouseMessageTime;

  // Save last mouse pos in screen coordinates for use with GetMouseMovePointsEx stuff
  FLastMousePos := ToolContext.MouseParams.ScreenPos;

  Tool.MouseDown(Button, ToolContext);

  // Prevent nested operations. Happens if you start an operation with mbLeft and
  // then press mbRight during the operation.
  if (FActivePaintTool <> nil) then
    exit;

  // BeginOperation will save ToolContext as FActivePaintToolContext if the operation is accepted
  BeginOperation(ToolContext);
end;

procedure TFormMain.Image32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
const
  MaxMouseMovePointCount = 64;
var
  MouseMovePoint: TMouseMovePoint;
  MouseMovePoints: array[0..MaxMouseMovePointCount-1] of TMouseMovePoint;
  MouseMovePointCount: integer;
  MouseMovePointIndex: integer;
  ScreenPos: TPoint;
  LastMouseMessageTime: Cardinal;
  LastViewPortPos: TPoint;
  LastShiftState: TShiftState;
begin
  if (Layer <> FPaintLayer) then
    exit;

  if (FActivePaintTool = nil) then
    exit;

  Assert(FActivePaintToolContext <> nil);

  LastMouseMessageTime := Cardinal(GetMessageTime);

  ScreenPos := ViewPortToScreen(GR32.Point(X, Y));

  // Ignore if mouse didn't move and shift state didn't change.
  // Note: There is an ABA race condition here if mouse moves from A to B to A.
  if (ScreenPos.X = FActivePaintToolContext.MouseParams.ScreenPos.X) and (ScreenPos.Y = FActivePaintToolContext.MouseParams.ScreenPos.Y) and (Shift = FActivePaintToolContext.MouseParams.ShiftState) then
//  if (ScreenPos.X = FLastMousePos.X) and (ScreenPos.Y = FLastMousePos.Y) and (Shift = FActivePaintToolContext.MouseParams.ShiftState) then
    exit;

  // Fetch history of mouse movement.
  // Get mouse coordinates up to and including the current movement message.
  // Note: GetMouseMovePointsEx is theoretically subject to ABA race condition.
  MouseMovePoint := Default(TMouseMovePoint);
  MouseMovePoint.x := ScreenPos.X and $0000FFFF; // Ensure that this number will pass through.
  MouseMovePoint.y := ScreenPos.Y and $0000FFFF;
  Cardinal(MouseMovePoint.time) := LastMouseMessageTime;

  MouseMovePointCount := GetMouseMovePointsEx(SizeOf(MouseMovePoint), MouseMovePoint, MouseMovePoints[0], MaxMouseMovePointCount, GMMP_USE_DISPLAY_POINTS);

  if (MouseMovePointCount = -1) then
  begin
    // If no history was retrieved we just store the current position in the history and proceed with that.
    MouseMovePointCount := 1;
    MouseMovePoints[0].x := ScreenPos.X;
    MouseMovePoints[0].y := ScreenPos.Y;
    Cardinal(MouseMovePoints[0].time) := LastMouseMessageTime;
  end else
  begin
    // Discard history older than last point we processed. Entries are stored most recent first.
    MouseMovePointIndex := 0;
    while (MouseMovePointIndex < MouseMovePointCount) do
    begin
      // Handle negative coordinates - required for multi monitor
      // TODO : Better handling of this; See GetMouseMovePointsEx documentation
      if (DWORD(MouseMovePoints[MouseMovePointIndex].x) >= $8000) then
        Dec(MouseMovePoints[MouseMovePointIndex].x, $00010000);

      if (DWORD(MouseMovePoints[MouseMovePointIndex].y) >= $8000) then
        Dec(MouseMovePoints[MouseMovePointIndex].y, $00010000);

      if (Cardinal(MouseMovePoints[MouseMovePointIndex].time) < FLastMouseMessageTime) or
        ((Cardinal(MouseMovePoints[MouseMovePointIndex].time) = FLastMouseMessageTime) and
         (MouseMovePoints[MouseMovePointIndex].x = FLastMousePos.X) and (MouseMovePoints[MouseMovePointIndex].y = FLastMousePos.Y)) then
      begin
        MouseMovePointCount := MouseMovePointIndex+1;
        break;
      end;

      Inc(MouseMovePointIndex);
    end;
  end;

  FLastMousePos := ScreenPos;
  FLastMouseMessageTime := LastMouseMessageTime;
  LastViewPortPos := FActivePaintToolContext.MouseParams.ViewPortPos;
  LastShiftState := FActivePaintToolContext.MouseParams.ShiftState;

  MouseMovePointIndex := MouseMovePointCount-1;

  FActivePaintToolContext.MouseParams.ShiftState := Shift + FMouseShift;

  while (MouseMovePointIndex >= 0) do
  begin
    ScreenPos.X := MouseMovePoints[MouseMovePointIndex].x;
    ScreenPos.Y := MouseMovePoints[MouseMovePointIndex].y;

    UpdateToolContext(FActivePaintToolContext, ScreenToViewPort(ScreenPos), FActivePaintTool.SnapMouse);

    FActivePaintToolContext.MouseParams.MouseMessageTime := Cardinal(MouseMovePoints[MouseMovePointIndex].time);

    if (FActivePaintToolContext.MouseParams.ViewPortPos <> LastViewPortPos) or (FActivePaintToolContext.MouseParams.ShiftState <> LastShiftState) then
    begin
      FActivePaintTool.MouseMove(FActivePaintToolContext);

      if (FActivePaintTool <> nil) then
      begin
        if (not ContinueOperation(FActivePaintToolContext)) then
          break;
      end;
    end;

    Dec(MouseMovePointIndex);
  end;
end;

procedure TFormMain.Image32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  // TCustomImage32.DblClick calls MouseUp(mbLeft, [], 0, 0) - ignore this
  if (Button = mbLeft) and (Shift = []) and (X = 0) and (Y = 0) then
    exit;

  if (FActivePaintTool = nil) then
    exit;

  UpdateToolContext(FActivePaintToolContext, GR32.Point(X, Y), FActivePaintTool.SnapMouse);
  FActivePaintToolContext.MouseParams.ShiftState := Shift + FMouseShift;
  FActivePaintToolContext.MouseParams.MouseMessageTime := Cardinal(GetMessageTime);

  FLastMousePos := FActivePaintToolContext.MouseParams.ScreenPos;

  FActivePaintTool.MouseUp(Button, FActivePaintToolContext);

  if (FActivePaintTool <> nil) then
  begin
    EndOperation(True);

    // Ensure mouse capture is released (this takes care of right-button which TImage32 doesn't handle properly)
    if (FActivePaintTool = nil) and (GetCapture = Image32.Handle) then
      ReleaseCapture;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormMain.Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
begin
  // Fully transparent background
  Buffer.Clear(0);
end;

//------------------------------------------------------------------------------

procedure TFormMain.PaintForm;
begin
  // Have the TImage32.Buffer paint itself to the screen via our interposer
  Image32.Paint;
end;

function TImage32.GetNeedUpdate: boolean;
begin
  Result := (FUpdateTimer.IsRunning) and (FUpdateTimer.ElapsedMilliseconds > 50);
end;

procedure TImage32.Invalidate;
begin
  inherited;

  if (not FUpdateTimer.IsRunning) then
    FUpdateTimer := TStopwatch.StartNew;
end;

//------------------------------------------------------------------------------
//
//      TImage32 interposer
//
//------------------------------------------------------------------------------
procedure TImage32.Paint;

  procedure PremultiplyBitmap(Bitmap: TBitmap32);
  var
    p: PColor32Entry;
    i: integer;
    PreMult: PLUT8;
  begin
    p := PColor32Entry(Bitmap.Bits);
    for i := 0 to Bitmap.Height*Bitmap.Width-1 do
    begin
      PreMult := @PremultiplyLUT[p.A];
      p.R := PreMult[p.R];
      p.G := PreMult[p.G];
      p.B := PreMult[p.B];
      inc(p);
    end;
  end;

  procedure MakeBitmapOpaque(Bitmap: TBitmap32);
  var
    p: PColor32Entry;
    i: integer;
  begin
    p := PColor32Entry(Bitmap.Bits);
    for i := 0 to Bitmap.Height*Bitmap.Width-1 do
    begin
      if (p.A = 0) then
        p.ARGB := $01000000; // Almost transparent, not visuall noticeable
      Inc(p);
    end;
  end;

var
  BlendFunction: TBlendFunction;
  BitmapPos: TPoint;
  BitmapSize: TSize;
  ParentForm: TWinControl;
begin
  FUpdateTimer.Stop;

  // Have TImage32 update the buffer
  DoPaintBuffer;

  // UpdateLayeredWindow needs alpha-premultiple ARGB
  PremultiplyBitmap(Buffer);

  // Make bitmap "not fully transparent" so we don't click through the transparent areas.
  // Disable this to have the form behave as a transparent form.
  MakeBitmapOpaque(Buffer);

  // Find parent form
  ParentForm := Self.Parent;
  while (ParentForm.Parent <> nil) do
    ParentForm := ParentForm.Parent;

  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := 255;
  BlendFunction.AlphaFormat := AC_SRC_ALPHA;

  BitmapPos := Point(0, 0);
  BitmapSize.cx := Buffer.Width;
  BitmapSize.cy := Buffer.Height;

  if (not UpdateLayeredWindow(ParentForm.Handle, 0, nil, @BitmapSize, Buffer.Canvas.Handle, @BitmapPos, 0, @BlendFunction, ULW_ALPHA)) then
    RaiseLastOSError;
end;

//------------------------------------------------------------------------------

procedure SetupPremultiplyLUT;
var
  Row, Col: integer;
begin
  for Row := 0 to 255 do
    for Col := Row to 255 do
    begin
      PremultiplyLUT[Row, Col] := Row * Col div 255;
      if (Row <> Col) then
        PremultiplyLUT[Col, Row] := PremultiplyLUT[Row, Col]; // a*b = b*a
    end;
end;

initialization
  SetupPremultiplyLUT;
end.
