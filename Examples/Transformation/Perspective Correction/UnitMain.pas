unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  GR32,
  GR32_Transforms,
  GR32_Image,
  GR32_Layers;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    CheckBoxLive: TCheckBox;
    ButtonApply: TButton;
    CheckBoxExtrapolate: TCheckBox;
    ImageSource: TImage32;
    ImageDest: TImage32;
    TimerMarchingAnts: TTimer;
    TimerUpdate: TTimer;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerMarchingAntsTimer(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure CheckBoxExtrapolateClick(Sender: TObject);
    procedure CheckBoxLiveClick(Sender: TObject);
  private type
    TSourceDest = (sdSource, sdDest);
  private
    FTransformation: TProjectiveTransformationEx;
    FLayers: array[TSourceDest] of TPolygonRubberbandLayer;
    FCorners: array[TSourceDest] of TFloatQuadrilateral;
    FActiveIndex: array[TSourceDest] of integer;
    FInvalidIndex: array[TSourceDest] of integer;
  private
    procedure LayerHandleClicked(Sender: TCustomRubberBandLayer; AIndex: integer);
    procedure LayerHandleMove(Sender: TCustomRubberBandLayer; AIndex: integer; var APos: TFloatPoint);
    procedure LayerHandlePaint(Sender: TCustomRubberBandLayer; Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer; var Handled: boolean);
    procedure LayerHandleUpdate(Sender: TCustomRubberBandLayer; Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer; var UpdateRect: TRect; var Handled: boolean);
    procedure LayerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    function IsCornerValid(const Quad: TFloatQuadrilateral; Index, ActiveIndex: integer): boolean;
    function MoveCorner(SourceDest: TSourceDest; var APos: TFloatPoint; ASnap: boolean): boolean;
    function SortCorners(SourceDest: TSourceDest): boolean;
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

uses
  Math,
  GR32_Geometry,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_Rasterizers,
  GR32.ImageFormats.JPG,
  GR32.Examples;

const
  FirstOutlineWidth = 1.5; // Width of first handle
  OtherOutlineWidth = 1.0; // Width of other handles
  ColorHandleFill: TColor32 = $30FFFFFF;
  ColorHandleActive: TColor32 = $7F007FFF;
  ColorHandleError: TColor32 = $FFFF0000;
  ColorHandleOutline: TColor32 = $FF00007F;

//------------------------------------------------------------------------------
constructor TFormMain.Create(AOwner: TComponent);
var
  SourceDest: TSourceDest;
begin
  inherited;

  ImageSource.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder + '\Notre Dame.jpg');
  ImageSource.Bitmap.ResamplerClassName := 'TLinearResampler';
  // ImageSource.Bitmap.ResamplerClassName := 'TDraftResampler';
  ImageDest.Bitmap.Assign(ImageSource.Bitmap);
  ImageSource.Scale := 0.5;
  ImageDest.Scale := 0.5;

  FLayers[sdSource] := ImageSource.Layers.Add<TPolygonRubberbandLayer>;
  FLayers[sdDest] := ImageDest.Layers.Add<TPolygonRubberbandLayer>;

  for SourceDest := Low(TSourceDest) to High(TSourceDest) do
  begin
    FLayers[SourceDest].Scaled := True;
    FLayers[SourceDest].Cursor := crSizeAll;
    FLayers[SourceDest].SetFrameStipple([clWhite32, clWhite32, clWhite32, clWhite32, clBlack32, clBlack32, clBlack32, clBlack32]);
    FLayers[SourceDest].HandleSize := 5;
    FLayers[SourceDest].OnHandleClicked := LayerHandleClicked;
    FLayers[SourceDest].OnHandleMove := LayerHandleMove;
    FLayers[SourceDest].OnMouseUp := LayerMouseUp;
    FLayers[SourceDest].OnPaintHandle := LayerHandlePaint;
    FLayers[SourceDest].OnUpdateHandle := LayerHandleUpdate;

    FActiveIndex[SourceDest] := -1;
    FInvalidIndex[SourceDest] := -1;
  end;

  FTransformation := TProjectiveTransformationEx.Create;
end;

destructor TFormMain.Destroy;
begin
  FTransformation.Free;

  inherited;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  ImageSource.Width := ClientWidth div 2;

  // Center bitmap in viewport.
  // The reason we don't just use BitmapAlign=baCenter is that
  // we would also like to be able to pan the image with the mouse.
  ImageSource.OffsetHorz := (ImageSource.Width - ImageSource.Bitmap.Width * ImageSource.Scale) * 0.5;
  ImageSource.OffsetVert := (ImageSource.Height - ImageSource.Bitmap.Height * ImageSource.Scale) * 0.5;

  ImageDest.OffsetHorz := (ImageDest.Width - ImageDest.Bitmap.Width * ImageDest.Scale) * 0.5;
  ImageDest.OffsetVert := (ImageDest.Height - ImageDest.Bitmap.Height * ImageDest.Scale) * 0.5;
end;

procedure TFormMain.FormShow(Sender: TObject);

  function RectToPolygon(const r: TFloatRect): TArrayOfFloatPoint;
  begin
    SetLength(Result, 4);
    Result[0].X := r.Left;
    Result[0].Y := r.Top;
    Result[1].X := r.Right;
    Result[1].Y := r.Top;
    Result[2].X := r.Right;
    Result[2].Y := r.Bottom;
    Result[3].X := r.Left;
    Result[3].Y := r.Bottom;
  end;

var
  Points: TArrayOfFloatPoint;
  SourceDest: TSourceDest;
  i: integer;
begin
  FLayers[sdSource].Location := FloatRect(ImageSource.GetBitmapRect);
  Points := BuildPolygonF([250.25, 45.25, 537.25, 49, 720, 532.5, 52.5, 532.5]);
  FLayers[sdSource].Vertices := TranslatePolygon(Points, FLayers[sdSource].Location.Left, FLayers[sdSource].Location.Top);
  // FLayerSource.Vertices := RectToPolygon(FLayerSource.Location);

  FLayers[sdDest].Location := FloatRect(ImageDest.GetBitmapRect);
  Points := BuildPolygonF([252, 50, 534, 50, 534, 529, 252, 529]);
  FLayers[sdDest].Vertices := TranslatePolygon(Points, FLayers[sdDest].Location.Left, FLayers[sdDest].Location.Top);
  // FLayerDest.Vertices := RectToPolygon(FLayerDest.Location);

  for SourceDest := Low(TSourceDest) to High(TSourceDest) do
    for i := Low(FCorners[SourceDest]) to High(FCorners[SourceDest]) do
      FCorners[SourceDest, i] := FLayers[SourceDest].Vertex[i];
end;

procedure TFormMain.CheckBoxExtrapolateClick(Sender: TObject);
begin
  if (CheckBoxLive.Checked) then
    ButtonApply.Click;
end;

procedure TFormMain.CheckBoxLiveClick(Sender: TObject);
begin
  if (CheckBoxLive.Checked) then
    ButtonApply.Click;
end;

procedure TFormMain.LayerHandleClicked(Sender: TCustomRubberBandLayer; AIndex: integer);
var
  SourceDest: TSourceDest;
begin
  if (Sender = FLayers[sdSource]) then
    SourceDest := sdSource
  else
    SourceDest := sdDest;

  FActiveIndex[SourceDest] := AIndex;
  Sender.Update;
end;

procedure TFormMain.LayerHandleMove(Sender: TCustomRubberBandLayer; AIndex: integer; var APos: TFloatPoint);
var
  SourceDest: TSourceDest;
  i: integer;
  Snap: boolean;
  HitTestVertex: ILayerHitTestVertex;
begin
  if (Sender = FLayers[sdSource]) then
    SourceDest := sdSource
  else
    SourceDest := sdDest;

  if (AIndex <> -1) then
  begin
    Snap := (ssShift in Sender.CurrentHitTest.Shift);

    if (not MoveCorner(SourceDest, APos, Snap)) then
      exit;
  end;

  if (CheckBoxLive.Checked) then
  begin
    TimerUpdate.Enabled := False;
    TimerUpdate.Enabled := True;
  end;

  if (SortCorners(SourceDest)) then
  begin
    // Corners has been reordered; Update vertices and hittest
    for i := Low(FCorners[SourceDest]) to High(FCorners[SourceDest]) do
      FLayers[SourceDest].Vertex[i] := FCorners[SourceDest, i];

    if Supports(Sender.CurrentHitTest, ILayerHitTestVertex, HitTestVertex) then
      HitTestVertex.Vertex := FActiveIndex[SourceDest];
  end;

  // Determine if polygon is convex
  FInvalidIndex[SourceDest] := -1;
  for i := Low(FCorners[SourceDest]) to High(FCorners[SourceDest]) do
    if (not IsCornerValid(FCorners[SourceDest], i, FActiveIndex[SourceDest])) then
    begin
      FInvalidIndex[SourceDest] := i;
      Sender.Update;
      break;
    end;
end;

procedure TFormMain.LayerHandlePaint(Sender: TCustomRubberBandLayer; Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer;
  var Handled: boolean);
var
  SourceDest: TSourceDest;
  Handle: TArrayOfFloatPoint;
  Outline: TArrayOfArrayOfFloatPoint;
  OutlineWidth: Single;
begin
  if (AIndex = -1) then
    exit;

  if (AIndex = 0) then
    OutlineWidth := FirstOutlineWidth
  else
    OutlineWidth := OtherOutlineWidth;
  Handle := Circle(p, Sender.HandleSize);
  Outline := BuildPolyPolyLine(PolyPolygon(Handle), True, OutlineWidth);

  if (Sender = FLayers[sdSource]) then
    SourceDest := sdSource
  else
    SourceDest := sdDest;

  var Renderer := TPolygonRenderer32VPR.Create(Buffer);
  try

    if (AIndex = FInvalidIndex[SourceDest]) then
      Renderer.Color := ColorHandleError
    else
    if (AIndex = FActiveIndex[SourceDest]) then
      Renderer.Color := ColorHandleActive
    else
      Renderer.Color := ColorHandleFill;

    Renderer.PolygonFS(Handle);
    Renderer.Color := ColorHandleOutline;
    Renderer.PolyPolygonFS(Outline);

  finally
    Renderer.Free;
  end;

  Handled := True;
end;

procedure TFormMain.LayerHandleUpdate(Sender: TCustomRubberBandLayer; Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer;
  var UpdateRect: TRect; var Handled: boolean);
var
  HandleRect: TFloatRect;
begin
  HandleRect.TopLeft := p;
  HandleRect.BottomRight := HandleRect.TopLeft;
  HandleRect.Inflate(Sender.HandleSize+FirstOutlineWidth, Sender.HandleSize+FirstOutlineWidth);

  UpdateRect := MakeRect(HandleRect, rrOutside);

  // Or simply:
  // UpdateRect.Inflate(Ceil(FirstOutlineWidth), Ceil(FirstOutlineWidth));
end;

procedure TFormMain.LayerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SourceDest: TSourceDest;
begin
  if (Sender = FLayers[sdSource]) then
    SourceDest := sdSource
  else
    SourceDest := sdDest;

  FActiveIndex[SourceDest] := -1;
  FLayers[SourceDest].Update;
end;

//------------------------------------------------------------------------------

procedure TFormMain.TimerMarchingAntsTimer(Sender: TObject);
begin
  if (FLayers[sdSource].CurrentHitTest <> nil) then
    FLayers[sdSource].FrameStippleCounter := FLayers[sdSource].FrameStippleCounter + 1.5;

  if (FLayers[sdDest].CurrentHitTest <> nil) then
    FLayers[sdDest].FrameStippleCounter := FLayers[sdDest].FrameStippleCounter + 1.5;
end;

procedure TFormMain.TimerUpdateTimer(Sender: TObject);
begin
  TimerUpdate.Enabled := False;
  ButtonApply.Click;
end;

//------------------------------------------------------------------------------

function TFormMain.IsCornerValid(const Quad: TFloatQuadrilateral; Index, ActiveIndex: integer): boolean;
var
  p0, p2: integer;
  v1, v2: TFloatPoint;
  Angle: TFloat;
begin
  p0 := Index-1;
  p2 := Index+1;
  if (p0 < 0) then
    p0 := 3;
  if (p2 > 3) then
    p2 := 0;

  // Translate to origin
  v1 := Quad[p0] - Quad[Index];
  v2 := Quad[p2] - Quad[Index];

  Angle := RadToDeg(ArcTan2(CrossProduct(v1, v2), Dot(v1, v2)));

  if (Index = ActiveIndex) then
    Angle := 1.0 * Angle;

  Result := (Angle < 0);
end;

//------------------------------------------------------------------------------

function TFormMain.MoveCorner(SourceDest: TSourceDest; var APos: TFloatPoint; ASnap: boolean): boolean;

  function CrossProduct(p, A, B: TFloatPoint): TFloat;
  begin
    Result := (p.X - A.X) * (B.Y - A.Y) - (p.Y - A.Y) * (B.X - A.X);
  end;

var
  OppositePos: TFloatPoint;
  PrevPos: TFloatPoint;
  NextPos: TFloatPoint;
  Cross: TFloat;
begin
  if (ASnap) then
  begin
    // Snap to 90 degree angle
    OppositePos := FCorners[SourceDest, (FActiveIndex[SourceDest]+2) mod 4];
    PrevPos := FCorners[SourceDest, (FActiveIndex[SourceDest]+4-1) mod 4];
    NextPos := FCorners[SourceDest, (FActiveIndex[SourceDest]+1) mod 4];

    // Find snap point that is on the opposite side of the opposite corner

    // Find (AB x AC) where A and B are the prev and next corners and C is the opposite corner
    Cross := CrossProduct(OppositePos, PrevPos, NextPos);

    // Create one of the two possible candidate points...
    APos.X := PrevPos.X;
    APos.Y := NextPos.Y;
    // ...and ensure that the cross product has the opposite sign.
    if (Sign(CrossProduct(APos, PrevPos, NextPos)) = Sign(Cross)) then
    begin
      // Our first attempt was on the same side. Use the other candidate instead.
      APos.X := NextPos.X;
      APos.Y := PrevPos.Y;
    end;
  end;

  Result := (APos <> FCorners[SourceDest, FActiveIndex[SourceDest]]);

  if (Result) then
    FCorners[SourceDest, FActiveIndex[SourceDest]] := APos;
end;

function TFormMain.SortCorners(SourceDest: TSourceDest): boolean;

  // Given three colinear points p, a, b, the function checks if
  // point p lies on line segment ab
  function OnSegment(const a, b, p: TFloatPoint): boolean;
  begin
    Result := (p.X <= Max(a.X, b.X)) and (p.X >= Min(a.X, b.X)) and (p.Y <= Max(a.Y, b.Y)) and (p.Y >= Min(a.Y, b.Y));
  end;

  // Given the ordered triplet (a, b, c), the function returns
  // the following values:
  //    0: a, b and c are colinear
  //    1: abc is clockwise
  //   -1: abc is counterclockwise
  // https://www.geeksforgeeks.org/orientation-3-ordered-points/
  function Orientation(const a, b, c: TFloatPoint): integer;
  begin
    Result := Sign((b.Y - a.Y) * (c.X - b.X) - (b.X - a.X) * (c.Y - b.Y));
  end;

  function Intersect(const a, b, c, d: TFloatPoint): boolean;
  var
    o1, o2, o3, o4: integer;
  begin
    o1 := Orientation(a, b, c);
    o2 := Orientation(a, b, d);
    o3 := Orientation(c, d, a);
    o4 := Orientation(c, d, b);

    // General case
    if (o1 <> o2) and (o3 <> o4) then
      Exit(True);

    // Special Cases
    // a, b and c are colinear and c lies on segment ab
    if (o1 = 0) and (OnSegment(a, b, c)) then
      Exit(True);

    // a, b and d are colinear and d lies on segment ab
    if (o2 = 0) and (OnSegment(a, b, d)) then
      Exit(True);

    // c, d and a are colinear and a lies on segment cd
    if (o3 = 0) and (OnSegment(c, d, a)) then
      Exit(True);

     // c, d and b are colinear and b lies on segment cd
    if (o4 = 0) and (OnSegment(c, d, b)) then
      Exit(True);

    Result := False;
  end;

  procedure Swap(a, b: integer);
  var
    n: TFloatPoint;
  begin
    n := FCorners[SourceDest, a];
    FCorners[SourceDest, a] := FCorners[SourceDest, b];
    FCorners[SourceDest, b] := n;

    if (FActiveIndex[SourceDest] = a) then
      FActiveIndex[SourceDest] := b
    else
    if (FActiveIndex[SourceDest] = b) then
      FActiveIndex[SourceDest] := a;

    if (FInvalidIndex[SourceDest] = a) then
      FInvalidIndex[SourceDest] := b
    else
    if (FInvalidIndex[SourceDest] = b) then
      FInvalidIndex[SourceDest] := a;
  end;

  procedure SortClockwise;
  begin
    if (Orientation(FCorners[SourceDest, 0], FCorners[SourceDest, 1], FCorners[SourceDest, 2]) < 0) then
    begin
      // Triangle abc is already clockwise.  Where does d fit?
      if (Orientation(FCorners[SourceDest, 0], FCorners[SourceDest, 2], FCorners[SourceDest, 3]) < 0) then
        Exit;

      if (Orientation(FCorners[SourceDest, 0], FCorners[SourceDest, 1], FCorners[SourceDest, 3]) < 0) then
        Swap(2, 3)
      else
        Swap(0, 3)
    end else
    if (Orientation(FCorners[SourceDest, 0], FCorners[SourceDest, 2], FCorners[SourceDest, 3]) < 0) then
    begin
      // Triangle abc is counterclockwise, i.e. acb is clockwise.
      // Also, acd is clockwise.
      if (Orientation(FCorners[SourceDest, 0], FCorners[SourceDest, 1], FCorners[SourceDest, 3]) < 0) then
        Swap(1, 2)
      else
        Swap(0, 1);
    end else
      // Triangle abc is counterclockwise, and acd is counterclockwise.
      // Therefore, abcd is counterclockwise.
      Swap(0, 2);

    Result := True;
  end;

  procedure FindTopLeft;
  var
    MinSum: TFLoat;
    MinIndex: integer;
    i: integer;
    Temp: TFloatQuadrilateral;
  begin
    MinSum := FCorners[SourceDest, 0].X + FCorners[SourceDest, 0].Y;
    MinIndex := 0;
    for i := 1 to 3 do
    begin
      var Sum := FCorners[SourceDest, i].X + FCorners[SourceDest, i].Y;
      if (Sum < MinSum) then
      begin
        MinSum := Sum;
        MinIndex := i;
      end else
      if (Sum = MinSum) and (FCorners[SourceDest, i].X < FCorners[SourceDest, MinIndex].X) then
        MinIndex := i;
    end;

    if (MinIndex = 0) then
      Exit;

    Temp := FCorners[SourceDest];
    for i := 0 to 3 do
      Temp[i] := FCorners[SourceDest, (MinIndex+i) mod 4];
    FCorners[SourceDest] := Temp;

    if (FActiveIndex[SourceDest] <> -1) then
      FActiveIndex[SourceDest] := (FActiveIndex[SourceDest] - MinIndex + 4) mod 4;
    if (FInvalidIndex[SourceDest] <> -1) then
      FInvalidIndex[SourceDest] := (FInvalidIndex[SourceDest] - MinIndex + 4) mod 4;

    Result := True;
  end;

begin
  Result := False;

  (*
  ** Order points so they appear in the array in clockwise order
  ** (i.e. moving along the vertices we always turn right).
  **
  ** Furthermore we would like to first point to be top-left-ish.
  **
  ** Remember that we use the bitmap coordinate system where Y is reversed.
  **
  **       X
  **   +------------>
  **   |
  ** Y |  A--B
  **   |  |  |
  **   |  D--C
  **   |
  **   V
  **
  ** Clockwise Sort algorithm based on:
  ** - https://stackoverflow.com/a/245079/2249664
  ** - https://stackoverflow.com/a/246063/2249664
  **
  ** Note that this sort algorithm only works if the polygon is convex.
  *)

  SortClockwise;

  FindTopLeft;
end;

//------------------------------------------------------------------------------

procedure TFormMain.ButtonApplyClick(Sender: TObject);
var
  SourceDest: TSourceDest;
  i: integer;
  Rasterizer: TRasterizer;
begin
  for SourceDest := Low(TSourceDest) to High(TSourceDest) do
  begin
    for i := Low(FCorners[SourceDest]) to High(FCorners[SourceDest]) do
      FCorners[SourceDest, i] := FLayers[SourceDest].Vertex[i];

    // Ensure that corners are stored clockwise, with first point top/left-most.
    // This enables us to do something sensible with the quad even if the user has
    // messed up the order. Unfortunately it also means that the user can't mirror
    // my reversing the quad on purpose.
    if (SortCorners(SourceDest)) then
      for i := Low(FCorners[SourceDest]) to High(FCorners[SourceDest]) do
        FLayers[SourceDest].Vertex[i] := FCorners[SourceDest, i];
  end;

  FTransformation.SourceQuad := FCorners[sdSource];
  FTransformation.DestQuad := FCorners[sdDest];

  FTransformation.Extrapolate := CheckBoxExtrapolate.Checked;

  ImageDest.Bitmap.BeginUpdate;
  try
    ImageDest.Bitmap.Clear(0);

    FTransformation.SrcRect := FloatRect(ImageSource.Bitmap.BoundsRect);
    (*
    Rasterizer := TMultithreadedRegularRasterizer.Create;
    try
      Transform(ImageDest.Bitmap, ImageSource.Bitmap, FTransformation, Rasterizer, False); // Forward projection
    finally
      Rasterizer.Free;
    end;
    *)
    Transform(ImageDest.Bitmap, ImageSource.Bitmap, FTransformation, False); // Forward projection

  finally
    ImageDest.Bitmap.EndUpdate;
  end;
  Update;
end;

end.
