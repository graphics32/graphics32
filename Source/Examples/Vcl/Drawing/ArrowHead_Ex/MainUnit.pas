unit MainUnit;

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF} Messages, SysUtils, Classes,
  Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  GR32, GR32_Image, GR32_Layers, GR32_Paths, GR32_Polygons, GR32_ArrowHeads;

type
  TFmArrowHeadDemo = class(TForm)
    ImgView32: TImgView32;
    pnlControl: TPanel;
    btnClose: TButton;
    rgArrowStyle: TRadioGroup;
    Edit1: TEdit;
    lblArrowSize: TLabel;
    rgPosition: TRadioGroup;
    CbAnimate: TCheckBox;
    Animation: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure rgArrowStyleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CbAnimateClick(Sender: TObject);
    procedure AnimationTimer(Sender: TObject);
    procedure ImgView32Resize(Sender: TObject);
    procedure ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    FArrowSize: Integer;
    FBoxIndex: Integer;
    FLastPos: TPoint;
    FDashes: TArrayOfFloat;
    FBoxCenter: array [0..1] of TFloatPoint;
    FVelocity: array [0..1] of TFloatPoint;
    FPattern: array [0..1] of TBitmap32;
    FBitmapFiller: TBitmapPolygonFiller;
    procedure SetArrowSize(const Value: Integer);
  protected
    procedure ArrowSizeChanged; virtual;
  public
    procedure ReDraw;

    property ArrowSize: Integer read FArrowSize write SetArrowSize;
  end;

var
  FmArrowHeadDemo: TFmArrowHeadDemo;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$R pattern.res}

uses
  Math, GR32_Geometry, GR32_VectorUtils;

function MakeArrayOfFloat(const Data: array of TFloat): TArrayOfFloat;
var
  Index, Len: Integer;
begin
  Len := Length(Data);
  SetLength(Result, Len);
  for Index := 0 to Len - 1
    do Result[Index] := Data[Index];
end;
//------------------------------------------------------------------------------

function MakeBezierCurve(const CtrlPts: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  Index: Integer;
begin
  with TFlattenedPath.Create do
  try
    MoveTo(CtrlPts[0]);
    for Index := 0 to (High(CtrlPts) - 3) div 3 do
      CurveTo(CtrlPts[Index * 3 + 1], CtrlPts[Index * 3 + 2],
        CtrlPts[Index * 3 + 3]);
    Result := Points;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function MakeBox(CenterPt: TFloatPoint; Size: TFloat): TArrayOfFloatPoint;
begin
  Size := Size * 0.5;
  SetLength(Result, 4);
  Result[0] := OffsetPoint(CenterPt, -Size, -Size);
  Result[1] := OffsetPoint(CenterPt,  Size, -Size);
  Result[2] := OffsetPoint(CenterPt,  Size,  Size);
  Result[3] := OffsetPoint(CenterPt, -Size,  Size);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.FormCreate(Sender: TObject);
begin
  Randomize;
  ImgView32.Bitmap.DrawMode := dmOpaque;
  ImgView32.SetupBitmap(True, clWhite32);

  FBoxIndex := -1;
  FArrowSize := 20;
  FDashes := MakeArrayOfFloat([14, 3, 3, 3, 3, 3]);
  FBoxCenter[0] := FloatPoint(80, 100);
  FBoxCenter[1] := FloatPoint(280, 300);
  FVelocity[0] := FloatPoint(2 * Random - 1, 2 * Random - 1);
  FVelocity[1] := FloatPoint(2 * Random - 1, 2 * Random - 1);

  FPattern[0] := TBitmap32.Create;
  FPattern[0].LoadFromResourceName(HInstance, 'PATTERN');
  FPattern[1] := TBitmap32.Create;
  FPattern[1].LoadFromResourceName(HInstance, 'PATTERN2');

  FBitmapFiller := TBitmapPolygonFiller.Create;

  Redraw;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.FormDestroy(Sender: TObject);
begin
  FPattern[0].Free;
  FPattern[1].Free;
  FBitmapFiller.Free;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.ImgView32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Index: Integer;
begin
  FBoxIndex := -1;
  for Index := 0 to High(FBoxCenter) do
    if PtInRect(FloatRect(FBoxCenter[Index].X - 35, FBoxCenter[Index].Y - 35,
      FBoxCenter[Index].X + 35, FBoxCenter[Index].Y + 35), Point(X, Y)) then
    begin
      FLastPos := Point(X, Y);
      FBoxIndex := Index;
      Exit;
    end;
end;

procedure TFmArrowHeadDemo.ImgView32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Index: Integer;
begin
  if FBoxIndex >= 0 then
  begin
    FBoxCenter[FBoxIndex].X := EnsureRange(FBoxCenter[FBoxIndex].X + X -
      FLastPos.X, 35, ImgView32.Width - 35);
    FBoxCenter[FBoxIndex].Y := EnsureRange(FBoxCenter[FBoxIndex].Y + Y -
      FLastPos.Y, 35, ImgView32.Height - 35);
    ReDraw;
    FLastPos := Point(X, Y);
  end
  else
  begin
    for Index := 0 to High(FBoxCenter) do
      if PtInRect(FloatRect(FBoxCenter[Index].X - 35, FBoxCenter[Index].Y - 35,
        FBoxCenter[Index].X + 35, FBoxCenter[Index].Y + 35), Point(X, Y)) then
      begin
        ImgView32.Cursor := crHandPoint;
        Exit;
      end;
    ImgView32.Cursor := crArrow;
  end;
end;

procedure TFmArrowHeadDemo.ImgView32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FBoxIndex := -1;
end;

procedure TFmArrowHeadDemo.ImgView32Resize(Sender: TObject);
begin
  ImgView32.Bitmap.SetSize(ImgView32.Width, ImgView32.Height);
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.ReDraw;
var
  Poly, ArrowPts: TArrayOfFloatPoint;
  Dist: TFloat;
  Arrow: TArrowHeadAbstract;
begin
  ImgView32.Bitmap.Clear(clWhite32);

  case rgArrowStyle.ItemIndex of
    1: Arrow := TArrowHeadSimple.Create(ArrowSize);
    2: Arrow := TArrowHeadFourPt.Create(ArrowSize);
    3: Arrow := TArrowHeadDiamond.Create(ArrowSize);
    4: Arrow := TArrowHeadCircle.Create(ArrowSize);
    else Arrow := nil;
  end;

  FBitmapFiller.Pattern := FPattern[0];
  Poly := MakeBox(FBoxCenter[0], 60);
  DashLineFS(ImgView32.Bitmap, Poly, FDashes, FBitmapFiller, $FF006600, True,
    10, 1.5);

  FBitmapFiller.Pattern := FPattern[1];
  Poly := MakeBox(FBoxCenter[1], 60);
  DashLineFS(ImgView32.Bitmap, Poly, FDashes, FBitmapFiller, $FF000066, True,
    10, 1.5);

  Dist := Hypot(FBoxCenter[0].X - FBoxCenter[1].X + 190,
    FBoxCenter[0].Y - FBoxCenter[1].Y);

  Poly := BuildPolygon([FBoxCenter[0].X + 35, FBoxCenter[0].Y,
    FBoxCenter[0].X + Dist, FBoxCenter[0].Y,
    FBoxCenter[1].X - Dist, FBoxCenter[1].Y,
    FBoxCenter[1].X - 35, FBoxCenter[1].Y]);
  Poly := MakeBezierCurve(Poly);

  if Assigned(Arrow) then
  begin
    //shorten path at specified end(s) and draw ...
    case rgPosition.ItemIndex of
      0: Poly := Shorten(Poly, ArrowSize, lpStart);
      1: Poly := Shorten(Poly, ArrowSize, lpEnd);
      2: Poly := Shorten(Poly, ArrowSize, lpBoth);
    end;
    PolylineFS(ImgView32.Bitmap, Poly, clBlack32, False, 2);

    //draw specified arrows ...
    if rgPosition.ItemIndex <> 1 then
    begin
      ArrowPts := Arrow.GetPoints(Poly, False);
      PolygonFS(ImgView32.Bitmap, ArrowPts, $60006600);
      PolylineFS(ImgView32.Bitmap, ArrowPts, $FF006600, True, 2);
    end;
    if rgPosition.ItemIndex <> 0 then
    begin
      ArrowPts := Arrow.GetPoints(Poly, True);
      PolygonFS(ImgView32.Bitmap, ArrowPts, $60000066);
      PolylineFS(ImgView32.Bitmap, ArrowPts, clNavy32, True, 2);
    end;
  end else
    PolylineFS(ImgView32.Bitmap, Poly, clBlack32, False, 2);
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.rgArrowStyleClick(Sender: TObject);
begin
  ReDraw;
end;

//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.Edit1Change(Sender: TObject);
begin
  ArrowSize := EnsureRange(StrToIntDef(Edit1.Text, ArrowSize), 5, 40);
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.CbAnimateClick(Sender: TObject);
begin
  FVelocity[0] := FloatPoint(2 * Random - 1, 2 * Random - 1);
  FVelocity[1] := FloatPoint(2 * Random - 1, 2 * Random - 1);
  Animation.Enabled := CbAnimate.Checked;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.AnimationTimer(Sender: TObject);
const
  CSize: Integer = 35;
var
  Index: Integer;
begin
  for Index := 0 to High(FBoxCenter) do
  begin
    if FBoxIndex <> Index then
      FBoxCenter[Index] := OffsetPoint(FBoxCenter[Index], FVelocity[Index].X, FVelocity[Index].Y);
    if (FBoxCenter[Index].X + CSize > ImgView32.Width) or (FBoxCenter[Index].X < CSize) then
      FVelocity[Index].X := - FVelocity[Index].X;
    if (FBoxCenter[Index].Y + CSize > ImgView32.Height) or (FBoxCenter[Index].Y < CSize) then
      FVelocity[Index].Y := - FVelocity[Index].Y;
  end;

  ReDraw;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.SetArrowSize(const Value: Integer);
begin
  if FArrowSize <> Value then
  begin
    FArrowSize := Value;
    ArrowSizeChanged;
  end;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.ArrowSizeChanged;
begin
  Redraw;
end;
//------------------------------------------------------------------------------

initialization

end.
