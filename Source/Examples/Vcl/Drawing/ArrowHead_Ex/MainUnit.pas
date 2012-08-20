unit MainUnit;

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, ExtCtrls, GR32, GR32_Image, GR32_Layers, GR32_Paths,
  GR32_Polygons, GR32_VectorUtils, GR32_ArrowHeads, GR32_Geometry;

type
  TFmArrowHeadDemo = class(TForm)
    ImgView32: TImgView32;
    pnlControl: TPanel;
    btnClose: TButton;
    rgArrowStyle: TRadioGroup;
    Edit1: TEdit;
    lblArrowSize: TLabel;
    rgPosition: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure rgArrowStyleClick(Sender: TObject);
  private
    FArrowSize: Integer;
    procedure ReDraw;
    procedure SetArrowSize(const Value: Integer);
  protected
    procedure ArrowSizeChanged; virtual;
  public
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
  Math;

function MakeArrayOfFloat(const a: array of TFloat): TArrayOfFloat;
var
  Index, Len: Integer;
begin
  Len := Length(a);
  SetLength(Result, Len);
  for Index := 0 to Len -1
    do Result[Index] := a[Index];
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloatPoints(const a: array of single): TArrayOfFloatPoint; overload;
var
  Index, Len: Integer;
begin
  Len := Length(a) div 2;
  SetLength(Result, Len);
  if Len = 0 then exit;
  for Index := 0 to Len - 1 do
  begin
    Result[Index].X := a[Index * 2];
    Result[Index].Y := a[Index * 2 + 1];
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloatPoints(const a: TArrayOfFixedPoint): TArrayOfFloatPoint; overload;
var
  Index, Len: Integer;
begin
  Len := Length(a);
  SetLength(Result, Len);
  for Index := 0 to Len -1
    do Result[Index] := FloatPoint(a[Index]);
end;
//------------------------------------------------------------------------------

function MakeBezierCurve(const CtrlPts: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  Index: Integer;
begin
  with TFlattenedPath.Create do
  try
    MoveTo(CtrlPts[0]);
    for Index := 0 to (High(CtrlPts) -3) div 3 do
      CurveTo(CtrlPts[Index * 3 + 1], CtrlPts[Index * 3 + 2],
        CtrlPts[Index * 3 + 3]);
    Result := Points;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function MakeBox(CentrePt: TFloatPoint; Size: TFloat): TArrayOfFloatPoint;
begin
  Size := Size * 0.5;
  SetLength(Result, 4);
  Result[0] := OffsetPoint(CentrePt, -Size, -Size);
  Result[1] := OffsetPoint(CentrePt,  Size, -Size);
  Result[2] := OffsetPoint(CentrePt,  Size,  Size);
  Result[3] := OffsetPoint(CentrePt, -Size,  Size);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.FormCreate(Sender: TObject);
begin
  ImgView32.SetupBitmap(True, clWhite32);
  ImgView32.Bitmap.DrawMode := dmBlend;
  ArrowSize := 20;
end;
//------------------------------------------------------------------------------

procedure TFmArrowHeadDemo.ReDraw;
var
  Poly, ArrowPts: TArrayOfFloatPoint;
  Pts: array [0..1] of TFloatPoint;
  Dashes: TArrayOfFloat;
  Arrow: TArrowHeadAbstract;
  Bmp: TBitmap32;
  BmpFiller: TBitmapPolygonFiller;
begin
  ImgView32.SetupBitmap(True, clWhite32);

  Bmp := TBitmap32.Create;
  BmpFiller := TBitmapPolygonFiller.Create;

  case rgArrowStyle.ItemIndex of
    1: Arrow := TArrowHeadSimple.Create(ArrowSize);
    2: Arrow := TArrowHeadFourPt.Create(ArrowSize);
    3: Arrow := TArrowHeadDiamond.Create(ArrowSize);
    4: Arrow := TArrowHeadCircle.Create(ArrowSize);
    else Arrow := nil;
  end;

  Dashes := MakeArrayOfFloat([14, 3, 3, 3, 3, 3]);
  Pts[0] := FloatPoint(80, 100);
  Pts[1] := FloatPoint(280, 300);

  Bmp.LoadFromResourceName(HInstance, 'PATTERN');
  BmpFiller.Pattern := Bmp;
  Poly := MakeBox(Pts[0], 60);
  DashLineFS(ImgView32.Bitmap, Poly, Dashes, BmpFiller, $FF006600, True, 10,
    1.5);

  Bmp.LoadFromResourceName(HInstance, 'PATTERN2');
  Poly := MakeBox(Pts[1], 60);
  DashLineFS(ImgView32.Bitmap, Poly, Dashes, BmpFiller, $FF000066, True, 10,
    1.5);

  Poly := MakeArrayOfFloatPoints([Pts[0].X + 35, Pts[0].Y,
    Pts[0].X + 95, Pts[0].Y,
    Pts[1].X - 95, Pts[1].Y,
    Pts[1].X - 35, Pts[1].Y]);
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

  Bmp.Free;
  BmpFiller.Free;
end;

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

end.
