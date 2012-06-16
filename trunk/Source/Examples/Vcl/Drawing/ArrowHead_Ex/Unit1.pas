unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GR32, GR32_Image, GR32_Layers, GR32_Paths,
  GR32_Polygons, GR32_VectorUtils, GR32_ArrowHeads, GR32_Geometry,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ImgView321: TImgView32;
    Panel1: TPanel;
    Button1: TButton;
    rgArrowStyle: TRadioGroup;
    Edit1: TEdit;
    Label1: TLabel;
    rgPosition: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    arrowSize: integer;
    procedure ReDraw;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R pattern.res}

function MakeArrayOfFloat(const a: array of TFloat): TArrayOfFloat;
var
  i, len: integer;
begin
  len := length(a);
  setlength(result, len);
  for i := 0 to len -1 do result[i] := a[i];
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloatPoints(const a: array of single): TArrayOfFloatPoint; overload;
var
  i, len: integer;
begin
  len := length(a) div 2;
  setlength(result, len);
  if len = 0 then exit;
  for i := 0 to len -1 do
  begin
    result[i].X := a[i*2];
    result[i].Y := a[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfFloatPoints(const a: TArrayOfFixedPoint): TArrayOfFloatPoint; overload;
var
  i, len: integer;
begin
  len := length(a);
  setlength(result, len);
  for i := 0 to len -1 do result[i] := FloatPoint(a[i]);
end;
//------------------------------------------------------------------------------

function MakeBezierCurve(const CtrlPts: TArrayOfFloatPoint): TArrayOfFloatPoint;
var
  i: integer;
begin
  with TFlattenedPath.Create do
  try
    MoveTo(CtrlPts[0]);
    for i := 0 to (high(CtrlPts) -3) div 3 do
      CurveTo(CtrlPts[i*3 + 1], CtrlPts[i*3 + 2], CtrlPts[i*3 + 3]);
    result := Points;
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function MakeBox(centrePt: TFloatPoint; size: TFloat): TArrayOfFloatPoint;
begin
  size := size/2;
  SetLength(result, 4);
  result[0] := OffsetPoint(centrePt, -size, -size);
  result[1] := OffsetPoint(centrePt, size, -size);
  result[2] := OffsetPoint(centrePt, size, size);
  result[3] := OffsetPoint(centrePt, -size, size);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImgView321.SetupBitmap(true, clWhite32);
  ImgView321.Bitmap.DrawMode := dmBlend;
  arrowSize := 20;
  ReDraw;
end;
//------------------------------------------------------------------------------

procedure TForm1.ReDraw;
var
  poly, arrowPts: TArrayOfFloatPoint;
  pts: array [0..1] of TFloatPoint;
  dashes: TArrayOfFloat;
  arrow: TArrowHeadAbstract;
  bmp: TBitmap32;
  bmpFiller: TBitmapPolygonFiller;
begin
  ImgView321.SetupBitmap(true, clWhite32);

  bmp := TBitmap32.Create;
  bmpFiller := TBitmapPolygonFiller.Create;

  case rgArrowStyle.ItemIndex of
    1: arrow := TArrowHeadSimple.Create(arrowSize);
    2: arrow := TArrowHeadFourPt.Create(arrowSize);
    3: arrow := TArrowHeadDiamond.Create(arrowSize);
    4: arrow := TArrowHeadCircle.Create(arrowSize);
    else arrow := nil;
  end;

  dashes := MakeArrayOfFloat([14,3,3,3,3,3]);
  pts[0] := FloatPoint(80, 100);
  pts[1] := FloatPoint(280, 300);

  bmp.LoadFromResourceName(hinstance, 'PATTERN');
  bmpFiller.Pattern := bmp;
  poly := MakeBox(pts[0], 60);
  dashLineFS( ImgView321.Bitmap, poly, dashes, bmpFiller, $FF006600, true, 10,1.5);

  bmp.LoadFromResourceName(hinstance, 'PATTERN2');
  poly := MakeBox(pts[1], 60);
  dashLineFS(ImgView321.Bitmap, poly, dashes, bmpFiller, $FF000066, true, 10,1.5);

  poly := MakeArrayOfFloatPoints([pts[0].X +35, pts[0].Y,
    pts[0].X +35 + 60, pts[0].Y, pts[1].X -35 - 60, pts[1].Y, pts[1].X -35, pts[1].Y]);
  poly := MakeBezierCurve(poly);

  if assigned(arrow) then
  begin
    //shorten path at specified end(s) and draw ...
    case rgPosition.ItemIndex of
      0: poly := Shorten(poly, arrowSize, lpStart);
      1: poly := Shorten(poly, arrowSize, lpEnd);
      2: poly := Shorten(poly, arrowSize, lpBoth);
    end;
    PolylineFS(ImgView321.Bitmap, poly, clBlack32, false, 2);

    //draw specified arrows ...
    if rgPosition.ItemIndex <> 1 then
    begin
      arrowPts := arrow.GetPoints(poly, false);
      PolygonFS(ImgView321.Bitmap, arrowPts, $60006600);
      PolylineFS(ImgView321.Bitmap, arrowPts, $FF006600, true, 2);
    end;
    if rgPosition.ItemIndex <> 0 then
    begin
      arrowPts := arrow.GetPoints(poly, true);
      PolygonFS(ImgView321.Bitmap, arrowPts, $60000066);
      PolylineFS(ImgView321.Bitmap, arrowPts, clNavy32, true, 2);
    end;
  end else
    PolylineFS(ImgView321.Bitmap, poly, clBlack32, false, 2);

  bmp.Free;
  bmpFiller.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Edit1Change(Sender: TObject);
begin
  arrowSize := StrToIntDef(Edit1.Text, arrowSize);
  if arrowSize < 5 then arrowSize := 5
  else if arrowSize > 40 then arrowSize := 40;
  Redraw;
end;
//------------------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

end.
