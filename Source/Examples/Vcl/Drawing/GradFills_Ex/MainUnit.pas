unit MainUnit;

interface

{$I GR32.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, ExtCtrls, StdCtrls, Menus, GR32, GR32_Polygons, GR32_Image, GR32_Layers,
  GR32_Transforms, GR32_ColorGradients;

type
  TControlButton = class(TPersistent)
  private
    FCenter: TPoint;
    FSize, FSizeSqr: TFloat;
    FAffineTransformation: TAffineTransformation;
    FOutline: TArrayOfFloatPoint;
    procedure SetCenter(const Value: TPoint);
  public
    constructor Create(const Center: TPoint; Size: TFloat);
    destructor Destroy; override;
    procedure Draw(const Bmp32: TBitmap32; Filler: TCustomPolygonFiller);
    function TestHitPoint(X, Y: Integer): Boolean;

    property Center: TPoint read FCenter write SetCenter;
    property Outline: TArrayOfFloatPoint read FOutline;
  end;

  TMainForm = class(TForm)
    ImgView32: TImgView32;
    LblColorStopsBottom: TLabel;
    LblColorStopsTop: TLabel;
    MainMenu: TMainMenu;
    MemoColorStopsBottom: TMemo;
    MemoColorStopsTop: TMemo;
    MnuExit: TMenuItem;
    MnuFile: TMenuItem;
    PnlControl: TPanel;
    RgpEllipseFillStyle: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BtnExitClick(Sender: TObject);
    procedure ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure MemoColorStopsTopChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RgpEllipseFillStyleClick(Sender: TObject);
  private
    FControlButtonFiller: TSamplerFiller;
    FRadialGradientSampler: TRadialGradientSampler;
    FControlButton: TControlButton;
    FLinearStartBtn: TControlButton;
    FLinearEndBtn: TControlButton;
    FRadialOriginBtn: TControlButton;
    FRadialXBtn: TControlButton;
    FRadialYBtn: TControlButton;
    FLinearBounds: TRect;
    FRadialBounds: TRect;
    FTextNotesPoly: TArrayOfArrayOfFloatPoint;
    FTextTopPoly: TArrayOfArrayOfFloatPoint;
    FTextBottomPoly: TArrayOfArrayOfFloatPoint;
    FTextGR32: TArrayOfArrayOfFloatPoint;
    procedure DrawImage;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Types, GR32_Geometry, GR32_VectorUtils, GR32_Paths, GR32_Text_VCL;

{$IFDEF FPC}
{$R *.lfm}
{$R data.res}
{$ELSE}
{$R *.dfm}
{$R data.res}
{$ENDIF}

const
  Colors: array[0..147] of TIdentMapEntry = (
    (Value: Integer($FF000000); Name: 'clBlack32'),
    (Value: Integer($FF3F3F3F); Name: 'clDimGray32'),
    (Value: Integer($FF7F7F7F); Name: 'clGray32'),
    (Value: Integer($FFBFBFBF); Name: 'clLightGray32'),
    (Value: Integer($FFFFFFFF); Name: 'clWhite32'),
    (Value: Integer($FF7F0000); Name: 'clMaroon32'),
    (Value: Integer($FF007F00); Name: 'clGreen32'),
    (Value: Integer($FF7F7F00); Name: 'clOlive32'),
    (Value: Integer($FF00007F); Name: 'clNavy32'),
    (Value: Integer($FF7F007F); Name: 'clPurple32'),
    (Value: Integer($FF007F7F); Name: 'clTeal32'),
    (Value: Integer($FFFF0000); Name: 'clRed32'),
    (Value: Integer($FF00FF00); Name: 'clLime32'),
    (Value: Integer($FFFFFF00); Name: 'clYellow32'),
    (Value: Integer($FF0000FF); Name: 'clBlue32'),
    (Value: Integer($FFFF00FF); Name: 'clFuchsia32'),
    (Value: Integer($FF00FFFF); Name: 'clAqua32'),
    (Value: Integer($FFF0F8FF); Name: 'clAliceBlue32'),
    (Value: Integer($FFFAEBD7); Name: 'clAntiqueWhite32'),
    (Value: Integer($FF7FFFD4); Name: 'clAquamarine32'),
    (Value: Integer($FFF0FFFF); Name: 'clAzure32'),
    (Value: Integer($FFF5F5DC); Name: 'clBeige32'),
    (Value: Integer($FFFFE4C4); Name: 'clBisque32'),
    (Value: Integer($FFFFEBCD); Name: 'clBlancheDalmond32'),
    (Value: Integer($FF8A2BE2); Name: 'clBlueViolet32'),
    (Value: Integer($FFA52A2A); Name: 'clBrown32'),
    (Value: Integer($FFDEB887); Name: 'clBurlyWood32'),
    (Value: Integer($FF5F9EA0); Name: 'clCadetblue32'),
    (Value: Integer($FF7FFF00); Name: 'clChartReuse32'),
    (Value: Integer($FFD2691E); Name: 'clChocolate32'),
    (Value: Integer($FFFF7F50); Name: 'clCoral32'),
    (Value: Integer($FF6495ED); Name: 'clCornFlowerBlue32'),
    (Value: Integer($FFFFF8DC); Name: 'clCornSilk32'),
    (Value: Integer($FFDC143C); Name: 'clCrimson32'),
    (Value: Integer($FF00008B); Name: 'clDarkBlue32'),
    (Value: Integer($FF008B8B); Name: 'clDarkCyan32'),
    (Value: Integer($FFB8860B); Name: 'clDarkGoldenRod32'),
    (Value: Integer($FFA9A9A9); Name: 'clDarkGray32'),
    (Value: Integer($FF006400); Name: 'clDarkGreen32'),
    (Value: Integer($FFA9A9A9); Name: 'clDarkGrey32'),
    (Value: Integer($FFBDB76B); Name: 'clDarkKhaki32'),
    (Value: Integer($FF8B008B); Name: 'clDarkMagenta32'),
    (Value: Integer($FF556B2F); Name: 'clDarkOliveGreen32'),
    (Value: Integer($FFFF8C00); Name: 'clDarkOrange32'),
    (Value: Integer($FF9932CC); Name: 'clDarkOrchid32'),
    (Value: Integer($FF8B0000); Name: 'clDarkRed32'),
    (Value: Integer($FFE9967A); Name: 'clDarkSalmon32'),
    (Value: Integer($FF8FBC8F); Name: 'clDarkSeaGreen32'),
    (Value: Integer($FF483D8B); Name: 'clDarkSlateBlue32'),
    (Value: Integer($FF2F4F4F); Name: 'clDarkSlateGray32'),
    (Value: Integer($FF2F4F4F); Name: 'clDarkSlateGrey32'),
    (Value: Integer($FF00CED1); Name: 'clDarkTurquoise32'),
    (Value: Integer($FF9400D3); Name: 'clDarkViolet32'),
    (Value: Integer($FFFF1493); Name: 'clDeepPink32'),
    (Value: Integer($FF00BFFF); Name: 'clDeepSkyBlue32'),
    (Value: Integer($FF1E90FF); Name: 'clDodgerBlue32'),
    (Value: Integer($FFB22222); Name: 'clFireBrick32'),
    (Value: Integer($FFFFFAF0); Name: 'clFloralWhite32'),
    (Value: Integer($FFDCDCDC); Name: 'clGainsBoro32'),
    (Value: Integer($FFF8F8FF); Name: 'clGhostWhite32'),
    (Value: Integer($FFFFD700); Name: 'clGold32'),
    (Value: Integer($FFDAA520); Name: 'clGoldenRod32'),
    (Value: Integer($FFADFF2F); Name: 'clGreenYellow32'),
    (Value: Integer($FF808080); Name: 'clGrey32'),
    (Value: Integer($FFF0FFF0); Name: 'clHoneyDew32'),
    (Value: Integer($FFFF69B4); Name: 'clHotPink32'),
    (Value: Integer($FFCD5C5C); Name: 'clIndianRed32'),
    (Value: Integer($FF4B0082); Name: 'clIndigo32'),
    (Value: Integer($FFFFFFF0); Name: 'clIvory32'),
    (Value: Integer($FFF0E68C); Name: 'clKhaki32'),
    (Value: Integer($FFE6E6FA); Name: 'clLavender32'),
    (Value: Integer($FFFFF0F5); Name: 'clLavenderBlush32'),
    (Value: Integer($FF7CFC00); Name: 'clLawnGreen32'),
    (Value: Integer($FFFFFACD); Name: 'clLemonChiffon32'),
    (Value: Integer($FFADD8E6); Name: 'clLightBlue32'),
    (Value: Integer($FFF08080); Name: 'clLightCoral32'),
    (Value: Integer($FFE0FFFF); Name: 'clLightCyan32'),
    (Value: Integer($FFFAFAD2); Name: 'clLightGoldenRodYellow32'),
    (Value: Integer($FF90EE90); Name: 'clLightGreen32'),
    (Value: Integer($FFD3D3D3); Name: 'clLightGrey32'),
    (Value: Integer($FFFFB6C1); Name: 'clLightPink32'),
    (Value: Integer($FFFFA07A); Name: 'clLightSalmon32'),
    (Value: Integer($FF20B2AA); Name: 'clLightSeagreen32'),
    (Value: Integer($FF87CEFA); Name: 'clLightSkyblue32'),
    (Value: Integer($FF778899); Name: 'clLightSlategray32'),
    (Value: Integer($FF778899); Name: 'clLightSlategrey32'),
    (Value: Integer($FFB0C4DE); Name: 'clLightSteelblue32'),
    (Value: Integer($FFFFFFE0); Name: 'clLightYellow32'),
    (Value: Integer($FFC0C0C0); Name: 'clLtGray32'),
    (Value: Integer($FFA0A0A4); Name: 'clMedGray32'),
    (Value: Integer($FF808080); Name: 'clDkGray32'),
    (Value: Integer($FFC0DCC0); Name: 'clMoneyGreen32'),
    (Value: Integer($FFA6CAF0); Name: 'clLegacySkyBlue32'),
    (Value: Integer($FFFFFBF0); Name: 'clCream32'),
    (Value: Integer($FF32CD32); Name: 'clLimeGreen32'),
    (Value: Integer($FFFAF0E6); Name: 'clLinen32'),
    (Value: Integer($FF66CDAA); Name: 'clMediumAquamarine32'),
    (Value: Integer($FF0000CD); Name: 'clMediumBlue32'),
    (Value: Integer($FFBA55D3); Name: 'clMediumOrchid32'),
    (Value: Integer($FF9370DB); Name: 'clMediumPurple32'),
    (Value: Integer($FF3CB371); Name: 'clMediumSeaGreen32'),
    (Value: Integer($FF7B68EE); Name: 'clMediumSlateBlue32'),
    (Value: Integer($FF00FA9A); Name: 'clMediumSpringGreen32'),
    (Value: Integer($FF48D1CC); Name: 'clMediumTurquoise32'),
    (Value: Integer($FFC71585); Name: 'clMediumVioletRed32'),
    (Value: Integer($FF191970); Name: 'clMidnightBlue32'),
    (Value: Integer($FFF5FFFA); Name: 'clMintCream32'),
    (Value: Integer($FFFFE4E1); Name: 'clMistyRose32'),
    (Value: Integer($FFFFE4B5); Name: 'clMoccasin32'),
    (Value: Integer($FFFFDEAD); Name: 'clNavajoWhite32'),
    (Value: Integer($FFFDF5E6); Name: 'clOldLace32'),
    (Value: Integer($FF6B8E23); Name: 'clOliveDrab32'),
    (Value: Integer($FFFFA500); Name: 'clOrange32'),
    (Value: Integer($FFFF4500); Name: 'clOrangeRed32'),
    (Value: Integer($FFDA70D6); Name: 'clOrchid32'),
    (Value: Integer($FFEEE8AA); Name: 'clPaleGoldenRod32'),
    (Value: Integer($FF98FB98); Name: 'clPaleGreen32'),
    (Value: Integer($FFAFEEEE); Name: 'clPaleTurquoise32'),
    (Value: Integer($FFDB7093); Name: 'clPaleVioletred32'),
    (Value: Integer($FFFFEFD5); Name: 'clPapayaWhip32'),
    (Value: Integer($FFFFDAB9); Name: 'clPeachPuff32'),
    (Value: Integer($FFCD853F); Name: 'clPeru32'),
    (Value: Integer($FFDDA0DD); Name: 'clPlum32'),
    (Value: Integer($FFB0E0E6); Name: 'clPowderBlue32'),
    (Value: Integer($FFBC8F8F); Name: 'clRosyBrown32'),
    (Value: Integer($FF4169E1); Name: 'clRoyalBlue32'),
    (Value: Integer($FF8B4513); Name: 'clSaddleBrown32'),
    (Value: Integer($FFFA8072); Name: 'clSalmon32'),
    (Value: Integer($FFF4A460); Name: 'clSandyBrown32'),
    (Value: Integer($FF2E8B57); Name: 'clSeaGreen32'),
    (Value: Integer($FFFFF5EE); Name: 'clSeaShell32'),
    (Value: Integer($FFA0522D); Name: 'clSienna32'),
    (Value: Integer($FFC0C0C0); Name: 'clSilver32'),
    (Value: Integer($FF87CEEB); Name: 'clSkyBlue32'),
    (Value: Integer($FF6A5ACD); Name: 'clSlateBlue32'),
    (Value: Integer($FF708090); Name: 'clSlateGray32'),
    (Value: Integer($FF708090); Name: 'clSlateGrey32'),
    (Value: Integer($FFFFFAFA); Name: 'clSnow32'),
    (Value: Integer($FF00FF7F); Name: 'clSpringGreen32'),
    (Value: Integer($FF4682B4); Name: 'clSteelBlue32'),
    (Value: Integer($FFD2B48C); Name: 'clTan32'),
    (Value: Integer($FFD8BFD8); Name: 'clThistle32'),
    (Value: Integer($FFFF6347); Name: 'clTomato32'),
    (Value: Integer($FF40E0D0); Name: 'clTurquoise32'),
    (Value: Integer($FFEE82EE); Name: 'clViolet32'),
    (Value: Integer($FFF5DEB3); Name: 'clWheat32'),
    (Value: Integer($FFF5F5F5); Name: 'clWhiteSmoke32'),
    (Value: Integer($FF9ACD32); Name: 'clYellowGreen32'));


{ Miscellaneous functions }

procedure StrToArrayColor32Gradient(s: TStrings; Gradient: TGradient32);
var
  I, J: Integer;
  Offset: TFloat;
  Color: TColor32;
  ColorStr: string;
begin
{$IFDEF COMPILER2005_UP}
  FormatSettings.DecimalSeparator := '.';
{$ELSE}
  DecimalSeparator := '.';
{$ENDIF}
  Gradient.ClearColors;
  for i := 0 to s.Count - 1 do
  begin
    j := Pos(':', s[i]);
    if j < 2 then
      Continue;
    Offset := StrToFloatDef(Copy(s[i], 1, j - 1), -1);
    if (Offset < 0) then
      Continue;
    ColorStr := Trim(Copy(s[i], j + 1, 80));
    if not IdentToInt(ColorStr, Integer(Color), Colors) then
      Color := TColor32(StrToIntDef(ColorStr, $01010101));
    if Color <> $01010101 then
      Gradient.AddColorStop(Offset, Color);
  end;
end;

function LoadPolysFromResource(const ResName: string): TArrayOfArrayOfFloatPoint;
var
  I,J, Count: Integer;
  ResStream: TResourceStream;

  function ReadInt: Integer;
  begin
    ResStream.Read(Result, SizeOf(Result));
  end;

  function ReadFloatPoint: TFloatPoint;
  begin
    ResStream.Read(Result.X, SizeOf(TFloat));
    ResStream.Read(Result.Y, SizeOf(TFloat));
  end;

begin
  ResStream := TResourceStream.Create(hInstance, Resname, RT_RCDATA);
  try
    Count := ReadInt;
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
    begin
      Count := ReadInt;
      SetLength(Result[I], Count);
      for J := 0 to Count - 1 do
        Result[I][J] := ReadFloatPoint;
    end;
  finally
    ResStream.Free;
  end;
end;


{ TControlButton }

constructor TControlButton.Create(const Center: TPoint; Size: TFloat);
begin
  inherited Create;

  FCenter := Center;
  FSize := Size;
  FSizeSqr := Sqr(FSize);
  FOutline := Circle(0, 0, Size);

  FAffineTransformation := TAffineTransformation.Create;
  FAffineTransformation.Matrix := IdentityMatrix;
end;

destructor TControlButton.Destroy;
begin
  FAffineTransformation.Free;
  inherited;
end;

procedure TControlButton.Draw(const Bmp32: TBitmap32;
  Filler: TCustomPolygonFiller);
begin
  with TRadialGradientSampler(TSamplerFiller(Filler).Sampler) do
  begin
    Center := FloatPoint(Self.FCenter.X - 2.5, Self.FCenter.Y - 2.5);
    Radius := 6;
  end;
  FAffineTransformation.Matrix[2, 0] := Center.X;
  FAffineTransformation.Matrix[2, 1] := Center.Y;
  PolygonFS(Bmp32, FOutline, Filler, pfWinding, FAffineTransformation);
  PolylineFS(Bmp32, FOutline, clBlack32, False, 1.0, jsMiter, esButt, 4,
    FAffineTransformation);
end;

procedure TControlButton.SetCenter(const Value: TPoint);
begin
  if (FCenter.X = Value.X) and (FCenter.Y = Value.Y) then Exit;
  FCenter := Value;
end;

function TControlButton.TestHitPoint(X, Y: Integer): Boolean;
begin
  Result := Sqr(FCenter.X - X) + Sqr(FCenter.Y - Y) < FSizeSqr;
end;


{TMainForm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  TextPath: TFlattenedPath;
begin
  ImgView32.SetupBitmap(True);

  FLinearBounds := Rect(50, 50, 350, 200);
  FRadialBounds := Rect(50, 250, 350, 400);

  FRadialGradientSampler := TRadialGradientSampler.Create;
  FRadialGradientSampler.Gradient.AddColorStop(0.0, $FFFFFFFF);
  FRadialGradientSampler.Gradient.AddColorStop(1.0, $FFA0A0A0);
  FControlButtonFiller := TSamplerFiller.Create(FRadialGradientSampler);
  FControlButtonFiller.Sampler := FRadialGradientSampler;

  //These text paths only need to be gotten once ...
  TextPath := TFlattenedPath.Create;
  try
    TextToPath(Self.Font.Handle, TextPath, FloatRect(50, 10, 250, 30),
      'nb: Click and drag control buttons to adjust gradients ...', 0);
    FTextNotesPoly := TextPath.Path;

    with FLinearBounds do
      TextToPath(Self.Font.Handle, TextPath,
        FloatRect(Left, Bottom, Left + 100,Bottom + 20), 'Linear gradients', 0);
    FTextTopPoly := TextPath.Path;

    with FRadialBounds do
      TextToPath(Self.Font.Handle, TextPath,
        FloatRect(Left, Bottom, Left + 100, Bottom + 20),
        'Radial gradients', 0);
    FTextBottomPoly := TextPath.Path;
  finally
    TextPath.Free;
  end;

  FTextGR32 := LoadPolysFromResource('Graphics32');

  FLinearStartBtn := TControlButton.Create(GR32.Point(100, 125), 4);
  FLinearEndBtn  := TControlButton.Create(GR32.Point(300, 125), 4);
  FRadialOriginBtn := TControlButton.Create(GR32.Point(250, 350), 4);

  with FRadialOriginBtn.Center do
  begin
    FRadialXBtn := TControlButton.Create(GR32.Point(X - 80, Y), 4);
    FRadialYBtn := TControlButton.Create(GR32.Point(X, Y + 40), 4);
  end;

  DrawImage;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLinearStartBtn.Free;
  FLinearEndBtn.Free;
  FRadialOriginBtn.Free;
  FRadialXBtn.Free;
  FRadialYBtn.Free;
end;

procedure TMainForm.ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FLinearStartBtn.TestHitPoint(X, Y) then
    FControlButton := FLinearStartBtn;
  if FLinearEndBtn.TestHitPoint(X, Y) then
    FControlButton := FLinearEndBtn;
  if FRadialXBtn.TestHitPoint(X, Y) then
    if ssCtrl in Shift then
    begin
      FRadialXBtn.FCenter.X := FRadialOriginBtn.Center.X -
        Abs(FRadialOriginBtn.Center.Y - FRadialYBtn.Center.Y);
      DrawImage;
    end
    else
      FControlButton := FRadialXBtn;
  if FRadialYBtn.TestHitPoint(X, Y) then
    if ssCtrl in Shift then
    begin
      FRadialYBtn.FCenter.Y := FRadialOriginBtn.Center.Y +
        Abs(FRadialOriginBtn.Center.X - FRadialXBtn.Center.X);
      DrawImage;
    end
    else
      FControlButton := FRadialYBtn;
  if FRadialOriginBtn.TestHitPoint(X, Y) then
    FControlButton := FRadialOriginBtn;
end;

procedure TMainForm.ImgView32MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  Delta: TPoint;
begin
  if FControlButton = FLinearStartBtn then
  begin
    X := EnsureRange(X, 10, ImgView32.ClientWidth - 10);
    Y := EnsureRange(Y, 10, ImgView32.ClientHeight - 10);
    with FLinearEndBtn do
      if (Abs(Center.X - X) < 1) and (Abs(Center.Y - Y) < 1) then Exit;
    FLinearStartBtn.Center := GR32.Point(X, Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlButton = FLinearEndBtn then
  begin
    X := EnsureRange(X, 10, ImgView32.ClientWidth - 10);
    Y := EnsureRange(Y, 10, ImgView32.ClientHeight - 10);
    with FLinearStartBtn do
      if (Abs(Center.X - X) < 1) and (Abs(Center.Y - Y) < 1) then Exit;
    FLinearEndBtn.Center := GR32.Point(X, Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlButton = FRadialOriginBtn then
  begin
    X := EnsureRange(X, FRadialBounds.Left, FRadialBounds.Right);
    Y := EnsureRange(Y, FRadialBounds.Top, FRadialBounds.Bottom);

    Delta.X := X - FRadialOriginBtn.Center.X;
    Delta.Y := Y - FRadialOriginBtn.Center.Y;
    FRadialOriginBtn.Center := GR32.Point(X, Y);
    with FRadialXBtn do Center := OffsetPoint(Center, Delta.X, Delta.Y);
    with FRadialYBtn do Center := OffsetPoint(Center, Delta.X, Delta.Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlButton = FRadialXBtn then
  begin
    X := EnsureRange(X, 10, ImgView32.ClientWidth - 10);
    Delta.X := X - FRadialOriginBtn.Center.X;
    if (Abs(Delta.X) < 3) then Exit;
    with FRadialXBtn do
      Center := GR32.Point(FRadialOriginBtn.Center.X + Delta.X, Center.Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlButton = FRadialYBtn then
  begin
    Y := EnsureRange(Y, 10, ImgView32.ClientHeight - 10);
    Delta.Y := Y - FRadialOriginBtn.Center.Y;
    if (Abs(Delta.Y) < 3) then Exit;
    with FRadialYBtn do
      Center := GR32.Point(Center.X, FRadialOriginBtn.Center.Y + Delta.Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end else
  begin

    if FLinearStartBtn.TestHitPoint(X, Y) or
      FLinearEndBtn.TestHitPoint(X, Y) or
      FRadialOriginBtn.TestHitPoint(X, Y) or
      FRadialXBtn.TestHitPoint(X, Y) or
      FRadialYBtn.TestHitPoint(X, Y) then
        Screen.Cursor := crHandPoint else
        Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ImgView32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FControlButton := nil;
end;

procedure TMainForm.DrawImage;
var
  PolygonTop, PolygonBottom: TArrayOfFloatPoint;
  Delta: TPoint;
  LinearGradFiller: TLinearGradientPolygonFiller;
  RadialGradFiller: TRadialGradientPolygonFiller;
  SVGStyleRadGradFiller: TSVGRadialGradientPolygonFiller;
const
  SimpleStyle = 0;
begin
  ImgView32.Bitmap.Clear(clCream32);
  ImgView32.Bitmap.FrameRectTS(FLinearBounds, clSilver32);
  ImgView32.Bitmap.FrameRectTS(FRadialBounds, clSilver32);

  //draw the top ellipse ...
  PolygonTop := Ellipse(200, 125, 100, 60);
  LinearGradFiller := TLinearGradientPolygonFiller.Create;
  try
    StrToArrayColor32Gradient(MemoColorStopsTop.Lines, LinearGradFiller.Gradient);
    LinearGradFiller.StartPoint := FloatPoint(FLinearStartBtn.Center);
    LinearGradFiller.EndPoint := FloatPoint(FLinearEndBtn.Center);
    PolygonFS(ImgView32.Bitmap, PolygonTop, LinearGradFiller);
    PolyLineFS(ImgView32.Bitmap, PolygonTop, ClBlack32, True, 1);

    //use LinearGradFiller to fill 'Graphics32' text  too ...
    LinearGradFiller.StartPoint := FloatPoint(230, 420);
    LinearGradFiller.EndPoint := FloatPoint(430, 420);
    PolyPolygonFS(ImgView32.Bitmap, FTextGR32, LinearGradFiller);
    PolyPolylineFS(ImgView32.Bitmap, FTextGR32, clBlack32, true, 1.2);
  finally
    LinearGradFiller.Free;
  end;

  //draw the bottom ellipse ...
  PolygonBottom := Ellipse(200, 325, 100, 60);
  if RgpEllipseFillStyle.ItemIndex = SimpleStyle then
  begin
    RadialGradFiller := TRadialGradientPolygonFiller.Create;
    try
      StrToArrayColor32Gradient(MemoColorStopsBottom.Lines, RadialGradFiller.Gradient);
      Delta.X := Abs(FRadialOriginBtn.Center.X - FRadialXBtn.Center.X);
      Delta.Y := Abs(FRadialOriginBtn.Center.Y - FRadialYBtn.Center.Y);
      with FRadialOriginBtn.FCenter do
        RadialGradFiller.EllipseBounds := FloatRect(X - Delta.X, Y - Delta.Y, X + Delta.X, Y + Delta.Y);
      PolygonFS(ImgView32.Bitmap, PolygonBottom, RadialGradFiller);
    finally
      RadialGradFiller.Free;
    end;
  end else
  begin
    SVGStyleRadGradFiller := TSVGRadialGradientPolygonFiller.Create;
    try
      StrToArrayColor32Gradient(MemoColorStopsBottom.Lines, SVGStyleRadGradFiller.Gradient);
      SVGStyleRadGradFiller.EllipseBounds := FloatRect(100, 265, 300, 385);
      SVGStyleRadGradFiller.FocalPoint := FloatPoint(FRadialOriginBtn.Center);
      PolygonFS(ImgView32.Bitmap, PolygonBottom, SVGStyleRadGradFiller);
    finally
      SVGStyleRadGradFiller.Free;
    end;
  end;
  PolylineFS(ImgView32.Bitmap, PolygonBottom, ClBlack32, True, 1);

  //draw some text ...
  PolyPolygonFS(ImgView32.Bitmap, FTextNotesPoly, clBlack32);
  PolyPolygonFS(ImgView32.Bitmap, FTextTopPoly, clBlack32);
  PolyPolygonFS(ImgView32.Bitmap, FTextBottomPoly, clBlack32);

  //finally, draw the control buttons ...
  FLinearStartBtn.Draw(ImgView32.Bitmap, FControlButtonFiller);
  FLinearEndBtn.Draw(ImgView32.Bitmap, FControlButtonFiller);
  FRadialOriginBtn.Draw(ImgView32.Bitmap, FControlButtonFiller);
  if RgpEllipseFillStyle.ItemIndex = SimpleStyle then
  begin
    FRadialXBtn.Draw(ImgView32.Bitmap, FControlButtonFiller);
    FRadialYBtn.Draw(ImgView32.Bitmap, FControlButtonFiller);
  end;
end;

procedure TMainForm.BtnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MemoColorStopsTopChange(Sender: TObject);
begin
  DrawImage;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TMainForm.RgpEllipseFillStyleClick(Sender: TObject);
begin
  DrawImage;
end;

end.
