unit MainUnit;

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, {$ELSE} Windows, {$ENDIF} SysUtils, Types,
  Classes, Graphics, Controls, Forms, Dialogs, Math, ExtCtrls, StdCtrls, Menus,
  GR32, GR32_Polygons, GR32_Image, GR32_Layers, GR32_Transforms,
  GR32_ColorGradients;

type
  TMainForm = class(TForm)
    BtnDefaults: TButton;
    CmbLUT: TComboBox;
    ImgView32: TImgView32;
    LblColorStopsTop: TLabel;
    LblLookupTableOrder: TLabel;
    MainMenu: TMainMenu;
    MemoColorStops: TMemo;
    MnuExit: TMenuItem;
    MnuFile: TMenuItem;
    MnuFileOpen: TMenuItem;
    MnuFileSaveAs: TMenuItem;
    MnuLookupTableOrder: TMenuItem;
    MnuOrder4: TMenuItem;
    MnuOrder5: TMenuItem;
    MnuOrder6: TMenuItem;
    MnuOrder7: TMenuItem;
    MnuOrder8: TMenuItem;
    MnuOrder9: TMenuItem;
    MnuOrder10: TMenuItem;
    MnuOrder11: TMenuItem;
    MnuOrder12: TMenuItem;
    MnuOrder13: TMenuItem;
    MnuPad: TMenuItem;
    MnuRadialFillStyle: TMenuItem;
    MnuReflect: TMenuItem;
    MnuRepeat: TMenuItem;
    MnuSimple: TMenuItem;
    MnuWrapMode: TMenuItem;
    MnuSVG: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    PnlControl: TPanel;
    RgpEllipseFillStyle: TRadioGroup;
    RgpWrapMode: TRadioGroup;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BtnDefaultsClick(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure CmbLUTChange(Sender: TObject);
    procedure ImgView32DblClick(Sender: TObject);
    procedure ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure MemoColorStopsChange(Sender: TObject);
    procedure MnuFileOpenClick(Sender: TObject);
    procedure MnuFileSaveAsClick(Sender: TObject);
    procedure MnuOrderClick(Sender: TObject);
    procedure MnuRadialFillStyleClick(Sender: TObject);
    procedure MnuSpreadClick(Sender: TObject);
    procedure RgpEllipseFillStyleClick(Sender: TObject);
    procedure RgpWrapModeClick(Sender: TObject);
  private
    FDpiScale: single;
    FKnobBitmap: TBitmap32;
    FKnobRadius: Integer;
    FControlKnob: PPoint;
    FLinearStart: TPoint;
    FLinearEnd: TPoint;
    FRadialOrigin: TPoint;
    FRadialX: TPoint;
    FRadialY: TPoint;

    FLinearBounds: TRect;
    FRadialBounds: TRect;
    FGradient: TColor32Gradient;
    FGradientLUT: TColor32LookupTable;
    FTextNotesPoly: TArrayOfArrayOfFloatPoint;
    FTextTopPoly: TArrayOfArrayOfFloatPoint;
    FTextBottomPoly: TArrayOfArrayOfFloatPoint;
    FTextGR32: TArrayOfArrayOfFloatPoint;
    procedure LUTOrderChangedHandler(Sender: TObject);
  public
    procedure DrawImage;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$R data.res}

uses
  GR32_Math,
  GR32_Geometry,
  GR32_VectorUtils,
  GR32_Gamma,
  GR32_Paths,
{$IFDEF FPC}
  GR32_Text_LCL_Win
{$ELSE}
  GR32_Text_VCL
{$ENDIF};

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

procedure StrToArrayColor32Gradient(s: TStrings; Gradient: TColor32Gradient);
var
  I, J: Integer;
  Offset: TFloat;
  Color: TColor32;
  ColorStr: string;
  LocalFormatSettings: TFormatSettings;
begin
  LocalFormatSettings := FormatSettings;
  LocalFormatSettings.DecimalSeparator := '.';

  Gradient.ClearColorStops;
  for i := 0 to s.Count - 1 do
  begin
    j := Pos(':', s[i]);
    if j < 2 then
      Continue;
    Offset := StrToFloatDef(Copy(s[i], 1, j - 1), -1, LocalFormatSettings);
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
  ResStream := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    Count := ReadInt;
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
    begin
      Count := ReadInt;
      SetLength(Result[I], Count);
      for J := 0 to Count - 1 do
        Result[I, J] := ReadFloatPoint;
    end;
  finally
    ResStream.Free;
  end;
end;

function DPIScale(value: integer): integer; overload;
begin
  result := mulDiv(value, screen.PixelsPerInch, 96);
end;

function DPIScale(value: single): single; overload;
begin
  result := value * screen.PixelsPerInch / 96;
end;

function DpiAwarePoint(const x, y: integer): TPoint;
begin
  result := Gr32.Point(DPIScale(x), DPIScale(y));
end;

function DpiAwareRect(const l, t, r, b: integer): TRect;
begin
  result := Rect(DPIScale(l), DPIScale(t), DPIScale(r), DPIScale(b));
end;

function DpiAwareFloatPoint(const x, y: integer): TFloatPoint;
begin
  result := FloatPoint(DPIScale(x), DPIScale(y));
end;

function DpiAwareFloatRect(const l, t, r, b: single): TFloatRect;
begin
  result := FloatRect(DPIScale(l), DPIScale(t), DPIScale(r), DPIScale(b));
end;

procedure OffsetPolygon(var polygon: TArrayOfFloatPoint; dx, dy: single);
var
  i: integer;
begin
  for i := 0 to high(polygon) do
  begin
    polygon[i].X := polygon[i].X + dx;
    polygon[i].Y := polygon[i].Y + dy;
  end;
end;

procedure OffsetPolyPolygon(var polygons: TArrayOfArrayOfFloatPoint; dx, dy: single);
var
  i: integer;
begin
  for i := 0 to high(polygons) do
    OffsetPolygon(polygons[i], dx, dy);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  TextPath: TFlattenedPath;
  Outline: TArrayOfFloatPoint;
  Filler: TSamplerFiller;
  Sampler: TRadialGradientSampler;
begin

  if Screen.PixelsPerInch > 96 then
    FDpiScale := Screen.PixelsPerInch/ 96 else
    FDpiScale := 1;

  ClientWidth := PnlControl.Width + DPIScale(400);
  ClientHeight := DPIScale(450);

  ImgView32.SetupBitmap(true, clCream32);

  FLinearBounds := DpiAwareRect(50, 50, 350, 200);
  FRadialBounds := DpiAwareRect(50, 250, 350, 400);

  FGradient := TColor32Gradient.Create;
  StrToArrayColor32Gradient(MemoColorStops.Lines, FGradient);

  FGradientLUT := TColor32LookupTable.Create;
  FGradientLUT.OnOrderChanged := LUTOrderChangedHandler;
  FGradient.FillColorLookUpTable(FGradientLUT);

  //These text paths only need to be gotten once ...
  TextPath := TFlattenedPath.Create;
  try
    TextToPath(Self.Font.Handle, TextPath, DpiAwareFloatRect(50, 10, 450, 30),
      'Click & drag control buttons to adjust gradients', 0);
    FTextNotesPoly := TextPath.Path;

    with FLinearBounds do
      TextToPath(Self.Font.Handle, TextPath,
        FloatRect(Left, Bottom, Left + DPIScale(150),Bottom + DPIScale(20)),
        'Linear gradients', 0);
    FTextTopPoly := TextPath.Path;

    with FRadialBounds do
      TextToPath(Self.Font.Handle, TextPath,
        FloatRect(Left, Bottom, Left + DPIScale(150), Bottom + DPIScale(20)),
        'Radial gradients', 0);
    FTextBottomPoly := TextPath.Path;
  finally
    TextPath.Free;
  end;

  FTextGR32 := LoadPolysFromResource('Graphics32_Crv');
  OffsetPolyPolygon(FTextGR32, DPIScale(-42), 0);
  if FDpiScale > 1 then
    FTextGR32 := ScalePolyPolygon(FTextGR32, FDpiScale, FDpiScale);

  FKnobRadius := DPIScale(4);
  FKnobBitmap := TBitmap32.Create;
  FKnobBitmap.SetSize(2 * FKnobRadius + 2, 2 * FKnobRadius + 2);
  FKnobBitmap.DrawMode := dmBlend;
  FKnobBitmap.CombineMode := cmMerge;
  Sampler := TRadialGradientSampler.Create;
  try
    Sampler.Gradient.AddColorStop(0.0, $FFFFFFFF);
    Sampler.Gradient.AddColorStop(1.0, $FFA0A0A0);
    Sampler.Radius := FKnobRadius + FKnobRadius div 2;
    Sampler.Center := FloatPoint(FKnobRadius - 1.5, FKnobRadius - 1.5);

    Filler := TSamplerFiller.Create(Sampler);
    try
      Filler.Sampler := Sampler;

      Outline := Circle(FKnobRadius + 1, FKnobRadius + 1, FKnobRadius);
      PolygonFS(FKnobBitmap, Outline, Filler, pfWinding);
      PolylineFS(FKnobBitmap, Outline, clBlack32, True);
    finally
      Filler.Free;
    end;
  finally
    Sampler.Free;
  end;

  FLinearStart := DpiAwarePoint(100, 125);
  FLinearEnd  := DpiAwarePoint(300, 125);
  FRadialOrigin := DpiAwarePoint(250, 350);

  with FRadialOrigin do
  begin
    FRadialX := GR32.Point(X - DPIScale(80), Y);
    FRadialY := GR32.Point(X, Y + DPIScale(40));
  end;

  DrawImage;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FGradient.Free;
  FKnobBitmap.Free;
end;

procedure TMainForm.ImgView32DblClick(Sender: TObject);
begin
  case 0 of
    0:
      begin
        FLinearStart := DpiAwarePoint(200, 70);
        FLinearEnd := DpiAwarePoint(200, 170);
      end;
    1:
      begin
        FLinearStart := DpiAwarePoint(200, 120);
        FLinearEnd := DpiAwarePoint(200, 120);
      end;
    2:
      begin
        FLinearStart := DpiAwarePoint(200, 120);
        FLinearEnd := DpiAwarePoint(201, 120);
      end;
    3:
      begin
        FLinearStart := DpiAwarePoint(200, 100);
        FLinearEnd := DpiAwarePoint(200, 140);
      end;
  end;

  FRadialOrigin := DpiAwarePoint(331, 325);

  DrawImage;
end;

function TestHitPoint(X, Y: Integer; Point: TPoint; Radius: TFloat): Boolean;
begin
  Result := Sqr(X - Point.X) + Sqr(Y - Point.Y) < Sqr(Radius);
end;

procedure TMainForm.ImgView32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if TestHitPoint(X, Y, FLinearStart, FKnobRadius) then
    FControlKnob := @FLinearStart;
  if TestHitPoint(X, Y, FLinearEnd, FKnobRadius) then
    FControlKnob := @FLinearEnd;
  if TestHitPoint(X, Y, FRadialX, FKnobRadius) then
    if ssCtrl in Shift then
    begin
      FRadialX.X := FRadialOrigin.X - Abs(FRadialOrigin.Y -
        FRadialY.Y);
      DrawImage;
    end
    else
      FControlKnob := @FRadialX;
  if TestHitPoint(X, Y, FRadialY, FKnobRadius) then
    if ssCtrl in Shift then
    begin
      FRadialY.Y := FRadialOrigin.Y + Abs(FRadialOrigin.X - FRadialX.X);
      DrawImage;
    end
    else
      FControlKnob := @FRadialY;
  if TestHitPoint(X, Y, FRadialOrigin, FKnobRadius) then
    FControlKnob := @FRadialOrigin;
end;

procedure TMainForm.ImgView32MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
var
  Delta: TPoint;
begin
  if FControlKnob = @FLinearStart then
  begin
    X := EnsureRange(X, 10, ImgView32.ClientWidth - 10);
    Y := EnsureRange(Y, 10, ImgView32.ClientHeight - 10);
    if (Abs(FLinearEnd.X - X) < 1) and (Abs(FLinearEnd.Y - Y) < 1) then
      Exit;
    FLinearStart := GR32.Point(X, Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlKnob = @FLinearEnd then
  begin
    X := EnsureRange(X, 10, ImgView32.ClientWidth - 10);
    Y := EnsureRange(Y, 10, ImgView32.ClientHeight - 10);
    if (Abs(FLinearStart.X - X) < 1) and (Abs(FLinearStart.Y - Y) < 1) then
      Exit;
    FLinearEnd := GR32.Point(X, Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlKnob = @FRadialOrigin then
  begin
    X := EnsureRange(X, FRadialBounds.Left, FRadialBounds.Right);
    Y := EnsureRange(Y, FRadialBounds.Top, FRadialBounds.Bottom);

    Delta.X := X - FRadialOrigin.X;
    Delta.Y := Y - FRadialOrigin.Y;
    FRadialOrigin := GR32.Point(X, Y);
    FRadialX := OffsetPoint(FRadialX, Delta.X, Delta.Y);
    FRadialY := OffsetPoint(FRadialY, Delta.X, Delta.Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlKnob = @FRadialX then
  begin
    X := EnsureRange(X, 10, ImgView32.ClientWidth - 10);
    Delta.X := X - FRadialOrigin.X;
    if (Abs(Delta.X) < 3) then Exit;
      FRadialX := GR32.Point(FRadialOrigin.X + Delta.X, FRadialX.Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end
  else if FControlKnob = @FRadialY then
  begin
    Y := EnsureRange(Y, 10, ImgView32.ClientHeight - 10);
    Delta.Y := Y - FRadialOrigin.Y;
    if (Abs(Delta.Y) < 3) then Exit;
    FRadialY := GR32.Point(FRadialY.X, FRadialOrigin.Y + Delta.Y);
    DrawImage;
    Screen.Cursor := crHandPoint;
  end else
  begin
    if TestHitPoint(X, Y, FLinearStart, FKnobRadius) or
      TestHitPoint(X, Y, FLinearEnd, FKnobRadius) or
      TestHitPoint(X, Y, FRadialOrigin, FKnobRadius) or
      TestHitPoint(X, Y, FRadialX, FKnobRadius) or
      TestHitPoint(X, Y, FRadialY, FKnobRadius) or
      Assigned(FControlKnob) then
    begin
      Screen.Cursor := crHandPoint;
      ImgView32.Cursor := crHandPoint;
    end
    else
    begin
      Screen.Cursor := crDefault;
      ImgView32.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.ImgView32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FControlKnob := nil;
end;

procedure TMainForm.DrawImage;
var
  PolygonTop, PolygonBottom: TArrayOfFloatPoint;
  Delta: TPoint;
  LinearGradFiller: TCustomLinearGradientPolygonFiller;
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
  if FDpiScale > 1 then
    PolygonTop := ScalePolygon(PolygonTop, FDpiScale, FDpiScale);

  LinearGradFiller := TLinearGradientPolygonFiller.Create(FGradientLUT);
  try
    LinearGradFiller.StartPoint := FloatPoint(FLinearStart);
    LinearGradFiller.EndPoint := FloatPoint(FLinearEnd);
    LinearGradFiller.WrapMode := TWrapMode(RgpWrapMode.ItemIndex);

    PolygonFS(ImgView32.Bitmap, PolygonTop, LinearGradFiller);
    PolyLineFS(ImgView32.Bitmap, PolygonTop, clBlack32, True, 1);

    //use LinearGradFiller to fill 'Graphics32' text  too ...
    LinearGradFiller.StartPoint := DpiAwareFloatPoint(230, 420);
    LinearGradFiller.EndPoint := DpiAwareFloatPoint(430, 420);
    PolyPolygonFS(ImgView32.Bitmap, FTextGR32, LinearGradFiller);
    PolyPolylineFS(ImgView32.Bitmap, FTextGR32, clBlack32, True, 1.2);
  finally
    LinearGradFiller.Free;
  end;

  //draw the bottom ellipse ...
  PolygonBottom := Ellipse(200, 325, 100, 60);
  if FDpiScale > 1 then
    PolygonBottom := ScalePolygon(PolygonBottom, FDpiScale, FDpiScale);

  if RgpEllipseFillStyle.ItemIndex = SimpleStyle then
  begin
    RadialGradFiller := TRadialGradientPolygonFiller.Create(FGradientLUT);
    try
      RadialGradFiller.WrapMode := TWrapMode(RgpWrapMode.ItemIndex);
      Delta.X := Abs(FRadialOrigin.X - FRadialX.X);
      Delta.Y := Abs(FRadialOrigin.Y - FRadialY.Y);
      with FRadialOrigin do
        RadialGradFiller.EllipseBounds := FloatRect(X - Delta.X, Y - Delta.Y,
          X + Delta.X, Y + Delta.Y);
      PolygonFS(ImgView32.Bitmap, PolygonBottom, RadialGradFiller);
    finally
      RadialGradFiller.Free;
    end;
  end else
  begin
    SVGStyleRadGradFiller := TSVGRadialGradientPolygonFiller.Create(FGradientLUT);
    try
      SVGStyleRadGradFiller.EllipseBounds := DpiAwareFloatRect(100, 265, 300, 385);
      SVGStyleRadGradFiller.FocalPoint := FloatPoint(FRadialOrigin);
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

  with ImgView32.Bitmap do
  begin
    Draw(FLinearStart.X - FKnobRadius, FLinearStart.Y - FKnobRadius, FKnobBitmap);
    Draw(FLinearEnd.X - FKnobRadius, FLinearEnd.Y - FKnobRadius, FKnobBitmap);
    Draw(FRadialOrigin.X - FKnobRadius, FRadialOrigin.Y - FKnobRadius, FKnobBitmap);
    if RgpEllipseFillStyle.ItemIndex = SimpleStyle then
    begin
      Draw(FRadialX.X - FKnobRadius, FRadialX.Y - FKnobRadius, FKnobBitmap);
      Draw(FRadialY.X - FKnobRadius, FRadialY.Y - FKnobRadius, FKnobBitmap);
    end;
  end;
end;

procedure TMainForm.BtnDefaultsClick(Sender: TObject);
begin
  with MemoColorStops do
  begin
    Clear;
    Lines.BeginUpdate;
    Lines.Add('0.0: clRed32');
    Lines.Add('0.1: clYellow32');
    Lines.Add('0.3: clLime32');
    Lines.Add('0.5: $AA00FFFF');
    Lines.Add('0.7: clBlue32');
    Lines.Add('0.9: clFuchsia32');
    Lines.Add('1.0: $80FF0000');
    Lines.EndUpdate;
  end;
end;

procedure TMainForm.BtnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.CmbLUTChange(Sender: TObject);
begin
  case CmbLUT.ItemIndex of
    0: MnuOrder4.Checked := True;
    1: MnuOrder5.Checked := True;
    2: MnuOrder6.Checked := True;
    3: MnuOrder7.Checked := True;
    4: MnuOrder8.Checked := True;
    5: MnuOrder9.Checked := True;
    6: MnuOrder10.Checked := True;
    7: MnuOrder11.Checked := True;
    8: MnuOrder12.Checked := True;
    9: MnuOrder13.Checked := True;
  end;
  FGradientLUT.Order := 4 + CmbLUT.ItemIndex;
end;

procedure TMainForm.MemoColorStopsChange(Sender: TObject);
begin
  StrToArrayColor32Gradient(MemoColorStops.Lines, FGradient);
  FGradient.FillColorLookUpTable(FGradientLUT);

  DrawImage;
end;

procedure TMainForm.MnuFileOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    MemoColorStops.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TMainForm.MnuFileSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    MemoColorStops.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.MnuOrderClick(Sender: TObject);
begin
  CmbLUT.ItemIndex := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
  FGradientLUT.Order := 4 + CmbLUT.ItemIndex;
end;

procedure TMainForm.MnuRadialFillStyleClick(Sender: TObject);
begin
  RgpEllipseFillStyle.ItemIndex := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
  DrawImage;
end;

procedure TMainForm.MnuSpreadClick(Sender: TObject);
begin
  RgpWrapMode.ItemIndex := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
  DrawImage;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TMainForm.RgpEllipseFillStyleClick(Sender: TObject);
begin
  case RgpEllipseFillStyle.ItemIndex of
    0: MnuSimple.Checked := True;
    1: MnuSVG.Checked := True;
  end;
  DrawImage;
end;

procedure TMainForm.RgpWrapModeClick(Sender: TObject);
begin
  case RgpWrapMode.ItemIndex of
    0: MnuPad.Checked := True;
    1: MnuReflect.Checked := True;
    2: MnuRepeat.Checked := True;
  end;
  DrawImage;
end;

procedure TMainForm.LUTOrderChangedHandler(Sender: TObject);
begin
  FGradient.FillColorLookUpTable(FGradientLUT);
  DrawImage;
end;

initialization
  SetGamma(1);

end.
