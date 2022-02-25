unit MainUnit;

// NOTE: This demo is yet incomplete and needs finishing until v2.0 can be
//       released!


interface

{$I GR32.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, GR32, GR32_Image, GR32_Polygons, GR32_Paths, GR32_Brushes;

type
  TFrmSvgPathRenderer = class(TForm)
    Image32: TImage32;
    LblPathData: TLabel;
    EditSVGPath: TEdit;
    ShpFillColor: TShape;
    ShpStrokeColor: TShape;
    ColorDialog: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditSVGPathChange(Sender: TObject);
    procedure EditSVGPathKeyPress(Sender: TObject; var Key: Char);
    procedure Image32Resize(Sender: TObject);
    procedure ShpFillColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShpStrokeColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FCanvas32: TCanvas32;
    FFill: TSolidBrush;
    FStroke: TStrokeBrush;
    procedure RenderPath(Path: AnsiString);
    procedure UpdatePath;
  end;

var
  FrmSvgPathRenderer: TFrmSvgPathRenderer;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Types, UITypes;

resourcestring
  RCStrNotYetImplemented = 'Not yet implemented!';
  RCStrUnknownFirstCommand = 'Unknown first command (%s)';
  RCStrUnknownCommand = 'Unknown command (%s)';

procedure TFrmSvgPathRenderer.FormCreate(Sender: TObject);
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);
  FCanvas32 := TCanvas32.Create(Image32.Bitmap);;
  FFill := TSolidBrush.Create(FCanvas32.Brushes);
  FFill.FillColor := SetAlpha(Color32(ShpFillColor.Brush.Color), $7F);
  FStroke := TStrokeBrush.Create(FCanvas32.Brushes);
  FStroke.FillColor := Color32(ShpStrokeColor.Brush.Color);
  FStroke.StrokeWidth := 3;
  FStroke.FillMode := pfWinding;
end;

procedure TFrmSvgPathRenderer.FormDestroy(Sender: TObject);
begin
  FCanvas32.Free;
end;

procedure TFrmSvgPathRenderer.Image32Resize(Sender: TObject);
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);
  EditSVGPathChange(Sender);
end;

procedure TFrmSvgPathRenderer.EditSVGPathKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    EditSVGPathChange(Sender);
end;

procedure TFrmSvgPathRenderer.EditSVGPathChange(Sender: TObject);
begin
  UpdatePath;
end;

procedure TFrmSvgPathRenderer.UpdatePath;
begin
  try
    if Length(EditSVGPath.Text) > 0 then
      RenderPath(AnsiString(EditSVGPath.Text));
  except
    on E: Exception do MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFrmSvgPathRenderer.RenderPath(Path: AnsiString);
type
  TPointF = record
    X, Y: Double;
  end;
  TSvgPathCommand = (pcMoveTo, pcLineTo, pcHorizontalLineTo, pcVerticalLineTo,
    pcCubicTo, pcSmoothCubicTo, pcQuadTo, pcSmoothQuadTo, pcArcTo, pcClosePath);
const
  CDeg2Rad = Pi / 180;
var
  Index: Integer;
  Command: TSvgPathCommand;
  Relative, ReadCommand: Boolean;
  LastPos, Current, Radius, FirstPoint: TPointF;
  Control: array [0..1] of TPointF;
  LargeArc, SweepFlag: Boolean;
  Angle: Double;

  procedure SkipWhitespaces;
  begin
    while Index <= Length(Path) do
      if Path[Index] in [#$20, #$9, #$D, #$A] then
        Inc(Index)
      else
        Break;
  end;

  procedure CommaWhitespaces;
  begin
    while Index <= Length(Path) do
      if Path[Index] in [#$20, #$9, #$D, #$A, ','] then
        Inc(Index)
      else
        Break;
  end;

  function ReadDigitSequence: string;
  begin
    while Index <= Length(Path) do
    begin
      if Path[Index] in ['0'..'9'] then
        Result := Result + Char(Path[Index])
      else
        Break;
      Inc(Index);
    end;
  end;

  function ReadNumber: Double;
  var
    FloatStr: string;
  begin
    if Index > Length(Path) then
      raise Exception.Create('No Data');

    if Path[Index] in ['+', '-'] then
    begin
      // actually read sign
      FloatStr := string(Path[Index]);
      Inc(Index);
    end
    else
      FloatStr := '';

    FloatStr := FloatStr + ReadDigitSequence;
    if (Index < Length(Path)) and (Path[Index] = '.') then
    begin
      // fractional number
      Inc(Index);
      FloatStr := FloatStr + FormatSettings.DecimalSeparator + ReadDigitSequence;
    end;

    if (Index < Length(Path)) and (Path[Index] in ['e', 'E']) then
    begin
      FloatStr := FloatStr + 'E';
      Inc(Index);

      // eventually read exponent sign
      if (Index < Length(Path)) and (Path[Index] in ['+', '-']) then
      begin
        FloatStr := FloatStr + string(Path[Index]);
        Inc(Index);
      end;

      FloatStr := FloatStr + ReadDigitSequence;
    end;
    Result := StrToFloat(FloatStr);
  end;

  procedure ReadPoint(var Point: TPointF);
  begin
    Point.X := ReadNumber;
    CommaWhitespaces;
    Point.Y := ReadNumber;
    SkipWhitespaces;
  end;

  procedure ArcEndpointToCenterParameterization;
  var
    StartAngle, DeltaAngle, MaxRadius: Double;
    ComplexAngle, TempStart, TempCenter, Center, Scale: TPointF;
    RadLen, Numr, Denr, Sig: Double;
  begin
    SinCos(Angle, ComplexAngle.X, ComplexAngle.Y);

    if Radius.X = Radius.Y then
    begin
      MaxRadius := Radius.X;
      TempStart.X :=  ComplexAngle.Y * (LastPos.X - Current.X) * 0.5 + ComplexAngle.X * (LastPos.Y - Current.Y) * 0.5;
      TempStart.Y := -ComplexAngle.X * (LastPos.X - Current.X) * 0.5 + ComplexAngle.Y * (LastPos.Y - Current.Y) * 0.5;

      RadLen := (Sqr(TempStart.X) + Sqr(TempStart.Y)) / Sqr(MaxRadius);
      if RadLen > 1 then
        MaxRadius := MaxRadius * Sqrt(RadLen);

      // compute (cx', cy')
      if LargeArc = SweepFlag then
        Sig := -1
      else
        Sig := 1;
      Sig := Sig * Sqrt(Sqr(MaxRadius) / (Sqr(TempStart.Y) + Sqr(TempStart.X)) - 1);

      if IsNaN(Sig) or (Abs(Sig) < 1E-6) then
        Sig := 0;
      TempCenter.x :=  Sig * TempStart.Y;
      TempCenter.y := -Sig * TempStart.X;

      Center.x := (LastPos.X + Current.X) * 0.5 + ComplexAngle.Y * TempCenter.x - ComplexAngle.X * TempCenter.y;
      Center.y := (LastPos.Y + Current.Y) * 0.5 + ComplexAngle.X * TempCenter.x + ComplexAngle.Y * TempCenter.y;

      StartAngle := ArcTan2(TempStart.y - TempCenter.y,
        TempStart.X - TempCenter.x);
      DeltaAngle := Pi + ArcTan2(TempStart.y + TempCenter.y,
        TempStart.X + TempCenter.x);

      if (SweepFlag = False) and (StartAngle > 0) then
        StartAngle := StartAngle - 2 * Pi;
      if (SweepFlag = True) and (StartAngle < 0) then
        StartAngle := StartAngle + 2 * Pi;
    end
    else
    begin
      TempStart.X :=  ComplexAngle.Y * (LastPos.X - Current.X) * 0.5 + ComplexAngle.X * (LastPos.Y - Current.Y) * 0.5;
      TempStart.Y := -ComplexAngle.X * (LastPos.X - Current.X) * 0.5 + ComplexAngle.Y * (LastPos.Y - Current.Y) * 0.5;

      RadLen := Sqr(TempStart.X) / Sqr(Radius.X) + Sqr(TempStart.Y) / Sqr(Radius.Y);
      if RadLen > 1 then
      begin
        Radius.X := Radius.X * Sqrt(RadLen);
        Radius.Y := Radius.Y * Sqrt(RadLen);
      end;

      // compute (cx', cy')
      Numr := Sqr(Radius.X) * (Sqr(Radius.Y) - Sqr(TempStart.Y)) - Sqr(Radius.Y) * Sqr(TempStart.X);
      Denr := Sqr(Radius.X) * Sqr(TempStart.Y) + Sqr(Radius.Y) * Sqr(TempStart.X);
      if LargeArc = SweepFlag then
        Sig := -1
      else
        Sig := 1;
      Sig := Sig * Sqrt(Numr / Denr);

      if IsNaN(Sig) or (Abs(Sig) < 1E-6) then
        Sig := 0;
      TempCenter.x := Sig *  Radius.x * TempStart.Y / Radius.y;
      TempCenter.y := Sig * -Radius.y * TempStart.X / Radius.x;

      // compute (cx, cy) from (cx', cy')
      Center.x := (LastPos.X + Current.X) * 0.5 + ComplexAngle.Y * TempCenter.x - ComplexAngle.X * TempCenter.y;
      Center.y := (LastPos.Y + Current.Y) * 0.5 + ComplexAngle.X * TempCenter.x + ComplexAngle.Y * TempCenter.y;

      StartAngle := ArcTan2((TempStart.y - TempCenter.y) / Radius.y,
        (TempStart.X - TempCenter.x) / Radius.X);
      DeltaAngle := Pi + ArcTan2((TempStart.y + TempCenter.y) / Radius.y,
        (TempStart.X + TempCenter.x) / Radius.X);

      if (SweepFlag = False) and (StartAngle > 0) then
        StartAngle := StartAngle - 2 * Pi;
      if (SweepFlag = True) and (StartAngle < 0) then
        StartAngle := StartAngle + 2 * Pi;

      if Radius.X > Radius.Y then
      begin
        MaxRadius := Radius.X;
        Scale.x := 1;
        Scale.y := Radius.y / Radius.X;
      end
      else
      begin
        MaxRadius := Radius.Y;
        Scale.x := Radius.x / Radius.y;
        Scale.y := 1;
      end;
    end;
  end;

begin
  FormatSettings.DecimalSeparator := '.';
  Image32.Bitmap.Clear($FFFFFFFF);

  // ignore all whitespaces ahead
  Index := 1;
  SkipWhitespaces;

  // check first path command is a move (absolute/relative)
  if Path[Index] in ['m', 'M'] then
    Command := pcLineTo // all subsequent coordinates are LineTo segments!
  else
    raise Exception.CreateFmt(RCStrUnknownFirstCommand, [Path[Index]]);
  Relative := Ord(Path[Index]) > 60;
  Inc(Index);

  SkipWhitespaces;
  Current.X := ReadNumber;
  CommaWhitespaces;
  SkipWhitespaces;
  Current.Y := ReadNumber;
  SkipWhitespaces;
  LastPos := FirstPoint;
  LastPos := Current;

  // ToDo: Evaluate 'Relative', implement subsequent LineTo commands!

  FCanvas32.MoveTo(LastPos.X, LastPos.Y);

  while Index <= Length(Path) do
  begin
    ReadCommand := True;
    case Path[Index] of
      'M', 'm': Command := pcMoveTo;
      'L', 'l': Command := pcLineTo;
      'H', 'h': Command := pcHorizontalLineTo;
      'V', 'v': Command := pcVerticalLineTo;
      'C', 'c': Command := pcCubicTo;
      'Q', 'q': Command := pcQuadTo;
      'S', 's': Command := pcSmoothCubicTo;
      'T', 't': Command := pcSmoothQuadTo;
      'A', 'a': Command := pcArcTo;
      'Z', 'z': Command := pcClosePath;
      '0'..'9', '+', '-', '.': ReadCommand := False;
    else
      raise Exception.CreateFmt(RCStrUnknownCommand, [Path[1]]);
    end;

    if ReadCommand then
    begin
      Relative := Ord(Path[Index]) > $60;
      Inc(Index);
      SkipWhitespaces;
    end;

    case Command of
      pcHorizontalLineTo:
        begin
          Current.X := ReadNumber;
          SkipWhitespaces;
        end;

      pcVerticalLineTo:
        begin
          Current.Y := ReadNumber;
          SkipWhitespaces;
        end;

      pcMoveTo, pcLineTo, pcSmoothQuadTo:
        begin
          ReadPoint(Current);
        end;

      pcSmoothCubicTo, pcQuadTo:
        begin
          ReadPoint(Control[0]);
          ReadPoint(Current);
        end;

      pcCubicTo:
        begin
          ReadPoint(Control[0]);
          ReadPoint(Control[1]);
          ReadPoint(Current);
        end;

      pcArcTo:
        begin
          ReadPoint(Radius);

          Angle := ReadNumber;
          SkipWhitespaces;

          // large arc flag
          LargeArc := Path[Index] = '1';
          Inc(Index);
          SkipWhitespaces;

          // sweep-flag
          SweepFlag := Path[Index] = '1';
          Inc(Index);
          SkipWhitespaces;

          ReadPoint(Current);
        end;
    end;

    case Command of
      pcMoveTo:
        begin
          // MoveTo performs an implicit EndPath;
          if Relative then
            FCanvas32.MoveToRelative(Current.X, Current.Y)
          else
            FCanvas32.MoveTo(Current.X, Current.Y);

          Command := pcLineTo; // all subsequent coordinates are LineTo segments!
        end;

      pcLineTo:
        if Relative then
          FCanvas32.LineToRelative(Current.X, Current.Y)
        else
          FCanvas32.LineTo(Current.X, Current.Y);

      pcHorizontalLineTo:
        if Relative then
          FCanvas32.HorizontalLineToRelative(Current.X)
        else
          FCanvas32.HorizontalLineTo(Current.X);

      pcVerticalLineTo:
        if Relative then
          FCanvas32.VerticalLineToRelative(Current.Y)
        else
          FCanvas32.VerticalLineTo(Current.Y);

      pcSmoothQuadTo:
        begin
          if Relative then
            FCanvas32.ConicToRelative(Current.X, Current.Y)
          else
            FCanvas32.ConicTo(Current.X, Current.Y)
        end;

      pcQuadTo:
        begin
          if Relative then
            FCanvas32.ConicToRelative(Control[0].X, Control[0].Y, Current.X, Current.Y)
          else
            FCanvas32.ConicTo(Control[0].X, Control[0].Y, Current.X, Current.Y);
        end;

      pcSmoothCubicTo:
        if Relative then
          FCanvas32.CurveToRelative(Control[0].X, Control[0].Y, Current.X, Current.Y)
        else
          FCanvas32.CurveTo(Control[0].X, Control[0].Y, Current.X, Current.Y);

      pcCubicTo:
        if Relative then
          FCanvas32.CurveToRelative(Control[0].X, Control[0].Y, Control[1].X, Control[1].Y, Current.X, Current.Y)
        else
          FCanvas32.CurveTo(Control[0].X, Control[0].Y, Control[1].X, Control[1].Y, Current.X, Current.Y);

      pcArcTo:
        begin
          raise Exception.Create(RCStrNotYetImplemented);
//          FCanvas32.Arc(Radius.X, Radius.Y, );
          //ArcEndpointToCenterParameterization;
        end;

      pcClosePath:
        begin
          FCanvas32.EndPath(True);
          Current := FirstPoint;
        end;
    end;

    LastPos := Current;
  end;

  FCanvas32.EndPath;
end;

procedure TFrmSvgPathRenderer.ShpFillColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShpFillColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShpFillColor.Brush.Color := ColorDialog.Color;
    FFill.FillColor := SetAlpha(Color32(ShpFillColor.Brush.Color), $7F);
    UpdatePath;
  end;
end;

procedure TFrmSvgPathRenderer.ShpStrokeColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := ShpStrokeColor.Brush.Color;
  if ColorDialog.Execute then
  begin
    ShpStrokeColor.Brush.Color := ColorDialog.Color;
    FStroke.FillColor := Color32(ShpStrokeColor.Brush.Color);
    UpdatePath;
  end;
end;

end.
