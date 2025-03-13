unit MainUnit;

{$include GR32.inc}

interface

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Menus, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Math,
  GR32,
  GR32_Image,
  GR32_Layers;

type
  TFrmBlurs = class(TForm)
    MnuBlurType: TMenuItem;
    CbxBidirectional: TCheckBox;
    MnuFile: TMenuItem;
    ImgViewPage1: TImgView32;
    ImgViewPage2: TImgView32;
    ImgViewPage3: TImgView32;
    LblBlurAngle: TLabel;
    LblBlurRadius: TLabel;
    MainMenu: TMainMenu;
    MnuExit: TMenuItem;
    MnuGaussianType: TMenuItem;
    MnuMotion: TMenuItem;
    MnuNone: TMenuItem;
    N1: TMenuItem;
    MnuOpen: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PnlControl: TPanel;
    RgpBlurType: TRadioGroup;
    SbrMain: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TbrBlurAngle: TTrackBar;
    TbrBlurRadius: TTrackBar;
    CheckBoxCorrectGamma: TCheckBox;
    LabelDelta: TLabel;
    TrackBarDelta: TTrackBar;
    PanelSelective: TPanel;
    MnuSelective: TMenuItem;
    PanelMotion: TPanel;
    TimerUpdate: TTimer;
    PanelRadius: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MnuExitClick(Sender: TObject);
    procedure MnuGaussianTypeClick(Sender: TObject);
    procedure MnuOpenClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure RgpBlurTypeClick(Sender: TObject);
    procedure TbrBlurAngleChange(Sender: TObject);
    procedure TbrBlurRadiusChange(Sender: TObject);
    procedure TrackBarDeltaChange(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
    FBitmapStoneWeed: TBitmap32;
    FBitmapIceland: TBitmap32;
    FBitmapRandBox: TBitmap32;
    FLayerBitmap: TBitmapLayer;

    FRedrawFlag: Boolean;

    procedure Redraw;
    procedure QueueUpdate;

  end;

var
  FrmBlurs: TFrmBlurs;

implementation

uses
{$if defined(UseInlining)}
  Types,
{$ifend}
  GR32.ImageFormats.JPG,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_System,
  GR32.Blur,
  GR32.Blur.SelectiveGaussian,
  GR32_Blurs;

{$R *.dfm}

const
  GaussianBlurSimple: array [Boolean] of TBlurFunction = (Blur32, GammaBlur32);
  GaussianBlurBounds: array [Boolean] of TBlurFunctionBounds = (Blur32, GammaBlur32);
  GaussianBlurRegion: array [Boolean] of TBlurFunctionRegion = (Blur32, GammaBlur32);

{ Miscellaneous functions }

procedure DrawFramedBox(Bmp32: TBitmap32; const Rec: TRect;
  Color1, Color2: TColor32; LineWidth: TFloat);
var
  Pts: TArrayOfFloatPoint;
begin
  if LineWidth < 1 then
    LineWidth := 1;
  SetLength(Pts, 6);

  Pts[0] := FloatPoint(Rec.Left, Rec.Bottom);
  Pts[1] := FloatPoint(Rec.Left, Rec.Top);
  Pts[2] := FloatPoint(Rec.Right, Rec.Top);
  Pts[3] := FloatPoint(Rec.Right - LineWidth, Rec.Top + LineWidth);
  Pts[4] := FloatPoint(Rec.Left + LineWidth, Rec.Top + LineWidth);
  Pts[5] := FloatPoint(Rec.Left + LineWidth, Rec.Bottom - LineWidth);
  PolygonFS(Bmp32, Pts, Color1);

  Pts[1] := FloatPoint(Rec.Right, Rec.Bottom);
  Pts[2] := FloatPoint(Rec.Right, Rec.Top);
  Pts[3] := FloatPoint(Rec.Right - LineWidth, Rec.Top + LineWidth);
  Pts[4] := FloatPoint(Rec.Right - LineWidth, Rec.Bottom - LineWidth);
  Pts[5] := FloatPoint(Rec.Left + LineWidth, Rec.Bottom - LineWidth);
  PolygonFS(Bmp32, Pts, Color2);
end;

{ TFrmBlurs }

procedure TFrmBlurs.FormCreate(Sender: TObject);
var
  X, Y: Integer;
const
  Colors: array [0 .. 21] of TColor32 = (clAliceBlue32, clAquamarine32,
    clAzure32, clBeige32, clBlueViolet32, clCadetblue32, clChocolate32,
    clCoral32, clCornFlowerBlue32, clCornSilk32, clCrimson32,
    clDarkBlue32, clDarkCyan32, clDarkGoldenRod32, clDarkGreen32,
    clDarkMagenta32, clDarkOrange32, clDarkOrchid32, clDarkRed32,
    clDarkSalmon32, clDarkSeaGreen32, clDarkSlateBlue32);
begin
  FBitmapStoneWeed := TBitmap32.create;
  FBitmapStoneWeed.DrawMode := dmBlend;
  FBitmapStoneWeed.LoadFromResourceName(hInstance, 'STONEWEED', RT_RCDATA);

  FBitmapIceland := TBitmap32.create;
  FBitmapIceland.DrawMode := dmBlend;
  FBitmapIceland.LoadFromResourceName(hInstance, 'ICELAND', RT_RCDATA);

  Randomize;
  FBitmapRandBox := TBitmap32.create;
  // Generate an image of full of random, semi-transparent, colored boxes ...
  FBitmapRandBox.SetSize(192, 272);
  for X := 0 to 11 do
    for Y := 0 to 16 do
      FBitmapRandBox.FillRectS(X * 16, Y * 16, 300 + (X + 1) * 16,
        40 + (Y + 1) * 16, SetAlpha(Colors[Random(22)], 128));

  FLayerBitmap := TBitmapLayer(ImgViewPage3.Layers.Add(TBitmapLayer));
  FLayerBitmap.Bitmap.DrawMode := dmBlend;

  RgpBlurType.ItemIndex := 1;

{$ifndef FPC}
  PnlControl.Padding.Left := 8;
  PnlControl.Padding.Right := 8;
  PnlControl.Padding.Top := 8;
  PnlControl.Padding.Bottom := 8;
  PanelSelective.Padding.Top := 8;
  PanelMotion.Padding.Top := 8;
  PanelRadius.Padding.Top := 8;
{$else}
  PnlControl.BorderSpacing.Around := 8;
  PanelSelective.BorderSpacing.Top := 8;
  PanelMotion.BorderSpacing.Top := 8;
  PanelRadius.BorderSpacing.Top := 8;
{$endif}
end;

procedure TFrmBlurs.FormDestroy(Sender: TObject);
begin
  FBitmapStoneWeed.Free;
  FBitmapIceland.Free;
  FBitmapRandBox.Free;
end;

procedure TFrmBlurs.Redraw;
var
  Radius: Integer;
  Rec, Rec2: TRect;
  Pts, Pts2: TArrayOfFloatPoint;
  WithGamma: Boolean;
  Stopwatch: TStopwatch;
begin
  if FRedrawFlag then
    Exit;

  FRedrawFlag := True;
  try

    Screen.Cursor := crHourGlass;

    Radius := TbrBlurRadius.Position;
    WithGamma := CheckBoxCorrectGamma.Checked;
    case PageControl.ActivePageIndex of
      0:
        begin
          ImgViewPage1.BeginUpdate;
          try
            ImgViewPage1.Bitmap.Assign(FBitmapIceland);

            Stopwatch := TStopwatch.StartNew;
            case RgpBlurType.ItemIndex of
              1:
                GaussianBlurSimple[WithGamma](ImgViewPage1.Bitmap, Radius);

              2:
                if WithGamma then
                  MotionBlurGamma(ImgViewPage1.Bitmap, Radius, TbrBlurAngle.Position, CbxBidirectional.Checked)
                else
                  MotionBlur(ImgViewPage1.Bitmap, Radius, TbrBlurAngle.Position, CbxBidirectional.Checked);

              3:
                if WithGamma then
                  GammaSelectiveGaussianBlur32(FBitmapIceland, ImgViewPage1.Bitmap, Radius, TrackBarDelta.Position)
                else
                  SelectiveGaussianBlur32(FBitmapIceland, ImgViewPage1.Bitmap, Radius, TrackBarDelta.Position);

            end;
            Stopwatch.Stop;
          finally
            ImgViewPage1.EndUpdate;
          end;
        end;

      1:
        begin
          ImgViewPage2.BeginUpdate;
          try
            ImgViewPage2.Bitmap.Assign(FBitmapStoneWeed);

            Pts := Star(130, 150, 90, 5, -0.5 * Pi);
            Pts2 := Ellipse(350, 250, 100, 60);

            Stopwatch := TStopwatch.StartNew;
            case RgpBlurType.ItemIndex of
              1:
                begin
                  GaussianBlurRegion[WithGamma](ImgViewPage2.Bitmap, Radius, Pts);
                  GaussianBlurRegion[WithGamma](ImgViewPage2.Bitmap, Radius, Pts2);
                end;

              2:
                if WithGamma then
                begin
                  MotionBlurGamma(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
                  MotionBlurGamma(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position, Pts2, CbxBidirectional.Checked);
                end
                else
                begin
                  MotionBlur(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
                  MotionBlur(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position, Pts2, CbxBidirectional.Checked);
                end;
            end;
            Stopwatch.Stop;

            PolylineFS(ImgViewPage2.Bitmap, Pts, clBlack32, True, 2.5);
            PolylineFS(ImgViewPage2.Bitmap, Pts2, clBlack32, True, 2.5);
          finally
            ImgViewPage2.EndUpdate;
          end;
        end;

      2:
        begin
          ImgViewPage3.BeginUpdate;
          try
            ImgViewPage3.SetupBitmap(True, Color32(clBtnFace));

            Rec := ImgViewPage3.GetBitmapRect;
            FLayerBitmap.Location := FloatRect(Rec);
            FLayerBitmap.Bitmap.SetSize(Rec.Width, Rec.Height);
            FLayerBitmap.Bitmap.Clear(0);

            // Colored squares on layer
            FLayerBitmap.Bitmap.Draw(300, 40, FBitmapRandBox);

            // Beveled box on background image
            Rec := Rect(40, 40, 240, 120);
            DrawFramedBox(ImgViewPage3.Bitmap, Rec, clWhite32, clGray32, Radius div 2);

            // Red rectangle on layer
            Rec2 := Rect(40, 160, 240, 320);
            FLayerBitmap.Bitmap.FillRectTS(Rec2, clRed32);

            GR32.InflateRect(Rec2, 20, 20);

            // Ellipse on top of colored squares
            Pts := Ellipse(395, 175, 60, 100);

            Stopwatch := TStopwatch.StartNew;
            case RgpBlurType.ItemIndex of
              1:
                begin
                  GaussianBlurBounds[WithGamma](ImgViewPage3.Bitmap, Radius, Rec);
                  GaussianBlurBounds[WithGamma](FLayerBitmap.Bitmap, Radius, Rec2);
                  GaussianBlurRegion[WithGamma](FLayerBitmap.Bitmap, Radius, Pts);
                end;

              2:
                if WithGamma then
                begin
                  MotionBlurGamma(ImgViewPage3.Bitmap, Radius, TbrBlurAngle.Position, Rec, CbxBidirectional.Checked);
                  MotionBlurGamma(FLayerBitmap.Bitmap, Radius, TbrBlurAngle.Position, Rec2, CbxBidirectional.Checked);
                  MotionBlurGamma(FLayerBitmap.Bitmap, Radius, TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
                end
                else
                begin
                  MotionBlur(ImgViewPage3.Bitmap, Radius, TbrBlurAngle.Position, Rec, CbxBidirectional.Checked);
                  MotionBlur(FLayerBitmap.Bitmap, Radius, TbrBlurAngle.Position, Rec2, CbxBidirectional.Checked);
                  MotionBlur(FLayerBitmap.Bitmap, Radius, TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
                end;
            end;
            Stopwatch.Stop;

            // Outline ellipse
            PolylineFS(FLayerBitmap.Bitmap, Pts, clTrBlack32, True, 2.5);

            // Outline red rectangle
            GR32.InflateRect(Rec2, 1, 1);
            PolylineFS(
              FLayerBitmap.Bitmap,
              Rectangle(Rec2),
              clBlack32,
              True,
              0.5);

          finally
            ImgViewPage3.EndUpdate;
          end;
        end;
    end;
    SbrMain.SimpleText := Format('  Blur drawing time: %d ms', [Stopwatch.ElapsedMilliseconds]);

  finally
    FRedrawFlag := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmBlurs.MnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmBlurs.RgpBlurTypeClick(Sender: TObject);

  procedure EnableGroup(Parent: TControl; State: boolean);
  var
    i: integer;
  begin
    Parent.Enabled := State;
    if (Parent is TWinControl) then
      for i := 0 to TWinControl(Parent).ControlCount-1 do
        EnableGroup(TWinControl(Parent).Controls[i], State);
  end;

begin
  MnuNone.Checked := (RgpBlurType.ItemIndex = 0);
  MnuGaussianType.Checked := (RgpBlurType.ItemIndex = 1);
  MnuMotion.Checked := (RgpBlurType.ItemIndex = 2);
  MnuSelective.Checked := (RgpBlurType.ItemIndex = 3);

  EnableGroup(PanelRadius, (RgpBlurType.ItemIndex <> 0));
  EnableGroup(PanelMotion, (RgpBlurType.ItemIndex = 2));
  EnableGroup(PanelSelective, (RgpBlurType.ItemIndex = 3));

  case RgpBlurType.ItemIndex of
    1: // The current Gaussian Blur begins introducing overflow artifacts at around radius=200
      TbrBlurRadius.Max := 200;
    2: // Motion blur internally limits the radius to 256
      TbrBlurRadius.Max := 256;
    3: // Selective blur is very slow, so limit the damage
      TbrBlurRadius.Max := 20;
  end;

  Redraw;
end;

procedure TFrmBlurs.TimerUpdateTimer(Sender: TObject);
begin
  TimerUpdate.Enabled := False;
  Redraw;
end;

procedure TFrmBlurs.TbrBlurRadiusChange(Sender: TObject);
begin
  LblBlurRadius.Caption := Format('Blur &Radius (%d)', [TbrBlurRadius.Position]);

  QueueUpdate;
end;

procedure TFrmBlurs.TrackBarDeltaChange(Sender: TObject);
begin
  LabelDelta.Caption := Format('Delta (%d)', [TrackBarDelta.Position]);

  QueueUpdate;
end;

procedure TFrmBlurs.TbrBlurAngleChange(Sender: TObject);
begin
  LblBlurAngle.Caption := Format('Blur &Angle (%d)', [TbrBlurAngle.Position]);

  QueueUpdate;
end;

procedure TFrmBlurs.MnuGaussianTypeClick(Sender: TObject);
begin
  if Sender = MnuGaussianType then
    RgpBlurType.ItemIndex := 1
  else
  if Sender = MnuMotion then
    RgpBlurType.ItemIndex := 2
  else
  if Sender = MnuSelective then
    RgpBlurType.ItemIndex := 3
  else
    RgpBlurType.ItemIndex := 0
end;

procedure TFrmBlurs.MnuOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FBitmapIceland.LoadFromFile(OpenDialog.FileName);
    PageControl.ActivePageIndex := 0;
    Redraw;
  end;
end;

procedure TFrmBlurs.PageControlChange(Sender: TObject);
begin
  Redraw;
end;

procedure TFrmBlurs.QueueUpdate;
begin
  TimerUpdate.Enabled := False;
  TimerUpdate.Enabled := True;
end;

end.
