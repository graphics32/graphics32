unit MainUnit;

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
    MnuFastGaussian: TMenuItem;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MnuExitClick(Sender: TObject);
    procedure MnuGaussianTypeClick(Sender: TObject);
    procedure MnuOpenClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure RgpBlurTypeClick(Sender: TObject);
    procedure TbrBlurAngleChange(Sender: TObject);
    procedure TbrBlurRadiusChange(Sender: TObject);
  private
    FBitmapStoneWeed: TBitmap32;
    FBitmapIceland: TBitmap32;
    FBitmapRandBox: TBitmap32;
    FLayerBitmap: TBitmapLayer;

    FRedrawFlag: Boolean;

    procedure Redraw;
  end;

var
  FrmBlurs: TFrmBlurs;

implementation

uses
  Types,
  GR32.ImageFormats.JPG,
  GR32_Polygons,
  GR32_VectorUtils,
  GR32_System,
  GR32_Blurs;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

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
  FBitmapIceland := TBitmap32.create;

  // Just use FBitmapStoneWeed momentarily to load a 600*400 image of ICELAND ...
  FBitmapStoneWeed.LoadFromResourceName(hInstance, 'ICELAND', RT_RCDATA);
  FBitmapIceland.SetSize(600, 400);
  FBitmapStoneWeed.DrawTo(FBitmapIceland, FBitmapIceland.BoundsRect, FBitmapStoneWeed.BoundsRect);

  // Now load the real STONEWEED image ...
  FBitmapStoneWeed.LoadFromResourceName(hInstance, 'STONEWEED', RT_RCDATA);

  Randomize;
  FBitmapRandBox := TBitmap32.create;
  // Generate an image of full of random colored boxes ...
  FBitmapRandBox.SetSize(192, 272);
  for X := 0 to 11 do
    for Y := 0 to 16 do
      FBitmapRandBox.FillRectS(X * 16, Y * 16, 300 + (X + 1) * 16,
        40 + (Y + 1) * 16, SetAlpha(Colors[Random(22)], 128));

  FLayerBitmap := TBitmapLayer(ImgViewPage3.Layers.Add(TBitmapLayer));
  FLayerBitmap.Bitmap.DrawMode := dmBlend;

  Redraw;
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

  Screen.Cursor := crHourGlass;

  Radius := TbrBlurRadius.Position;
  WithGamma := CheckBoxCorrectGamma.Checked;
  case PageControl.ActivePageIndex of
    0:
      begin
        ImgViewPage1.BeginUpdate;
        ImgViewPage1.Bitmap.Assign(FBitmapIceland);

        Stopwatch := TStopwatch.StartNew;
        case RgpBlurType.ItemIndex of
          1:
            GaussianBlurSimple[WithGamma](ImgViewPage1.Bitmap, Radius);

          2:
            FastBlurSimple[WithGamma](ImgViewPage1.Bitmap, Radius);

          3:
            if WithGamma then
              MotionBlurGamma(ImgViewPage1.Bitmap, Radius, TbrBlurAngle.Position, CbxBidirectional.Checked)
            else
              MotionBlur(ImgViewPage1.Bitmap, Radius, TbrBlurAngle.Position, CbxBidirectional.Checked)
        end;
        Stopwatch.Stop;
        ImgViewPage1.EndUpdate;
      end;

    1:
      begin
        ImgViewPage2.BeginUpdate;
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
            begin
              FastBlurRegion[WithGamma](ImgViewPage2.Bitmap, Radius, Pts);
              FastBlurRegion[WithGamma](ImgViewPage2.Bitmap, Radius, Pts2);
            end;

          3:
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
        ImgViewPage2.EndUpdate;
      end;

    2:
      begin
        ImgViewPage3.BeginUpdate;
        ImgViewPage3.SetupBitmap(True, Color32(clBtnFace));
        FLayerBitmap.Bitmap.Clear(0);

        Rec := ImgViewPage3.GetBitmapRect;
        FLayerBitmap.Location := FloatRect(Rec);
        FLayerBitmap.Bitmap.SetSize(Rec.Width, Rec.Height);
        FLayerBitmap.Bitmap.Draw(300, 40, FBitmapRandBox);

        Rec := Rect(40, 40, 240, 120);
        DrawFramedBox(ImgViewPage3.Bitmap, Rec, clWhite32, clGray32, Radius div 2);

        Rec2 := Rect(40, 160, 240, 320);
        FLayerBitmap.Bitmap.FillRect(Rec2.Left, Rec2.Top, Rec2.Right, Rec2.Bottom, clRed32);
        GR32.InflateRect(Rec2, 20, 20);

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
            begin
              FastBlurBounds[WithGamma](ImgViewPage3.Bitmap, Radius, Rec);
              FastBlurBounds[WithGamma](FLayerBitmap.Bitmap, Radius, Rec2);
              FastBlurRegion[WithGamma](FLayerBitmap.Bitmap, Radius, Pts);
            end;

          3:
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

        PolylineFS(FLayerBitmap.Bitmap, Pts, clBlack32, True, 2.5);

        PolylineFS(
          FLayerBitmap.Bitmap,
          BuildPolygonF([Rec2.Left, Rec2.Top, Rec2.Right, Rec2.Top, Rec2.Right, Rec2.Bottom, Rec2.Left, Rec2.Bottom]),
          clBlack32,
          True,
          0.5);

        ImgViewPage3.EndUpdate;
      end;
  end;
  SbrMain.SimpleText := Format('  Blur drawing time: %d ms', [Stopwatch.ElapsedMilliseconds]);
  Screen.Cursor := crDefault;
  FRedrawFlag := False;
end;

procedure TFrmBlurs.MnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmBlurs.RgpBlurTypeClick(Sender: TObject);
begin
  MnuNone.Checked := RgpBlurType.ItemIndex = 0;
  MnuGaussianType.Checked := RgpBlurType.ItemIndex = 1;
  MnuFastGaussian.Checked := RgpBlurType.ItemIndex = 2;
  MnuMotion.Checked := RgpBlurType.ItemIndex = 3;
  LblBlurAngle.Enabled := MnuMotion.Checked;
  TbrBlurAngle.Enabled := MnuMotion.Checked;
  CbxBidirectional.Enabled := MnuMotion.Checked;
  Redraw;
end;

procedure TFrmBlurs.TbrBlurRadiusChange(Sender: TObject);
begin
  LblBlurRadius.Caption := Format('Blur &Radius (%d)', [TbrBlurRadius.Position]);
  Redraw;
end;

procedure TFrmBlurs.TbrBlurAngleChange(Sender: TObject);
begin
  LblBlurAngle.Caption := Format('Blur &Angle (%d)', [TbrBlurAngle.Position]);
  Redraw;
end;

procedure TFrmBlurs.MnuGaussianTypeClick(Sender: TObject);
begin
  if Sender = MnuNone then
    RgpBlurType.ItemIndex := 0
  else
  if Sender = MnuGaussianType then
    RgpBlurType.ItemIndex := 1
  else
  if Sender = MnuFastGaussian then
    RgpBlurType.ItemIndex := 2
  else
    RgpBlurType.ItemIndex := 3;
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

end.
