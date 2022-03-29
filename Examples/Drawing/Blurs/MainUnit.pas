unit MainUnit;

interface

uses
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Menus, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Math, GR32, GR32_Image, GR32_Layers, GR32_System;

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
    FPerfTimer: TPerfTimer;
    FDuration: string;

    FReDrawFlag: Boolean;

    FStoneWeedImage: TBitmap32;
    FIcelandImage: TBitmap32;
    FRandBoxImage: TBitmap32;
    FBmpLayer: TBitmapLayer;
    procedure ReDraw;
  end;

var
  FrmBlurs: TFrmBlurs;

implementation

uses
  Types,
  {$IFNDEF FPC} JPEG, {$ELSE} LazJPG, {$ENDIF}
  GR32_Polygons, GR32_VectorUtils, GR32_Blurs, GR32_Resamplers;

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
  if LineWidth < 1 then LineWidth := 1;
  SetLength(Pts, 6);
  with Rec do
  begin
    Pts[0] := FloatPoint(Left, Bottom);
    Pts[1] := FloatPoint(Left, Top);
    Pts[2] := FloatPoint(Right, Top);
    Pts[3] := FloatPoint(Right - LineWidth, Top + LineWidth);
    Pts[4] := FloatPoint(Left + LineWidth, Top + LineWidth);
    Pts[5] := FloatPoint(Left + LineWidth, Bottom - LineWidth);
    PolygonFS(Bmp32, Pts, Color1);
    Pts[1] := FloatPoint(Right, Bottom);
    Pts[2] := FloatPoint(Right, Top);
    Pts[3] := FloatPoint(Right - LineWidth, Top + LineWidth);
    Pts[4] := FloatPoint(Right - LineWidth, Bottom - LineWidth);
    Pts[5] := FloatPoint(Left + LineWidth, Bottom - LineWidth);
    PolygonFS(Bmp32, Pts, Color2);
  end;
end;

procedure LoadJPGResource(const ResName: string; Bmp32: TBitmap32);
var
  ResStream: TResourceStream;
  JPEG: TJPEGImage;
begin
  JPEG := TJPEGImage.Create;
  ResStream := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    JPEG.LoadFromStream(ResStream);
    Bmp32.Assign(JPEG);
  finally
    ResStream.Free;
    JPEG.Free;
  end;
end;


{ TFrmBlurs }

procedure TFrmBlurs.FormCreate(Sender: TObject);
var
  I, J: Integer;
const
  Colors: array [0 .. 21] of TColor32 = (clAliceBlue32, clAquamarine32,
    clAzure32, clBeige32, clBlueViolet32, clCadetblue32, clChocolate32,
    clCoral32, clCornFlowerBlue32, clCornSilk32, clCrimson32,
    clDarkBlue32, clDarkCyan32, clDarkGoldenRod32, clDarkGreen32,
    clDarkMagenta32, clDarkOrange32, clDarkOrchid32, clDarkRed32,
    clDarkSalmon32, clDarkSeaGreen32, clDarkSlateBlue32);
begin
  FStoneWeedImage := TBitmap32.create;
  FIcelandImage := TBitmap32.create;

  // Just use FStoneWeedImage momentarily to load a 600*400 image of ICELAND ...
  LoadJPGResource('ICELAND', FStoneWeedImage);
  FIcelandImage.SetSize(600, 400);
  FStoneWeedImage.DrawTo(FIcelandImage, FIcelandImage.BoundsRect,
    FStoneWeedImage.BoundsRect);
  // Now load the real STONEWEED image ...
  LoadJPGResource('STONEWEED', FStoneWeedImage);

  FPerfTimer := TPerfTimer.Create;

  Randomize;
  FRandBoxImage := TBitmap32.create;
  //generate an image of full of random boxes ...
  FRandBoxImage.SetSize(192, 272);
  for I := 0 to 11 do
    for J := 0 to 16 do
      FRandBoxImage.FillRectS(I * 16, J * 16, 300 + (I + 1) * 16,
        40 + (J +1) * 16, SetAlpha(Colors[Random(22)], 128));

  FBmpLayer := TBitmapLayer(ImgViewPage3.Layers.Add(TBitmapLayer));
  FBmpLayer.Bitmap.DrawMode := dmBlend;

  ReDraw;
end;

procedure TFrmBlurs.FormDestroy(Sender: TObject);
begin
  FPerfTimer.Free;
  FStoneWeedImage.Free;
  FIcelandImage.Free;
  FRandBoxImage.Free;
end;

procedure TFrmBlurs.ReDraw;
var
  Radius: Integer;
  Rec, Rec2: TRect;
  Pts, Pts2: TArrayOfFloatPoint;
  WithGamma: Boolean;
begin
  if FReDrawFlag then
    Exit;
  FReDrawFlag := True;
  Radius := TbrBlurRadius.Position;
  Screen.Cursor := crHourGlass;
  WithGamma := CheckBoxCorrectGamma.Checked;
  case PageControl.ActivePageIndex of
    0:
      begin
        ImgViewPage1.BeginUpdate;
        ImgViewPage1.Bitmap.Assign(FIcelandImage);

        FPerfTimer.Start;
        case RgpBlurType.ItemIndex of
          1:
            GaussianBlurSimple[WithGamma](ImgViewPage1.Bitmap, Radius);
          2:
            FastBlurSimple[WithGamma](ImgViewPage1.Bitmap, Radius);
          3:
            if WithGamma then
              MotionBlurGamma(ImgViewPage1.Bitmap, Radius,
               TbrBlurAngle.Position, CbxBidirectional.Checked)
            else
              MotionBlur(ImgViewPage1.Bitmap, Radius,
               TbrBlurAngle.Position, CbxBidirectional.Checked)
        end;
        FDuration := FPerfTimer.ReadMilliseconds;
        ImgViewPage1.EndUpdate;
        ImgViewPage1.Repaint;
        Application.ProcessMessages;
      end;
    1:
      begin
        ImgViewPage2.BeginUpdate;
        ImgViewPage2.Bitmap.Assign(FStoneWeedImage);

        Pts := Star(130, 150, 90, 5, -0.5 * Pi);
        Pts2 := Ellipse(350, 250, 100, 60);

        FPerfTimer.Start;
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
              MotionBlurGamma(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position,
                Pts, CbxBidirectional.Checked);
              MotionBlurGamma(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position,
                Pts2, CbxBidirectional.Checked);
            end
            else
            begin
              MotionBlur(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position,
                Pts, CbxBidirectional.Checked);
              MotionBlur(ImgViewPage2.Bitmap, Radius, TbrBlurAngle.Position,
                Pts2, CbxBidirectional.Checked);
            end;
        end;
        FDuration := FPerfTimer.ReadMilliseconds;
        Application.ProcessMessages;

        PolylineFS(ImgViewPage2.Bitmap, Pts, clBlack32, True, 2.5);
        PolylineFS(ImgViewPage2.Bitmap, Pts2, clBlack32, True, 2.5);
        ImgViewPage2.EndUpdate;
        ImgViewPage2.Repaint;
      end;
    2:
      begin
        ImgViewPage3.BeginUpdate;
        ImgViewPage3.SetupBitmap(True, Color32(clBtnFace));
        FBmpLayer.Bitmap.Clear(0);

        with ImgViewPage3.GetBitmapRect do
        begin
          FBmpLayer.Location := FloatRect(Left, Top, Right, Bottom);
          FBmpLayer.Bitmap.SetSize(Right - Left, Bottom - Top)
        end;
        FBmpLayer.Bitmap.Draw(300, 40, FRandBoxImage);

        Rec := Rect(40, 40, 240, 120);
        DrawFramedBox(ImgViewPage3.Bitmap, Rec, clWhite32, clGray32, Radius div 2);

        Rec2 := Rect(40, 160, 240, 320);
        with Rec2 do
          FBmpLayer.Bitmap.FillRect(Left, Top, Right, Bottom, clRed32);
        GR32.InflateRect(Rec2, 20, 20);

        Pts := Ellipse(395, 175, 60, 100);

        FPerfTimer.Start;
        case RgpBlurType.ItemIndex of
          1:
            begin
              GaussianBlurBounds[WithGamma](ImgViewPage3.Bitmap, Radius, Rec);
              GaussianBlurBounds[WithGamma](FBmpLayer.Bitmap, Radius, Rec2);
              GaussianBlurRegion[WithGamma](FBmpLayer.Bitmap, Radius, Pts);
            end;
          2:
            begin
              FastBlurBounds[WithGamma](ImgViewPage3.Bitmap, Radius, Rec);
              FastBlurBounds[WithGamma](FBmpLayer.Bitmap, Radius, Rec2);
              FastBlurRegion[WithGamma](FBmpLayer.Bitmap, Radius, Pts);
            end;
          3:
            if WithGamma then
            begin
              MotionBlurGamma(ImgViewPage3.Bitmap, Radius,
                TbrBlurAngle.Position, Rec, CbxBidirectional.Checked);
              MotionBlurGamma(FBmpLayer.Bitmap, Radius,
                TbrBlurAngle.Position, Rec2, CbxBidirectional.Checked);
              MotionBlurGamma(FBmpLayer.Bitmap, Radius,
                TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
            end
            else
            begin
              MotionBlur(ImgViewPage3.Bitmap, Radius,
                TbrBlurAngle.Position, Rec, CbxBidirectional.Checked);
              MotionBlur(FBmpLayer.Bitmap, Radius,
                TbrBlurAngle.Position, Rec2, CbxBidirectional.Checked);
              MotionBlur(FBmpLayer.Bitmap, Radius,
                TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
            end;
        end;
        FDuration := FPerfTimer.ReadMilliseconds;
        Application.ProcessMessages;

        PolylineFS(FBmpLayer.Bitmap, Pts, clBlack32, True, 2.5);

        with Rec2 do
          PolylineFS(
            FBmpLayer.Bitmap,
            BuildPolygonF([
              Left, Top, Right, Top, Right, Bottom, Left, Bottom]),
            clBlack32,
            True,
            0.5);

        ImgViewPage3.EndUpdate;
        ImgViewPage3.Repaint;
      end;
  end;
  SbrMain.SimpleText := Format('  Blur drawing time: %s ms', [FDuration]);
  Screen.Cursor := crDefault;
  FReDrawFlag := False;
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
  ReDraw;
end;

procedure TFrmBlurs.TbrBlurRadiusChange(Sender: TObject);
begin
  LblBlurRadius.Caption :=
    Format('Blur &Radius (%d)', [TbrBlurRadius.Position]);
  ReDraw;
end;

procedure TFrmBlurs.TbrBlurAngleChange(Sender: TObject);
begin
  LblBlurAngle.Caption :=
    Format('Blur &Angle (%d)', [TbrBlurAngle.Position]);
  ReDraw;
end;

procedure TFrmBlurs.MnuGaussianTypeClick(Sender: TObject);
begin
  if Sender = MnuNone then
    RgpBlurType.ItemIndex := 0
  else if Sender = MnuGaussianType then
    RgpBlurType.ItemIndex := 1
  else if Sender = MnuFastGaussian then
    RgpBlurType.ItemIndex := 2
  else
    RgpBlurType.ItemIndex := 3;
end;

procedure TFrmBlurs.MnuOpenClick(Sender: TObject);
var
  Extension: String;
begin
  if OpenDialog.Execute then
  begin
    Extension := Lowercase(ExtractFileExt(OpenDialog.FileName));
    FIcelandImage.LoadFromFile(OpenDialog.FileName);
    PageControl.ActivePageIndex := 0;
    ReDraw;
  end;
end;

procedure TFrmBlurs.PageControlChange(Sender: TObject);
begin
  ReDraw;
end;

end.
