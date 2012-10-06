unit MainUnit;

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, Windows,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Math, GR32, GR32_Image, GR32_Layers;

type
{$IFDEF FPC}
  TLargeInteger = Int64;
{$ENDIF}

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
    ReDrawing: boolean;

    QPF: TLargeInteger;
    QPCounter1, QPCounter2: TLargeInteger;

    BalloonImage: TBitmap32;
    IcelandImage: TBitmap32;
    RandBoxImage: TBitmap32;
    BmpLayer: TBitmapLayer;
    procedure ReDraw;
  public
    { Public declarations }
  end;

var
  FrmBlurs: TFrmBlurs;

implementation

uses
  {$IFNDEF FPC} JPEG, {$ENDIF} GR32_Polygons, GR32_VectorUtils, GR32_Blurs,
  GR32_Png, GR32_System;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$R images.res}

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

procedure LoadResourceImage(const ResName: string; Bmp32: TBitmap32);
var
  Png: TPortableNetworkGraphic32;
  Rs: TResourceStream;
begin
  Png := TPortableNetworkGraphic32.Create;
  Rs := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    Png.LoadFromStream(Rs);
    Bmp32.Assign(Png);
  finally
    Png.Free;
    Rs.Free;
  end;
end;

procedure LoadPNGFileImage(const Filename: string; Bmp32: TBitmap32);
var
  Png: TPortableNetworkGraphic32;
begin
  Png := TPortableNetworkGraphic32.Create;
  try
    Png.LoadFromFile(Filename);
    Bmp32.Assign(Png);
  finally
    Png.Free;
  end;
end;

{ TMainForm methods }

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
  BalloonImage := TBitmap32.create;
  LoadResourceImage('BALLOONS', BalloonImage);

  IcelandImage := TBitmap32.create;
  LoadResourceImage('ICELAND', IcelandImage);

  Randomize;
  RandBoxImage := TBitmap32.create;
  //generate an image of full of random boxes ...
  RandBoxImage.SetSize(192, 272);
  for I := 0 to 11 do
    for J := 0 to 16 do
      RandBoxImage.FillRectS(I * 16,
        J * 16, 300 + (I + 1) * 16, 40 + (J +1) * 16,
        SetAlpha(Colors[Random(22)], 128));

  BmpLayer := TBitmapLayer(ImgViewPage3.Layers.Add(TBitmapLayer));
  BmpLayer.Bitmap.DrawMode := dmBlend;

  QueryPerformanceFrequency(QPF);
  ReDraw;
end;

procedure TFrmBlurs.FormDestroy(Sender: TObject);
begin
  BalloonImage.Free;
  IcelandImage.Free;
  RandBoxImage.Free;
end;

procedure TFrmBlurs.ReDraw;
var
  Radius: Integer;
  Rec, Rec2: TRect;
  Pts, Pts2: TArrayOfFloatPoint;
begin
  if ReDrawing then
    Exit;
  ReDrawing := true;
  Radius := TbrBlurRadius.Position;
  Screen.Cursor := crHourGlass;
  case PageControl.ActivePageIndex of
    0:
    begin
      ImgViewPage1.BeginUpdate;
      ImgViewPage1.Bitmap.Assign(BalloonImage);
      QueryPerformanceCounter(QPCounter1);
      case RgpBlurType.ItemIndex of
        1: GaussianBlur(ImgViewPage1.Bitmap, Radius);
        2: FastBlur(ImgViewPage1.Bitmap, Radius);
        3: MotionBlur(ImgViewPage1.Bitmap, Radius,
             TbrBlurAngle.Position, CbxBidirectional.Checked);
      end;
      QueryPerformanceCounter(QPCounter2);
      ImgViewPage1.EndUpdate;
      ImgViewPage1.Repaint;
      Application.ProcessMessages;
    end;
    1:
    begin
      ImgViewPage2.BeginUpdate;
      ImgViewPage2.Bitmap.Assign(IcelandImage);
      //5 pointed star ...
      Pts := BuildPolygon([10, 40, 40, 40, 50, 10, 60, 40, 90, 40, 65, 60, 75, 90, 50, 70, 25, 90, 35, 60]);
      Pts := ScalePolygon(Pts, 2, 2);
      Pts := TranslatePolygon(Pts, 30, 50);

      Pts2 := Ellipse(350, 250, 100, 60);

      QueryPerformanceCounter(QPCounter1);
      case RgpBlurType.ItemIndex of
        1:
          begin
            GaussianBlur(ImgViewPage2.Bitmap, Radius, Pts);
            GaussianBlur(ImgViewPage2.Bitmap, Radius, Pts2);
          end;
        2:
          begin
            FastBlur(ImgViewPage2.Bitmap, Radius, Pts);
            FastBlur(ImgViewPage2.Bitmap, Radius, Pts2);
          end;
        3:
          begin
            MotionBlur(ImgViewPage2.Bitmap, Radius,
              TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
            MotionBlur(ImgViewPage2.Bitmap, Radius,
              TbrBlurAngle.Position, Pts2, CbxBidirectional.Checked);
          end;
      end;
      QueryPerformanceCounter(QPCounter2);
      Application.ProcessMessages;

      PolylineFS(ImgViewPage2.Bitmap, Pts, clBlack32, true, 2.5);
      PolylineFS(ImgViewPage2.Bitmap, Pts2, clBlack32, true, 2.5);
      ImgViewPage2.EndUpdate;
      ImgViewPage2.Repaint;
    end;
    2:
    begin
      ImgViewPage3.BeginUpdate;
      ImgViewPage3.SetupBitmap(true, Color32(clBtnFace));
      BmpLayer.Bitmap.Clear(0);

      with ImgViewPage3.GetBitmapRect do
      begin
        BmpLayer.Location := FloatRect(Left, Top, Right, Bottom);
        BmpLayer.Bitmap.SetSize(Right - Left, Bottom - Top)
      end;
      BmpLayer.Bitmap.Draw(300, 40, RandBoxImage);

      Rec := Rect(40,40,240,120);
      DrawFramedBox(ImgViewPage3.Bitmap, Rec, clWhite32, clGray32, Radius div 2);

      Rec2 := Rect(40,160,240,320);
      with Rec2 do
        BmpLayer.Bitmap.FillRect(Left, Top, Right, Bottom, clRed32);
      InflateRect(Rec2, 20, 20);

      Pts := Ellipse(395,175, 60, 100);

      QueryPerformanceCounter(QPCounter1);
      case RgpBlurType.ItemIndex of
        1:
          begin
            GaussianBlur(ImgViewPage3.Bitmap, Radius, Rec);
            GaussianBlur(BmpLayer.Bitmap, Radius, Rec2);
            GaussianBlur(BmpLayer.Bitmap, Radius, Pts);
          end;
        2:
          begin
            FastBlur(ImgViewPage3.Bitmap, Radius, Rec);
            FastBlur(BmpLayer.Bitmap, Radius, Rec2);
            FastBlur(BmpLayer.Bitmap, Radius, Pts);
          end;
        3:
          begin
            MotionBlur(ImgViewPage3.Bitmap, Radius,
              TbrBlurAngle.Position, Rec, CbxBidirectional.Checked);
            MotionBlur(BmpLayer.Bitmap, Radius,
              TbrBlurAngle.Position, Rec2, CbxBidirectional.Checked);
            MotionBlur(BmpLayer.Bitmap, Radius,
              TbrBlurAngle.Position, Pts, CbxBidirectional.Checked);
          end;
      end;

      QueryPerformanceCounter(QPCounter2);
      Application.ProcessMessages;

      PolylineFS(BmpLayer.Bitmap, Pts, clBlack32, true, 2.5);

      with Rec2 do
      PolylineFS(BmpLayer.Bitmap,
        BuildPolygon([Left,Top,Right,Top,Right,Bottom,Left,Bottom]),
        clBlack32, true, 1.0);

      ImgViewPage3.EndUpdate;
      ImgViewPage3.Repaint;
    end;
  end;
  SbrMain.SimpleText := Format('  Blur drawing time: %0.2n ms',[
    1000 * (QPCounter2 - QPCounter1) / QPF]);
  Screen.Cursor := crDefault;
  ReDrawing := false;
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
    if Extension = '.png' then
      LoadPNGFileImage(OpenDialog.FileName, BalloonImage) else
      BalloonImage.LoadFromFile(OpenDialog.FileName);
    PageControl.ActivePageIndex := 0;
    ReDraw;
  end;
end;

procedure TFrmBlurs.PageControlChange(Sender: TObject);
begin
  ReDraw;
end;

end.

