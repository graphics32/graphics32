unit Main;

{$I GR32.inc}

interface

uses
  Forms, Classes, Controls,
  GR32,
  GR32_Image;

type
  TFormMain = class(TForm)
    ImgView: TImgView32;
    procedure FormCreate(Sender: TObject);
  private
  protected
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Graphics,
  GR32.Examples,
  GR32.ImageFormats.PNG32;

{ TFormMain }

{.$define SOLID_DROPSHADOW}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\coffee.png');

  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Background.CheckersExponent := 3; // Size of each tile becomes 2^3 = 8 pixels
  ImgView.Background.PatternBitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\bumps.bmp');
  ImgView.Background.OuterBorderColor := clGray;
  ImgView.Background.InnerBorderWidth := 8;
  ImgView.Background.InnerBorderColor := clWhite;
  ImgView.Background.DropShadowOffset := 6;
{$ifdef SOLID_DROPSHADOW}
  ImgView.Background.DropShadowSize := 4;
  ImgView.Background.DropShadowColor := $20000000;
{$else SOLID_DROPSHADOW}
  ImgView.Background.DropShadowBitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\dropshadow.bmp');
  ImgView.Background.DropShadowBitmap.MasterAlpha := 128;
{$endif SOLID_DROPSHADOW}
end;

end.
