unit Main;

{$include GR32.inc}

interface

uses
  Forms, Classes, Controls,
  Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  GR32,
  GR32_Image {, GR32_ImageTransparent};

type
  TFormMain = class(TForm)
    Image32: TImage32;
    Image1: TImage;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Image32Click(Sender: TObject);
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Image1.Picture.LoadFromFile(Graphics32Examples.MediaFolder+'\Dice.png');
  Image32.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\coffee.png');
  Image32.Transparent := True;
end;

procedure TFormMain.Image32Click(Sender: TObject);
begin
  Image32.Invalidate;
end;

end.
