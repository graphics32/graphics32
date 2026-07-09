unit Main;

{$include GR32.inc}

interface

uses
  Forms, Classes, Controls,
  Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  GR32,
  GR32_Image;

type
  TFormMain = class(TForm)
    Image32: TImage32;
    Image1: TImage;
    MemoRed: TMemo;
    ImageSprite: TImage32;
    TimerAnimate: TTimer;
    MemoBlue: TMemo;
    Panel1: TPanel;
    CheckBoxSprite: TCheckBox;
    CheckBoxDice: TCheckBox;
    CheckBoxRedText: TCheckBox;
    CheckBoxCoffeeCup: TCheckBox;
    CheckBoxBlueText: TCheckBox;
    Label1: TLabel;
    Shape1: TShape;
    CheckBoxShape: TCheckBox;
    LabelHatchInfo: TLabel;
    PaintBox32: TPaintBox32;
    CheckBoxPaintBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure TimerAnimateTimer(Sender: TObject);
    procedure CheckBoxSpriteClick(Sender: TObject);
    procedure CheckBoxBlueTextClick(Sender: TObject);
    procedure CheckBoxRedTextClick(Sender: TObject);
    procedure CheckBoxCoffeeCupClick(Sender: TObject);
    procedure CheckBoxDiceClick(Sender: TObject);
    procedure CheckBoxShapeClick(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBoxPaintBoxClick(Sender: TObject);
  private
    FSpriteVector: TPoint;
  protected
    procedure UpdateTransparentBackgrounds;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Types,
  Graphics,
  SysUtils,
  GR32_Layers,
  GR32_LowLevel,
  GR32_Paths,
  GR32_Brushes,
  GR32.Text.Types,
  GR32.Examples,
  GR32.ImageFormats.PNG32;

{ TFormMain }

procedure TFormMain.CheckBoxBlueTextClick(Sender: TObject);
begin
  MemoBlue.Visible := TCheckBox(Sender).Checked;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.CheckBoxCoffeeCupClick(Sender: TObject);
begin
  Image32.Visible := TCheckBox(Sender).Checked;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.CheckBoxDiceClick(Sender: TObject);
begin
  Image1.Visible := TCheckBox(Sender).Checked;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.CheckBoxPaintBoxClick(Sender: TObject);
begin
  PaintBox32.Visible := TCheckBox(Sender).Checked;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.CheckBoxRedTextClick(Sender: TObject);
begin
  MemoRed.Visible := TCheckBox(Sender).Checked;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.CheckBoxShapeClick(Sender: TObject);
begin
  Shape1.Visible := TCheckBox(Sender).Checked;
  LabelHatchInfo.Visible := Shape1.Visible;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.CheckBoxSpriteClick(Sender: TObject);
begin
  ImageSprite.Visible := TCheckBox(Sender).Checked;
  TimerAnimate.Enabled := ImageSprite.Visible;
  UpdateTransparentBackgrounds;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Sprite: TBitmapLayer;
begin
  Image1.Picture.LoadFromFile(Graphics32Examples.MediaFolder+'\Dice.png');
  Image32.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\coffee.png');
  Image32.Transparent := True;

  PaintBox32.Transparent := True;

  ImageSprite.Transparent := True;
  Sprite := TBitmapLayer.Create(ImageSprite.Layers);
  Sprite.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\sprite1.bmp');
  Sprite.Bitmap.DrawMode := dmBlend;
  Sprite.Location := FloatRect(0, 0, Sprite.Bitmap.Width, Sprite.Bitmap.Height);

  FSpriteVector.X := 3;
  FSpriteVector.Y := 2;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  PaintBox32.Invalidate;
end;

procedure TFormMain.PaintBox32PaintBuffer(Sender: TObject);
var
  Canvas: TCanvas32;
  SolidBrush: TSolidBrush;
  StrokeBrush: TStrokeBrush;
  r: TFloatRect;
begin
  PaintBox32.Buffer.Font.Height := 40;
  PaintBox32.Buffer.Font.Style := [fsBold];
  Canvas := TCanvas32.Create(PaintBox32.Buffer);
  try
    SolidBrush := TSolidBrush.Create(Canvas.Brushes);
    SolidBrush.FillColor := clTrBlue32;

    StrokeBrush := TStrokeBrush.Create(Canvas.Brushes);
    StrokeBrush.FillColor := clWhitesmoke32;
    StrokeBrush.StrokeWidth := 2;

    r := Canvas.MeasureText(PaintBox32.ClientRect, '88:88:88.888', DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    Canvas.RenderText(r, FormatDateTime('hh:nn:ss.zzz', Now), DT_LEFT or DT_VCENTER or DT_SINGLELINE);
  finally
    Canvas.Free;
  end;
end;

procedure TFormMain.TimerAnimateTimer(Sender: TObject);
begin
  if (ImageSprite.Width = 0) or (ImageSprite.Height = 0) then
    exit;

  var Sprite := TBitmapLayer(ImageSprite.Layers[0]);

  var Origin := Sprite.Location.TopLeft + FSpriteVector;

  if (Origin.X < 0) or (Origin.X > ImageSprite.Width - Sprite.Bitmap.Width) then
  begin
    FSpriteVector.X := -FSpriteVector.X;
    Origin.X := Reflect(Trunc(Origin.X), 0, ImageSprite.Width - Sprite.Bitmap.Width);
  end;

  if (Origin.Y < 0) or (Origin.Y > ImageSprite.Height - Sprite.Bitmap.Height) then
  begin
    FSpriteVector.Y := -FSpriteVector.Y;
    Origin.Y := Reflect(Trunc(Origin.Y), 0, ImageSprite.Height - Sprite.Bitmap.Height);
  end;

  Sprite.Location := FloatRect(Origin.X, Origin.Y, Origin.X+Sprite.Bitmap.Width, Origin.Y+Sprite.Bitmap.Height);
  Image32.Invalidate;
  PaintBox32.Invalidate;
end;

procedure TFormMain.UpdateTransparentBackgrounds;
begin
  Image32.Invalidate;
  ImageSprite.Invalidate;
  PaintBox32.Invalidate;
end;

end.
