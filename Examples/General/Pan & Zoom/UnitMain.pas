unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32_Image,
  GR32_Layers;

const
  MSG_INTRO = WM_USER;

type
  TFormMain = class(TForm)
    PanelLeft: TPanel;
    Label1: TLabel;
    Image: TImage32;
    PanelRight: TPanel;
    Label2: TLabel;
    ImgView: TImgView32;
    LabelImgViewZoom: TLabel;
    LabelImageZoom: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageChange(Sender: TObject);
    procedure ImgViewChange(Sender: TObject);
  private
    procedure MsgIntro(var Msg: TMessage); message MSG_INTRO;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  GR32;

const
  sMediaFolder = '..\..\..\..\Media';

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  Image.Background.CheckersStyle := bcsMedium;

  ImgView.Bitmap.DrawMode := dmBlend;
  Image.Bitmap.DrawMode := dmBlend;

  ImgView.Bitmap.LoadFromFile(sMediaFolder+'\coffee.png');
  Image.Bitmap.LoadFromFile(sMediaFolder+'\coffee.png');

  ImgView.MousePan.Enabled := True;
  Image.MousePan.Enabled := True;

  ImgView.MouseZoom.Enabled := True;
  Image.MouseZoom.Enabled := True;
  ImgView.MouseZoom.Animate := True;
  Image.MouseZoom.Animate := True;

  // MouseWheel events aren't sent to the control unless TabStop=True
  ImgView.TabStop := True;
  Image.TabStop := True;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle, MSG_INTRO, 0, 0);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  PanelLeft.Width := ClientWidth div 2;
end;

procedure TFormMain.ImageChange(Sender: TObject);
begin
  LabelImageZoom.Caption := Format('Zoom: %.4n', [Image.Scale]);
end;

procedure TFormMain.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Size: TSize;
begin
  if (Button = mbMiddle) then
  begin
    TImage32(Sender).BeginUpdate;
    try
      // Reset Zoom...
      TImage32(Sender).Scale := 1;

      // ...and Center image
      Size := TImage32(Sender).GetBitmapSize;
      TImage32(Sender).OffsetHorz := (TImage32(Sender).Width-Size.cx) div 2;
      TImage32(Sender).OffsetVert := (TImage32(Sender).Height-Size.cy) div 2;
    finally
      TImage32(Sender).EndUpdate;
    end;
    TImage32(Sender).Changed;
  end;
end;

procedure TFormMain.ImgViewChange(Sender: TObject);
begin
  LabelImgViewZoom.Caption := Format('Zoom: %.4n', [ImgView.Scale]);
end;

procedure TFormMain.ImgViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Button = mbMiddle) then
  begin
    TImgView32(Sender).BeginUpdate;
    try
      // Reset Zoom...
      TImgView32(Sender).Scale := 1;

      // ...and Center image
      TImgView32(Sender).ScrollToCenter(TImgView32(Sender).Bitmap.Width div 2, TImgView32(Sender).Bitmap.Height div 2);
    finally
      TImgView32(Sender).EndUpdate;
    end;
    TImgView32(Sender).Changed;
  end;
end;

procedure TFormMain.MsgIntro(var Msg: TMessage);
begin
  ShowMessage('Use left mouse button to pan, mouse-wheel to zoom and middle button to center and reset scale.');
end;

end.
