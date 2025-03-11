unit UnitMain;

interface

{$include GR32.inc}

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
{$if defined(UseInlining)}
  Types,
{$ifend}
  GR32.Examples,
  GR32,
  GR32_PNG,
  GR32_PortableNetworkGraphic; // Required for inline expansion

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  Image.Background.CheckersStyle := bcsMedium;

  ImgView.Bitmap.DrawMode := dmBlend;
  Image.Bitmap.DrawMode := dmBlend;

  ImgView.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\coffee.png');
  Image.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\coffee.png');

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
  Pivot: TPoint;
begin
  if (Button = mbMiddle) then
  begin
    TImage32(Sender).BeginUpdate;
    try
      // Reset Zoom...
      TImage32(Sender).Scale := 1;

      // ...and Center image
      TImage32(Sender).ScrollToCenter;
    finally
      TImage32(Sender).EndUpdate;
    end;
  end else
  if (Button = mbRight) then
  begin
    // Right-click centers pixel under cursor
    Pivot := GR32.Point(X, Y);
    Pivot := TImage32(Sender).ControlToBitmap(Pivot);
    TImage32(Sender).ScrollToCenter(Pivot.X, Pivot.Y);
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
      TImgView32(Sender).ScrollToCenter;
    finally
      TImgView32(Sender).EndUpdate;
    end;
  end;
end;

procedure TFormMain.MsgIntro(var Msg: TMessage);
begin
  ShowMessage('Use left mouse button to pan, mouse-wheel to zoom and middle button to center and reset scale.');
end;

end.
