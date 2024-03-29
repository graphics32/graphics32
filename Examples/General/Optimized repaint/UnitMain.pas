unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32,
  GR32_Layers,
  GR32_Image, Vcl.ComCtrls;

const
  MSG_CLEAR = WM_USER;

type
  TFormMain = class(TForm)
    PaintBox32: TPaintBox32;
    ImgView32: TImgView32;
    PanelTop: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Image32: TImage32;
    Panel4: TPanel;
    Label5: TLabel;
    ImgView32Layers: TImgView32;
    MemoHelp: TMemo;
    Panel5: TPanel;
    Panel6: TPanel;
    ButtonClear: TButton;
    RadioGroupRepaint: TRadioGroup;
    Splitter1: TSplitter;
    ButtonDraw: TButton;
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ButtonClearClick(Sender: TObject);
    procedure RadioGroupRepaintClick(Sender: TObject);
    procedure ButtonDrawClick(Sender: TObject);
  private
    FLayer: TBitmapLayer;
    procedure ClearBackBuffers;
    procedure ClearBackBuffer(Buffer: TCustomBitmap32);
    procedure DrawStuff(Buffer: TCustomBitmap32);
    procedure MsgClear(var Msg: TMessage); message MSG_CLEAR;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

const
  ColorNotRepainted: TColor32 = clBlack32;
  ColorRepainted: TColor32 = clGreen32;
  ColorDraw: TColor32 = clRed32;

const
  sHelp = 'This example illustrates how repaint optimization works.'+#13+
    'Each control has had its backbuffer cleared to black and has then been allowed to repaint itself. The backbuffer has then been cleared to green with repaint disabled.'+#13+
    'When an area of the control is now repainted, for example by drawing something on it, the green color will reveal itself where the control is repainted.'+#13+
    'Tip: If you compile this with UPDATERECT_DEBUGDRAW defined, the Windows update rects are made visible during repaint.';

procedure TFormMain.ClearBackBuffer(Buffer: TCustomBitmap32);
begin
  // What we see:
  Buffer.Clear(ColorNotRepainted);
  Update;

  // What's really there (and not visible until we repaint something):
  Buffer.BeginLockUpdate;
  try
    Buffer.Clear(ColorRepainted);
  finally
    Buffer.EndLockUpdate;
  end;
end;

procedure TFormMain.ClearBackBuffers;
begin
  ClearBackBuffer(PaintBox32.Buffer);
  ClearBackBuffer(Image32.Bitmap);
  ClearBackBuffer(ImgView32.Bitmap);
  ClearBackBuffer(FLayer.Bitmap);
end;

procedure TFormMain.DrawStuff(Buffer: TCustomBitmap32);
begin
  // Single pixel
  Buffer.PixelS[150, 50] := ColorDraw;
  Buffer.Changed(MakeRect(150, 50, 151, 51));

  // Diagonal lines
  Buffer.MoveTo(10, 10);
  Buffer.LineToAS(110, 110);
  Buffer.MoveTo(110, 10);
  Buffer.LineToAS(10, 110);

  // Overlapping rects
  Buffer.FillRectS(10, 120, 40, 150, ColorDraw);
  Buffer.FillRectS(30, 140, 60, 170, ColorDraw);

  // Single rect
  Buffer.FrameRectS(100, 140, 150, 200, ColorDraw);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView32.Bitmap.PenColor := ColorDraw;
  Image32.Bitmap.PenColor := ColorDraw;
  PaintBox32.Buffer.PenColor := ColorDraw;

  FLayer := TBitmapLayer(ImgView32Layers.Layers.Add(TBitmapLayer));
  FLayer.Bitmap.PenColor := ColorDraw;
  FLayer.OnMouseDown := PaintBox32MouseDown;

  MemoHelp.Lines.Text := sHelp;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  Panel1.Width := ClientWidth div 4;
  Panel3.Width := Panel1.Width;
  Panel4.Width := Panel1.Width;

  Image32.Bitmap.SetSize(Image32.ClientWidth, Image32.ClientHeight);
  ImgView32.Bitmap.SetSize(ImgView32.ClientWidth, ImgView32.ClientHeight);
  FLayer.Bitmap.SetSize(ImgView32Layers.ClientWidth, ImgView32Layers.ClientHeight);
  FLayer.Location := FloatRect(FLayer.Bitmap.BoundsRect);

  ClearBackBuffers;
end;

procedure TFormMain.MsgClear(var Msg: TMessage);
begin
  ClearBackBuffer(TCustomBitmap32(Msg.WParam));
end;

procedure TFormMain.ButtonClearClick(Sender: TObject);
begin
  ClearBackBuffers;
end;

procedure TFormMain.ButtonDrawClick(Sender: TObject);
begin
  DrawStuff(PaintBox32.Buffer);
  DrawStuff(Image32.Bitmap);
  DrawStuff(ImgView32.Bitmap);
  DrawStuff(FLayer.Bitmap);
end;

procedure TFormMain.Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Buffer: TCustomBitmap32;
  p: TPoint;
begin
  if (Layer <> nil) then
    Buffer := TBitmapLayer(Layer).Bitmap
  else
  if (Sender is TCustomImage32) then
    Buffer := TCustomImage32(Sender).Bitmap
  else
  if (Sender is TCustomPaintBox32) then
    Buffer := TCustomPaintBox32(Sender).Buffer
  else
    exit;

  if (Button = mbLeft) then
  begin
    p := Point(X, Y);
    if (Sender is TCustomImage32) then
      p := TCustomImage32(Sender).ControlToBitmap(p);

    Buffer.MoveTo(p.X, p.Y);
  end else
  if (Button = mbRight) then
    DrawStuff(Buffer)
  else
  if (Button = mbMiddle) then
  begin
    // Because TCustomPaintBox32 by default batches paint updates inside the mouse event handlers
    // we need to defer the clear.
    PostMessage(Handle, MSG_CLEAR, WPARAM(Buffer), 0);
  end;
end;

procedure TFormMain.ImgView32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Buffer: TCustomBitmap32;
  p: TPoint;
begin
  if (not (ssLeft in Shift)) then
    exit;

  if (Layer <> nil) then
    Buffer := TBitmapLayer(Layer).Bitmap
  else
  if (Sender is TCustomImage32) then
    Buffer := TCustomImage32(Sender).Bitmap
  else
  if (Sender is TCustomPaintBox32) then
    Buffer := TCustomPaintBox32(Sender).Buffer
  else
    exit;

  p := Point(X, Y);
  if (Sender is TCustomImage32) then
    p := TCustomImage32(Sender).ControlToBitmap(p);

  Buffer.LineToAS(p.X, p.Y);
end;

procedure TFormMain.PaintBox32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Image32MouseDown(Sender, Button, Shift, X, Y, nil);
end;

procedure TFormMain.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  ImgView32MouseMove(Sender, Shift, X, Y, nil);
end;

procedure TFormMain.PaintBox32PaintBuffer(Sender: TObject);
begin
  TPaintBox32(Sender).Buffer.BeginLockUpdate;
  try
    TPaintBox32(Sender).Buffer.Clear(ColorNotRepainted);
  finally
    TPaintBox32(Sender).Buffer.EndLockUpdate;
  end;
end;

procedure TFormMain.RadioGroupRepaintClick(Sender: TObject);
var
  RepaintMode: TRepaintMode;
begin
  RepaintMode := TRepaintMode(RadioGroupRepaint.ItemIndex);

  PaintBox32.RepaintMode := RepaintMode;
  Image32.RepaintMode := RepaintMode;
  ImgView32.RepaintMode := RepaintMode;
  ImgView32Layers.RepaintMode := RepaintMode;

  ClearBackBuffers;
end;

end.
