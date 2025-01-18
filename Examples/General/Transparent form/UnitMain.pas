unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32.Paint.Host.API,
  GR32.Paint.Controller.API,
  GR32.Paint.MouseController.API;

//------------------------------------------------------------------------------

type
  // Interposer
  // Redirects WM_PAINT handling to UpdateLayeredWindow
  TImage32 = class(GR32_Image.TImage32)
  protected
    procedure Paint; override;
  end;

//------------------------------------------------------------------------------

type
  TFormMain = class(TForm)
    Image32: TImage32;
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    FPaintMouseController: IBitmap32PaintMouseController;
    FPaintController: IBitmap32PaintController;
    FPaintHost: IBitmap32PaintHost;
    FPaintLayer: TBitmapLayer;
  private
    procedure SetPaintTool(ToolID: integer);
  protected
    procedure CreateWindowHandle(const Context: TCreateParams); override;
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Types,
  GR32_Blend,
  GR32.Paint.Host,
  GR32.Paint.Controller,
  GR32.Paint.MouseController,
  GR32.Paint.Tool.Pen,
  GR32.Paint.Tool.Brush;

//------------------------------------------------------------------------------

constructor TFormMain.Create(AOwner: TComponent);

  procedure SetupPaintTools;
  begin
    FPaintHost := TBitmap32PaintHost.Create(Image32);
    FPaintController := TBitmap32PaintController.Create(Image32, FPaintHost);
    (* This also works fine
    FPaintController := TCustomBitmap32PaintController.Create(FPaintHost);
    *)
    FPaintMouseController := TBitmap32PaintMouseController.Create(FPaintHost, FPaintController);

    FPaintHost.PaintLayer := FPaintLayer;

    FPaintHost.ColorPrimary := clWhite32;
    FPaintHost.ColorSecondary := clBlack32;

    SetPaintTool(1);
  end;

begin
  inherited;

  SetBounds(0, 0, Monitor.Width, Monitor.Height);

  // We never resize the image so ensure that the buffer fits the output area exactly
  Image32.BufferOversize := 0;

  // Load a bitmap so we have something to look at. This is completely optional.
  Image32.Bitmap.LoadFromResourceName(HInstance, 'DICE', 'PNG');

  // Create a bitmap layer we can paint on.
  // We could also just have painted directly on the TImage32.Bitmap
  FPaintLayer := Image32.Layers.Add<TBitmapLayer>;
  FPaintLayer.Location := FloatRect(Image32.BoundsRect);
  FPaintLayer.Bitmap.SetSize(Image32.Width, Image32.Height);
  FPaintLayer.Bitmap.DrawMode := dmBlend;
  // Since we are blending onto a transparent bitmap, and we need that bitmap to
  // stay transparent, we must use the Merge combine mode.
  FPaintLayer.Bitmap.CombineMode := cmMerge;

  // Do not clear the TImage32 background; We need it transparent
  if (Image32.PaintStages[0].Stage = PST_CLEAR_BACKGND) then
    Image32.PaintStages[0].Stage := PST_CUSTOM;

  // Setup the paint tools so we can draw on the screen
  SetupPaintTools;

end;

procedure TFormMain.CreateWindowHandle(const Context: TCreateParams);
var
  ExStyle: NativeInt;
begin
  inherited;

  // An alpha-blended, transparent windows must use the WS_EX_LAYERED
  // windows style.
  // https://learn.microsoft.com/en-us/windows/win32/winmsg/extended-window-styles#WS_EX_LAYERED
  // https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setlayeredwindowattributes

  // CreateWindowHandle removes WS_EX_LAYERED so we can't set it in CreateParams,
  // before the handle is created.
  // Instead we must set it here, after the handle has been created.
  ExStyle := GetWindowLongA(Handle, GWL_EXSTYLE);
  if (ExStyle and WS_EX_LAYERED = 0) then
    SetWindowLong(Handle, GWL_EXSTYLE, ExStyle or WS_EX_LAYERED);
end;

//------------------------------------------------------------------------------

procedure TFormMain.DoShow;
begin
  inherited;

  // Initial paint
  Image32.Paint;
end;

//------------------------------------------------------------------------------

procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '0'..'9': SetPaintTool(Ord(Key) - Ord('0'));
  else
    Close;
  end;
  Key := #0;
end;

//------------------------------------------------------------------------------

procedure TFormMain.SetPaintTool(ToolID: integer);
begin
  case ToolID of
    1:
      begin
        FPaintController.PaintTool := TBitmap32PaintToolCircularBrush.Create(FPaintHost);
        TBitmap32PaintToolCircularBrush(FPaintController.PaintTool).BrushSize := 50;
      end;

    2:
      begin
        FPaintController.PaintTool := TBitmap32PaintToolSmudgeBrush.Create(FPaintHost);
        TBitmap32PaintToolSmudgeBrush(FPaintController.PaintTool).BrushSize := 50;
      end;

    3:
      begin
        FPaintController.PaintTool := TBitmap32PaintToolPen.Create(FPaintHost);
      end;
  else
    FPaintController.PaintTool := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TFormMain.Image32MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FPaintMouseController.HandleMouseDown(Sender, Button, Shift, X, Y, Layer);
end;

procedure TFormMain.Image32MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FPaintMouseController.HandleMouseMove(Sender, Shift, X, Y, Layer);
end;

procedure TFormMain.Image32MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  FPaintMouseController.HandleMouseUp(Sender, Button, Shift, X, Y, Layer);
end;

//------------------------------------------------------------------------------

procedure TFormMain.Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
begin
  // Fully transparent background
  Buffer.Clear(0);
end;


//------------------------------------------------------------------------------
//
//      TImage32 interposer
//
//------------------------------------------------------------------------------
procedure TImage32.Paint;

  procedure PremultiplyBitmap(Bitmap: TBitmap32);
  var
    p: PColor32Entry;
    i: integer;
    PreMult: PLUT8;
  begin
    p := PColor32Entry(Bitmap.Bits);
    for i := 0 to Bitmap.Height*Bitmap.Width-1 do
    begin
      PreMult := @MulDiv255Table[p.A];
      p.R := PreMult[p.R];
      p.G := PreMult[p.G];
      p.B := PreMult[p.B];
      inc(p);
    end;
  end;

  procedure MakeBitmapOpaque(Bitmap: TBitmap32);
  var
    p: PColor32Entry;
    i: integer;
  begin
    p := PColor32Entry(Bitmap.Bits);
    for i := 0 to Bitmap.Height*Bitmap.Width-1 do
    begin
      if (p.A = 0) then
        p.ARGB := $01000000; // Almost transparent, not visuall noticeable
      Inc(p);
    end;
  end;

var
  BlendFunction: TBlendFunction;
  BitmapPos: TPoint;
  BitmapSize: TSize;
  ParentForm: TWinControl;
begin
  // Have TImage32 update the buffer
  DoPaintBuffer;

  // UpdateLayeredWindow needs alpha-premultiple ARGB
  PremultiplyBitmap(Buffer);

  // Make bitmap "not fully transparent" so we don't click through the transparent areas.
  // Disable this to have the form behave as a transparent form.
  MakeBitmapOpaque(Buffer);

  // Find parent form
  ParentForm := Self.Parent;
  while (ParentForm.Parent <> nil) do
    ParentForm := ParentForm.Parent;

  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := 255;
  BlendFunction.AlphaFormat := AC_SRC_ALPHA;

  BitmapPos := GR32.Point(0, 0);
  BitmapSize.cx := Buffer.Width;
  BitmapSize.cy := Buffer.Height;

  if (not UpdateLayeredWindow(ParentForm.Handle, 0, nil, @BitmapSize, Buffer.Canvas.Handle, @BitmapPos, 0, @BlendFunction, ULW_ALPHA)) then
    RaiseLastOSError;
end;

//------------------------------------------------------------------------------

end.
