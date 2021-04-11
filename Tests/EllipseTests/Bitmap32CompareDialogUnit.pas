unit Bitmap32CompareDialogUnit;

{
  This unit provides a dialog that can be used in unit-tests to visually compare two
  bitmaps. It lets the user zoom in and has a way to toggle between both images for easy
  visual comparison.

  Use the CheckBitmapsEqual function to compare two bitmaps for pixel equality. It will
  show a modal TBitmapCompareDialog if they are not equal.
}

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  GR32;

type
  TBitmapCompareDialog = class(TForm)
    ButtonWantHave: TButton;
    ButtonOverlay: TButton;
    ButtonDiff: TButton;
    Button100: TButton;
    Button200: TButton;
    Button300: TButton;
    Button400: TButton;
    Button500: TButton;
    Button600: TButton;
    Button700: TButton;
    Button800: TButton;
    LeftBitmap: TPaintBox;
    RightBitmap: TPaintBox;
    Overlay: TPaintBox;
    LabelWant: TLabel;
    LabelHave: TLabel;
    LabelOverlay: TLabel;
    LabelColorUnderMouse: TLabel;
    ToggleTimer: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LeftBitmapPaint(Sender: TObject);
    procedure RightBitmapPaint(Sender: TObject);
    procedure OverlayPaint(Sender: TObject);
    procedure ToggleTimerTimer(Sender: TObject);
    procedure ButtonWantHaveClick(Sender: TObject);
    procedure ButtonOverlayClick(Sender: TObject);
    procedure Button100Click(Sender: TObject);
    procedure Button200Click(Sender: TObject);
    procedure Button300Click(Sender: TObject);
    procedure Button400Click(Sender: TObject);
    procedure Button500Click(Sender: TObject);
    procedure Button600Click(Sender: TObject);
    procedure Button700Click(Sender: TObject);
    procedure Button800Click(Sender: TObject);
    procedure UpdateColorUnderMouse(Sender: TObject = nil; Shift: TShiftState = [];
      X: Integer = 0; Y: Integer = 0);
    procedure ButtonDiffClick(Sender: TObject);
  public
    class procedure Execute(Want, Have: TBitmap32);
  private
    Want, Have, Diff1, Diff2: TBitmap32;
    ShowWant: Boolean;
    MinWidth, MinHeight: Integer;
    Zoom: Integer;
    procedure Redraw;
  private
    ShowMode: Integer;
  private const
    // ShowMode can be one of these:
    ShowSideBySide = 0;
    ShowOverlay = 1;
    ShowDiff = 2;
  end;

// BitmapsEqual returns true if both bitmaps have the same dimensions and all pixels are
// the same.
function BitmapsEqual(Want, Have: TBitmap32): Boolean;

// CheckBitmapsEqual compares all pixel colors in the two given bitmaps and shows a modal
// TBitmapCompareDialog if the sizes of the bitmaps or any pixels are different.
procedure CheckBitmapsEqual(Want, Have: TBitmap32);

implementation

{$R *.dfm}

uses
  System.Math,
  GR32_Backends,
  TestFramework;

function BitmapsEqual(Want, Have: TBitmap32): Boolean;
var
  X, Y: Integer;
begin
  Result := false;
  if Want.Width <> Have.Width then
    Exit;
  if Want.Height <> Have.Height then
    Exit;
  for Y := 0 to Want.Height - 1 do
    for X := 0 to Want.Width - 1 do
      if Want.Canvas.Pixels[X, Y] <> Have.Canvas.Pixels[X, Y] then
        Exit;
  Result := true;
end;

procedure CheckBitmapsEqual(Want, Have: TBitmap32);
begin
  if not BitmapsEqual(Want, Have) then
  begin
    TBitmapCompareDialog.Execute(Want, Have);
    raise ETestFailure.Create('bitmaps differ')at ReturnAddress;
  end;
end;

function DiffBitmaps(A, B: TBitmap32; DiffColor: TColor): TBitmap32;
var
  X, Y: Integer;
begin
  Result := TBitmap32.Create;
  Result.SetSize(Max(A.Width, B.Width), Max(A.Height, B.Height));
  Result.Canvas.Brush.Color := DiffColor;
  Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
  for Y := 0 to Min(A.Height, B.Height) - 1 do
    for X := 0 to Min(A.Width, B.Width) - 1 do
      if A.Canvas.Pixels[X, Y] = B.Canvas.Pixels[X, Y] then
        Result.Canvas.Pixels[X, Y] := B.Canvas.Pixels[X, Y];
end;

class procedure TBitmapCompareDialog.Execute(Want, Have: TBitmap32);
var
  Dlg: TBitmapCompareDialog;
begin
  Dlg := TBitmapCompareDialog.Create(nil);

  Dlg.Want := Want;
  Dlg.Have := Have;
  Dlg.Diff1 := DiffBitmaps(Want, Have, clRed);
  Dlg.Diff2 := DiffBitmaps(Want, Have, clGreen);

  Dlg.MinWidth := Dlg.ClientWidth;
  Dlg.MinHeight := Dlg.ClientHeight;

  Dlg.ButtonWantHave.Click;
  Dlg.Button100.Click;

  Dlg.ShowModal;

  Dlg.Diff1.Free;
  Dlg.Diff2.Free;
  Dlg.Free;
end;

procedure TBitmapCompareDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = Word('W') then
    ButtonWantHave.Click;
  if Key = Word('O') then
    ButtonOverlay.Click;
  if Key = Word('D') then
    ButtonDiff.Click;
  if (Key = Word('1')) or (Key = VK_NUMPAD1) then
    Button100.Click;
  if (Key = Word('2')) or (Key = VK_NUMPAD2) then
    Button200.Click;
  if (Key = Word('3')) or (Key = VK_NUMPAD3) then
    Button300.Click;
  if (Key = Word('4')) or (Key = VK_NUMPAD4) then
    Button400.Click;
  if (Key = Word('5')) or (Key = VK_NUMPAD5) then
    Button500.Click;
  if (Key = Word('6')) or (Key = VK_NUMPAD6) then
    Button600.Click;
  if (Key = Word('7')) or (Key = VK_NUMPAD7) then
    Button700.Click;
  if (Key = Word('8')) or (Key = VK_NUMPAD8) then
    Button800.Click;
  if Key = VK_ESCAPE then
    Close;
end;

procedure TBitmapCompareDialog.LeftBitmapPaint(Sender: TObject);
begin
  LeftBitmap.Canvas.Pen.Color := clGreen;
  LeftBitmap.Canvas.Rectangle(0, 0, LeftBitmap.Width, LeftBitmap.Height);
  LeftBitmap.Canvas.CopyRect(Rect(1, 1, 1 + Zoom * Want.Width, 1 + Zoom * Want.Height),
    Want.Canvas, Rect(0, 0, Want.Width, Want.Height));
end;

procedure TBitmapCompareDialog.RightBitmapPaint(Sender: TObject);
begin
  RightBitmap.Canvas.Pen.Color := clRed;
  RightBitmap.Canvas.Rectangle(0, 0, RightBitmap.Width, RightBitmap.Height);
  RightBitmap.Canvas.CopyRect(Rect(1, 1, 1 + Zoom * Have.Width, 1 + Zoom * Have.Height),
    Have.Canvas, Rect(0, 0, Have.Width, Have.Height));
end;

procedure TBitmapCompareDialog.OverlayPaint(Sender: TObject);
var
  Bmp: TBitmap32;
begin
  if ShowMode = ShowDiff then
  begin
    if ShowWant then
      Bmp := Diff1
    else
      Bmp := Diff2;

    Overlay.Canvas.Pen.Color := clMaroon;
  end
  else if ShowWant then
  begin
    Bmp := Want;
    Overlay.Canvas.Pen.Color := clGreen;
  end
  else
  begin
    Bmp := Have;
    Overlay.Canvas.Pen.Color := clRed;
  end;

  Overlay.Canvas.Brush.Color := $FF00FF;
  Overlay.Canvas.FillRect(Rect(0, 0, Overlay.Width, Overlay.Height));
  Overlay.Canvas.Rectangle(0, 0, Overlay.Width, Overlay.Height);
  Overlay.Canvas.CopyRect(Rect(1, 1, 1 + Zoom * Bmp.Width, 1 + Zoom * Bmp.Height),
    Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height));
end;

procedure TBitmapCompareDialog.ToggleTimerTimer(Sender: TObject);
begin
  ShowWant := not ShowWant;
  Redraw;
end;

procedure ShowColorUnderMouse(B: TPaintBox; L: TLabel);
var
  P: TPoint;
  C: TColor;
begin
  P := B.ScreenToClient(Mouse.CursorPos);
  if B.ClientRect.Contains(P) then
  begin
    C := B.Canvas.Pixels[P.X, P.Y];
    L.Caption := Format('RGB(%3.d,%3.d,%3.d)  $%.6x  Integer(%.8d)',
      [C and $FF, (C and $FF00) shr 8, (C and $FF0000) shr 16, C, C]);
  end;
end;

procedure TBitmapCompareDialog.UpdateColorUnderMouse(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  LabelColorUnderMouse.Caption := '<Color Under Mouse>';
  ShowColorUnderMouse(LeftBitmap, LabelColorUnderMouse);
  ShowColorUnderMouse(RightBitmap, LabelColorUnderMouse);
  ShowColorUnderMouse(Overlay, LabelColorUnderMouse);
end;

procedure TBitmapCompareDialog.ButtonWantHaveClick(Sender: TObject);
begin
  ShowMode := ShowSideBySide;
  if Visible then
    ButtonWantHave.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.ButtonOverlayClick(Sender: TObject);
begin
  ShowMode := ShowOverlay;
  ButtonOverlay.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.ButtonDiffClick(Sender: TObject);
begin
  ShowMode := ShowDiff;
  ButtonDiff.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button100Click(Sender: TObject);
begin
  Zoom := 1;
  if Visible then
    Button100.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button200Click(Sender: TObject);
begin
  Zoom := 2;
  Button200.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button300Click(Sender: TObject);
begin
  Zoom := 3;
  Button300.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button400Click(Sender: TObject);
begin
  Zoom := 4;
  Button400.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button500Click(Sender: TObject);
begin
  Zoom := 5;
  Button500.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button600Click(Sender: TObject);
begin
  Zoom := 6;
  Button600.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button700Click(Sender: TObject);
begin
  Zoom := 7;
  Button700.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Button800Click(Sender: TObject);
begin
  Zoom := 8;
  Button800.SetFocus;
  Redraw;
end;

procedure TBitmapCompareDialog.Redraw;
var
  OldW, OldH: Integer;
begin
  LeftBitmap.Visible := ShowMode = ShowSideBySide;
  RightBitmap.Visible := ShowMode = ShowSideBySide;
  LabelWant.Visible := ShowMode = ShowSideBySide;
  LabelHave.Visible := ShowMode = ShowSideBySide;
  Overlay.Visible := ShowMode <> ShowSideBySide;
  LabelOverlay.Left := 0;
  LabelOverlay.Width := Self.ClientWidth;
  LabelOverlay.Visible := Overlay.Visible;

  if ShowMode = ShowDiff then
  begin
    LabelOverlay.Font.Color := clMaroon;
    LabelOverlay.Caption := 'Diff';
  end
  else if ShowWant then
  begin
    LabelOverlay.Font.Color := clGreen;
    LabelOverlay.Caption := 'Want';
  end
  else
  begin
    LabelOverlay.Font.Color := clRed;
    LabelOverlay.Caption := 'Have';
  end;

  LeftBitmap.Width := 2 + Zoom * Want.Width;
  LeftBitmap.Height := 2 + Zoom * Want.Height;
  RightBitmap.Width := 2 + Zoom * Have.Width;
  RightBitmap.Height := 2 + Zoom * Have.Height;
  Overlay.Width := 2 + Max(Zoom * Want.Width, Zoom * Have.Width);
  Overlay.Height := 2 + Max(Zoom * Want.Height, Zoom * Have.Height);

  OldW := Self.ClientWidth;
  OldH := Self.ClientHeight;

  if ShowMode = ShowSideBySide then
  begin
    Self.ClientWidth := Max(MinWidth, 2 * Max(LeftBitmap.Width, RightBitmap.Width) + 14);
    Self.ClientHeight := Max(MinHeight, Max(LeftBitmap.Top + LeftBitmap.Height,
      RightBitmap.Top + RightBitmap.Height) + 36);
  end
  else
  begin
    Self.ClientWidth := Max(MinWidth, Max(LeftBitmap.Width, RightBitmap.Width) + 14);
    Self.ClientHeight := Max(MinHeight, Overlay.Top + Overlay.Height + 36);
  end;

  Self.Left := Self.Left - (Self.ClientWidth - OldW) div 2;
  Self.Top := Self.Top - (Self.ClientHeight - OldH) div 2;

  LeftBitmap.Left := Self.ClientWidth div 2 - LeftBitmap.Width - 2;
  RightBitmap.Left := Self.ClientWidth div 2 + 2;
  Overlay.Left := (Self.ClientWidth - Overlay.Width) div 2;

  LabelWant.Left := LeftBitmap.Left + LeftBitmap.Width - LabelWant.Width;
  LabelHave.Left := RightBitmap.Left;

  LeftBitmap.Invalidate;
  RightBitmap.Invalidate;
  Overlay.Invalidate;
  UpdateColorUnderMouse;
end;

end.
