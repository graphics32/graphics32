unit GR32_Dsgn_Bitmap;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF CLX}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF LINUX}Libc,{$ENDIF}
  QT, QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QComCtrls,
  QMenus, QImgList, QTypes, QClipbrd,
{$ELSE}
  Windows, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ExtDlgs,
  ComCtrls, Menus, ToolWin, Registry, ImgList, Clipbrd,
{$ENDIF}
  SysUtils, Classes, Consts,
  GR32, GR32_Image, GR32_Layers, GR32_Filters,
{$IFDEF COMPILER6}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF};

type
  TPictureEditorForm = class(TForm)
    ToolBar1: TToolBar;
    Load: TToolButton;
    Save: TToolButton;
    ImageList: TImageList;
    Clear: TToolButton;
    ToolButton2: TToolButton;
    Copy: TToolButton;
    Paste: TToolButton;
    Timer: TTimer;
    PageControl: TPageControl;
    ImageSheet: TTabSheet;
    AlphaSheet: TTabSheet;
    // TODO: Remove
    //OpenDialog: TOpenPictureDialog;
    //SaveDialog: TSavePictureDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PopupMenu: TPopupMenu;
    mnSave: TMenuItem;
    mnSeparator: TMenuItem;
    mnCopy: TMenuItem;
    mnPaste: TMenuItem;
    mnClear: TMenuItem;
    Load1: TMenuItem;
    mnSeparator2: TMenuItem;
    mnInvert: TMenuItem;
    Panel1: TPanel;
    OKButton: TButton;
    Cancel: TButton;
    Label1: TLabel;
    MagnCombo: TComboBox;
    Panel2: TPanel;
    Bevel1: TBevel;
    procedure LoadClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure mnInvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MagnComboChange(Sender: TObject);
  protected
    AlphaChannel: TImage32;
    RGBChannels: TImage32;
    procedure AlphaChannelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure RGBChannelsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    function CurrentImage: TImage32;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TBitmap32Editor = class(TComponent)
  private
    FBitmap32: TBitmap32;
    FPicDlg: TPictureEditorForm;
    procedure SetBitmap32(Value: TBitmap32);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property Bitmap32: TBitmap32 read FBitmap32 write SetBitmap32;
  end;

  TBitmap32Property = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
{$IFDEF EXT_PROP_EDIT}
    procedure PropDrawValue(Canvas: TCanvas; const ARect: TRect; Selected: Boolean); override;
{$ENDIF}
  end;

  TImage32Editor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{$IFDEF CLX}
{$R *.xfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TPictureEditorForm }

procedure TPictureEditorForm.LoadClick(Sender: TObject);
var
  Picture: TPicture;
  DoAlpha: Boolean;
  S: string;
begin
  if OpenDialog.Execute then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(OpenDialog.Filename);
      DoAlpha := False;
      if (Picture.Graphic is TBitmap) and (Picture.Bitmap.PixelFormat = pf32Bit) then
      begin
        S := ExtractFileName(OpenDialog.FileName);
        S := '''' + S + ''' file contains RGB and Alpha channels.'#13#10 +
          'Do you want to load all channels?';
        case MessageDlg(S, mtConfirmation, mbYesNoCancel, 0) of
          mrYes: DoAlpha := True;
          mrCancel: Exit;
        end;
      end;

      if DoAlpha then
      begin
        RGBChannels.Bitmap.Assign(Picture.Bitmap);
        AlphaToGrayscale(AlphaChannel.Bitmap, RGBChannels.Bitmap);
        RGBChannels.Bitmap.ResetAlpha;
      end
      else with CurrentImage do
      begin
        Bitmap.Assign(Picture);
        if CurrentImage = AlphaChannel then ColorToGrayscale(Bitmap, Bitmap);
      end;
    finally
      Picture.Free;
    end;
  end;
end;

procedure TPictureEditorForm.SaveClick(Sender: TObject);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Picture.Bitmap.Assign(CurrentImage.Bitmap);
{$IFDEF CLX}
    Picture.Bitmap.PixelFormat := pf32Bit;
{$ELSE}
    Picture.Bitmap.PixelFormat := pf24Bit;
{$ENDIF}

    if Picture.Graphic <> nil then
    begin
      with SaveDialog do
      begin
        DefaultExt := GraphicExtension(TGraphicClass(Picture.Graphic.ClassType));
        Filter := GraphicFilter(TGraphicClass(Picture.Graphic.ClassType));
        if Execute then Picture.SaveToFile(Filename);
      end;
    end;
  finally
    Picture.Free;
  end;
end;

procedure TPictureEditorForm.ClearClick(Sender: TObject);
begin
  CurrentImage.Bitmap.Delete;
end;

procedure TPictureEditorForm.CopyClick(Sender: TObject);
begin
  Clipboard.Assign(CurrentImage.Bitmap);
end;

procedure TPictureEditorForm.PasteClick(Sender: TObject);
begin
{$IFDEF CLX}
  if Clipboard.Provides('image/delphi.bitmap') or
     Clipboard.Provides('image/delphi.picture') then
     CurrentImage.Bitmap.Assign(Clipboard);
{$ELSE}
  if Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_PICTURE) then
    CurrentImage.Bitmap.Assign(Clipboard);
{$ENDIF}
  if CurrentImage = AlphaChannel then
    ColorToGrayscale(CurrentImage.Bitmap, CurrentImage.Bitmap);
end;

procedure TPictureEditorForm.TimerTimer(Sender: TObject);
begin
  Save.Enabled := not CurrentImage.Bitmap.Empty;
  Clear.Enabled := Save.Enabled;
  Copy.Enabled := Save.Enabled;

{$IFDEF CLX}
  Paste.Enabled := Clipboard.Provides('image/delphi.bitmap') or
    Clipboard.Provides('image/delphi.picture');
{$ELSE}
  Paste.Enabled := Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_PICTURE);
{$ENDIF}
end;

function TPictureEditorForm.CurrentImage: TImage32;
begin
  if PageControl.ActivePage = ImageSheet then Result := RGBChannels
  else Result := AlphaChannel;
end;

procedure TPictureEditorForm.PopupMenuPopup(Sender: TObject);
begin
  mnSave.Enabled := not CurrentImage.Bitmap.Empty;
  mnClear.Enabled := Save.Enabled;
  mnCopy.Enabled := Save.Enabled;
  mnInvert.Enabled := Save.Enabled;
{$IFDEF CLX}
  mnPaste.Enabled := Clipboard.Provides('image/delphi.bitmap') or
    Clipboard.Provides('image/delphi.picture');
{$ELSE}
  mnPaste.Enabled := Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_PICTURE);
{$ENDIF}
end;

procedure TPictureEditorForm.mnInvertClick(Sender: TObject);
begin
  InvertRGB(CurrentImage.Bitmap, CurrentImage.Bitmap);
end;

procedure TPictureEditorForm.FormCreate(Sender: TObject);
begin
  MagnCombo.ItemIndex := 2;
{$IFDEF CLX}
  OpenDialog.Filter := GraphicFilter(TGraphic, True);
  SaveDialog.Filter := GraphicFilter(TGraphic, True);
{$ELSE}
  OpenDialog.Filter := GraphicFilter(TGraphic);
  SaveDialog.Filter := GraphicFilter(TGraphic);
{$ENDIF}
end;

procedure TPictureEditorForm.MagnComboChange(Sender: TObject);
const
  MAGN: array[0..6] of Integer = (25, 50, 100, 200, 400, 800, -1);
var
  S: Integer;
begin
  S := MAGN[MagnCombo.ItemIndex];
  if S = -1 then
  begin
    RGBChannels.ScaleMode := smResize;
    AlphaChannel.ScaleMode := smResize;
  end
  else
  begin
    RGBChannels.ScaleMode := smScale;
    RGBChannels.Scale := S / 100;
    AlphaChannel.ScaleMode := smScale;
    AlphaChannel.Scale := S / 100;
  end;
end;

constructor TPictureEditorForm.Create(AOwner: TComponent);
begin
  inherited;
  RGBChannels := TImage32.Create(Self);
  RGBChannels.Parent := ImageSheet;
  RGBChannels.Align := alClient;
  RGBChannels.OnMouseMove := RGBChannelsMouseMove;
  AlphaChannel := TImage32.Create(Self);
  AlphaChannel.Parent := AlphaSheet;
  AlphaChannel.Align := alClient;
  AlphaChannel.OnMouseMove := AlphaChannelMouseMove;
end;


{ TBitmap32Editor }

constructor TBitmap32Editor.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap32 := TBitmap32.Create;
  FPicDlg := TPictureEditorForm.Create(Self);
end;

destructor TBitmap32Editor.Destroy;
begin
  FBitmap32.Free;
  inherited;
end;

function TBitmap32Editor.Execute: Boolean;
var
  B: TBitmap32;
begin
  FPicDlg.RGBChannels.Bitmap := FBitmap32;
  AlphaToGrayscale(FPicDlg.AlphaChannel.Bitmap, FBitmap32);
  Result := (FPicDlg.ShowModal = mrOK);
  if Result then
  begin
    FBitmap32.Assign(FPicDlg.RGBChannels.Bitmap);
    FBitmap32.ResetAlpha;
    if not FBitmap32.Empty and not FPicDlg.AlphaChannel.Bitmap.Empty then
    begin
      B := TBitmap32.Create;
      try
        B.SetSize(FBitmap32.Width, FBitmap32.Height);
        FPicDlg.AlphaChannel.Bitmap.DrawTo(B, Rect(0, 0, B.Width, B.Height));
        IntensityToAlpha(FBitmap32, B);
      finally
        B.Free;
      end;
    end;
  end;
end;

procedure TBitmap32Editor.SetBitmap32(Value: TBitmap32);
begin
  try
  FBitmap32.Assign(Value);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{ TBitmap32Property }

procedure TBitmap32Property.Edit;
var
  BitmapEditor: TBitmap32Editor;
begin
  try
    BitmapEditor := TBitmap32Editor.Create(nil);
    try
      BitmapEditor.Bitmap32 := TBitmap32(Pointer(GetOrdValue));
      if BitmapEditor.Execute then
      begin
        SetOrdValue(Longint(BitmapEditor.Bitmap32));
        Designer.Modified;
      end;
    finally
      BitmapEditor.Free;
    end;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TBitmap32Property.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paSubProperties];
end;

function TBitmap32Property.GetValue: string;
var
  Bitmap: TBitmap32;
begin
  try
    Bitmap := TBitmap32(GetOrdValue);
    if (Bitmap = nil) or Bitmap.Empty then Result := srNone
    else Result := Format('%s [%d,%d]', [Bitmap.ClassName, Bitmap.Width, Bitmap.Height]);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{$IFDEF EXT_PROP_EDIT}
procedure TBitmap32Property.PropDrawValue(Canvas: TCanvas;
  const ARect: TRect; Selected: Boolean);
var
  Bitmap32: TBitmap32;
  R: TRect;
begin
  Bitmap32 := TBitmap32(GetOrdValue);
  if Bitmap32.Empty then inherited
  else
  begin
    R := ARect;
    R.Right := R.Left + R.Bottom - R.Top;
    Bitmap32.DrawTo(Canvas.Handle, R, Classes.Rect(0, 0, Bitmap32.Width, Bitmap32.Height));
    R.Left := R.Right;
    R.Right := ARect.Right;
    inherited PropDrawValue(Canvas, R, Selected);
  end;
end;
{$ENDIF}

procedure TBitmap32Property.SetValue(const Value: string);
begin
  if Value = '' then SetOrdValue(0);
end;

{ TImage32Editor }

procedure TImage32Editor.ExecuteVerb(Index: Integer);
var
  Img: TCustomImage32;
  BitmapEditor: TBitmap32Editor;
begin
  Img := Component as TCustomImage32;
  if Index = 0 then
  begin
    BitmapEditor := TBitmap32Editor.Create(nil);
    try
      BitmapEditor.Bitmap32 := Img.Bitmap;
      if BitmapEditor.Execute then
      begin
        Img.Bitmap := BitmapEditor.Bitmap32;
        Designer.Modified;
      end;
    finally
      BitmapEditor.Free;
    end;
  end;
end;

function TImage32Editor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := 'Bitmap32 Editor...';
end;

function TImage32Editor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TPictureEditorForm.AlphaChannelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
begin
  if AlphaChannel.Bitmap <> nil then
  begin
    P := AlphaChannel.ControlToBitmap(Point(X, Y));
    X := P.X;
    Y := P.Y;
    if (X >= 0) and (Y >= 0) and (X < AlphaChannel.Bitmap.Width) and
      (Y < AlphaChannel.Bitmap.Height) then
      Panel2.Caption := 'Alpha: $' +
        IntToHex(AlphaChannel.Bitmap[X, Y] and $FF, 2) +
        Format('     '#9'X: %d'#9'Y: %d', [X, Y])
    else
      Panel2.Caption := '';
  end
  else Panel2.Caption := '';
end;

procedure TPictureEditorForm.RGBChannelsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
begin
  if RGBChannels.Bitmap <> nil then
  begin
    P := RGBChannels.ControlToBitmap(Point(X, Y));
    X := P.X;
    Y := P.Y;
    if (X >= 0) and (Y >= 0) and (X < RGBChannels.Bitmap.Width) and
      (Y < RGBChannels.Bitmap.Height) then
      Panel2.Caption := 'RGB: $' +
        IntToHex(RGBChannels.Bitmap[X, Y] and $00FFFFFF, 6) +
        Format(#9'X: %d'#9'Y: %d', [X, Y])
    else
      Panel2.Caption := '';
  end
  else Panel2.Caption := '';
end;

end.
