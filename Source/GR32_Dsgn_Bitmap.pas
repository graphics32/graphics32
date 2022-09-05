unit GR32_Dsgn_Bitmap;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
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
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, RtlConsts, Buttons, LazIDEIntf, PropEdits,
  ComponentEditors,
{$ELSE}
  Windows, ExtDlgs, ToolWin, Registry, ImgList, Consts, DesignIntf,
  DesignEditors, VCLEditors, Actions,
{$ENDIF}
  Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics, Dialogs, Menus,
  SysUtils, Classes, Clipbrd, ActnList,
  GR32, GR32_Image, GR32_Layers, GR32_Filters;

type
  TPictureEditorForm = class(TForm)
    TabSheetAlpha: TTabSheet;
    Bevel1: TBevel;
    Cancel: TButton;
    ButtonClear: TToolButton;
    ButtonCopy: TToolButton;
    ImageList: TImageList;
    TabSheetRGB: TTabSheet;
    Label1: TLabel;
    ButtonLoad: TToolButton;
    MagnCombo: TComboBox;
    MenuItemClear: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemInvert: TMenuItem;
    MenuItemLoad: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemSave: TMenuItem;
    mnSeparator: TMenuItem;
    mnSeparator2: TMenuItem;
    OKButton: TButton;
    PageControl: TPageControl;
    Panel1: TPanel;
    ButtonPaste: TToolButton;
    PopupMenu: TPopupMenu;
    ButtonSave: TToolButton;
    ToolBar: TToolBar;
    ToolButton2: TToolButton;
    ActionList: TActionList;
    ActionLoad: TAction;
    ActionSave: TAction;
    ActionClear: TAction;
    ActionCopy: TAction;
    ActionPaste: TAction;
    ActionInvert: TAction;
    TabSheetRGBA: TTabSheet;
    StatusBar: TStatusBar;
    procedure MagnComboChange(Sender: TObject);
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionHasBitmapUpdate(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionInvertExecute(Sender: TObject);
  protected
{$IFDEF PLATFORM_INDEPENDENT}
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
{$ELSE}
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
{$ENDIF}
    ImageAllChannels: TImage32;
    ImageRGBChannels: TImage32;
    ImageAlphaChannel: TImage32;
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImagePaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    function CurrentImage: TImage32;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromImage(Source: TPersistent);
  end;

  TBitmap32Editor = class(TComponent)
  private
    FBitmap32: TBitmap32;
    procedure SetBitmap32(Value: TBitmap32);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property Bitmap32: TBitmap32 read FBitmap32 write SetBitmap32;
  end;

  TBitmap32Property = class(TClassProperty
{$IFDEF EXT_PROP_EDIT}
    , ICustomPropertyDrawing, ICustomPropertyDrawing80
{$ENDIF}
  )
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
{$IFDEF EXT_PROP_EDIT}
    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(Canvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
{$ENDIF}
  end;

  TImage32Editor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  GR32_Resamplers,
  GR32_Backends_Generic;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TPictureEditorForm }

function TPictureEditorForm.CurrentImage: TImage32;
begin
  if PageControl.ActivePage = TabSheetRGBA then
    Result := ImageAllChannels
  else
  if PageControl.ActivePage = TabSheetRGB then
    Result := ImageRGBChannels
  else
    Result := ImageAlphaChannel;
end;

procedure TPictureEditorForm.ImagePaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
const            //0..1
  Colors: array [Boolean] of TColor32 = ($FFFFFFFF, $FFB0B0B0);
var
  R: TRect;
  I, J: Integer;
  OddY: Integer;
  TilesHorz, TilesVert: Integer;
  TileX, TileY: Integer;
  TileHeight, TileWidth: Integer;
begin
  TileHeight := 13;
  TileWidth := 13;

  TilesHorz := Buffer.Width div TileWidth;
  TilesVert := Buffer.Height div TileHeight;
  TileY := 0;

  for J := 0 to TilesVert do
  begin
    TileX := 0;
    OddY := J and $1;
    for I := 0 to TilesHorz do
    begin
      R.Left := TileX;
      R.Top := TileY;
      R.Right := TileX + TileWidth;
      R.Bottom := TileY + TileHeight;
      Buffer.FillRectS(R, Colors[I and $1 = OddY]);
      Inc(TileX, TileWidth);
    end;
    Inc(TileY, TileHeight);
  end;
end;

procedure TPictureEditorForm.LoadFromImage(Source: TPersistent);
begin
  if CurrentImage = ImageAllChannels then
  begin
    // Load RGBA bitmap, separate into RGB and A

    // Load RGBA
    ImageAllChannels.Bitmap.Assign(Source);
    ImageAllChannels.Bitmap.DrawMode := dmBlend;

    // Separate RGB
    ImageRGBChannels.Bitmap.Assign(ImageAllChannels.Bitmap);
    ImageRGBChannels.Bitmap.ResetAlpha;

    // Separate A
    AlphaToGrayscale(ImageAlphaChannel.Bitmap, ImageAllChannels.Bitmap);
    ImageAlphaChannel.Bitmap.ResetAlpha;
  end else
  if CurrentImage = ImageRGBChannels then
  begin
    // Load RGB bitmap, keep existing A

    // Load RGB
    if (Source <> nil) then
    begin
      ImageRGBChannels.Bitmap.Assign(Source);
      ImageRGBChannels.Bitmap.ResetAlpha;
    end else
      ImageRGBChannels.Bitmap.Clear($FF000000);

    // Merge A and RGB into RGBA
    ImageAllChannels.Bitmap.Assign(ImageRGBChannels.Bitmap);
    ImageAllChannels.Bitmap.DrawMode := dmBlend;
    if (not ImageAlphaChannel.Bitmap.Empty) then
      IntensityToAlpha(ImageAllChannels.Bitmap, ImageAlphaChannel.Bitmap)
    else
      ImageAllChannels.Bitmap.ResetAlpha;
  end else
  if CurrentImage = ImageAlphaChannel then
  begin
    // Load A bitmap, keep existing RGB
    if (Source <> nil) then
      ImageAlphaChannel.Bitmap.Assign(Source)
    else
      ImageAlphaChannel.Bitmap.Clear($FFFFFFFF);
    ColorToGrayscale(ImageAlphaChannel.Bitmap, ImageAlphaChannel.Bitmap);

    // Merge A and RGB into RGBA
    if (not ImageRGBChannels.Bitmap.Empty) then
    begin
      ImageAllChannels.Bitmap.Assign(ImageRGBChannels.Bitmap);
      ImageAllChannels.Bitmap.DrawMode := dmBlend;
    end else
    begin
      ImageAllChannels.Bitmap.SetSizeFrom(ImageAlphaChannel.Bitmap);
      ImageAllChannels.Bitmap.Clear;
    end;
    IntensityToAlpha(ImageAllChannels.Bitmap, ImageAlphaChannel.Bitmap);
  end;
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
    ImageAllChannels.ScaleMode := smResize;
    ImageRGBChannels.ScaleMode := smResize;
    ImageAlphaChannel.ScaleMode := smResize;
  end else
  begin
    ImageAllChannels.ScaleMode := smScale;
    ImageAllChannels.Scale := S / 100;
    ImageRGBChannels.ScaleMode := smScale;
    ImageRGBChannels.Scale := S / 100;
    ImageAlphaChannel.ScaleMode := smScale;
    ImageAlphaChannel.Scale := S / 100;
  end;
end;

constructor TPictureEditorForm.Create(AOwner: TComponent);

  function CreateImage32(AParent: TWinControl): TImage32;
  begin
    Result := TImage32.Create(Self);
    Result.Parent := AParent;
    Result.Align := alClient;
    Result.BitmapAlign := baCenter;
    Result.Cursor := crCross;
    Result.PopupMenu := PopupMenu;
    Result.OnMouseMove := ImageMouseMove;
    Result.OnPaintStage := ImagePaintStage;
    if (Result.PaintStages[0].Stage = PST_CLEAR_BACKGND) then
      Result.PaintStages[0].Stage := PST_CUSTOM;
  end;

begin
  inherited;

  ImageAllChannels := CreateImage32(TabSheetRGBA);
  ImageAllChannels.Bitmap.DrawMode := dmBlend;
  ImageRGBChannels := CreateImage32(TabSheetRGB);
  ImageAlphaChannel := CreateImage32(TabSheetAlpha);

{$IFDEF PLATFORM_INDEPENDENT}
  OpenDialog := TOpenDialog.Create(Self);
  SaveDialog := TSaveDialog.Create(Self);
{$ELSE}
  OpenDialog := TOpenPictureDialog.Create(Self);
  SaveDialog := TSavePictureDialog.Create(Self);
{$ENDIF}
  MagnCombo.ItemIndex := 2;
  OpenDialog.Filter := GraphicFilter(TGraphic);
  SaveDialog.Filter := GraphicFilter(TGraphic);
end;


{ TBitmap32Editor }

constructor TBitmap32Editor.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap32 := TBitmap32.Create;
end;

destructor TBitmap32Editor.Destroy;
begin
  FBitmap32.Free;
  inherited;
end;

function TBitmap32Editor.Execute: Boolean;
var
  PictureEditorForm: TPictureEditorForm;
begin
  PictureEditorForm := TPictureEditorForm.Create(Self);
  try

    PictureEditorForm.LoadFromImage(FBitmap32);

    Result := (PictureEditorForm.ShowModal = mrOK);

    if Result then
      FBitmap32.Assign(PictureEditorForm.ImageAllChannels.Bitmap);

  finally
    PictureEditorForm.Free;
  end;
end;

procedure TBitmap32Editor.SetBitmap32(Value: TBitmap32);
begin
  try
    FBitmap32.Assign(Value);
  except
    on E: Exception do
      ShowMessage(E.Message);
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
{$IFDEF FPC}
      BitmapEditor.Bitmap32 := TBitmap32(GetObjectValue);
{$ELSE}
      BitmapEditor.Bitmap32 := TBitmap32(Pointer(GetOrdValue));
{$ENDIF}
      if BitmapEditor.Execute then
      begin
{$IFDEF FPC}
        SetPtrValue(BitmapEditor.Bitmap32);
{$ELSE}
        SetOrdValue(Longint(BitmapEditor.Bitmap32));
{$ENDIF}
      end;
    finally
      BitmapEditor.Free;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
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
{$IFDEF FPC}
    Bitmap := TBitmap32(GetObjectValue);
{$ELSE}
    Bitmap := TBitmap32(GetOrdValue);
{$ENDIF}
    if (Bitmap = nil) or Bitmap.Empty then
      Result := srNone
    else
      Result := Format('%s [%d,%d]', [Bitmap.ClassName, Bitmap.Width, Bitmap.Height]);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{$IFDEF EXT_PROP_EDIT}
procedure TBitmap32Property.PropDrawValue(Canvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Bitmap32: TBitmap32;
  TmpBitmap: TBitmap32;
  R: TRect;
begin
  Bitmap32 := TBitmap32(GetOrdValue);
  if Bitmap32.Empty then
    DefaultPropertyDrawValue(Self, Canvas, ARect)
  else
  begin
    R := ARect;
    R.Right := R.Left + R.Bottom - R.Top;

    TmpBitmap := TBitmap32.Create;
    TmpBitmap.Width := R.Right - R.Left;
    TmpBitmap.Height := R.Bottom - R.Top;
    TDraftResampler.Create(TmpBitmap);
    TmpBitmap.Draw(TmpBitmap.BoundsRect, Bitmap32.BoundsRect, Bitmap32);
    TmpBitmap.DrawTo(Canvas.Handle, R, TmpBitmap.BoundsRect);
    TmpBitmap.Free;

    R.Left := R.Right;
    R.Right := ARect.Right;
    DefaultPropertyDrawValue(Self, Canvas, R);
  end;
end;

procedure TBitmap32Property.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TBitmap32Property.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TBitmap32Property.PropDrawValueRect(const ARect: TRect): TRect;
begin
  if TBitmap32(GetOrdValue).Empty then
    Result := ARect
  else
    Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

{$ENDIF}

procedure TBitmap32Property.SetValue(const Value: string);
begin
  if Value = '' then
    SetOrdValue(0);
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
  if Index = 0 then
    Result := 'Bitmap32 Editor...';
end;

function TImage32Editor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TPictureEditorForm.ActionClearExecute(Sender: TObject);
begin
  LoadFromImage(nil);
end;

procedure TPictureEditorForm.ActionLoadExecute(Sender: TObject);
var
  Picture: TPicture;
  Bitmap: TBitmap32;
begin
  if not OpenDialog.Execute then
    exit;

  // Load bitmap directly if file is a BMP
  // (this works around alleged bug in TBitmap->TBitmap32 conversion with LCL-Gtk backend)
  if (SameText(ExtractFileExt(OpenDialog.Filename), '.bmp')) then
  begin
    Bitmap := TBitmap32.Create(TMemoryBackend);
    try
      Bitmap.LoadFromFile(OpenDialog.Filename);
      LoadFromImage(Bitmap);
    finally
      Bitmap.Free;
    end;
  end else
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(OpenDialog.Filename);
      LoadFromImage(Picture);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TPictureEditorForm.ActionPasteExecute(Sender: TObject);
var
  Bitmap: TBitmap32;
begin
  Bitmap := TBitmap32.Create;
  try
    Bitmap.Assign(Clipboard);
    LoadFromImage(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TPictureEditorForm.ActionPasteUpdate(Sender: TObject);
begin
  try
    TAction(Sender).Enabled := Clipboard.HasFormat(CF_PICTURE);
  except
{$IFDEF FPC}
    TAction(Sender).Enabled := False;
{$ELSE FPC}
    on E: EClipboardException do
      TAction(Sender).Enabled := False; // Something else has the clipboard open
{$ENDIF FPC}
  end;
end;

procedure TPictureEditorForm.ActionSaveExecute(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  if (CurrentImage.Bitmap.Empty) then
    exit;

  SaveDialog.DefaultExt := GraphicExtension(TBitmap);
  SaveDialog.Filter := GraphicFilter(TBitmap);

  if not SaveDialog.Execute then
    exit;

  if (CurrentImage = ImageAllChannels) then
    // Save in 32-bit RGBA bitmap
    ImageAllChannels.Bitmap.SaveToFile(SaveDialog.Filename)
  else
  begin
    // Save 24-bit RGB bitmap
    Bitmap := TBitmap.Create;
    try
      Bitmap.Assign(CurrentImage.Bitmap);
      Bitmap.PixelFormat := pf24Bit;

      Bitmap.SaveToFile(SaveDialog.Filename)
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TPictureEditorForm.ActionCopyExecute(Sender: TObject);
begin
  Clipboard.Assign(CurrentImage.Bitmap);
end;

procedure TPictureEditorForm.ActionHasBitmapUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (CurrentImage <> nil) and (not CurrentImage.Bitmap.Empty);
end;

procedure TPictureEditorForm.ActionInvertExecute(Sender: TObject);
begin
  if (CurrentImage = ImageAllChannels) then
  begin
    Invert(ImageAllChannels.Bitmap, ImageAllChannels.Bitmap);
    InvertRGB(ImageRGBChannels.Bitmap, ImageRGBChannels.Bitmap);
    InvertRGB(ImageAlphaChannel.Bitmap, ImageAlphaChannel.Bitmap);
  end else
  if (CurrentImage = ImageRGBChannels) then
  begin
    InvertRGB(ImageAllChannels.Bitmap, ImageAllChannels.Bitmap);
    InvertRGB(ImageRGBChannels.Bitmap, ImageRGBChannels.Bitmap);
  end else
  begin
    Invert(ImageAllChannels.Bitmap, ImageAllChannels.Bitmap, [ccAlpha]);
    InvertRGB(ImageAlphaChannel.Bitmap, ImageAlphaChannel.Bitmap);
  end;
end;

procedure TPictureEditorForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  Image: TImage32;
  P: TPoint;
  Color: TColor32Entry;
  ColorHex: string;
  ColorChannels: string;
begin
  Image := TImage32(Sender);

  if (Image.Bitmap = nil) or (Image.Bitmap.Empty) then
  begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
    exit;
  end;

  P := Image.ControlToBitmap(Point(X, Y));

  if (P.X >= 0) and (P.Y >= 0) and
    (P.X < Image.Bitmap.Width) and (P.Y < Image.Bitmap.Height) then
  begin
    Color := TColor32Entry(Image.Bitmap[P.X, P.Y]);

    if (Image = ImageAllChannels) then
    begin
      ColorHex := Format('ARGB: $%.8X', [Color.ARGB]);
      ColorChannels := Format('A:%-3d R:%-3d G:%-3d B:%-3d', [Color.A, Color.R, Color.G, Color.B]);
    end else
    if (Image = ImageRGBChannels) then
    begin
      ColorHex := Format('RGB: $%.6X', [Color.ARGB and $00FFFFFF]);
      ColorChannels := Format('R:%-3d G:%-3d B:%-3d', [Color.R, Color.G, Color.B]);
    end else
    begin
      ColorHex := Format('Alpha: $%.2X', [Color.R]);
      ColorChannels := Format('A:%-3d', [Color.R]);
    end;

    StatusBar.Panels[0].Text := ColorHex;
    StatusBar.Panels[1].Text := ColorChannels;
    StatusBar.Panels[2].Text := Format('X:%-2d Y:%-2d', [P.X, P.Y])
  end else
  begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
  end;
end;

end.
