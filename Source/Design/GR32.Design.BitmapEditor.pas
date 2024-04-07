unit GR32.Design.BitmapEditor;

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
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, RtlConsts, Buttons, LazIDEIntf, PropEdits,
  ComponentEditors,
{$ELSE}
  Windows, ExtDlgs, ToolWin, Registry, ImgList, Consts, DesignIntf,
  DesignEditors, VCLEditors, Actions, System.ImageList,
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
    ButtonLoad: TToolButton;
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
    LabelZoom: TLabel;
    ToolButton1: TToolButton;
    ButtonHelp: TToolButton;
    ActionHelp: TAction;
    ButtonGrid: TToolButton;
    ActionGrid: TAction;
    Bitmap32List: TBitmap32List;
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionHasBitmapUpdate(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionPasteUpdate(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionInvertExecute(Sender: TObject);
    procedure ActionHelpExecute(Sender: TObject);
    procedure ActionGridExecute(Sender: TObject);
    procedure ActionGridUpdate(Sender: TObject);
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
    LayerPixelGrid: TCustomLayer;
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ImageChanged(Sender: TObject);
    function CurrentImage: TImage32;
    procedure ResetZoomAndCenter(Image: TImage32);
    procedure SyncZoomAndPan;
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
  Math,
  Types,
  GR32.ImageFormats,
  GR32_Resamplers,
  GR32_Backends_Generic;

{$R *.dfm}

resourcestring
  sInfo = 'Width: %.0n, Height: %.0n';
  sInfoEmpty = '(empty)';
  sZoom = 'Zoom: %.0n%%';
  sHelp = 'Pan by clicking and dragging.'#13+
    'Zoom with the mouse wheel.'#13+
    'Reset zoom and center with the middle mouse button.';

//------------------------------------------------------------------------------
//
//      TPixelGridLayer
//
//------------------------------------------------------------------------------
// Displays a pixel grid on top of the image
//------------------------------------------------------------------------------
type
  TPixelGridLayer = class(TCustomLayer)
  private
    FImage: TCustomImage32;
    FNeedStipple: array[0..1] of boolean;
    FStipple: array[0..1] of TArrayOfColor32;
    FPattern: array[0..1] of DWORD;
    FColorOn: array[0..1] of TColor32;
    FColorOff: array[0..1] of TColor32;
    procedure SetColorOff(const Index: Integer; const Value: TColor32);
    procedure SetColorOn(const Index: Integer; const Value: TColor32);
    procedure SetPattern(const Index: Integer; const Value: DWORD);
    procedure SetStipple(const Index: Integer; const Value: TArrayOfColor32);
    function GetStipple(const Index: Integer): TArrayOfColor32;
  protected
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection; AImage: TCustomImage32); reintroduce;

    class procedure CreateStipple(var Stipple: TArrayOfColor32; Pattern: DWORD; ColorOn, ColorOff: TColor32);

    property MajorStipple: TArrayOfColor32 index 0 read GetStipple write SetStipple;
    property MajorPattern: DWORD index 0 read FPattern[0] write SetPattern;
    property MajorColorOn: TColor32 index 0 read FColorOn[0] write SetColorOn;
    property MajorColorOff: TColor32 index 0 read FColorOff[0] write SetColorOff;

    property MinorStipple: TArrayOfColor32 index 1 read GetStipple write SetStipple;
    property MinorPattern: DWORD index 1 read FPattern[1] write SetPattern;
    property MinorColorOn: TColor32 index 1 read FColorOn[1] write SetColorOn;
    property MinorColorOff: TColor32 index 1 read FColorOff[1] write SetColorOff;
  end;

//------------------------------------------------------------------------------
//
//      TPixelGridLayer
//
//------------------------------------------------------------------------------
constructor TPixelGridLayer.Create(ALayerCollection: TLayerCollection;
  AImage: TCustomImage32);
begin
  inherited Create(ALayerCollection);
  FImage := AImage;

  FNeedStipple[0] := True;
  FNeedStipple[1] := True;

  FPattern[0] := $55555555;
  FColorOn[0] := $ff353535;
  FColorOff[0] := $ffa0a0a0;

  FPattern[1] := $55555555;
  FColorOn[1] := $ffa0a0a0;
  FColorOff[1] := $ffbfbfbf;
end;

class procedure TPixelGridLayer.CreateStipple(var Stipple: TArrayOfColor32; Pattern: DWORD; ColorOn, ColorOff: TColor32);
var
  i: integer;
  Mask: DWORD;
begin
  SetLength(Stipple, 32);
  Mask := $80000000;
  i := 0;
  while (Mask <> 0) do
  begin
    if (Pattern and Mask = 0) then
      Stipple[i] := ColorOff
    else
      Stipple[i] := ColorOn;

    Mask := Mask shr 1;
    inc(i);
  end;
end;

function TPixelGridLayer.GetStipple(const Index: Integer): TArrayOfColor32;
begin
  if (FNeedStipple[Index]) then
    CreateStipple(FStipple[Index], FPattern[Index], FColorOn[Index], FColorOff[Index]);
  Result := FStipple[Index];
end;

procedure TPixelGridLayer.Paint(Buffer: TBitmap32);
var
  i: integer;
  Step: integer;
  MinStep: integer;
  Lines: integer;
  Rect: TRect;
  Size: TSize;
  p: TPoint;
begin
  try
    if (Abs(FImage.Scale) >= 4) then
      Step := 1
    else
      Step := 4;

    // Enforce minimal grid of 4 pixels
    MinStep := Ceil(4 / Abs(FImage.Scale));

    Step := Max(Step, MinStep);

    Rect := FImage.GetBitmapRect;
    Size.cx := FImage.Bitmap.Width;
    Size.cy := FImage.Bitmap.Height;

    // Minor grid
    Buffer.StippleStep := 1;
    Buffer.SetStipple(MinorStipple);
    i := Step;
    Lines := 1;
    while (i < Size.cx) or (i < Size.cy) do
    begin
      if (Abs(FImage.Scale) <= 4) or (Lines mod 8 <> 0) then
      begin
        p := FImage.BitmapToControl(GR32.Point(i, i));

        // Vertical line
        if (i < Size.cx) then
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(p.X, Rect.Top+1, Rect.Bottom-1);
        end;
        // Horizontal line
        if (i < Size.cy) then
        begin
          Buffer.StippleCounter := 0;
          Buffer.HorzLineTSP(Rect.Left+1, p.Y, Rect.Right-1);
        end;
      end;
      inc(i, Step);
      inc(Lines);
    end;

    // Major grid
    Buffer.SetStipple(MajorStipple);
    i := Step*8;
    if (Abs(FImage.Scale) > 4) then
      while (i < Size.cx) or (i < Size.cy) do
      begin
        p := FImage.BitmapToControl(GR32.Point(i, i));

        // Vertical line
        if (i < Size.cx) then
        begin
          Buffer.StippleCounter := 0;
          Buffer.VertLineTSP(p.X, Rect.Top+1, Rect.Bottom-1);
        end;
        // Horizontal line
        if (i < Size.cy) then
        begin
          Buffer.StippleCounter := 0;
          Buffer.HorzLineTSP(Rect.Left+1, p.Y, Rect.Right-1);
        end;

        inc(i, Step*8);
      end;

    // Vertical border kines
    Buffer.StippleCounter := 1;
    Buffer.VertLineTSP(Rect.Left, Rect.Top, Rect.Bottom);
    Buffer.StippleCounter := 1;
    Buffer.VertLineTSP(Rect.Right, Rect.Top, Rect.Bottom);
    // Horizontal border lines
    Buffer.StippleCounter := 0;
    Buffer.HorzLineTSP(Rect.Left+1, Rect.Top, Rect.Right-1);
    Buffer.StippleCounter := 0;
    Buffer.HorzLineTSP(Rect.Left+1, Rect.Bottom, Rect.Right-1);
  except
    // Prevent AV flood due to repaint
    Visible := False;
    raise;
  end;
end;

procedure TPixelGridLayer.SetColorOff(const Index: Integer; const Value: TColor32);
begin
  FColorOff[Index] := Value;
  FNeedStipple[Index] := True;
end;

procedure TPixelGridLayer.SetColorOn(const Index: Integer; const Value: TColor32);
begin
  FColorOn[Index] := Value;
  FNeedStipple[Index] := True;
end;

procedure TPixelGridLayer.SetPattern(const Index: Integer; const Value: DWORD);
begin
  FPattern[Index] := Value;
  FNeedStipple[Index] := True;
end;

procedure TPixelGridLayer.SetStipple(const Index: Integer;
  const Value: TArrayOfColor32);
begin
  FStipple[Index] := Value;
  FNeedStipple[Index] := False;
end;

{ TPictureEditorForm }

function TPictureEditorForm.CurrentImage: TImage32;
begin
  if PageControl.ActivePage = TabSheetRGB then
    Result := ImageRGBChannels
  else
  if PageControl.ActivePage = TabSheetAlpha then
    Result := ImageAlphaChannel
  else
    Result := ImageAllChannels
end;

procedure TPictureEditorForm.LoadFromImage(Source: TPersistent);

  procedure UpdateImageBackground(Image: TImage32);
  begin
    if (Image.Bitmap.Empty) then
    begin
      Image.Background.OuterBorderColor := clNone;
      Image.Background.InnerBorderColor := clNone;
      Image.Background.InnerBorderWidth := 0;
      Image.Background.FillStyle := bfsCheckers;
    end else
    begin
      Image.Background.OuterBorderColor := clGray;
      Image.Background.InnerBorderColor := clWhite;
      Image.Background.InnerBorderWidth := 8;
      Image.Background.FillStyle := bfsColor;
    end;
  end;

begin
  ImageAllChannels.BeginUpdate;
  ImageRGBChannels.BeginUpdate;
  ImageAlphaChannel.BeginUpdate;
  try
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

    ResetZoomAndCenter(ImageAllChannels);
    ResetZoomAndCenter(ImageRGBChannels);
    ResetZoomAndCenter(ImageAlphaChannel);

    UpdateImageBackground(ImageAllChannels);
    UpdateImageBackground(ImageRGBChannels);
    UpdateImageBackground(ImageAlphaChannel);
  finally
    ImageAllChannels.EndUpdate;
    ImageRGBChannels.EndUpdate;
    ImageAlphaChannel.EndUpdate;
  end;
  ImageAllChannels.Changed;
  ImageRGBChannels.Changed;
  ImageAlphaChannel.Changed;

  if (ImageAllChannels.Bitmap.Empty) then
    StatusBar.Panels[3].Text := sInfoEmpty
  else
    StatusBar.Panels[3].Text := Format(sInfo, [1.0*ImageAllChannels.Bitmap.Width, 1.0*ImageAllChannels.Bitmap.Height]);
end;

procedure TPictureEditorForm.ResetZoomAndCenter(Image: TImage32);
var
  Size: TSize;
begin
  Image.BeginUpdate;
  try
    // Reset Zoom...
    Image.Scale := 1;

    // ...and Center image
    Size := Image.GetBitmapSize;
    Image.OffsetHorz := (Image.Width-Size.cx) div 2;
    Image.OffsetVert := (Image.Height-Size.cy) div 2;
  finally
    Image.EndUpdate;
  end;
  Image.Changed;
end;

procedure TPictureEditorForm.SyncZoomAndPan;

  procedure DoSync(Image: TImage32);
  begin
    if (Image = CurrentImage) then
      exit;

    Image.BeginUpdate; // Avoid recursion
    try
      Image.Scale := CurrentImage.Scale;
      Image.OffsetHorz := CurrentImage.OffsetHorz;
      Image.OffsetVert := CurrentImage.OffsetVert;
    finally
      Image.EndUpdate;
    end;
    // Invalidate without firing OnChange
    Image.ForceFullInvalidate;
  end;

begin
  if (CurrentImage = nil) then
    exit;

  LabelZoom.Caption := Format(sZoom, [CurrentImage.Scale * 100]);

  DoSync(ImageAllChannels);
  DoSync(ImageRGBChannels);
  DoSync(ImageAlphaChannel);
end;

constructor TPictureEditorForm.Create(AOwner: TComponent);

  function CreateImage32(AParent: TWinControl): TImage32;
  begin
    Result := TImage32.Create(Self);
    Result.Parent := AParent;
    Result.Align := alClient;
    Result.BitmapAlign := baCustom;
    Result.Cursor := crCross;
    Result.PopupMenu := PopupMenu;
    Result.Background.CheckersStyle := bcsMedium;
    Result.Background.OuterBorderColor := clGray;
    Result.Background.InnerBorderColor := clWhite;
    Result.Background.InnerBorderWidth := 8;
    Result.Background.FillStyle := bfsCheckers;
    Result.MousePan.Enabled := True;
    Result.MousePan.PanCursor := crSizeAll;
    Result.MouseZoom.Enabled := True;
    Result.MouseZoom.Animate := True;
    Result.TabStop := True; // Required for mouse wheel
    Result.Scale := 1;
    Result.ScaleMode := smScale;
    Result.OnMouseMove := ImageMouseMove;
    Result.OnMouseDown := ImageMouseDown;
    Result.OnChange := ImageChanged;
  end;

  procedure LoadGlyphs;
  var
    i: integer;
    Bitmap: TBitmap;
begin
    // We're not storing bitmaps in the imagelist in order to support FPC.
    // FPC's TImageList doesn't have the ColorDepth property.
    ImageList.Clear;
{$ifndef FPC}
    ImageList.ColorDepth := cd32bit;
{$endif FPC}
    Bitmap := TBitmap.Create;
    try
      for i := 0 to Bitmap32List.Bitmaps.Count-1 do
      begin
        Bitmap.Assign(Bitmap32List.Bitmaps[i].Bitmap);
        ImageList.AddMasked(Bitmap, -1);
      end;
    finally
      Bitmap.Free;
    end;
    Bitmap32List.Bitmaps.Clear;
  end;

begin
  inherited;

  LoadGlyphs;

  ImageAllChannels := CreateImage32(TabSheetRGBA);
  ImageRGBChannels := CreateImage32(TabSheetRGB);
  ImageAlphaChannel := CreateImage32(TabSheetAlpha);

  ImageAllChannels.Bitmap.DrawMode := dmBlend;

  LayerPixelGrid := TPixelGridLayer.Create(ImageAllChannels.Layers, ImageAllChannels);
  LayerPixelGrid.Visible := False;

{$IFDEF PLATFORM_INDEPENDENT}
  OpenDialog := TOpenDialog.Create(Self);
  SaveDialog := TSaveDialog.Create(Self);
{$ELSE}
  OpenDialog := TOpenPictureDialog.Create(Self);
  SaveDialog := TSavePictureDialog.Create(Self);
{$ENDIF}
  OpenDialog.Filter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True) +
    '|' + SDefaultFilter;
  SaveDialog.Filter := ImageFormatManager.BuildFileFilter(IImageFormatWriter) +
    '|' + SDefaultFilter;
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
  Bitmap: TBitmap32;
begin
  if not OpenDialog.Execute then
    exit;

  Bitmap := TBitmap32.Create(TMemoryBackend);
  try
    Bitmap.LoadFromFile(OpenDialog.Filename);
    LoadFromImage(Bitmap);
  finally
    Bitmap.Free;
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
    TAction(Sender).Enabled := ImageFormatManager.ClipboardFormats.CanPasteFromClipboard;
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

  if not SaveDialog.Execute then
    exit;

  if (CurrentImage = ImageAllChannels) or
    (not SameText(ExtractFileExt(SaveDialog.Filename), GraphicExtension(TBitmap))) then
    // Save in 32-bit RGBA bitmap (or whatever format we have chosen)
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

procedure TPictureEditorForm.ActionGridExecute(Sender: TObject);
begin
  LayerPixelGrid.Visible := TAction(Sender).Checked;
end;

procedure TPictureEditorForm.ActionGridUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := LayerPixelGrid.Visible;
end;

procedure TPictureEditorForm.ActionHasBitmapUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (CurrentImage <> nil) and (not CurrentImage.Bitmap.Empty);
end;

procedure TPictureEditorForm.ActionHelpExecute(Sender: TObject);
begin
  ShowMessage(sHelp);
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

procedure TPictureEditorForm.ImageChanged(Sender: TObject);
begin
  SyncZoomAndPan;
end;

procedure TPictureEditorForm.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Button = mbMiddle) then
    ResetZoomAndCenter(TImage32(Sender));
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

  if (Image.IsMousePanning) then
    exit;

  if (Image.Bitmap = nil) or (Image.Bitmap.Empty) then
  begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
    exit;
  end;

  P := Image.ControlToBitmap(GR32.Point(X, Y));

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
