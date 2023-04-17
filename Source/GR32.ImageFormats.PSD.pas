unit GR32.ImageFormats.PSD;

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
 * The Original Code is PSD Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Lamdalili
 *
 * Portions created by the Initial Developer are Copyright (C) 2023
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Anders Melander <anders@melander.dk>
 *
 * ***** END LICENSE BLOCK ***** *)

// WEAKPACKAGEUNIT so we can include the unit in the GR32 design time
// package in order to have the design time editor support the various formats.
{$WEAKPACKAGEUNIT ON}

interface

{$I GR32.inc}

uses
  Generics.Collections,
  Classes,
  SysUtils,
  GR32,
  GR32_Image;

//------------------------------------------------------------------------------
//
//      PSD simple types
//
//------------------------------------------------------------------------------
type
  TPSDLayerBlendMode = (
    lbmPass,
    lbmNormal,
    lbmDarken,
    lbmLighten,
    lbmHue,
    lbmSaturation,
    lbmColor,
    lbmLuminosity,
    lbmMultiply,
    lbmScreen,
    lbmDissolve,
    lbmOverlay,
    lbmHardLight,
    lbmSoftLight,
    lbmDifference,
    lbmExclusion,
    lbmColorDodge,
    lbmColorBurn,
    lbmLinearLight,
    lbmLinearBurn,
    lbmDarkerColor,
    lbmLinearDodge,
    lbmPinLight,
    lbmVividLight,
    lbmHardMix,
    lbmLighterColor,
    lbmSubtract,
    lbmDivide
  );

  TPSDLayerCompression = (
    lcRAW,
    lcRLE,
    lcZIP,
    lcPredictedZIP
  );

  TPSDLayerOptions = set of (
    loTransparencyProtected,
    loHidden,
    loIrrelevantData,
    loFlag3,
    loFlag4
  );


type
  EPhotoshopDocument = class(Exception);

//------------------------------------------------------------------------------
//
//      TCustomPhotoshopLayer
//
//------------------------------------------------------------------------------
// Represents a single PSD layer
//------------------------------------------------------------------------------
type
  TPhotoshopDocument = class;

  TCustomPhotoshopLayer = class abstract
  private
    FDocument: TPhotoshopDocument;
    FTop: integer;
    FLeft: integer;
    FHeight: integer;
    FWidth: integer;
    FName: string;
    FBlendMode: TPSDLayerBlendMode;
    FOpacity: Byte;
    FOptions: TPSDLayerOptions;
    FClipping: boolean;
    FCompression: TPSDLayerCompression;
    FUseDocumentCompression: boolean;
  protected
    procedure SetDocument(const Value: TPhotoshopDocument);
    function GetIndex: integer;
    procedure SetIndex(const Value: integer);
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);
    procedure SetCompression(const Value: TPSDLayerCompression);
    function GetCompression: TPSDLayerCompression;
    procedure SetUseDocumentCompression(const Value: boolean);

    procedure GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var Bytes); virtual; abstract;
  public
    constructor Create(ADocument: TPhotoshopDocument = nil); virtual;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight :integer);

    procedure BeginScan; virtual;
    procedure EndScan; virtual;

    property Document: TPhotoshopDocument read FDocument write SetDocument;
    property Index: integer read GetIndex write SetIndex;

    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
    property Height: integer read FHeight write FHeight;
    property Width: integer read FWidth write FWidth;
    property Name: string read FName write FName;
    property BlendMode: TPSDLayerBlendMode read FBlendMode write FBlendMode;
    property Opacity: Byte read FOpacity write FOpacity;
    property Options: TPSDLayerOptions read FOptions write FOptions;
    property Clipping: boolean read FClipping write FClipping;
    property Compression: TPSDLayerCompression read GetCompression write SetCompression;
    property UseDocumentCompression: boolean read FUseDocumentCompression write SetUseDocumentCompression;
  end;

  TPhotoshopLayerClass = class of TCustomPhotoshopLayer;

//------------------------------------------------------------------------------
//
//      TPhotoshopDocument
//
//------------------------------------------------------------------------------
// Represents a PSD document/file, (typically) containing one or more layers
//------------------------------------------------------------------------------
  TPhotoshopDocument = class(TPersistent)
  private type

    TPhotoshopLayers = class
    private
      FDocument: TPhotoshopDocument;
      FLayers: TObjectList<TCustomPhotoshopLayer>;
    protected
      function GetCount: integer;
      function GetLayer(Index: integer): TCustomPhotoshopLayer;
      procedure AddLayer(ALayer: TCustomPhotoshopLayer);
      procedure RemoveLayer(ALayer: TCustomPhotoshopLayer);
      function IndexOf(ALayer: TCustomPhotoshopLayer): integer;
      procedure Move(OldIndex, NewIndex: integer);
    public
      constructor Create(ADocument: TPhotoshopDocument);
      destructor Destroy; override;

      function Add(ALayerClass: TPhotoshopLayerClass = nil): TCustomPhotoshopLayer;
      procedure Clear;

      property Count: integer read GetCount;
      property Layers[Index: integer]: TCustomPhotoshopLayer read GetLayer; default;
    end;

  private
    FLayers: TPhotoshopLayers;
    FWidth: Integer;
    FHeight: Integer;
    FBackground: TCustomPhotoshopLayer;
    FCompression: TPSDLayerCompression;
  private
    class var
      FDefaultLayerClass: TPhotoshopLayerClass;
      FDefaultCompression: TPSDLayerCompression;
  protected
    procedure SetBackground(const Value: TCustomPhotoshopLayer);
    procedure SetCompression(const Value: TPSDLayerCompression);
    procedure AddLayer(ALayer: TCustomPhotoshopLayer);
    procedure RemoveLayer(ALayer: TCustomPhotoshopLayer);
  public
    constructor Create(ABackground: TCustomPhotoshopLayer = nil);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    procedure SetSize(AWidth, AHeight: Integer);

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;

    // Layers: The individual PSD layers
    property Layers: TPhotoshopLayers read FLayers;
    // Background: An optional preview of the flattened image.
    property Background: TCustomPhotoshopLayer read FBackground write SetBackground;

    // Default background and layer compression. Initialized to the
    // value of DefaultCompression.
    property Compression: TPSDLayerCompression read FCompression write SetCompression;

    class property DefaultLayerClass: TPhotoshopLayerClass read FDefaultLayerClass write FDefaultLayerClass;
    class property DefaultCompression: TPSDLayerCompression read FDefaultCompression write FDefaultCompression;
  end;


//------------------------------------------------------------------------------
//
//      TPhotoshopLayer32
//
//------------------------------------------------------------------------------
// Layer wrapping a TBitmap32
// Note that the layer only references the bitmap; It doesn't own it.
//------------------------------------------------------------------------------
type
  TPhotoshopLayer32 = class(TCustomPhotoshopLayer)
  private
    FBitmap: TCustomBitmap32;
    FOwnsBitmap: boolean;
  protected
    procedure GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var Bytes); override;
    procedure SetBitmap(const Value: TCustomBitmap32);
  public
    destructor Destroy; override;

    property Bitmap: TCustomBitmap32 read FBitmap write SetBitmap;
    property OwnsBitmap: boolean read FOwnsBitmap write FOwnsBitmap;
  end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TCustomImage32
//
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument); overload;

//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TBitmap32
//
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument); overload;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  ZLib,
  Math,
  GR32_Layers,
  GR32.ImageFormats,
  GR32.ImageFormats.PSD.Writer;

const
  PsdSignature: AnsiString        = '8BPS'#00#01;
  PsdSignatureMask: AnsiString    = #$ff#$ff#$ff#$ff#$ff#$ff;

//------------------------------------------------------------------------------
//
//      TImageFormatAdapterPSD
//
//------------------------------------------------------------------------------
// Implements IImageFormatAdapter for the PSD image format using
// TPhotoshopDocument.
//------------------------------------------------------------------------------
type
  TImageFormatAdapterPSD = class(TCustomImageFormatAdapter,
    IImageFormatAdapter,
    IImageFormatFileInfo,
    IImageFormatWriter)
  strict protected
    // IImageFormatAdapter
    function CanAssignFrom(Source: TPersistent): boolean; override;
    function AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean; override;
    function CanAssignTo(Dest: TPersistent): boolean; override;
    function AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean; override;
  private
    // IImageFormatFileInfo
    function ImageFormatDescription: string;
    function ImageFormatFileTypes: TFileTypes;
  private
    // IImageFormatWriter
    procedure SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
  end;


//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterPSD.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := False;
end;

function TImageFormatAdapterPSD.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
begin
  Result := inherited;
end;

//------------------------------------------------------------------------------

function TImageFormatAdapterPSD.CanAssignTo(Dest: TPersistent): boolean;
begin
  Result := (Dest is TPhotoshopDocument);
end;

function TImageFormatAdapterPSD.AssignTo(Source: TCustomBitmap32; Dest: TPersistent): boolean;
begin
  if (Dest is TPhotoshopDocument) then
  begin
    CreatePhotoshopDocument(Source, TPhotoshopDocument(Dest));
    Result := True;
  end else
    Result := inherited;
end;

//------------------------------------------------------------------------------
// IImageFormatFileInfo
//------------------------------------------------------------------------------

function TImageFormatAdapterPSD.ImageFormatFileTypes: TFileTypes;
begin
  Result := ['psd'];
end;

function TImageFormatAdapterPSD.ImageFormatDescription: string;
resourcestring
  sImageFormatPSDName = 'PSD images';
begin
  Result := sImageFormatPSDName;
end;

//------------------------------------------------------------------------------
// IImageFormatWriter
//------------------------------------------------------------------------------
procedure TImageFormatAdapterPSD.SaveToStream(ASource: TCustomBitmap32; AStream: TStream);
var
  PSD: TPhotoshopDocument;
begin
  PSD := TPhotoshopDocument.Create;
  try
    CreatePhotoshopDocument(ASource, PSD);
    TPhotoshopDocumentWriter.SaveToStream(PSD, AStream);
  finally
    PSD.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TCustomImage32
//
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument);
var
  PSDLayer: TCustomPhotoshopLayer;
  i: integer;
  SourceLayer: TCustomLayer;
  BackgroundBitmap: TBitmap32;
  Location: TFloatRect;
  LayerBitmap: TBitmap32;
begin
  ADocument.Clear;

  if AImage.Bitmap.Empty then
    Exit;

  BackgroundBitmap := TBitmap32.Create;
  try
    // Create flattened bitmap for use as background
    BackgroundBitmap.SetSizeFrom(AImage.Bitmap);
    AImage.PaintTo(BackgroundBitmap, BackgroundBitmap.BoundsRect);

    PSDLayer := TPhotoshopLayer32.Create;
    try

      TPhotoshopLayer32(PSDLayer).Bitmap := BackgroundBitmap;

      // We need to keep the bitmap alive when this function
      // returns so transfer ownership to the layer.
      TPhotoshopLayer32(PSDLayer).OwnsBitmap := True;
      BackgroundBitmap := nil;

      ADocument.Background := PSDLayer; // Document now owns the layer

    except
      PSDLayer.Free;
      raise;
    end;

  except
    BackgroundBitmap.Free;
    raise;
  end;

  for i := 0 to AImage.Layers.Count -1 do
  begin
    SourceLayer := AImage.Layers[i];
    if not (SourceLayer is TBitmapLayer) then
      continue;

    LayerBitmap := TBitmapLayer(SourceLayer).Bitmap;
    Location := TBitmapLayer(SourceLayer).Location;

    PSDLayer := ADocument.Layers.Add(TPhotoshopLayer32);
    PSDLayer.Opacity := LayerBitmap.MasterAlpha;
    PSDLayer.Left := Round(Location.Left);
    PSDLayer.Top := Round(Location.Top);
    // Layer just references the bitmap; It doesn't own it.
    TPhotoshopLayer32(PSDLayer).Bitmap :=  LayerBitmap;
    if (not SourceLayer.Visible) then
      PSDLayer.Options := PSDLayer.Options + [loHidden];

    PSDLayer.Name := Format('Layer %d', [i+1]);
  end;
end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TBitmap32
//
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument); overload;
var
  PSDLayer: TCustomPhotoshopLayer;
begin
  ADocument.Clear;

  if ABitmap.Empty then
    Exit;

  PSDLayer := TPhotoshopLayer32.Create;
  try

    TPhotoshopLayer32(PSDLayer).Bitmap := ABitmap;
    ADocument.Background := PSDLayer; // Document now owns the layer

  except
    PSDLayer.Free;
    raise;
  end;
end;



//------------------------------------------------------------------------------
//
//      TCustomPhotoshopLayer
//
//------------------------------------------------------------------------------

constructor TCustomPhotoshopLayer.Create(ADocument: TPhotoshopDocument);
begin
  inherited Create;
  FBlendMode := lbmNormal;
  FOpacity := $FF;
  FUseDocumentCompression := True;
  SetDocument(ADocument);
end;

destructor TCustomPhotoshopLayer.Destroy;
begin
  if (FDocument <> nil) and (FDocument.Background = Self) then
    FDocument.FBackground := nil; // Do not go through setter
  SetDocument(nil);
  inherited;
end;

procedure TCustomPhotoshopLayer.BeginScan;
begin
end;

procedure TCustomPhotoshopLayer.EndScan;
begin
end;

function TCustomPhotoshopLayer.GetBoundsRect: TRect;
begin
  Result := Rect(Left, Top, Left+Width, Top+Height);
end;

function TCustomPhotoshopLayer.GetCompression: TPSDLayerCompression;
begin
  if (FUseDocumentCompression) and (FDocument <> nil) then
    Result := FDocument.Compression
  else
    Result := FCompression;
end;

function TCustomPhotoshopLayer.GetIndex: integer;
begin
  if (FDocument <> nil) and (FDocument.Background <> Self) then
    Result := FDocument.FLayers.IndexOf(Self)
  else
    Result := -1;
end;

procedure TCustomPhotoshopLayer.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  Left := ALeft;
  Top := ATop;
  Width := AWidth;
  Height := AHeight;
end;

procedure TCustomPhotoshopLayer.SetBoundsRect(const Value: TRect);
begin
  SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);
end;

procedure TCustomPhotoshopLayer.SetCompression(const Value: TPSDLayerCompression);
begin
  if (Value = lcPredictedZIP) then
    raise EPhotoshopDocument.Create('"ZIP with prediction"-compression is not implemented');
  FCompression := Value;
  FUseDocumentCompression := False;
end;

procedure TCustomPhotoshopLayer.SetDocument(const Value: TPhotoshopDocument);
begin
  if (FDocument = Value) then
    exit;

  if (FDocument <> nil) then
    FDocument.RemoveLayer(Self);

  FDocument := Value;

  if (FDocument <> nil) then
    FDocument.AddLayer(Self);
end;

procedure TCustomPhotoshopLayer.SetIndex(const Value: integer);
begin
  if (Value <> Index) and (FDocument <> nil) then
  begin
    if (FDocument.Background = Self) then
      raise EPhotoshopDocument.Create('Cannot set the index of the background layer');

    FDocument.FLayers.Move(Index, Value);
  end;
end;

procedure TCustomPhotoshopLayer.SetUseDocumentCompression(const Value: boolean);
begin
  FUseDocumentCompression := Value;
end;


//------------------------------------------------------------------------------
//
//      TPhotoshopDocument.TPhotoshopLayers
//
//------------------------------------------------------------------------------
constructor TPhotoshopDocument.TPhotoshopLayers.Create(ADocument: TPhotoshopDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FLayers := TObjectList<TCustomPhotoshopLayer>.Create;
end;

destructor TPhotoshopDocument.TPhotoshopLayers.Destroy;
begin
  FLayers.Free;
  inherited;
end;

procedure TPhotoshopDocument.TPhotoshopLayers.Clear;
begin
  FLayers.Clear;
end;

procedure TPhotoshopDocument.TPhotoshopLayers.AddLayer(ALayer: TCustomPhotoshopLayer);
begin
  if (not FLayers.Contains(ALayer)) then
    FLayers.Add(ALayer);
end;

procedure TPhotoshopDocument.TPhotoshopLayers.RemoveLayer(ALayer: TCustomPhotoshopLayer);
begin
  FLayers.Extract(ALayer);
end;

function TPhotoshopDocument.TPhotoshopLayers.Add(ALayerClass: TPhotoshopLayerClass): TCustomPhotoshopLayer;
begin
  if (ALayerClass = nil) then
    ALayerClass := FDocument.DefaultLayerClass;

  // Layer.Create->Layer.SetDocument->Document.AddLayer->Document.Layers.AddLayer
  Result := ALayerClass.Create(FDocument);
end;

function TPhotoshopDocument.TPhotoshopLayers.GetCount: integer;
begin
  Result := FLayers.Count;
end;

function TPhotoshopDocument.TPhotoshopLayers.GetLayer(Index: integer): TCustomPhotoshopLayer;
begin
  Result := FLayers[Index];
end;

function TPhotoshopDocument.TPhotoshopLayers.IndexOf(ALayer: TCustomPhotoshopLayer): integer;
begin
  Result := FLayers.IndexOf(ALayer);
end;

procedure TPhotoshopDocument.TPhotoshopLayers.Move(OldIndex, NewIndex: integer);
begin
  FLayers.Move(OldIndex, NewIndex);
end;

//------------------------------------------------------------------------------
//
//      TPhotoshopDocument
//
//------------------------------------------------------------------------------
constructor TPhotoshopDocument.Create(ABackground: TCustomPhotoshopLayer);
begin
  inherited Create;
  FLayers := TPhotoshopLayers.Create(Self);
  FCompression := FDefaultCompression;
  FBackground := ABackground;
end;

destructor TPhotoshopDocument.Destroy;
begin
  FBackground.Free;
  FLayers.Free;
  inherited;
end;

procedure TPhotoshopDocument.Assign(Source: TPersistent);
begin
  if (Source is TCustomImage32) then
    CreatePhotoshopDocument(TCustomImage32(Source), Self)
  else
    inherited;
end;

procedure TPhotoshopDocument.Clear;
begin
  SetBackground(nil);
  FLayers.Clear;
  SetSize(0, 0);
end;

procedure TPhotoshopDocument.AddLayer(ALayer: TCustomPhotoshopLayer);
begin
  if (ALayer.Document = Self) and (ALayer <> Background) then
    FLayers.AddLayer(ALayer);
end;

procedure TPhotoshopDocument.RemoveLayer(ALayer: TCustomPhotoshopLayer);
begin
  if (ALayer.Document = Self) then
  begin
    if (ALayer <> Background) then
      FLayers.RemoveLayer(ALayer)
    else
      FBackground := nil;
  end;
end;

procedure TPhotoshopDocument.SetBackground(const Value: TCustomPhotoshopLayer);
begin
  if (FBackground = Value) then
    Exit;

  FBackground.Free;

  FBackground := Value;

  if FBackground <> nil then
  begin
    // In case layer is already in layer list this extracts it...
    FBackground.Document := nil;
    // ...and reattaches it without adding it to the layer list
    FBackground.Document := Self;

    FWidth := FBackground.Width;
    FHeight := FBackground.Height;
  end;
end;

procedure TPhotoshopDocument.SetCompression(const Value: TPSDLayerCompression);
begin
  if (Value = lcPredictedZIP) then
    raise EPhotoshopDocument.Create('"ZIP with prediction"-compression is not implemented');
  FCompression := Value;
end;

procedure TPhotoshopDocument.SetSize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;


//------------------------------------------------------------------------------
//
//      TPhotoshopLayer32
//
//------------------------------------------------------------------------------
procedure TPhotoshopLayer32.SetBitmap(const Value: TCustomBitmap32);
begin
  if (FOwnsBitmap) and (FBitmap <> nil) then
    FBitmap.Free;

  FBitmap := Value;

  if (FBitmap <> nil) then
  begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
  end;
end;

destructor TPhotoshopLayer32.Destroy;
begin
  if (FOwnsBitmap) then
    FBitmap.Free;

  inherited;
end;

procedure TPhotoshopLayer32.GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var Bytes);
var
  Count: integer;
  pDest: PByte;
  pSource: PByte;
begin
  if (Bitmap = nil) then
  begin
    FillChar(Bytes, Width, $FF);
    Exit;
  end;

  Assert(Width = Bitmap.Width);

  pSource := @(PColor32Entry(Bitmap.ScanLine[ALine]).Components[AChannel]);
  pDest := @Bytes;

  Count := Width;
  while (Count > 0) do
  begin
    pDest^ := pSource^;

    Inc(pDest);
    Inc(pSource, SizeOf(TColor32));
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TPhotoshopDocument.DefaultLayerClass := TPhotoshopLayer32;
  TPhotoshopDocument.DefaultCompression := lcRLE;

  ImageFormatManager.RegisterImageFormat(TImageFormatAdapterPSD.Create, ImageFormatPriorityNormal);
end.

