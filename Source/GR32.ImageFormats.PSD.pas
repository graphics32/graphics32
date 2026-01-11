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

interface

{$include GR32.inc}

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
//      TPhotoshopLayerProperty
//
//------------------------------------------------------------------------------
// Represents "additional information" associated with a layer.
//------------------------------------------------------------------------------
type
  TCustomPhotoshopLayer = class;

  TCustomPhotoshopLayerProperty = class
  private
    FLayer: TCustomPhotoshopLayer;
    FKey: AnsiString;
  public
    constructor Create(ALayer: TCustomPhotoshopLayer; const AKey: AnsiString); virtual;
    property Key: AnsiString read FKey;
  end;

  TPhotoshopLayerPropertyClass = class of TCustomPhotoshopLayerProperty;

  TPhotoshopLayerProperty = class(TCustomPhotoshopLayerProperty)
  private
    FData: TBytes;
  public
    property Data: TBytes read FData write FData;
  end;


//------------------------------------------------------------------------------
//
//      TCustomPhotoshopLayer
//
//------------------------------------------------------------------------------
// Represents a single PSD layer
//------------------------------------------------------------------------------
  TPhotoshopDocument = class;

  TCustomPhotoshopLayer = class abstract
  private type
    TLayerProperties = TObjectList<TCustomPhotoshopLayerProperty>;
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
    FLayerProperties: TLayerProperties;
  protected
    procedure SetDocument(const Value: TPhotoshopDocument);
    function GetIndex: integer;
    procedure SetIndex(const Value: integer);
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);
    procedure SetCompression(const Value: TPSDLayerCompression);
    function GetCompression: TPSDLayerCompression;
    procedure SetUseDocumentCompression(const Value: boolean);
    function AddLayerProperty(APropertyClass: TPhotoshopLayerPropertyClass; const AKey: AnsiString): TCustomPhotoshopLayerProperty;

    procedure GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var ABytes); virtual; abstract;
    procedure SetChannelScanLine(AChannel: TColor32Component; ALine: integer; const ABytes); virtual; abstract;

  function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
    procedure SetHeight(const Value: Integer); virtual;
    procedure SetWidth(const Value: Integer); virtual;
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
    property Height: Integer read GetHeight write SetHeight;
    property Width: Integer read GetWidth write SetWidth;
    property LayerHeight: Integer read FHeight;
    property LayerWidth: Integer read FWidth;
    property Name: string read FName write FName;
    property BlendMode: TPSDLayerBlendMode read FBlendMode write FBlendMode;
    property Opacity: Byte read FOpacity write FOpacity;
    property Options: TPSDLayerOptions read FOptions write FOptions;
    property Clipping: boolean read FClipping write FClipping;
    property Compression: TPSDLayerCompression read GetCompression write SetCompression;
    property UseDocumentCompression: boolean read FUseDocumentCompression write SetUseDocumentCompression;
    // TODO : Replace LayerProperties with a proper list representation
    property LayerProperties: TLayerProperties read FLayerProperties;
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
    procedure AssignTo(Dest: TPersistent); override;
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

    // Background: A composite of the flattened image.
    // If the document contains no layers this is the primary image. Otherwise
    // it is usually just used as a preview of the image. Applications that
    // cannot handle layers will often just load this bitmap and ignore the
    // layers, while application that does handle layers will ignore the
    // background if the image contains layers. For this reason you should only
    // omit the background if you are sure that the reader will not require it.
    // If no background is specified then a fully transparent bitmap will be
    // saved in its place.
    property Background: TCustomPhotoshopLayer read FBackground write SetBackground;

    // Default background and layer compression. Initialized to the
    // value of DefaultCompression.
    property Compression: TPSDLayerCompression read FCompression write SetCompression;

    // DefaultLayerClass: The type of layer create when calling Layers.Add
    // with no layer type specified.
    class property DefaultLayerClass: TPhotoshopLayerClass read FDefaultLayerClass write FDefaultLayerClass;

    // DefaultCompression: The default background and layer compression used
    // if no explicit compression type is specified.
    class property DefaultCompression: TPSDLayerCompression read FDefaultCompression write FDefaultCompression;
  end;


//------------------------------------------------------------------------------
//
//      TCustomPhotoshopBitmapLayer32
//
//------------------------------------------------------------------------------
// Abstract layer with a TBitmap32
//------------------------------------------------------------------------------
type
  TCustomPhotoshopBitmapLayer32 = class abstract(TCustomPhotoshopLayer)
  private
    FSourceTop: integer;
    FSourceLeft: integer;
  protected
    procedure GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var ABytes); override;
    procedure SetChannelScanLine(AChannel: TColor32Component; ALine: integer; const ABytes); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetBitmap: TCustomBitmap32; virtual; abstract;
    function GetSourceRect: TRect;
    procedure SetSourceRect(const Value: TRect);

    property SourceTop: integer read FSourceTop write FSourceTop;
    property SourceLeft: integer read FSourceLeft write FSourceLeft;
  public
    property Bitmap: TCustomBitmap32 read GetBitmap;

    // SourceRect: The area of the bitmap used to produce the layer bitmap.
    // By default the whole bitmap is used, but SourceRect can be used to
    // only use a section of it.
    property SourceRect: TRect read GetSourceRect write SetSourceRect;
  end;


//------------------------------------------------------------------------------
//
//      TPhotoshopLayer32
//
//------------------------------------------------------------------------------
// Layer *wrapping* a TBitmap32
// Note that by default the layer only references the bitmap; It doesn't own it.
//------------------------------------------------------------------------------
type
  TPhotoshopLayer32 = class(TCustomPhotoshopBitmapLayer32)
  private
    FBitmap: TCustomBitmap32;
    FOwnsBitmap: boolean;
  protected
    function GetBitmap: TCustomBitmap32; override;
    procedure SetBitmap(const Value: TCustomBitmap32);
  public
    destructor Destroy; override;

    property Bitmap: TCustomBitmap32 read GetBitmap write SetBitmap;

    // OwnsBitmap: Specifies if the layers owns the bitmap referenced by
    // the Bitmap property. Default: False
    property OwnsBitmap: boolean read FOwnsBitmap write FOwnsBitmap;
  end;


//------------------------------------------------------------------------------
//
//      TPhotoshopBitmapLayer32
//
//------------------------------------------------------------------------------
// Layer *containing* a TBitmap32
// The layers owns the bitmap.
//------------------------------------------------------------------------------
type
  TPhotoshopBitmapLayer32 = class(TCustomPhotoshopBitmapLayer32)
  private
    FBitmap: TCustomBitmap32;
  protected
    function GetBitmap: TCustomBitmap32; override;
  public
    destructor Destroy; override;
  end;


//------------------------------------------------------------------------------
//
//      TPhotoshopPlaceholderLayer
//
//------------------------------------------------------------------------------
// Represents a PSD layer that doesn't have bitmap data, like an adjustment
// layer (um... nope).
//------------------------------------------------------------------------------
type
  TPhotoshopPlaceholderLayer = class(TCustomPhotoshopLayer)
  protected
    procedure GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var Bytes); override;
    procedure SetChannelScanLine(AChannel: TColor32Component; ALine: integer; const ABytes); override;
  end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TCustomImage32
//
//------------------------------------------------------------------------------
// The function produces a PSD where the background is the composite of the
// TCustomImage32 and its layers (i.e. a flattened view of the image) and one
// PSD layer for each bitmap layer in the TCustomImage32.
//
// If the TCustomImage32 does not have layers then the TCustomImage32.Bitmap
// will be exported as the "background" in a PSD with no layers, otherwise the
// Bitmap will be exported as a PSD layer.
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument); overload;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TBitmap32
//
//------------------------------------------------------------------------------
// The function produces a PSD with no layers but with a background based on the
// bitmap.
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument); overload;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  Types,
  GR32_Layers,
  GR32_Backends_Generic,
  GR32.ImageFormats,
  GR32.ImageFormats.PSD.Writer,
  GR32.ImageFormats.PSD.Reader;

const
{$if defined(DynArrayOps)}
  FileSignaturePsd: TBytes        = [$38, $42, $50, $53, $00, $01]; // '8BPS'#00#01;
  FileSignaturePsdMask: TBytes    = [$ff, $ff, $ff, $ff, $ff, $ff];
{$else}
  FileSignaturePsd: array[0..5] of byte     = ($38, $42, $50, $53, $00, $01); // '8BPS'#00#01;
  FileSignaturePsdMask: array[0..5] of byte = ($ff, $ff, $ff, $ff, $ff, $ff);
{$ifend}

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
    IImageFormatWriter,
    IImageFormatReader)
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
  private
    // IImageFormatReader
    function CanLoadFromStream(AStream: TStream): boolean;
    function LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
  end;


//------------------------------------------------------------------------------
// IImageFormatAdapter
//------------------------------------------------------------------------------
function TImageFormatAdapterPSD.CanAssignFrom(Source: TPersistent): boolean;
begin
  Result := False;
end;

function TImageFormatAdapterPSD.AssignFrom(Dest: TCustomBitmap32; Source: TPersistent): boolean;
var
  PSD: TPhotoshopDocument;
  i: Integer;
begin
  if (Source is TPhotoshopDocument) then
  begin
    PSD := TPhotoshopDocument(Source);

    Dest.SetSize(PSD.Width, PSD.Height);
    Dest.Clear;

    for i := 0 to PSD.Layers.Count - 1 do
      if PSD.Layers[i] is TCustomPhotoshopBitmapLayer32 then
        TCustomPhotoshopBitmapLayer32(PSD.Layers[i]).Bitmap.DrawTo(Dest, PSD.Layers[i].Left, PSD.Layers[i].Top);

    Result := True;
  end else
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
{$if defined(DynArrayOps)}
  Result := ['psd'];
{$else}
  MakeFileTypes(['psd']);
{$ifend}
end;

resourcestring
  sImageFormatPSDName = 'PSD images';

function TImageFormatAdapterPSD.ImageFormatDescription: string;
begin
  Result := sImageFormatPSDName;
end;

//------------------------------------------------------------------------------
// IImageFormatReader
//------------------------------------------------------------------------------
function TImageFormatAdapterPSD.CanLoadFromStream(AStream: TStream): boolean;
begin
  Result := CheckFileSignature(AStream, FileSignaturePsd, FileSignaturePsdMask);
end;

function TImageFormatAdapterPSD.LoadFromStream(ADest: TCustomBitmap32; AStream: TStream): boolean;
var
  PSD: TPhotoshopDocument;
  I: Integer;
begin
  if (not CanLoadFromStream(AStream)) then
    Exit(False);

  PSD := TPhotoshopDocument.Create;
  try
    PhotoshopDocumentReader.LoadFromStream(PSD, AStream);

    ADest.SetSize(PSD.Width, PSD.Height);
    ADest.Clear;

    for I := 0 to PSD.Layers.Count - 1 do
      if PSD.Layers[I] is TCustomPhotoshopBitmapLayer32 then
        TCustomPhotoshopBitmapLayer32(PSD.Layers[I]).Bitmap.DrawTo(ADest, PSD.Layers[I].Left, PSD.Layers[I].Top);
  finally
    PSD.Free;
  end;

  Result := True;
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
    PhotoshopDocumentWriter.SaveToStream(PSD, AStream);
  finally
    PSD.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TCustomImage32
//
//------------------------------------------------------------------------------
type
  TBitmapLayerCracker = class(TCustomIndirectBitmapLayer);

resourcestring
  sPSDLayerName = 'Layer %d';

procedure CreatePhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument);
var
  i: integer;
  ImageWidth, ImageHeight: integer;
  PSDLayer: TCustomPhotoshopLayer;
  SourceLayer: TCustomLayer;
  BackgroundBitmap: TBitmap32;
  Location: TFloatRect;
  LayerBitmap: TCustomBitmap32;
begin
  ADocument.Clear;

  if (AImage.Bitmap.Empty) and (AImage.Layers.Count = 0) then
    Exit;

  // Add the main bitmap as a layer
  if (not AImage.Bitmap.Empty) then
  begin
    PSDLayer := ADocument.Layers.Add(TPhotoshopLayer32);
    PSDLayer.Opacity := AImage.Bitmap.MasterAlpha;
    // Layer just references the bitmap; It doesn't own it.
    TPhotoshopLayer32(PSDLayer).Bitmap :=  AImage.Bitmap;

    PSDLayer.Name := Format(sPSDLayerName, [ADocument.Layers.Count]);
  end;

  for i := 0 to AImage.Layers.Count - 1 do
  begin
    SourceLayer := AImage.Layers[i];
    if not (SourceLayer is TCustomIndirectBitmapLayer) then
      continue;

    LayerBitmap := TBitmapLayerCracker(SourceLayer).Bitmap;
    Location := TBitmapLayerCracker(SourceLayer).Location;

    PSDLayer := ADocument.Layers.Add(TPhotoshopLayer32);
    PSDLayer.Opacity := LayerBitmap.MasterAlpha;
    PSDLayer.Left := Round(Location.Left);
    PSDLayer.Top := Round(Location.Top);
    // Layer just references the bitmap; It doesn't own it.
    TPhotoshopLayer32(PSDLayer).Bitmap :=  LayerBitmap;
    if (not SourceLayer.Visible) then
      PSDLayer.Options := PSDLayer.Options + [loHidden];

    PSDLayer.Name := Format(sPSDLayerName, [ADocument.Layers.Count]);
  end;

  BackgroundBitmap := TBitmap32.Create(TMemoryBackend);
  try
    if (AImage.Bitmap.Empty) then
    begin
      // The image has no bitmap - Calculate size from the layers instead
      ImageWidth := 0;
      ImageHeight := 0;
      for i := 0 to ADocument.Layers.Count - 1 do
      begin
        PSDLayer := ADocument.Layers[i];

        ImageWidth := Max(ImageWidth, PSDLayer.Left + PSDLayer.Width);
        ImageHeight := Max(ImageHeight, PSDLayer.Top + PSDLayer.Height);
      end;

      if (ImageWidth = 0) and (ImageHeight = 0) then
        exit;

      BackgroundBitmap.SetSize(ImageWidth, ImageHeight);
    end else
      BackgroundBitmap.SetSizeFrom(AImage.Bitmap);

    // We clear the background with:
    //
    //   $00xxxxxx to make it transparent for those that can handle transparent PSD
    //
    //   $xxFFFFFF to make it white for those that can't handle transparent PSD
    //
    // If the image contains layers and the reader can handle them then the
    // background is ignored; The background is only used when there are no
    // layers or if the reader cannot handle layers.
    BackgroundBitmap.Clear($00FFFFFF);

    // Create flattened bitmap for use as background
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

  finally
    BackgroundBitmap.Free;
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
//      TCustomPhotoshopLayerProperty
//
//------------------------------------------------------------------------------
constructor TCustomPhotoshopLayerProperty.Create(ALayer: TCustomPhotoshopLayer; const AKey: AnsiString);
begin
  FLayer := ALayer;
  FKey := AKey;
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
  FLayerProperties.Free;
  inherited;
end;

function TCustomPhotoshopLayer.AddLayerProperty(APropertyClass: TPhotoshopLayerPropertyClass; const AKey: AnsiString): TCustomPhotoshopLayerProperty;
begin
  if (FLayerProperties = nil) then
    FLayerProperties := TLayerProperties.Create;

  Result := APropertyClass.Create(Self, AKey);
  FLayerProperties.Add(Result);
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

function TCustomPhotoshopLayer.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TCustomPhotoshopLayer.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TCustomPhotoshopLayer.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TCustomPhotoshopLayer.SetWidth(const Value: Integer);
begin
  FWidth := Value;
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

procedure TPhotoshopDocument.AssignTo(Dest: TPersistent);
var
  Image: TCustomImage32;
  i: integer;
  PsdLayer: TCustomPhotoshopBitmapLayer32;
  BitmapLayer: TBitmapLayer;
begin
  if (Dest is TCustomImage32) then
  begin

    Image := TCustomImage32(Dest);
    Image.Bitmap.SetSize(0, 0);
    Image.Layers.Clear;

    if (Layers.Count > 0) then
    begin
      for i := 0 to Layers.Count-1 do
      begin
        PsdLayer := TCustomPhotoshopBitmapLayer32(Layers[i]);
        if (not (PsdLayer is TCustomPhotoshopBitmapLayer32)) then
          continue;

        if (i = 0) and (PsdLayer.Left = 0) and (PsdLayer.Top = 0) and (not PsdLayer.Bitmap.Empty) then
        begin
          // First layer is used as the background
          Image.Bitmap.Assign(PsdLayer.Bitmap);
          Image.Bitmap.DrawMode := dmBlend;
          Image.Bitmap.MasterAlpha := PsdLayer.Opacity;
        end else
        begin
          BitmapLayer := TBitmapLayer.Create(Image.Layers);
          BitmapLayer.Bitmap.Assign(PsdLayer.Bitmap);
          BitmapLayer.Bitmap.DrawMode := dmBlend;
          BitmapLayer.Bitmap.MasterAlpha := PsdLayer.Opacity;
          BitmapLayer.Location := GR32.FloatRect(PsdLayer.Left, PsdLayer.Top, PsdLayer.Left + PsdLayer.Width, PsdLayer.Top + PsdLayer.Height);
          BitmapLayer.Scaled := True; // Layers are relative to main bitmap
        end;
      end;
    end else
    begin
      Image.Bitmap.DrawMode := dmBlend;
      Image.Bitmap.MasterAlpha := 255;
    end;

  end else
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
  Width := AWidth;
  Height := AHeight;
end;


//------------------------------------------------------------------------------
//
//      TCustomPhotoshopBitmapLayer32
//
//------------------------------------------------------------------------------
procedure TCustomPhotoshopBitmapLayer32.GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var ABytes);
var
  Count: integer;
  pDest: PByte;
  pSource: PByte;
begin
  if (Width = 0) or (Height = 0) then
    Exit;

  if (Bitmap = nil) then
  begin
    FillChar(ABytes, Width, $FF);
    Exit;
  end;

  pDest := @ABytes;
  pSource := @(PColor32Entry(Bitmap.ScanLine[ALine + FSourceTop]).Components[AChannel]);
  Inc(pSource, FSourceLeft * SizeOf(TColor32));

  Count := Width;
  while (Count > 0) do
  begin
    pDest^ := pSource^;

    Inc(pDest);
    Inc(pSource, SizeOf(TColor32));
    Dec(Count);
  end;
end;

procedure TCustomPhotoshopBitmapLayer32.SetChannelScanLine(AChannel: TColor32Component; ALine: integer; const ABytes);
var
  Count: integer;
  pDest: PByte;
  pSource: PByte;
begin
  if (Bitmap = nil) then
    exit;

  pSource := @ABytes;
  pDest := @(PColor32Entry(Bitmap.ScanLine[ALine + FSourceTop]).Components[AChannel]);
  Inc(pSource, FSourceLeft * SizeOf(TColor32));

  Count := Width;
  while (Count > 0) do
  begin
    pDest^ := pSource^;

    Inc(pDest, SizeOf(TColor32));
    Inc(pSource);
    Dec(Count);
  end;
end;

function TCustomPhotoshopBitmapLayer32.GetSourceRect: TRect;
begin
  if (Bitmap <> nil) then
  begin
    Result.Top :=  Min(FSourceTop, Bitmap.Height);
    Result.Left :=  Min(FSourceLeft, Bitmap.Width);
  end else
  begin
    Result.Top :=  0;
    Result.Left :=  0;
  end;
  Result.Width := Width;
  Result.Height := Height;
end;

function TCustomPhotoshopBitmapLayer32.GetHeight: Integer;
begin
  // Size of bitmap can have changed since assignment
  // so we need to reevaluate the size.
  if (Bitmap <> nil) then
    Result := Min(inherited GetHeight, Max(0, Bitmap.Height - FSourceTop))
  else
    Result := 0;
end;

function TCustomPhotoshopBitmapLayer32.GetWidth: Integer;
begin
  // Size of bitmap can have changed since assignment
  // so we need to reevaluate the size.
  if (Bitmap <> nil) then
    Result := Min(inherited GetWidth, Max(0, Bitmap.Width - FSourceLeft))
  else
    Result := 0;
end;

procedure TCustomPhotoshopBitmapLayer32.SetSourceRect(const Value: TRect);
var
  SourceRect: TRect;
begin
  if (Bitmap <> nil) then
    GR32.IntersectRect(SourceRect, Value, Bitmap.BoundsRect)
  else
  begin
    SourceRect.Top := Max(0, Value.Top);
    SourceRect.Left := Max(0, Value.Left);
    SourceRect.Bottom := Max(SourceRect.Top, Value.Top);
    SourceRect.Right := Max(SourceRect.Left, Value.Left);
  end;

  FSourceTop := SourceRect.Top;
  FSourceLeft := SourceRect.Left;

  Width := SourceRect.Width;
  Height := SourceRect.Height;
end;

//------------------------------------------------------------------------------
//
//      TPhotoshopLayer32
//
//------------------------------------------------------------------------------
destructor TPhotoshopLayer32.Destroy;
begin
  if (FOwnsBitmap) then
    FBitmap.Free;

  inherited;
end;

function TPhotoshopLayer32.GetBitmap: TCustomBitmap32;
begin
  Result := FBitmap;
end;

procedure TPhotoshopLayer32.SetBitmap(const Value: TCustomBitmap32);
begin
  if (FOwnsBitmap) and (FBitmap <> nil) then
    FBitmap.Free;

  FBitmap := Value;

  SourceTop := 0;
  SourceLeft := 0;

  if (FBitmap <> nil) then
  begin
    Height := FBitmap.Height;
    Width := FBitmap.Width;
  end else
  begin
    Height := 0;
    Width := 0;
  end;
end;


//------------------------------------------------------------------------------
//
//      TPhotoshopBitmapLayer32
//
//------------------------------------------------------------------------------
destructor TPhotoshopBitmapLayer32.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

function TPhotoshopBitmapLayer32.GetBitmap: TCustomBitmap32;
begin
  if (FBitmap = nil) then
  begin
    FBitmap := TBitmap32.Create(LayerWidth, LayerHeight);
    FBitmap.Clear;
  end;
  Result := FBitmap;
end;


//------------------------------------------------------------------------------
//
//      TPhotoshopPlaceholderLayer
//
//------------------------------------------------------------------------------
procedure TPhotoshopPlaceholderLayer.GetChannelScanLine(AChannel: TColor32Component; ALine: integer; var Bytes);
begin
  // A placeholder layer does not contain any bitmap data so we
  // just return an empty scanline.
  FillChar(Bytes, Width, 0);
end;

procedure TPhotoshopPlaceholderLayer.SetChannelScanLine(AChannel: TColor32Component; ALine: integer; const ABytes);
begin
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ImageFormatHandle: integer = 0;

initialization
  TPhotoshopDocument.DefaultLayerClass := TCustomPhotoshopBitmapLayer32;
  TPhotoshopDocument.DefaultCompression := lcRLE;

  ImageFormatHandle := ImageFormatManager.RegisterImageFormat(TImageFormatAdapterPSD.Create, ImageFormatPriorityNormal);
finalization
  ImageFormatManager.UnregisterImageFormat(ImageFormatHandle);
end.

