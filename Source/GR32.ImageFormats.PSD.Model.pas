unit GR32.ImageFormats.PSD.Model;

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
    loTransparencyProtected,    // If set, layer transparency is protected
    loHidden,                   // If set, the layer is not visible
    loObsolete,                 // Option is obsolete
    loIgnorePixelDataValid,     // If set, the loIgnorePixelData option is valid
    loIgnorePixelData           // If set, the pixel data is not for display
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
    procedure SetChannelScanlinePixel(AChannel: TColor32Component; ALine, AOffset: integer; AByte: byte); virtual; abstract;

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
      function GetEnumerator: TEnumerator<TCustomPhotoshopLayer>;

      property Count: integer read GetCount;
      property Layers[Index: integer]: TCustomPhotoshopLayer read GetLayer; default;
    end;

  private
    FLayers: TPhotoshopLayers;
    FWidth: Integer;
    FHeight: Integer;
    FBackground: TCustomPhotoshopLayer;
    FCompression: TPSDLayerCompression;
    FChannels: integer;
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

    // Number of channels in document.
    // Set by the PSD reader. The writer always uses a value of 4 (R, G, B, and A)
    property Channels: integer read FChannels write FChannels;

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
    procedure SetChannelScanlinePixel(AChannel: TColor32Component; ALine, AOffset: integer; AByte: byte); override;
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
    procedure SetChannelScanlinePixel(AChannel: TColor32Component; ALine, AOffset: integer; AByte: byte); override;
  end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TCustomImage32
//
//------------------------------------------------------------------------------
// This function produces a PSD from a TCustomImage32 bitmap and its layers.
// The function can optionally construct a background image from a composite of
// the TCustomImage32 bitmap and its layers (a flattened view of the image).
//------------------------------------------------------------------------------
type
  TPhotoshopExportOptions = set of (
    peCompositeAsLayer,         // If the image has a bitmap or more than one
                                // layer, a composite of the bitmap and all
                                // layers will be saved as the first layer.

    peCompositeAsBackground,    // Save a composite of the image (bitmap and all
                                // layers) as the PSD background image.

    peBackgroundAsLayer,        // TCustomImage32.Bitmap is saved as a layer.

    peSingleAsLayer,            // Normally, if a document has a single bitmap,
                                // this bitmap is saved as the PSD background.
                                // This option disables that behavior and forces
                                // the single bitmap to be saved as a PSD layer;
                                // piSingleAsLayer will be used if the image
                                // has a bitmap and no layers, or no bitmap and
                                // a single layer.
                                // The option takes precedence over the
                                // peComposite* options meaning that if the
                                // option is not specified, and the image only
                                // contains a single bitmap, then this bitmap
                                // will be saved as the PSD background image.

    peIgnoreBitmap              // Do not export the image bitmap.

  );

procedure CreatePhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopExportOptions = []); overload;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TBitmap32
//
//------------------------------------------------------------------------------
// The function produces a PSD from a single bitmap.
// The bitmap will either become:
//  1) The PSD background image.
//     This format is chosen if the peSingleAsLayer option is not set.
//  2) A layer in the PSD.
//     This format is chosen if the piSingleAsLayer option is set.
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopExportOptions = []); overload;


//------------------------------------------------------------------------------
//
//      Load a TPhotoshopDocument into a TCustomImage32
//
//------------------------------------------------------------------------------
type
  TPhotoshopImportOptions = set of (
    piBackgroundFromFirstLayer, // First visible layer will be imported as the
                                // image background, if the layer isn't empty
                                // and is positioned at (0, 0).

    piBackgroundIsComposite,    // The background image is a composite of all
                                // visible layers.

    piBackgroundIsOpaque        // The background composite is an opaque blend
                                // of all visible layers onto a background
                                // cleared with the specified background color.
                                // Otherwise, the background composite is a
                                // transparent merge of all visible layers.
  );

procedure LoadImageFromPhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopImportOptions = []; ABackground: TColor32 = clBlack32);


//------------------------------------------------------------------------------
//
//      Load a TPhotoshopDocument into a TCustomBitmap32
//
//------------------------------------------------------------------------------
// The result bitmap is either a composite of all the layers in the document or
// the bitmap of the first visible layer in the bitmap, depending on the
// specified options.
//
// The options are interpreted slightly different from that of the
// TCustomImage32 import:
//
//   piBackgroundFromFirstLayer: The first visible layer bitmap is imported.
//                               If the options isn't set then a composite of
//                               all visible layers is returned.
//
//   piBackgroundIsComposite:    This option is ignored.
//
//   piBackgroundIsOpaque:       If set, the returned bitmap is first cleared
//                               with the specified background color, and then
//                               the visible layers are blended onto it.
//                               Otherwise the returned bitmap is a transparent
//                               merge of all visible layers.
//------------------------------------------------------------------------------
procedure LoadBitmapFromPhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopImportOptions = []; ABackground: TColor32 = clBlack32);


//------------------------------------------------------------------------------

var
  // Default options used when exporting via TPhotoshopDocument.Assign(TCustomBitmap32),
  // or TPhotoshopDocument.Assign(TCustomImage32)
  DefaultPhotoshopExportOptions: TPhotoshopExportOptions = [peCompositeAsBackground];

var
  // Default options used when importing via TCustomBitmap32.Assign(TPhotoshopDocument),
  // TCustomImage32.Assign(TPhotoshopDocument), and TCustomBitmap32.LoadFromStream
  DefaultPhotoshopImportOptions: TPhotoshopImportOptions = [piBackgroundIsComposite];
  DefaultPhotoshopImportBackground: TColor32 = clBlack32;


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
  GR32.ImageFormats.PSD.Types,
  GR32.ImageFormats.PSD.Writer,
  GR32.ImageFormats.PSD.Reader;

//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TCustomImage32
//
//------------------------------------------------------------------------------
type
  TBitmapLayerCracker = class(TCustomIndirectBitmapLayer);

resourcestring
  sPSDLayerName = 'Layer %d';
  sPSDBackgroundName = 'Background';

procedure CreatePhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopExportOptions);
var
  SourceLayer: TCustomLayer;
  BitmapLayer: TCustomIndirectBitmapLayer;
  AnyBitmapLayers: boolean;
  PhotoshopLayer: TCustomPhotoshopBitmapLayer32;
  MainBitmap: TCustomBitmap32;
  CompositeBitmap: TBitmap32;
  Location: TFloatRect;
  LayerBitmap: TCustomBitmap32;
begin
  ADocument.Clear;

  // Does the image only have a single bitmap?
  BitmapLayer := nil;
  AnyBitmapLayers := False;
  for SourceLayer in AImage.Layers do
  begin
    if not (SourceLayer is TCustomIndirectBitmapLayer) then
      continue;

    AnyBitmapLayers := True;

    // Verify that the layer can be represented by a background image
    if (not SourceLayer.Visible) or (TCustomIndirectBitmapLayer(SourceLayer).Bitmap.MasterAlpha <> 255) or
      (TPositionedLayer(SourceLayer).Location.Left <> 0) or (TPositionedLayer(SourceLayer).Location.Top <> 0) then
    begin
      // Nope; All those properties would be lost
      BitmapLayer := nil;
      break;
    end;

    if (BitmapLayer <> nil) then
    begin
      // We already had a bitmap layer; Clear to indicate that there isn't a single bitmap layer
      BitmapLayer := nil;
      break;
    end else
      BitmapLayer := TCustomIndirectBitmapLayer(SourceLayer);
  end;

  if (peIgnoreBitmap in AOptions) or (AImage.Bitmap.Empty) then
    MainBitmap := nil
  else
    MainBitmap := AImage.Bitmap;

  if (MainBitmap = nil) and (not AnyBitmapLayers) then
    // No bitmaps; Nothing to save
    Exit;

  (*
  ** Single bitmap
  *)
  if (MainBitmap <> nil) xor (BitmapLayer <> nil) then
  begin
    if (BitmapLayer <> nil) then
      // No bitmap but a single bitmap layer
      LayerBitmap := BitmapLayer.Bitmap
    else
      // A bitmap and no bitmap layers
      LayerBitmap := MainBitmap;

    PhotoshopLayer := TPhotoshopLayer32.Create;
    TPhotoshopLayer32(PhotoshopLayer).Bitmap := LayerBitmap;

    if (peSingleAsLayer in AOptions) then
    begin
      PhotoshopLayer.Document := ADocument;
      PhotoshopLayer.Name := Format(sPSDLayerName, [ADocument.Layers.Count]);
    end else
    begin
      ADocument.Background := PhotoshopLayer;
      PhotoshopLayer.Name := sPSDBackgroundName;
    end;

    exit; // Done
  end;

  (*
  ** Composite image
  *)
  if ([peCompositeAsLayer, peCompositeAsBackground] * AOptions <> []) then
  begin
    CompositeBitmap := nil;
    try

      // Construct composite
      CompositeBitmap := TBitmap32.Create(TMemoryBackend);

      if (MainBitmap = nil) then
      begin
        // The image has no bitmap - Calculate size from the layers instead
        Location := FloatRect(0,0,0,0);

        for SourceLayer in AImage.Layers do
        begin
          // TODO : This will also consider design layers; We need a TCustomLayer.IsDesign. TCustomImage32.PaintTo should also take this account.
          if not (SourceLayer is TCustomIndirectBitmapLayer) then
            continue;

          GR32.UnionRect(Location, Location, TPositionedLayer(SourceLayer).Location);
        end;

        if (Location.Width = 0) or (Location.Height = 0) then
          exit;

        CompositeBitmap.SetSize(Ceil(Location.Width), Ceil(Location.Height), False);
      end else
        CompositeBitmap.SetSizeFrom(MainBitmap, False);

      // We clear the background with:
      //
      //   $00xxxxxx to make it transparent for those that can handle transparent PSD
      //
      //   $xxFFFFFF to make it white for those that can't handle transparent PSD
      //
      // If the image contains layers and the reader can handle them then the
      // background is ignored; The background is only used when there are no
      // layers or if the reader cannot handle layers.
      CompositeBitmap.Clear($00FFFFFF);

      // Create flattened bitmap (composite)
      // TODO : Hide the main bitmap while we flatten if peIgnoreBitmap is set
      AImage.PaintTo(CompositeBitmap, CompositeBitmap.BoundsRect);

      if (peCompositeAsLayer in AOptions) then
      begin
        PhotoshopLayer := TPhotoshopLayer32.Create(ADocument);
        TPhotoshopLayer32(PhotoshopLayer).Bitmap := CompositeBitmap;
        // If there is a composite background then it will own the bitmap. Otherwise
        // this layer owns it.
        TPhotoshopLayer32(PhotoshopLayer).OwnsBitmap := not(peCompositeAsBackground in AOptions);
      end;

      if (peCompositeAsBackground in AOptions) then
      begin
        PhotoshopLayer := TPhotoshopLayer32.Create;
        TPhotoshopLayer32(PhotoshopLayer).Bitmap := CompositeBitmap;
        TPhotoshopLayer32(PhotoshopLayer).OwnsBitmap := True;
        CompositeBitmap := nil;
        ADocument.Background := PhotoshopLayer;
      end;

    finally
      CompositeBitmap.Free;
    end;
  end;

  (*
  ** Add the main bitmap as a layer
  *)
  if (peBackgroundAsLayer in AOptions) and (MainBitmap <> nil) then
  begin
    PhotoshopLayer := TPhotoshopLayer32.Create(ADocument);
    PhotoshopLayer.Opacity := MainBitmap.MasterAlpha;
    // Layer just references the bitmap; It doesn't own it.
    TPhotoshopLayer32(PhotoshopLayer).Bitmap :=  MainBitmap;

    PhotoshopLayer.Name := Format(sPSDLayerName, [ADocument.Layers.Count]);
  end;

  (*
  ** Add layers
  *)
  for SourceLayer in AImage.Layers do
  begin
    if not (SourceLayer is TCustomIndirectBitmapLayer) then
      continue;

    LayerBitmap := TBitmapLayerCracker(SourceLayer).Bitmap;
    Location := TBitmapLayerCracker(SourceLayer).Location;

    PhotoshopLayer := TPhotoshopLayer32.Create(ADocument);
    PhotoshopLayer.Opacity := LayerBitmap.MasterAlpha;
    PhotoshopLayer.Left := Round(Location.Left);
    PhotoshopLayer.Top := Round(Location.Top);
    // Layer just references the bitmap; It doesn't own it.
    TPhotoshopLayer32(PhotoshopLayer).Bitmap :=  LayerBitmap;
    if (not SourceLayer.Visible) then
      PhotoshopLayer.Options := PhotoshopLayer.Options + [loHidden];

    PhotoshopLayer.Name := Format(sPSDLayerName, [ADocument.Layers.Count]);
  end;

  // At this point, if we don't have a PSD background, then we mimic
  // PhotoShop and create a background containing an opaque (not
  // really though) white bitmap in order to "maximize compatibility".
  if (ADocument.Background = nil) then
  begin
    PhotoshopLayer := TPhotoshopBitmapLayer32.Create;
    try

      Location := FloatRect(0,0,1,1);

      for SourceLayer in AImage.Layers do
      begin
        if not (SourceLayer is TCustomIndirectBitmapLayer) then
          continue;

        GR32.UnionRect(Location, Location, TPositionedLayer(SourceLayer).Location);
      end;

      PhotoshopLayer.Bitmap.SetSize(Ceil(Location.Right), Ceil(Location.Bottom), False);
      PhotoshopLayer.Bitmap.Clear($00FFFFFF);

    except
      PhotoshopLayer.Free;
      raise;
    end;

    ADocument.Background := PhotoshopLayer;
  end;
end;


//------------------------------------------------------------------------------
//
//      Construct a TPhotoshopDocument from a TBitmap32
//
//------------------------------------------------------------------------------
procedure CreatePhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopExportOptions); overload;
var
  PSDLayer: TCustomPhotoshopLayer;
begin
  ADocument.Clear;

  if ABitmap.Empty then
    Exit;

  PSDLayer := TPhotoshopLayer32.Create; // Create layer with no owner
  try

    TPhotoshopLayer32(PSDLayer).Bitmap := ABitmap;

    if (peSingleAsLayer in AOptions) then
      PSDLayer.Document := ADocument // Document now owns the layer
    else
      ADocument.Background := PSDLayer; // Document now owns the layer

  except
    PSDLayer.Free;
    raise;
  end;
end;


//------------------------------------------------------------------------------
//
//      Load a TPhotoshopDocument into a TCustomImage32
//
//------------------------------------------------------------------------------
procedure LoadImageFromPhotoshopDocument(AImage: TCustomImage32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopImportOptions; ABackground: TColor32);
var
  Layer: TCustomPhotoshopLayer;
  BitmapLayer: TBitmapLayer;
begin
  AImage.Bitmap.Delete;
  AImage.Bitmap.MasterAlpha := 255;
  AImage.Bitmap.DrawMode := dmBlend;
  AImage.Layers.Clear;

  if (piBackgroundFromFirstLayer in AOptions) then
    Exclude(AOptions, piBackgroundIsComposite);

  if (ADocument.Layers.Count > 0) then
  begin
    for Layer in ADocument.Layers do
    begin
      if (not (Layer is TCustomPhotoshopBitmapLayer32)) then
        continue;

      if (piBackgroundFromFirstLayer in AOptions) and (Layer.Left = 0) and (Layer.Top = 0) and (not (loHidden in Layer.Options)) and (not TCustomPhotoshopBitmapLayer32(Layer).Bitmap.Empty) then
      begin
        // First visible layer is used as the background
        AImage.Bitmap.Assign(TCustomPhotoshopBitmapLayer32(Layer).Bitmap);
        AImage.Bitmap.MasterAlpha := Layer.Opacity;

        Exclude(AOptions, piBackgroundFromFirstLayer);
        continue;
      end;

      BitmapLayer := TBitmapLayer.Create(AImage.Layers);
      BitmapLayer.Bitmap.Assign(TCustomPhotoshopBitmapLayer32(Layer).Bitmap);
      BitmapLayer.Bitmap.DrawMode := dmBlend;
      BitmapLayer.Bitmap.MasterAlpha := Layer.Opacity;
      BitmapLayer.Location := GR32.FloatRect(Layer.Left, Layer.Top, Layer.Left + Layer.Width, Layer.Top + Layer.Height);
      BitmapLayer.Scaled := True; // Layers are relative to main bitmap
      BitmapLayer.Visible := not (loHidden in Layer.Options);
    end;
  end;

  if (piBackgroundIsComposite in AOptions) then
    // Construct composite via the PSD image adapter
    LoadBitmapFromPhotoshopDocument(AImage.Bitmap, ADocument, AOptions * [piBackgroundIsComposite, piBackgroundIsOpaque], ABackground);
end;


//------------------------------------------------------------------------------
//
//      Load a TPhotoshopDocument into a TCustomBitmap32
//
//------------------------------------------------------------------------------
// The result bitmap is a composite of all the layers in the document.
//------------------------------------------------------------------------------
procedure LoadBitmapFromPhotoshopDocument(ABitmap: TCustomBitmap32; ADocument: TPhotoshopDocument; AOptions: TPhotoshopImportOptions; ABackground: TColor32);
var
  Layer: TCustomPhotoshopLayer;
  SourceBitmap: TCustomBitmap32;
  SaveCombineMode: TCombineMode;
  SaveMasterAlpha: Cardinal;
begin
  ABitmap.Delete;

  for Layer in ADocument.Layers do
    if Layer is TCustomPhotoshopBitmapLayer32 then
    begin
      if (loHidden in Layer.Options) or (TCustomPhotoshopBitmapLayer32(Layer).Bitmap.Empty) then
        continue;

      SourceBitmap := TCustomPhotoshopBitmapLayer32(Layer).Bitmap;

      if (piBackgroundFromFirstLayer in AOptions) then
      begin
        ABitmap.Assign(SourceBitmap);
        break;
      end;

      if (ABitmap.Empty) then
      begin
        ABitmap.SetSize(ADocument.Width, ADocument.Height, not(piBackgroundIsOpaque in AOptions));
        if (piBackgroundIsOpaque in AOptions) then
          ABitmap.Clear(ABackground);
        ABitmap.MasterAlpha := 255;
      end;

      SaveCombineMode := SourceBitmap.CombineMode;
      SaveMasterAlpha := SourceBitmap.MasterAlpha;

      SourceBitmap.BeginLockUpdate; // Prevent temporary source bitmap changes from triggering a Changed event
      try
        SourceBitmap.MasterAlpha := Layer.Opacity;

        if not(piBackgroundIsOpaque in AOptions) then
          SourceBitmap.CombineMode := cmMerge; // Must merge or background will become opaque regardless of layer alpha

        SourceBitmap.DrawTo(ABitmap, Layer.Left, Layer.Top);

        if not(piBackgroundIsOpaque in AOptions) then
          SourceBitmap.CombineMode := SaveCombineMode;

        SourceBitmap.MasterAlpha := SaveMasterAlpha;
      finally
        SourceBitmap.EndLockUpdate;
      end;
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

function TPhotoshopDocument.TPhotoshopLayers.GetEnumerator: TEnumerator<TCustomPhotoshopLayer>;
begin
  Result := FLayers.GetEnumerator;
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
  FChannels := 4;
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
    CreatePhotoshopDocument(TCustomImage32(Source), Self, DefaultPhotoshopExportOptions)
  else
    inherited;
end;

procedure TPhotoshopDocument.AssignTo(Dest: TPersistent);
begin
  if (Dest is TCustomImage32) then
    LoadImageFromPhotoshopDocument(TCustomImage32(Dest), Self, DefaultPhotoshopImportOptions, DefaultPhotoshopImportBackground)
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

procedure TCustomPhotoshopBitmapLayer32.SetChannelScanlinePixel(AChannel: TColor32Component; ALine, AOffset: integer; AByte: byte);
var
  pDest: PByte;
begin
  if (Bitmap = nil) then
    exit;

  pDest := @(PColor32Entry(Bitmap.PixelPtr[AOffset, ALine + FSourceTop]).Components[AChannel]);
  pDest^ := AByte;
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
    FBitmap.DrawMode := dmBlend;
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

procedure TPhotoshopPlaceholderLayer.SetChannelScanlinePixel(AChannel: TColor32Component; ALine, AOffset: integer; AByte: byte);
begin
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TPhotoshopDocument.DefaultLayerClass := TPhotoshopLayer32;
  TPhotoshopDocument.DefaultCompression := lcRLE;

finalization
end.

