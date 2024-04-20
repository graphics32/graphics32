unit GR32_PortableNetworkGraphic;

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
 * The Original Code is GR32PNG for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}
{$I GR32_PngCompilerSwitches.inc}

// The following defines controls if the corresponding incomplete chunk
// implementations should be enabled. They are disabled by default because
// a complete implementation is required in order to pass the roundtrip unit
// tests.
{-$define PNG_CHUNK_SUGGESTED_PALETTE}
{-$define PNG_CHUNK_INTERNATIONAL_TEXT}

uses
  Classes, Graphics, SysUtils,
{$IFDEF FPC}
  ZBase, ZDeflate, ZInflate;
{$ELSE}
  {$IFDEF ZLibEx}
    ZLibEx, ZLibExApi;
  {$ELSE}
    {$if (CompilerVersion >= 32)} System.zlib; {$else} zlib; {$ifend}
  {$ENDIF}
{$ENDIF}

type
  {$A1}
  TColorType = (
    ctGrayscale = 0,
    ctTrueColor = 2,
    ctIndexedColor = 3,
    ctGrayscaleAlpha = 4,
    ctTrueColorAlpha = 6
  );

  TFilterMethod = (
    fmAdaptiveFilter = 0
  );

  TAdaptiveFilterMethod = (
    afmNone = 0,
    afmSub = 1,
    afmUp = 2,
    afmAverage = 3,
    afmPaeth = 4
  );

  TAvailableAdaptiveFilterMethod = (aafmSub, aafmUp, aafmAverage, aafmPaeth);
  TAvailableAdaptiveFilterMethods = set of TAvailableAdaptiveFilterMethod;

  TInterlaceMethod = (
    imNone = 0,
    imAdam7 = 1
  );

  TRGB24 = packed record
    R, G, B: Byte;
  end;
  PRGB24 = ^TRGB24;
  TRGB24Array = array [0..0] of TRGB24;
  PRGB24Array = ^TRGB24Array;

  TRGB24Word = packed record
    R, G, B : Word;
  end;
  PRGB24Word = ^TRGB24Word;

  TRGB32 = packed record
    R, G, B, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  TRGB32Word = packed record
    R, G, B, A: Word;
  end;
  PRGB32Word = ^TRGB32Word;

  PByteArray = SysUtils.PByteArray;
  TByteArray = SysUtils.TByteArray;

  TChunkName = array [0..3] of AnsiChar;

  EPngError = class(Exception);

  {$IFDEF FPC}
  TZStreamRec = z_stream;
  {$ENDIF}

  {$A4}

  TCustomChunk = class abstract(TPersistent)
  protected
    function GetChunkNameAsString: AnsiString; virtual; abstract;
    function GetChunkName: TChunkName; virtual; abstract;
    function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkName: TChunkName read GetChunkName;
    property ChunkNameAsString: AnsiString read GetChunkNameAsString;
    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TCustomDefinedChunk = class abstract(TCustomChunk)
  protected
    function GetChunkNameAsString: AnsiString; override;
    function GetChunkName: TChunkName; override;
    class function GetClassChunkName: TChunkName; virtual; abstract;
  public
    property ChunkName: TChunkName read GetClassChunkName;
  end;

  TCustomDefinedChunkClass = class of TCustomDefinedChunk;

  TChunkPngImageHeader = class(TCustomDefinedChunk)
  private
    FWidth                 : Integer;
    FHeight                : Integer;
    FBitDepth              : Byte;
    FColorType             : TColorType;
    FCompressionMethod     : Byte;
    FFilterMethod          : TFilterMethod;
    FInterlaceMethod       : TInterlaceMethod;
    FAdaptiveFilterMethods : TAvailableAdaptiveFilterMethods;
    function GetHasPalette: Boolean;
    function GetBytesPerRow: Integer;
    function GetPixelByteSize: Integer;
    procedure SetCompressionMethod(const Value: Byte);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetAdaptiveFilterMethods(const Value: TAvailableAdaptiveFilterMethods);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    procedure ResetToDefault; virtual;

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property BitDepth: Byte read FBitDepth write FBitDepth;
    property ColorType: TColorType read FColorType write FColorType;
    property CompressionMethod: Byte read FCompressionMethod write SetCompressionMethod;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read FAdaptiveFilterMethods write SetAdaptiveFilterMethods;
    property FilterMethod: TFilterMethod read FFilterMethod write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read FInterlaceMethod write FInterlaceMethod;
    property HasPalette: Boolean read GetHasPalette;

    property BytesPerRow: Integer read GetBytesPerRow;
    property PixelByteSize: Integer read GetPixelByteSize;
  end;

  TCustomDefinedChunkWithHeader = class(TCustomDefinedChunk)
  protected
    FHeader : TChunkPngImageHeader;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); reintroduce; virtual;

    procedure HeaderChanged; virtual;

    property Header: TChunkPngImageHeader read FHeader;
  end;
  TCustomDefinedChunkWithHeaderClass = class of TCustomDefinedChunkWithHeader;

  TChunkPngImageData = class(TCustomDefinedChunkWithHeader)
  private
    FData : TMemoryStream;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Data: TMemoryStream read FData;
  end;

  TChunkPngPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteEntries : array of TRGB24;
    function GetPaletteEntry(Index: Cardinal): TRGB24;
    function GetCount: Cardinal;
    procedure SetCount(const Value: Cardinal);
    procedure SetPaletteEntry(Index: Cardinal; const Value: TRGB24);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure PaletteEntriesChanged; virtual;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property PaletteEntry[Index: Cardinal]: TRGB24 read GetPaletteEntry write SetPaletteEntry; default;
    property Count: Cardinal read GetCount write SetCount;
  end;

  TChunkPngGamma = class(TCustomDefinedChunkWithHeader)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
    procedure SetGammaAsSingle(const Value: Single);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle write SetGammaAsSingle;
  end;

  TChunkPngStandardColorSpaceRGB = class(TCustomDefinedChunkWithHeader)
  private
    FRenderingIntent : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property RenderingIntent: Byte read FRenderingIntent write FRenderingIntent;
  end;

  TChunkPngPrimaryChromaticities = class(TCustomDefinedChunkWithHeader)
  private
    FWhiteX : Integer;
    FWhiteY : Integer;
    FRedX   : Integer;
    FRedY   : Integer;
    FGreenX : Integer;
    FGreenY : Integer;
    FBlueX  : Integer;
    FBlueY  : Integer;
    function GetBlueX: Single;
    function GetBlueY: Single;
    function GetGreenX: Single;
    function GetGreenY: Single;
    function GetRedX: Single;
    function GetRedY: Single;
    function GetWhiteX: Single;
    function GetWhiteY: Single;
    procedure SetBlueX(const Value: Single);
    procedure SetBlueY(const Value: Single);
    procedure SetGreenX(const Value: Single);
    procedure SetGreenY(const Value: Single);
    procedure SetRedX(const Value: Single);
    procedure SetRedY(const Value: Single);
    procedure SetWhiteX(const Value: Single);
    procedure SetWhiteY(const Value: Single);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property WhiteX: Integer read FWhiteX write FWhiteX;
    property WhiteY: Integer read FWhiteY write FWhiteY;
    property RedX: Integer read FRedX write FRedX;
    property RedY: Integer read FRedY write FRedY;
    property GreenX: Integer read FGreenX write FGreenX;
    property GreenY: Integer read FGreenY write FGreenY;
    property BlueX: Integer read FBlueX write FBlueX;
    property BlueY: Integer read FBlueY write FBlueY;

    property WhiteXAsSingle: Single read GetWhiteX write SetWhiteX;
    property WhiteYAsSingle: Single read GetWhiteY write SetWhiteY;
    property RedXAsSingle: Single read GetRedX write SetRedX;
    property RedYAsSingle: Single read GetRedY write SetRedY;
    property GreenXAsSingle: Single read GetGreenX write SetGreenX;
    property GreenYAsSingle: Single read GetGreenY write SetGreenY;
    property BlueXAsSingle: Single read GetBlueX write SetBlueX;
    property BlueYAsSingle: Single read GetBlueY write SetBlueY;
  end;

  TChunkPngTime = class(TCustomDefinedChunkWithHeader)
  private
    FYear   : Word;
    FMonth  : Byte;
    FDay    : Byte;
    FHour   : Byte;
    FMinute : Byte;
    FSecond : Byte;
    function GetModifiedDateTime: TDateTime;
    procedure SetModifiedDateTime(const Value: TDateTime);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Year: Word read FYear write FYear;
    property Month: Byte read FMonth write FMonth;
    property Day: Byte read FDay write FDay;
    property Hour: Byte read FHour write FHour;
    property Minute: Byte read FMinute write FMinute;
    property Second: Byte read FSecond write FSecond;
    property ModifiedDateTime: TDateTime read GetModifiedDateTime write SetModifiedDateTime;
  end;

  TChunkPngEmbeddedIccProfile = class(TCustomDefinedChunkWithHeader)
  private
    FProfileName       : AnsiString;
    FCompressionMethod : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property ProfileName: AnsiString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TCustomPngSignificantBits = class abstract(TPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    constructor Create(BitDepth: Integer = 8); virtual; abstract;

    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngSignificantBitsFormat0 = class(TCustomPngSignificantBits)
  private
    FGrayBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
  end;

  TPngSignificantBitsFormat23 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
  end;

  TPngSignificantBitsFormat4 = class(TCustomPngSignificantBits)
  private
    FGrayBits  : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TPngSignificantBitsFormat6 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(BitDepth: Integer = 8); override;

    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TChunkPngSignificantBits = class(TCustomDefinedChunkWithHeader)
  private
    FSignificantBits : TCustomPngSignificantBits;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    procedure HeaderChanged; override;

    property SignificantBits: TCustomPngSignificantBits read FSignificantBits;
  end;

  TCustomPngBackgroundColor = class abstract(TPersistent)
  protected
    class function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngBackgroundColorFormat04 = class(TCustomPngBackgroundColor)
  private
    FGraySampleValue : Word;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngBackgroundColorFormat26 = class(TCustomPngBackgroundColor)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngBackgroundColorFormat3 = class(TCustomPngBackgroundColor)
  private
    FIndex : Byte;
  protected
    class function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

  TChunkPngBackgroundColor = class(TCustomDefinedChunkWithHeader)
  private
    FBackground : TCustomPngBackgroundColor;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    procedure HeaderChanged; override;

    property Background: TCustomPngBackgroundColor read FBackground;
  end;

  TChunkPngImageHistogram = class(TCustomDefinedChunkWithHeader)
  private
    FHistogram : array of Word;
    function GetCount: Cardinal;
    function GetFrequency(Index: Cardinal): Word;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
    property Frequency[Index: Cardinal]: Word read GetFrequency;
  end;

  TSuggestedPalette8ByteEntry = record
    Red       : Byte;
    Green     : Byte;
    Blue      : Byte;
    Alpha     : Byte;
    Frequency : Word;
  end;
  PSuggestedPalette8ByteEntry = ^TSuggestedPalette8ByteEntry;
  TSuggestedPalette8ByteArray = array [0..0] of TSuggestedPalette8ByteEntry;
  PSuggestedPalette8ByteArray = ^TSuggestedPalette8ByteArray;

  TSuggestedPalette16ByteEntry = record
    Red       : Word;
    Green     : Word;
    Blue      : Word;
    Alpha     : Word;
    Frequency : Word;
  end;
  PSuggestedPalette16ByteEntry = ^TSuggestedPalette16ByteEntry;
  TSuggestedPalette16ByteArray = array [0..0] of TSuggestedPalette16ByteEntry;
  PSuggestedPalette16ByteArray = ^TSuggestedPalette16ByteArray;

{$ifdef PNG_CHUNK_SUGGESTED_PALETTE}
  TChunkPngSuggestedPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteName : AnsiString;
    FData        : Pointer;
    FCount       : Cardinal;
    FSampleDepth : Byte;
    function GetCount: Cardinal;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
  end;
{$endif PNG_CHUNK_SUGGESTED_PALETTE}

  TCustomPngTransparency = class abstract(TPersistent)
  protected
    function GetChunkSize: Cardinal; virtual; abstract;
  public
    procedure ReadFromStream(Stream: TStream); virtual; abstract;
    procedure WriteToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGraySampleValue : Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: Cardinal;
    function GetTransparency(Index: Cardinal): Byte;
  protected
    FTransparency : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
    property Transparency[Index: Cardinal]: Byte read GetTransparency;
  end;

  TChunkPngTransparency = class(TCustomDefinedChunkWithHeader)
  protected
    FTransparency : TCustomPngTransparency;
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    procedure HeaderChanged; override;

    property Transparency: TCustomPngTransparency read FTransparency;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomDefinedChunkWithHeader)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

  TChunkPngPhysicalScale = class(TCustomDefinedChunkWithHeader)
  private
    FUnitSpecifier  : Byte;
    FUnitsPerPixelX : Single;
    FUnitsPerPixelY : Single;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property UnitsPerPixelX: Single read FUnitsPerPixelX write FUnitsPerPixelX;
    property UnitsPerPixelY: Single read FUnitsPerPixelY write FUnitsPerPixelY;
  end;

  TChunkPngImageOffset = class(TCustomDefinedChunkWithHeader)
  private
    FImagePositionX : Integer;
    FImagePositionY : Integer;
    FUnitSpecifier  : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property ImagePositionX: Integer read FImagePositionX write FImagePositionX;
    property ImagePositionY: Integer read FImagePositionY write FImagePositionY;
  end;

  TChunkPngPixelCalibrator = class(TCustomDefinedChunkWithHeader)
  private
    FCalibratorName : AnsiString;
    FOriginalZeroes : array [0..1] of Integer;
    FEquationType   : Byte;
    FNumberOfParams : Byte;
    FUnitName       : AnsiString;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property CalibratorName: AnsiString read FCalibratorName write FCalibratorName;
    property OriginalZeroMin: Integer read FOriginalZeroes[0] write FOriginalZeroes[0];
    property OriginalZeroMax: Integer read FOriginalZeroes[1] write FOriginalZeroes[1];
    property EquationType: Byte read FEquationType write FEquationType;
    property NumberOfParams: Byte read FNumberOfParams write FNumberOfParams;
  end;

  TCustomChunkPngText = class(TCustomDefinedChunkWithHeader)
  private
    procedure SetKeyword(const Value: AnsiString);
    procedure SetText(const Value: AnsiString);
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
    procedure AssignTo(Dest: TPersistent); override;
    procedure KeywordChanged; virtual;
    procedure TextChanged; virtual;
  public
    property Keyword: AnsiString read FKeyword write SetKeyword;
    property Text: AnsiString read FText write SetText;
  end;

  TChunkPngText = class(TCustomChunkPngText)
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;
  end;

  TChunkPngCompressedText = class(TCustomChunkPngText)
  private
    FCompressionMethod : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure SetCompressionMethod(const Value: Byte);

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write SetCompressionMethod;
  end;

{$ifdef PNG_CHUNK_INTERNATIONAL_TEXT}
  TChunkPngInternationalText = class(TCustomChunkPngText)
  private
    FCompressionMethod : Byte;
    FCompressionFlag   : Byte;
    FLanguageString    : AnsiString;
    FTranslatedKeyword : string;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: AnsiString read FLanguageString write FLanguageString;
    property TranslatedKeyword: string read FTranslatedKeyword write FTranslatedKeyword;
  end;
{$endif PNG_CHUNK_INTERNATIONAL_TEXT}

  TChunkPngUnknown = class(TCustomChunk)
  private
    function GetData(index: Integer): Byte;
    procedure SetData(index: Integer; const Value: Byte);
  protected
    FChunkName  : TChunkName;
    FDataStream : TMemoryStream;
    function GetChunkName: TChunkName; override;
    function GetChunkNameAsString: AnsiString; override;
    function GetChunkSize: Cardinal; override;
    function CalculateChecksum: Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ChunkName: TChunkName); virtual;
    destructor Destroy; override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Data[index : Integer]: Byte read GetData write SetData;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TChunkList = class(TPersistent)
  private
    FChunks : array of TCustomChunk;
    function GetCount: Cardinal;
  protected
    function GetChunk(Index: Integer): TCustomChunk;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;

    procedure Add(Item: TCustomChunk);
    procedure Clear; virtual;
    procedure Delete(Index: Cardinal);
    function IndexOf(Item: TCustomChunk): Integer;
    procedure Remove(Item: TCustomChunk);

    property Count: Cardinal read GetCount;
    property Chunks[Index: Integer]: TCustomChunk read GetChunk; default;
  end;

  TCustomPngCoder = class abstract
  protected
    FStream       : TStream;
    FHeader       : TChunkPngImageHeader;
    FGamma        : TChunkPngGamma;
    FPalette      : TChunkPngPalette;
    FTransparency : TCustomPngTransparency;

    FRowBuffer    : array [0..1] of PByteArray;
    FAlphaTable   : PByteArray;
    FMappingTable : PByteArray;
    procedure BuildMappingTables; virtual;

    procedure EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure DecodeFilterSub(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterUp(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);

    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency : TCustomPngTransparency = nil); virtual;
    destructor Destroy; override;
  end;

  TScanLineCallback = function(Bitmap: TObject; Y: Integer): Pointer of object;

  TCustomPngDecoder = class abstract(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
  public
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;
  TCustomPngDecoderClass = class of TCustomPngDecoder;

  TCustomPngEncoder = class abstract(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
  public
    procedure EncodeFromScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;
  TCustomPngEncoderClass = class of TCustomPngEncoder;

  TCustomPngTranscoder = class abstract(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;

    procedure Transcode; virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil;
      Transparency: TCustomPngTransparency = nil); override;
    destructor Destroy; override;
  end;
  TCustomPngTranscoderClass = class of TCustomPngTranscoder;

  TPortableNetworkGraphic = class(TInterfacedPersistent, IStreamPersist)
  private
    FCompressionLevel : Byte;
    function GetBitDepth: Byte;
    function GetColorType: TColorType;
    function GetCompressionMethod: Byte;
    function GetFilterMethod: TFilterMethod;
    function GetHeight: Integer;
    function GetInterlaceMethod: TInterlaceMethod;
    function GetPaletteEntry(Index: Integer): TRGB24;
    function GetPaletteEntryCount: Integer;
    function GetWidth: Integer;
    function GetGamma: Single;
    function GetModifiedTime: TDateTime;
    function GetPixelsPerUnitX: Cardinal;
    function GetPixelsPerUnitY: Cardinal;
    function GetPixelUnit: Byte;
    procedure SetPixelsPerUnitX(const Value: Cardinal);
    procedure SetPixelsPerUnitY(const Value: Cardinal);
    procedure SetPixelUnit(const Value: Byte);
    procedure SetBitDepth(const Value: Byte);
    procedure SetChromaChunk(const Value: TChunkPngPrimaryChromaticities);
    procedure SetColorType(const Value: TColorType);
    procedure SetCompressionMethod(const Value: Byte);
    procedure SetCompressionLevel(const Value: Byte);
    procedure SetFilterMethods(const Value: TAvailableAdaptiveFilterMethods);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetGamma(const Value: Single);
    procedure SetModifiedTime(const Value: TDateTime);
    procedure SetHeight(const Value: Integer);
    procedure SetImageHeader(const Value: TChunkPngImageHeader);
    procedure SetInterlaceMethod(const Value: TInterlaceMethod);
    procedure SetGammaChunk(const Value: TChunkPngGamma);
    procedure SetPaletteChunk(const Value: TChunkPngPalette);
    procedure SetTransparencyChunk(const Value: TChunkPngTransparency);
    procedure SetPhysicalDimensions(const Value: TChunkPngPhysicalPixelDimensions);
    procedure SetSignificantBits(const Value: TChunkPngSignificantBits);
    procedure SetTimeChunk(const Value: TChunkPngTime);
    procedure SetWidth(const Value: Integer);

    function CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal; overload;
    function CalculateCRC(Stream: TStream): Cardinal; overload;
    {$IFDEF CheckCRC}
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    {$ENDIF}
    procedure ReadImageDataChunk(Stream: TStream; Size: Integer);
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName; ChunkSize: Integer);
    function GetFilterMethods: TAvailableAdaptiveFilterMethods;
    procedure SetBackgroundChunk(const Value: TChunkPngBackgroundColor);
  protected
    FImageHeader         : TChunkPngImageHeader;
    FPaletteChunk        : TChunkPngPalette;
    FGammaChunk          : TChunkPngGamma;
    FTimeChunk           : TChunkPngTime;
    FSignificantBits     : TChunkPngSignificantBits;
    FPhysicalDimensions  : TChunkPngPhysicalPixelDimensions;
    FChromaChunk         : TChunkPngPrimaryChromaticities;
    FTransparencyChunk   : TChunkPngTransparency;
    FBackgroundChunk     : TChunkPngBackgroundColor;
    FDataChunkList       : TChunkList;
    FAdditionalChunkList : TChunkList;

    procedure Clear; virtual;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CopyImageData(Stream: TStream);
    procedure StoreImageData(Stream: TStream);
    procedure DecompressImageDataToStream(Stream: TStream);
    procedure CompressImageDataFromStream(Stream: TStream);

    procedure CompressionLevelChanged; virtual;
    procedure AdaptiveFilterMethodsChanged; virtual;
    procedure InterlaceMethodChanged; virtual;

    property ImageHeader: TChunkPngImageHeader read FImageHeader write SetImageHeader;
    property PaletteChunk: TChunkPngPalette read FPaletteChunk write SetPaletteChunk;
    property TransparencyChunk: TChunkPngTransparency read FTransparencyChunk write SetTransparencyChunk;
    property BackgroundChunk: TChunkPngBackgroundColor read FBackgroundChunk write SetBackgroundChunk;
    property GammaChunk: TChunkPngGamma read FGammaChunk write SetGammaChunk;
    property TimeChunk: TChunkPngTime read FTimeChunk write SetTimeChunk;
    property PhysicalPixelDimensionsChunk: TChunkPngPhysicalPixelDimensions read FPhysicalDimensions write SetPhysicalDimensions;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure LoadFromFile(Filename: TFilename); virtual;
    procedure SaveToFile(Filename: TFilename); virtual;

    class function CanLoad(const FileName: TFileName): Boolean; overload;
    class function CanLoad(Stream: TStream): Boolean; overload;

    function HasPhysicalPixelDimensionsInformation: Boolean;
    function HasGammaInformation: Boolean;
    function HasModifiedTimeInformation: Boolean;
    procedure RemovePhysicalPixelDimensionsInformation;
    procedure RemoveGammaInformation;
    procedure RemoveModifiedTimeInformation;

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property BitDepth: Byte read GetBitDepth write SetBitDepth;
    property ColorType: TColorType read GetColorType write SetColorType;
    property CompressionMethod: Byte read GetCompressionMethod write SetCompressionMethod;
    property CompressionLevel: Byte read FCompressionLevel write SetCompressionLevel;
    property AdaptiveFilterMethods: TAvailableAdaptiveFilterMethods read GetFilterMethods write SetFilterMethods;
    property FilterMethod: TFilterMethod read GetFilterMethod write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read GetInterlaceMethod write SetInterlaceMethod;
    property PaletteEntry[Index: Integer]: TRGB24 read GetPaletteEntry;
    property PaletteEntryCount: Integer read GetPaletteEntryCount;
    property Gamma: Single read GetGamma write SetGamma;
    property ModifiedTime: TDateTime read GetModifiedTime write SetModifiedTime;
    property PixelsPerUnitX: Cardinal read GetPixelsPerUnitX write SetPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read GetPixelsPerUnitY write SetPixelsPerUnitY;
    property PixelUnit: Byte read GetPixelUnit write SetPixelUnit;

    property SignificantBitsChunk: TChunkPngSignificantBits read FSignificantBits write SetSignificantBits;
    property PrimaryChromaticitiesChunk: TChunkPngPrimaryChromaticities read FChromaChunk write SetChromaChunk;
  end;

procedure RegisterPngChunk(ChunkClass: TCustomDefinedChunkWithHeaderClass);
procedure RegisterPngChunks(ChunkClasses: array of TCustomDefinedChunkWithHeaderClass);
function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomDefinedChunkWithHeaderClass;

function ColorTypeToString(Value: TColorType): string;
function InterlaceMethodToString(Value: TInterlaceMethod): string;

implementation

uses
  Math,
  GR32_LowLevel,
  GR32.BigEndian;

resourcestring
  RCStrAncillaryUnknownChunk = 'Unknown chunk is marked as ancillary';
  RCStrChunkSizeTooSmall = 'Chunk size too small!';
  RCStrDataIncomplete = 'Data not complete';
  RCStrChunkInvalid = 'Invalid chunk data';
  RCStrDirectCompressionMethodSetError = 'Compression Method may not be specified directly yet!';
  RCStrDirectFilterMethodSetError = 'Filter Method may not be specified directly yet!';
  RCStrDirectGammaSetError = 'Gamma may not be specified directly yet!';
  RCStrDirectHeightSetError = 'Height may not be specified directly yet!';
  RCStrDirectWidthSetError = 'Width may not be specified directly yet!';
  RCStrEmptyChunkList = 'Chunk list is empty';
  RCStrHeaderInvalid = 'The provided header is not valid!';
  RCStrIncompletePalette = 'Palette is incomplete';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrNewHeaderError = 'New header may not be nil!';
  RCStrNotAValidPNGFile = 'Not a valid PNG file';
  RCStrNotYetImplemented = 'PNG feature not implemented (%s)';
  RCStrChunkNotImplemented = 'Chunk type %s not implemented';
  RCStrPaletteLimited = 'Palette is limited to 256 entries';
  RCStrSeveralChromaChunks = 'Primary chromaticities chunk defined twice!';
  RCStrSeveralGammaChunks = 'Gamma chunk defined twice!';
  RCStrSeveralPaletteChunks = 'Palette chunk defined twice!';
  RCStrSeveralTransparencyChunks = 'Transparency chunk defined twice!';
  RCStrSeveralBackgroundChunks = 'Background chunk defined twice!';
  RCStrSeveralPhysicalPixelDimensionChunks = 'Several physical pixel dimenson chunks found';
  RCStrSeveralSignificantBitsChunksFound = 'Several significant bits chunks found';
  RCStrSeveralTimeChunks = 'Time chunk appears twice!';
  RCStrMissingIDATChunk = 'IDAT chunk missing';
  RCStrUnknownColorType = 'Unknown color type!';
  RCStrUnspecifiedPixelUnit = 'Unspecified unit';
  RCStrUnsupportedCompressionMethod = 'Compression method not supported!';
  RCStrUnsupportedCompressMethod = 'Unsupported compression method';
  RCStrUnsupportedFilter = 'Unsupported Filter';
  RCStrUnsupportedFilterMethod = 'Unsupported filter method';
  RCStrUnsupportedInterlaceMethod = 'Unsupported interlace method';
  RCStrUnsupportedColorType = 'Unsupported color type';
  RCStrWrongBitdepth = 'Wrong Bitdepth';
  RCStrWrongInterlaceMethod = 'Wrong interlace method';
  RCStrWrongPixelPerUnit = 'Pixel per unit may not be zero!';
  RCStrWrongTransparencyFormat = 'Wrong transparency format';
  RCStrInvalidCompressionLevel = 'Invalid compression level';
  RCStrBitDepthTranscodingError = 'Bit depth may not be specified directly yet!';
  RCStrColorTypeTranscodingError = 'Color Type may not be specified directly yet!';
  RCStrGrayscale = 'Grayscale';
  RCStrTrueColor = 'True Color';
  RCStrIndexedColor = 'Indexed Color';
  RCStrGrayscaleAlpha = 'Transparent Grayscale';
  RCStrTrueColorAlpha = 'Transparent True Color';
  RCStrInterlacingNone = 'None';
  RCStrInterlacingAdam7 = 'Adam7';
  {$IFDEF CheckCRC}
  RCStrCRCError = 'CRC Error';
  {$ENDIF}

type
  TCrcTable = array [0..255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable : PCrcTable;
  GPngChunkClasses: array of TCustomDefinedChunkWithHeaderClass;


const
  CPngMagic = #$0D#$0A#$1A#$0A;

  CRowStart        : array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart     : array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement    : array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement : array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);

type
  TPngNonInterlacedToAdam7Transcoder = class(TCustomPngTranscoder)
  protected
    procedure Transcode; override;
  end;

  TPngAdam7ToNonInterlacedTranscoder = class(TCustomPngTranscoder)
  protected
    procedure Transcode; override;
  end;

function IsPngChunkRegistered(ChunkClass: TCustomDefinedChunkWithHeaderClass): Boolean;
var
  ChunkClassIndex : Integer;
begin
  Result := False;
  for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
    if GPngChunkClasses[ChunkClassIndex] = ChunkClass then
    begin
      Result := True;
      Exit;
    end;
end;

procedure RegisterPngChunk(ChunkClass: TCustomDefinedChunkWithHeaderClass);
begin
  Assert(not IsPngChunkRegistered(ChunkClass), 'PNG chunk already registered');

  SetLength(GPngChunkClasses, Length(GPngChunkClasses) + 1);
  GPngChunkClasses[Length(GPngChunkClasses) - 1] := ChunkClass;
end;

procedure RegisterPngChunks(ChunkClasses: array of TCustomDefinedChunkWithHeaderClass);
var
  ChunkClassIndex : Integer;
begin
  for ChunkClassIndex := 0 to Length(ChunkClasses) - 1 do
    RegisterPngChunk(ChunkClasses[ChunkClassIndex]);
end;

function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomDefinedChunkWithHeaderClass;
var
  ChunkClassIndex : Integer;
begin
  Result := nil;
  for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
    if GPngChunkClasses[ChunkClassIndex].GetClassChunkName = ChunkName then
    begin
      Result := GPngChunkClasses[ChunkClassIndex];
      Exit;
    end;
end;


{ conversion }

function ColorTypeToString(Value: TColorType): string;
const
  CColorTypeNames : array [TColorType] of string = (RCStrGrayScale,
    'undefined', RCStrTrueColor, RCStrIndexedColor, RCStrGrayscaleAlpha,
    'undefined', RCStrTrueColorAlpha);
begin
  Result := CColorTypeNames[Value];
end;

function InterlaceMethodToString(Value: TInterlaceMethod): string;
const
  CInterlaceMethodNames : array [TInterlaceMethod] of string = (RCStrInterlacingNone,
    RCStrInterlacingAdam7);
begin
  Result := CInterlaceMethodNames[Value];
end;


{ zlib functions }

procedure ZCompress(Data: Pointer; Size: Integer; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
  FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

  with ZStreamRecord do
  begin
    next_in := Data;
    avail_in := Size;
    {$IFNDEF FPC}
    {$IFNDEF ZLibEx}
    zalloc := zlibAllocMem;
    zfree := zlibFreeMem;
    {$ENDIF}
    {$ENDIF}
  end;

  {$IFDEF FPC}
  if DeflateInit_(@ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during compression');
  {$ELSE}
  if DeflateInit_(ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during compression');
  {$ENDIF}

  GetMem(TempBuffer, CBufferSize);
  try
    while ZStreamRecord.avail_in > 0 do
    begin
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      deflate(ZStreamRecord, Z_NO_FLUSH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    end;

    repeat
      ZStreamRecord.next_out := TempBuffer;
      ZStreamRecord.avail_out := CBufferSize;

      ZResult := deflate(ZStreamRecord, Z_FINISH);

      Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
    until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
  finally
    FreeMem(TempBuffer);
  end;

  if deflateEnd(ZStreamRecord) > 0 then
    raise EPngError.Create('Error on stream validation');
end;

procedure ZCompress(const Input: TMemoryStream; const Output: TStream;
  Level: Byte = Z_BEST_COMPRESSION); overload;
begin
  ZCompress(Input.Memory, Input.Size, Output, Level);
end;

procedure ZDecompress(Data: Pointer; Size: Integer; const Output: TStream); overload;
const
  CBufferSize = $8000;
var
  ZStreamRecord : TZStreamRec;
  ZResult       : Integer;
  TempBuffer    : Pointer;
begin
  FillChar(ZStreamRecord, SizeOf(TZStreamRec), 0);

  with ZStreamRecord do
  begin
    next_in := Data;
    avail_in := Size;
    {$IFNDEF FPC}
    {$IFNDEF ZLibEx}
    zalloc := zlibAllocMem;
    zfree := zlibFreeMem;
    {$ENDIF}
    {$ENDIF}
  end;

  {$IFDEF FPC}
  if inflateInit_(@ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during decompression');
  {$ELSE}
  if inflateInit_(ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
    raise EPngError.Create('Error during decompression');
  {$ENDIF}
  try

    GetMem(TempBuffer, CBufferSize);
    try
      ZResult := Z_OK;

      while (ZStreamRecord.avail_in > 0) and (ZResult <> Z_STREAM_END) do
      begin
        ZStreamRecord.next_out := TempBuffer;
        ZStreamRecord.avail_out := CBufferSize;

        ZResult := inflate(ZStreamRecord, Z_NO_FLUSH);

        if ZResult < 0 then
          raise EPngError.CreateFmt('Error during decompression: %d', [ZResult]);

        Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
      end;

    finally
      FreeMem(TempBuffer);
    end;

  finally
    if inflateEnd(ZStreamRecord) > 0 then
      raise EPngError.Create('Error on stream validation');
  end;
end;

procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;
begin
  ZDecompress(Input.Memory, Input.Size, Output);
end;


{ TCustomDefinedChunk }

function TCustomDefinedChunk.GetChunkName: TChunkName;
begin
  Result := GetClassChunkName;
end;

function TCustomDefinedChunk.GetChunkNameAsString: AnsiString;
begin
  Result := AnsiString(GetClassChunkName);
end;


{ TChunkPngUnknown }

constructor TChunkPngUnknown.Create(ChunkName: TChunkName);
begin
  FChunkName := ChunkName;
  FDataStream := TMemoryStream.Create;
end;

destructor TChunkPngUnknown.Destroy;
begin
  FDataStream.Free;
  inherited;
end;

function TChunkPngUnknown.CalculateChecksum: Integer;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FDataStream.Size-1 do
    Inc(Result, PByteArray(FDataStream.Memory)[i]);
end;

procedure TChunkPngUnknown.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngUnknown then
  begin
    TChunkPngUnknown(Dest).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
    TChunkPngUnknown(Dest).FChunkName := FChunkName;
  end else
    inherited AssignTo(Dest);
end;

function TChunkPngUnknown.GetData(Index: Integer): Byte;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
begin
  if (Index < 0) or (Index >= FDataStream.Size) then
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [index]);

  Result := PByteArray(FDataStream.Memory)[Index];
end;

function TChunkPngUnknown.GetChunkSize: Cardinal;
begin
  Result := FDataStream.Size;
end;

function TChunkPngUnknown.GetChunkNameAsString: AnsiString;
begin
  Result := FChunkName;
end;

function TChunkPngUnknown.GetChunkName: TChunkName;
begin
  Result := FChunkName;
end;

procedure TChunkPngUnknown.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  Assert(Stream.Position+ChunkSize <= Stream.Size);

  FDataStream.Clear;
  FDataStream.Position := 0;
  if ChunkSize > 0 then
    FDataStream.CopyFrom(Stream, ChunkSize);
end;

procedure TChunkPngUnknown.WriteToStream(Stream: TStream);
begin
  FDataStream.Position := 0;
  Stream.CopyFrom(FDataStream, 0);
end;

procedure TChunkPngUnknown.SetData(Index: Integer; const Value: Byte);
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of Byte;
begin
  if (Index < 0) or (Index >= FDataStream.Size) then
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  PByteArray(FDataStream.Memory)[Index] := Value;
end;


{ TChunkPngImageHeader }

constructor TChunkPngImageHeader.Create;
begin
  inherited;
  FAdaptiveFilterMethods := [aafmSub, aafmUp, aafmAverage, aafmPaeth];

  ResetToDefault;
end;

procedure TChunkPngImageHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngImageHeader then
  with TChunkPngImageHeader(Dest) do
  begin
    FWidth                 := Self.FWidth;
    FHeight                := Self.FHeight;
    FBitDepth              := Self.FBitDepth;
    FColorType             := Self.FColorType;
    FCompressionMethod     := Self.FCompressionMethod;
    FFilterMethod          := Self.FFilterMethod;
    FInterlaceMethod       := Self.FInterlaceMethod;
    FAdaptiveFilterMethods := Self.FAdaptiveFilterMethods;
  end
  else
    inherited;
end;

function TChunkPngImageHeader.GetBytesPerRow: Integer;
begin
  case FColorType of
    ctGrayscale,
    ctIndexedColor:
      Result := ((FWidth * FBitDepth + $7) and not $7) shr 3;

    ctGrayscaleAlpha:
      Result := 2 * (FBitDepth shr 3) * FWidth;

    ctTrueColor:
      Result := 3 * (FBitDepth shr 3) * FWidth;

    ctTrueColorAlpha:
      Result := 4 * (FBitDepth shr 3) * FWidth;
  else
    raise EPngError.Create(RCStrUnknownColorType);
  end;
end;

class function TChunkPngImageHeader.GetClassChunkName: TChunkName;
begin
  Result := 'IHDR';
end;

function TChunkPngImageHeader.GetChunkSize: Cardinal;
begin
  Result := 13;
end;

procedure TChunkPngImageHeader.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read width
  FWidth := BigEndian.ReadCardinal(Stream);

  // read height
  FHeight := BigEndian.ReadCardinal(Stream);

  // read bit depth
  Stream.Read(FBitDepth, 1);

  // read Color type
  Stream.Read(FColorType, 1);

  // check consistency between Color type and bit depth
  case FColorType of
    ctGrayscale:
      if not (FBitDepth in [1, 2, 4, 8, 16]) then
        raise EPngError.Create(RCStrWrongBitdepth);

    ctTrueColor,
    ctGrayscaleAlpha,
    ctTrueColorAlpha:
      if not (FBitDepth in [8, 16]) then
        raise EPngError.Create(RCStrWrongBitdepth);

    ctIndexedColor:
      if not (FBitDepth in [1, 2, 4, 8]) then
        raise EPngError.Create(RCStrWrongBitdepth);
  else
    raise EPngError.Create(RCStrUnsupportedColorType);
  end;

  // read compression method
  Stream.Read(FCompressionMethod, 1);

  // check for compression method
  if FCompressionMethod <> 0 then
    raise EPngError.Create(RCStrUnsupportedCompressMethod);

  // read filter method
  Stream.Read(FFilterMethod, 1);

  // check for filter method
  if FFilterMethod <> fmAdaptiveFilter then
    raise EPngError.Create(RCStrUnsupportedFilterMethod);

  // read interlace method
  Stream.Read(FInterlaceMethod, 1);

  // check for interlace method
  if not (FInterlaceMethod in [imNone, imAdam7]) then
    raise EPngError.Create(RCStrUnsupportedInterlaceMethod);
end;

procedure TChunkPngImageHeader.WriteToStream(Stream: TStream);
begin
  // write width
  BigEndian.WriteCardinal(Stream, FWidth);

  // write height
  BigEndian.WriteCardinal(Stream, FHeight);

  // write bit depth
  Stream.Write(FBitDepth, 1);

  // write Color type
  Stream.Write(FColorType, 1);

  // write compression method
  Stream.Write(FCompressionMethod, 1);

  // write filter method
  Stream.Write(FFilterMethod, 1);

  // write interlace method
  Stream.Write(FInterlaceMethod, 1);
end;

function TChunkPngImageHeader.GetPixelByteSize: Integer;
begin
  case ColorType of
    ctGrayscale:
      if FBitDepth = 16 then
        Result := 2
      else
        Result := 1;

    ctTrueColor:
      Result := 3 * FBitDepth div 8;

    ctIndexedColor:
      Result := 1;

    ctGrayscaleAlpha:
      Result := 2 * FBitDepth div 8;

    ctTrueColorAlpha:
      Result := 4 * FBitDepth div 8;
  else
    Result := 0;
  end;
end;

function TChunkPngImageHeader.GetHasPalette: Boolean;
begin
  Result := FColorType in [ctIndexedColor];
end;

procedure TChunkPngImageHeader.ResetToDefault;
begin
  FWidth             := 0;
  FHeight            := 0;
  FBitDepth          := 8;
  FColorType         := ctTrueColor;
  FCompressionMethod := 0;
  FFilterMethod      := fmAdaptiveFilter;
  FInterlaceMethod   := imNone;
end;

procedure TChunkPngImageHeader.SetAdaptiveFilterMethods(
  const Value: TAvailableAdaptiveFilterMethods);
begin
  FAdaptiveFilterMethods := Value;
end;

procedure TChunkPngImageHeader.SetCompressionMethod(const Value: Byte);
begin
  // check for compression method
  if Value <> 0 then
    raise EPngError.Create(RCStrUnsupportedCompressMethod);
end;

procedure TChunkPngImageHeader.SetFilterMethod(const Value: TFilterMethod);
begin
  // check for filter method
  if Value <> fmAdaptiveFilter then
    raise EPngError.Create(RCStrUnsupportedFilterMethod);
end;


{ TCustomDefinedChunkWithHeader }

procedure TCustomDefinedChunkWithHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDefinedChunkWithHeader then
    with TCustomDefinedChunkWithHeader(Dest) do
    begin
      FHeader.Assign(Self.FHeader);
    end
  else
    inherited;
end;

constructor TCustomDefinedChunkWithHeader.Create(Header: TChunkPngImageHeader);
begin
  if not (Header is TChunkPngImageHeader) then
    raise EPngError.Create(RCStrHeaderInvalid);

  FHeader := Header;
  inherited Create;
end;

procedure TCustomDefinedChunkWithHeader.HeaderChanged;
begin
 // purely virtual, do nothing by default
end;


{ TChunkPngPalette }

procedure TChunkPngPalette.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPalette then
    with TChunkPngPalette(Dest) do
    begin
      SetLength(FPaletteEntries, Length(Self.FPaletteEntries));
      Move(Self.FPaletteEntries[0], FPaletteEntries[0], Length(Self.FPaletteEntries) * SizeOf(TRGB24));
    end
  else
    inherited;
end;

class function TChunkPngPalette.GetClassChunkName: TChunkName;
begin
  Result := 'PLTE';
end;

function TChunkPngPalette.GetPaletteEntry(Index: Cardinal): TRGB24;
begin
  if (Index < Count) then
    Result := FPaletteEntries[Index]
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TChunkPngPalette.SetPaletteEntry(Index: Cardinal; const Value: TRGB24);
begin
  if (Index < Count) then
    FPaletteEntries[Index] := Value
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

function TChunkPngPalette.GetCount: Cardinal;
begin
  Result := Length(FPaletteEntries);
end;

function TChunkPngPalette.GetChunkSize: Cardinal;
begin
  Result := Length(FPaletteEntries) * SizeOf(TRGB24);
end;

procedure TChunkPngPalette.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (ChunkSize mod SizeOf(TRGB24)) <> 0 then
    raise EPngError.Create(RCStrIncompletePalette);

  SetLength(FPaletteEntries, ChunkSize div SizeOf(TRGB24));

  Stream.Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
end;

procedure TChunkPngPalette.WriteToStream(Stream: TStream);
begin
  Stream.Write(FPaletteEntries[0], ChunkSize);
end;

procedure TChunkPngPalette.PaletteEntriesChanged;
begin
  // nothing todo here yet
end;

procedure TChunkPngPalette.SetCount(const Value: Cardinal);
begin
  if Value > 256 then
    raise EPngError.Create(RCStrPaletteLimited);

  if Value <> Cardinal(Length(FPaletteEntries)) then
  begin
    SetLength(FPaletteEntries, Value);
    PaletteEntriesChanged;
  end;
end;


{ TChunkPngTransparency }

procedure TChunkPngTransparency.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngTransparency then
    with TChunkPngTransparency(Dest) do
    begin
      FTransparency.Assign(Self.FTransparency);
    end
  else
    inherited;
end;

constructor TChunkPngTransparency.Create(Header: TChunkPngImageHeader);
begin
  inherited;
  case Header.ColorType of
    ctGrayscale:
      FTransparency := TPngTransparencyFormat0.Create;

    ctTrueColor:
      FTransparency := TPngTransparencyFormat2.Create;

    ctIndexedColor:
      FTransparency := TPngTransparencyFormat3.Create;
  end;
end;

destructor TChunkPngTransparency.Destroy;
begin
  FTransparency.Free;
  inherited;
end;

class function TChunkPngTransparency.GetClassChunkName: TChunkName;
begin
  Result := 'tRNS';
end;

procedure TChunkPngTransparency.HeaderChanged;
var
  OldTransparency : TCustomPngTransparency;
begin
  inherited;

  // store old transparency object
  OldTransparency := FTransparency;

  // change transparency object class
  case FHeader.ColorType of
    ctGrayscale:
      if not (FTransparency is TPngTransparencyFormat0) then
        FTransparency := TPngTransparencyFormat0.Create;

    ctTrueColor:
      if not (FTransparency is TPngTransparencyFormat2) then
        FTransparency := TPngTransparencyFormat2.Create;

    ctIndexedColor:
      if not (FTransparency is TPngTransparencyFormat3) then
        FTransparency := TPngTransparencyFormat3.Create;
  else
    FTransparency := nil;
  end;

  if (OldTransparency <> nil) and (OldTransparency <> FTransparency) then
  begin
    if (FTransparency <> nil) then
      FTransparency.Assign(OldTransparency);
    OldTransparency.Free;
  end;
end;

function TChunkPngTransparency.GetChunkSize: Cardinal;
begin
  if (FTransparency <> nil) then
    Result := FTransparency.ChunkSize
  else
    Result := 0;
end;

procedure TChunkPngTransparency.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (FTransparency <> nil) then
    FTransparency.ReadFromStream(Stream);
end;

procedure TChunkPngTransparency.WriteToStream(Stream: TStream);
begin
  // check consistency
  case FHeader.ColorType of
    ctGrayscale:
      if not (FTransparency is TPngTransparencyFormat0) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);

    ctTrueColor:
      if not (FTransparency is TPngTransparencyFormat2) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);

    ctIndexedColor:
      if not (FTransparency is TPngTransparencyFormat3) then
        raise EPngError.Create(RCStrWrongTransparencyFormat);
  end;

  if (FTransparency <> nil) then
    FTransparency.WriteToStream(Stream);
end;


{ TPngTransparencyFormat0 }

procedure TPngTransparencyFormat0.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngTransparencyFormat0 then
    with TPngTransparencyFormat0(Dest) do
    begin
      FGraySampleValue := Self.FGraySampleValue;
    end
  else
    inherited;
end;

function TPngTransparencyFormat0.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngTransparencyFormat0.ReadFromStream(Stream: TStream);
begin
  inherited;

  FGraySampleValue := BigEndian.ReadWord(Stream);
end;

procedure TPngTransparencyFormat0.WriteToStream(Stream: TStream);
begin
  inherited;

  BigEndian.WriteWord(Stream, FGraySampleValue);
end;


{ TPngTransparencyFormat2 }

procedure TPngTransparencyFormat2.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngTransparencyFormat2 then
    with TPngTransparencyFormat2(Dest) do
    begin
      FRedSampleValue := Self.FRedSampleValue;
      FBlueSampleValue := Self.FBlueSampleValue;
      FGreenSampleValue := Self.FGreenSampleValue;
    end
  else
    inherited;
end;

function TPngTransparencyFormat2.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngTransparencyFormat2.ReadFromStream(Stream: TStream);
begin
  inherited;

  FRedSampleValue  := BigEndian.ReadWord(Stream);
  FBlueSampleValue  := BigEndian.ReadWord(Stream);
  FGreenSampleValue  := BigEndian.ReadWord(Stream);
end;

procedure TPngTransparencyFormat2.WriteToStream(Stream: TStream);
begin
  inherited;

  BigEndian.WriteWord(Stream, FRedSampleValue);
  BigEndian.WriteWord(Stream, FBlueSampleValue);
  BigEndian.WriteWord(Stream, FGreenSampleValue);
end;


{ TPngTransparencyFormat3 }

procedure TPngTransparencyFormat3.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngTransparencyFormat3 then
    with TPngTransparencyFormat3(Dest) do
    begin
      SetLength(FTransparency, Length(Self.FTransparency));
      Move(Self.FTransparency[0], FTransparency, Length(FTransparency));
    end
  else
    inherited;
end;

function TPngTransparencyFormat3.GetChunkSize: Cardinal;
begin
  Result := Count;
end;

function TPngTransparencyFormat3.GetCount: Cardinal;
begin
  Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Cardinal): Byte;
begin
  if Index < Count then
    Result := FTransparency[Index]
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.ReadFromStream(Stream: TStream);
begin
  inherited;

  SetLength(FTransparency, Stream.Size - Stream.Position);
  Stream.Read(FTransparency[0], Length(FTransparency));
end;

procedure TPngTransparencyFormat3.WriteToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FTransparency[0], Length(FTransparency));
end;


{ TChunkPngPhysicalPixelDimensions }

procedure TChunkPngPhysicalPixelDimensions.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPhysicalPixelDimensions then
    with TChunkPngPhysicalPixelDimensions(Dest) do
    begin
      FPixelsPerUnitX := Self.FPixelsPerUnitX;
      FPixelsPerUnitY := Self.FPixelsPerUnitY;
      FUnit := Self.FUnit;
    end
  else
    inherited;
end;

class function TChunkPngPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
  Result := 'pHYs';
end;

function TChunkPngPhysicalPixelDimensions.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngPhysicalPixelDimensions.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read pixels per unit, X axis
  FPixelsPerUnitX := BigEndian.ReadCardinal(Stream);

  // read pixels per unit, Y axis
  FPixelsPerUnitY := BigEndian.ReadCardinal(Stream);

  // read unit
  Stream.Read(FUnit, 1);
end;

procedure TChunkPngPhysicalPixelDimensions.WriteToStream(Stream: TStream);
begin
  // write pixels per unit, X axis
  BigEndian.WriteCardinal(Stream, FPixelsPerUnitX);

  // write pixels per unit, Y axis
  BigEndian.WriteCardinal(Stream, FPixelsPerUnitY);

  // write unit
  Stream.Write(FUnit, 1);
end;


{ TChunkPngPhysicalScale }

procedure TChunkPngPhysicalScale.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPhysicalScale then
    with TChunkPngPhysicalScale(Dest) do
    begin
      FUnitSpecifier  := Self.FUnitSpecifier;
      FUnitsPerPixelX := Self.FUnitsPerPixelX;
      FUnitsPerPixelY := Self.FUnitsPerPixelY;
    end
  else
    inherited;
end;

class function TChunkPngPhysicalScale.GetClassChunkName: TChunkName;
begin
  Result := 'sCAL';
end;

function TChunkPngPhysicalScale.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TChunkPngPhysicalScale.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read unit specifier
  Stream.Read(FUnitSpecifier, 1);

  // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;

procedure TChunkPngPhysicalScale.WriteToStream(Stream: TStream);
begin
  raise EPngError.CreateFmt(RCStrChunkNotImplemented, [ChunkNameAsString]);
  // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;


{ TChunkPngImageOffset }

procedure TChunkPngImageOffset.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngImageOffset then
    with TChunkPngImageOffset(Dest) do
    begin
      FImagePositionX := Self.FImagePositionX;
      FImagePositionY := Self.FImagePositionY;
      FUnitSpecifier  := Self.FUnitSpecifier;
    end
  else
    inherited;
end;

class function TChunkPngImageOffset.GetClassChunkName: TChunkName;
begin
  Result := 'oFFs';
end;

function TChunkPngImageOffset.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngImageOffset.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read image positions
  FImagePositionX := BigEndian.ReadCardinal(Stream);
  FImagePositionY := BigEndian.ReadCardinal(Stream);

  // read unit specifier
  Stream.Read(FUnitSpecifier, 1);
end;

procedure TChunkPngImageOffset.WriteToStream(Stream: TStream);
begin
  // write image positions
  BigEndian.WriteCardinal(Stream, FImagePositionX);
  BigEndian.WriteCardinal(Stream, FImagePositionY);

  // write unit specifier
  Stream.Write(FUnitSpecifier, 1);
end;


{ TChunkPngPixelCalibrator }

procedure TChunkPngPixelCalibrator.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPixelCalibrator then
    with TChunkPngPixelCalibrator(Dest) do
    begin
      FCalibratorName    := Self.FCalibratorName;
      FOriginalZeroes[0] := Self.FOriginalZeroes[0];
      FOriginalZeroes[1] := Self.FOriginalZeroes[1];
      FEquationType      := Self.FEquationType;
      FNumberOfParams    := Self.FNumberOfParams;
      FUnitName          := Self.FUnitName;
     end
  else
    inherited;
end;

class function TChunkPngPixelCalibrator.GetClassChunkName: TChunkName;
begin
  Result := 'pCAL';
end;

function TChunkPngPixelCalibrator.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TChunkPngPixelCalibrator.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index      : Integer;
  ParamIndex : Integer;
begin
  // read keyword
  Index := 1;
  SetLength(FCalibratorName, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FCalibratorName[Index], SizeOf(Byte));
    if FCalibratorName[Index] = #0 then
    begin
      SetLength(FCalibratorName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // read original zeros
  FOriginalZeroes[0] := BigEndian.ReadCardinal(Stream);
  FOriginalZeroes[1] := BigEndian.ReadCardinal(Stream);

  // read equation type
  Stream.Read(FEquationType, 1);

  // read number of parameters
  Stream.Read(FNumberOfParams, 1);

  // read keyword
  Index := 1;
  SetLength(FUnitName, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FUnitName[Index], SizeOf(Byte));
    if FUnitName[Index] = #0 then
    begin
      SetLength(FUnitName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  for ParamIndex := 0 to FNumberOfParams - 2 do
  begin
    // yet todo
  end;
end;

procedure TChunkPngPixelCalibrator.WriteToStream(Stream: TStream);
begin
  inherited;

end;


{ TCustomChunkPngText }

procedure TCustomChunkPngText.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunkPngText then
   with TCustomChunkPngText(Dest) do
   begin
    FKeyword := Self.FKeyword;
    FText    := Self.FText;
   end
 else inherited;
end;

procedure TCustomChunkPngText.SetKeyword(const Value: AnsiString);
begin
  if FKeyword <> Value then
  begin
    FKeyword := Value;
    KeywordChanged;
  end;
end;

procedure TCustomChunkPngText.SetText(const Value: AnsiString);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TCustomChunkPngText.KeywordChanged;
begin
  // yet empty
end;

procedure TCustomChunkPngText.TextChanged;
begin
  // yet empty
end;


{ TChunkPngText }

class function TChunkPngText.GetClassChunkName: TChunkName;
begin
  Result := 'tEXt';
end;

function TChunkPngText.GetChunkSize: Cardinal;
begin
  Result := Length(FKeyword) + Length(FText) + 1;
end;

procedure TChunkPngText.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
var
  Index : Integer;
begin
  // read keyword
  Index := 1;
  SetLength(FKeyword, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FKeyword[Index], SizeOf(Byte));
    if FKeyword[Index] = #0 then
    begin
      SetLength(FKeyword, Index - 1);
      Break;
    end;
    Inc(Index);
    if (Index > High(FKeyword)) then
      raise EPngError.Create(RCStrChunkInvalid);
  end;

  // read text
  SetLength(FText, Stream.Size - Stream.Position);
  if (Stream.Position < Stream.Size) then
    Stream.Read(FText[1], SizeOf(Byte)*(Stream.Size-Stream.Position));
end;

procedure TChunkPngText.WriteToStream(Stream: TStream);
var
  Temp  : Byte;
begin
  // write keyword
  Stream.Write(FKeyword[1], Length(FKeyword));

  // write separator
  Temp := 0;
  Stream.Write(Temp, 1);

  // write text
  if (Length(FText) > 0) then
    Stream.Write(FText[1], Length(FText));
end;


{ TChunkPngCompressedText }

procedure TChunkPngCompressedText.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngCompressedText then
    with TChunkPngCompressedText(Dest) do
    begin
      FCompressionMethod := Self.FCompressionMethod;
    end
  else
    inherited;
end;

class function TChunkPngCompressedText.GetClassChunkName: TChunkName;
begin
  Result := 'zTXt';
end;

function TChunkPngCompressedText.GetChunkSize: Cardinal;
var
  OutputStream: TMemoryStream;
begin
  // calculate chunk size
  Result := Length(FKeyword) + 1 + 1; // +1 = separator, +1 = compression method

  if (Length(FText) > 0) then
  begin
    OutputStream := TMemoryStream.Create;
    try
      // compress text
      ZCompress(@FText[1], Length(FText), OutputStream);

      Inc(Result, OutputStream.Size);
    finally
      OutputStream.Free;
    end;
  end;
end;

procedure TChunkPngCompressedText.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  DataIn     : Pointer;
  DataInSize : Integer;
  Output     : TMemoryStream;
  Index      : Integer;
begin
  inherited;

  // read keyword and null separator
  Index := 1;
  SetLength(FKeyword, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FKeyword[Index], SizeOf(Byte));
    if FKeyword[Index] = #0 then
    begin
      SetLength(FKeyword, Index - 1);
      Break;
    end;
    Inc(Index);
    if (Index > High(FKeyword)) then
      raise EPngError.Create(RCStrChunkInvalid);
  end;

  // read compression method
  Stream.Read(FCompressionMethod, SizeOf(Byte));
  if FCompressionMethod <> 0 then
    raise EPngError.Create(RCStrUnsupportedCompressMethod);

  // read text
  DataInSize := Stream.Size - Stream.Position;
  GetMem(DataIn, DataInSize);
  try
    Stream.Read(DataIn^, DataInSize);

    Output := TMemoryStream.Create;
    try
      ZDecompress(DataIn, DataInSize, Output);
      SetLength(FText, Output.Size);
      Move(Output.Memory^, FText[1], Output.Size);
    finally
      Output.Free;
    end;
  finally
    FreeMem(DataIn);
  end;
end;

procedure TChunkPngCompressedText.SetCompressionMethod(const Value: Byte);
begin
  if Value <> 0 then
    raise EPngError.Create(RCStrUnsupportedCompressMethod);

  FCompressionMethod := Value;
end;

procedure TChunkPngCompressedText.WriteToStream(Stream: TStream);
var
  OutputStream: TMemoryStream;
  Temp         : Byte;
begin
  if (Length(FKeyword) = 0) then
    raise EPngError.Create(RCStrChunkInvalid);

  // write keyword
  Stream.Write(FKeyword[1], Length(FKeyword));

  // write separator
  Temp := 0;
  Stream.Write(Temp, 1);

  // write compression method
  Stream.Write(FCompressionMethod, SizeOf(Byte));

  if (Length(FText) > 0) then
  begin
    OutputStream := TMemoryStream.Create;
    try
      // compress text
      ZCompress(@FText[1], Length(FText), OutputStream);

      // write text
      Stream.Write(OutputStream.Memory^, OutputStream.Size);
    finally
      OutputStream.Free;
    end;
  end;
end;


{ TChunkPngInternationalText }
{$ifdef PNG_CHUNK_INTERNATIONAL_TEXT}
procedure TChunkPngInternationalText.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngInternationalText then
    with TChunkPngInternationalText(Dest) do
    begin
      FCompressionMethod := Self.FCompressionMethod;
      FCompressionFlag   := Self.FCompressionFlag;
      FLanguageString    := Self.FLanguageString;
      FTranslatedKeyword := Self.FTranslatedKeyword;
    end
  else
    inherited;
end;

class function TChunkPngInternationalText.GetClassChunkName: TChunkName;
begin
  Result := 'iTXt';
end;

function TChunkPngInternationalText.GetChunkSize: Cardinal;
begin
  Result := 0;
end;

procedure TChunkPngInternationalText.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index : Integer;
begin
  inherited;

  // read keyword
  Index := 1;
  SetLength(FKeyword, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FKeyword[Index], SizeOf(Byte));
    if FKeyword[Index] = #0 then
    begin
      SetLength(FKeyword, Index - 1);
      Break;
    end;
   Inc(Index);
  end;

  // read compression flag
  Stream.Read(FCompressionFlag, SizeOf(Byte));

  // read compression method
  Stream.Read(FCompressionMethod, SizeOf(Byte));

  // read language string
  Index := 1;
  SetLength(FLanguageString, 10);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FLanguageString[Index], SizeOf(Byte));
    if FLanguageString[Index] = #0 then
    begin
      SetLength(FLanguageString, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // yet todo!
  Exit;
end;

procedure TChunkPngInternationalText.WriteToStream(Stream: TStream);
begin
  // TODO
  raise EPngError.CreateFmt(RCStrChunkNotImplemented, [ChunkNameAsString]);
end;
{$endif PNG_CHUNK_INTERNATIONAL_TEXT}

{ TChunkPngImageData }

constructor TChunkPngImageData.Create;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TChunkPngImageData.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TChunkPngImageData.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngImageData then
    with TChunkPngImageData(Dest) do
    begin
      FData.Seek(0, soFromBeginning);
      Self.FData.Seek(0, soFromBeginning);
      FData.CopyFrom(Self.FData, Self.FData.Size);
      FData.Seek(0, soFromBeginning);
    end
  else
    inherited;
end;

class function TChunkPngImageData.GetClassChunkName: TChunkName;
begin
  Result := 'IDAT';
end;

function TChunkPngImageData.GetChunkSize: Cardinal;
begin
  Result := FData.Size;
end;

procedure TChunkPngImageData.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  inherited;

  FData.CopyFrom(Stream, ChunkSize);
end;

procedure TChunkPngImageData.WriteToStream(Stream: TStream);
begin
  FData.Seek(0, soFromBeginning);
  Stream.CopyFrom(FData, FData.Size);
end;


{ TChunkPngTime }

procedure TChunkPngTime.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngTime then
    with TChunkPngTime(Dest) do
    begin
      FYear   := Self.FYear;
      FMonth  := Self.FMonth;
      FDay    := Self.FDay;
      FHour   := Self.FHour;
      FMinute := Self.FMinute;
      FSecond := Self.FSecond;
    end
  else
    inherited;
end;

class function TChunkPngTime.GetClassChunkName: TChunkName;
begin
  Result := 'tIME';
end;

function TChunkPngTime.GetModifiedDateTime: TDateTime;
begin
  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

function TChunkPngTime.GetChunkSize: Cardinal;
begin
  Result := 7;
end;

procedure TChunkPngTime.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read year
  FYear := BigEndian.ReadWord(Stream);

  // read month
  Stream.Read(FMonth, SizeOf(Byte));

  // read day
  Stream.Read(FDay, SizeOf(Byte));

  // read hour
  Stream.Read(FHour, SizeOf(Byte));

  // read minute
  Stream.Read(FMinute, SizeOf(Byte));

  // read second
  Stream.Read(FSecond, SizeOf(Byte));
end;

procedure TChunkPngTime.WriteToStream(Stream: TStream);
begin
  // write year
  BigEndian.WriteWord(Stream, FYear);

  // write month
  Stream.Write(FMonth, SizeOf(Byte));

  // write day
  Stream.Write(FDay, SizeOf(Byte));

  // write hour
  Stream.Write(FHour, SizeOf(Byte));

  // write minute
  Stream.Write(FMinute, SizeOf(Byte));

  // write second
  Stream.Write(FSecond, SizeOf(Byte));
end;

procedure TChunkPngTime.SetModifiedDateTime(const Value: TDateTime);
var
  mnth : Word;
  day  : Word;
  hour : Word;
  min  : Word;
  sec  : Word;
  msec : Word;
begin
  DecodeDate(Value, FYear, mnth, day);
  FMonth := mnth;
  FDay := day;
  DecodeTime(Value, hour, min, sec, msec);
  FHour := hour;
  FMinute := min;
  FSecond := sec;
end;


{ TChunkPngEmbeddedIccProfile }

procedure TChunkPngEmbeddedIccProfile.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngEmbeddedIccProfile then
    with TChunkPngEmbeddedIccProfile(Dest) do
    begin
      FProfileName       := Self.FProfileName;
      FCompressionMethod := Self.FCompressionMethod;
    end
  else
    inherited;
end;

class function TChunkPngEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
  Result := 'iCCP';
end;

function TChunkPngEmbeddedIccProfile.GetChunkSize: Cardinal;
begin
  Result := Length(FProfileName) + 2;
end;

procedure TChunkPngEmbeddedIccProfile.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index : Integer;
begin
  // read keyword
  Index := 1;
  SetLength(FProfileName, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FProfileName[Index], SizeOf(Byte));
    if FProfileName[Index] = #0 then
    begin
      SetLength(FProfileName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // read compression method
  Stream.Read(FCompressionMethod, 1);

  // not yet completed
end;

procedure TChunkPngEmbeddedIccProfile.WriteToStream(Stream: TStream);
var
  Temp  : Byte;
begin
  // write keyword
  Stream.Write(FProfileName[1], Length(FProfileName));

  // write separator
  Temp := 0;
  Stream.Write(Temp, 1);

  // write compression method
  Stream.Write(FCompressionMethod, 1);
end;


{ TChunkPngGamma }

procedure TChunkPngGamma.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngGamma then
    with TChunkPngGamma(Dest) do
    begin
      FGamma := Self.FGamma;
    end
  else
    inherited;
end;

class function TChunkPngGamma.GetClassChunkName: TChunkName;
begin
  Result := 'gAMA';
end;

function TChunkPngGamma.GetGammaAsSingle: Single;
begin
  Result := FGamma * 1E-5;
end;

procedure TChunkPngGamma.SetGammaAsSingle(const Value: Single);
begin
  FGamma := Round(Value * 1E5);
end;

function TChunkPngGamma.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TChunkPngGamma.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read gamma
  FGamma := BigEndian.ReadCardinal(Stream);
end;

procedure TChunkPngGamma.WriteToStream(Stream: TStream);
begin
  // write gamma
  BigEndian.WriteCardinal(Stream, FGamma);
end;


{ TChunkPngStandardColorSpaceRGB }

procedure TChunkPngStandardColorSpaceRGB.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngStandardColorSpaceRGB then
    with TChunkPngStandardColorSpaceRGB(Dest) do
    begin
      FRenderingIntent := Self.FRenderingIntent;
    end
  else
    inherited;
end;

class function TChunkPngStandardColorSpaceRGB.GetClassChunkName: TChunkName;
begin
  Result := 'sRGB';
end;

function TChunkPngStandardColorSpaceRGB.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TChunkPngStandardColorSpaceRGB.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read rendering intent
  Stream.Read(FRenderingIntent, SizeOf(Byte));
end;

procedure TChunkPngStandardColorSpaceRGB.WriteToStream(Stream: TStream);
begin
  // write rendering intent
  Stream.Write(FRenderingIntent, SizeOf(Byte));
end;


{ TChunkPngPrimaryChromaticities }

class function TChunkPngPrimaryChromaticities.GetClassChunkName: TChunkName;
begin
  Result := 'cHRM';
end;

procedure TChunkPngPrimaryChromaticities.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngPrimaryChromaticities then
    with TChunkPngPrimaryChromaticities(Dest) do
    begin
      FWhiteX := Self.FWhiteX;
      FWhiteY := Self.FWhiteY;
      FRedX   := Self.FRedX;
      FRedY   := Self.FRedY;
      FGreenX := Self.FGreenX;
      FGreenY := Self.FGreenY;
      FBlueX  := Self.FBlueX;
      FBlueY  := Self.FBlueY;
    end
  else
    inherited;
end;

function TChunkPngPrimaryChromaticities.GetBlueX: Single;
begin
  Result := FBlueX * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetBlueY: Single;
begin
  Result := FBlueY * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetGreenX: Single;
begin
  Result := FGreenX * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetGreenY: Single;
begin
  Result := FGreenY * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetRedX: Single;
begin
  Result := FRedX * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetRedY: Single;
begin
  Result := FRedY * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetWhiteX: Single;
begin
  Result := FWhiteX * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetWhiteY: Single;
begin
  Result := FWhiteY * 1E-5;
end;

function TChunkPngPrimaryChromaticities.GetChunkSize: Cardinal;
begin
  Result := 32;
end;

procedure TChunkPngPrimaryChromaticities.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read white point x
  FWhiteX := BigEndian.ReadCardinal(Stream);

  // read white point y
  FWhiteY := BigEndian.ReadCardinal(Stream);

  // read red x
  FRedX := BigEndian.ReadCardinal(Stream);

  // read red y
  FRedY := BigEndian.ReadCardinal(Stream);

  // read green x
  FGreenX := BigEndian.ReadCardinal(Stream);

  // read green y
  FGreenY := BigEndian.ReadCardinal(Stream);

  // read blue x
  FBlueX := BigEndian.ReadCardinal(Stream);

  // read blue y
  FBlueY := BigEndian.ReadCardinal(Stream);
end;

procedure TChunkPngPrimaryChromaticities.WriteToStream(Stream: TStream);
begin
  // write white point x
  BigEndian.WriteCardinal(Stream, FWhiteX);

  // write white point y
  BigEndian.WriteCardinal(Stream, FWhiteY);

  // write red x
  BigEndian.WriteCardinal(Stream, FRedX);

  // write red y
  BigEndian.WriteCardinal(Stream, FRedY);

  // write green x
  BigEndian.WriteCardinal(Stream, FGreenX);

  // write green y
  BigEndian.WriteCardinal(Stream, FGreenY);

  // write blue x
  BigEndian.WriteCardinal(Stream, FBlueX);

  // write blue y
  BigEndian.WriteCardinal(Stream, FBlueY);
end;

procedure TChunkPngPrimaryChromaticities.SetBlueX(const Value: Single);
begin
  FBlueX := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetBlueY(const Value: Single);
begin
  FBlueY := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenX(const Value: Single);
begin
  FGreenX := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenY(const Value: Single);
begin
  FGreenY := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetRedX(const Value: Single);
begin
  FRedX := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetRedY(const Value: Single);
begin
  FRedY := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteX(const Value: Single);
begin
  FWhiteX := Round(Value * 1E5);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteY(const Value: Single);
begin
  FWhiteY := Round(Value * 1E5);
end;


{ TPngSignificantBitsFormat0 }

constructor TPngSignificantBitsFormat0.Create(BitDepth: Integer = 8);
begin
  inherited;
  FGrayBits := BitDepth;
end;

procedure TPngSignificantBitsFormat0.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat0 then
    with TPngSignificantBitsFormat0(Dest) do
    begin
      FGrayBits := Self.FGrayBits;
    end
  else
    inherited;
end;

class function TPngSignificantBitsFormat0.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngSignificantBitsFormat0.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FGrayBits, 1);
end;

procedure TPngSignificantBitsFormat0.WriteToStream(Stream: TStream);
begin
  Stream.Write(FGrayBits, 1);
end;


{ TPngSignificantBitsFormat23 }

constructor TPngSignificantBitsFormat23.Create(BitDepth: Integer = 8);
begin
  inherited;
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
end;

procedure TPngSignificantBitsFormat23.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(Dest) do
    begin
      FRedBits   := Self.FRedBits;
      FBlueBits  := Self.FBlueBits;
      FGreenBits := Self.FGreenBits;
    end
  else
    inherited;
end;

class function TPngSignificantBitsFormat23.GetChunkSize: Cardinal;
begin
  Result := 3;
end;

procedure TPngSignificantBitsFormat23.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
end;

procedure TPngSignificantBitsFormat23.WriteToStream(Stream: TStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
end;


{ TPngSignificantBitsFormat4 }

constructor TPngSignificantBitsFormat4.Create(BitDepth: Integer = 8);
begin
  inherited;
  FGrayBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat4.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat4 then
    with TPngSignificantBitsFormat4(Dest) do
    begin
      FGrayBits  := Self.FGrayBits;
      FAlphaBits := Self.FAlphaBits;
    end
  else if Dest is TPngSignificantBitsFormat0 then
    with TPngSignificantBitsFormat0(Dest) do
      FGrayBits  := Self.FGrayBits
  else
    inherited;
end;

class function TPngSignificantBitsFormat4.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngSignificantBitsFormat4.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FGrayBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat4.WriteToStream(Stream: TStream);
begin
  Stream.Write(FGrayBits, 1);
  Stream.Write(FAlphaBits, 1);
end;


{ TPngSignificantBitsFormat6 }

constructor TPngSignificantBitsFormat6.Create(BitDepth: Integer = 8);
begin
  inherited;
  FRedBits := BitDepth;
  FGreenBits := BitDepth;
  FBlueBits := BitDepth;
  FAlphaBits := BitDepth;
end;

procedure TPngSignificantBitsFormat6.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngSignificantBitsFormat6 then
    with TPngSignificantBitsFormat6(Dest) do
    begin
      FRedBits   := Self.FRedBits;
      FBlueBits  := Self.FBlueBits;
      FGreenBits := Self.FGreenBits;
      FAlphaBits := Self.FAlphaBits;
    end
  else if Dest is TPngSignificantBitsFormat23 then
    with TPngSignificantBitsFormat23(Dest) do
    begin
      FRedBits   := Self.FRedBits;
      FBlueBits  := Self.FBlueBits;
      FGreenBits := Self.FGreenBits;
    end
  else
    inherited;
end;

class function TPngSignificantBitsFormat6.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TPngSignificantBitsFormat6.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FRedBits, 1);
  Stream.Read(FGreenBits, 1);
  Stream.Read(FBlueBits, 1);
  Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat6.WriteToStream(Stream: TStream);
begin
  Stream.Write(FRedBits, 1);
  Stream.Write(FGreenBits, 1);
  Stream.Write(FBlueBits, 1);
  Stream.Write(FAlphaBits, 1);
end;


{ TChunkPngSignificantBits }

procedure TChunkPngSignificantBits.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngSignificantBits then
    with TChunkPngSignificantBits(Dest) do
    begin
      FSignificantBits.Assign(Self.FSignificantBits);
    end
  else
    inherited;
end;

constructor TChunkPngSignificantBits.Create(Header: TChunkPngImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale:
      FSignificantBits := TPngSignificantBitsFormat0.Create(Header.BitDepth);

    ctTrueColor,
    ctIndexedColor:
      FSignificantBits := TPngSignificantBitsFormat23.Create(Header.BitDepth);

    ctGrayscaleAlpha:
      FSignificantBits := TPngSignificantBitsFormat4.Create(Header.BitDepth);

    ctTrueColorAlpha:
      FSignificantBits := TPngSignificantBitsFormat6.Create(Header.BitDepth);
  end;
end;

destructor TChunkPngSignificantBits.Destroy;
begin
  FSignificantBits.Free;

  inherited;
end;

class function TChunkPngSignificantBits.GetClassChunkName: TChunkName;
begin
  Result := 'sBIT';
end;

procedure TChunkPngSignificantBits.HeaderChanged;
var
  OldSignificantBits : TCustomPngSignificantBits;
begin
  inherited;

  // store old SignificantBits object
  OldSignificantBits := FSignificantBits;

  // change SignificantBits object class
  case FHeader.ColorType of
    ctGrayscale:
      if not (FSignificantBits is TPngSignificantBitsFormat0) then
        FSignificantBits := TPngSignificantBitsFormat0.Create(FHeader.BitDepth);

    ctTrueColor, ctIndexedColor:
      if not (FSignificantBits is TPngSignificantBitsFormat23) then
        FSignificantBits := TPngSignificantBitsFormat23.Create(FHeader.BitDepth);

    ctTrueColorAlpha:
      if not (FSignificantBits is TPngSignificantBitsFormat4) then
        FSignificantBits := TPngSignificantBitsFormat4.Create(FHeader.BitDepth);

    ctGrayscaleAlpha :
      if not (FSignificantBits is TPngSignificantBitsFormat6) then
        FSignificantBits := TPngSignificantBitsFormat6.Create(FHeader.BitDepth);
  else
    FSignificantBits := nil;
  end;

  if (OldSignificantBits <> nil) and (OldSignificantBits <> FSignificantBits) then
  begin
    if (FSignificantBits <> nil) then
      FSignificantBits.Assign(OldSignificantBits);
    OldSignificantBits.Free;
  end;
end;

function TChunkPngSignificantBits.GetChunkSize: Cardinal;
begin
  if (FSignificantBits <> nil) then
    Result := FSignificantBits.GetChunkSize
  else
    Result := 0;
end;

procedure TChunkPngSignificantBits.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  if (FSignificantBits <> nil) then
    FSignificantBits.ReadFromStream(Stream);
end;

procedure TChunkPngSignificantBits.WriteToStream(Stream: TStream);
begin
  if (FSignificantBits <> nil) then
    FSignificantBits.WriteToStream(Stream);
end;


{ TPngBackgroundColorFormat04 }

procedure TPngBackgroundColorFormat04.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngBackgroundColorFormat04 then
    with TPngBackgroundColorFormat04(Dest) do
    begin
      FGraySampleValue := Self.FGraySampleValue;
    end
  else
    inherited;
end;

class function TPngBackgroundColorFormat04.GetChunkSize: Cardinal;
begin
  Result := 2;
end;

procedure TPngBackgroundColorFormat04.ReadFromStream(Stream: TStream);
begin
  FGraySampleValue := BigEndian.ReadWord(Stream);
end;

procedure TPngBackgroundColorFormat04.WriteToStream(Stream: TStream);
begin
  BigEndian.WriteWord(Stream, FGraySampleValue);
end;


{ TPngBackgroundColorFormat26 }

procedure TPngBackgroundColorFormat26.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngBackgroundColorFormat26 then
    with TPngBackgroundColorFormat26(Dest) do
    begin
      FRedSampleValue := Self.FRedSampleValue;
      FBlueSampleValue := Self.FBlueSampleValue;
      FGreenSampleValue := Self.FGreenSampleValue;
    end
  else
    inherited;
end;

class function TPngBackgroundColorFormat26.GetChunkSize: Cardinal;
begin
  Result := 6;
end;

procedure TPngBackgroundColorFormat26.ReadFromStream(Stream: TStream);
begin
  FRedSampleValue := BigEndian.ReadWord(Stream);
  FGreenSampleValue := BigEndian.ReadWord(Stream);
  FBlueSampleValue := BigEndian.ReadWord(Stream);
end;

procedure TPngBackgroundColorFormat26.WriteToStream(Stream: TStream);
begin
  BigEndian.WriteWord(Stream, FRedSampleValue);
  BigEndian.WriteWord(Stream, FGreenSampleValue);
  BigEndian.WriteWord(Stream, FBlueSampleValue);
end;


{ TPngBackgroundColorFormat3 }

procedure TPngBackgroundColorFormat3.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngBackgroundColorFormat3 then
    with TPngBackgroundColorFormat3(Dest) do
    begin
      FIndex := Self.FIndex;
    end
  else
    inherited;
end;

class function TPngBackgroundColorFormat3.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngBackgroundColorFormat3.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColorFormat3.WriteToStream(Stream: TStream);
begin
  Stream.Write(FIndex, 1);
end;


{ TChunkPngBackgroundColor }

procedure TChunkPngBackgroundColor.AssignTo(Dest: TPersistent);
begin
  if Dest is TChunkPngBackgroundColor then
    with TChunkPngBackgroundColor(Dest) do
    begin
      FBackground.Assign(Self.FBackground);
    end
  else
    inherited;
end;

constructor TChunkPngBackgroundColor.Create(Header: TChunkPngImageHeader);
begin
  inherited;

  case Header.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      FBackground := TPngBackgroundColorFormat04.Create;

    ctTrueColor, ctTrueColorAlpha:
      FBackground := TPngBackgroundColorFormat26.Create;

    ctIndexedColor:
      FBackground := TPngBackgroundColorFormat3.Create;
  end;
end;

destructor TChunkPngBackgroundColor.Destroy;
begin
  FBackground.Free;
  inherited;
end;

class function TChunkPngBackgroundColor.GetClassChunkName: TChunkName;
begin
  Result := 'bKGD';
end;

procedure TChunkPngBackgroundColor.HeaderChanged;
var
  OldBackground : TCustomPngBackgroundColor;
begin
  inherited;

  // store old background object
  OldBackground := FBackground;

  // change background object class
  case FHeader.ColorType of
    ctGrayscale, ctGrayscaleAlpha:
      if not (FBackground is TPngBackgroundColorFormat04) then
        FBackground := TPngBackgroundColorFormat04.Create;

    ctTrueColor, ctTrueColorAlpha :
      if not (FBackground is TPngBackgroundColorFormat26) then
        FBackground := TPngBackgroundColorFormat26.Create;

    ctIndexedColor :
      if not (FBackground is TPngBackgroundColorFormat3) then
        FBackground := TPngBackgroundColorFormat3.Create;
  else
    FBackground := nil;
  end;

  if (OldBackground <> nil) and (OldBackground <> FBackground)  then
  begin
    if (FBackground <> nil) then
      FBackground.Assign(OldBackground);
    OldBackground.Free;
  end;
end;

function TChunkPngBackgroundColor.GetChunkSize: Cardinal;
begin
  if (FBackground <> nil) then
    Result := FBackground.GetChunkSize
  else
    Result := 0;
end;

procedure TChunkPngBackgroundColor.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  if (FBackground <> nil) then
    FBackground.ReadFromStream(Stream);
end;

procedure TChunkPngBackgroundColor.WriteToStream(Stream: TStream);
begin
  if (FBackground <> nil) then
    FBackground.WriteToStream(Stream);
end;


{ TChunkPngImageHistogram }

class function TChunkPngImageHistogram.GetClassChunkName: TChunkName;
begin
  Result := 'hIST';
end;

function TChunkPngImageHistogram.GetCount: Cardinal;
begin
  Result := Length(FHistogram);
end;

function TChunkPngImageHistogram.GetFrequency(Index: Cardinal): Word;
begin
  if Index < Count then
    Result := FHistogram[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TChunkPngImageHistogram.GetChunkSize: Cardinal;
begin
  Result := Count * SizeOf(Word);
end;

procedure TChunkPngImageHistogram.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
var
  Index : Integer;
begin
  // check size
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // adjust histogram array size
  SetLength(FHistogram, ChunkSize div 2);

  // read histogram data
  for Index := 0 to Length(FHistogram) - 1 do
    FHistogram[Index] := BigEndian.ReadWord(Stream);
end;

procedure TChunkPngImageHistogram.WriteToStream(Stream: TStream);
var
  Index : Integer;
begin
  // write histogram data
  for Index := 0 to Length(FHistogram) - 1 do
    BigEndian.WriteWord(Stream, FHistogram[Index]);
end;


{ TChunkPngSuggestedPalette }
{$ifdef PNG_CHUNK_SUGGESTED_PALETTE}
constructor TChunkPngSuggestedPalette.Create(Header: TChunkPngImageHeader);
begin
  inherited;
  FData := nil;
  FCount := 0;
end;

class function TChunkPngSuggestedPalette.GetClassChunkName: TChunkName;
begin
  Result := 'sPLT';
end;

function TChunkPngSuggestedPalette.GetCount: Cardinal;
begin
  Result := FCount;
end;

function TChunkPngSuggestedPalette.GetChunkSize: Cardinal;
begin
  Result := Cardinal(Length(FPaletteName)) + 2 +
    (4 * (FSampleDepth shr 3) + 2) * Count;
end;

procedure TChunkPngSuggestedPalette.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index      : Integer;
  DataSize   : Integer;
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read palette name
  Index := 1;
  SetLength(FPaletteName, 80);
  while (Stream.Position < ChunkSize) do
  begin
    Stream.Read(FPaletteName[Index], SizeOf(Byte));
    if FPaletteName[Index] = #0 then
    begin
      SetLength(FPaletteName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // read sample depth
  Stream.Read(FSampleDepth, 1);

  DataSize := Integer(ChunkSize) - Length(FPaletteName) - 2;
  Assert(DataSize >= 0);
  Assert(DataSize mod 2 = 0);
  Assert(DataSize mod (4 * (FSampleDepth shr 3) + 2) = 0);
  FCount := DataSize div (4 * (FSampleDepth shr 3) + 2);
  ReallocMem(FData, DataSize);

  if FSampleDepth = 8 then
    for Index := 0 to FCount - 1 do
      with PSuggestedPalette8ByteArray(FData)^[Index] do
      begin
        Stream.Read(Red, 1);
        Stream.Read(Green, 1);
        Stream.Read(Blue, 1);
        Stream.Read(Alpha, 1);
        Frequency := BigEndian.ReadWord(Stream);
      end
  else if FSampleDepth = 16 then
    for Index := 0 to FCount - 1 do
      with PSuggestedPalette16ByteArray(FData)^[Index] do
      begin
        Red := BigEndian.ReadWord(Stream);
        Green := BigEndian.ReadWord(Stream);
        Blue := BigEndian.ReadWord(Stream);
        Alpha := BigEndian.ReadWord(Stream);
        Frequency := BigEndian.ReadWord(Stream);
      end;
end;

procedure TChunkPngSuggestedPalette.WriteToStream(Stream: TStream);
begin
  // TODO
  raise EPngError.CreateFmt(RCStrChunkNotImplemented, [ChunkNameAsString]);
end;
{$endif PNG_CHUNK_SUGGESTED_PALETTE}


{ TChunkList }

destructor TChunkList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TChunkList.Add(Item: TCustomChunk);
begin
  SetLength(FChunks, Length(FChunks) + 1);
  FChunks[Length(FChunks) - 1] := Item;
end;

procedure TChunkList.AssignTo(Dest: TPersistent);
var
  Index      : Integer;
  ChunkClass : TCustomDefinedChunkWithHeaderClass;
begin
  if Dest is TChunkList then
    with TChunkList(Dest) do
    begin
      Clear;
      SetLength(FChunks, Self.Count);
      for Index := 0 to Self.Count - 1 do
        if Self.FChunks[Index] is TCustomDefinedChunkWithHeader then
        begin
          ChunkClass := TCustomDefinedChunkWithHeaderClass(Self.FChunks[Index].ClassType);
          FChunks[Index] := ChunkClass.Create(TCustomDefinedChunkWithHeader(Self.FChunks[Index]).FHeader);
          FChunks[Index].Assign(Self.FChunks[Index]);
        end
        else
          inherited;
    end
    else
      inherited;
end;

procedure TChunkList.Clear;
var
  Index : Integer;
begin
  for Index := 0 to Count - 1 do
    FChunks[Index].Free;
  SetLength(FChunks, 0)
end;

procedure TChunkList.Delete(Index: Cardinal);
begin
  if Index >= Count then
    raise EPngError.Create(RCStrEmptyChunkList);
  FChunks[Index].Free;
  if Index < Count then
    System.Move(FChunks[Index + 1], FChunks[Index], (Count - Index) * SizeOf(Pointer));
  SetLength(FChunks, Length(FChunks) - 1);
end;

function TChunkList.GetChunk(Index: Integer): TCustomChunk;
begin
  if Cardinal(Index) >= Cardinal(Count) then
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    Result := FChunks[Index];
end;

function TChunkList.GetCount: Cardinal;
begin
  Result := Length(FChunks);
end;

function TChunkList.IndexOf(Item: TCustomChunk): Integer;
begin
  for Result := 0 to Count - 1 do
    if FChunks[Result] = Item then
      Exit;
  Result := -1;
end;

procedure TChunkList.Remove(Item: TCustomChunk);
begin
  Delete(IndexOf(Item));
end;


{ TCustomPngCoder }

constructor TCustomPngCoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil; Transparency : TCustomPngTransparency = nil);
begin
  FStream       := Stream;
  FHeader       := Header;
  FGamma        := Gamma;
  FPalette      := Palette;
  FTransparency := Transparency;
  FMappingTable := nil;
  FAlphaTable   := nil;
  BuildMappingTables;
  inherited Create;
end;

destructor TCustomPngCoder.Destroy;
begin
  Dispose(FMappingTable);
  Dispose(FAlphaTable);
  inherited;
end;

procedure TCustomPngCoder.BuildMappingTables;
var
  Index        : Integer;
  Palette      : PRGB24Array;
  FracVal      : Single;
  Color        : TRGB24;
  MaxByte      : Byte;
  PreCalcGamma : Extended;
const
  COne255th : Extended = 1 / 255;
begin
  if FHeader.HasPalette then
  begin
    if (FPalette <> nil) then
    begin
      GetMem(FMappingTable, FPalette.Count * SizeOf(TRGB24));
      Palette := PRGB24Array(FMappingTable);

      if (FGamma <> nil) then
      begin
        PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
        for Index := 0 to FPalette.Count - 1 do
        begin
          Color := FPalette.PaletteEntry[Index];
          Palette[Index].R := Round(Power((Color.R * COne255th), PreCalcGamma) * 255);
          Palette[Index].G := Round(Power((Color.G * COne255th), PreCalcGamma) * 255);
          Palette[Index].B := Round(Power((Color.B * COne255th), PreCalcGamma) * 255);
        end;
      end
      else
        for Index := 0 to FPalette.Count - 1 do
          Palette[Index] := FPalette.PaletteEntry[Index];
    end
    else
    begin
      // create gray scale palette
      GetMem(FMappingTable, 256 * SizeOf(TRGB24));
      Palette := PRGB24Array(FMappingTable);
      MaxByte := ((1 shl FHeader.BitDepth) - 1) and $FF;
      FracVal := 1 / MaxByte;

      if (FGamma <> nil) then
      begin
        PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
        for Index := 0 to FPalette.Count - 1 do
        begin
          Palette[Index].R := Round(Power(Index * FracVal, PreCalcGamma) * 255);
          Palette[Index].G := Palette[Index].R;
          Palette[Index].B := Palette[Index].B;
        end;
      end
      else
      begin
        for Index := 0 to MaxByte do
        begin
          Palette[Index].R := Round(255 * (Index * FracVal));
          Palette[Index].G := Palette[Index].R;
          Palette[Index].B := Palette[Index].R;
        end;
      end;
    end;

   // build alpha table
   GetMem(FAlphaTable, 256);
   FillChar(FAlphaTable^, 256, $FF);

   // eventually fill alpha table
   if FTransparency is TPngTransparencyFormat3 then
     with TPngTransparencyFormat3(FTransparency) do
       for Index := 0 to Count - 1 do
         FAlphaTable[Index] := Transparency[Index];
  end
  else
  begin
    GetMem(FMappingTable, 256);
    if (FGamma <> nil) and (FGamma.Gamma <> 0) then
    begin
      PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
      for Index := 0 to $FF do
        FMappingTable[Index] := Round(Power((Index * COne255th), PreCalcGamma) * 255);
    end
    else
      for Index := 0 to $FF do
        FMappingTable[Index] := Index;
  end;
end;

procedure TCustomPngCoder.DecodeFilterSub(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] + CurrentRow[Index - PixelByteSize]) and $FF;
{$ELSE}
asm
{$IFDEF Target_x64}
  // RCX = Self
  // RDX = CurrentRow
  // R9  = BytesPerRow
        ADD     RDX, 1
        MOV     RAX, RDX
        MOV     RCX, BytesPerRow
        ADD     RAX, PixelByteSize
        SUB     RCX, PixelByteSize
        LEA     RAX, [RAX + RCX]
        LEA     RDX, [RDX + RCX]
        NEG     RCX
        JNL     @Done

@Start:
        MOV     R8B, [RAX + RCX].Byte
        ADD     R8B, [RDX + RCX].Byte
        MOV     [RAX + RCX].Byte, R8B

        ADD     RCX, 1
        JS      @Start

@Done:
{$ENDIF}
{$IFDEF Target_x86}
        ADD     EDX, 1
        MOV     EAX, EDX
        MOV     ECX, BytesPerRow.DWORD
        ADD     EAX, PixelByteSize.DWORD
        SUB     ECX, PixelByteSize.DWORD
        LEA     EAX, [EAX + ECX]
        LEA     EDX, [EDX + ECX]
        NEG     ECX
        JNL     @Done

        PUSH    EBX

@Start:
        MOV     BL, [EAX + ECX].Byte
        ADD     BL, [EDX + ECX].Byte
        MOV     [EAX + ECX].Byte, BL

        ADD     ECX, 1
        JS      @Start

        POP     EBX

@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterUp(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
  for Index := 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index]) and $FF;
{$ELSE}
asm
{$IFDEF Target_x64}
        // RCX = Self
        // RDX = CurrentRow
        // R8  = PreviousRow
        // R9  = BytesPerRow
        MOV     RAX, RDX
        MOV     RDX, R8
        MOV     RCX, BytesPerRow
        LEA     RAX, [RAX + RCX + 1]
        LEA     RDX, [RDX + RCX + 1]
        NEG     RCX
        JNL     @Done

@Start:
        MOV     R8B, [RAX + RCX].Byte
        ADD     R8B, [RDX + RCX].Byte
        MOV     [RAX + RCX].Byte, R8B

        ADD     RCX, 1
        JS      @Start

@Done:
{$ENDIF}
{$IFDEF Target_x86}
        MOV     EAX, EDX
        MOV     EDX, ECX
        MOV     ECX, BytesPerRow.DWORD
        LEA     EAX, [EAX + ECX + 1]
        LEA     EDX, [EDX + ECX + 1]
        NEG     ECX
        JNL     @Done

        PUSH    EBX

@Start:
        MOV     BL, [EAX + ECX].Byte
        ADD     BL, [EDX + ECX].Byte
        MOV     [EAX + ECX].Byte, BL

        ADD     ECX, 1
        JS      @Start

        POP     EBX

@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index : Integer;
begin
  for Index := 1 to PixelByteSize do
    CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] +
      (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

function PaethPredictor(a, b, c: Byte): Integer; {$IFNDEF TARGET_x64} pascal; {$ENDIF}
{$IFDEF PUREPASCAL}
var
  DistA, DistB, DistC: Integer;
begin
  DistA := Abs(b - c);
  DistB := Abs(a - c);
  DistC := Abs(a + b - c * 2);

  if (DistA <= DistB) and (DistA <= DistC) then Result := a else
  if DistB <= DistC then
    Result := b
  else
    Result := c;
{$ELSE}
asm
{$IFDEF TARGET_x64}
        // RCX = a
        // RDX = b
        // R8  = c

        // calculate DistA = Abs(b - c)
        MOVZX   RAX, DL          // RAX = b
        SUB     RAX, R8          // RAX = b - c
        MOV     R10, RAX         // R10 = b - c
        JAE     @PositiveDistA   // if  R10 >= 0 then
        NOT     RAX              //   ...
        INC     RAX              //   RAX = Abs(b - c) = DistA

        @PositiveDistA:

        // calculate DistB = Abs(a - c)
        MOVZX   R11, CL          // R11 = a
        SUB     R11, R8          // R11 = a - c
        MOV     R9, R11          // R9 = a - c
        JAE     @PositiveDistB   // if  R9 >= 0 then
        NOT     R11              //   ...
        INC     R11              //   R11 = Abs(a - c) = DistB

        @PositiveDistB:

        // calculate DistC = Abs(a + b - c * 2)
        ADD     R10, R9          // R10 = b - c + a - c = a + b - 2 * c
        JNL     @PositiveDistC   // if R10 >= 0 then
        NOT     R10              //   ...
        INC     R10              //   R10 = Abs(a + b - c * 2) = DistC

        @PositiveDistC:

        MOV     R9, RAX          // R9 = DistA
        SUB     R9, R11          // R9 = DistA - DistB
        JA      @NextCheck       // if (DistA <= DistB) then
        MOV     R9, RAX          // R9 = DistA
        SUB     R9, R10          // R9 = DistA - DistC
        JA      @NextCheck       // if (DistA <= DistC) then

        MOV     RAX, RCX         // RAX = a
        JMP     @Done            // Exit

        @NextCheck:
        MOV     R9, R11          // R9 = DistB
        SUB     R9, R10          // R9 = DistB - DistC
        JA      @ResultC         // if (DistB <= DistC) then

        MOV     RAX, RDX         // RAX = b
        JMP     @Done

        @ResultC:
        MOV     RAX, R8          // RAX = c

        @Done:
{$ELSE}
        MOVZX   EDX, c
        PUSH    EBX
        MOVZX   EAX, b
        SUB     EAX, EDX
        JAE     @PositiveDistA
        NOT     EAX
        INC     EAX

@PositiveDistA:
        MOVZX   EBX, a
        SUB     EBX, EDX
        JAE     @PositiveDistB
        NOT     EBX
        INC     EBX

@PositiveDistB:
        MOVZX   ECX, a
        SUB     ECX, EDX
        MOVZX   EDX, b
        ADD     ECX, EDX
        MOVZX   EDX, c
        SUB     ECX, EDX
        JAE     @PositiveDistC
        NOT     ECX
        INC     ECX

@PositiveDistC:
        MOV     EDX, EAX
        SUB     EDX, EBX
        JA      @NextCheck
        MOV     EDX, EAX
        SUB     EDX, ECX
        JA      @NextCheck

        MOVZX   EDX, a
        MOV     Result, EDX
        JMP     @Done

@NextCheck:
        MOV     EDX, EBX
        SUB     EDX, ECX
        JA      @ResultC

        MOVZX   EDX, b
        MOV     Result, EDX
        JMP     @Done

@ResultC:
        MOVZX   EDX, c
        MOV     Result, EDX

@Done:
        POP     EBX
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index : Integer;
begin
  DecodeFilterUp(CurrentRow, PreviousRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] +
      PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
      PreviousRow[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  // copy first pixel
  Move(CurrentRow[1], OutputRow[1], PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - CurrentRow[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  for Index := 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  for Index := 1 to PixelByteSize do
    OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  EncodeFilterUp(CurrentRow, PreviousRow, OutputRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] -
      PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
      PreviousRow[Index - PixelByteSize])) and $FF;
end;


{ TCustomPngDecoder }

procedure TCustomPngDecoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
  case FilterMethod of
    afmNone    : ;

    afmSub     : DecodeFilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmUp      : DecodeFilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmAverage : DecodeFilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmPaeth   : DecodeFilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  else
    raise EPngError.Create(RCStrUnsupportedFilter);
  end;
end;

procedure TCustomPngDecoder.EncodeFilterRow(CurrentRow, PreviousRow, OutputRow,
  TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
  raise Exception.Create('Class is only meant for decoding');
end;


{ TCustomPngEncoder }

function CalculateRowSum(CurrentRow: PByteArray; BytesPerRow: Integer): Cardinal;
var
  Index : Integer;
begin
  Result := 0;
  for Index := 1 to BytesPerRow do
    Result := Result + Cardinal(Abs(SmallInt(CurrentRow[Index])));
end;

procedure TCustomPngEncoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
var
  PixelIndex : Integer;
  CurrentSum : Cardinal;
  BestSum    : Cardinal;
begin
  BestSum := 0;
  OutputRow^[0] := 0;
  for PixelIndex := 1 to BytesPerRow do
    BestSum := BestSum + CurrentRow[PixelIndex];
  Move(CurrentRow^[1], OutputRow^[1], BytesPerRow);

  // check whether sub pre filter shall be used
  if aafmSub in FHeader.AdaptiveFilterMethods then
  begin
    // calculate sub filter
    EncodeFilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if sub filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 1;
    end;
  end;

  // check whether up pre filter shall be used
  if aafmUp in FHeader.AdaptiveFilterMethods then
  begin
    // calculate up filter
    EncodeFilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if up filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 2;
    end;
  end;

  // check whether average pre filter shall be used
  if aafmAverage in FHeader.AdaptiveFilterMethods then
  begin
    // calculate average filter
    EncodeFilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if average filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 3;
    end;
  end;

  // check whether paeth pre filter shall be used
  if aafmPaeth in FHeader.AdaptiveFilterMethods then
  begin
    // calculate paeth filter
    EncodeFilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if paeth filter is the current best filter
    if CurrentSum < BestSum then
    begin
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 4;
    end;
  end;
end;

procedure TCustomPngEncoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
  raise Exception.Create('Class is only meant for encoding');
end;


{ TCustomPngTranscoder }

constructor TCustomPngTranscoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil; Transparency: TCustomPngTransparency = nil);
begin
  inherited;
  GetMem(FRowBuffer[0], FHeader.BytesPerRow + 1);
  GetMem(FRowBuffer[1], FHeader.BytesPerRow + 1);
end;

destructor TCustomPngTranscoder.Destroy;
begin
  Dispose(FRowBuffer[0]);
  Dispose(FRowBuffer[1]);
  inherited;
end;

procedure TCustomPngTranscoder.DecodeFilterRow(
  FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
begin
  case FilterMethod of
    afmNone    : ;

    afmSub     : DecodeFilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmUp      : DecodeFilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmAverage : DecodeFilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmPaeth   : DecodeFilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  else
    raise EPngError.Create(RCStrUnsupportedFilter);
  end;
end;

procedure TCustomPngTranscoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
var
  PixelIndex : Integer;
  CurrentSum : Cardinal;
  BestSum    : Cardinal;
begin
  BestSum := 0;
  OutputRow^[0] := 0;
  for PixelIndex := 1 to BytesPerRow do
    BestSum := BestSum + CurrentRow[PixelIndex];
  Move(CurrentRow^[1], OutputRow^[1], BytesPerRow);

  // check whether sub pre filter shall be used
  if aafmSub in FHeader.AdaptiveFilterMethods then
  begin
    // calculate sub filter
    EncodeFilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if sub filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 1;
    end;
  end;

  // check whether up pre filter shall be used
  if aafmUp in FHeader.AdaptiveFilterMethods then
  begin
    // calculate up filter
    EncodeFilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if up filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 2;
    end;
  end;

  // check whether average pre filter shall be used
  if aafmAverage in FHeader.AdaptiveFilterMethods then
  begin
    // calculate average filter
    EncodeFilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if average filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 3;
    end;
  end;

  // check whether paeth pre filter shall be used
  if aafmPaeth in FHeader.AdaptiveFilterMethods then
  begin
    // calculate paeth filter
    EncodeFilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if paeth filter is the current best filter
    if CurrentSum < BestSum then
    begin
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 4;
    end;
  end;
end;


{ TPortableNetworkGraphic }

constructor TPortableNetworkGraphic.Create;
begin
  FImageHeader         := TChunkPngImageHeader.Create;
  FDataChunkList       := TChunkList.Create;
  FAdditionalChunkList := TChunkList.Create;

  FCompressionLevel    := Z_BEST_COMPRESSION;
  inherited;
end;

destructor TPortableNetworkGraphic.Destroy;
begin
  FAdditionalChunkList.Clear;

  FAdditionalChunkList.Free;
  FDataChunkList.Free;
  FImageHeader.Free;

  FPaletteChunk.Free;
  FGammaChunk.Free;
  FTimeChunk.Free;
  FSignificantBits.Free;
  FPhysicalDimensions.Free;
  FChromaChunk.Free;
  FTransparencyChunk.Free;
  FBackgroundChunk.Free;

  inherited;
end;

procedure TPortableNetworkGraphic.SetPaletteChunk(
  const Value: TChunkPngPalette);
begin
  if (FPaletteChunk <> nil) then
    if (Value <> nil) then
      FPaletteChunk.Assign(Value)
    else
      FreeAndNil(FPaletteChunk)
  else
    if (Value <> nil) then
    begin
      FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
      FPaletteChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetPhysicalDimensions(
  const Value: TChunkPngPhysicalPixelDimensions);
begin
  if (FPhysicalDimensions <> nil) then
    if (Value <> nil) then
      FPhysicalDimensions.Assign(Value)
    else
      FreeAndNil(FPhysicalDimensions)
  else
    if (Value <> nil) then
    begin
      FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
      FPhysicalDimensions.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetSignificantBits(
  const Value: TChunkPngSignificantBits);
begin
  if (FSignificantBits <> nil) then
    if (Value <> nil) then
      FSignificantBits.Assign(Value)
    else
      FreeAndNil(FSignificantBits)
  else
    if (Value <> nil) then
    begin
      FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
      FSignificantBits.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetTimeChunk(const Value: TChunkPngTime);
begin
  if (FTimeChunk <> nil) then
    if (Value <> nil) then
      FTimeChunk.Assign(Value)
    else
      FreeAndNil(FTimeChunk)
  else
    if (Value <> nil) then
    begin
      FTimeChunk := TChunkPngTime.Create(FImageHeader);
      FTimeChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetTransparencyChunk(
  const Value: TChunkPngTransparency);
begin
  if (FTransparencyChunk <> nil) then
    if (Value <> nil) then
      FTransparencyChunk.Assign(Value)
    else
      FreeAndNil(FTransparencyChunk)
  else
    if (Value <> nil) then
    begin
      FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
      FTransparencyChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitX(const Value: Cardinal);
begin
  if Value = 0 then
    raise EPngError.Create(RCStrWrongPixelPerUnit);

  if not (FPhysicalDimensions <> nil) then
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

  FPhysicalDimensions.PixelsPerUnitX := Value;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitY(const Value: Cardinal);
begin
  if Value = 0 then
    raise EPngError.Create(RCStrWrongPixelPerUnit);

  if not (FPhysicalDimensions <> nil) then
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

  FPhysicalDimensions.PixelsPerUnitY := Value;
end;

procedure TPortableNetworkGraphic.SetPixelUnit(const Value: Byte);
begin
  if Value > 1 then
    raise EPngError.Create(RCStrUnspecifiedPixelUnit);

  if not (FPhysicalDimensions <> nil) then
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

  FPhysicalDimensions.PixelUnit := Value;
end;

procedure TPortableNetworkGraphic.SetChromaChunk(
  const Value: TChunkPngPrimaryChromaticities);
begin
  if (FChromaChunk <> nil) then
    if (Value <> nil) then
      FChromaChunk.Assign(Value)
    else
      FreeAndNil(FChromaChunk)
  else
    if (Value <> nil) then
    begin
      FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
      FChromaChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetGammaChunk(const Value: TChunkPngGamma);
begin
  if (FGammaChunk <> nil) then
    if (Value <> nil) then
      FGammaChunk.Assign(Value)
    else
      FreeAndNil(FGammaChunk)
  else
    if (Value <> nil) then
    begin
      FGammaChunk := TChunkPngGamma.Create(FImageHeader);
      FGammaChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetBackgroundChunk(
  const Value: TChunkPngBackgroundColor);
begin
  if (FGammaChunk <> nil) then
    if (Value <> nil) then
      FBackgroundChunk.Assign(Value)
    else
      FreeAndNil(FBackgroundChunk)
  else
    if (Value <> nil) then
    begin
      FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
      FBackgroundChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetImageHeader(
  const Value: TChunkPngImageHeader);
begin
  if not (Value <> nil) then
    raise EPngError.Create(RCStrNewHeaderError)
  else
    FImageHeader.Assign(Value);
end;

procedure TPortableNetworkGraphic.SetBitDepth(const Value: Byte);
begin
  raise EPngError.CreateFmt(RCStrBitDepthTranscodingError, [Value]);
end;

procedure TPortableNetworkGraphic.SetColorType(const Value: TColorType);
begin
  raise EPngError.CreateFmt(RCStrColorTypeTranscodingError, [Integer(Value)]);
end;

procedure TPortableNetworkGraphic.SetFilterMethods(
  const Value: TAvailableAdaptiveFilterMethods);
begin
  if (FImageHeader <> nil) then
    if FImageHeader.FAdaptiveFilterMethods <> Value then
    begin
      FImageHeader.FAdaptiveFilterMethods := Value;
      AdaptiveFilterMethodsChanged;
    end;
end;

procedure TPortableNetworkGraphic.SetCompressionLevel(const Value: Byte);
begin
  if not (Value in [1..9]) then
    raise EPngError.Create(RCStrInvalidCompressionLevel);

  if FCompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    CompressionLevelChanged;
  end;
end;

procedure TPortableNetworkGraphic.SetCompressionMethod(const Value: Byte);
begin
  raise EPngError.CreateFmt(RCStrDirectCompressionMethodSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetFilterMethod(const Value: TFilterMethod);
begin
  raise EPngError.CreateFmt(RCStrDirectFilterMethodSetError, [Integer(Value)]);
end;

procedure TPortableNetworkGraphic.SetWidth(const Value: Integer);
begin
  raise EPngError.CreateFmt(RCStrDirectWidthSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetInterlaceMethod(
  const Value: TInterlaceMethod);
begin
  if Value <> FImageHeader.InterlaceMethod then
  begin
    InterlaceMethodChanged;
    FImageHeader.InterlaceMethod := Value;
  end;
end;

procedure TPortableNetworkGraphic.SetModifiedTime(const Value: TDateTime);
begin
  if (FTimeChunk <> nil) then
    FTimeChunk.ModifiedDateTime := Value;
end;

procedure TPortableNetworkGraphic.SetGamma(const Value: Single);
begin
  raise EPngError.CreateFmt(RCStrDirectGammaSetError, [Value]);
end;

procedure TPortableNetworkGraphic.SetHeight(const Value: Integer);
begin
  raise EPngError.CreateFmt(RCStrDirectHeightSetError, [Value]);
end;

procedure TPortableNetworkGraphic.CopyImageData(Stream: TStream);
var
  DataIndex   : Integer;
begin
  // combine all data chunks first
  for DataIndex := 0 to FDataChunkList.Count - 1 do
  begin
    // make sure the chunk is inded an image data chunk
    Assert(FDataChunkList[DataIndex] is TChunkPngImageData);

    // concat current chunk to data stream
    with TChunkPngImageData(FDataChunkList[DataIndex]) do
    begin
      Data.Seek(0, soFromBeginning);
      Stream.CopyFrom(Data, Data.Size);
    end;
  end;
end;

procedure TPortableNetworkGraphic.StoreImageData(Stream: TStream);
var
  DataChunk : TChunkPngImageData;
  ChunkSize : Integer;
begin
  // delete old image data
  FDataChunkList.Clear;

  ChunkSize := Stream.Size;
  while Stream.Position < Stream.Size do
  begin
    DataChunk := TChunkPngImageData.Create(ImageHeader);

    if (Stream.Size - Stream.Position) < ChunkSize then
      ChunkSize := (Stream.Size - Stream.Position);

    // copy data to IDAT chunk
    DataChunk.Data.CopyFrom(Stream, ChunkSize);

    // add data chunk to data chunk list
    FDataChunkList.Add(DataChunk);
  end;
end;

procedure TPortableNetworkGraphic.DecompressImageDataToStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // copy image data from all data chunks to one continous data stream
    CopyImageData(DataStream);

    // check whether compression method is supported
    if FImageHeader.CompressionMethod <> 0 then
      raise EPngError.Create(RCStrUnsupportedCompressionMethod);

    // reset data stream position to zero
    DataStream.Seek(0, soFromBeginning);

    // decompress z-stream
    ZDecompress(DataStream, Stream);
  finally
    DataStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.CompressImageDataFromStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // set compression method
    FImageHeader.CompressionMethod := 0;

    // compress Stream to DataStream
    if Stream is TMemoryStream then
      ZCompress(TMemoryStream(Stream), DataStream, FCompressionLevel)
    else
      raise EPngError.CreateFmt(RCStrNotYetImplemented, ['source stream must be TMemoryStream']);

    // reset data stream position to zero
    DataStream.Seek(0, soFromBeginning);

    // copy image data from all data chunks to one continous data stream
    StoreImageData(DataStream);
  finally
    DataStream.Free;
  end;
end;

class function TPortableNetworkGraphic.CanLoad(const FileName: TFileName): Boolean;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := CanLoad(FileStream);
  finally
    FileStream.Free;
  end;
end;

class function TPortableNetworkGraphic.CanLoad(Stream: TStream): Boolean;
var
  ChunkID : TChunkName;
begin
  Result := Stream.Size >= 4;

  if Result then
  begin
    Stream.Read(ChunkID, 4);
    Stream.Seek(-4, soFromCurrent);
    Result := ChunkID = '‰PNG';
  end;
end;

procedure TPortableNetworkGraphic.LoadFromFile(Filename: TFilename);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Integer;
  ChunkCRC     : Cardinal;
  ChunkClass   : TCustomDefinedChunkWithHeaderClass;
  Chunk        : TCustomDefinedChunkWithHeader;
  MemoryStream : TMemoryStream;
  GotIDAT      : boolean;
  SavePos      : UInt64;
const
  PNG_SIG: TChunkName = (AnsiChar($89), 'P', 'N', 'G');
begin
  GotIDAT := False;
  Clear;

  // check for minimum file size
  if Stream.Size < 8 then
    raise EPngError.Create(RCStrNotAValidPNGFile);

  // read chunk ID
  Stream.Read(ChunkName, 4);
  if not CompareMem(@ChunkName, @PNG_SIG, SizeOf(TChunkName)) then
    raise EPngError.Create(RCStrNotAValidPNGFile);

  // read PNG magic
  Stream.Read(ChunkName, 4);
  if ChunkName <> CPngMagic then
    raise EPngError.Create(RCStrNotAValidPNGFile);

  MemoryStream := TMemoryStream.Create;
  try
    // read image header chunk size
    ChunkSize := BigEndian.ReadCardinal(Stream);
    if ChunkSize > Stream.Size - 12 then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // read image header chunk ID
    SavePos := Stream.Position;
    Stream.Read(ChunkName, 4);
    if ChunkName <> 'IHDR' then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // reset position to the chunk start and copy stream to memory
    Stream.Position := SavePos;
    MemoryStream.CopyFrom(Stream, ChunkSize + 4);
    MemoryStream.Position := 4;

    // load image header
    FImageHeader.ReadFromStream(MemoryStream, ChunkSize);

    // read image header chunk size
    ChunkCRC := 0;
    Stream.Read(ChunkCRC, 4);
    {$IFDEF CheckCRC}
    if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
      raise EPngError.Create(RCStrCRCError);
    {$ENDIF}

    while Stream.Position < Stream.Size do
    begin
      // read image header chunk size
      ChunkSize := BigEndian.ReadCardinal(Stream);
      if Stream.Position+ChunkSize+4 > Stream.Size then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      // read chunk ID
      SavePos := Stream.Position;
      Stream.Read(ChunkName, 4);

      // check for stream end
      if ChunkName = 'IEND' then
      begin
        // read image header chunk size
        Stream.Read(ChunkCRC, 4);

        {$IFDEF CheckCRC}
        if ChunkCRC <> 2187346606 then
          raise EPngError.Create(RCStrCRCError);
        {$ENDIF}

        Break;
      end;

      // reset position to the chunk start and copy stream to memory
      Stream.Position := SavePos;
      MemoryStream.Clear;
      MemoryStream.CopyFrom(Stream, ChunkSize + 4);

      // reset memory stream to beginning of the chunk
      MemoryStream.Seek(4, soFromBeginning);

      if ChunkName = 'IHDR' then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      if ChunkName = 'IDAT' then
      begin
        ReadImageDataChunk(MemoryStream, ChunkSize);
        GotIDAT := True;
      end else
      if ChunkName = 'gAMA' then
      begin
        if (FGammaChunk <> nil) then
          raise EPngError.Create(RCStrSeveralGammaChunks);
        FGammaChunk := TChunkPngGamma.Create(FImageHeader);
        FGammaChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'cHRM' then
      begin
        if (FChromaChunk <> nil) then
          raise EPngError.Create(RCStrSeveralChromaChunks);
        FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
        FChromaChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'tIME' then
      begin
        if (FTimeChunk <> nil) then
          raise EPngError.Create(RCStrSeveralTimeChunks);
        FTimeChunk := TChunkPngTime.Create(FImageHeader);
        FTimeChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'sBIT' then
      begin
        if (FSignificantBits <> nil) then
          raise EPngError.Create(RCStrSeveralSignificantBitsChunksFound);
        FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
        FSignificantBits.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'pHYs' then
      begin
        if (FPhysicalDimensions <> nil) then
          raise EPngError.Create(RCStrSeveralPhysicalPixelDimensionChunks);
        FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
        FPhysicalDimensions.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'PLTE' then
      begin
        if (FPaletteChunk <> nil) then
          raise EPngError.Create(RCStrSeveralPaletteChunks);
        FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
        FPaletteChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'tRNS' then
      begin
        if (FTransparencyChunk <> nil) then
          raise EPngError.Create(RCStrSeveralTransparencyChunks);
        FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
        FTransparencyChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      if ChunkName = 'bKGD' then
      begin
        if (FBackgroundChunk <> nil) then
          raise EPngError.Create(RCStrSeveralBackgroundChunks);
        FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
        FBackgroundChunk.ReadFromStream(MemoryStream, ChunkSize);
      end else
      begin
        ChunkClass := FindPngChunkByChunkName(ChunkName);
        if ChunkClass <> nil then
        begin
          Chunk := ChunkClass.Create(FImageHeader);
          Chunk.ReadFromStream(MemoryStream, ChunkSize);
          FAdditionalChunkList.Add(Chunk);
        end
        else
        begin
          // check if chunk is ancillary
          if (Byte(ChunkName[0]) and $80) <> 0 then
            raise EPngError.Create(RCStrAncillaryUnknownChunk);
          ReadUnknownChunk(MemoryStream, ChunkName, ChunkSize);
        end;
      end;

      // read & check CRC
      Stream.Read(ChunkCRC, 4);
      {$IFDEF CheckCRC}
      if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
        raise EPngError.Create(RCStrCRCError);
      {$ENDIF}
    end;
  finally
    MemoryStream.Free;
  end;

  if (not GotIDAT) then
    raise EPngError.Create(RCStrMissingIDATChunk);
end;

procedure TPortableNetworkGraphic.SaveToFile(Filename: TFilename);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.SaveToStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Cardinal;
  CRC          : Cardinal;
  MemoryStream : TMemoryStream;
  Index        : Integer;

  procedure SaveChunkToStream(Chunk: TCustomChunk);
  begin
    MemoryStream.Clear;

    // store chunk size directly to stream
    ChunkSize := Chunk.ChunkSize;
    BigEndian.WriteCardinal(Stream, ChunkSize);

    // store chunk name to memory stream
    ChunkName := Chunk.ChunkName;
    MemoryStream.Write(ChunkName, 4);

    // save chunk to memory stream
    Chunk.WriteToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Position := 0;
    Stream.CopyFrom(MemoryStream, 0);

    // calculate and write CRC
    CRC := Swap32(CalculateCRC(MemoryStream));
    Stream.Write(CRC, SizeOf(Cardinal));
  end;

begin
  // write chunk ID
  ChunkName := '‰PNG';
  Stream.Write(ChunkName, 4);

  // write PNG magic
  ChunkName := CPngMagic;
  Stream.Write(ChunkName, 4);

  MemoryStream := TMemoryStream.Create;
  try
    // store chunk size directly to stream
    ChunkSize := FImageHeader.ChunkSize;
    BigEndian.WriteCardinal(Stream, ChunkSize);

    // store chunk name to memory stream
    ChunkName := FImageHeader.ChunkName;
    MemoryStream.Write(ChunkName, 4);

    // save image header to memory stream
    FImageHeader.WriteToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Position := 0;;
    Stream.CopyFrom(MemoryStream, 0);

    // calculate and write CRC
    CRC := Swap32(CalculateCRC(MemoryStream));
    Stream.Write(CRC, SizeOf(Cardinal));

    // eventually save physical pixel dimensions chunk
    if (FPhysicalDimensions <> nil) then
      SaveChunkToStream(FPhysicalDimensions);

    // eventually save significant bits chunk
    if (FSignificantBits <> nil) then
      SaveChunkToStream(FSignificantBits);

    // eventually save gamma chunk
    if (FGammaChunk <> nil) then
      SaveChunkToStream(FGammaChunk);

    // eventually save chroma chunk
    if (FChromaChunk <> nil) then
      SaveChunkToStream(FChromaChunk);

    // eventually save palette chunk
    if (FPaletteChunk <> nil) then
      SaveChunkToStream(FPaletteChunk);

    // eventually save transparency chunk
    if (FTransparencyChunk <> nil) then
      SaveChunkToStream(FTransparencyChunk);

    // eventually save background chunk
    if (FBackgroundChunk <> nil) then
      SaveChunkToStream(FBackgroundChunk);

    // store additional chunks
    for Index := 0 to FAdditionalChunkList.Count - 1 do
      SaveChunkToStream(TCustomChunk(FAdditionalChunkList[Index]));

    // save data streams
    for Index := 0 to FDataChunkList.Count - 1 do
      SaveChunkToStream(TCustomChunk(FDataChunkList[Index]));
  finally
    MemoryStream.Free;
  end;

  // write chunk size
  BigEndian.WriteCardinal(Stream, 0);

  // write chunk ID
  ChunkName := 'IEND';
  Stream.Write(ChunkName, 4);

  // write CRC
  CRC := 2187346606;
  Stream.Write(CRC, 4);
end;

procedure TPortableNetworkGraphic.ReadUnknownChunk(Stream: TStream;
  ChunkName: TChunkName; ChunkSize: Integer);
var
  UnknownChunk : TChunkPngUnknown;
begin
  UnknownChunk := TChunkPngUnknown.Create(ChunkName);
  UnknownChunk.ReadFromStream(Stream, ChunkSize);
  FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TPortableNetworkGraphic.RemoveGammaInformation;
begin
  FreeAndNil(FGammaChunk);
end;

procedure TPortableNetworkGraphic.RemoveModifiedTimeInformation;
begin
  FreeAndNil(FTimeChunk);
end;

procedure TPortableNetworkGraphic.RemovePhysicalPixelDimensionsInformation;
begin
  FreeAndNil(FPhysicalDimensions);
end;

procedure TPortableNetworkGraphic.CompressionLevelChanged;
var
  TempStream : TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Seek(0, soFromBeginning);
    CompressImageDataFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.AdaptiveFilterMethodsChanged;
begin
  if FDataChunkList.Count > 0 then
  begin
    // transcoding!
    raise EPngError.CreateFmt(RCStrNotYetImplemented, ['AdaptiveFilterMethods transcoding']);
  end;
end;

procedure TPortableNetworkGraphic.InterlaceMethodChanged;
var
  TempStream      : TMemoryStream;
  TranscoderClass : TCustomPngTranscoderClass;
begin
  TempStream := TMemoryStream.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Seek(0, soFromBeginning);

    case FImageHeader.InterlaceMethod of
      imNone  : TranscoderClass := TPngNonInterlacedToAdam7Transcoder;
      imAdam7 : TranscoderClass := TPngAdam7ToNonInterlacedTranscoder;
    else
      raise EPngError.Create(RCStrWrongInterlaceMethod);
    end;

    with TranscoderClass.Create(TempStream, FImageHeader) do
    try
      Transcode;
    finally
      Free;
    end;

    TempStream.Seek(0, soFromBeginning);
    CompressImageDataFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TPortableNetworkGraphic.ReadImageDataChunk(Stream: TStream; Size: Integer);
var
  ImageDataChunk : TChunkPngImageData;
begin
  ImageDataChunk := TChunkPngImageData.Create(FImageHeader);
  ImageDataChunk.ReadFromStream(Stream, Size);
  FDataChunkList.Add(ImageDataChunk);
end;

procedure TPortableNetworkGraphic.Assign(Source: TPersistent);
begin
  if Source is TPortableNetworkGraphic then
    with TPortableNetworkGraphic(Source) do
    begin
      if (Self.FImageHeader <> nil) then
        Self.FImageHeader.Assign(FImageHeader);

      // assign palette chunk
      if (Self.FPaletteChunk <> nil) then
        if (FPaletteChunk <> nil) then
          Self.FPaletteChunk.Assign(FPaletteChunk)
        else
          FreeAndNil(Self.FPaletteChunk)
      else if (FPaletteChunk <> nil) then
      begin
        Self.FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
        Self.FPaletteChunk.Assign(FPaletteChunk);
      end;

      // assign gamma chunk
      if (Self.FGammaChunk <> nil) then
        if (FGammaChunk <> nil) then
          Self.FGammaChunk.Assign(FGammaChunk)
        else
          FreeAndNil(Self.FGammaChunk)
      else if (FGammaChunk <> nil) then
      begin
        Self.FGammaChunk := TChunkPngGamma.Create(FImageHeader);
        Self.FGammaChunk.Assign(FGammaChunk);
      end;

      // assign time chunk
      if (Self.FTimeChunk <> nil) then
        if (FTimeChunk <> nil) then
          Self.FTimeChunk.Assign(FTimeChunk)
        else
          FreeAndNil(Self.FTimeChunk)
      else if (FTimeChunk <> nil) then
      begin
        Self.FTimeChunk := TChunkPngTime.Create(FImageHeader);
        Self.FTimeChunk.Assign(FTimeChunk);
      end;

      // assign significant bits
      if (Self.FSignificantBits <> nil) then
        if (FSignificantBits <> nil) then
          Self.FSignificantBits.Assign(FSignificantBits)
        else
          FreeAndNil(Self.FSignificantBits)
      else if (FSignificantBits <> nil) then
      begin
        Self.FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
        Self.FSignificantBits.Assign(FSignificantBits);
      end;

      // assign physical dimensions
      if (Self.FPhysicalDimensions <> nil) then
        if (FPhysicalDimensions <> nil) then
          Self.FPhysicalDimensions.Assign(FPhysicalDimensions)
        else
          FreeAndNil(Self.FPhysicalDimensions)
      else if (FPhysicalDimensions <> nil) then
      begin
        Self.FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
        Self.FPhysicalDimensions.Assign(FPhysicalDimensions);
      end;

      // assign primary chromaticities
      if (Self.FChromaChunk <> nil) then
        if (FChromaChunk <> nil) then
          Self.FChromaChunk.Assign(FChromaChunk)
        else
          FreeAndNil(Self.FChromaChunk)
      else if (FChromaChunk <> nil) then
      begin
        Self.FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
        Self.FChromaChunk.Assign(FChromaChunk);
      end;

      // assign transparency
      if (Self.FTransparencyChunk <> nil) then
        if (FTransparencyChunk <> nil) then
          Self.FTransparencyChunk.Assign(FTransparencyChunk)
        else
          FreeAndNil(Self.FTransparencyChunk)
      else if (FTransparencyChunk <> nil) then
      begin
        Self.FTransparencyChunk := TChunkPngTransparency.Create(FImageHeader);
        Self.FTransparencyChunk.Assign(FTransparencyChunk);
      end;

      // assign background
      if (Self.FBackgroundChunk <> nil) then
        if (FBackgroundChunk <> nil) then
          Self.FBackgroundChunk.Assign(FBackgroundChunk)
        else
          FreeAndNil(Self.FBackgroundChunk)
      else if (FBackgroundChunk <> nil) then
      begin
        Self.FBackgroundChunk := TChunkPngBackgroundColor.Create(FImageHeader);
        Self.FBackgroundChunk.Assign(FBackgroundChunk);
      end;

      if (Self.FDataChunkList <> nil) then
        Self.FDataChunkList.Assign(FDataChunkList);
      if (Self.FAdditionalChunkList <> nil) then
        Self.FAdditionalChunkList.Assign(FAdditionalChunkList);
     end
  else
    inherited;
end;

procedure TPortableNetworkGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TPortableNetworkGraphic then
    with TPortableNetworkGraphic(Dest) do
    begin
      FImageHeader.Assign(Self.FImageHeader);
      FPaletteChunk.Assign(Self.FPaletteChunk);
      FGammaChunk.Assign(Self.FGammaChunk);
      FTimeChunk.Assign(Self.FTimeChunk);
      FSignificantBits.Assign(Self.FSignificantBits);
      FPhysicalDimensions.Assign(Self.FPhysicalDimensions);
      FChromaChunk.Assign(Self.FChromaChunk);
      FTransparencyChunk.Assign(Self.FTransparencyChunk);
      FBackgroundChunk.Assign(Self.FBackgroundChunk);
      FDataChunkList.Assign(Self.FDataChunkList);
      FAdditionalChunkList.Assign(Self.FAdditionalChunkList);
    end
  else
    inherited;
end;

function TPortableNetworkGraphic.CalculateCRC(Stream: TStream): Cardinal;
var
  CrcValue : Cardinal;
  Value    : Byte;
begin
  if Stream is TMemoryStream then
    Result := CalculateCRC(TMemoryStream(Stream).Memory, Stream.Size)
  else
  begin
    Stream.Position := 0;

    // initialize CRC
    CrcValue := $FFFFFFFF;
    {$IFDEF FPC}
    Value := 0;
    {$ENDIF}

    while Stream.Position < Stream.Size do
    begin
      Stream.Read(Value, 1);

      CrcValue := GCrcTable^[(CrcValue xor Value) and $FF] xor (CrcValue shr 8);
    end;

    Result := (CrcValue xor $FFFFFFFF);

    Stream.Position := 0;
  end;
end;

function TPortableNetworkGraphic.CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal;
{$if defined(PUREPASCAL)}
var
  CrcValue : Cardinal;
  Pos      : Cardinal;
begin
  // ignore size (offset by 4 bytes)
  Pos := 0;

  // initialize CRC
  CrcValue := $FFFFFFFF;

  while Pos < Count do
  begin
    CrcValue := GCrcTable^[(CrcValue xor Buffer^) and $FF] xor (CrcValue shr 8);
    Inc(Buffer);
    Inc(Pos);
  end;

  Result := (CrcValue xor $FFFFFFFF);
{$else}
asm
{$IFDEF Target_x64}
        PUSH    RBX
        PUSH    RDI
        MOV     RCX, R8
        JS      @Done
        NEG     RCX
        MOV     RBX, $FFFFFFFF

{$IFNDEF FPC}
        MOV     RDI, [GCrcTable]
{$ELSE}
        MOV     RDI, [RIP + GCrcTable]
{$ENDIF}

@Start:
        MOVZX   EAX, [RDX].BYTE
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [RDI + 4 * RAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     RDX
        INC     RCX
        JS      @Start

        XOR     EBX, $FFFFFFFF
        MOV     RAX, RBX

@Done:
        POP     RDI
        POP     RBX
{$ELSE}
        PUSH    EBX
        PUSH    EDI
        JS      @Done
        NEG     ECX
        MOV     EBX, $FFFFFFFF

        MOV     EDI, [GCrcTable]

@Start:
        MOVZX   EAX, [EDX].BYTE
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [EDI + 4 * EAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     EDX
        INC     ECX
        JS      @Start

        XOR     EAX, $FFFFFFFF

@Done:
        POP     EDI
        POP     EBX
{$ENDIF}
{$ifend}
end;

{$IFDEF CheckCRC}
function TPortableNetworkGraphic.CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
begin
  Result := CalculateCRC(Stream) = CRC;
end;
{$ENDIF}

function TPortableNetworkGraphic.GetBitDepth: Byte;
begin
  Result := FImageHeader.BitDepth;
end;

function TPortableNetworkGraphic.GetColorType: TColorType;
begin
  Result := FImageHeader.ColorType;
end;

function TPortableNetworkGraphic.GetCompressionMethod: Byte;
begin
  Result := FImageHeader.CompressionMethod;
end;

function TPortableNetworkGraphic.GetFilterMethod: TFilterMethod;
begin
  Result := FImageHeader.FilterMethod;
end;

function TPortableNetworkGraphic.GetFilterMethods: TAvailableAdaptiveFilterMethods;
begin
  Result := FImageHeader.FAdaptiveFilterMethods;
end;

function TPortableNetworkGraphic.GetGamma: Single;
begin
  if (FGammaChunk <> nil) then
    Result := FGammaChunk.GammaAsSingle
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetHeight: Integer;
begin
  Result := FImageHeader.Height;
end;

function TPortableNetworkGraphic.GetInterlaceMethod: TInterlaceMethod;
begin
  Result := FImageHeader.InterlaceMethod;
end;

function TPortableNetworkGraphic.GetModifiedTime: TDateTime;
begin
  if (FTimeChunk <> nil) then
    Result := EncodeDate(FTimeChunk.Year, FTimeChunk.Month, FTimeChunk.Day) +
      EncodeTime(FTimeChunk.Hour, FTimeChunk.Minute, FTimeChunk.Second, 0)
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetPaletteEntry(Index: Integer): TRGB24;
begin
  if (FPaletteChunk <> nil) then
    Result := FPaletteChunk.PaletteEntry[Index]
  else
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPortableNetworkGraphic.GetPaletteEntryCount: Integer;
begin
  if (FPaletteChunk <> nil) then
    Result := FPaletteChunk.Count
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitX: Cardinal;
begin
  if (FPhysicalDimensions <> nil) then
    Result := FPhysicalDimensions.PixelsPerUnitX
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitY: Cardinal;
begin
  if (FPhysicalDimensions <> nil) then
    Result := FPhysicalDimensions.PixelsPerUnitY
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetPixelUnit: Byte;
begin
  if (FPhysicalDimensions <> nil) then
    Result := FPhysicalDimensions.PixelUnit
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetWidth: Integer;
begin
  Result := FImageHeader.Width;
end;

function TPortableNetworkGraphic.HasGammaInformation: Boolean;
begin
  Result := (FGammaChunk <> nil);
end;

function TPortableNetworkGraphic.HasModifiedTimeInformation: Boolean;
begin
  Result := (FTimeChunk <> nil);
end;

function TPortableNetworkGraphic.HasPhysicalPixelDimensionsInformation: Boolean;
begin
  Result := (FPhysicalDimensions <> nil);
end;

procedure TPortableNetworkGraphic.Clear;
begin
  // clear chunk lists
  FDataChunkList.Clear;
  FAdditionalChunkList.Clear;

  // reset image header to default
  FImageHeader.ResetToDefault;

  FreeAndNil(FPaletteChunk);
  FreeAndNil(FGammaChunk);
  FreeAndNil(FChromaChunk);
  FreeAndNil(FTransparencyChunk);
  FreeAndNil(FBackgroundChunk);
  FreeAndNil(FTimeChunk);
  FreeAndNil(FSignificantBits);
  FreeAndNil(FPhysicalDimensions);
end;


{ TPngNonInterlacedToAdam7Transcoder }

procedure TPngNonInterlacedToAdam7Transcoder.Transcode;
var
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  Index         : Integer;
  PassRow       : Integer;
  Source        : PByte;
  Destination   : PByte;
  TempData      : PByteArray;
  OutputRow     : PByteArray;
  TempBuffer    : PByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  PixelByteSize := FHeader.PixelByteSize;

  GetMem(TempData, FHeader.Height * FHeader.BytesPerRow);
  Destination := PByte(TempData);
  try

    ///////////////////////////////////
    // decode image (non-interlaced) //
    ///////////////////////////////////

    // clear previous row
    FillChar(FRowBuffer[1 - CurrentRow]^[0], FHeader.BytesPerRow + 1, 0);

    for Index := 0 to FHeader.Height - 1 do
    begin
      // read data from stream
      if FStream.Read(FRowBuffer[CurrentRow][0], FHeader.BytesPerRow + 1) <> FHeader.BytesPerRow + 1 then
        raise EPngError.Create(RCStrDataIncomplete);

      // filter current row
      DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
        FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], FHeader.BytesPerRow,
        PixelByteSize);

      // transfer data from row to temp data
      Move(FRowBuffer[CurrentRow][1], Destination^, PixelByteSize * FHeader.Width);
      Inc(Destination, FHeader.Width * PixelByteSize);

      // flip current row
      CurrentRow := 1 - CurrentRow;
    end;

    // reset position to zero
    FStream.Seek(0, soFromBeginning);

    // The Adam7 interlacer uses 7 passes to create the complete image
    for CurrentPass := 0 to 6 do
    begin
      // calculate some intermediate variables
      PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] +
        CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

      case FHeader.ColorType of
        ctGrayscale      : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctIndexedColor   : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctTrueColor      : RowByteSize := (PixelPerRow * FHeader.BitDepth * 3) div 8;
        ctGrayscaleAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 2) div 8;
        ctTrueColorAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 4) div 8;
      else
        Continue;
      end;

      PassRow := CRowStart[CurrentPass];

      // clear previous row
      FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize + 1, 0);

      // check if pre filter is used and eventually calculate pre filter
      if (FHeader.ColorType <> ctIndexedColor) and
        not (FHeader.AdaptiveFilterMethods = []) then
      begin
        GetMem(OutputRow, RowByteSize + 1);
        GetMem(TempBuffer, RowByteSize + 1);
        try
          while PassRow < FHeader.Height do
          begin
            Index := CColumnStart[CurrentPass];
            Source := @TempData[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
            Destination := @FRowBuffer[CurrentRow][1];

            repeat
              // copy bytes per pixels
              Move(Source^, Destination^, PixelByteSize);

              Inc(Source, CColumnIncrement[CurrentPass] * PixelByteSize);
              Inc(Destination, PixelByteSize);
              Inc(Index, CColumnIncrement[CurrentPass]);
            until Index >= FHeader.Width;

            // filter current row
            EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
              OutputRow, TempBuffer, RowByteSize, FHeader.PixelByteSize);
            Assert(OutputRow[0] in [0..4]);

            // write data to data stream
            FStream.Write(OutputRow[0], RowByteSize + 1);

            // prepare for the next pass
            Inc(PassRow, CRowIncrement[CurrentPass]);
            CurrentRow := 1 - CurrentRow;
          end;
        finally
          Dispose(OutputRow);
          Dispose(TempBuffer);
        end;
      end
      else
        while PassRow < FHeader.Height do
        begin
          Index := CColumnStart[CurrentPass];
          Source := @TempData[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
          Destination := @FRowBuffer[CurrentRow][1];

          repeat
            // copy bytes per pixels
            Move(Source^, Destination^, PixelByteSize);

            Inc(Source, CColumnIncrement[CurrentPass] * PixelByteSize);
            Inc(Destination, PixelByteSize);
            Inc(Index, CColumnIncrement[CurrentPass]);
          until Index >= FHeader.Width;

          // set filter method 0
          FRowBuffer[CurrentRow][0] := 0;

          // write data to data stream
          FStream.Write(FRowBuffer[CurrentRow][0], RowByteSize + 1);

          // prepare for the next pass
          Inc(PassRow, CRowIncrement[CurrentPass]);
          CurrentRow := 1 - CurrentRow;
        end;
    end;
  finally
    Dispose(TempData);
  end;
end;


{ TPngAdam7ToNonInterlacedTranscoder }

procedure TPngAdam7ToNonInterlacedTranscoder.Transcode;
var
  CurrentRow    : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  Index         : Integer;
  PassRow       : Integer;
  Source        : PByte;
  Destination   : PByte;
  TempData      : PByteArray;
  OutputRow     : PByteArray;
  TempBuffer    : PByteArray;
begin
  // initialize variables
  CurrentRow := 0;
  PixelByteSize := FHeader.PixelByteSize;

  GetMem(TempData, FHeader.Height * FHeader.BytesPerRow);
  try

    /////////////////////////////////////
    // decode image (Adam7-interlaced) //
    /////////////////////////////////////

    // The Adam7 deinterlacer uses 7 passes to create the complete image
    for CurrentPass := 0 to 6 do
    begin
      // calculate some intermediate variables
      PixelPerRow := (FHeader.Width - CColumnStart[CurrentPass] +
        CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];

      case FHeader.ColorType of
        ctGrayscale      : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctIndexedColor   : RowByteSize := (PixelPerRow * FHeader.BitDepth + 7) div 8;
        ctTrueColor      : RowByteSize := (PixelPerRow * FHeader.BitDepth * 3) div 8;
        ctGrayscaleAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 2) div 8;
        ctTrueColorAlpha : RowByteSize := (PixelPerRow * FHeader.BitDepth * 4) div 8;
      else
        Continue;
      end;

      PassRow := CRowStart[CurrentPass];

      // clear previous row
      FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

      // process pixels
      while PassRow < FHeader.Height do
      begin
        // get interlaced row data
        if FStream.Read(FRowBuffer[CurrentRow][0], RowByteSize + 1) <> (RowByteSize + 1) then
          raise EPngError.Create(RCStrDataIncomplete);

        DecodeFilterRow(TAdaptiveFilterMethod(FRowBuffer[CurrentRow]^[0]),
          FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow], RowByteSize, PixelByteSize);

        Index := CColumnStart[CurrentPass];
        Source := @FRowBuffer[CurrentRow][1];
        Destination := @TempData[PassRow * FHeader.BytesPerRow + Index * PixelByteSize];
        repeat
          // copy bytes per pixels
          Move(Source^, Destination^, PixelByteSize);

          Inc(Source, PixelByteSize);
          Inc(Destination, CColumnIncrement[CurrentPass] * PixelByteSize);
          Inc(Index, CColumnIncrement[CurrentPass]);
        until Index >= FHeader.Width;

        // prepare for the next pass
        Inc(PassRow, CRowIncrement[CurrentPass]);
        CurrentRow := 1 - CurrentRow;
      end;
    end;


    // reset position to zero
    FStream.Seek(0, soFromBeginning);


    /////////////////////////////////
    // encode image non-interlaced //
    /////////////////////////////////

    // clear previous row buffer
    FillChar(FRowBuffer[1 - CurrentRow]^[0], FHeader.BytesPerRow, 0);
    Source := PByte(TempData);

    // check if pre filter is used and eventually calculate pre filter
    if (FHeader.ColorType <> ctIndexedColor) and
      not (FHeader.AdaptiveFilterMethods = []) then
    begin
      GetMem(OutputRow, FHeader.BytesPerRow + 1);
      GetMem(TempBuffer, FHeader.BytesPerRow + 1);
      try
        for Index := 0 to FHeader.Height - 1 do
        begin
          // copy bytes per pixels
          Move(Source^, FRowBuffer[CurrentRow][1], FHeader.Width * PixelByteSize);
          Inc(Source, FHeader.Width * PixelByteSize);

          // filter current row
          EncodeFilterRow(FRowBuffer[CurrentRow], FRowBuffer[1 - CurrentRow],
            OutputRow, TempBuffer, FHeader.BytesPerRow, FHeader.PixelByteSize);

          // write data to data stream
          FStream.Write(OutputRow[0], FHeader.BytesPerRow + 1);

          // flip current row used
          CurrentRow := 1 - CurrentRow;
        end;
      finally
        Dispose(OutputRow);
        Dispose(TempBuffer);
      end;
    end
    else
      for Index := 0 to FHeader.Height - 1 do
      begin
        // copy bytes per pixels
        Move(Source^, FRowBuffer[CurrentRow][1], FHeader.Width * PixelByteSize);
        Inc(Source, FHeader.Width * PixelByteSize);

        // set filter method to none
        FRowBuffer[CurrentRow][0] := 0;

        // write data to data stream
        FStream.Write(FRowBuffer[CurrentRow][0], FHeader.BytesPerRow + 1);

        // flip current row used
        CurrentRow := 1 - CurrentRow;
      end;

  finally
    Dispose(TempData);
  end;
end;


procedure BuildCrcTable(Polynomial: Cardinal);
var
  c    : Cardinal;
  n, k : Integer;
begin
  // allocate CRC table memory
  GetMem(GCrcTable, 256 * SizeOf(Cardinal));

  // fill CRC table
  for n := 0 to 255 do
  begin
    c := n;
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := Polynomial xor (c shr 1)
      else
        c := c shr 1;
    end;
    GCrcTable^[n] := c;
  end;
end;

initialization
  BuildCrcTable($EDB88320);
  RegisterPngChunks([TChunkPngImageData, TChunkPngPalette, TChunkPngGamma,
    TChunkPngStandardColorSpaceRGB, TChunkPngPrimaryChromaticities,
    TChunkPngTime, TChunkPngTransparency, TChunkPngEmbeddedIccProfile,
    TChunkPngPhysicalPixelDimensions,
    TChunkPngText, TChunkPngCompressedText,
    TChunkPngImageHistogram, TChunkPngBackgroundColor,
    TChunkPngSignificantBits, TChunkPngImageOffset, TChunkPngPixelCalibrator]);
{$ifdef PNG_CHUNK_SUGGESTED_PALETTE}
  RegisterPngChunks([TChunkPngSuggestedPalette]);
{$endif PNG_CHUNK_SUGGESTED_PALETTE}
{$ifdef PNG_CHUNK_INTERNATIONAL_TEXT}
  RegisterPngChunks([TChunkPngInternationalText]);
{$endif PNG_CHUNK_INTERNATIONAL_TEXT}

finalization
  if (GCrcTable <> nil) then
    Dispose(GCrcTable);

end.
