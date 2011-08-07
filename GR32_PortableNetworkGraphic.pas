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
 * The Original Code is Graphics32
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

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Classes, Graphics, SysUtils,
  {$IFDEF FPC} ZBase, ZDeflate, ZInflate; {$ELSE}
  {$IFDEF ZLibEx}ZLibEx, ZLibExApi; {$ELSE}zlib; {$ENDIF} {$ENDIF}

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
  TCustomChunk = class(TInterfacedPersistent, IStreamPersist)
  protected
    FChunkName  : TChunkName;
    FChunkSize  : Cardinal;
    function GetChunkName: AnsiString; virtual;
    function GetChunkSize: Cardinal; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetChunkName(const Value: AnsiString); virtual;
  public
    constructor Create; virtual;
    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromFile(FileName : TFileName); virtual;
    procedure SaveToFile(FileName : TFileName); virtual;
    property ChunkName: AnsiString read GetChunkName write SetChunkName;
    property ChunkSize: Cardinal read GetChunkSize;
  end;

  TCustomDefinedChunk = class(TCustomChunk)
  protected
    FFilePosition : Cardinal;
    procedure SetChunkName(const Value: AnsiString); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    class function GetClassChunkName : TChunkName; virtual; abstract;
  published
    property FilePosition : Cardinal read FFilePosition;
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
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ResetToDefault; virtual;

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property BitDepth: Byte read FBitDepth write FBitDepth;
    property ColorType: TColorType read FColorType write FColorType;
    property CompressionMethod: Byte read FCompressionMethod write SetCompressionMethod;
    property CompressionFilterMethods: TAvailableAdaptiveFilterMethods read FAdaptiveFilterMethods write SetAdaptiveFilterMethods;
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
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Data: TMemoryStream read FData;
  end;

  TChunkPngPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteEntries : array of TRGB24;
    function GetPaletteEntry(Index: Integer): TRGB24;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetPaletteEntry(Index: Integer; const Value: TRGB24);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure PaletteEntriesChanged; virtual;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PaletteEntry[Index: Integer]: TRGB24 read GetPaletteEntry write SetPaletteEntry; default;
    property Count: Integer read GetCount write SetCount;
  end;

  TChunkPngGamma = class(TCustomDefinedChunkWithHeader)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
    procedure SetGammaAsSingle(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle write SetGammaAsSingle;
  end;

  TChunkPngStandardColorSpaceRGB = class(TCustomDefinedChunkWithHeader)
  private
    FRenderingIntent : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ProfileName: AnsiString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TCustomPngSignificantBits = class(TPersistent)
  protected
    class function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngSignificantBitsFormat0 = class(TCustomPngSignificantBits)
  private
    FGrayBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GrayBits: Byte read FGrayBits write FGrayBits;
  end;

  TPngSignificantBitsFormat23 = class(TCustomPngSignificantBits)
  private
    FRedBits   : Byte;
    FBlueBits  : Byte;
    FGreenBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
  end;

  TPngSignificantBitsFormat4 = class(TCustomPngSignificantBits)
  private
    FGrayBits  : Byte;
    FAlphaBits : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedBits: Byte read FRedBits write FRedBits;
    property BlueBits: Byte read FBlueBits write FBlueBits;
    property GreenBits: Byte read FGreenBits write FGreenBits;
    property AlphaBits: Byte read FAlphaBits write FAlphaBits;
  end;

  TChunkPngSignificantBits = class(TCustomDefinedChunkWithHeader)
  private
    FSignificantBits : TCustomPngSignificantBits;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;

    property SignificantBits: TCustomPngSignificantBits read FSignificantBits;
  end;

  TCustomPngBackgroundColor = class(TPersistent)
  protected
    class function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngBackgroundColorFormat04 = class(TCustomPngBackgroundColor)
  private
    FGraySampleValue : Word;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngBackgroundColorFormat26 = class(TCustomPngBackgroundColor)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngBackgroundColorFormat3 = class(TCustomPngBackgroundColor)
  private
    FIndex : Byte;
  protected
    class function GetChunkSize: Integer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

  TChunkPngBackgroundColor = class(TCustomDefinedChunkWithHeader)
  private
    FBackground : TCustomPngBackgroundColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;

    property Background: TCustomPngBackgroundColor read FBackground;
  end;

  TChunkPngImageHistogram = class(TCustomDefinedChunkWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngSuggestedPalette = class(TCustomDefinedChunkWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TCustomPngTransparency = class(TPersistent)
  protected
    function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGraySampleValue : Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraySampleValue: Word read FGraySampleValue write FGraySampleValue;
  end;

  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property RedSampleValue: Word read FRedSampleValue write FRedSampleValue;
    property BlueSampleValue: Word read FBlueSampleValue write FBlueSampleValue;
    property GreenSampleValue: Word read FGreenSampleValue write FGreenSampleValue;
  end;

  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: Integer;
    function GetTransparency(Index: Integer): Byte;
  protected
    FTransparency : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Count: Integer read GetCount;
    property Transparency[Index: Integer]: Byte read GetTransparency;
  end;

  TChunkPngTransparency = class(TCustomDefinedChunkWithHeader)
  protected
    FTransparency : TCustomPngTransparency;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;

    property Transparency: TCustomPngTransparency read FTransparency;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomDefinedChunkWithHeader)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

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
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CalibratorName: AnsiString read FCalibratorName write FCalibratorName;
    property OriginalZeroMin: Integer read FOriginalZeroes[0] write FOriginalZeroes[0];
    property OriginalZeroMax: Integer read FOriginalZeroes[1] write FOriginalZeroes[1];
    property EquationType: Byte read FEquationType write FEquationType;
    property NumberOfParams: Byte read FNumberOfParams write FNumberOfParams;
  end;

  TCustomDefinedChunkTextChunk = class(TCustomDefinedChunkWithHeader)
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Keyword: AnsiString read FKeyword write FKeyword;
    property Text: AnsiString read FText write FText;
  end;

  TChunkPngTextChunk = class(TCustomDefinedChunkTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngCompressedTextChunk = class(TCustomDefinedChunkTextChunk)
  private
    FCompressionMethod : Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TChunkPngInternationalTextChunk = class(TCustomDefinedChunkTextChunk)
  private
    FCompressionMethod : Byte;
    FCompressionFlag   : Byte;
    FLanguageString    : AnsiString;
    FTranslatedKeyword : string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: AnsiString read FLanguageString write FLanguageString;
    property TranslatedKeyword: string read FTranslatedKeyword write FTranslatedKeyword;
  end;

  TUnknownChunk = class(TCustomChunk)
  private
    function GetData(index: Integer): Byte;
    procedure SetData(index: Integer; const Value: Byte);
  protected
    FDataStream : TMemoryStream;
    function CalculateChecksum: Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;

    property Data[index : Integer]: Byte read GetData write SetData;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TChunkList = class(TPersistent)
  private
    FChunks : array of TCustomChunk;
    function GetCount: Integer;
  protected
    function GetChunk(Index: Integer): TCustomChunk;
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;

    procedure Add(Item: TCustomChunk);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function IndexOf(Item: TCustomChunk): Integer;
    procedure Remove(Item: TCustomChunk);

    property Count: Integer read GetCount;
    property Chunks[Index: Integer]: TCustomChunk read GetChunk; default;
  end;

  TCustomPngCoder = class
  protected
    FStream       : TStream;
    FHeader       : TChunkPngImageHeader;
    FGamma        : TChunkPngGamma;
    FPalette      : TChunkPngPalette;

    FRowBuffer    : array [0..1] of PByteArray;
    FMappingTable : PByteArray;
    procedure BuildMappingTable; virtual;

    procedure EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure DecodeFilterSub(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure DecodeFilterUp(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil); virtual;
    destructor Destroy; override;
  end;

  TScanLineCallback = function(Bitmap: TObject; Y: Integer): Pointer of object;

  TCustomPngDecoder = class(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
  public
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;
  TCustomPngDecoderClass = class of TCustomPngDecoder;

  TCustomPngEncoder = class(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
  public
    procedure EncodeFromScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;
  TCustomPngEncoderClass = class of TCustomPngEncoder;

  TCustomPngTranscoder = class(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;

    procedure Transcode; virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TChunkPngImageHeader;
      Gamma: TChunkPngGamma = nil; Palette: TChunkPngPalette = nil); override;
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
    function GetPaletteEntry(index: Integer): TRGB24;
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
    procedure SetPhysicalDimensions(const Value: TChunkPngPhysicalPixelDimensions);
    procedure SetSignificantBits(const Value: TChunkPngSignificantBits);
    procedure SetTimeChunk(const Value: TChunkPngTime);
    procedure SetWidth(const Value: Integer);

    function CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal; overload;
    function CalculateCRC(Stream: TStream): Cardinal; overload;
    {$IFDEF CheckCRC}
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    {$ENDIF}
    procedure ReadImageDataChunk(Stream: TStream);
    procedure ReadUnknownChunk(Stream: TStream);
    function GetFilterMethods: TAvailableAdaptiveFilterMethods;
  protected
    FImageHeader         : TChunkPngImageHeader;
    FPaletteChunk        : TChunkPngPalette;
    FGammaChunk          : TChunkPngGamma;
    FTimeChunk           : TChunkPngTime;
    FSignificantBits     : TChunkPngSignificantBits;
    FPhysicalDimensions  : TChunkPngPhysicalPixelDimensions;
    FChromaChunk         : TChunkPngPrimaryChromaticities;
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
    property CompressionFilterMethods: TAvailableAdaptiveFilterMethods read GetFilterMethods write SetFilterMethods;
    property FilterMethod: TFilterMethod read GetFilterMethod write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read GetInterlaceMethod write SetInterlaceMethod;
    property PaletteEntry[index: Integer]: TRGB24 read GetPaletteEntry;
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

implementation

uses
  Math;

resourcestring
  RCStrAncillaryUnknownChunk = 'Unknown chunk is marked as ancillary';
  RCStrChunkSizeTooSmall = 'Chunk size too small!';
  RCStrDataIncomplete = 'Data not complete';
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
  RCStrNotYetImplemented = 'Not yet implemented';
  RCStrPaletteLimited = 'Palette is limited to 256 entries';
  RCStrSeveralChromaChunks = 'Primary chromaticities chunk defined twice!';
  RCStrSeveralGammaChunks = 'Gamma chunk defined twice!';
  RCStrSeveralPaletteChunks = 'Palette chunk defined twice!';
  RCStrSeveralPhysicalPixelDimensionChunks = 'Several physical pixel dimenson chunks found';
  RCStrSeveralSignificantBitsChunksFound = 'Several significant bits chunks found';
  RCStrSeveralTimeChunks = 'Time chunk appears twice!';
  RCStrUnknownColorType = 'Unknown color type!';
  RCStrUnspecifiedPixelUnit = 'Unspecified unit';
  RCStrUnsupportedCompressionMethod = 'Compression method not supported!';
  RCStrUnsupportedCompressMethod = 'Unsupported compression method';
  RCStrUnsupportedFilter = 'Unsupported Filter';
  RCStrUnsupportedFilterMethod = 'Unsupported filter method';
  RCStrUnsupportedInterlaceMethod = 'Unsupported interlace method';
  RCStrWrongBitdepth = 'Wrong Bitdepth';
  RCStrWrongInterlaceMethod = 'Wrong interlace method';
  RCStrWrongPixelPerUnit = 'Pixel per unit may not be zero!';
  RCStrWrongTransparencyFormat = 'Wrong transparency format';
  RCStrInvalidCompressionLevel = 'Invalid compression level';
  RCStrBitDepthTranscodingError = 'Bit depth may not be specified directly yet!';
  RCStrColorTypeTranscodingError = 'Color Type may not be specified directly yet!';
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
 Assert(IsPngChunkRegistered(ChunkClass) = False);
 SetLength(GPngChunkClasses, Length(GPngChunkClasses) + 1);
 GPngChunkClasses[Length(GPngChunkClasses) - 1] := ChunkClass;
end;

procedure RegisterPngChunks(ChunkClasses: array of TCustomDefinedChunkWithHeaderClass);
var
  ChunkClassIndex : Integer;
begin
 for ChunkClassIndex := 0 to Length(ChunkClasses) - 1
  do RegisterPngChunk(ChunkClasses[ChunkClassIndex]);
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


{ Byte Ordering }

type
  T16Bit = record
    case Integer of
      0 :  (v: SmallInt);
      1 :  (b: array[0..1] of Byte);
  end;

  T32Bit = record
    case Integer of
      0 :  (v: LongInt);
      1 :  (b: array[0..3] of Byte);
  end;

function Swap16(Value: SmallInt): SmallInt;
var
  t: Byte;
begin
 with T16Bit(Value) do
  begin
   t := b[0];
   b[0] := b[1];
   b[1] := t;
   Result := v;
  end;
end;

function Swap32(Value: LongInt): LongInt;
var
  Temp: Byte;
begin
 with T32Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[3];
   b[3] := Temp;
   Temp := b[1];
   b[1] := b[2];
   b[2] := Temp;
   Result := v;
  end;
end;

procedure Flip16(var Value);
var
  t: Byte;
begin
 with T16Bit(Value) do
  begin
   t := b[0];
   b[0] := b[1];
   b[1] := t;
  end;
end;

procedure Flip32(var Value);
var
  Temp: Byte;
begin
 with T32Bit(Value) do
  begin
   Temp := b[0];
   b[0] := b[3];
   b[3] := Temp;
   Temp := b[1];
   b[1] := b[2];
   b[2] := Temp;
  end;
end;


{ Stream I/O functions }

function ReadSwappedWord(Stream: TStream): Word;
begin
 {$IFDEF FPC}
 Result := 0;
 {$ENDIF}
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(Word));
 {$ENDIF}
 Result := Swap16(Result);
end;

function ReadSwappedSmallInt(Stream: TStream): SmallInt;
begin
 {$IFDEF FPC}
 Result := 0;
 {$ENDIF}
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(SmallInt));
 {$ENDIF}
 Result := Swap16(Result);
end;

function ReadSwappedCardinal(Stream: TStream): Cardinal;
begin
 {$IFDEF FPC}
 Result := 0;
 {$ENDIF}
 {$IFDEF ValidateEveryReadOperation}
 if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal)
  then raise EPascalTypeStremReadError.Create(RCStrStreamReadError);
 {$ELSE}
 Stream.Read(Result, SizeOf(Cardinal));
 {$ENDIF}
 Result := Swap32(Result);
end;

procedure WriteSwappedWord(Stream: TStream; Value: Word);
begin
 Value := Swap16(Value);
 Stream.Write(Value, SizeOf(Word));
end;

procedure WriteSwappedSmallInt(Stream: TStream; Value: SmallInt);
begin
 Value := Swap16(Value);
 Stream.Write(Value, SizeOf(SmallInt));
end;

procedure WriteSwappedCardinal(Stream: TStream; Value: Cardinal);
begin
 Value := Swap32(Value);
 Stream.Write(Value, SizeOf(Cardinal));
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
 if DeflateInit_(@ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during compression');
 {$ELSE}
 if DeflateInit_(ZStreamRecord, Level, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during compression');
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

 if deflateEnd(ZStreamRecord) > 0
  then raise EPngError.Create('Error on stream validation');
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
 if inflateInit_(@ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during decompression');
 {$ELSE}
 if inflateInit_(ZStreamRecord, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0
  then raise EPngError.Create('Error during decompression');
 {$ENDIF}

 GetMem(TempBuffer, CBufferSize);
 try
  while ZStreamRecord.avail_in > 0 do
   begin
    ZStreamRecord.next_out := TempBuffer;
    ZStreamRecord.avail_out := CBufferSize;

    inflate(ZStreamRecord, Z_NO_FLUSH);

    Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
   end;

  repeat
   ZStreamRecord.next_out := TempBuffer;
   ZStreamRecord.avail_out := CBufferSize;

   ZResult := inflate(ZStreamRecord, Z_FINISH);

   Output.Write(TempBuffer^, CBufferSize - ZStreamRecord.avail_out);
  until (ZResult = Z_STREAM_END) and (ZStreamRecord.avail_out > 0);
 finally
  FreeMem(TempBuffer);
 end;

 if inflateEnd(ZStreamRecord) > 0
  then raise EPngError.Create('Error on stream validation');
end;

procedure ZDecompress(const Input: TMemoryStream; const Output: TStream); overload;
begin
 ZDecompress(Input.Memory, Input.Size, Output);
end;


{ TCustomChunk }

constructor TCustomChunk.Create;
begin
 FChunkName := '';
 FChunkSize := 0;
end;

procedure TCustomChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunk then
  begin
   TCustomChunk(Dest).FChunkName := FChunkName;
   TCustomChunk(Dest).FChunkSize := FChunkSize;
  end
 else inherited;
end;

function TCustomChunk.GetChunkName: AnsiString;
begin
 Result := AnsiString(FChunkName);
end;

function TCustomChunk.GetChunkSize: Cardinal;
begin
 Result := FChunkSize;
end;

procedure TCustomChunk.LoadFromFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   LoadFromStream(FileStream);
  finally
   Free;
  end;
end;

procedure TCustomChunk.SaveToFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   Free;
  end;
end;

procedure TCustomChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   Assert(Position <= Size + 8);

   Read(FChunkSize, 4);
   Read(FChunkName, 4);
  end;

 Flip32(FChunkSize);
end;

procedure TCustomChunk.SaveToStream(Stream: TStream);
var
  TempSize : Cardinal;
begin
 TempSize := Swap32(FChunkSize);

 with Stream do
  begin
   Write(TempSize, 4);
   Write(FChunkName[0], 4);
  end;
end;

procedure TCustomChunk.SetChunkName(const Value: AnsiString);
var
  ChunkNameSize : Integer;
begin
 ChunkNameSize := Length(Value);
 if ChunkNameSize > 3 then ChunkNameSize := 4;
 Move(Value[1], FChunkName[0], ChunkNameSize);
end;


{ TCustomDefinedChunk }

constructor TCustomDefinedChunk.Create;
begin
 inherited;
 FFilePosition := 0;
 FChunkName := GetClassChunkName;
end;

procedure TCustomDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomDefinedChunk
  then TCustomDefinedChunk(Dest).FFilePosition := FFilePosition;
end;

procedure TCustomDefinedChunk.LoadFromStream(Stream: TStream);
var
  TempChunkName : TChunkName;
begin
 with Stream do
  begin
   Position := Position + 4;
   Read(TempChunkName, 4);
   Assert(TempChunkName = FChunkName);
   Position := Position - 8;
   inherited;
  end;
end;

procedure TCustomDefinedChunk.SetChunkName(const Value: AnsiString);
begin
 inherited;
 if Value <> FChunkName
  then raise EPngError.Create('Chunk name must always be ''' +
    string(AnsiString(FChunkName)) + '''');
end;


{ TUnknownChunk }

function TUnknownChunk.CalculateChecksum: Integer;
var
  b : Byte;
begin
 with FDataStream do
  begin
   Position := 0;
   Result := 0;
   while Position < Size do
    begin
     Read(b, 1);
     Result := Result + b;
    end;
  end;
end;

constructor TUnknownChunk.Create;
begin
 inherited;
 FDataStream := TMemoryStream.Create;
end;

destructor TUnknownChunk.Destroy;
begin
 FreeAndNil(FDataStream);
 inherited;
end;

procedure TUnknownChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TUnknownChunk then
  begin
   TUnknownChunk(Dest).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
  end;
end;

function TUnknownChunk.GetData(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < FDataStream.Size)
  then
   with FDataStream do
    begin
     Position := Index;
     Read(Result, 1);
    end
  else raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [index]);
end;

procedure TUnknownChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   inherited;
   Assert(FChunkSize <= Size);
   Assert(FChunkName <> #0#0#0#0);
   FDataStream.Clear;
   FDataStream.Size := FChunkSize;
   FDataStream.Position := 0;
   if FChunkSize > 0
    then FDataStream.CopyFrom(Stream, FChunkSize);
  end;
end;

procedure TUnknownChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   FChunkSize := FDataStream.Size;
   inherited;
   FDataStream.Position := 0;
   CopyFrom(FDataStream, FDataStream.Position);
  end;
end;

procedure TUnknownChunk.SetData(Index: Integer; const Value: Byte);
begin
 if (Index >= 0) and (Index < FDataStream.Size)
  then
   with FDataStream do
    begin
     Position := Index;
     Write(Value, 1);
    end
  else raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
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
 else inherited;
end;

function TChunkPngImageHeader.GetBytesPerRow: Integer;
begin
 case FColorType of
  ctGrayscale,
  ctIndexedColor   : Result := ((FWidth * FBitDepth + $7) and not $7) shr 3;
  ctGrayscaleAlpha : Result := 2 * (FBitDepth shr 3) * FWidth;
  ctTrueColor      : Result := 3 * (FBitDepth shr 3) * FWidth;
  ctTrueColorAlpha : Result := 4 * (FBitDepth shr 3) * FWidth;
  else raise EPngError.Create(RCStrUnknownColorType);
 end;
end;

class function TChunkPngImageHeader.GetClassChunkName: TChunkName;
begin
 Result := 'IHDR';
end;

function TChunkPngImageHeader.GetPixelByteSize: Integer;
begin
 case ColorType of
  ctGrayscale :
   if FBitDepth = 16
    then Result := 2
    else Result := 1;
  ctTrueColor : Result := 3 * FBitDepth div 8;
  ctIndexedColor : Result := 1;
  ctGrayscaleAlpha : Result := 2 * FBitDepth div 8;
  ctTrueColorAlpha : Result := 4 * FBitDepth div 8;
  else Result := 0;
 end;
end;

function TChunkPngImageHeader.GetHasPalette: Boolean;
begin
 Result := FColorType in [ctIndexedColor];
end;

procedure TChunkPngImageHeader.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 13
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read width
   FWidth := ReadSwappedCardinal(Stream);

   // read height
   FHeight := ReadSwappedCardinal(Stream);

   // read bit depth
   Read(FBitDepth, 1);

   // read Color type
   Read(FColorType, 1);

   // check consistency between Color type and bit depth
   case FColorType of
    ctGrayscale :
      if not (FBitDepth in [1, 2, 4, 8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
    ctTrueColor,
    ctGrayscaleAlpha,
    ctTrueColorAlpha :
      if not (FBitDepth in [8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
    ctIndexedColor :
      if not (FBitDepth in [1, 2, 4, 8]) then raise EPngError.Create(RCStrWrongBitdepth);
   end;

   // read compression method
   Read(FCompressionMethod, 1);

   // check for compression method
   if FCompressionMethod <> 0
    then raise EPngError.Create(RCStrUnsupportedCompressMethod);

   // read filter method
   Read(FFilterMethod, 1);

   // check for filter method
   if FFilterMethod <> fmAdaptiveFilter
    then raise EPngError.Create(RCStrUnsupportedFilterMethod);

   // read interlace method
   Read(FInterlaceMethod, 1);

   // check for interlace method
   if not (FInterlaceMethod in [imNone, imAdam7])
    then raise EPngError.Create(RCStrUnsupportedInterlaceMethod);
  end;
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

procedure TChunkPngImageHeader.SaveToStream(Stream: TStream);
begin
 FChunkSize := 13;

 inherited;

 with Stream do
  begin
   // write width
   WriteSwappedCardinal(Stream, FWidth);

   // write height
   WriteSwappedCardinal(Stream, FHeight);

   // write bit depth
   Write(FBitDepth, 1);

   // write Color type
   Write(FColorType, 1);

   // write compression method
   Write(FCompressionMethod, 1);

   // write filter method
   Write(FFilterMethod, 1);

   // write interlace method
   Write(FInterlaceMethod, 1);
  end;
end;

procedure TChunkPngImageHeader.SetAdaptiveFilterMethods(
  const Value: TAvailableAdaptiveFilterMethods);
begin
 FAdaptiveFilterMethods := Value;
end;

procedure TChunkPngImageHeader.SetCompressionMethod(const Value: Byte);
begin
 // check for compression method
 if Value <> 0
  then raise EPngError.Create(RCStrUnsupportedCompressMethod);
end;

procedure TChunkPngImageHeader.SetFilterMethod(const Value: TFilterMethod);
begin
 // check for filter method
 if Value <> fmAdaptiveFilter
  then raise EPngError.Create(RCStrUnsupportedFilterMethod);
end;


{ TCustomDefinedChunkWithHeader }

procedure TCustomDefinedChunkWithHeader.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDefinedChunkWithHeader then
  with TCustomDefinedChunkWithHeader(Dest) do
   begin
    FHeader.Assign(Self.FHeader);
   end
 else inherited;
end;

constructor TCustomDefinedChunkWithHeader.Create(Header: TChunkPngImageHeader);
begin
 if not (Header is TChunkPngImageHeader)
  then raise EPngError.Create(RCStrHeaderInvalid);

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
 else inherited;
end;

class function TChunkPngPalette.GetClassChunkName: TChunkName;
begin
 Result := 'PLTE';
end;

function TChunkPngPalette.GetPaletteEntry(Index: Integer): TRGB24;
begin
 if (Index >= 0) and (Index < Count)
  then Result := FPaletteEntries[Index]
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TChunkPngPalette.SetPaletteEntry(Index: Integer; const Value: TRGB24);
begin
 if (Index >= 0) and (Index < Count)
  then FPaletteEntries[Index] := Value
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

function TChunkPngPalette.GetCount: Integer;
begin
 Result := Length(FPaletteEntries);
end;

procedure TChunkPngPalette.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if ((Size - Position) mod SizeOf(TRGB24)) <> 0
    then raise EPngError.Create(RCStrIncompletePalette);

   SetLength(FPaletteEntries, (Size - Position) div SizeOf(TRGB24));

   Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
  end;
end;

procedure TChunkPngPalette.PaletteEntriesChanged;
begin
 // nothing todo here yet
end;

procedure TChunkPngPalette.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 FChunkSize := Length(FPaletteEntries) * SizeOf(TRGB24);

 inherited;

 Stream.Write(FPaletteEntries[0], FChunkSize);
end;

procedure TChunkPngPalette.SetCount(const Value: Integer);
begin
 if Value > 256
  then raise EPngError.Create(RCStrPaletteLimited);

 if Value <> Length(FPaletteEntries) then
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
 else inherited;
end;

constructor TChunkPngTransparency.Create(Header: TChunkPngImageHeader);
begin
 inherited;
 case Header.ColorType of
  ctGrayscale    : FTransparency := TPngTransparencyFormat0.Create;
  ctTrueColor    : FTransparency := TPngTransparencyFormat2.Create;
  ctIndexedColor : FTransparency := TPngTransparencyFormat3.Create;
 end;
end;

destructor TChunkPngTransparency.Destroy;
begin
 if Assigned(FTransparency)
  then FreeAndNil(FTransparency);
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
  ctGrayscale     : if not (FTransparency is TPngTransparencyFormat0) then
                     begin
                      FTransparency := TPngTransparencyFormat0.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  ctTrueColor     : if not (FTransparency is TPngTransparencyFormat2) then
                     begin
                      FTransparency := TPngTransparencyFormat2.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  ctIndexedColor  : if not (FTransparency is TPngTransparencyFormat3) then
                     begin
                      FTransparency := TPngTransparencyFormat3.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  else if Assigned(FTransparency) then FreeAndNil(FTransparency);

 end;
end;

procedure TChunkPngTransparency.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FTransparency)
  then FTransparency.LoadFromStream(Stream);
end;

procedure TChunkPngTransparency.SaveToStream(Stream: TStream);
begin
 if Assigned(FTransparency)
  then FChunkSize := FTransparency.ChunkSize
  else FChunkSize := 0;

 inherited;

 // check consistency
 case FHeader.ColorType of
  ctGrayscale    : if not (FTransparency is TPngTransparencyFormat0)
                    then raise EPngError.Create(RCStrWrongTransparencyFormat);
  ctTrueColor    : if not (FTransparency is TPngTransparencyFormat2)
                    then raise EPngError.Create(RCStrWrongTransparencyFormat);
  ctIndexedColor : if not (FTransparency is TPngTransparencyFormat3)
                    then raise EPngError.Create(RCStrWrongTransparencyFormat);
 end;

 if Assigned(FTransparency)
  then FTransparency.SaveToStream(Stream);
end;


{ TPngTransparencyFormat0 }

procedure TPngTransparencyFormat0.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngTransparencyFormat0 then
  with TPngTransparencyFormat0(Dest) do
   begin
    FGraySampleValue := Self.FGraySampleValue;
   end
 else inherited;
end;

function TPngTransparencyFormat0.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngTransparencyFormat0.LoadFromStream(Stream: TStream);
begin
 inherited;

 FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat0.SaveToStream(Stream: TStream);
begin
 inherited;

 WriteSwappedWord(Stream, FGraySampleValue);
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
 else inherited;
end;

function TPngTransparencyFormat2.GetChunkSize: Integer;
begin
 Result := 6;
end;

procedure TPngTransparencyFormat2.LoadFromStream(Stream: TStream);
begin
 inherited;

 FRedSampleValue  := ReadSwappedWord(Stream);
 FBlueSampleValue  := ReadSwappedWord(Stream);
 FGreenSampleValue  := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat2.SaveToStream(Stream: TStream);
begin
 inherited;

 WriteSwappedWord(Stream, FRedSampleValue);
 WriteSwappedWord(Stream, FBlueSampleValue);
 WriteSwappedWord(Stream, FGreenSampleValue);
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
 else inherited;
end;

function TPngTransparencyFormat3.GetChunkSize: Integer;
begin
 Result := Count;
end;

function TPngTransparencyFormat3.GetCount: Integer;
begin
 Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < Count)
  then Result := FTransparency[Index]
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   SetLength(FTransparency, Size - Position);
   Read(FTransparency[0], Length(FTransparency));
  end;
end;

procedure TPngTransparencyFormat3.SaveToStream(Stream: TStream);
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
    FUnit           := Self.FUnit;
   end
 else inherited;
end;

class function TChunkPngPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
 Result := 'pHYs';
end;

procedure TChunkPngPhysicalPixelDimensions.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 9
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read pixels per unit, X axis
   FPixelsPerUnitX := ReadSwappedCardinal(Stream);

   // read pixels per unit, Y axis
   FPixelsPerUnitY := ReadSwappedCardinal(Stream);

   // read unit
   Read(FUnit, 1);
  end;
end;

procedure TChunkPngPhysicalPixelDimensions.SaveToStream(Stream: TStream);
begin
 FChunkSize := 9;

 inherited;

 with Stream do
  begin
   // write pixels per unit, X axis
   WriteSwappedCardinal(Stream, FPixelsPerUnitX);

   // write pixels per unit, Y axis
   WriteSwappedCardinal(Stream, FPixelsPerUnitY);

   // write unit
   Write(FUnit, 1);
  end;
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
 else inherited;
end;

class function TChunkPngPhysicalScale.GetClassChunkName: TChunkName;
begin
 Result := 'sCAL';
end;

procedure TChunkPngPhysicalScale.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 4
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read unit specifier
   Read(FUnitSpecifier, 1);

   // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
  end;
end;

procedure TChunkPngPhysicalScale.SaveToStream(Stream: TStream);
begin
 inherited;

 raise EPngError.Create(RCStrNotYetImplemented);
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
 else inherited;
end;

class function TChunkPngImageOffset.GetClassChunkName: TChunkName;
begin
 Result := 'oFFs';
end;

procedure TChunkPngImageOffset.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 9
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read image positions
   FImagePositionX := ReadSwappedCardinal(Stream);
   FImagePositionY := ReadSwappedCardinal(Stream);

   // read unit specifier
   Read(FUnitSpecifier, 1);
  end;
end;

procedure TChunkPngImageOffset.SaveToStream(Stream: TStream);
begin
 FChunkSize := 9;

 inherited;

 // read image positions
 WriteSwappedCardinal(Stream, FImagePositionX);
 WriteSwappedCardinal(Stream, FImagePositionY);

 // read unit specifier
 Write(FUnitSpecifier, 1);
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
 else inherited;
end;

class function TChunkPngPixelCalibrator.GetClassChunkName: TChunkName;
begin
 Result := 'pCAL';
end;

procedure TChunkPngPixelCalibrator.LoadFromStream(Stream: TStream);
var
  Index      : Integer;
  ParamIndex : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FCalibratorName, 80);
   while (Position < Size) do
    begin
     Read(FCalibratorName[Index], SizeOf(Byte));
     if FCalibratorName[Index] = #0 then
      begin
       SetLength(FCalibratorName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read original zeros
   FOriginalZeroes[0] := ReadSwappedCardinal(Stream);
   FOriginalZeroes[1] := ReadSwappedCardinal(Stream);

   // read equation type
   Stream.Read(FEquationType, 1);

   // read number of parameters
   Stream.Read(FNumberOfParams, 1);

   // read keyword
   Index := 1;
   SetLength(FUnitName, 80);
   while (Position < Size) do
    begin
     Read(FUnitName[Index], SizeOf(Byte));
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
end;

procedure TChunkPngPixelCalibrator.SaveToStream(Stream: TStream);
begin
  inherited;

end;


{ TChunkPngTextChunk }

class function TChunkPngTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'tEXt';
end;

procedure TChunkPngTextChunk.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read text
   Index := 1;
   SetLength(FText, Size - Position);
   while (Position < Size) do
    begin
     Read(FText[Index], SizeOf(Byte));
     Inc(Index);
    end;
  end;
end;

procedure TChunkPngTextChunk.SaveToStream(Stream: TStream);
var
  Temp  : Byte;
begin
 FChunkSize := Length(FKeyword) + Length(FText) + 1;

 inherited;

 with Stream do
  begin
   // write keyword
   Write(FKeyword[1], Length(FKeyword));

   // write separator
   Temp := 0;
   Write(Temp, 1);

   // write text
   Write(FText[1], Length(FText));
  end;
end;


{ TChunkPngCompressedTextChunk }

procedure TChunkPngCompressedTextChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngCompressedTextChunk then
  with TChunkPngCompressedTextChunk(Dest) do
   begin
    FCompressionMethod := Self.FCompressionMethod;
   end
 else inherited;
end;

class function TChunkPngCompressedTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'zTXt';
end;

procedure TChunkPngCompressedTextChunk.LoadFromStream(Stream: TStream);
var
  DataIn     : Pointer;
  DataInSize : Integer;
  Output     : TMemoryStream;
  Index      : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression method
   Read(FCompressionMethod, SizeOf(Byte));

   // read text
   if FCompressionMethod = 0 then
    begin
     DataInSize := Size - Position;
     GetMem(DataIn, DataInSize);
     try
      Read(DataIn^, DataInSize);

      Output := TMemoryStream.Create;
      try
       ZDecompress(DataIn, DataInSize, Output);
       SetLength(FText, Output.Size);
       Move(Output.Memory^, FText[1], Output.Size);
      finally
       FreeAndNil(Output);
      end;
     finally
      FreeMem(DataIn);
     end;
    end;
  end;
end;

procedure TChunkPngCompressedTextChunk.SaveToStream(Stream: TStream);
var
  OutputStream : TMemoryStream;
  Temp         : Byte;
begin
 OutputStream := TMemoryStream.Create;
 try
  // compress text
  ZCompress(@FText[1], Length(FText), OutputStream);

  // calculate chunk size
  FChunkSize := Length(FKeyword) + OutputStream.Size + 1;

  inherited;

  with Stream do
   begin
    // write keyword
    Write(FKeyword[1], Length(FKeyword));

    // write separator
    Temp := 0;
    Write(Temp, 1);

    // write text
    Write(FText[1], Length(FText));

    // write compression method
    Write(FCompressionMethod, SizeOf(Byte));

    // write text
    Write(OutputStream.Memory^, OutputStream.Size);
   end;
 finally
  FreeAndNil(OutputStream);
 end;
end;


{ TChunkPngInternationalTextChunk }

procedure TChunkPngInternationalTextChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngInternationalTextChunk then
  with TChunkPngInternationalTextChunk(Dest) do
   begin
    FCompressionMethod := Self.FCompressionMethod;
    FCompressionFlag   := Self.FCompressionFlag;
    FLanguageString    := Self.FLanguageString;
    FTranslatedKeyword := Self.FTranslatedKeyword;
   end
 else inherited;
end;

class function TChunkPngInternationalTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'iTXt';
end;

procedure TChunkPngInternationalTextChunk.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression flag
   Read(FCompressionFlag, SizeOf(Byte));

   // read compression method
   Read(FCompressionMethod, SizeOf(Byte));

   // read language string
   Index := 1;
   SetLength(FLanguageString, 10);
   while (Position < Size) do
    begin
     Read(FLanguageString[Index], SizeOf(Byte));
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
end;

procedure TChunkPngInternationalTextChunk.SaveToStream(Stream: TStream);
begin
 raise EPngError.Create(RCStrNotYetImplemented);
end;


{ TChunkPngImageData }

constructor TChunkPngImageData.Create;
begin
 inherited;
 FData := TMemoryStream.Create;
end;

destructor TChunkPngImageData.Destroy;
begin
 FreeAndNil(FData);
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
 else inherited;
end;

class function TChunkPngImageData.GetClassChunkName: TChunkName;
begin
 Result := 'IDAT';
end;

procedure TChunkPngImageData.LoadFromStream(Stream: TStream);
begin
 inherited;

 FData.CopyFrom(Stream, Stream.Size - Stream.Position);
end;

procedure TChunkPngImageData.SaveToStream(Stream: TStream);
begin
 FChunkSize := FData.Size;
 inherited;

 FData.Seek(0, soFromBeginning);
 Stream.CopyFrom(FData, FChunkSize);
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
 else inherited;
end;

class function TChunkPngTime.GetClassChunkName: TChunkName;
begin
 Result := 'tIME';
end;

function TChunkPngTime.GetModifiedDateTime: TDateTime;
begin
 Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

procedure TChunkPngTime.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 7
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read year
   FYear := ReadSwappedWord(Stream);

   // read month
   Read(FMonth, SizeOf(Byte));

   // read day
   Read(FDay, SizeOf(Byte));

   // read hour
   Read(FHour, SizeOf(Byte));

   // read minute
   Read(FMinute, SizeOf(Byte));

   // read second
   Read(FSecond, SizeOf(Byte));
  end;
end;

procedure TChunkPngTime.SaveToStream(Stream: TStream);
begin
 FChunkSize := 7;

 inherited;

 with Stream do
  begin
   // write year
   WriteSwappedWord(Stream, FYear);

   // write month
   Write(FMonth, SizeOf(Byte));

   // write day
   Write(FDay, SizeOf(Byte));

   // write hour
   Write(FHour, SizeOf(Byte));

   // write minute
   Write(FMinute, SizeOf(Byte));

   // write second
   Write(FSecond, SizeOf(Byte));
  end;
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
 else inherited;
end;

class function TChunkPngEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
 Result := 'iCCP';
end;

procedure TChunkPngEmbeddedIccProfile.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FProfileName, 80);
   while (Position < Size) do
    begin
     Read(FProfileName[Index], SizeOf(Byte));
     if FProfileName[Index] = #0 then
      begin
       SetLength(FProfileName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression method
   Read(FCompressionMethod, 1);

   // not yet completed
  end;
end;

procedure TChunkPngEmbeddedIccProfile.SaveToStream(Stream: TStream);
var
  Temp  : Byte;
begin
 FChunkSize := Length(FProfileName) + 2;

 inherited;

 with Stream do
  begin
   // write keyword
   Write(FProfileName[1], Length(FProfileName));

   // write separator
   Temp := 0;
   Write(Temp, 1);

   // write compression method
   Write(FCompressionMethod, 1);
  end;
end;


{ TChunkPngGamma }

procedure TChunkPngGamma.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngGamma then
  with TChunkPngGamma(Dest) do
   begin
    FGamma := Self.FGamma;
   end
 else inherited;
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

procedure TChunkPngGamma.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 4
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read gamma
   FGamma := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngGamma.SaveToStream(Stream: TStream);
begin
 FChunkSize := 4;

 inherited;

 with Stream do
  begin
   // write gamma
   WriteSwappedCardinal(Stream, FGamma);
  end;
end;


{ TChunkPngStandardColorSpaceRGB }

procedure TChunkPngStandardColorSpaceRGB.AssignTo(Dest: TPersistent);
begin
 if Dest is TChunkPngStandardColorSpaceRGB then
  with TChunkPngStandardColorSpaceRGB(Dest) do
   begin
    FRenderingIntent := Self.FRenderingIntent;
   end
 else inherited;
end;

class function TChunkPngStandardColorSpaceRGB.GetClassChunkName: TChunkName;
begin
 Result := 'sRGB';
end;

procedure TChunkPngStandardColorSpaceRGB.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 1
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read rendering intent
   Read(FRenderingIntent, SizeOf(Byte));
  end;
end;

procedure TChunkPngStandardColorSpaceRGB.SaveToStream(Stream: TStream);
begin
 FChunkSize := 1;

 inherited;

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
 else inherited;
end;

function TChunkPngPrimaryChromaticities.GetBlueX: Single;
begin
 Result := FBlueX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetBlueY: Single;
begin
 Result := FBlueY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenX: Single;
begin
 Result := FGreenX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenY: Single;
begin
 Result := FGreenY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedX: Single;
begin
 Result := FRedX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedY: Single;
begin
 Result := FRedY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteX: Single;
begin
 Result := FWhiteX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteY: Single;
begin
 Result := FWhiteY * 1E-6;
end;

procedure TChunkPngPrimaryChromaticities.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 32
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read white point x
   FWhiteX := ReadSwappedCardinal(Stream);

   // read white point y
   FWhiteY := ReadSwappedCardinal(Stream);

   // read red x
   FRedX := ReadSwappedCardinal(Stream);

   // read red y
   FRedY := ReadSwappedCardinal(Stream);

   // read green x
   FGreenX := ReadSwappedCardinal(Stream);

   // read green y
   FGreenY := ReadSwappedCardinal(Stream);

   // read blue x
   FBlueX := ReadSwappedCardinal(Stream);

   // read blue y
   FBlueY := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngPrimaryChromaticities.SaveToStream(Stream: TStream);
begin
 FChunkSize := 32;

 inherited;


 with Stream do
  begin
   if Size < 32
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // write white point x
   WriteSwappedCardinal(Stream, FWhiteX);

   // write white point y
   WriteSwappedCardinal(Stream, FWhiteY);

   // write red x
   WriteSwappedCardinal(Stream, FRedX);

   // write red y
   WriteSwappedCardinal(Stream, FRedY);

   // write green x
   WriteSwappedCardinal(Stream, FGreenX);

   // write green y
   WriteSwappedCardinal(Stream, FGreenY);

   // write blue x
   WriteSwappedCardinal(Stream, FBlueX);

   // write blue y
   WriteSwappedCardinal(Stream, FBlueY);
  end;
end;

procedure TChunkPngPrimaryChromaticities.SetBlueX(const Value: Single);
begin
 FBlueX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetBlueY(const Value: Single);
begin
 FBlueY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenX(const Value: Single);
begin
 FGreenX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenY(const Value: Single);
begin
 FGreenY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedX(const Value: Single);
begin
 FRedX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedY(const Value: Single);
begin
 FRedY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteX(const Value: Single);
begin
 FWhiteX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteY(const Value: Single);
begin
 FWhiteY := Round(Value * 1E6);
end;


{ TPngSignificantBitsFormat0 }

procedure TPngSignificantBitsFormat0.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat0 then
  with TPngSignificantBitsFormat0(Dest) do
   begin
    FGrayBits := Self.FGrayBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat0.GetChunkSize: Integer;
begin
 Result := 1;
end;

procedure TPngSignificantBitsFormat0.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FGrayBits, 1);
end;

procedure TPngSignificantBitsFormat0.SaveToStream(Stream: TStream);
begin
 Stream.Write(FGrayBits, 1);
end;


{ TPngSignificantBitsFormat23 }

procedure TPngSignificantBitsFormat23.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat23 then
  with TPngSignificantBitsFormat23(Dest) do
   begin
    FRedBits   := Self.FRedBits;
    FBlueBits  := Self.FBlueBits;
    FGreenBits := Self.FGreenBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat23.GetChunkSize: Integer;
begin
 Result := 3;
end;

procedure TPngSignificantBitsFormat23.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FRedBits, 1);
 Stream.Read(FGreenBits, 1);
 Stream.Read(FBlueBits, 1);
end;

procedure TPngSignificantBitsFormat23.SaveToStream(Stream: TStream);
begin
 Stream.Write(FRedBits, 1);
 Stream.Write(FGreenBits, 1);
 Stream.Write(FBlueBits, 1);
end;


{ TPngSignificantBitsFormat4 }

procedure TPngSignificantBitsFormat4.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngSignificantBitsFormat4 then
  with TPngSignificantBitsFormat4(Dest) do
   begin
    FGrayBits  := Self.FGrayBits;
    FAlphaBits := Self.FAlphaBits;
   end
 else inherited;
end;

class function TPngSignificantBitsFormat4.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngSignificantBitsFormat4.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FGrayBits, 1);
 Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat4.SaveToStream(Stream: TStream);
begin
 Stream.Write(FGrayBits, 1);
 Stream.Write(FAlphaBits, 1);
end;


{ TPngSignificantBitsFormat6 }

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
 else inherited;
end;

class function TPngSignificantBitsFormat6.GetChunkSize: Integer;
begin
 Result := 4;
end;

procedure TPngSignificantBitsFormat6.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FRedBits, 1);
 Stream.Read(FGreenBits, 1);
 Stream.Read(FBlueBits, 1);
 Stream.Read(FAlphaBits, 1);
end;

procedure TPngSignificantBitsFormat6.SaveToStream(Stream: TStream);
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
 else inherited;
end;

constructor TChunkPngSignificantBits.Create(Header: TChunkPngImageHeader);
begin
 inherited;

 case Header.ColorType of
  ctGrayscale      : FSignificantBits := TPngSignificantBitsFormat0.Create;
  ctTrueColor,
  ctIndexedColor   : FSignificantBits := TPngSignificantBitsFormat23.Create;
  ctGrayscaleAlpha : FSignificantBits := TPngSignificantBitsFormat4.Create;
  ctTrueColorAlpha : FSignificantBits := TPngSignificantBitsFormat6.Create;
 end;
end;

destructor TChunkPngSignificantBits.Destroy;
begin
 if Assigned(FSignificantBits)
  then FreeAndNil(FSignificantBits);

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
  ctGrayscale :
   if not (FSignificantBits is TPngSignificantBitsFormat0) then
    begin
     FSignificantBits := TPngSignificantBitsFormat0.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
  ctTrueColor, ctIndexedColor :
   if not (FSignificantBits is TPngSignificantBitsFormat23) then
    begin
     FSignificantBits := TPngSignificantBitsFormat23.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
   ctTrueColorAlpha :
   if not (FSignificantBits is TPngSignificantBitsFormat4) then
    begin
     FSignificantBits := TPngSignificantBitsFormat4.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
  ctGrayscaleAlpha :
   if not (FSignificantBits is TPngSignificantBitsFormat6) then
    begin
     FSignificantBits := TPngSignificantBitsFormat6.Create;
     if Assigned(OldSignificantBits) then
      begin
       FSignificantBits.Assign(OldSignificantBits);
       FreeAndNil(OldSignificantBits);
      end;
    end;
  else if Assigned(FSignificantBits) then FreeAndNil(FSignificantBits);
 end;
end;

procedure TChunkPngSignificantBits.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FSignificantBits) then
  begin
   if Stream.Size < FSignificantBits.ChunkSize
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   FSignificantBits.LoadFromStream(Stream);
  end;
end;

procedure TChunkPngSignificantBits.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 if Assigned(FSignificantBits)
  then FChunkSize := FSignificantBits.GetChunkSize
  else FChunkSize := 0;

 inherited;

 if Assigned(FSignificantBits)
  then FSignificantBits.SaveToStream(Stream);
end;


{ TPngBackgroundColorFormat04 }

procedure TPngBackgroundColorFormat04.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngBackgroundColorFormat04 then
  with TPngBackgroundColorFormat04(Dest) do
   begin
    FGraySampleValue := Self.FGraySampleValue;
   end
 else inherited;
end;

class function TPngBackgroundColorFormat04.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngBackgroundColorFormat04.LoadFromStream(Stream: TStream);
begin
 FGraySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat04.SaveToStream(Stream: TStream);
begin
 WriteSwappedWord(Stream, FGraySampleValue);
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
 else inherited;
end;

class function TPngBackgroundColorFormat26.GetChunkSize: Integer;
begin
 Result := 6;
end;

procedure TPngBackgroundColorFormat26.LoadFromStream(Stream: TStream);
begin
 FRedSampleValue := ReadSwappedWord(Stream);
 FGreenSampleValue := ReadSwappedWord(Stream);
 FBlueSampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColorFormat26.SaveToStream(Stream: TStream);
begin
 WriteSwappedWord(Stream, FRedSampleValue);
 WriteSwappedWord(Stream, FGreenSampleValue);
 WriteSwappedWord(Stream, FBlueSampleValue);
end;


{ TPngBackgroundColorFormat3 }

procedure TPngBackgroundColorFormat3.AssignTo(Dest: TPersistent);
begin
 if Dest is TPngBackgroundColorFormat3 then
  with TPngBackgroundColorFormat3(Dest) do
   begin
    FIndex := Self.FIndex;
   end
 else inherited;
end;

class function TPngBackgroundColorFormat3.GetChunkSize: Integer;
begin
 Result := 1;
end;

procedure TPngBackgroundColorFormat3.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColorFormat3.SaveToStream(Stream: TStream);
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
 else inherited;
end;

constructor TChunkPngBackgroundColor.Create(Header: TChunkPngImageHeader);
begin
 inherited;

 case Header.ColorType of
  ctGrayscale, ctGrayscaleAlpha : FBackground := TPngBackgroundColorFormat04.Create;
  ctTrueColor, ctTrueColorAlpha: FBackground := TPngBackgroundColorFormat26.Create;
  ctIndexedColor: FBackground := TPngBackgroundColorFormat3.Create;
 end;
end;

destructor TChunkPngBackgroundColor.Destroy;
begin
 if Assigned(FBackground)
  then FreeAndNil(FBackground);
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
  ctGrayscale, ctGrayscaleAlpha :
   if not (FBackground is TPngBackgroundColorFormat04) then
    begin
     FBackground := TPngBackgroundColorFormat04.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  ctTrueColor, ctTrueColorAlpha :
   if not (FBackground is TPngBackgroundColorFormat26) then
    begin
     FBackground := TPngBackgroundColorFormat26.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  ctIndexedColor :
   if not (FBackground is TPngBackgroundColorFormat3) then
    begin
     FBackground := TPngBackgroundColorFormat3.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  else if Assigned(FBackground) then FreeAndNil(FBackground);

 end;
end;

procedure TChunkPngBackgroundColor.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FBackground) then
  begin
   if Stream.Size < FBackground.ChunkSize
    then raise EPngError.Create(RCStrChunkSizeTooSmall);
  end;
end;

procedure TChunkPngBackgroundColor.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 if Assigned(FBackground)
  then FChunkSize := FBackground.GetChunkSize
  else FChunkSize := 0;

 inherited;

 if Assigned(FBackground)
  then FBackground.SaveToStream(Stream);
end;


{ TChunkPngImageHistogram }

class function TChunkPngImageHistogram.GetClassChunkName: TChunkName;
begin
 Result := 'hIST';
end;

procedure TChunkPngImageHistogram.LoadFromStream(Stream: TStream);
begin
 inherited;

 // yet todo
end;

procedure TChunkPngImageHistogram.SaveToStream(Stream: TStream);
begin
 inherited;

 raise EPngError.Create(RCStrNotYetImplemented);
 // yet todo
end;


{ TChunkPngSuggestedPalette }

class function TChunkPngSuggestedPalette.GetClassChunkName: TChunkName;
begin
  Result := 'sPLT';
end;

procedure TChunkPngSuggestedPalette.LoadFromStream(Stream: TStream);
begin
  inherited;

end;

procedure TChunkPngSuggestedPalette.SaveToStream(Stream: TStream);
begin
  inherited;

  raise EPngError.Create(RCStrNotYetImplemented);
  // yet todo
end;


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
     else inherited;
   end else inherited;
end;

procedure TChunkList.Clear;
var
  Index : Integer;
begin
 for Index := 0 to Count - 1
  do FreeAndNil(FChunks[Index]);
 SetLength(FChunks, 0)
end;

procedure TChunkList.Delete(Index: Integer);
begin
 if (Index < 0) or (Index >= Count)
  then raise EPngError.Create(RCStrEmptyChunkList);
 FreeAndNil(FChunks[Index]);
 if Index < Count
  then System.Move(FChunks[Index + 1], FChunks[Index], (Count - Index) * SizeOf(Pointer));
 SetLength(FChunks, Length(FChunks) - 1);
end;

function TChunkList.GetChunk(Index: Integer): TCustomChunk;
begin
 if Cardinal(Index) >= Cardinal(Count)
  then raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else Result := FChunks[Index];
end;

function TChunkList.GetCount: Integer;
begin
 Result := Length(FChunks);
end;

function TChunkList.IndexOf(Item: TCustomChunk): Integer;
begin
 for Result := 0 to Count - 1 do
  if FChunks[Result] = Item
   then Exit;
 Result := -1;
end;

procedure TChunkList.Remove(Item: TCustomChunk);
begin
 Delete(IndexOf(Item));
end;


{ TCustomPngCoder }

constructor TCustomPngCoder.Create(Stream: TStream;
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma = nil;
  Palette: TChunkPngPalette = nil);
begin
 FStream       := Stream;
 FHeader       := Header;
 FGamma        := Gamma;
 FPalette      := Palette;
 FMappingTable := nil;
 BuildMappingTable;
 inherited Create;
end;

destructor TCustomPngCoder.Destroy;
begin
 Dispose(FMappingTable);
 inherited;
end;

procedure TCustomPngCoder.BuildMappingTable;
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
   if Assigned(FPalette) then
    begin
     GetMem(FMappingTable, FPalette.Count * SizeOf(TRGB24));
     Palette := PRGB24Array(FMappingTable);

     if Assigned(FGamma) then
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
      for Index := 0 to FPalette.Count - 1
       do Palette[Index] := FPalette.PaletteEntry[Index];
    end
   else
    begin
     // create gray scale palette
     GetMem(FMappingTable, 256 * SizeOf(TRGB24));
     Palette := PRGB24Array(FMappingTable);
     MaxByte := ((1 shl FHeader.BitDepth) - 1) and $FF;
     FracVal := 1 / MaxByte;

     if Assigned(FGamma) then
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
  end
 else
  begin
   GetMem(FMappingTable, 256);
   if Assigned(FGamma) and (FGamma.Gamma <> 0) then
    begin
     PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
     for Index := 0 to $FF
      do FMappingTable[Index] := Round(Power((Index * COne255th), PreCalcGamma) * 255);
    end
   else
    for Index := 0 to $FF
     do FMappingTable[Index] := Index;
  end;
end;

procedure TCustomPngCoder.DecodeFilterSub(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + CurrentRow[Index - PixelByteSize]) and $FF;
{$ELSE}
asm
 ADD     EDX, 1
 MOV     EAX, EDX
 MOV     ECX, BytesPerRow
 ADD     EAX, PixelByteSize
 SUB     ECX, PixelByteSize
 LEA     EAX, EAX + ECX
 LEA     EDX, EDX + ECX
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
end;

procedure TCustomPngCoder.DecodeFilterUp(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index]) and $FF;
{$ELSE}
asm
 MOV     EAX, EDX
 MOV     EDX, ECX
 MOV     ECX, BytesPerRow
 LEA     EAX, EAX + ECX + 1
 LEA     EDX, EDX + ECX + 1
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
end;

procedure TCustomPngCoder.DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to PixelByteSize
  do CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index] shr 1) and $FF;

 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

{-$UNDEF PUREPASCAL}

function PaethPredictor(a, b, c: Byte): Integer; {$IFNDEF TARGET_x64} pascal; {$ENDIF}
{$IFDEF PUREPASCAL}
var
  DistA, DistB, DistC: Integer;
begin
 DistA := Abs(b - c);
 DistB := Abs(a - c);
 DistC := Abs(a + b - c * 2);

 if (DistA <= DistB) and (DistA <= DistC) then Result := a else
 if DistB <= DistC
  then Result := b
  else Result := c;
{$ELSE}
asm
{$IFDEF TARGET_x64}
        PUSH    RBX

        // calculate DistA
        MOVZX   RAX, b
        SUB     RAX, R8
        MOV     R10, RAX
        JAE     @PositiveDistA
        NOT     RAX
        INC     RAX

        @PositiveDistA:

        // calculate DistB
        MOVZX   RBX, a
        SUB     RBX, R8
        MOV     R11, RBX
        JAE     @PositiveDistB
        NOT     RBX
        INC     RBX

        @PositiveDistB:

        // calculate DistC
        ADD     R10, R11
        JAE     @PositiveDistC
        NOT     R10
        INC     R10

        @PositiveDistC:
        MOV     R11, RCX
        MOV     RCX, R10

        MOV     R12, RAX
        SUB     R12, RBX
        JA      @NextCheck
        MOV     R12, RAX
        SUB     R12, RCX
        JA      @NextCheck

        MOV     RAX, R11
        JMP     @Done

        @NextCheck:
        MOV     R12, RBX
        SUB     R12, RCX
        JA      @ResultC

        MOV     RAX, RDX
        JMP     @Done

        @ResultC:
        MOV     RAX, R8

        @Done:
        POP     RBX
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
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 DecodeFilterUp(CurrentRow, PreviousRow, PixelByteSize, PixelByteSize);

 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] +
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

 for Index := PixelByteSize + 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] - CurrentRow[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to PixelByteSize
  do OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index] shr 1) and $FF;

 for Index := PixelByteSize + 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] - (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 EncodeFilterUp(CurrentRow, PreviousRow, OutputRow, PixelByteSize, PixelByteSize);

 for Index := PixelByteSize + 1 to BytesPerRow
  do OutputRow[Index] := (CurrentRow[Index] -
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
  else raise EPngError.Create(RCStrUnsupportedFilter);
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
 for Index := 1 to BytesPerRow
  do Result := Result + Cardinal(Abs(SmallInt(CurrentRow[Index])));
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
 for PixelIndex := 1 to BytesPerRow
  do BestSum := BestSum + CurrentRow[PixelIndex];
 Move(CurrentRow^[1], OutputRow^[1], BytesPerRow);

 // check whether sub pre filter shall be used
 if aafmSub in FHeader.CompressionFilterMethods then
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
 if aafmUp in FHeader.CompressionFilterMethods then
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
 if aafmAverage in FHeader.CompressionFilterMethods then
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
 if aafmPaeth in FHeader.CompressionFilterMethods then
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
  Header: TChunkPngImageHeader; Gamma: TChunkPngGamma;
  Palette: TChunkPngPalette);
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
  else raise EPngError.Create(RCStrUnsupportedFilter);
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
 for PixelIndex := 1 to BytesPerRow
  do BestSum := BestSum + CurrentRow[PixelIndex];
 Move(CurrentRow^[1], OutputRow^[1], BytesPerRow);

 // check whether sub pre filter shall be used
 if aafmSub in FHeader.CompressionFilterMethods then
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
 if aafmUp in FHeader.CompressionFilterMethods then
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
 if aafmAverage in FHeader.CompressionFilterMethods then
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
 if aafmPaeth in FHeader.CompressionFilterMethods then
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

 FreeAndNil(FAdditionalChunkList);
 FreeAndNil(FDataChunkList);
 FreeAndNil(FImageHeader);

 // free palette chunk
 if Assigned(FPaletteChunk)
  then FreeAndNil(FPaletteChunk);

 // free gamma chunk
 if Assigned(FGammaChunk)
  then FreeAndNil(FGammaChunk);

 // free time chunk
 if Assigned(FTimeChunk)
  then FreeAndNil(FTimeChunk);

 // free time chunk
 if Assigned(FSignificantBits)
  then FreeAndNil(FSignificantBits);

 // free physical pixel dimensions chunk
 if Assigned(FPhysicalDimensions)
  then FreeAndNil(FPhysicalDimensions);

 // free primary chromaticities chunk
 if Assigned(FChromaChunk)
  then FreeAndNil(FChromaChunk);

 inherited;
end;

procedure TPortableNetworkGraphic.SetPaletteChunk(
  const Value: TChunkPngPalette);
begin
 if Assigned(FPaletteChunk) then
  if Assigned(Value)
   then FPaletteChunk.Assign(Value)
   else FreeAndNil(FPaletteChunk)
  else
   if Assigned(Value) then
    begin
     FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
     FPaletteChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetPhysicalDimensions(
  const Value: TChunkPngPhysicalPixelDimensions);
begin
 if Assigned(FPhysicalDimensions) then
  if Assigned(Value)
   then FPhysicalDimensions.Assign(Value)
   else FreeAndNil(FPhysicalDimensions)
  else
   if Assigned(Value) then
    begin
     FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
     FPhysicalDimensions.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetSignificantBits(
  const Value: TChunkPngSignificantBits);
begin
 if Assigned(FSignificantBits) then
  if Assigned(Value)
   then FSignificantBits.Assign(Value)
   else FreeAndNil(FSignificantBits)
  else
   if Assigned(Value) then
    begin
     FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
     FSignificantBits.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetTimeChunk(const Value: TChunkPngTime);
begin
 if Assigned(FTimeChunk) then
  if Assigned(Value)
   then FTimeChunk.Assign(Value)
   else FreeAndNil(FTimeChunk)
  else
   if Assigned(Value) then
    begin
     FTimeChunk := TChunkPngTime.Create(FImageHeader);
     FTimeChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitX(const Value: Cardinal);
begin
 if Value = 0
  then raise EPngError.Create(RCStrWrongPixelPerUnit);

 if not Assigned(FPhysicalDimensions)
  then FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

 FPhysicalDimensions.PixelsPerUnitX := Value;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitY(const Value: Cardinal);
begin
 if Value = 0
  then raise EPngError.Create(RCStrWrongPixelPerUnit);

 if not Assigned(FPhysicalDimensions)
  then FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

 FPhysicalDimensions.PixelsPerUnitY := Value;
end;

procedure TPortableNetworkGraphic.SetPixelUnit(const Value: Byte);
begin
 if Value > 1
  then raise EPngError.Create(RCStrUnspecifiedPixelUnit);

 if not Assigned(FPhysicalDimensions)
  then FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);

 FPhysicalDimensions.PixelUnit := Value;
end;

procedure TPortableNetworkGraphic.SetChromaChunk(
  const Value: TChunkPngPrimaryChromaticities);
begin
 if Assigned(FChromaChunk) then
  if Assigned(Value)
   then FChromaChunk.Assign(Value)
   else FreeAndNil(FChromaChunk)
  else
   if Assigned(Value) then
    begin
     FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
     FChromaChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetGammaChunk(const Value: TChunkPngGamma);
begin
 if Assigned(FGammaChunk) then
  if Assigned(Value)
   then FGammaChunk.Assign(Value)
   else FreeAndNil(FGammaChunk)
  else
   if Assigned(Value) then
    begin
     FGammaChunk := TChunkPngGamma.Create(FImageHeader);
     FGammaChunk.Assign(Value);
    end;
end;

procedure TPortableNetworkGraphic.SetImageHeader(
  const Value: TChunkPngImageHeader);
begin
 if not Assigned(Value)
  then raise EPngError.Create(RCStrNewHeaderError)
  else FImageHeader.Assign(Value);
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
 if Assigned(FImageHeader) then
  if FImageHeader.FAdaptiveFilterMethods <> Value then
   begin
    FImageHeader.FAdaptiveFilterMethods := Value;
    AdaptiveFilterMethodsChanged;
   end;
end;

procedure TPortableNetworkGraphic.SetCompressionLevel(const Value: Byte);
begin
 if not (Value in [1..9])
  then raise EPngError.Create(RCStrInvalidCompressionLevel);

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
 if Assigned(FTimeChunk)
  then FTimeChunk.ModifiedDateTime := Value;
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

   if (Stream.Size - Stream.Position) < ChunkSize
    then ChunkSize := (Stream.Size - Stream.Position);

   // copy data to IDAT chunk
   DataChunk.Data.CopyFrom(Stream, ChunkSize);

   // add data chunk to data chunk list
   FDataChunkList.Add(DataChunk);
  end;
end;

procedure TPortableNetworkGraphic.DecompressImageDataToStream(Stream: TStream);
var
  DataStream : TMemoryStream;
begin
 DataStream := TMemoryStream.Create;
 try
  // copy image data from all data chunks to one continous data stream
  CopyImageData(DataStream);

  // check whether compression method is supported
  if FImageHeader.CompressionMethod <> 0
   then raise EPngError.Create(RCStrUnsupportedCompressionMethod);

  // reset data stream position to zero
  DataStream.Seek(0, soFromBeginning);

  // decompress z-stream
  ZDecompress(DataStream, Stream);
 finally
  FreeAndNil(DataStream);
 end;
end;

procedure TPortableNetworkGraphic.CompressImageDataFromStream(Stream: TStream);
var
  DataStream : TMemoryStream;
begin
 DataStream := TMemoryStream.Create;
 try
  // set compression method
  FImageHeader.CompressionMethod := 0;

  // compress Stream to DataStream
  if Stream is TMemoryStream
   then ZCompress(TMemoryStream(Stream), DataStream, FCompressionLevel)
   else raise EPngError.Create(RCStrNotYetImplemented);

  // reset data stream position to zero
  DataStream.Seek(0, soFromBeginning);

  // copy image data from all data chunks to one continous data stream
  StoreImageData(DataStream);
 finally
  FreeAndNil(DataStream);
 end;
end;

class function TPortableNetworkGraphic.CanLoad(const FileName: TFileName): Boolean;
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   Result := CanLoad(FileStream);
  finally
   Free;
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
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 try
  LoadFromStream(FileStream);
 finally
  FreeAndNil(FileStream);
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
begin
 with Stream do
  begin
   Clear;

   // check for minimum file size
   if Size < 8
    then raise EPngError.Create(RCStrNotAValidPNGFile);

   // read chunk ID
   Read(ChunkName, 4);
   if ChunkName <> '‰PNG'
    then raise EPngError.Create(RCStrNotAValidPNGFile);

   // read PNG magic
   Read(ChunkName, 4);
   if ChunkName <> CPngMagic
    then raise EPngError.Create(RCStrNotAValidPNGFile);

   MemoryStream := TMemoryStream.Create;
   try
    // read image header chunk size
    ChunkSize := ReadSwappedCardinal(Stream);
    if ChunkSize > Stream.Size - 12
     then raise EPngError.Create(RCStrNotAValidPNGFile);

    // read image header chunk ID
    Read(ChunkName, 4);
    if ChunkName <> 'IHDR'
     then raise EPngError.Create(RCStrNotAValidPNGFile);

    // reset position to the chunk start and copy stream to memory
    Seek(-8, soCurrent);
    MemoryStream.CopyFrom(Stream, ChunkSize + 8);
    MemoryStream.Seek(0, soFromBeginning);

    // load image header
    FImageHeader.LoadFromStream(MemoryStream);

    // read image header chunk size
    ChunkCRC := 0;
    Read(ChunkCRC, 4);
    {$IFDEF CheckCRC}
    if not CheckCRC(MemoryStream, Swap32(ChunkCRC))
     then raise EPngError.Create(RCStrCRCError);
    {$ENDIF}

    while Stream.Position < Stream.Size do
     begin
      // read image header chunk size
      ChunkSize := ReadSwappedCardinal(Stream);
      if ChunkSize > Stream.Size - Stream.Position - 4
       then raise EPngError.Create(RCStrNotAValidPNGFile);

      // read chunk ID
      Read(ChunkName, 4);

      // check for stream end
      if ChunkName = 'IEND' then
       begin
        // read image header chunk size
        Read(ChunkCRC, 4);

        {$IFDEF CheckCRC}
        if ChunkCRC <> 2187346606
         then raise EPngError.Create(RCStrCRCError);
        {$ENDIF}

        Break;
       end;

      {$IFNDEF LinearStream}
      // reset position to the chunk start and copy stream to memory
      Seek(-8, soCurrent);
      {$ENDIF}
      MemoryStream.Clear;
      {$IFDEF LinearStream}
      WriteSwappedCardinal(MemoryStream, ChunkSize);
      MemoryStream.Write(ChunkName, 4);
      MemoryStream.CopyFrom(Stream, ChunkSize);
      {$ELSE}
      MemoryStream.CopyFrom(Stream, ChunkSize + 8);
      {$ENDIF}

      // reset memory stream to beginning of the chunk
      MemoryStream.Seek(0, soFromBeginning);

      if ChunkName = 'IHDR'
       then raise EPngError.Create(RCStrNotAValidPNGFile) else
      if ChunkName = 'IDAT'
       then ReadImageDataChunk(MemoryStream) else
      if ChunkName = 'gAMA' then
       begin
        if Assigned(FGammaChunk)
         then raise EPngError.Create(RCStrSeveralGammaChunks);
        FGammaChunk := TChunkPngGamma.Create(FImageHeader);
        FGammaChunk.LoadFromStream(MemoryStream);
       end else
      if ChunkName = 'cHRM' then
       begin
        if Assigned(FChromaChunk)
         then raise EPngError.Create(RCStrSeveralChromaChunks);
        FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
        FChromaChunk.LoadFromStream(MemoryStream);
       end else
      if ChunkName = 'tIME' then
       begin
        if Assigned(FTimeChunk)
         then raise EPngError.Create(RCStrSeveralTimeChunks);
        FTimeChunk := TChunkPngTime.Create(FImageHeader);
        FTimeChunk.LoadFromStream(MemoryStream);
       end else
      if ChunkName = 'sBIT' then
       begin
        if Assigned(FSignificantBits)
         then raise EPngError.Create(RCStrSeveralSignificantBitsChunksFound);
        FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
        FSignificantBits.LoadFromStream(MemoryStream);
       end else
      if ChunkName = 'pHYs' then
       begin
        if Assigned(FPhysicalDimensions)
         then raise EPngError.Create(RCStrSeveralPhysicalPixelDimensionChunks);
        FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
        FPhysicalDimensions.LoadFromStream(MemoryStream);
       end else
      if ChunkName = 'PLTE' then
       begin
        if Assigned(FPaletteChunk)
         then raise EPngError.Create(RCStrSeveralPaletteChunks);
        FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
        FPaletteChunk.LoadFromStream(MemoryStream);
       end
      else
       begin
        ChunkClass := FindPngChunkByChunkName(ChunkName);
        if ChunkClass <> nil then
         begin
          Chunk := ChunkClass.Create(FImageHeader);
          Chunk.LoadFromStream(MemoryStream);
          FAdditionalChunkList.Add(Chunk);
         end
        else
         begin
          // check if chunk is ancillary
          if (Byte(ChunkName[0]) and $80) = 0
           then ReadUnknownChunk(MemoryStream)
           else raise EPngError.Create(RCStrAncillaryUnknownChunk);
         end;
       end;

      // read & check CRC
      Read(ChunkCRC, 4);
      {$IFDEF CheckCRC}
      if not CheckCRC(MemoryStream, Swap32(ChunkCRC))
       then raise EPngError.Create(RCStrCRCError);
      {$ENDIF}

     end;
   finally
    FreeAndNil(MemoryStream);
   end;
  end;
end;

procedure TPortableNetworkGraphic.SaveToFile(Filename: TFilename);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 try
  SaveToStream(FileStream);
 finally
  FreeAndNil(FileStream);
 end;
end;

procedure TPortableNetworkGraphic.SaveToStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  CRC          : Cardinal;
  MemoryStream : TMemoryStream;
  Index        : Integer;

  procedure SaveChunkToStream(Chunk: TCustomChunk);
  begin
   MemoryStream.Clear;
   Chunk.SaveToStream(MemoryStream);

   // copy memory stream to stream
   MemoryStream.Seek(0, soFromBeginning);
   Stream.CopyFrom(MemoryStream, MemoryStream.Size);

   // calculate and write CRC
   CRC := Swap32(CalculateCRC(MemoryStream));
   Write(CRC, SizeOf(Cardinal));
  end;

begin
 with Stream do
  begin
   // write chunk ID
   ChunkName := '‰PNG';
   Write(ChunkName, 4);

   // write PNG magic
   ChunkName := CPngMagic;
   Write(ChunkName, 4);

   MemoryStream := TMemoryStream.Create;
   try
    // save image header to memory stream
    FImageHeader.SaveToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Seek(0, soFromBeginning);
    Stream.CopyFrom(MemoryStream, MemoryStream.Size);

    // calculate and write CRC
    CRC := Swap32(CalculateCRC(MemoryStream));
    Write(CRC, SizeOf(Cardinal));

    // eventually save physical pixel dimensions chunk
    if Assigned(FPhysicalDimensions) then
     begin
      MemoryStream.Clear;
      FPhysicalDimensions.SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;

    // eventually save significant bits chunk
    if Assigned(FSignificantBits) then
     begin
      MemoryStream.Clear;
      FSignificantBits.SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;

    // eventually save gamma chunk
    if Assigned(FGammaChunk) then
     begin
      MemoryStream.Clear;
      FGammaChunk.SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;

    // eventually save chroma chunk
    if Assigned(FChromaChunk) then
     begin
      MemoryStream.Clear;
      FChromaChunk.SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;

    // eventually save palette chunk
    if Assigned(FPaletteChunk) then
     begin
      MemoryStream.Clear;
      FPaletteChunk.SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;

    // store additional chunks
    for Index := 0 to FAdditionalChunkList.Count - 1 do
     begin
      MemoryStream.Clear;
      TCustomDefinedChunk(FAdditionalChunkList[Index]).SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;

    // save data streams
    for Index := 0 to FDataChunkList.Count - 1 do
     begin
      MemoryStream.Clear;
      TCustomDefinedChunk(FDataChunkList[Index]).SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));
     end;
   finally
    FreeAndNil(MemoryStream);
   end;

   // write chunk size
   WriteSwappedCardinal(Stream, 0);

   // write chunk ID
   ChunkName := 'IEND';
   Write(ChunkName, 4);

   // write CRC
   CRC := 2187346606;
   Write(CRC, 4);
  end;
end;

procedure TPortableNetworkGraphic.ReadUnknownChunk(Stream: TStream);
var
  UnknownChunk : TUnknownChunk;
begin
 UnknownChunk := TUnknownChunk.Create;
 UnknownChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TPortableNetworkGraphic.RemoveGammaInformation;
begin
 if Assigned(FGammaChunk)
  then FreeAndNil(FGammaChunk);
end;

procedure TPortableNetworkGraphic.RemoveModifiedTimeInformation;
begin
 if Assigned(FTimeChunk)
  then FreeAndNil(FTimeChunk);
end;

procedure TPortableNetworkGraphic.RemovePhysicalPixelDimensionsInformation;
begin
 if Assigned(FPhysicalDimensions)
  then FreeAndNil(FPhysicalDimensions);
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
  FreeAndNil(TempStream);
 end;
end;

procedure TPortableNetworkGraphic.AdaptiveFilterMethodsChanged;
begin
 if FDataChunkList.Count > 0 then
  begin
   // transcoding!
   raise EPngError.Create(RCStrNotYetImplemented);
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
   else raise EPngError.Create(RCStrWrongInterlaceMethod);
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
  FreeAndNil(TempStream);
 end;
end;

procedure TPortableNetworkGraphic.ReadImageDataChunk(Stream: TStream);
var
  ImageDataChunk : TChunkPngImageData;
begin
 ImageDataChunk := TChunkPngImageData.Create(FImageHeader);
 ImageDataChunk.LoadFromStream(Stream);
 FDataChunkList.Add(ImageDataChunk);
end;

procedure TPortableNetworkGraphic.Assign(Source: TPersistent);
begin
 if Source is TPortableNetworkGraphic then
  with TPortableNetworkGraphic(Source) do
   begin
    if Assigned(Self.FImageHeader) then Self.FImageHeader.Assign(FImageHeader);

    // assign palette chunk
    if Assigned(Self.FPaletteChunk) then
     if Assigned(FPaletteChunk)
      then Self.FPaletteChunk.Assign(FPaletteChunk)
      else FreeAndNil(Self.FPaletteChunk) else
    if Assigned(FPaletteChunk) then
     begin
      Self.FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
      Self.FPaletteChunk.Assign(FPaletteChunk);
     end;

    // assign gamma chunk
    if Assigned(Self.FGammaChunk) then
     if Assigned(FGammaChunk)
      then Self.FGammaChunk.Assign(FGammaChunk)
      else FreeAndNil(Self.FGammaChunk) else
    if Assigned(FGammaChunk) then
     begin
      Self.FGammaChunk := TChunkPngGamma.Create(FImageHeader);
      Self.FGammaChunk.Assign(FGammaChunk);
     end;

    // assign time chunk
    if Assigned(Self.FTimeChunk) then
     if Assigned(FTimeChunk)
      then Self.FTimeChunk.Assign(FTimeChunk)
      else FreeAndNil(Self.FTimeChunk) else
    if Assigned(FTimeChunk) then
     begin
      Self.FTimeChunk := TChunkPngTime.Create(FImageHeader);
      Self.FTimeChunk.Assign(FTimeChunk);
     end;

    // assign significant bits
    if Assigned(Self.FSignificantBits) then
     if Assigned(FSignificantBits)
      then Self.FSignificantBits.Assign(FSignificantBits)
      else FreeAndNil(Self.FSignificantBits) else
    if Assigned(FSignificantBits) then
     begin
      Self.FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
      Self.FSignificantBits.Assign(FSignificantBits);
     end;

    // assign physical dimensions
    if Assigned(Self.FPhysicalDimensions) then
     if Assigned(FPhysicalDimensions)
      then Self.FPhysicalDimensions.Assign(FPhysicalDimensions)
      else FreeAndNil(Self.FPhysicalDimensions) else
    if Assigned(FPhysicalDimensions) then
     begin
      Self.FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create(FImageHeader);
      Self.FPhysicalDimensions.Assign(FPhysicalDimensions);
     end;

    // assign primary chromaticities
    if Assigned(Self.FChromaChunk) then
     if Assigned(FChromaChunk)
      then Self.FChromaChunk.Assign(FChromaChunk)
      else FreeAndNil(Self.FChromaChunk) else
    if Assigned(FChromaChunk) then
     begin
      Self.FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
      Self.FChromaChunk.Assign(FChromaChunk);
     end;

    if Assigned(Self.FDataChunkList) then Self.FDataChunkList.Assign(FDataChunkList);
    if Assigned(Self.FAdditionalChunkList) then Self.FAdditionalChunkList.Assign(FAdditionalChunkList);
   end
 else inherited;
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
    FDataChunkList.Assign(Self.FDataChunkList);
    FAdditionalChunkList.Assign(Self.FAdditionalChunkList);
   end
 else inherited;
end;

function TPortableNetworkGraphic.CalculateCRC(Stream: TStream): Cardinal;
var
  CrcValue : Cardinal;
  Value    : Byte;
begin
 if Stream is TMemoryStream
  then Result := CalculateCRC(TMemoryStream(Stream).Memory, Stream.Size)
  else
   with Stream do
    begin
     Seek(4, soFromBeginning);

     // initialize CRC
     CrcValue := $FFFFFFFF;
     {$IFDEF FPC}
     Value := 0;
     {$ENDIF}

     while Position < Size do
      begin
       Read(Value, 1);

       CrcValue := GCrcTable^[(CrcValue xor Value) and $FF] xor (CrcValue shr 8);
      end;

     Result := (CrcValue xor $FFFFFFFF);

     Seek(0, soFromBeginning);
    end;
end;

function TPortableNetworkGraphic.CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
var
  CrcValue : Cardinal;
  Pos      : Cardinal;
begin
 // ignore size (offset by 4 bytes)
 Pos := 4;
 Inc(Buffer, 4);

 // initialize CRC
 CrcValue := $FFFFFFFF;

 while Pos < Count do
  begin
   CrcValue := GCrcTable^[(CrcValue xor Buffer^) and $FF] xor (CrcValue shr 8);
   Inc(Buffer);
   Inc(Pos);
  end;

 Result := (CrcValue xor $FFFFFFFF);
{$ELSE}
asm
{$IFDEF Target_x64}
        PUSH    RBX
        PUSH    RDI
        MOV     RCX, R8
        ADD     RDX, 4
        SUB     RCX, 4
        JS      @Done
        NEG     RCX
        MOV     RBX, $FFFFFFFF

        MOV     RDI, [GCrcTable]

@Start:
        MOV     EAX, [RDX]
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
        ADD     EDX, 4
        SUB     ECX, 4
        JS      @Done
        NEG     ECX
        MOV     EBX, $FFFFFFFF

        MOV     EDI, [GCrcTable]

@Start:
        MOVZX   EAX, [EDX]
        XOR     EAX, EBX
        AND     EAX, $FF
        MOV     EAX, [EDI + 4 * EAX]
        SHR     EBX, 8
        XOR     EAX, EBX
        MOV     EBX, EAX

        INC     EDX
        INC     ECX
        JS      @Start

        XOR     EBX, $FFFFFFFF
        MOV     Result, EBX

@Done:
        POP     EDI
        POP     EBX
{$ENDIF}
{$ENDIF}
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
 if Assigned(FGammaChunk)
  then Result := FGammaChunk.GammaAsSingle
  else Result := 1;
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
 if Assigned(FTimeChunk) then
  with FTimeChunk
   do Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0)
  else Result := 0;
end;

function TPortableNetworkGraphic.GetPaletteEntry(Index: Integer): TRGB24;
begin
 if Assigned(FPaletteChunk)
  then Result := FPaletteChunk.PaletteEntry[Index]
  else raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPortableNetworkGraphic.GetPaletteEntryCount: Integer;
begin
 if Assigned(FPaletteChunk)
  then Result := FPaletteChunk.Count
  else Result := 0;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitX: Cardinal;
begin
 if Assigned(FPhysicalDimensions)
  then Result := FPhysicalDimensions.PixelsPerUnitX
  else Result := 1;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitY: Cardinal;
begin
 if Assigned(FPhysicalDimensions)
  then Result := FPhysicalDimensions.PixelsPerUnitY
  else Result := 1;
end;

function TPortableNetworkGraphic.GetPixelUnit: Byte;
begin
 if Assigned(FPhysicalDimensions)
  then Result := FPhysicalDimensions.PixelUnit
  else Result := 0;
end;

function TPortableNetworkGraphic.GetWidth: Integer;
begin
 Result := FImageHeader.Width;
end;

function TPortableNetworkGraphic.HasGammaInformation: Boolean;
begin
 Result := Assigned(FGammaChunk);
end;

function TPortableNetworkGraphic.HasModifiedTimeInformation: Boolean;
begin
 Result := Assigned(FTimeChunk);
end;

function TPortableNetworkGraphic.HasPhysicalPixelDimensionsInformation: Boolean;
begin
 Result := Assigned(FPhysicalDimensions);
end;

procedure TPortableNetworkGraphic.Clear;
begin
 // clear chunk lists
 FDataChunkList.Clear;
 FAdditionalChunkList.Clear;

 // reset image header to default
 FImageHeader.ResetToDefault;

 // free palette chunk
 if Assigned(FPaletteChunk)
  then FreeAndNil(FPaletteChunk);

 // free gamma chunk
 if Assigned(FGammaChunk)
  then FreeAndNil(FGammaChunk);

 // free gamma chunk
 if Assigned(FChromaChunk)
  then FreeAndNil(FChromaChunk);

 // free time chunk
 if Assigned(FTimeChunk)
  then FreeAndNil(FTimeChunk);

 // free time chunk
 if Assigned(FSignificantBits)
  then FreeAndNil(FSignificantBits);

 // free physical pixel dimensions chunk
 if Assigned(FPhysicalDimensions)
  then FreeAndNil(FPhysicalDimensions);
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
 RowByteSize := 0;
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
    if FStream.Read(FRowBuffer[CurrentRow][0], FHeader.BytesPerRow + 1) <> FHeader.BytesPerRow + 1
     then raise EPngError.Create(RCStrDataIncomplete);

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

    with FHeader do
     case ColorType of
      ctGrayscale      : RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
      ctIndexedColor   : RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
      ctTrueColor      : RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
      ctGrayscaleAlpha : RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
      ctTrueColorAlpha : RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
      else Continue;
     end;

    PassRow := CRowStart[CurrentPass];

    // clear previous row
    FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize + 1, 0);

    // check if pre filter is used and eventually calculate pre filter
    if (FHeader.ColorType <> ctIndexedColor) and
      not (FHeader.CompressionFilterMethods = []) then
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
 RowByteSize := 0;
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

    with FHeader do
     case ColorType of
      ctGrayscale      : RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
      ctIndexedColor   : RowByteSize := (PixelPerRow * BitDepth + 7) div 8;
      ctTrueColor      : RowByteSize := (PixelPerRow * BitDepth * 3) div 8;
      ctGrayscaleAlpha : RowByteSize := (PixelPerRow * BitDepth * 2) div 8;
      ctTrueColorAlpha : RowByteSize := (PixelPerRow * BitDepth * 4) div 8;
      else Continue;
     end;

    PassRow := CRowStart[CurrentPass];

    // clear previous row
    FillChar(FRowBuffer[1 - CurrentRow]^[0], RowByteSize, 0);

    // process pixels
    while PassRow < FHeader.Height do
     begin
      // get interlaced row data
      if FStream.Read(FRowBuffer[CurrentRow][0], RowByteSize + 1) <> (RowByteSize + 1)
       then raise EPngError.Create(RCStrDataIncomplete);

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
    not (FHeader.CompressionFilterMethods = []) then
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
     if (c and 1) <> 0
      then c := Polynomial xor (c shr 1)
      else c := c shr 1;
    end;
   GCrcTable^[n] := c;
  end;
end;

{ TCustomDefinedChunkTextChunk }

procedure TCustomDefinedChunkTextChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDefinedChunkTextChunk then
  with TCustomDefinedChunkTextChunk(Dest) do
   begin
    FKeyword := Self.FKeyword;
    FText    := Self.FText;
   end
 else inherited;
end;

initialization
  BuildCrcTable($EDB88320);
  RegisterPngChunks([TChunkPngImageData, TChunkPngPalette, TChunkPngGamma,
    TChunkPngStandardColorSpaceRGB, TChunkPngPrimaryChromaticities,
    TChunkPngTime, TChunkPngTransparency, TChunkPngEmbeddedIccProfile,
    TChunkPngPhysicalPixelDimensions, TChunkPngTextChunk,
    TChunkPngCompressedTextChunk, TChunkPngInternationalTextChunk,
    TChunkPngImageHistogram, TChunkPngBackgroundColor,
    TChunkPngSignificantBits, TChunkPngImageOffset, TChunkPngPixelCalibrator]);


finalization
  if Assigned(GCrcTable) then Dispose(GCrcTable);

end.
