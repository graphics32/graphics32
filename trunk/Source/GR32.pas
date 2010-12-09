unit GR32;

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
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *   J. Tulach <tulach at position.cz>
 *   Jouni Airaksinen <markvera at spacesynth.net>
 *   Timothy Weber <teejaydub at users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, types, Controls, Graphics,{$ELSE}
  Windows, Messages, Controls, Graphics,{$ENDIF}
  Classes, SysUtils, GR32_System;
  
{ Version Control }

const
  Graphics32Version = '1.9.0';

{ 32-bit Color }

type
  PColor32 = ^TColor32;
  TColor32 = type Cardinal;

  PColor32Array = ^TColor32Array;
  TColor32Array = array [0..0] of TColor32;
  TArrayOfColor32 = array of TColor32;

  TColor32Component = (ccBlue, ccGreen, ccRed, ccAlpha);
  TColor32Components = set of TColor32Component;

  PColor32Entry = ^TColor32Entry;
  TColor32Entry = packed record
    case Integer of
      0: (B, G, R, A: Byte);
      1: (ARGB: TColor32);
      2: (Planes: array[0..3] of Byte);
      3: (Components: array[TColor32Component] of Byte);
  end;

  PColor32EntryArray = ^TColor32EntryArray;
  TColor32EntryArray = array [0..0] of TColor32Entry;
  TArrayOfColor32Entry = array of TColor32Entry;

  PPalette32 = ^TPalette32;
  TPalette32 = array [Byte] of TColor32;

const
  // Some predefined color constants
  clBlack32               = TColor32($FF000000);
  clDimGray32             = TColor32($FF3F3F3F);
  clGray32                = TColor32($FF7F7F7F);
  clLightGray32           = TColor32($FFBFBFBF);
  clWhite32               = TColor32($FFFFFFFF);
  clMaroon32              = TColor32($FF7F0000);
  clGreen32               = TColor32($FF007F00);
  clOlive32               = TColor32($FF7F7F00);
  clNavy32                = TColor32($FF00007F);
  clPurple32              = TColor32($FF7F007F);
  clTeal32                = TColor32($FF007F7F);
  clRed32                 = TColor32($FFFF0000);
  clLime32                = TColor32($FF00FF00);
  clYellow32              = TColor32($FFFFFF00);
  clBlue32                = TColor32($FF0000FF);
  clFuchsia32             = TColor32($FFFF00FF);
  clAqua32                = TColor32($FF00FFFF);

  // Some semi-transparent color constants
  clTrWhite32             = TColor32($7FFFFFFF);
  clTrBlack32             = TColor32($7F000000);
  clTrRed32               = TColor32($7FFF0000);
  clTrGreen32             = TColor32($7F00FF00);
  clTrBlue32              = TColor32($7F0000FF);

// Color construction and conversion functions
function Color32(WinColor: TColor): TColor32; overload;
function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;
function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;
function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
function WinColor(Color32: TColor32): TColor;
function ArrayOfColor32(Colors: array of TColor32): TArrayOfColor32;

// Color component access
procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);
procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte);
function Color32Components(R, G, B, A: Boolean): TColor32Components;
function RedComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function GreenComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function BlueComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function AlphaComponent(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function Intensity(Color32: TColor32): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

// Color space conversion
function HSLtoRGB(H, S, L: Single): TColor32; overload;
procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single); overload;
function HSLtoRGB(H, S, L: Integer): TColor32; overload;
procedure RGBtoHSL(RGB: TColor32; out H, S, L: Byte); overload;

{$IFNDEF PLATFORM_INDEPENDENT}
// Palette conversion functions
function WinPalette(const P: TPalette32): HPALETTE;
{$ENDIF}

{ A fixed-point type }

type
  // This type has data bits arrangement compatible with Windows.TFixed
  PFixed = ^TFixed;
  TFixed = type Integer;

  PFixedRec = ^TFixedRec;
  TFixedRec = packed record
    case Integer of
      0: (Fixed: TFixed);
      1: (Frac: Word; Int: SmallInt);
  end;

  PFixedArray = ^TFixedArray;
  TFixedArray = array [0..0] of TFixed;
  PArrayOfFixed = ^TArrayOfFixed;
  TArrayOfFixed = array of TFixed;
  PArrayOfArrayOfFixed = ^TArrayOfArrayOfFixed;
  TArrayOfArrayOfFixed = array of TArrayOfFixed;

  // TFloat determines the precision level for certain floating-point operations
  PFloat = ^TFloat;
  TFloat = Single;

{ Other dynamic arrays }
type
  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of Byte;
  PArrayOfByte = ^TArrayOfByte;
  TArrayOfByte = array of Byte;

  PWordArray = ^TWordArray;
  TWordArray = array [0..0] of Word;
  PArrayOfWord = ^TArrayOfWord;
  TArrayOfWord = array of Word;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array [0..0] of Integer;
  PArrayOfInteger = ^TArrayOfInteger;
  TArrayOfInteger = array of Integer;
  PArrayOfArrayOfInteger = ^TArrayOfArrayOfInteger;
  TArrayOfArrayOfInteger = array of TArrayOfInteger;

  PSingleArray = ^TSingleArray;
  TSingleArray = array [0..0] of Single;
  PArrayOfSingle = ^TArrayOfSingle;
  TArrayOfSingle = array of Single;

  PFloatArray = ^TFloatArray;
  TFloatArray = array [0..0] of TFloat;
  PArrayOfFloat = ^TArrayOfFloat;
  TArrayOfFloat = array of TFloat;

const
  // Fixed point math constants
  FixedOne = $10000;
  FixedPI  = Round(PI * FixedOne);
  FixedToFloat = 1/FixedOne;

function Fixed(S: Single): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Fixed(I: Integer): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Points }

type
{$IFNDEF FPC}
{$IFNDEF BCB}
  PPoint = ^TPoint;
  TPoint = Windows.TPoint;
{$ENDIF}
{$ENDIF}

  PPointArray = ^TPointArray;
  TPointArray = array [0..0] of TPoint;
  PArrayOfPoint = ^TArrayOfPoint;
  TArrayOfPoint = array of TPoint;
  PArrayOfArrayOfPoint = ^TArrayOfArrayOfPoint;
  TArrayOfArrayOfPoint = array of TArrayOfPoint;

  PFloatPoint = ^TFloatPoint;
  TFloatPoint = record
    X, Y: TFloat;
  end;

  PFloatPointArray = ^TFloatPointArray;
  TFloatPointArray = array [0..0] of TFloatPoint;
  PArrayOfFloatPoint = ^TArrayOfFloatPoint;
  TArrayOfFloatPoint = array of TFloatPoint;
  PArrayOfArrayOfFloatPoint = ^TArrayOfArrayOfFloatPoint;
  TArrayOfArrayOfFloatPoint = array of TArrayOfFloatPoint;

  PFixedPoint = ^TFixedPoint;
  TFixedPoint = record
    X, Y: TFixed;
  end;

  PFixedPointArray = ^TFixedPointArray;
  TFixedPointArray = array [0..0] of TFixedPoint;
  PArrayOfFixedPoint = ^TArrayOfFixedPoint;
  TArrayOfFixedPoint = array of TFixedPoint;
  PArrayOfArrayOfFixedPoint = ^TArrayOfArrayOfFixedPoint;
  TArrayOfArrayOfFixedPoint = array of TArrayOfFixedPoint;

// construction and conversion of point types
function Point(X, Y: Integer): TPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Point(const FP: TFloatPoint): TPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Point(const FXP: TFixedPoint): TPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatPoint(X, Y: Single): TFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatPoint(const P: TPoint): TFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatPoint(const FXP: TFixedPoint): TFloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(X, Y: Integer): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(X, Y: Single): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(const P: TPoint): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedPoint(const FP: TFloatPoint): TFixedPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Rectangles }

type
{$IFNDEF FPC}
  PRect = Windows.PRect;
  TRect = Windows.TRect;
{$ENDIF}

  PFloatRect = ^TFloatRect;
  TFloatRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: TFloat);
      1: (TopLeft, BottomRight: TFloatPoint);
  end;

  PFixedRect = ^TFixedRect;
  TFixedRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: TFixed);
      1: (TopLeft, BottomRight: TFixedPoint);
  end;

  TRectRounding = (rrClosest, rrOutside, rrInside);

// Rectangle construction/conversion functions
function MakeRect(const L, T, R, B: Integer): TRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function MakeRect(const FR: TFloatRect; Rounding: TRectRounding = rrClosest): TRect; overload;
function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding = rrClosest): TRect; overload;
function FixedRect(const L, T, R, B: TFixed): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedRect(const ARect: TRect): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedRect(const FR: TFloatRect): TFixedRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const L, T, R, B: TFloat): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const ARect: TRect): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRect(const FXR: TFixedRect): TFloatRect; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

// Some basic operations over rectangles
function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean; overload;
function IntersectRect(out Dst: TFloatRect; const FR1, FR2: TFloatRect): Boolean; overload;
function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean; overload;
function UnionRect(out Rect: TFloatRect; const R1, R2: TFloatRect): Boolean; overload;
function EqualRect(const R1, R2: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function EqualRect(const R1, R2: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure InflateRect(var R: TRect; Dx, Dy: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure InflateRect(var FR: TFloatRect; Dx, Dy: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure OffsetRect(var R: TRect; Dx, Dy: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure OffsetRect(var FR: TFloatRect; Dx, Dy: TFloat); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function IsRectEmpty(const R: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function IsRectEmpty(const FR: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TRect; const P: TPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TFloatRect; const P: TPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TRect; const P: TFloatPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function PtInRect(const R: TFloatRect; const P: TFloatPoint): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function EqualRectSize(const R1, R2: TRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function EqualRectSize(const R1, R2: TFloatRect): Boolean; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

type
{ TBitmap32 draw mode }
  TDrawMode = (dmOpaque, dmBlend, dmCustom, dmTransparent);
  TCombineMode = (cmBlend, cmMerge);

  TWrapMode = (wmClamp, wmRepeat, wmMirror);
  TWrapProc = function(Value, Max: Integer): Integer;
  TWrapProcEx = function(Value, Min, Max: Integer): Integer;

{$IFDEF DEPRECATEDMODE}
{ Stretch filters }
  TStretchFilter = (sfNearest, sfDraft, sfLinear, sfCosine, sfSpline,
    sfLanczos, sfMitchell);
{$ENDIF}

{ Gamma bias for line/pixel antialiasing }

var
  GAMMA_TABLE: array [Byte] of Byte;

procedure SetGamma(Gamma: Single = 0.7);

type
  { TPlainInterfacedPersistent }
  { TPlainInterfacedPersistent provides simple interface support with
    optional reference-counting operation. }
  TPlainInterfacedPersistent = class(TPersistent, IInterface)
  private
    FRefCounted: Boolean;
    FRefCount: Integer;
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    property RefCounted: Boolean read FRefCounted write FRefCounted;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property RefCount: Integer read FRefCount;
  end;

  { TNotifiablePersistent }
  { TNotifiablePersistent provides a change notification mechanism }
  TNotifiablePersistent = class(TPlainInterfacedPersistent)
  private
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
  protected
    property UpdateCount: Integer read FUpdateCount;
  public
    procedure Changed; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TThreadPersistent }
  { TThreadPersistent is an ancestor for TBitmap32 object. In addition to
    TPersistent methods, it provides thread-safe locking and change notification }
  TThreadPersistent = class(TNotifiablePersistent)
  private
    FLockCount: Integer;
  protected
    {$IFDEF FPC}
    FLock: TCriticalSection;
    {$ELSE}
    FLock: TRTLCriticalSection;
    {$ENDIF}
    property LockCount: Integer read FLockCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  { TCustomMap }
  { An ancestor for bitmaps and similar 2D distributions wich have width and
    height properties }
  TCustomMap = class(TThreadPersistent)
  protected
    FHeight: Integer;
    FWidth: Integer;
    FOnResize: TNotifyEvent;
    procedure SetHeight(NewHeight: Integer); virtual;
    procedure SetWidth(NewWidth: Integer); virtual;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); virtual;
  public
    procedure Delete; virtual;
    function  Empty: Boolean; virtual;
    procedure Resized; virtual;
    function SetSizeFrom(Source: TPersistent): Boolean;
    function SetSize(NewWidth, NewHeight: Integer): Boolean; virtual;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;

  { TBitmap32 }
  { This is the core of Graphics32 unit. The TBitmap32 class is responsible
    for storage of a bitmap, as well as for drawing in it.
    The OnCombine event is fired only when DrawMode is set to dmCustom and two
    bitmaps are blended together. Unlike most normal events, it does not contain
    "Sender" parameter and is not called through some virtual method. This
    (a little bit non-standard) approach allows for faster operation. }

const
  // common cases
  AREAINFO_RECT         = $80000000;
  AREAINFO_LINE         = $40000000; // 24 bits for line width in pixels...
  AREAINFO_ELLIPSE      = $20000000;
  AREAINFO_ABSOLUTE     = $10000000;

  AREAINFO_MASK         = $FF000000;

type
  TPixelCombineEvent = procedure(F: TColor32; var B: TColor32; M: TColor32) of object;
  TAreaChangedEvent = procedure(Sender: TObject; const Area: TRect;
    const Info: Cardinal) of object;

  TCustomResampler = class;

  TCustomBackend = class;
  TCustomBackendClass = class of TCustomBackend;

  TCustomBitmap32 = class(TCustomMap)
  private
    FBackend: TCustomBackend;
    FBits: PColor32Array;
    FClipRect: TRect;
    FFixedClipRect: TFixedRect;
    F256ClipRect: TRect;
    FClipping: Boolean;
    FDrawMode: TDrawMode;
    FCombineMode: TCombineMode;
    FWrapMode: TWrapMode;

    FMasterAlpha: Cardinal;
    FOuterColor: TColor32;
    FPenColor: TColor32;
    FStippleCounter: Single;
    FStipplePattern: TArrayOfColor32;
    FStippleStep: Single;
{$IFDEF DEPRECATEDMODE}
    FStretchFilter: TStretchFilter;
{$ENDIF}
    FOnPixelCombine: TPixelCombineEvent;
    FOnAreaChanged: TAreaChangedEvent;
    FOldOnAreaChanged: TAreaChangedEvent;
    FMeasuringMode: Boolean;
    FResampler: TCustomResampler;
    procedure BackendChangedHandler(Sender: TObject); virtual;
    procedure BackendChangingHandler(Sender: TObject); virtual;

{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array;     {$IFDEF USEINLINING} inline; {$ENDIF}
{$ENDIF}

    function GetPixelPtr(X, Y: Integer): PColor32;
    function GetScanLine(Y: Integer): PColor32Array;

    procedure SetCombineMode(const Value: TCombineMode);
    procedure SetDrawMode(Value: TDrawMode);
    procedure SetWrapMode(Value: TWrapMode);
    procedure SetMasterAlpha(Value: Cardinal);
{$IFDEF DEPRECATEDMODE}
    procedure SetStretchFilter(Value: TStretchFilter);
{$ENDIF}
    procedure SetClipRect(const Value: TRect);
    procedure SetResampler(Resampler: TCustomResampler);
    function GetResamplerClassName: string;
    procedure SetResamplerClassName(Value: string);
  protected
    WrapProcHorz: TWrapProcEx;
    WrapProcVert: TWrapProcEx;
    BlendProc: Pointer;
    RasterX, RasterY: Integer;
    RasterXF, RasterYF: TFixed;
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
    procedure CopyMapTo(Dst: TCustomBitmap32); virtual;
    procedure CopyPropertiesTo(Dst: TCustomBitmap32); virtual;
    procedure AssignTo(Dst: TPersistent); override;
    function  Equal(B: TCustomBitmap32): Boolean;
    procedure SET_T256(X, Y: Integer; C: TColor32);
    procedure SET_TS256(X, Y: Integer; C: TColor32);
    function  GET_T256(X, Y: Integer): TColor32;
    function  GET_TS256(X, Y: Integer): TColor32;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DefineProperties(Filer: TFiler); override;

    procedure InitializeBackend; virtual;
    procedure FinalizeBackend; virtual;
    procedure SetBackend(const Backend: TCustomBackend); virtual;

    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; override;

    function  GetPixel(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelS(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelW(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

    function  GetPixelF(X, Y: Single): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelFS(X, Y: Single): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  GetPixelFW(X, Y: Single): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

    function  GetPixelX(X, Y: TFixed): TColor32;
    function  GetPixelXS(X, Y: TFixed): TColor32;
    function  GetPixelXW(X, Y: TFixed): TColor32;

    function GetPixelFR(X, Y: Single): TColor32;
    function GetPixelXR(X, Y: TFixed): TColor32;

    function  GetPixelB(X, Y: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure SetPixel(X, Y: Integer; Value: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure SetPixelS(X, Y: Integer; Value: TColor32);
    procedure SetPixelW(X, Y: Integer; Value: TColor32); {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure SetPixelF(X, Y: Single; Value: TColor32);  {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure SetPixelFS(X, Y: Single; Value: TColor32);
    procedure SetPixelFW(X, Y: Single; Value: TColor32);

    procedure SetPixelX(X, Y: TFixed; Value: TColor32);
    procedure SetPixelXS(X, Y: TFixed; Value: TColor32);
    procedure SetPixelXW(X, Y: TFixed; Value: TColor32);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function  BoundsRect: TRect;
    function  Empty: Boolean; override;
    procedure Clear; overload;
    procedure Clear(FillColor: TColor32); overload;
    procedure Delete; override;

    procedure BeginMeasuring(const Callback: TAreaChangedEvent);
    procedure EndMeasuring;

    function ReleaseBackend: TCustomBackend;

    procedure PropertyChanged; virtual;
    procedure Changed; overload; override;
    procedure Changed(const Area: TRect; const Info: Cardinal = AREAINFO_RECT); reintroduce; overload; virtual;

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream; SaveTopDown: Boolean = False); virtual;

    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string; SaveTopDown: Boolean = False); virtual;

    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);

    procedure ResetAlpha; overload;
    procedure ResetAlpha(const AlphaValue: Byte); overload;

    procedure Draw(DstX, DstY: Integer; Src: TCustomBitmap32); overload;
    procedure Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TCustomBitmap32); overload;
    procedure Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32); overload;

    procedure SetPixelT(X, Y: Integer; Value: TColor32); overload;
    procedure SetPixelT(var Ptr: PColor32; Value: TColor32); overload;
    procedure SetPixelTS(X, Y: Integer; Value: TColor32);

    procedure DrawTo(Dst: TCustomBitmap32); overload;
    procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer); overload;
    procedure DrawTo(Dst: TCustomBitmap32; const DstRect: TRect); overload;
    procedure DrawTo(Dst: TCustomBitmap32; const DstRect, SrcRect: TRect); overload;

    procedure SetStipple(NewStipple: TArrayOfColor32); overload;
    procedure SetStipple(NewStipple: array of TColor32); overload;
    procedure AdvanceStippleCounter(LengthPixels: Single);
    function  GetStippleColor: TColor32;

    procedure HorzLine(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineS(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineT(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
    procedure HorzLineTSP(X1, Y, X2: Integer);
    procedure HorzLineX(X1, Y, X2: TFixed; Value: TColor32);
    procedure HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);

    procedure VertLine(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineS(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineT(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
    procedure VertLineTSP(X, Y1, Y2: Integer);
    procedure VertLineX(X, Y1, Y2: TFixed; Value: TColor32);
    procedure VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);

    procedure Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean = False);
    procedure LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
    procedure LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
    procedure LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean = False); overload;
    procedure LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean = False); overload;
    procedure LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
    procedure LineFP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;
    procedure LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean = False); overload;
    procedure LineFSP(X1, Y1, X2, Y2: Single; L: Boolean = False); overload;

    property  PenColor: TColor32 read FPenColor write FPenColor;
    procedure MoveTo(X, Y: Integer);
    procedure LineToS(X, Y: Integer);
    procedure LineToTS(X, Y: Integer);
    procedure LineToAS(X, Y: Integer);
    procedure MoveToX(X, Y: TFixed);
    procedure MoveToF(X, Y: Single);
    procedure LineToXS(X, Y: TFixed);
    procedure LineToFS(X, Y: Single);
    procedure LineToXSP(X, Y: TFixed);
    procedure LineToFSP(X, Y: Single);

    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
    procedure FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FillRectS(const ARect: TRect; Value: TColor32); overload;
    procedure FillRectTS(const ARect: TRect; Value: TColor32); overload;

    procedure FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
    procedure FrameRectTSP(X1, Y1, X2, Y2: Integer);
    procedure FrameRectS(const ARect: TRect; Value: TColor32); overload;
    procedure FrameRectTS(const ARect: TRect; Value: TColor32); overload;

    procedure RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer); overload;
    procedure RaiseRectTS(const ARect: TRect; Contrast: Integer); overload;

    procedure Roll(Dx, Dy: Integer; FillBack: Boolean; FillColor: TColor32);
    procedure FlipHorz(Dst: TCustomBitmap32 = nil);
    procedure FlipVert(Dst: TCustomBitmap32 = nil);
    procedure Rotate90(Dst: TCustomBitmap32 = nil);
    procedure Rotate180(Dst: TCustomBitmap32 = nil);
    procedure Rotate270(Dst: TCustomBitmap32 = nil);

    procedure ResetClipRect;

    property  Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;
    property  PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;
    property  PixelW[X, Y: Integer]: TColor32 read GetPixelW write SetPixelW;
    property  PixelX[X, Y: TFixed]: TColor32 read GetPixelX write SetPixelX;
    property  PixelXS[X, Y: TFixed]: TColor32 read GetPixelXS write SetPixelXS;
    property  PixelXW[X, Y: TFixed]: TColor32 read GetPixelXW write SetPixelXW;
    property  PixelF[X, Y: Single]: TColor32 read GetPixelF write SetPixelF;
    property  PixelFS[X, Y: Single]: TColor32 read GetPixelFS write SetPixelFS;
    property  PixelFW[X, Y: Single]: TColor32 read GetPixelFW write SetPixelFW;
    property  PixelFR[X, Y: Single]: TColor32 read GetPixelFR;
    property  PixelXR[X, Y: TFixed]: TColor32 read GetPixelXR;

    property Backend: TCustomBackend read FBackend write SetBackend;

{$IFDEF BITS_GETTER}
    property Bits: PColor32Array read GetBits;
{$ELSE}
    property Bits: PColor32Array read FBits;
{$ENDIF}

    property ClipRect: TRect read FClipRect write SetClipRect;
    property Clipping: Boolean read FClipping;

    property PixelPtr[X, Y: Integer]: PColor32 read GetPixelPtr;
    property ScanLine[Y: Integer]: PColor32Array read GetScanLine;
    property StippleCounter: Single read FStippleCounter write FStippleCounter;
    property StippleStep: Single read FStippleStep write FStippleStep;

    property MeasuringMode: Boolean read FMeasuringMode;
  published
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmOpaque;
    property CombineMode: TCombineMode read FCombineMode write SetCombineMode default cmBlend;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode default wmClamp;
    property MasterAlpha: Cardinal read FMasterAlpha write SetMasterAlpha default $FF;
    property OuterColor: TColor32 read FOuterColor write FOuterColor default 0;
{$IFDEF DEPRECATEDMODE}
    property StretchFilter: TStretchFilter read FStretchFilter write SetStretchFilter default sfNearest;
{$ENDIF}
    property ResamplerClassName: string read GetResamplerClassName write SetResamplerClassName;
    property Resampler: TCustomResampler read FResampler write SetResampler;
    property OnChange;
    property OnPixelCombine: TPixelCombineEvent read FOnPixelCombine write FOnPixelCombine;
    property OnAreaChanged: TAreaChangedEvent read FOnAreaChanged write FOnAreaChanged;
    property OnResize;
  end;

  TBitmap32 = class(TCustomBitmap32)
  private
    FOnHandleChanged: TNotifyEvent;
      
    procedure BackendChangedHandler(Sender: TObject); override;
    procedure BackendChangingHandler(Sender: TObject); override;
    
    procedure FontChanged(Sender: TObject);
    procedure CanvasChanged(Sender: TObject);
    function GetCanvas: TCanvas;         {$IFDEF USEINLINING} inline; {$ENDIF}

    function GetBitmapInfo: TBitmapInfo; {$IFDEF USEINLINING} inline; {$ENDIF}
    function GetHandle: HBITMAP;         {$IFDEF USEINLINING} inline; {$ENDIF}
    function GetHDC: HDC;                {$IFDEF USEINLINING} inline; {$ENDIF}

    function GetFont: TFont;
    procedure SetFont(Value: TFont);
  protected
    procedure InitializeBackend; override;
    procedure FinalizeBackend; override;
    procedure SetBackend(const Backend: TCustomBackend); override;
    
    procedure HandleChanged; virtual;
    procedure CopyPropertiesTo(Dst: TCustomBitmap32); override;
  public
  {$IFDEF BCB}
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal); overload;
  {$ELSE}
    procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;
  {$ENDIF}

{$IFDEF BCB}
    procedure DrawTo(hDst: Cardinal; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: Cardinal; const DstRect, SrcRect: TRect); overload;
    procedure TileTo(hDst: Cardinal; const DstRect, SrcRect: TRect);
{$ELSE}
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;
    procedure TileTo(hDst: HDC; const DstRect, SrcRect: TRect);
{$ENDIF}

    procedure UpdateFont;
    procedure Textout(X, Y: Integer; const Text: String); overload;
    procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: String); overload;
    procedure Textout(DstRect: TRect; const Flags: Cardinal; const Text: String); overload;
    function  TextExtent(const Text: String): TSize;
    function  TextHeight(const Text: String): Integer;
    function  TextWidth(const Text: String): Integer;
    procedure RenderText(X, Y: Integer; const Text: String; AALevel: Integer; Color: TColor32);
    procedure TextoutW(X, Y: Integer; const Text: Widestring); overload;
    procedure TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring); overload;
    procedure TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring); overload;
    function  TextExtentW(const Text: Widestring): TSize;
    function  TextHeightW(const Text: Widestring): Integer;
    function  TextWidthW(const Text: Widestring): Integer;
    procedure RenderTextW(X, Y: Integer; const Text: Widestring; AALevel: Integer; Color: TColor32);

    property  Canvas: TCanvas read GetCanvas;
    function  CanvasAllocated: Boolean;
    procedure DeleteCanvas;

    property Font: TFont read GetFont write SetFont;

    property BitmapHandle: HBITMAP read GetHandle;
    property BitmapInfo: TBitmapInfo read GetBitmapInfo;
    property Handle: HDC read GetHDC;
  published
    property OnHandleChanged: TNotifyEvent read FOnHandleChanged write FOnHandleChanged;
  end;

  { TCustomBackend }
  { This class functions as backend for the TBitmap32 class.
    It manages and provides the backing buffer as well as OS or
    graphics subsystem specific features.}

  TCustomBackend = class(TThreadPersistent)
  protected
    FBits: PColor32Array;
    FOwner: TCustomBitmap32;
    FOnChanging: TNotifyEvent;

    procedure Changing; virtual;

{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; virtual;
{$ENDIF}

    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); virtual;
    procedure FinalizeSurface; virtual;
  public
    constructor Create; overload; override;
    constructor Create(Owner: TCustomBitmap32); reintroduce; overload; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear; virtual;
    function Empty: Boolean; virtual;

    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean = True); virtual;

{$IFDEF BITS_GETTER}
    property Bits: PColor32Array read GetBits;
{$ELSE}
    property Bits: PColor32Array read FBits;
{$ENDIF}

    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  { TCustomSampler }
  TCustomSampler = class(TNotifiablePersistent)
  public
    function GetSampleInt(X, Y: Integer): TColor32; virtual;
    function GetSampleFixed(X, Y: TFixed): TColor32; virtual;
    function GetSampleFloat(X, Y: TFloat): TColor32; virtual;
    procedure PrepareSampling; virtual;
    procedure FinalizeSampling; virtual;
    function HasBounds: Boolean; virtual;
    function GetSampleBounds: TFloatRect; virtual;
  end;


  TPixelAccessMode = (pamUnsafe, pamSafe, pamWrap, pamTransparentEdge);

  { TCustomResampler }
  { Base class for TCustomBitmap32 specific resamplers. }
  TCustomResampler = class(TCustomSampler)
  private
    FBitmap: TCustomBitmap32;
    FClipRect: TRect;
    FPixelAccessMode: TPixelAccessMode;
    procedure SetPixelAccessMode(const Value: TPixelAccessMode);
  protected
    function GetWidth: TFloat; virtual;
    procedure Resample(
      Dst: TCustomBitmap32; DstRect: TRect; DstClip: TRect;
      Src: TCustomBitmap32; SrcRect: TRect;
      CombineOp: TDrawMode; CombineCallBack: TPixelCombineEvent); virtual; abstract;
    procedure AssignTo(Dst: TPersistent); override;
    property ClipRect: TRect read FClipRect;
  public
    constructor Create; overload; virtual;
    constructor Create(ABitmap: TCustomBitmap32); overload; virtual;
    procedure Changed; override;
    procedure PrepareSampling; override;
    function HasBounds: Boolean; override;
    function GetSampleBounds: TFloatRect; override;
    property Bitmap: TCustomBitmap32 read FBitmap write FBitmap;
    property Width: TFloat read GetWidth;
  published
    property PixelAccessMode: TPixelAccessMode read FPixelAccessMode write SetPixelAccessMode default pamSafe;
  end;
  TCustomResamplerClass = class of TCustomResampler;

function GetPlatformBackendClass: TCustomBackendClass;

var
  StockBitmap: TBitmap;

implementation

uses
  GR32_Blend, GR32_Filters, GR32_LowLevel, Math, GR32_Math,
  GR32_Resamplers, GR32_Containers, GR32_Backends, GR32_Backends_Generic,
{$IFDEF FPC}
  Clipbrd,
  {$IFDEF Windows}
    GR32_Backends_LCL_Win,
  {$ENDIF}
  {$IF defined(LCLGtk) or defined(LCLGtk2)}
    GR32_Backends_LCL_Gtk,
  {$IFEND}
  {$IFDEF LCLCarbon}
    GR32_Backends_LCL_Carbon,
  {$ENDIF}
{$ELSE}
  Clipbrd, GR32_Backends_VCL,
{$ENDIF}
  GR32_DrawingEx;

type
  { We can not use the Win32 defined record here since we are cross-platform. }
  TBmpHeader = packed record
    bfType: Word;
    bfSize: LongInt;
    bfReserved: LongInt;
    bfOffBits: LongInt;
    biSize: LongInt;
    biWidth: LongInt;
    biHeight: LongInt;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: LongInt;
    biSizeImage: LongInt;
    biXPelsPerMeter: LongInt;
    biYPelsPerMeter: LongInt;
    biClrUsed: LongInt;
    biClrImportant: LongInt;
  end;

  TGraphicAccess = class(TGraphic);

const
  ZERO_RECT: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);


{ Color construction and conversion functions }

function Color32(WinColor: TColor): TColor32; overload;
{$IFDEF WIN_COLOR_FIX}
var
  I: Longword;
{$ENDIF}
begin
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);

{$IFDEF WIN_COLOR_FIX}
  Result := $FF000000;
  I := (WinColor and $00FF0000) shr 16;
  if I <> 0 then Result := Result or TColor32(Integer(I) - 1);
  I := WinColor and $0000FF00;
  if I <> 0 then Result := Result or TColor32(Integer(I) - $00000100);
  I := WinColor and $000000FF;
  if I <> 0 then Result := Result or TColor32(Integer(I) - 1) shl 16;
{$ELSE}
  asm
        MOV    EAX,WinColor
        BSWAP  EAX
        MOV    AL,$FF
        ROR    EAX,8
        MOV    Result,EAX
  end;
{$ENDIF}
end;

function Color32(R, G, B: Byte; A: Byte = $FF): TColor32; overload;
{$IFNDEF TARGET_x86}
begin
  Result := (A shl 24) or
            (R shl 16) or
            (G shl  8) or B;
{$ELSE}
asm
        MOV  AH,A
        SHL  EAX,16
        MOV  AH,DL
        MOV  AL,CL
{$ENDIF}
end;

function Color32(Index: Byte; var Palette: TPalette32): TColor32; overload;
begin
  Result := Palette[Index];
end;

function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;
begin
  Result := TColor32(Alpha) shl 24 + TColor32(Intensity) shl 16 +
    TColor32(Intensity) shl 8 + TColor32(Intensity);
end;

function WinColor(Color32: TColor32): TColor;
{$IFNDEF TARGET_x86}
begin
  Result := ((Color32 and $00FF0000) shl 16) or
             (Color32 and $0000FF00) or
            ((Color32 and $000000FF) shr 16);
{$ELSE}
asm
  // the alpha channel byte is set to zero!
        ROL    EAX,8  // ABGR  ->  BGRA
        XOR    AL,AL  // BGRA  ->  BGR0
        BSWAP  EAX    // BGR0  ->  0RGB
{$ENDIF}
end;

function ArrayOfColor32(Colors: array of TColor32): TArrayOfColor32;
var
  L: Integer;
begin
  // build a dynamic color array from specified colors
  L := High(Colors) + 1;
  SetLength(Result, L);
  MoveLongword(Colors[0], Result[0], L);
end;

procedure Color32ToRGB(Color32: TColor32; var R, G, B: Byte);
begin
  R := (Color32 and $00FF0000) shr 16;
  G := (Color32 and $0000FF00) shr 8;
  B := Color32 and $000000FF;
end;

procedure Color32ToRGBA(Color32: TColor32; var R, G, B, A: Byte);
begin
  A := Color32 shr 24;
  R := (Color32 and $00FF0000) shr 16;
  G := (Color32 and $0000FF00) shr 8;
  B := Color32 and $000000FF;
end; 

function Color32Components(R, G, B, A: Boolean): TColor32Components;
const
  ccR : array[Boolean] of TColor32Components = ([], [ccRed]);
  ccG : array[Boolean] of TColor32Components = ([], [ccGreen]);
  ccB : array[Boolean] of TColor32Components = ([], [ccBlue]);
  ccA : array[Boolean] of TColor32Components = ([], [ccAlpha]);
begin
  Result := ccR[R] + ccG[G] + ccB[B] + ccA[A];
end;

function RedComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $00FF0000) shr 16;
end;

function GreenComponent(Color32: TColor32): Integer;
begin
  Result := (Color32 and $0000FF00) shr 8;
end;

function BlueComponent(Color32: TColor32): Integer;
begin
  Result := Color32 and $000000FF;
end;

function AlphaComponent(Color32: TColor32): Integer;
begin
  Result := Color32 shr 24;
end;

function Intensity(Color32: TColor32): Integer;
begin
// (R * 61 + G * 174 + B * 21) / 256
  Result := (
    (Color32 and $00FF0000) shr 16 * 61 +
    (Color32 and $0000FF00) shr 8 * 174 +
    (Color32 and $000000FF) * 21
    ) shr 8;
end;

function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;
begin
  if NewAlpha < 0 then NewAlpha := 0
  else if NewAlpha > 255 then NewAlpha := 255;
  Result := (Color32 and $00FFFFFF) or (TColor32(NewAlpha) shl 24);
end;

{ Color space conversions }

function HSLtoRGB(H, S, L: Single): TColor32;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 * OneOverThree - Hue) * 6
    else V := M1;
    Result := Round(255 * V);
  end;

begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + OneOverThree);
    G := HueToColor(H);
    B := HueToColor(H - OneOverThree)
  end;
  Result := Color32(R, G, B, 255);
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L : Single);
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  R := RedComponent(RGB) / 255;
  G := GreenComponent(RGB) / 255;
  B := BlueComponent(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    if R = Cmax then H := (G - B) / D
    else
      if G = Cmax then H  := 2 + (B - R) / D
      else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1
  end;
end;

function HSLtoRGB(H, S, L: Integer): TColor32;
var
  V, M, M1, M2, VSF: Integer;
begin
  if L <= $7F then
    V := L * (256 + S) shr 8
  else
    V := L + S - L * S div 255;
  if V <= 0 then
    Result := Color32(0, 0, 0, 0)
  else
  begin
    M := L * 2 - V;
    H := H * 6;
    VSF := (V - M) * (H and $FF) shr 8;
    M1 := M + VSF;
    M2 := V - VSF;
    case H shr 8 of
      0: Result := Color32(V, M1, M, 0);
      1: Result := Color32(M2, V, M, 0);
      2: Result := Color32(M, V, M1, 0);
      3: Result := Color32(M, M2, V, 0);
      4: Result := Color32(M1, M, V, 0);
      5: Result := Color32(V, M, M2, 0);
    else
      Result := 0;
    end;
  end;
end;

function Max(const A, B, C: Integer): Integer; overload;
{$IFNDEF TARGET_x86}
begin
  if A > B then 
  	Result := A
  else 
  	Result := B;
  	
  if C > Result then 
  	Result := C;   
{$ELSE}
asm
      CMP       EDX,EAX
      db $0F,$4F,$C2           /// CMOVG     EAX,EDX
      CMP       ECX,EAX
      db $0F,$4F,$C1           /// CMOVG     EAX,ECX
{$ENDIF}
end;

function Min(const A, B, C: Integer): Integer; overload;
{$IFNDEF TARGET_x86}
begin
  if A < B then 
  	Result := A
  else 
  	Result := B;
  
  if C < Result then 
  	Result := C;
{$ELSE}
asm
      CMP       EDX,EAX
      db $0F,$4C,$C2           /// CMOVL     EAX,EDX
      CMP       ECX,EAX
      db $0F,$4C,$C1           /// CMOVL     EAX,ECX
{$ENDIF}
end;

procedure RGBtoHSL(RGB: TColor32; out H, S, L: Byte);
var
  R, G, B, D, Cmax, Cmin, HL: Integer;
begin
  R := (RGB shr 16) and $ff;
  G := (RGB shr 8) and $ff;
  B := RGB and $ff;

  Cmax := Max(R, G, B);
  Cmin := Min(R, G, B);
  L := (Cmax + Cmin) div 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := (Cmax - Cmin) * 255;
    if L <= $7F then
      S := D div (Cmax + Cmin)
    else
      S := D div (255 * 2 - Cmax - Cmin);

    D := D * 6;
    if R = Cmax then
      HL := (G - B) * 255 * 255 div D
    else if G = Cmax then
      HL := 255 * 2 div 6 + (B - R) * 255 * 255 div D
    else
      HL := 255 * 4 div 6 + (R - G) * 255 * 255 div D;

    if HL < 0 then HL := HL + 255 * 2;
    H := HL;
  end;
end;

{ Palette conversion }

function WinPalette(const P: TPalette32): HPALETTE;
var
  L: TMaxLogPalette;
  L0: LOGPALETTE absolute L;
  I: Cardinal;
  Cl: TColor32;
begin
  L.palVersion := $300;
  L.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    Cl := P[I];
    with L.palPalEntry[I] do
    begin
      peFlags := 0;
      peRed := RedComponent(Cl);
      peGreen := GreenComponent(Cl);
      peBlue := BlueComponent(Cl);
    end;
  end;
  Result := CreatePalette(l0);
end;


{ Fixed-point conversion routines }

function Fixed(S: Single): TFixed;
begin
  Result := Round(S * 65536);
end;

function Fixed(I: Integer): TFixed;
begin
  Result := I shl 16;
end;


{ Points }

function Point(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Point(const FP: TFloatPoint): TPoint;
begin
  Result.X := Round(FP.X);
  Result.Y := Round(FP.Y);
end;

function Point(const FXP: TFixedPoint): TPoint;
begin
  Result.X := FixedRound(FXP.X);
  Result.Y := FixedRound(FXP.Y);
end;

function FloatPoint(X, Y: Single): TFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function FloatPoint(const P: TPoint): TFloatPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function FloatPoint(const FXP: TFixedPoint): TFloatPoint;
const
  F = 1 / 65536;
begin
  with FXP do
  begin
    Result.X := X * F;
    Result.Y := Y * F;
  end;
end;

function FixedPoint(X, Y: Integer): TFixedPoint; overload;
begin
  Result.X := X shl 16;
  Result.Y := Y shl 16;
end;

function FixedPoint(X, Y: Single): TFixedPoint; overload;
begin
  Result.X := Round(X * 65536);
  Result.Y := Round(Y * 65536);
end;

function FixedPoint(const P: TPoint): TFixedPoint; overload;
begin
  Result.X := P.X shl 16;
  Result.Y := P.Y shl 16;
end;

function FixedPoint(const FP: TFloatPoint): TFixedPoint; overload;
begin
  Result.X := Round(FP.X * 65536);
  Result.Y := Round(FP.Y * 65536);
end;


{ Rectangles }

function MakeRect(const L, T, R, B: Integer): TRect;
begin
  with Result do
  begin
    Left := L;
    Top := T;
    Right := R;
    Bottom := B;
  end;
end;

function MakeRect(const FR: TFloatRect; Rounding: TRectRounding): TRect;
begin
  with FR do
    case Rounding of
      rrClosest:
        begin
          Result.Left := Round(Left);
          Result.Top := Round(Top);
          Result.Right := Round(Right);
          Result.Bottom := Round(Bottom);
        end;

      rrInside:
        begin
          Result.Left := Ceil(Left);
          Result.Top := Ceil(Top);
          Result.Right := Floor(Right);
          Result.Bottom := Floor(Bottom);
          if Result.Right < Result.Left then Result.Right := Result.Left;
          if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
        end;

      rrOutside:
        begin
          Result.Left := Floor(Left);
          Result.Top := Floor(Top);
          Result.Right := Ceil(Right);
          Result.Bottom := Ceil(Bottom);
        end;
    end;
end;

function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding): TRect;
begin
  with FXR do
    case Rounding of
      rrClosest:
        begin
          Result.Left := FixedRound(Left);
          Result.Top := FixedRound(Top);
          Result.Right := FixedRound(Right);
          Result.Bottom := FixedRound(Bottom);
        end;

      rrInside:
        begin
          Result.Left := FixedCeil(Left);
          Result.Top := FixedCeil(Top);
          Result.Right := FixedFloor(Right);
          Result.Bottom := FixedFloor(Bottom);
          if Result.Right < Result.Left then Result.Right := Result.Left;
          if Result.Bottom < Result.Top then Result.Bottom := Result.Top;
        end;

      rrOutside:
        begin
          Result.Left := FixedFloor(Left);
          Result.Top := FixedFloor(Top);
          Result.Right := FixedCeil(Right);
          Result.Bottom := FixedCeil(Bottom);
        end;
    end;
end;

function FixedRect(const L, T, R, B: TFixed): TFixedRect;
begin
  with Result do
  begin
    Left := L;
    Top := T;
    Right := R;
    Bottom := B;
  end;
end;

function FixedRect(const ARect: TRect): TFixedRect;
begin
  with Result do
  begin
    Left := ARect.Left shl 16;
    Top := ARect.Top shl 16;
    Right := ARect.Right shl 16;
    Bottom := ARect.Bottom shl 16;
  end;
end;

function FixedRect(const FR: TFloatRect): TFixedRect;
begin
  with Result do
  begin
    Left := Round(FR.Left * 65536);
    Top := Round(FR.Top * 65536);
    Right := Round(FR.Right * 65536);
    Bottom := Round(FR.Bottom * 65536);
  end;
end;

function FloatRect(const L, T, R, B: TFloat): TFloatRect;
begin
  with Result do
  begin
    Left := L;
    Top := T;
    Right := R;
    Bottom := B;
  end;
end;

function FloatRect(const ARect: TRect): TFloatRect;
begin
  with Result do
  begin
    Left := ARect.Left;
    Top := ARect.Top;
    Right := ARect.Right;
    Bottom := ARect.Bottom;
  end;
end;

function FloatRect(const FXR: TFixedRect): TFloatRect;
begin
  with Result do
  begin
    Left := FXR.Left * FixedToFloat;
    Top := FXR.Top * FixedToFloat;
    Right := FXR.Right * FixedToFloat;
    Bottom := FXR.Bottom * FixedToFloat;
  end;
end;

function IntersectRect(out Dst: TRect; const R1, R2: TRect): Boolean;
begin
  if R1.Left >= R2.Left then Dst.Left := R1.Left else Dst.Left := R2.Left;
  if R1.Right <= R2.Right then Dst.Right := R1.Right else Dst.Right := R2.Right;
  if R1.Top >= R2.Top then Dst.Top := R1.Top else Dst.Top := R2.Top;
  if R1.Bottom <= R2.Bottom then Dst.Bottom := R1.Bottom else Dst.Bottom := R2.Bottom;
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then Dst := ZERO_RECT;
end;

function IntersectRect(out Dst: TFloatRect; const FR1, FR2: TFloatRect): Boolean;
begin
  Dst.Left   := Math.Max(FR1.Left,   FR2.Left);
  Dst.Right  := Math.Min(FR1.Right,  FR2.Right);
  Dst.Top    := Math.Max(FR1.Top,    FR2.Top);
  Dst.Bottom := Math.Min(FR1.Bottom, FR2.Bottom);
  Result := (Dst.Right >= Dst.Left) and (Dst.Bottom >= Dst.Top);
  if not Result then FillLongword(Dst, 4, 0);
end;

function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean;
begin
  Rect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then Rect.Left := R2.Left;
    if R2.Top < R1.Top then Rect.Top := R2.Top;
    if R2.Right > R1.Right then Rect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then Rect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(Rect);
  if not Result then Rect := ZERO_RECT;
end;

function UnionRect(out Rect: TFloatRect; const R1, R2: TFloatRect): Boolean;
begin
  Rect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then Rect.Left := R2.Left;
    if R2.Top < R1.Top then Rect.Top := R2.Top;
    if R2.Right > R1.Right then Rect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then Rect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(Rect);
  if not Result then FillLongword(Rect, 4, 0);
end;

function EqualRect(const R1, R2: TRect): Boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TRect));
end;

function EqualRect(const R1, R2: TFloatRect): Boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TFloatRect));
end;

function EqualRectSize(const R1, R2: TRect): Boolean;
begin
  Result := ((R1.Right - R1.Left) = (R2.Right - R2.Left)) and
    ((R1.Bottom - R1.Top) = (R2.Bottom - R2.Top));
end;

function EqualRectSize(const R1, R2: TFloatRect): Boolean;
var
  _R1: TFixedRect;
  _R2: TFixedRect;
begin
  _R1 := FixedRect(R1);
  _R2 := FixedRect(R2);
  Result := ((_R1.Right - _R1.Left) = (_R2.Right - _R2.Left)) and
    ((_R1.Bottom - _R1.Top) = (_R2.Bottom - _R2.Top));
end;

procedure InflateRect(var R: TRect; Dx, Dy: Integer);
begin
  Dec(R.Left, Dx); Dec(R.Top, Dy);
  Inc(R.Right, Dx); Inc(R.Bottom, Dy);
end;

procedure InflateRect(var FR: TFloatRect; Dx, Dy: TFloat);
begin
  with FR do
  begin
    Left := Left - Dx; Top := Top - Dy;
    Right := Right + Dx; Bottom := Bottom + Dy;
  end;
end;

procedure OffsetRect(var R: TRect; Dx, Dy: Integer);
begin
  Inc(R.Left, Dx); Inc(R.Top, Dy);
  Inc(R.Right, Dx); Inc(R.Bottom, Dy);
end;

procedure OffsetRect(var FR: TFloatRect; Dx, Dy: TFloat);
begin
  with FR do
  begin
    Left := Left + Dx; Top := Top + Dy;
    Right := Right + Dx; Bottom := Bottom + Dy;
  end;
end;

function IsRectEmpty(const R: TRect): Boolean;
begin
  Result := (R.Right <= R.Left) or (R.Bottom <= R.Top);
end;

function IsRectEmpty(const FR: TFloatRect): Boolean;
begin
  Result := (FR.Right <= FR.Left) or (FR.Bottom <= FR.Top);
end;

function PtInRect(const R: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

function PtInRect(const R: TFloatRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

function PtInRect(const R: TRect; const P: TFloatPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

function PtInRect(const R: TFloatRect; const P: TFloatPoint): Boolean;
begin
  Result := (P.X >= R.Left) and (P.X < R.Right) and
    (P.Y >= R.Top) and (P.Y < R.Bottom);
end;

{ Gamma / Pixel Shape Correction table }

procedure SetGamma(Gamma: Single);
var
  i: Integer;
begin
  for i := 0 to 255 do
    GAMMA_TABLE[i] := Round(255 * Power(i / 255, Gamma));
end;

function GetPlatformBackendClass: TCustomBackendClass;
begin
{$IFDEF FPC}
  Result := TLCLBackend;
{$ELSE}
  Result := TGDIBackend;
{$ENDIF}
end;

{ TSimpleInterfacedPersistent }

function TPlainInterfacedPersistent._AddRef: Integer;
begin
  if FRefCounted then
    Result := InterlockedIncrement(FRefCount)
  else
    Result := -1;
end;

function TPlainInterfacedPersistent._Release: Integer;
begin
  if FRefCounted then
  begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 then
      Destroy;
  end
  else
    Result := -1;
end;

function TPlainInterfacedPersistent.QueryInterface(
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TPlainInterfacedPersistent.AfterConstruction;
begin
  inherited;

  // Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TPlainInterfacedPersistent.BeforeDestruction;
begin
  if RefCounted and (RefCount <> 0) then
    raise Exception.Create('Unmatched reference counting.');

  inherited;
end;

class function TPlainInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;

  // Set an implicit refcount so that refcounting
  // during construction won't destroy the object.
  TPlainInterfacedPersistent(Result).FRefCount := 1;
end;


{ TNotifiablePersistent }

procedure TNotifiablePersistent.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TNotifiablePersistent.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TNotifiablePersistent.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TThreadPersistent.EndUpdate');
  Dec(FUpdateCount);
end;


{ TThreadPersistent }

constructor TThreadPersistent.Create;
begin
  InitializeCriticalSection(FLock);
end;

destructor TThreadPersistent.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TThreadPersistent.Lock;
begin
  InterlockedIncrement(FLockCount);
  EnterCriticalSection(FLock);
end;

procedure TThreadPersistent.Unlock;
begin
  LeaveCriticalSection(FLock);
  InterlockedDecrement(FLockCount);
end;


{ TCustomMap }

procedure TCustomMap.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TCustomMap.Delete;
begin
  SetSize(0, 0);
end;

function TCustomMap.Empty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;

procedure TCustomMap.Resized;
begin
  if Assigned(FOnResize) then FOnResize(Self);
end;

procedure TCustomMap.SetHeight(NewHeight: Integer);
begin
  SetSize(Width, NewHeight);
end;

function TCustomMap.SetSize(NewWidth, NewHeight: Integer): Boolean;
begin
  if NewWidth < 0 then NewWidth := 0;
  if NewHeight < 0 then NewHeight := 0;
  Result := (NewWidth <> FWidth) or (NewHeight <> FHeight);
  if Result then
  begin
    ChangeSize(FWidth, FHeight, NewWidth, NewHeight);
    Changed;
    Resized;
  end;
end;

function TCustomMap.SetSizeFrom(Source: TPersistent): Boolean;
begin
  if Source is TCustomMap then
    Result := SetSize(TCustomMap(Source).Width, TCustomMap(Source).Height)
  else if Source is TGraphic then
    Result := SetSize(TGraphic(Source).Width, TGraphic(Source).Height)
  else if Source is TControl then
    Result := SetSize(TControl(Source).Width, TControl(Source).Height)
  else if Source = nil then
    Result := SetSize(0, 0)
  else
    raise Exception.Create('Can''t set size from ''' + Source.ClassName + '''');
end;

procedure TCustomMap.SetWidth(NewWidth: Integer);
begin
  SetSize(NewWidth, Height);
end;


{ TCustomBitmap32 }

constructor TCustomBitmap32.Create;
begin
  inherited;

  InitializeBackend;

  FOuterColor := $00000000;  // by default as full transparency black

  FMasterAlpha := $FF;
  FPenColor := clWhite32;
  FStippleStep := 1;
  FCombineMode := cmBlend;
  BlendProc := @BLEND_MEM[FCombineMode]^;
  WrapProcHorz := GetWrapProcEx(WrapMode);
  WrapProcVert := GetWrapProcEx(WrapMode);
  FResampler := TNearestResampler.Create(Self);
end;

destructor TCustomBitmap32.Destroy;
begin
  BeginUpdate;
  Lock;
  try
    SetSize(0, 0);
    FResampler.Free;
    FinalizeBackend;
  finally
    Unlock;
  end;
  inherited;
end;

procedure TCustomBitmap32.InitializeBackend;
begin
  TMemoryBackend.Create(Self);
end;

procedure TCustomBitmap32.FinalizeBackend;
begin
  // Drop ownership of backend now:
  // It's a zombie now.
  FBackend.FOwner := nil;
  FBackend.OnChange := nil;
  FBackend.OnChanging := nil;

  (*
  Release our reference to the backend

  Note: The backend won't necessarily be freed immediately.

  This is required to circumvent a problem with the magic procedure cleanup
  of interfaces that have ref-counting forcefully disabled:

  Quality Central report #9157 and #9500:
  http://qc.codegear.com/wc/qcmain.aspx?d=9157
  http://qc.codegear.com/wc/qcmain.aspx?d=9500

  If any backend interface is used within the same procedure in which
  the owner bitmap is also freed, the magic procedure cleanup will
  clear that particular interface long after the bitmap and its backend
  are gone. This will result in all sorts of madness - mostly heap corruption
  and AVs.

  Here is an example:

  procedure Test;
  var
    MyBitmap: TBitmap32;
  begin
     MyBitmap := TBitmap32.Create;
     MyBitmap.SetSize(100, 100);
     (MyBitmap.Backend as ICanvasSupport).Canvas;
     MyBitmap.Free;
  end; // _IntfClear will try to clear (MyBitmap.Backend as ICanvasSupport)
       // which points to the interface at the previous location of MyBitmap.Backend in memory.
       // MyBitmap.Backend is gone and the _Release call is invalid, so raise hell .

  Here is an example for a correct workaround:

  procedure Test;
  var
    MyBitmap: TBitmap32;
    CanvasIntf: ICanvasSupport;
  begin
    MyBitmap := TBitmap32.Create;
    MyBitmap.SetSize(100, 100);
    CanvasIntf := MyBitmap.Backend as ICanvasSupport;
    CanvasIntf.Canvas;
    CanvasIntf := nil; // this will call _IntfClear and IInterface._Release
    MyBitmap.Free;
  end; // _IntfClear will try to clear CanvasIntf,
       // it's nil, no _Release is called, everything is fine.

  Since the above code is pretty fiddly, we introduce ref-counting for the
  backend. That way the backend will be released once all references are dropped.

  So, release our reference to the backend now:
  *)
  FBackend._Release;
  FBackend := nil;
end;

procedure TCustomBitmap32.SetBackend(const Backend: TCustomBackend);
begin
  if Assigned(Backend) and (Backend <> FBackend) then
  begin
    BeginUpdate;

    Backend.FOwner := Self;

    if Assigned(FBackend) then
    begin
      Backend.Assign(FBackend);
      FinalizeBackend;
    end;

    FBackend := Backend;
    FBackend.OnChange := BackendChangedHandler;
    FBackend.OnChanging := BackendChangingHandler;

    EndUpdate;
    
    FBackend.Changed;
    Changed;
  end;
end;

function TCustomBitmap32.ReleaseBackend: TCustomBackend;
begin
  FBackend._AddRef; // Increase ref-count for external use
  Result := FBackend;
end;

function TCustomBitmap32.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
  Result := FBackend.QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := inherited QueryInterface(IID, Obj);
end;

procedure TCustomBitmap32.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer);
begin
  FBackend.ChangeSize(Width, Height, NewWidth, NewHeight);
end;

procedure TCustomBitmap32.BackendChangingHandler(Sender: TObject);
begin
  // descendants can override this method.
end;

procedure TCustomBitmap32.BackendChangedHandler(Sender: TObject);
begin
  FBits := FBackend.Bits;
  ResetClipRect;
end;

function TCustomBitmap32.Empty: Boolean;
begin
  Result := FBackend.Empty or inherited Empty;
end;

procedure TCustomBitmap32.Clear;
begin
  Clear(clBlack32);
end;

procedure TCustomBitmap32.Clear(FillColor: TColor32);
begin
  if Empty then Exit;
  if not MeasuringMode then
    if Clipping then
      FillRect(FClipRect.Left, FClipRect.Top, FClipRect.Right, FClipRect.Bottom, FillColor)
    else
      FillLongword(Bits[0], Width * Height, FillColor);
  Changed;
end;

procedure TCustomBitmap32.Delete;
begin
  SetSize(0, 0);
end;

procedure TCustomBitmap32.AssignTo(Dst: TPersistent);

  procedure AssignToBitmap(Bmp: TBitmap; SrcBitmap: TCustomBitmap32);
  var
    SavedBackend: TCustomBackend;
  begin
    RequireBackendSupport(SrcBitmap, [IDeviceContextSupport], romOr, False, SavedBackend);
    try
      Bmp.HandleType := bmDIB;
      Bmp.PixelFormat := pf32Bit;

{$IFDEF COMPILER2009}
      Bmp.SetSize(SrcBitmap.Width, SrcBitmap.Height);
{$ELSE}
      Bmp.Width := SrcBitmap.Width;
      Bmp.Height := SrcBitmap.Height;
{$ENDIF}

      if Supports(SrcBitmap.Backend, IFontSupport) then // this is optional
        Bmp.Canvas.Font.Assign((SrcBitmap.Backend as IFontSupport).Font);

      if SrcBitmap.Empty then Exit;

      (SrcBitmap.Backend as IDeviceContextSupport).DrawTo(Bmp.Canvas.Handle, BoundsRect, BoundsRect)
    finally
      RestoreBackend(SrcBitmap, SavedBackend);
    end;
  end;

var
  Bmp: TBitmap;
begin
  if Dst is TPicture then
    AssignToBitmap(TPicture(Dst).Bitmap, Self)
  else if Dst is TBitmap then
    AssignToBitmap(TBitmap(Dst), Self)
  else if Dst is TClipboard then
  begin
    Bmp := TBitmap.Create;
    try
      AssignToBitmap(Bmp, Self);
      TClipboard(Dst).Assign(Bmp);
    finally
      Bmp.Free;
    end;
  end
  else
    inherited;
end;

procedure TCustomBitmap32.Assign(Source: TPersistent);

  procedure AssignFromGraphicPlain(TargetBitmap: TCustomBitmap32;
    SrcGraphic: TGraphic; FillColor: TColor32; ResetAlphaAfterDrawing: Boolean);
  var
    SavedBackend: TCustomBackend;
    Canvas: TCanvas;
  begin
    RequireBackendSupport(TargetBitmap, [IDeviceContextSupport, ICanvasSupport], romOr, True, SavedBackend);
    try
      TargetBitmap.SetSize(SrcGraphic.Width, SrcGraphic.Height);
      if TargetBitmap.Empty then Exit;

      TargetBitmap.Clear(FillColor);

      if Supports(TargetBitmap.Backend, IDeviceContextSupport) then
      begin
        Canvas := TCanvas.Create;
        try
          Canvas.Handle := (TargetBitmap.Backend as IDeviceContextSupport).Handle;
          TGraphicAccess(SrcGraphic).Draw(Canvas,
            MakeRect(0, 0, TargetBitmap.Width, TargetBitmap.Height));
        finally
          Canvas.Free;
        end;
      end else
      if Supports(TargetBitmap.Backend, ICanvasSupport) then
        TGraphicAccess(SrcGraphic).Draw((TargetBitmap.Backend as ICanvasSupport).Canvas,
          MakeRect(0, 0, TargetBitmap.Width, TargetBitmap.Height))
      else raise Exception.Create('Inpropriate Backend');

      if ResetAlphaAfterDrawing then
        ResetAlpha;
    finally
      RestoreBackend(TargetBitmap, SavedBackend);
    end;
  end;

  procedure AssignFromGraphicMasked(TargetBitmap: TCustomBitmap32; SrcGraphic: TGraphic);
  var
    TempBitmap: TCustomBitmap32;
    I: integer;
    DstP, SrcP: PColor32;
    DstColor: TColor32;
  begin
    AssignFromGraphicPlain(TargetBitmap, SrcGraphic, clWhite32, False); // mask on white
    if TargetBitmap.Empty then
    begin
      TargetBitmap.Clear;
      Exit;
    end;

    TempBitmap := TCustomBitmap32.Create;
    try
      AssignFromGraphicPlain(TempBitmap, SrcGraphic, clRed32, False); // mask on red

      DstP := @TargetBitmap.Bits[0];
      SrcP := @TempBitmap.Bits[0];
      for I := 0 to TargetBitmap.Width * TargetBitmap.Height - 1 do
      begin
        DstColor := DstP^ and $00FFFFFF;
        // this checks for transparency by comparing the pixel-color of the
        // temporary bitmap (red masked) with the pixel of our
        // bitmap (white masked). If they match, make that pixel opaque
        if DstColor = (SrcP^ and $00FFFFFF) then
          DstP^ := DstColor or $FF000000
        else
        // if the colors do not match (that is the case if there is a
        // match "is clRed32 = clWhite32 ?"), just make that pixel
        // transparent:
          DstP^ := DstColor;

         Inc(SrcP); Inc(DstP);
      end;
    finally
      TempBitmap.Free;
    end;
  end;

  procedure AssignFromBitmap(TargetBitmap: TCustomBitmap32; SrcBmp: TBitmap);
  var
    TransparentColor: TColor32;
    DstP: PColor32;
    I: integer;
    DstColor: TColor32;
  begin
    AssignFromGraphicPlain(TargetBitmap, SrcBmp, 0, SrcBmp.PixelFormat <> pf32bit);
    if TargetBitmap.Empty then Exit;

    if SrcBmp.Transparent then
    begin
      TransparentColor := Color32(SrcBmp.TransparentColor) and $00FFFFFF;
      DstP := @TargetBitmap.Bits[0];
      for I := 0 to TargetBitmap.Width * TargetBitmap.Height - 1 do
      begin
        DstColor := DstP^ and $00FFFFFF;
        if DstColor = TransparentColor then
          DstP^ := DstColor;
        Inc(DstP);
      end;
    end;

    if Supports(TargetBitmap.Backend, IFontSupport) then // this is optional
      (TargetBitmap.Backend as IFontSupport).Font.Assign(SrcBmp.Canvas.Font);
  end;

  procedure AssignFromIcon(TargetBitmap: TCustomBitmap32; SrcIcon: TIcon);
  var
    I: Integer;
    P: PColor32Entry;
    ReassignFromMasked: Boolean;
  begin
    AssignFromGraphicPlain(TargetBitmap, SrcIcon, 0, False);
    if TargetBitmap.Empty then Exit;

    // Check if the icon was painted with a merged alpha channel.
    // The happens transparently for new-style 32-bit icons.
    // For all other bit depths GDI will reset our alpha channel to opaque.
    ReassignFromMasked := True;
    P := PColor32Entry(@TargetBitmap.Bits[0]);
    for I := 0 to TargetBitmap.Height * TargetBitmap.Width - 1 do
    begin
      if P.A > 0 then
      begin
        ReassignFromMasked := False;
        Break;
      end;
      Inc(P);
    end;

    // No alpha values found? Use masked approach...
    if ReassignFromMasked then
      AssignFromGraphicMasked(TargetBitmap, SrcIcon);
  end;

  procedure AssignFromGraphic(TargetBitmap: TCustomBitmap32; SrcGraphic: TGraphic);
  begin
    if SrcGraphic is TBitmap then
      AssignFromBitmap(TargetBitmap, TBitmap(SrcGraphic))
    else if SrcGraphic is TIcon then
      AssignFromIcon(TargetBitmap, TIcon(SrcGraphic))
{$IFNDEF PLATFORM_INDEPENDENT}
    else if SrcGraphic is TMetaFile then
      AssignFromGraphicMasked(TargetBitmap, SrcGraphic)
{$ENDIF}
    else
      AssignFromGraphicPlain(TargetBitmap, SrcGraphic, clWhite32, True);
  end;

var
  Picture: TPicture;
begin
  BeginUpdate;
  try
    if not Assigned(Source) then
      SetSize(0, 0)
    else if Source is TCustomBitmap32 then
    begin
      TCustomBitmap32(Source).CopyMapTo(Self);
      TCustomBitmap32(Source).CopyPropertiesTo(Self);
    end
    else if Source is TGraphic then
      AssignFromGraphic(Self, TGraphic(Source))
    else if Source is TPicture then
      AssignFromGraphic(Self, TPicture(Source).Graphic)
    else if Source is TClipboard then
    begin
      Picture := TPicture.Create;
      try
        Picture.Assign(TClipboard(Source));
        AssignFromGraphic(Self, Picture.Graphic);
      finally
        Picture.Free;
      end;
    end
    else
      inherited; // default handler
  finally;
    EndUpdate;
    Changed;
  end;
end;

procedure TCustomBitmap32.CopyMapTo(Dst: TCustomBitmap32);
begin
  Dst.SetSize(Width, Height);
  if not Empty then
    MoveLongword(Bits[0], Dst.Bits[0], Width * Height);
end;

procedure TCustomBitmap32.CopyPropertiesTo(Dst: TCustomBitmap32);
begin
  with Dst do
  begin
    DrawMode := Self.DrawMode;
    CombineMode := Self.CombineMode;
    WrapMode := Self.WrapMode;
    MasterAlpha := Self.MasterAlpha;
    OuterColor := Self.OuterColor;

{$IFDEF DEPRECATEDMODE}
    StretchFilter := Self.StretchFilter;
{$ENDIF}
    ResamplerClassName := Self.ResamplerClassName;
    if Assigned(Resampler) and Assigned(Self.Resampler) then
      Resampler.Assign(Self.Resampler);
  end;
end;

{$IFDEF BITS_GETTER}
function TCustomBitmap32.GetBits: PColor32Array;
begin
  Result := FBackend.Bits;
end;
{$ENDIF}

procedure TCustomBitmap32.SetPixel(X, Y: Integer; Value: TColor32);
begin
  Bits[X + Y * Width] := Value;
end;

procedure TCustomBitmap32.SetPixelS(X, Y: Integer; Value: TColor32);
begin
  if {$IFDEF CHANGED_IN_PIXELS}not FMeasuringMode and{$ENDIF}
    (X >= FClipRect.Left) and (X < FClipRect.Right) and
    (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    Bits[X + Y * Width] := Value;

{$IFDEF CHANGED_IN_PIXELS}
  Changed(MakeRect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

function TCustomBitmap32.GetScanLine(Y: Integer): PColor32Array;
begin
  Result := @Bits[Y * FWidth];
end;

function TCustomBitmap32.GetPixel(X, Y: Integer): TColor32;
begin
  Result := Bits[X + Y * Width];
end;

function TCustomBitmap32.GetPixelS(X, Y: Integer): TColor32;
begin
  if (X >= FClipRect.Left) and (X < FClipRect.Right) and
     (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    Result := Bits[X + Y * Width]
  else
    Result := OuterColor;
end;

function TCustomBitmap32.GetPixelPtr(X, Y: Integer): PColor32;
begin
  Result := @Bits[X + Y * Width];
end;

procedure TCustomBitmap32.Draw(DstX, DstY: Integer; Src: TCustomBitmap32);
begin
  if Assigned(Src) then Src.DrawTo(Self, DstX, DstY);
end;

procedure TCustomBitmap32.Draw(DstX, DstY: Integer; const SrcRect: TRect; Src: TCustomBitmap32);
begin
  if Assigned(Src) then Src.DrawTo(Self, DstX, DstY, SrcRect);
end;

procedure TCustomBitmap32.Draw(const DstRect, SrcRect: TRect; Src: TCustomBitmap32);
begin
  if Assigned(Src) then Src.DrawTo(Self, DstRect, SrcRect);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32);
begin
  BlockTransfer(Dst, 0, 0, Dst.ClipRect, Self, BoundsRect, DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer);
begin
  BlockTransfer(Dst, DstX, DstY, Dst.ClipRect, Self, BoundsRect, DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; DstX, DstY: Integer; const SrcRect: TRect);
begin
  BlockTransfer(Dst, DstX, DstY, Dst.ClipRect, Self, SrcRect, DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; const DstRect: TRect);
begin
  StretchTransfer(Dst, DstRect, Dst.ClipRect, Self, BoundsRect, Resampler, DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.DrawTo(Dst: TCustomBitmap32; const DstRect, SrcRect: TRect);
begin
  StretchTransfer(Dst, DstRect, Dst.ClipRect, Self, SrcRect, Resampler, DrawMode, FOnPixelCombine);
end;

procedure TCustomBitmap32.ResetAlpha;
begin
  ResetAlpha($FF);
end;

procedure TCustomBitmap32.ResetAlpha(const AlphaValue: Byte);
var
  I: Integer;
  P: PByteArray;
begin
  if not FMeasuringMode then
  begin
    P := Pointer(Bits);
    Inc(P, 3); //shift the pointer to 'alpha' component of the first pixel

    I := Width * Height;

    if I > 16 then
    begin
      I := I * 4 - 64;
      Inc(P, I);

      //16x enrolled loop
      I := - I;
      repeat
        P[I] := AlphaValue;
        P[I +  4] := AlphaValue;
        P[I +  8] := AlphaValue;
        P[I + 12] := AlphaValue;
        P[I + 16] := AlphaValue;
        P[I + 20] := AlphaValue;
        P[I + 24] := AlphaValue;
        P[I + 28] := AlphaValue;
        P[I + 32] := AlphaValue;
        P[I + 36] := AlphaValue;
        P[I + 40] := AlphaValue;
        P[I + 44] := AlphaValue;
        P[I + 48] := AlphaValue;
        P[I + 52] := AlphaValue;
        P[I + 56] := AlphaValue;
        P[I + 60] := AlphaValue;
        Inc(I, 64)
      until I > 0;

      //eventually remaining bits
      Dec(I, 64);
      while I < 0 do
      begin
        P[I + 64] := AlphaValue;
        Inc(I, 4);
      end;
    end
    else
    begin
      Dec(I);
      I := I * 4;
      while I >= 0 do
      begin
        P[I] := AlphaValue;
        Dec(I, 4);
      end;
    end;
  end;
  Changed;
end;

function TCustomBitmap32.GetPixelB(X, Y: Integer): TColor32;
begin
  // WARNING: this function should never be used on empty bitmaps !!!
  if X < 0 then X := 0
  else if X >= Width then X := Width - 1;
  if Y < 0 then Y := 0
  else if Y >= Height then Y := Height - 1;
  Result := Bits[X + Y * Width];
end;

procedure TCustomBitmap32.SetPixelT(X, Y: Integer; Value: TColor32);
begin
  TBlendMem(BlendProc)(Value, Bits[X + Y * Width]);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelT(var Ptr: PColor32; Value: TColor32);
begin
  TBlendMem(BlendProc)(Value, Ptr^);
  Inc(Ptr);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelTS(X, Y: Integer; Value: TColor32);
begin
  if {$IFDEF CHANGED_IN_PIXELS}not FMeasuringMode and{$ENDIF}
    (X >= FClipRect.Left) and (X < FClipRect.Right) and
    (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
  begin
    TBlendMem(BlendProc)(Value, Bits[X + Y * Width]);
    EMMS;
  end;
{$IFDEF CHANGED_IN_PIXELS}
  Changed(MakeRect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

procedure TCustomBitmap32.SET_T256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
begin
  { Warning: EMMS should be called after using this method }

  flrx := X and $FF;
  flry := Y and $FF;

  {$IFNDEF TARGET_x86}
  X := X div 256;
  Y := Y div 256;
  {$ELSE}
  asm
    SAR X, 8
    SAR Y, 8
  end;
  {$ENDIF}

  P := @Bits[X + Y * FWidth];
  if FCombineMode = cmBlend then
  begin
    A := C shr 24;  // opacity
    celx := A * GAMMA_TABLE[flrx xor 255];
    cely := GAMMA_TABLE[flry xor 255];
    flrx := A * GAMMA_TABLE[flrx];
    flry := GAMMA_TABLE[flry];

    CombineMem(C, P^, celx * cely shr 16); Inc(P);
    CombineMem(C, P^, flrx * cely shr 16); Inc(P, FWidth);
    CombineMem(C, P^, flrx * flry shr 16); Dec(P);
    CombineMem(C, P^, celx * flry shr 16);
  end
  else
  begin
    celx := GAMMA_TABLE[flrx xor 255];
    cely := GAMMA_TABLE[flry xor 255];
    flrx := GAMMA_TABLE[flrx];
    flry := GAMMA_TABLE[flry];
    
    CombineMem(MergeReg(C, P^), P^, celx * cely shr 8); Inc(P);
    CombineMem(MergeReg(C, P^), P^, flrx * cely shr 8); Inc(P, FWidth);
    CombineMem(MergeReg(C, P^), P^, flrx * flry shr 8); Dec(P);
    CombineMem(MergeReg(C, P^), P^, celx * flry shr 8);
  end;
end;

procedure TCustomBitmap32.SET_TS256(X, Y: Integer; C: TColor32);
var
  flrx, flry, celx, cely: Longword;
  P: PColor32;
  A: TColor32;
begin
  { Warning: EMMS should be called after using this method }

  // we're checking against Left - 1 and Top - 1 due to antialiased values...
  if (X < F256ClipRect.Left - 256) or (X >= F256ClipRect.Right) or
     (Y < F256ClipRect.Top - 256) or (Y >= F256ClipRect.Bottom) then Exit;

  flrx := X and $FF;
  flry := Y and $FF;

  {$IFNDEF TARGET_x86}
  X := X div 256;
  Y := Y div 256;
  {$ELSE}
  asm
    SAR X, 8
    SAR Y, 8
  end;
  {$ENDIF}

  P := @Bits[X + Y * FWidth];
  if FCombineMode = cmBlend then
  begin
    A := C shr 24;  // opacity
    celx := A * GAMMA_TABLE[flrx xor 255];
    cely := GAMMA_TABLE[flry xor 255];
    flrx := A * GAMMA_TABLE[flrx];
    flry := GAMMA_TABLE[flry];

    if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
       (X < FClipRect.Right - 1) and (Y < FClipRect.Bottom - 1) then
    begin
      CombineMem(C, P^, celx * cely shr 16); Inc(P);
      CombineMem(C, P^, flrx * cely shr 16); Inc(P, FWidth);
      CombineMem(C, P^, flrx * flry shr 16); Dec(P);
      CombineMem(C, P^, celx * flry shr 16);
    end
    else // "pixel" lies on the edge of the bitmap
    with FClipRect do
    begin
      if (X >= Left) and (Y >= Top) then CombineMem(C, P^, celx * cely shr 16); Inc(P);
      if (X < Right - 1) and (Y >= Top) then CombineMem(C, P^, flrx * cely shr 16); Inc(P, FWidth);
      if (X < Right - 1) and (Y < Bottom - 1) then CombineMem(C, P^, flrx * flry shr 16); Dec(P);
      if (X >= Left) and (Y < Bottom - 1) then CombineMem(C, P^, celx * flry shr 16);
    end;
  end
  else
  begin
    celx := GAMMA_TABLE[flrx xor 255];
    cely := GAMMA_TABLE[flry xor 255];
    flrx := GAMMA_TABLE[flrx];
    flry := GAMMA_TABLE[flry];

    if (X >= FClipRect.Left) and (Y >= FClipRect.Top) and
       (X < FClipRect.Right - 1) and (Y < FClipRect.Bottom - 1) then
    begin
      CombineMem(MergeReg(C, P^), P^, celx * cely shr 8); Inc(P);
      CombineMem(MergeReg(C, P^), P^, flrx * cely shr 8); Inc(P, FWidth);
      CombineMem(MergeReg(C, P^), P^, flrx * flry shr 8); Dec(P);
      CombineMem(MergeReg(C, P^), P^, celx * flry shr 8);
    end
    else // "pixel" lies on the edge of the bitmap
    with FClipRect do
    begin
      if (X >= Left) and (Y >= Top) then CombineMem(MergeReg(C, P^), P^, celx * cely shr 8); Inc(P);
      if (X < Right - 1) and (Y >= Top) then CombineMem(MergeReg(C, P^), P^, flrx * cely shr 8); Inc(P, FWidth);
      if (X < Right - 1) and (Y < Bottom - 1) then CombineMem(MergeReg(C, P^), P^, flrx * flry shr 8); Dec(P);
      if (X >= Left) and (Y < Bottom - 1) then CombineMem(MergeReg(C, P^), P^, celx * flry shr 8);
    end;
  end;
end;

procedure TCustomBitmap32.SetPixelF(X, Y: Single; Value: TColor32);
begin
  SET_T256(Round(X * 256), Round(Y * 256), Value);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelX(X, Y: TFixed; Value: TColor32);
begin
  X := (X + $7F) shr 8;
  Y := (Y + $7F) shr 8;
  SET_T256(X, Y, Value);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelFS(X, Y: Single; Value: TColor32);
begin
{$IFDEF CHANGED_IN_PIXELS}
  if not FMeasuringMode then
  begin
{$ENDIF}
    SET_TS256(Round(X * 256), Round(Y * 256), Value);
    EMMS;
{$IFDEF CHANGED_IN_PIXELS}
  end;
  Changed(MakeRect(FloatRect(X, Y, X + 1, Y + 1)));
{$ENDIF}
end;

procedure TCustomBitmap32.SetPixelFW(X, Y: Single; Value: TColor32);
begin
{$IFDEF CHANGED_IN_PIXELS}
  if not FMeasuringMode then
  begin
{$ENDIF}
    SetPixelXW(Round(X * FixedOne), Round(Y * FixedOne), Value);
    EMMS;
{$IFDEF CHANGED_IN_PIXELS}
  end;
  Changed(MakeRect(FloatRect(X, Y, X + 1, Y + 1)));
{$ENDIF}
end;

procedure TCustomBitmap32.SetPixelXS(X, Y: TFixed; Value: TColor32);
begin
{$IFDEF CHANGED_IN_PIXELS}
  if not FMeasuringMode then
  begin
{$ENDIF}
    {$IFNDEF TARGET_x86}
    X := (X + $7F) div 256;
    Y := (Y + $7F) div 256;
    {$ELSE}
    asm
          ADD X, $7F
          ADD Y, $7F
          SAR X, 8
          SAR Y, 8
    end;
    {$ENDIF}

    SET_TS256(X, Y, Value);
    EMMS;
{$IFDEF CHANGED_IN_PIXELS}
  end;
  Changed(MakeRect(X, Y, X + 1, Y + 1));
{$ENDIF}
end;

function TCustomBitmap32.GET_T256(X, Y: Integer): TColor32;
// When using this, remember that it interpolates towards next x and y!
var
  Pos: Integer;
begin
  Pos := (X shr 8) + (Y shr 8) * FWidth;
  Result := Interpolator(GAMMA_TABLE[X and $FF xor 255],
                         GAMMA_TABLE[Y and $FF xor 255],
                         @Bits[Pos], @Bits[Pos + FWidth]);
end;

function TCustomBitmap32.GET_TS256(X, Y: Integer): TColor32;
var
  Width256, Height256: Integer;
begin
	if (X >= F256ClipRect.Left) and (Y >= F256ClipRect.Top) then
  begin
    Width256 := (FClipRect.Right - 1) shl 8;
    Height256 := (FClipRect.Bottom - 1) shl 8;

		if (X < Width256) and (Y < Height256) then
			Result := GET_T256(X,Y)
		else if (X = Width256) and (Y <= Height256) then
			// We're exactly on the right border: no need to interpolate.
			Result := Pixel[FClipRect.Right - 1, Y shr 8]
		else if (X <= Width256) and (Y = Height256) then
			// We're exactly on the bottom border: no need to interpolate.
			Result := Pixel[X shr 8, FClipRect.Bottom - 1]
		else
			Result := FOuterColor;
	end
  else
		Result := FOuterColor;
end;

function TCustomBitmap32.GetPixelF(X, Y: Single): TColor32;
begin
  Result := GET_T256(Round(X * 256), Round(Y * 256));
  EMMS;
end;

function TCustomBitmap32.GetPixelFS(X, Y: Single): TColor32;
begin
  Result := GET_TS256(Round(X * 256), Round(Y * 256));
  EMMS;
end;

function TCustomBitmap32.GetPixelFW(X, Y: Single): TColor32;
begin
  Result := GetPixelXW(Round(X * FixedOne), Round(Y * FixedOne));
  EMMS;
end;

function TCustomBitmap32.GetPixelX(X, Y: TFixed): TColor32;
begin
  X := (X + $7F) shr 8;
  Y := (Y + $7F) shr 8;
  Result := GET_T256(X, Y);
  EMMS;
end;

function TCustomBitmap32.GetPixelXS(X, Y: TFixed): TColor32;
{$IFDEF PUREPASCAL}
begin
  X := (X + $7F) div 256;
  Y := (Y + $7F) div 256;
  Result := GET_TS256(X, Y);
  EMMS;
{$ELSE}
{$IFDEF TARGET_x86}
asm
  ADD X, $7F
  ADD Y, $7F
  SAR X, 8
  SAR Y, 8
  CALL TCustomBitmap32.GET_TS256
  MOV Result, EAX
  cmp MMX_ACTIVE.Integer, $00
  jz @Exit
  db $0F, $77               /// EMMS
@Exit:
{$ENDIF}
{$ENDIF}
end;

function TCustomBitmap32.GetPixelFR(X, Y: Single): TColor32;
begin
  Result := FResampler.GetSampleFloat(X, Y);
end;

function TCustomBitmap32.GetPixelXR(X, Y: TFixed): TColor32;
begin
  Result := FResampler.GetSampleFixed(X, Y);
end;

function TCustomBitmap32.GetPixelW(X, Y: Integer): TColor32;
begin
  with FClipRect do
    Result := Bits[FWidth * WrapProcVert(Y, Top, Bottom - 1) + WrapProcHorz(X, Left, Right - 1)];
end;

procedure TCustomBitmap32.SetPixelW(X, Y: Integer; Value: TColor32);
begin
  with FClipRect do
    Bits[FWidth * WrapProcVert(Y, Top, Bottom - 1) + WrapProcHorz(X, Left, Right - 1)] := Value;
end;

function TCustomBitmap32.GetPixelXW(X, Y: TFixed): TColor32;
var
  X1, X2, Y1, Y2 :Integer;
  W: Integer;
begin
  X2 := TFixedRec(X).Int;
  Y2 := TFixedRec(Y).Int;

  with FClipRect do
  begin
    W := Right - 1;
    X1 := WrapProcHorz(X2, Left, W);
    X2 := WrapProcHorz(X2 + 1, Left, W);
    W := Bottom - 1;
    Y1 := WrapProcVert(Y2, Top, W) * Width;
    Y2 := WrapProcVert(Y2 + 1, Top, W) * Width;
  end;

  W := WordRec(TFixedRec(X).Frac).Hi;

  Result := CombineReg(CombineReg(Bits[X2 + Y2], Bits[X1 + Y2], W),
                       CombineReg(Bits[X2 + Y1], Bits[X1 + Y1], W),
                       WordRec(TFixedRec(Y).Frac).Hi);
  EMMS;
end;

procedure TCustomBitmap32.SetPixelXW(X, Y: TFixed; Value: TColor32);
begin
  {$IFNDEF TARGET_x86}
  X := (X + $7F) div 256;
  Y := (Y + $7F) div 256;
  {$ELSE}
  asm
        ADD X, $7F
        ADD Y, $7F
        SAR X, 8
        SAR Y, 8
  end;
  {$ENDIF}

  with F256ClipRect do
    SET_T256(WrapProcHorz(X, Left, Right - 128), WrapProcVert(Y, Top, Bottom - 128), Value);
  EMMS;
end;


procedure TCustomBitmap32.SetStipple(NewStipple: TArrayOfColor32);
begin
  FStippleCounter := 0;
  FStipplePattern := Copy(NewStipple, 0, Length(NewStipple));
end;

procedure TCustomBitmap32.SetStipple(NewStipple: array of TColor32);
var
  L: Integer;
begin
  FStippleCounter := 0;
  L := High(NewStipple) + 1;
  SetLength(FStipplePattern, L);
  MoveLongword(NewStipple[0], FStipplePattern[0], L);
end;

procedure TCustomBitmap32.AdvanceStippleCounter(LengthPixels: Single);
var
  L: Integer;
  Delta: Single;
begin
  L := Length(FStipplePattern);
  Delta := LengthPixels * FStippleStep;
  if (L = 0) or (Delta = 0) then Exit;
  FStippleCounter := FStippleCounter + Delta;
  FStippleCounter := FStippleCounter - Floor(FStippleCounter / L) * L;
end;

function TCustomBitmap32.GetStippleColor: TColor32;
var
  L: Integer;
  NextIndex, PrevIndex: Integer;
  PrevWeight: Integer;
begin
  L := Length(FStipplePattern);
  if L = 0 then
  begin
    // no pattern defined, just return something and exit
    Result := clBlack32;
    Exit;
  end;
  while FStippleCounter >= L do FStippleCounter := FStippleCounter - L;
  while FStippleCounter < 0 do FStippleCounter := FStippleCounter + L;
  PrevIndex := Round(FStippleCounter - 0.5);
  PrevWeight := 255 - Round(255 * (FStippleCounter - PrevIndex));
  if PrevIndex < 0 then FStippleCounter := L - 1;
  NextIndex := PrevIndex + 1;
  if NextIndex >= L then NextIndex := 0;
  if PrevWeight = 255 then Result := FStipplePattern[PrevIndex]
  else
  begin
    Result := CombineReg(
      FStipplePattern[PrevIndex],
      FStipplePattern[NextIndex],
      PrevWeight);
    EMMS;
  end;
  FStippleCounter := FStippleCounter + FStippleStep;
end;

procedure TCustomBitmap32.HorzLine(X1, Y, X2: Integer; Value: TColor32);
begin
  FillLongword(Bits[X1 + Y * Width], X2 - X1 + 1, Value);
end;

procedure TCustomBitmap32.HorzLineS(X1, Y, X2: Integer; Value: TColor32);
begin
  if FMeasuringMode then
    Changed(MakeRect(X1, Y, X2, Y + 1))
  else if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) and
    TestClip(X1, X2, FClipRect.Left, FClipRect.Right) then
  begin
    HorzLine(X1, Y, X2, Value);
    Changed(MakeRect(X1, Y, X2, Y + 1));
  end;
end;

procedure TCustomBitmap32.HorzLineT(X1, Y, X2: Integer; Value: TColor32);
var
  i: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  if X2 < X1 then Exit;
  P := PixelPtr[X1, Y];
  BlendMem := TBlendMem(BlendProc);
  for i := X1 to X2 do
  begin
    BlendMem(Value, P^);
    Inc(P);
  end;
  EMMS;
end;

procedure TCustomBitmap32.HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
begin
  if FMeasuringMode then
    Changed(MakeRect(X1, Y, X2, Y + 1))
  else if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) and
    TestClip(X1, X2, FClipRect.Left, FClipRect.Right) then
  begin
    HorzLineT(X1, Y, X2, Value);
    Changed(MakeRect(X1, Y, X2, Y + 1));
  end;
end;

procedure TCustomBitmap32.HorzLineTSP(X1, Y, X2: Integer);
var
  I, N: Integer;
begin
  if FMeasuringMode then
    Changed(MakeRect(X1, Y, X2, Y + 1))
  else
  begin
    if Empty then Exit;
    if (Y >= FClipRect.Top) and (Y < FClipRect.Bottom) then
    begin
      if ((X1 < FClipRect.Left) and (X2 < FClipRect.Left)) or
         ((X1 >= FClipRect.Right) and (X2 >= FClipRect.Right)) then
      begin
        AdvanceStippleCounter(Abs(X2 - X1) + 1);
        Exit;
      end;
      if X1 < FClipRect.Left then
      begin
        AdvanceStippleCounter(FClipRect.Left - X1);
        X1 := FClipRect.Left;
      end
      else if X1 >= FClipRect.Right then
      begin
        AdvanceStippleCounter(X1 - (FClipRect.Right - 1));
        X1 := FClipRect.Right - 1;
      end;
      N := 0;
      if X2 < FClipRect.Left then
      begin
        N := FClipRect.Left - X2;
        X2 := FClipRect.Left;
      end
      else if X2 >= FClipRect.Right then
      begin
        N := X2 - (FClipRect.Right - 1);
        X2 := FClipRect.Right - 1;
      end;

      if X2 >= X1 then
        for I := X1 to X2 do SetPixelT(I, Y, GetStippleColor)
      else
        for I := X1 downto X2 do SetPixelT(I, Y, GetStippleColor);

      Changed(MakeRect(X1, Y, X2, Y + 1));

      if N > 0 then AdvanceStippleCounter(N);
    end
    else
      AdvanceStippleCounter(Abs(X2 - X1) + 1);
  end;
end;

procedure TCustomBitmap32.HorzLineX(X1, Y, X2: TFixed; Value: TColor32);
//Author: Michael Hansen
var
  I: Integer;
  ChangedRect: TFixedRect;
  X1F, X2F, YF, Count: Integer;
  Wx1, Wx2, Wy, Wt: TColor32;
  PDst: PColor32;
begin
  if X1 > X2 then Swap(X1, X2);

  ChangedRect := FixedRect(X1, Y, X2, Y + 1);
  try
    X1F := X1 shr 16;
    X2F := X2 shr 16;
    YF := Y shr 16;

    PDst := PixelPtr[X1F, YF];

    Wy := Y and $ffff xor $ffff;
    Wx1 := X1 and $ffff xor $ffff;
    Wx2 := X2 and $ffff;

    Count := X2F - X1F - 1;
    if Wy > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wy * Wx1) shr 24]);
      Wt := GAMMA_TABLE[Wy shr 8];
      Inc(PDst);
      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst);
      end;
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wy * Wx2) shr 24]);
    end;

    PDst := PixelPtr[X1F, YF + 1];

    Wy := Wy xor $ffff;
    if Wy > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wy * Wx1) shr 24]);
      Inc(PDst);
      Wt := GAMMA_TABLE[Wy shr 8];
      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst);
      end;
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wy * Wx2) shr 24]);
    end;

  finally
    EMMS;
    Changed(MakeRect(ChangedRect), AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);
//author: Michael Hansen
var
  ChangedRect: TFixedRect;
begin
  if X1 > X2 then Swap(X1, X2);
  ChangedRect := FixedRect(X1, Y, X2, Y + 1);
  if not FMeasuringMode then
  begin
    X1 := Constrain(X1, FFixedClipRect.Left, FFixedClipRect.Right);
    X2 := Constrain(X2, FFixedClipRect.Left, FFixedClipRect.Right);
    if (Abs(X2 - X1) > FIXEDONE) and InRange(Y, FFixedClipRect.Top, FFixedClipRect.Bottom - FIXEDONE) then
      HorzLineX(X1, Y, X2, Value)
    else
      LineXS(X1, Y, X2, Y, Value);
  end;
  Changed(MakeRect(ChangedRect), AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.VertLine(X, Y1, Y2: Integer; Value: TColor32);
var
  I, NH, NL: Integer;
  P: PColor32;
begin
  if Y2 < Y1 then Exit;
  P := PixelPtr[X, Y1];
  I := Y2 - Y1 + 1;
  NH := I shr 2;
  NL := I and $03;
  for I := 0 to NH - 1 do
  begin
    P^ := Value; Inc(P, Width);
    P^ := Value; Inc(P, Width);
    P^ := Value; Inc(P, Width);
    P^ := Value; Inc(P, Width);
  end;
  for I := 0 to NL - 1 do
  begin
    P^ := Value; Inc(P, Width);
  end;
end;

procedure TCustomBitmap32.VertLineS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if FMeasuringMode then
    Changed(MakeRect(X, Y1, X + 1, Y2))
  else if (X >= FClipRect.Left) and (X < FClipRect.Right) and
    TestClip(Y1, Y2, FClipRect.Top, FClipRect.Bottom) then
  begin
    VertLine(X, Y1, Y2, Value);
    Changed(MakeRect(X, Y1, X + 1, Y2));
  end;
end;

procedure TCustomBitmap32.VertLineT(X, Y1, Y2: Integer; Value: TColor32);
var
  i: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
begin
  P := PixelPtr[X, Y1];
  BlendMem := TBlendMem(BlendProc);
  for i := Y1 to Y2 do
  begin
    BlendMem(Value, P^);
    Inc(P, Width);
  end;
  EMMS;
end;

procedure TCustomBitmap32.VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
begin
  if FMeasuringMode then
    Changed(MakeRect(X, Y1, X + 1, Y2))
  else if (X >= FClipRect.Left) and (X < FClipRect.Right) and
    TestClip(Y1, Y2, FClipRect.Top, FClipRect.Bottom) then
  begin
    VertLineT(X, Y1, Y2, Value);
    Changed(MakeRect(X, Y1, X + 1, Y2));
  end;
end;

procedure TCustomBitmap32.VertLineTSP(X, Y1, Y2: Integer);
var
  I, N: Integer;
begin
  if FMeasuringMode then
    Changed(MakeRect(X, Y1, X + 1, Y2))
  else
  begin
    if Empty then Exit;
    if (X >= FClipRect.Left) and (X < FClipRect.Right) then
    begin
      if ((Y1 < FClipRect.Top) and (Y2 < FClipRect.Top)) or
         ((Y1 >= FClipRect.Bottom) and (Y2 >= FClipRect.Bottom)) then
      begin
        AdvanceStippleCounter(Abs(Y2 - Y1) + 1);
        Exit;
      end;
      if Y1 < FClipRect.Top then
      begin
        AdvanceStippleCounter(FClipRect.Top - Y1);
        Y1 := FClipRect.Top;
      end
      else if Y1 >= FClipRect.Bottom then
      begin
        AdvanceStippleCounter(Y1 - (FClipRect.Bottom - 1));
        Y1 := FClipRect.Bottom - 1;
      end;
      N := 0;
      if Y2 < FClipRect.Top then
      begin
        N := FClipRect.Top - Y2;
        Y2 := FClipRect.Top;
      end
      else if Y2 >= FClipRect.Bottom then
      begin
        N := Y2 - (FClipRect.Bottom - 1);
        Y2 := FClipRect.Bottom - 1;
      end;

      if Y2 >= Y1 then
        for I := Y1 to Y2 do SetPixelT(X, I, GetStippleColor)
      else
        for I := Y1 downto Y2 do SetPixelT(X, I, GetStippleColor);

      Changed(MakeRect(X, Y1, X + 1, Y2));

      if N > 0 then AdvanceStippleCounter(N);
    end
    else
      AdvanceStippleCounter(Abs(Y2 - Y1) + 1);
  end;
end;

procedure TCustomBitmap32.VertLineX(X, Y1, Y2: TFixed; Value: TColor32);
//Author: Michael Hansen
var
  I: Integer;
  ChangedRect: TFixedRect;
  Y1F, Y2F, XF, Count: Integer;
  Wy1, Wy2, Wx, Wt: TColor32;
  PDst: PColor32;
begin
  if Y1 > Y2 then Swap(Y1, Y2);

  ChangedRect := FixedRect(X, Y1, X + 1, Y2);
  try
    Y1F := Y1 shr 16;
    Y2F := Y2 shr 16;
    XF := X shr 16;

    PDst := PixelPtr[XF, Y1F];

    Wx := X and $ffff xor $ffff;
    Wy1 := Y1 and $ffff xor $ffff;
    Wy2 := Y2 and $ffff;

    Count := Y2F - Y1F - 1;
    if Wx > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wx * Wy1) shr 24]);
      Wt := GAMMA_TABLE[Wx shr 8];
      Inc(PDst, FWidth);
      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst, FWidth);
      end;
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wx * Wy2) shr 24]);
    end;

    PDst := PixelPtr[XF + 1, Y1F];

    Wx := Wx xor $ffff;
    if Wx > 0 then
    begin
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wx * Wy1) shr 24]);
      Inc(PDst, FWidth);
      Wt := GAMMA_TABLE[Wx shr 8];
      for I := 0 to Count - 1 do
      begin
        CombineMem(Value, PDst^, Wt);
        Inc(PDst, FWidth);
      end;
      CombineMem(Value, PDst^, GAMMA_TABLE[(Wx * Wy2) shr 24]);
    end;

  finally
    EMMS;
    Changed(MakeRect(ChangedRect), AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);
//author: Michael Hansen
var
  ChangedRect: TFixedRect;
begin
  if Y1 > Y2 then Swap(Y1, Y2);
  ChangedRect := FixedRect(X, Y1, X + 1, Y2);
  if not FMeasuringMode then
  begin
    Y1 := Constrain(Y1, FFixedClipRect.Top, FFixedClipRect.Bottom - FIXEDONE);
    Y2 := Constrain(Y2, FFixedClipRect.Top, FFixedClipRect.Bottom - FIXEDONE);
    if (Abs(Y2 - Y1) > FIXEDONE) and InRange(X, FFixedClipRect.Left, FFixedClipRect.Right - FIXEDONE) then
      VertLineX(X, Y1, Y2, Value)
    else
      LineXS(X, Y1, X, Y2, Value);
  end;
  Changed(MakeRect(ChangedRect), AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.Line(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);
  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then Sx := 1
    else if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then VertLine(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLine(X1, Y2 + 1, Y1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    if Dy > 0 then Sy := 1
    else if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if X2 > X1 then HorzLine(X1, Y1, X2 - 1, Value)
      else HorzLine(X2 + 1, Y1, X1, Value);
      if L then Pixel[X2, Y2] := Value;
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    if Dx > Dy then
    begin
      Delta := Dx shr 1;
      for I := 0 to Dx - 1 do
      begin
        P^ := Value;
        Inc(P, Sx);
        Inc(Delta, Dy);
        if Delta >= Dx then
        begin
          Inc(P, Sy);
          Dec(Delta, Dx);
        end;
      end;
    end
    else // Dx < Dy
    begin
      Delta := Dy shr 1;
      for I := 0 to Dy - 1 do
      begin
        P^ := Value;
        Inc(P, Sy);
        Inc(Delta, Dx);
        if Delta >= Dy then
        begin
          Inc(P, Sx);
          Dec(Delta, Dy);
        end;
      end;
    end;
    if L then P^ := Value;
  finally
    Changed(ChangedRect, AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.LineS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dx2, Dy2,Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, rem, term, e: Integer;
  OC: Int64;
  Swapped, CheckAux: Boolean;
  P: PColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then VertLineS(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLineS(X1, Y2 + 1, Y1, Value);
      if L then PixelS[X2, Y2] := Value;
      Changed;
      Exit;
    end
    else if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then HorzLineS(X1, Y1, X2 - 1, Value)
      else if Dx < 0 then HorzLineS(X2 + 1, Y1, X1, Value);
      if L then PixelS[X2, Y2] := Value;
      Changed;
      Exit;
    end;

    Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
    Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      If (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
      Sx := 1;
    end
    else
    begin
      If (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      If (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
      Sy := 1;
    end
    else
    begin
      If (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swapped := True;
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
    end
    else
      Swapped := False;

    // Bresenham's set up:
    Dx2 := Dx shl 1; Dy2 := Dy shl 1;
    xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
    CheckAux := True;

    // clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      OC := Int64(Dx2) * (Cy1 - Y1) - Dx;
      Inc(xd, OC div Dy2);
      rem := OC mod Dy2;
      if xd > Cx2 then Exit;
      if xd >= Cx1 then
      begin
        yd := Cy1;
        Dec(e, rem + Dx);
        if rem > 0 then
        begin
          Inc(xd);
          Inc(e, Dy2);
        end;
        CheckAux := False; // to avoid ugly labels we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckAux and (X1 < Cx1) then
    begin
      OC := Int64(Dy2) * (Cx1 - X1);
      Inc(yd, OC div Dx2);
      rem := OC mod Dx2;
      if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then Exit;
      xd := Cx1;
      Inc(e, rem);
      if (rem >= Dx) then
      begin
        Inc(yd);
        Dec(e, Dx2);
      end;
    end;

    // set auxiliary var to indicate that temp is not clipped, since
    // temp still has the unclipped value assigned at setup.
    CheckAux := False;

    // is the segment exiting the clipping rect?
    if Y2 > Cy2 then
    begin
      OC := Dx2 * (Cy2 - Y1) + Dx;
      term := X1 + OC div Dy2;
      rem := OC mod Dy2;
      if rem = 0 then Dec(term);
      CheckAux := True; // set auxiliary var to indicate that temp is clipped
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      CheckAux := True; // set auxiliary var to indicate that temp is clipped
    end;

    Inc(term);

    if Sy = -1 then
      yd := -yd;

    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
    end;

    Dec(Dx2, Dy2);

    if Swapped then
    begin
      PI := Sx * Width;
      P := @Bits[yd + xd * Width];
    end
    else
    begin
      PI := Sx;
      Sy := Sy * Width;
      P := @Bits[xd + yd * Width];
    end;

    // do we need to skip the last pixel of the line and is temp not clipped?
    if not(L or CheckAux) then
    begin
      if xd < term then
        Dec(term)
      else
        Inc(term);
    end;

    while xd <> term do
    begin
      Inc(xd, Sx);

      P^ := Value;
      Inc(P, PI);
      if e >= 0 then
      begin
        Inc(P, Sy);
        Dec(e, Dx2);
      end
      else
        Inc(e, Dy2);
    end;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.LineT(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dy, Dx, Sy, Sx, I, Delta: Integer;
  P: PColor32;
  BlendMem: TBlendMem;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);
  try
    Dx := X2 - X1;
    Dy := Y2 - Y1;

    if Dx > 0 then Sx := 1
    else if Dx < 0 then
    begin
      Dx := -Dx;
      Sx := -1;
    end
    else // Dx = 0
    begin
      if Dy > 0 then VertLineT(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLineT(X1, Y2 + 1, Y1, Value);
      if L then SetPixelT(X2, Y2, Value);
      Exit;
    end;

    if Dy > 0 then Sy := 1
    else if Dy < 0 then
    begin
      Dy := -Dy;
      Sy := -1;
    end
    else // Dy = 0
    begin
      if X2 > X1 then HorzLineT(X1, Y1, X2 - 1, Value)
      else HorzLineT(X2 + 1, Y1, X1, Value);
      if L then SetPixelT(X2, Y2, Value);
      Exit;
    end;

    P := PixelPtr[X1, Y1];
    Sy := Sy * Width;

    try
      BlendMem := TBlendMem(BlendProc);
      if Dx > Dy then
      begin
        Delta := Dx shr 1;
        for I := 0 to Dx - 1 do
        begin
          BlendMem(Value, P^);
          Inc(P, Sx);
          Inc(Delta, Dy);
          if Delta >= Dx then
          begin
            Inc(P, Sy);
            Dec(Delta, Dx);
          end;
        end;
      end
      else // Dx < Dy
      begin
        Delta := Dy shr 1;
        for I := 0 to Dy - 1 do
        begin
          BlendMem(Value, P^);
          Inc(P, Sy);
          Inc(Delta, Dx);
          if Delta >= Dy then
          begin
            Inc(P, Sx);
            Dec(Delta, Dy);
          end;
        end;
      end;
      if L then BlendMem(Value, P^);
    finally
      EMMS;
    end;
  finally
    Changed(ChangedRect, AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.LineTS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, Dx2, Dy2, rem, term, e: Integer;
  OC: Int64;
  Swapped, CheckAux: Boolean;
  P: PColor32;
  BlendMem: TBlendMem;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then VertLineTS(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLineTS(X1, Y2 + 1, Y1, Value);
      if L then SetPixelTS(X2, Y2, Value);
      Exit;
    end
    else if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then HorzLineTS(X1, Y1, X2 - 1, Value)
      else if Dx < 0 then HorzLineTS(X2 + 1, Y1, X1, Value);
      if L then SetPixelTS(X2, Y2, Value);
      Exit;
    end;

    Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
    Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      If (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
      Sx := 1;
    end
    else
    begin
      If (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      If (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
      Sy := 1;
    end
    else
    begin
      If (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swapped := True;
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
    end
    else
      Swapped := False;

    // Bresenham's set up:
    Dx2 := Dx shl 1; Dy2 := Dy shl 1;
    xd := X1; yd := Y1; e := Dy2 - Dx; term := X2;
    CheckAux := True;

    // clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      OC := Int64(Dx2) * (Cy1 - Y1) - Dx;
      Inc(xd, OC div Dy2);
      rem := OC mod Dy2;
      if xd > Cx2 then Exit;
      if xd >= Cx1 then
      begin
        yd := Cy1;
        Dec(e, rem + Dx);
        if rem > 0 then
        begin
          Inc(xd);
          Inc(e, Dy2);
        end;
        CheckAux := False; // to avoid ugly labels we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckAux and (X1 < Cx1) then
    begin
      OC := Int64(Dy2) * (Cx1 - X1);
      Inc(yd, OC div Dx2);
      rem := OC mod Dx2;
      if (yd > Cy2) or (yd = Cy2) and (rem >= Dx) then Exit;
      xd := Cx1;
      Inc(e, rem);
      if (rem >= Dx) then
      begin
        Inc(yd);
        Dec(e, Dx2);
      end;
    end;

    // set auxiliary var to indicate that temp is not clipped, since
    // temp still has the unclipped value assigned at setup.
    CheckAux := False;

    // is the segment exiting the clipping rect?
    if Y2 > Cy2 then
    begin
      OC := Int64(Dx2) * (Cy2 - Y1) + Dx;
      term := X1 + OC div Dy2;
      rem := OC mod Dy2;
      if rem = 0 then Dec(term);
      CheckAux := True; // set auxiliary var to indicate that temp is clipped
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      CheckAux := True; // set auxiliary var to indicate that temp is clipped
    end;

    Inc(term);

    if Sy = -1 then
      yd := -yd;

    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
    end;

    Dec(Dx2, Dy2);

    if Swapped then
    begin
      PI := Sx * Width;
      P := @Bits[yd + xd * Width];
    end
    else
    begin
      PI := Sx;
      Sy := Sy * Width;
      P := @Bits[xd + yd * Width];
    end;

    // do we need to skip the last pixel of the line and is temp not clipped?
    if not(L or CheckAux) then
    begin
      if xd < term then
        Dec(term)
      else
        Inc(term);
    end;

    try
      BlendMem := BLEND_MEM[FCombineMode]^;
      while xd <> term do
      begin
        Inc(xd, Sx);

        BlendMem(Value, P^);
        Inc(P, PI);
        if e >= 0 then
        begin
          Inc(P, Sy);
          Dec(e, Dx2);
        end
        else
          Inc(e, Dy2);
      end;
    finally
      EMMS;
    end;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.LineX(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean);
var
  n, i: Integer;
  nx, ny, hyp: Integer;
  A: TColor32;
  h: Single;
  ChangedRect: TFixedRect;
begin
  ChangedRect := FixedRect(X1, Y1, X2, Y2);
  try
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Hypot(nx, ny);
    if L then Inc(hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      h := 65536 / hyp;
      nx := Round(nx * h); ny := Round(ny * h);
      for i := 0 to n - 1 do
      begin
        SET_T256(X1 shr 8, Y1 shr 8, Value);
        Inc(X1, nx);
        Inc(Y1, ny);
      end;
    end;
    A := Value shr 24;
    hyp := hyp - n shl 16;
    A := A * Cardinal(hyp) shl 8 and $FF000000;
    SET_T256((X1 + X2 - nx) shr 9, (Y1 + Y2 - ny) shr 9, Value and $00FFFFFF + A);
  finally
    EMMS;
    Changed(MakeRect(ChangedRect), AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.LineF(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
begin
  LineX(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), Value, L);
end;

procedure TCustomBitmap32.LineXS(X1, Y1, X2, Y2: TFixed; Value: TColor32; L: Boolean);
var
  n, i: Integer;
  ex, ey, nx, ny, hyp: Integer;
  A: TColor32;
  h: Single;
  ChangedRect: TFixedRect;
begin
  ChangedRect := FixedRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    ex := X2; ey := Y2;

    // Check for visibility and clip the coordinates
    if not ClipLine(Integer(X1), Integer(Y1), Integer(X2), Integer(Y2),
      FFixedClipRect.Left - $10000, FFixedClipRect.Top - $10000,
      FFixedClipRect.Right, FFixedClipRect.Bottom) then Exit;

    { TODO : Handle L on clipping here... }

    if (ex <> X2) or (ey <> Y2) then L := True;

    // Check if it lies entirely in the bitmap area. Even after clipping
    // some pixels may lie outside the bitmap due to antialiasing
    if (X1 > FFixedClipRect.Left) and (X1 < FFixedClipRect.Right - $20000) and
       (Y1 > FFixedClipRect.Top) and (Y1 < FFixedClipRect.Bottom - $20000) and
       (X2 > FFixedClipRect.Left) and (X2 < FFixedClipRect.Right - $20000) and
       (Y2 > FFixedClipRect.Top) and (Y2 < FFixedClipRect.Bottom - $20000) then
    begin
      LineX(X1, Y1, X2, Y2, Value, L);
      Exit;
    end;

    // If we are still here, it means that the line touches one or several bitmap
    // boundaries. Use the safe version of antialiased pixel routine
    try
      nx := X2 - X1; ny := Y2 - Y1;
      Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
      hyp := Hypot(nx, ny);
      if L then Inc(Hyp, 65536);
      if hyp < 256 then Exit;
      n := hyp shr 16;
      if n > 0 then
      begin
        h := 65536 / hyp;
        nx := Round(nx * h); ny := Round(ny * h);
        for i := 0 to n - 1 do
        begin
          SET_TS256(SAR_8(X1), SAR_8(Y1), Value);
          X1 := X1 + nx;
          Y1 := Y1 + ny;
        end;
      end;
      A := Value shr 24;
      hyp := hyp - n shl 16;
      A := A * Longword(hyp) shl 8 and $FF000000;
      SET_TS256(SAR_9(X1 + X2 - nx), SAR_9(Y1 + Y2 - ny), Value and $00FFFFFF + A);
    finally
      EMMS;
    end;
  end;
  Changed(MakeRect(ChangedRect), AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.LineFS(X1, Y1, X2, Y2: Single; Value: TColor32; L: Boolean);
begin
  LineXS(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), Value, L);
end;

procedure TCustomBitmap32.LineXP(X1, Y1, X2, Y2: TFixed; L: Boolean);
var
  n, i: Integer;
  nx, ny, hyp: Integer;
  A, C: TColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(FixedRect(X1, Y1, X2, Y2));
  try
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Hypot(nx, ny);
    if L then Inc(hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      nx := Round(nx / hyp * 65536);
      ny := Round(ny / hyp * 65536);
      for i := 0 to n - 1 do
      begin
        C := GetStippleColor;
        SET_T256(X1 shr 8, Y1 shr 8, C);
        EMMS;
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;
    C := GetStippleColor;
    A := C shr 24;
    hyp := hyp - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_T256((X1 + X2 - nx) shr 9, (Y1 + Y2 - ny) shr 9, C and $00FFFFFF + A);
    EMMS;
  finally
    Changed(ChangedRect, AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.LineFP(X1, Y1, X2, Y2: Single; L: Boolean);
begin
  LineXP(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), L);
end;

procedure TCustomBitmap32.LineXSP(X1, Y1, X2, Y2: TFixed; L: Boolean);
const
  StippleInc: array [Boolean] of Single = (0, 1);
var
  n, i: Integer;
  sx, sy, ex, ey, nx, ny, hyp: Integer;
  A, C: TColor32;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(FixedRect(X1, Y1, X2, Y2));
  
  if not FMeasuringMode then
  begin
    sx := X1; sy := Y1; ex := X2; ey := Y2;

    // Check for visibility and clip the coordinates
    if not ClipLine(Integer(X1), Integer(Y1), Integer(X2), Integer(Y2),
      FFixedClipRect.Left - $10000, FFixedClipRect.Top - $10000,
      FFixedClipRect.Right, FFixedClipRect.Bottom) then
    begin
      AdvanceStippleCounter(Hypot((X2 - X1) shr 16, (Y2 - Y1) shr 16) - StippleInc[L]);
      Exit;
    end;

    if (ex <> X2) or (ey <> Y2) then L := True;

    // Check if it lies entirely in the bitmap area. Even after clipping
    // some pixels may lie outside the bitmap due to antialiasing
    if (X1 > FFixedClipRect.Left) and (X1 < FFixedClipRect.Right - $20000) and
       (Y1 > FFixedClipRect.Top) and (Y1 < FFixedClipRect.Bottom - $20000) and
       (X2 > FFixedClipRect.Left) and (X2 < FFixedClipRect.Right - $20000) and
       (Y2 > FFixedClipRect.Top) and (Y2 < FFixedClipRect.Bottom - $20000) then
    begin
      LineXP(X1, Y1, X2, Y2, L);
      Exit;
    end;

    if (sx <> X1) or (sy <> Y1) then
      AdvanceStippleCounter(Hypot((X1 - sx) shr 16, (Y1 - sy) shr 16));

    // If we are still here, it means that the line touches one or several bitmap
    // boundaries. Use the safe version of antialiased pixel routine
    nx := X2 - X1; ny := Y2 - Y1;
    Inc(X1, 127); Inc(Y1, 127); Inc(X2, 127); Inc(Y2, 127);
    hyp := Hypot(nx, ny);
    if L then Inc(hyp, 65536);
    if hyp < 256 then Exit;
    n := hyp shr 16;
    if n > 0 then
    begin
      nx := Round(nx / hyp * 65536); ny := Round(ny / hyp * 65536);
      for i := 0 to n - 1 do
      begin
        C := GetStippleColor;
        SET_TS256(SAR_8(X1), SAR_8(Y1), C);
        EMMS;
        X1 := X1 + nx;
        Y1 := Y1 + ny;
      end;
    end;
    C := GetStippleColor;
    A := C shr 24;
    hyp := hyp - n shl 16;
    A := A * Longword(hyp) shl 8 and $FF000000;
    SET_TS256(SAR_9(X1 + X2 - nx), SAR_9(Y1 + Y2 - ny), C and $00FFFFFF + A);
    EMMS;

    if (ex <> X2) or (ey <> Y2) then
      AdvanceStippleCounter(Hypot((X2 - ex) shr 16, (Y2 - ey) shr 16) - StippleInc[L]);
  end;

  Changed(ChangedRect, AREAINFO_LINE + 4);
end;

procedure TCustomBitmap32.LineFSP(X1, Y1, X2, Y2: Single; L: Boolean);
begin
  LineXSP(Fixed(X1), Fixed(Y1), Fixed(X2), Fixed(Y2), L);
end;

procedure TCustomBitmap32.LineA(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Dx, Dy, Sx, Sy, D: Integer;
  EC, EA: Word;
  CI: Byte;
  P: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  if (X1 = X2) or (Y1 = Y2) then
  begin
    LineT(X1, Y1, X2, Y2, Value, L);
    Exit;
  end;

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  if Dx > 0 then Sx := 1
  else
  begin
    Sx := -1;
    Dx := -Dx;
  end;

  if Dy > 0 then Sy := 1
  else
  begin
    Sy := -1;
    Dy := -Dy;
  end;

  try
    EC := 0;
    BLEND_MEM[FCombineMode]^(Value, Bits[X1 + Y1 * Width]);
    BlendMemEx := BLEND_MEM_EX[FCombineMode]^;

    if Dy > Dx then
    begin
      EA := Dx shl 16 div Dy;
      if not L then Dec(Dy);
      while Dy > 0 do
      begin
        Dec(Dy);
        D := EC;
        Inc(EC, EA);
        if EC <= D then Inc(X1, Sx);
        Inc(Y1, Sy);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        Inc(P, Sx);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      end;
    end
    else // DY <= DX
    begin
      EA := Dy shl 16 div Dx;
      if not L then Dec(Dx);
      while Dx > 0 do
      begin
        Dec(Dx);
        D := EC;
        Inc(EC, EA);
        if EC <= D then Inc(Y1, Sy);
        Inc(X1, Sx);
        CI := EC shr 8;
        P := @Bits[X1 + Y1 * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        if Sy = 1 then Inc(P, Width) else Dec(P, Width);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
      end;
    end;
  finally
    EMMS;
    Changed(MakeRect(X1, Y1, X2, Y2), AREAINFO_LINE + 2);
  end;
end;

procedure TCustomBitmap32.LineAS(X1, Y1, X2, Y2: Integer; Value: TColor32; L: Boolean);
var
  Cx1, Cx2, Cy1, Cy2, PI, Sx, Sy, Dx, Dy, xd, yd, rem, term, tmp: Integer;
  CheckVert, CornerAA, TempClipped: Boolean;
  D1, D2: PInteger;
  EC, EA, ED, D: Word;
  CI: Byte;
  P: PColor32;
  BlendMemEx: TBlendMemEx;
  ChangedRect: TRect;
begin
  ChangedRect := MakeRect(X1, Y1, X2, Y2);

  if not FMeasuringMode then
  begin
    If (FClipRect.Right - FClipRect.Left = 0) or
       (FClipRect.Bottom - FClipRect.Top = 0) then Exit;

    Dx := X2 - X1; Dy := Y2 - Y1;

    // check for trivial cases...
    if Abs(Dx) = Abs(Dy) then // diagonal line?
    begin
      LineTS(X1, Y1, X2, Y2, Value, L);
      Exit;
    end
    else if Dx = 0 then // vertical line?
    begin
      if Dy > 0 then VertLineTS(X1, Y1, Y2 - 1, Value)
      else if Dy < 0 then VertLineTS(X1, Y2 + 1, Y1, Value);
      if L then SetPixelTS(X2, Y2, Value);
      Exit;
    end
    else if Dy = 0 then // horizontal line?
    begin
      if Dx > 0 then HorzLineTS(X1, Y1, X2 - 1, Value)
      else if Dx < 0 then HorzLineTS(X2 + 1, Y1, X1, Value);
      if L then SetPixelTS(X2, Y2, Value);
      Exit;
    end;

    Cx1 := FClipRect.Left; Cx2 := FClipRect.Right - 1;
    Cy1 := FClipRect.Top;  Cy2 := FClipRect.Bottom - 1;

    if Dx > 0 then
    begin
      if (X1 > Cx2) or (X2 < Cx1) then Exit; // segment not visible
      Sx := 1;
    end
    else
    begin
      if (X2 > Cx2) or (X1 < Cx1) then Exit; // segment not visible
      Sx := -1;
      X1 := -X1;   X2 := -X2;   Dx := -Dx;
      Cx1 := -Cx1; Cx2 := -Cx2;
      Swap(Cx1, Cx2);
    end;

    if Dy > 0 then
    begin
      if (Y1 > Cy2) or (Y2 < Cy1) then Exit; // segment not visible
      Sy := 1;
    end
    else
    begin
      if (Y2 > Cy2) or (Y1 < Cy1) then Exit; // segment not visible
      Sy := -1;
      Y1 := -Y1;   Y2 := -Y2;   Dy := -Dy;
      Cy1 := -Cy1; Cy2 := -Cy2;
      Swap(Cy1, Cy2);
    end;

    if Dx < Dy then
    begin
      Swap(X1, Y1); Swap(X2, Y2); Swap(Dx, Dy);
      Swap(Cx1, Cy1); Swap(Cx2, Cy2); Swap(Sx, Sy);
      D1 := @yd; D2 := @xd;
      PI := Sy;
    end
    else
    begin
      D1 := @xd; D2 := @yd;
      PI := Sy * Width;
    end;

    rem := 0;
    EA := Dy shl 16 div Dx;
    EC := 0;
    xd := X1; yd := Y1;
    CheckVert := True;
    CornerAA := False;
    BlendMemEx := BLEND_MEM_EX[FCombineMode]^;

    // clipping rect horizontal entry
    if Y1 < Cy1 then
    begin
      tmp := (Cy1 - Y1) * 65536;
      rem := tmp - 65536; // rem := (Cy1 - Y1 - 1) * 65536;
      if tmp mod EA > 0 then
        tmp := tmp div EA + 1
      else
        tmp := tmp div EA;

      xd := Math.Min(xd + tmp, X2 + 1);
      EC := tmp * EA;

      if rem mod EA > 0 then
        rem := rem div EA + 1
      else
        rem := rem div EA;

      tmp := tmp - rem;

      // check whether the line is partly visible
      if xd > Cx2 then
        // do we need to draw an antialiased part on the corner of the clip rect?
        If xd <= Cx2 + tmp then
          CornerAA := True
        else
          Exit;

      if (xd {+ 1} >= Cx1) or CornerAA then
      begin
        yd := Cy1;
        rem := xd; // save old xd

        ED := EC - EA;
        term := SwapConstrain(xd - tmp, Cx1, Cx2);

        If CornerAA then
        begin
          Dec(ED, (xd - Cx2 - 1) * EA);
          xd := Cx2 + 1;
        end;

        // do we need to negate the vars?
        if Sy = -1 then yd := -yd;
        if Sx = -1 then
        begin
          xd := -xd;
          term := -term;
        end;

        // draw special case horizontal line entry (draw only last half of entering segment)
        try
          while xd <> term do
          begin
            Inc(xd, -Sx);
            BlendMemEx(Value, Bits[D1^ + D2^ * Width], GAMMA_TABLE[ED shr 8]);
            Dec(ED, EA);
          end;
        finally
          EMMS;
        end;

        If CornerAA then
        begin
          // we only needed to draw the visible antialiased part of the line,
          // everything else is outside of our cliprect, so exit now since
          // there is nothing more to paint...
          { TODO : Handle Changed here... }
          Changed;
          Exit;
        end;

        if Sy = -1 then yd := -yd;  // negate back
        xd := rem;  // restore old xd
        CheckVert := False; // to avoid ugly labels we set this to omit the next check
      end;
    end;

    // clipping rect vertical entry
    if CheckVert and (X1 < Cx1) then
    begin
      tmp := (Cx1 - X1) * EA;
      Inc(yd, tmp div 65536);
      EC := tmp;
      xd := Cx1;
      if (yd > Cy2) then
        Exit
      else if (yd = Cy2) then
        CornerAA := True;
    end;

    term := X2;
    TempClipped := False;
    CheckVert := False;

    // horizontal exit?
    if Y2 > Cy2 then
    begin
      tmp := (Cy2 - Y1) * 65536;
      term := X1 + tmp div EA;
      if not(tmp mod EA > 0) then
        Dec(Term);

      if term < Cx2 then
      begin
        rem := tmp + 65536; // was: rem := (Cy2 - Y1 + 1) * 65536;
        if rem mod EA > 0 then
          rem := X1 + rem div EA + 1
        else
          rem := X1 + rem div EA;

        if rem > Cx2 then rem := Cx2;
        CheckVert := True;
      end;

      TempClipped := True;
    end;

    if term > Cx2 then
    begin
      term := Cx2;
      TempClipped := True;
    end;

    Inc(term);

    if Sy = -1 then yd := -yd;
    if Sx = -1 then
    begin
      xd := -xd;
      term := -term;
      rem := -rem;
    end;

    // draw line
    if not CornerAA then
    try
      // do we need to skip the last pixel of the line and is temp not clipped?
      if not(L or TempClipped) and not CheckVert then
      begin
        if xd < term then
          Dec(term)
        else if xd > term then
          Inc(term);
      end;

      while xd <> term do
      begin
        CI := EC shr 8;
        P := @Bits[D1^ + D2^ * Width];
        BlendMemEx(Value, P^, GAMMA_TABLE[CI xor 255]);
        Inc(P, PI);
        BlendMemEx(Value, P^, GAMMA_TABLE[CI]);
        // check for overflow and jump to next line...
        D := EC;
        Inc(EC, EA);
        if EC <= D then
          Inc(yd, Sy);

        Inc(xd, Sx);
      end;
    finally
      EMMS;
    end;

    // draw special case horizontal line exit (draw only first half of exiting segment)
    If CheckVert then
    try
      while xd <> rem do
      begin
        BlendMemEx(Value, Bits[D1^ + D2^ * Width], GAMMA_TABLE[EC shr 8 xor 255]);
        Inc(EC, EA);
        Inc(xd, Sx);
      end;
    finally
      EMMS;
    end;
  end;

  Changed(ChangedRect, AREAINFO_LINE + 2);
end;

procedure TCustomBitmap32.MoveTo(X, Y: Integer);
begin
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.LineToS(X, Y: Integer);
begin
  LineS(RasterX, RasterY, X, Y, PenColor);
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.LineToTS(X, Y: Integer);
begin
  LineTS(RasterX, RasterY, X, Y, PenColor);
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.LineToAS(X, Y: Integer);
begin
  LineAS(RasterX, RasterY, X, Y, PenColor);
  RasterX := X;
  RasterY := Y;
end;

procedure TCustomBitmap32.MoveToX(X, Y: TFixed);
begin
  RasterXF := X;
  RasterYF := Y;
end;

procedure TCustomBitmap32.MoveToF(X, Y: Single);
begin
  RasterXF := Fixed(X);
  RasterYF := Fixed(Y);
end;

procedure TCustomBitmap32.LineToXS(X, Y: TFixed);
begin
  LineXS(RasterXF, RasterYF, X, Y, PenColor);
  RasterXF := X;
  RasterYF := Y;
end;

procedure TCustomBitmap32.LineToFS(X, Y: Single);
begin
  LineToXS(Fixed(X), Fixed(Y));
end;

procedure TCustomBitmap32.LineToXSP(X, Y: TFixed);
begin
  LineXSP(RasterXF, RasterYF, X, Y);
  RasterXF := X;
  RasterYF := Y;
end;

procedure TCustomBitmap32.LineToFSP(X, Y: Single);
begin
  LineToXSP(Fixed(X), Fixed(Y));
end;

procedure TCustomBitmap32.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  j: Integer;
  P: PColor32Array;
begin
  if Assigned(FBits) then
    for j := Y1 to Y2 - 1 do
    begin
      P := Pointer(@Bits[j * FWidth]);
      FillLongword(P[X1], X2 - X1, Value);
    end;
    
  Changed(MakeRect(X1, Y1, X2, Y2));
end;

procedure TCustomBitmap32.FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if not FMeasuringMode and
    (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if X1 < FClipRect.Left then X1 := FClipRect.Left;
    if Y1 < FClipRect.Top then Y1 := FClipRect.Top;
    if X2 > FClipRect.Right then X2 := FClipRect.Right;
    if Y2 > FClipRect.Bottom then Y2 := FClipRect.Bottom;
    FillRect(X1, Y1, X2, Y2, Value);
  end;
  Changed(MakeRect(X1, Y1, X2, Y2));
end;

procedure TCustomBitmap32.FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
var
  i, j: Integer;
  P: PColor32;
  A: Integer;
begin
  A := Value shr 24;
  if A = $FF then
    FillRect(X1, Y1, X2, Y2, Value) // calls Changed...
  else if A <> 0 then
  try
    Dec(Y2);
    Dec(X2);
    for j := Y1 to Y2 do
    begin
      P := GetPixelPtr(X1, j);
      if CombineMode = cmBlend then
      begin
        for i := X1 to X2 do
        begin
          CombineMem(Value, P^, A);
          Inc(P);
        end;
      end
      else
      begin
        for i := X1 to X2 do
        begin
          MergeMem(Value, P^);
          Inc(P);
        end;
      end;
    end;
  finally
    EMMS;
    Changed(MakeRect(X1, Y1, X2 + 1, Y2 + 1));
  end;
end;

procedure TCustomBitmap32.FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  if not FMeasuringMode and
    (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if X1 < FClipRect.Left then X1 := FClipRect.Left;
    if Y1 < FClipRect.Top then Y1 := FClipRect.Top;
    if X2 > FClipRect.Right then X2 := FClipRect.Right;
    if Y2 > FClipRect.Bottom then Y2 := FClipRect.Bottom;
    FillRectT(X1, Y1, X2, Y2, Value);
  end;
  Changed(MakeRect(X1, Y1, X2, Y2));
end;

procedure TCustomBitmap32.FillRectS(const ARect: TRect; Value: TColor32);
begin
  if FMeasuringMode then // shortcut...
    Changed(ARect)
  else
    with ARect do FillRectS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.FillRectTS(const ARect: TRect; Value: TColor32);
begin
  if FMeasuringMode then // shortcut...
    Changed(ARect)
  else
    with ARect do FillRectTS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    Dec(Y2);
    Dec(X2);
    HorzLineS(X1, Y1, X2, Value);
    if Y2 > Y1 then HorzLineS(X1, Y2, X2, Value);
    if Y2 > Y1 + 1 then
    begin
      VertLineS(X1, Y1 + 1, Y2 - 1, Value);
      if X2 > X1 then VertLineS(X2, Y1 + 1, Y2 - 1, Value);
    end;
  end;
end;

procedure TCustomBitmap32.FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32);
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
    (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    Dec(Y2);
    Dec(X2);
    HorzLineTS(X1, Y1, X2, Value);
    if Y2 > Y1 then HorzLineTS(X1, Y2, X2, Value);
    if Y2 > Y1 + 1 then
    begin
      VertLineTS(X1, Y1 + 1, Y2 - 1, Value);
      if X2 > X1 then VertLineTS(X2, Y1 + 1, Y2 - 1, Value);
    end;
  end;
end;

procedure TCustomBitmap32.FrameRectTSP(X1, Y1, X2, Y2: Integer);
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
    (X1 < Width) and (Y1 < Height) and  // don't check against ClipRect here
    (X2 > 0) and (Y2 > 0) then          // due to StippleCounter
  begin
    Dec(X2);
    Dec(Y2);
    if X1 = X2 then
      if Y1 = Y2 then
      begin
        SetPixelT(X1, Y1, GetStippleColor);
        Changed(MakeRect(X1, Y1, X1 + 1, Y1 + 1));
      end
      else
        VertLineTSP(X1, Y1, Y2)
    else
      if Y1 = Y2 then HorzLineTSP(X1, Y1, X2)
      else
      begin
        HorzLineTSP(X1, Y1, X2 - 1);
        VertLineTSP(X2, Y1, Y2 - 1);
        HorzLineTSP(X2, Y2, X1 + 1);
        VertLineTSP(X1, Y2, Y1 + 1);
      end;
  end;
end;

procedure TCustomBitmap32.FrameRectS(const ARect: TRect; Value: TColor32);
begin
  with ARect do FrameRectS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.FrameRectTS(const ARect: TRect; Value: TColor32);
begin
  with ARect do FrameRectTS(Left, Top, Right, Bottom, Value);
end;

procedure TCustomBitmap32.RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer);
var
  C1, C2: TColor32;
begin
  // measuring is handled in inner drawing operations...
  if (X2 > X1) and (Y2 > Y1) and
     (X1 < FClipRect.Right) and (Y1 < FClipRect.Bottom) and
     (X2 > FClipRect.Left) and (Y2 > FClipRect.Top) then
  begin
    if (Contrast > 0) then
    begin
      C1 := SetAlpha(clWhite32, Clamp(Contrast * 512 div 100));
      C2 := SetAlpha(clBlack32, Clamp(Contrast * 255 div 100));
    end
    else if Contrast < 0 then
    begin
      Contrast := -Contrast;
      C1 := SetAlpha(clBlack32, Clamp(Contrast * 255 div 100));
      C2 := SetAlpha(clWhite32, Clamp(Contrast * 512 div 100));
    end
    else Exit;

    Dec(X2);
    Dec(Y2);
    HorzLineTS(X1, Y1, X2, C1);
    HorzLineTS(X1, Y2, X2, C2);
    Inc(Y1);
    Dec(Y2);
    VertLineTS(X1, Y1, Y2, C1);
    VertLineTS(X2, Y1, Y2, C2);
  end;
end;

procedure TCustomBitmap32.RaiseRectTS(const ARect: TRect; Contrast: Integer);
begin
  with ARect do RaiseRectTS(Left, Top, Right, Bottom, Contrast);
end;

procedure TCustomBitmap32.LoadFromStream(Stream: TStream);
var
  I, W: integer;
  Header: TBmpHeader;
  B: TBitmap;
begin
  Stream.ReadBuffer(Header, SizeOf(TBmpHeader));

  // Check for Windows bitmap magic bytes and general compatibility of the
  // bitmap data that ought to be loaded...
  if (Header.bfType = $4D42) and
    (Header.biBitCount = 32) and (Header.biPlanes = 1) and
    (Header.biCompression = 0) then
  begin
    SetSize(Header.biWidth, Abs(Header.biHeight));

    // Check whether the bitmap is saved top-down
    if Header.biHeight > 0 then
    begin
      W := Width shl 2;
      for I := Height - 1 downto 0 do
        Stream.ReadBuffer(Scanline[I]^, W);
    end
    else
      Stream.ReadBuffer(Bits^, Width * Height shl 2);
  end
  else
  begin
    Stream.Seek(-SizeOf(TBmpHeader), soFromCurrent);
    B := TBitmap.Create;
    try
      B.LoadFromStream(Stream);
      Assign(B);
    finally
      B.Free;
    end;
  end;

  Changed;
end;

procedure TCustomBitmap32.SaveToStream(Stream: TStream; SaveTopDown: Boolean = False);
var
  Header: TBmpHeader;
  BitmapSize: Integer;
  I, W: Integer;
begin
  BitmapSize := Width * Height shl 2;

  Header.bfType := $4D42; // Magic bytes for Windows Bitmap
  Header.bfSize := BitmapSize + SizeOf(TBmpHeader);
  Header.bfReserved := 0;
  // Save offset relative. However, the spec says it has to be file absolute,
  // which we can not do properly within a stream...
  Header.bfOffBits := SizeOf(TBmpHeader);
  Header.biSize := $28;
  Header.biWidth := Width;

  if SaveTopDown then
    Header.biHeight := Height
  else
    Header.biHeight := -Height;

  Header.biPlanes := 1;
  Header.biBitCount := 32;
  Header.biCompression := 0; // bi_rgb
  Header.biSizeImage := BitmapSize;
  Header.biXPelsPerMeter := 0;
  Header.biYPelsPerMeter := 0;
  Header.biClrUsed := 0;
  Header.biClrImportant := 0;

  Stream.WriteBuffer(Header, SizeOf(TBmpHeader));

  if SaveTopDown then
  begin
    W := Width shl 2;
    for I := Height - 1 downto 0 do
      Stream.WriteBuffer(PixelPtr[0, I]^, W);
  end
  else
  begin
    // NOTE: We can save the whole buffer in one run because
    // we do not support scanline strides (yet).
    Stream.WriteBuffer(Bits^, BitmapSize);
  end;
end;

procedure TCustomBitmap32.LoadFromFile(const FileName: string);
var
  FileStream: TFileStream;
  Header: TBmpHeader;
  P: TPicture;
begin
  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    FileStream.ReadBuffer(Header, SizeOf(TBmpHeader));

    // Check for Windows bitmap magic bytes...
    if Header.bfType = $4D42 then
    begin
      // if it is, use our stream read method...
      FileStream.Seek(-SizeOf(TBmpHeader), soFromCurrent);
      LoadFromStream(FileStream);
      Exit;
    end
  finally
    FileStream.Free;
  end;

  // if we got here, use the fallback approach via TPicture...
  P := TPicture.Create;
  try
    P.LoadFromFile(FileName);
    Assign(P);
  finally
    P.Free;
  end;
end;

procedure TCustomBitmap32.SaveToFile(const FileName: string; SaveTopDown: Boolean = False);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(FileStream, SaveTopDown);
  finally
    FileStream.Free;
  end;
end;

procedure TCustomBitmap32.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.LoadFromResourceID(Instance, ResID);
    Assign(B);
  finally
    B.Free;
    Changed;
  end;
end;

procedure TCustomBitmap32.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.LoadFromResourceName(Instance, ResName);
    Assign(B);
  finally
    B.Free;
    Changed;
  end;
end;

function TCustomBitmap32.Equal(B: TCustomBitmap32): Boolean;
var
  S1, S2: TMemoryStream;
begin
  Result := (B <> nil) and (ClassType = B.ClassType);

  if Empty or B.Empty then
  begin
    Result := Empty and B.Empty;
    Exit;
  end;

  if Result then
  begin
    S1 := TMemoryStream.Create;
    try
      SaveToStream(S1);
      S2 := TMemoryStream.Create;
      try
        B.SaveToStream(S2);
        Result := (S1.Size = S2.Size) and CompareMem(S1.Memory, S2.Memory, S1.Size);
      finally
        S2.Free;
      end;
    finally
      S1.Free;
    end;
  end;
end;

procedure TCustomBitmap32.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TCustomBitmap32) or
        not Equal(TCustomBitmap32(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

procedure TCustomBitmap32.ReadData(Stream: TStream);
var
  w, h: Integer;
begin
  try
    Stream.ReadBuffer(w, 4);
    Stream.ReadBuffer(h, 4);
    SetSize(w, h);
    Stream.ReadBuffer(Bits[0], FWidth * FHeight * 4);
  finally
    Changed;
  end;
end;

procedure TCustomBitmap32.WriteData(Stream: TStream);
begin
  Stream.WriteBuffer(FWidth, 4);
  Stream.WriteBuffer(FHeight, 4);
  Stream.WriteBuffer(Bits[0], FWidth * FHeight * 4);
end;

procedure TCustomBitmap32.SetCombineMode(const Value: TCombineMode);
begin
  if FCombineMode <> Value then
  begin
    FCombineMode := Value;
    BlendProc := @BLEND_MEM[FCombineMode]^;
    Changed;
  end;
end;

procedure TCustomBitmap32.SetDrawMode(Value: TDrawMode);
begin
  if FDrawMode <> Value then
  begin
    FDrawMode := Value;
    Changed;
  end;
end;

procedure TCustomBitmap32.SetWrapMode(Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    WrapProcHorz := GetWrapProcEx(WrapMode, FClipRect.Left, FClipRect.Right - 1);
    WrapProcVert := GetWrapProcEx(WrapMode, FClipRect.Top, FClipRect.Bottom - 1);
    Changed;
  end;
end;

procedure TCustomBitmap32.SetMasterAlpha(Value: Cardinal);
begin
  if FMasterAlpha <> Value then
  begin
    FMasterAlpha := Value;
    Changed;
  end;
end;

{$IFDEF DEPRECATEDMODE}
procedure TCustomBitmap32.SetStretchFilter(Value: TStretchFilter);
begin
  if FStretchFilter <> Value then
  begin
    FStretchFilter := Value;

    case FStretchFilter of
      sfNearest: TNearestResampler.Create(Self);
      sfDraft:   TDraftResampler.Create(Self);
      sfLinear:  TLinearResampler.Create(Self);
    else
      TKernelResampler.Create(Self);
      with FResampler as TKernelResampler do
        case FStretchFilter of
          sfCosine: Kernel := TCosineKernel.Create;
          sfSpline: Kernel := TSplineKernel.Create;
          sfLanczos: Kernel := TLanczosKernel.Create;
          sfMitchell: Kernel := TMitchellKernel.Create;
        end;
    end;

    Changed;
  end;
end;
{$ENDIF}

procedure TCustomBitmap32.Roll(Dx, Dy: Integer; FillBack: Boolean; FillColor: TColor32);
var
  Shift, L: Integer;
  R: TRect;
begin
  if Empty or ((Dx = 0) and (Dy = 0)) then Exit;
  if (Abs(Dx) >= Width) or (Abs(Dy) >= Height) then
  begin
    if FillBack then Clear(FillColor);
    Exit;
  end;

  Shift := Dx + Dy * Width;
  L := (Width * Height - Abs(Shift));

  if Shift > 0 then
    Move(Bits[0], Bits[Shift], L shl 2)
  else
    MoveLongword(Bits[-Shift], Bits[0], L);

  if FillBack then
  begin
    R := MakeRect(0, 0, Width, Height);
    OffsetRect(R, Dx, Dy);
    IntersectRect(R, R, MakeRect(0, 0, Width, Height));
    if R.Top > 0 then FillRect(0, 0, Width, R.Top, FillColor)
    else if R.Top = 0 then FillRect(0, R.Bottom, Width, Height, FillColor);
    if R.Left > 0 then FillRect(0, R.Top, R.Left, R.Bottom, FillColor)
    else if R.Left = 0 then FillRect(R.Right, R.Top, Width, R.Bottom, FillColor);
  end;

  Changed;
end;

procedure TCustomBitmap32.FlipHorz(Dst: TCustomBitmap32);
var
  i, j: Integer;
  P1, P2: PColor32;
  tmp: TColor32;
  W, W2: Integer;
begin
  W := Width;
  if (Dst = nil) or (Dst = Self) then
  begin
    { In-place flipping }
    P1 := PColor32(Bits);
    P2 := P1;
    Inc(P2, Width - 1);
    W2 := Width shr 1;
    for J := 0 to Height - 1 do
    begin
      for I := 0 to W2 - 1 do
      begin
        tmp := P1^;
        P1^ := P2^;
        P2^ := tmp;
        Inc(P1);
        Dec(P2);
      end;
      Inc(P1, W - W2);
      Inc(P2, W + W2);
    end;
    Changed;
  end
  else
  begin
    { Flip to Dst }
    Dst.BeginUpdate;
    Dst.SetSize(W, Height);
    P1 := PColor32(Bits);
    P2 := PColor32(Dst.Bits);
    Inc(P2, W - 1);
    for J := 0 to Height - 1 do
    begin
      for I := 0 to W - 1 do
      begin
        P2^ := P1^;
        Inc(P1);
        Dec(P2);
      end;
      Inc(P2, W shl 1);
    end;
    Dst.EndUpdate;
    Dst.Changed;
  end;
end;

procedure TCustomBitmap32.FlipVert(Dst: TCustomBitmap32);
var
  J, J2: Integer;
  Buffer: PColor32Array;
  P1, P2: PColor32;
begin
  if (Dst = nil) or (Dst = Self) then
  begin
    { in-place }
    J2 := Height - 1;
    GetMem(Buffer, Width shl 2);
    for J := 0 to Height div 2 - 1 do
    begin
      P1 := PixelPtr[0, J];
      P2 := PixelPtr[0, J2];
      MoveLongword(P1^, Buffer^, Width);
      MoveLongword(P2^, P1^, Width);
      MoveLongword(Buffer^, P2^, Width);
      Dec(J2);
    end;
    FreeMem(Buffer);
    Changed;
  end
  else
  begin
    Dst.SetSize(Width, Height);
    J2 := Height - 1;
    for J := 0 to Height - 1 do
    begin
      MoveLongword(PixelPtr[0, J]^, Dst.PixelPtr[0, J2]^, Width);
      Dec(J2);
    end;
    Dst.Changed;
  end;
end;

procedure TCustomBitmap32.Rotate90(Dst: TCustomBitmap32);
var
  Tmp: TCustomBitmap32;
  X, Y, I, J: Integer;
begin
  if Dst = nil then
  begin
    Tmp := TCustomBitmap32.Create;
    Dst := Tmp;
  end
  else
  begin
    Tmp := nil;
    Dst.BeginUpdate;
  end;

  Dst.SetSize(Height, Width);
  I := 0;
  for Y := 0 to Height - 1 do
  begin
    J := Height - 1 - Y;
    for X := 0 to Width - 1 do
    begin
      Dst.Bits[J] := Bits[I];
      Inc(I);
      Inc(J, Height);
    end;
  end;

  if Tmp <> nil then
  begin
    Tmp.CopyMapTo(Self);
    Tmp.Free;
  end
  else
  begin
    Dst.EndUpdate;
    Dst.Changed;
  end;
end;

procedure TCustomBitmap32.Rotate180(Dst: TCustomBitmap32);
var
  I, I2: Integer;
  Tmp: TColor32;
begin
  if Dst <> nil then
  begin
    Dst.SetSize(Width, Height);
    I2 := Width * Height - 1;
    for I := 0 to Width * Height - 1 do
    begin
      Dst.Bits[I2] := Bits[I];
      Dec(I2);
    end;
    Dst.Changed;
  end
  else
  begin
    I2 := Width * Height - 1;
    for I := 0 to Width * Height div 2 - 1 do
    begin
      Tmp := Bits[I2];
      Bits[I2] := Bits[I];
      Bits[I] := Tmp;
      Dec(I2);
    end;
    Changed;
  end;
end;

procedure TCustomBitmap32.Rotate270(Dst: TCustomBitmap32);
var
  Tmp: TCustomBitmap32;
  X, Y, I, J: Integer;
begin
  if Dst = nil then
  begin
    Tmp := TCustomBitmap32.Create; { TODO : Revise creating of temporary bitmaps here... }
    Dst := Tmp;
  end
  else
  begin
    Tmp := nil;
    Dst.BeginUpdate;
  end;

  Dst.SetSize(Height, Width);
  I := 0;
  for Y := 0 to Height - 1 do
  begin
    J := (Width - 1) * Height + Y;
    for X := 0 to Width - 1 do
    begin
      Dst.Bits[J] := Bits[I];
      Inc(I);
      Dec(J, Height);
    end;
  end;

  if Tmp <> nil then
  begin
    Tmp.CopyMapTo(Self);
    Tmp.Free;
  end
  else
  begin
    Dst.EndUpdate;
    Dst.Changed;
  end;
end;

function TCustomBitmap32.BoundsRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

procedure TCustomBitmap32.SetClipRect(const Value: TRect);
begin
  IntersectRect(FClipRect, Value, BoundsRect);
  FFixedClipRect := FixedRect(FClipRect);
  with FClipRect do
    F256ClipRect := Rect(Left shl 8, Top shl 8, Right shl 8, Bottom shl 8);
  FClipping := not EqualRect(FClipRect, BoundsRect);
  WrapProcHorz := GetWrapProcEx(WrapMode, FClipRect.Left, FClipRect.Right - 1);
  WrapProcVert := GetWrapProcEx(WrapMode, FClipRect.Top, FClipRect.Bottom - 1);
end;

procedure TCustomBitmap32.ResetClipRect;
begin
  ClipRect := BoundsRect;
end;

procedure TCustomBitmap32.BeginMeasuring(const Callback: TAreaChangedEvent);
begin
  FMeasuringMode := True;
  FOldOnAreaChanged := FOnAreaChanged;
  FOnAreaChanged := Callback;
end;

procedure TCustomBitmap32.EndMeasuring;
begin
  FMeasuringMode := False;
  FOnAreaChanged := FOldOnAreaChanged;
end;

procedure TCustomBitmap32.PropertyChanged;
begin
  // don't force invalidation of whole bitmap area as this is unnecessary
  inherited Changed;
end;

procedure TCustomBitmap32.Changed;
begin
  if ((FUpdateCount = 0) or FMeasuringMode) and Assigned(FOnAreaChanged) then
    FOnAreaChanged(Self, BoundsRect, AREAINFO_RECT);

  if not FMeasuringMode then
    inherited;
end;

procedure TCustomBitmap32.Changed(const Area: TRect; const Info: Cardinal);
begin
  if ((FUpdateCount = 0) or FMeasuringMode) and Assigned(FOnAreaChanged) then
    FOnAreaChanged(Self, Area, Info);

  if not FMeasuringMode then
    inherited Changed;
end;

procedure TCustomBitmap32.SetResampler(Resampler: TCustomResampler);
begin
  if Assigned(Resampler) and (FResampler <> Resampler) then
  begin
    if Assigned(FResampler) then FResampler.Free;
    FResampler := Resampler;
    Changed;
  end;
end;

function TCustomBitmap32.GetResamplerClassName: string;
begin
  Result := FResampler.ClassName;
end;

procedure TCustomBitmap32.SetResamplerClassName(Value: string);
var
  ResamplerClass: TCustomResamplerClass;
begin
  if (Value <> '') and (FResampler.ClassName <> Value) and Assigned(ResamplerList) then
  begin
    ResamplerClass := TCustomResamplerClass(ResamplerList.Find(Value));
    if Assigned(ResamplerClass) then ResamplerClass.Create(Self);
  end;
end;

{ TBitmap32 }

procedure TBitmap32.InitializeBackend;
begin
  Backend := GetPlatformBackendClass.Create;
end;

procedure TBitmap32.FinalizeBackend;
begin
  if Supports(Backend, IFontSupport) then
    (Backend as IFontSupport).OnFontChange := nil;

  if Supports(Backend, ICanvasSupport) then
    (Backend as ICanvasSupport).OnCanvasChange := nil;

  inherited;
end;

procedure TBitmap32.BackendChangingHandler(Sender: TObject);
begin
  inherited;
  FontChanged(Self);
  DeleteCanvas;
end;

procedure TBitmap32.BackendChangedHandler(Sender: TObject);
begin
  inherited;
  HandleChanged;
end;

procedure TBitmap32.FontChanged(Sender: TObject);
begin
  // TODO: still required?
end;

procedure TBitmap32.CanvasChanged(Sender: TObject);
begin
  Changed;
end;

procedure TBitmap32.CopyPropertiesTo(Dst: TCustomBitmap32);
begin
  inherited;

  if Dst is TBitmap32 then
    TBitmap32(Dst).Font.Assign(Self.Font);
end;

function TBitmap32.GetCanvas: TCanvas;
begin
  Result := (FBackend as ICanvasSupport).Canvas;
end;

function TBitmap32.GetBitmapInfo: TBitmapInfo;
begin
  Result := (FBackend as IBitmapContextSupport).BitmapInfo;
end;

function TBitmap32.GetHandle: HBITMAP;
begin
  Result := (FBackend as IBitmapContextSupport).BitmapHandle;
end;

function TBitmap32.GetHDC: HDC;
begin
  Result := (FBackend as IDeviceContextSupport).Handle;
end;

function TBitmap32.GetFont: TFont;
begin
  Result := (FBackend as IFontSupport).Font;
end;

procedure TBitmap32.SetBackend(const Backend: TCustomBackend);
begin
  if Assigned(Backend) and (Backend <> FBackend) then
  begin
    if Supports(Backend, IFontSupport) then
      (Backend as IFontSupport).OnFontChange := FontChanged;

    if Supports(Backend, ICanvasSupport) then
      (Backend as ICanvasSupport).OnCanvasChange := CanvasChanged;

    inherited;
  end;
end;

procedure TBitmap32.SetFont(Value: TFont);
begin
  (FBackend as IFontSupport).Font := Value;
end;

procedure TBitmap32.HandleChanged;
begin
  if Assigned(FOnHandleChanged) then FOnHandleChanged(Self);
end;

{$IFDEF BCB}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: Cardinal);
{$ELSE}
procedure TBitmap32.Draw(const DstRect, SrcRect: TRect; hSrc: HDC);
{$ENDIF}
begin
  (FBackend as IDeviceContextSupport).Draw(DstRect, SrcRect, hSrc);
end;

procedure TBitmap32.DrawTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; DstX, DstY: Integer);
begin
  if Empty then Exit;
  (FBackend as IDeviceContextSupport).DrawTo(hDst, DstX, DstY);
end;

procedure TBitmap32.DrawTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; const DstRect, SrcRect: TRect);
begin
  if Empty then Exit;
  (FBackend as IDeviceContextSupport).DrawTo(hDst, DstRect, SrcRect);
end;

procedure TBitmap32.TileTo(hDst: {$IFDEF BCB}Cardinal{$ELSE}HDC{$ENDIF}; const DstRect, SrcRect: TRect);
const
  MaxTileSize = 1024;
var
  DstW, DstH: Integer;
  TilesX, TilesY: Integer;
  Buffer: TCustomBitmap32;
  I, J: Integer;
  ClipRect, R: TRect;
  X, Y: Integer;
begin
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  TilesX := (DstW + MaxTileSize - 1) div MaxTileSize;
  TilesY := (DstH + MaxTileSize - 1) div MaxTileSize;
  Buffer := TBitmap32.Create;
  try
    for J := 0 to TilesY - 1 do
    begin
      for I := 0 to TilesX - 1 do
      begin
        ClipRect.Left := I * MaxTileSize;
        ClipRect.Top := J * MaxTileSize;
        ClipRect.Right := (I + 1) * MaxTileSize;
        ClipRect.Bottom := (J + 1) * MaxTileSize;
        if ClipRect.Right > DstW then ClipRect.Right := DstW;
        if ClipRect.Bottom > DstH then ClipRect.Bottom := DstH;
        X := ClipRect.Left;
        Y := ClipRect.Top;
        OffsetRect(ClipRect, -X, -Y);
        R := DstRect;
        OffsetRect(R, -X - DstRect.Left, -Y - DstRect.Top);
        Buffer.SetSize(ClipRect.Right, ClipRect.Bottom);
        StretchTransfer(Buffer, R, ClipRect, Self, SrcRect, Resampler, DrawMode, FOnPixelCombine);

        (Buffer.Backend as IDeviceContextSupport).DrawTo(
          hDst,
          MakeRect(X + DstRect.Left, Y + DstRect.Top, ClipRect.Right, ClipRect.Bottom),
          MakeRect(0, 0, Buffer.Width, Buffer.Height)
        );
      end;
    end;
  finally
    Buffer.Free;
  end;
end;

procedure TBitmap32.UpdateFont;
begin
  (FBackend as IFontSupport).UpdateFont;
end;

// Text and Fonts //

function TBitmap32.TextExtent(const Text: String): TSize;
begin
  Result := (FBackend as ITextSupport).TextExtent(Text);
end;

function TBitmap32.TextExtentW(const Text: Widestring): TSize;
begin
  Result := (FBackend as ITextSupport).TextExtentW(Text);
end;

// -------------------------------------------------------------------

procedure TBitmap32.Textout(X, Y: Integer; const Text: String);
begin
  (FBackend as ITextSupport).Textout(X, Y, Text);
end;

procedure TBitmap32.TextoutW(X, Y: Integer; const Text: Widestring);
begin
  (FBackend as ITextSupport).TextoutW(X, Y, Text);
end;

// -------------------------------------------------------------------

procedure TBitmap32.Textout(X, Y: Integer; const ClipRect: TRect; const Text: String);
begin
  (FBackend as ITextSupport).Textout(X, Y, ClipRect, Text);
end;

procedure TBitmap32.TextoutW(X, Y: Integer; const ClipRect: TRect; const Text: Widestring);
begin
  (FBackend as ITextSupport).TextoutW(X, Y, ClipRect, Text);
end;

// -------------------------------------------------------------------

procedure TBitmap32.Textout(DstRect: TRect; const Flags: Cardinal; const Text: String);
begin
  (FBackend as ITextSupport).Textout(DstRect, Flags, Text);
end;

procedure TBitmap32.TextoutW(DstRect: TRect; const Flags: Cardinal; const Text: Widestring);
begin
  (FBackend as ITextSupport).TextoutW(DstRect, Flags, Text);
end;

// -------------------------------------------------------------------

function TBitmap32.TextHeight(const Text: String): Integer;
begin
  Result := (FBackend as ITextSupport).TextExtent(Text).cY;
end;

function TBitmap32.TextHeightW(const Text: Widestring): Integer;
begin
  Result := (FBackend as ITextSupport).TextExtentW(Text).cY;
end;

// -------------------------------------------------------------------

function TBitmap32.TextWidth(const Text: String): Integer;
begin
  Result := (FBackend as ITextSupport).TextExtent(Text).cX;
end;

function TBitmap32.TextWidthW(const Text: Widestring): Integer;
begin
  Result := (FBackend as ITextSupport).TextExtentW(Text).cX;
end;

// -------------------------------------------------------------------

procedure SetFontAntialiasing(const Font: TFont; Quality: Cardinal);
var
  LogFont: TLogFont;
begin
  with LogFont do
  begin
    lfHeight := Font.Height;
    lfWidth := 0; { have font mapper choose }

    {$IFDEF COMPILER2005}
    lfEscapement := Font.Orientation;
    lfOrientation := Font.Orientation;
    {$ELSE}
    lfEscapement := 0;
    lfOrientation := 0;
    {$ENDIF}

    if fsBold in Font.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;

    lfItalic := Byte(fsItalic in Font.Style);
    lfUnderline := Byte(fsUnderline in Font.Style);
    lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    lfCharSet := Byte(Font.Charset);

    // TODO DVT Added cast to fix TFontDataName to String warning. Need to verify is OK
    if AnsiCompareText(Font.Name, 'Default') = 0 then  // do not localize
      StrPCopy(lfFaceName, string(DefFontData.Name))
    else
      StrPCopy(lfFaceName, Font.Name);

    lfQuality := Quality;

    { Only True Type fonts support the angles }
    if lfOrientation <> 0 then
      lfOutPrecision := OUT_TT_ONLY_PRECIS
    else
      lfOutPrecision := OUT_DEFAULT_PRECIS;

    lfClipPrecision := CLIP_DEFAULT_PRECIS;

    case Font.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Font.Handle := CreateFontIndirect(LogFont);
end;

procedure TextBlueToAlpha(const B: TCustomBitmap32; const Color: TColor32);
(*
asm
 push edi
 mov ecx, [B+$44].Integer
 imul ecx, [B+$40].Integer
 mov edi, [B+$54].Integer
 @PixelLoop:
 mov eax, [edi]
 shl eax, 24
 add eax, Color
 mov [edi], eax
 add edi, 4
 loop @PixelLoop
 pop edi
end;
*)
var
  I: Integer;
  P: PColor32;
  C: TColor32;
begin
  // convert blue channel to alpha and fill the color
  P := @B.Bits[0];
  for I := 0 to B.Width * B.Height - 1 do
  begin
    C := P^;
    if C <> 0 then
    begin
      C := P^ shl 24; // transfer blue channel to alpha
      C := C + Color;
      P^ := C;
    end;
    Inc(P);
  end;
end;

procedure TextScaleDown(const B, B2: TCustomBitmap32; const N: Integer;
  const Color: TColor32); // use only the blue channel
var
  I, J, X, Y, P, Q, Sz, S: Integer;
  Src: PColor32;
  Dst: PColor32;
begin
  Sz := 1 shl N - 1;
  Dst := B.PixelPtr[0, 0];
  for J := 0 to B.Height - 1 do
  begin
    Y := J shl N;
    for I := 0 to B.Width - 1 do
    begin
      X := I shl N;
      S := 0;
      for Q := Y to Y + Sz do
      begin
        Src := B2.PixelPtr[X, Q];
        for P := X to X + Sz do
        begin
          S := S + Integer(Src^ and $000000FF);
          Inc(Src);
        end;
      end;
      S := S shr N shr N;
      Dst^ := TColor32(S shl 24) + Color;
      Inc(Dst);
    end;
  end;
end;

procedure TBitmap32.RenderText(X, Y: Integer; const Text: String; AALevel: Integer; Color: TColor32);
var
  B, B2: TBitmap32;
  Sz: TSize;
  Alpha: TColor32;
  PaddedText: String;
begin
  if Empty then Exit;

  Alpha := Color shr 24;
  Color := Color and $00FFFFFF;
  AALevel := Constrain(AALevel, -1, 4);
  PaddedText := Text + ' ';

  if AALevel > -1 then
    SetFontAntialiasing(Font, NONANTIALIASED_QUALITY)
  else
    SetFontAntialiasing(Font, ANTIALIASED_QUALITY);

  { TODO : Optimize Clipping here }
  B := TBitmap32.Create;
  with B do
  try
    if AALevel <= 0 then
    begin
      Sz := Self.TextExtent(PaddedText);
      if Sz.cX > Self.Width then Sz.cX := Self.Width;
      if Sz.cY > Self.Height then Sz.cX := Self.Height;
      SetSize(Sz.cX, Sz.cY);
      Font := Self.Font;
      Clear(0);
      Font.Color := clWhite;
      Textout(0, 0, Text);
      TextBlueToAlpha(B, Color);
    end
    else
    begin
      B2 := TBitmap32.Create;
      with B2 do
      try
        Font := Self.Font;
        Font.Size := Self.Font.Size shl AALevel;
        Font.Color := clWhite;
        Sz := TextExtent(PaddedText);
        Sz.Cx := Sz.cx + 1 shl AALevel;
        Sz.Cy := Sz.cy + 1 shl AALevel;
        SetSize(Sz.Cx, Sz.Cy);
        Clear(0);
        Textout(0, 0, Text);
        B.SetSize(Sz.cx shr AALevel, Sz.cy shr AALevel);
        TextScaleDown(B, B2, AALevel, Color);
      finally
        Free;
      end;
    end;

    DrawMode := dmBlend;
    MasterAlpha := Alpha;
    CombineMode := CombineMode;

    DrawTo(Self, X, Y);
  finally
    Free;
  end;

  SetFontAntialiasing(Font, DEFAULT_QUALITY);
end;

procedure TBitmap32.RenderTextW(X, Y: Integer; const Text: Widestring; AALevel: Integer; Color: TColor32);
var
  B, B2: TBitmap32;
  Sz: TSize;
  Alpha: TColor32;
  StockCanvas: TCanvas;
  PaddedText: Widestring;
begin
  if Empty then Exit;

  Alpha := Color shr 24;
  Color := Color and $00FFFFFF;
  AALevel := Constrain(AALevel, -1, 4);
  PaddedText := Text + ' ';

  if AALevel > -1 then
    SetFontAntialiasing(Font, NONANTIALIASED_QUALITY)
  else
    SetFontAntialiasing(Font, ANTIALIASED_QUALITY);

  { TODO : Optimize Clipping here }
  B := TBitmap32.Create;
  try
    if AALevel = 0 then
    begin
      Sz := TextExtentW(PaddedText);
      B.SetSize(Sz.cX, Sz.cY);
      B.Font := Font;
      B.Clear(0);
      B.Font.Color := clWhite;
      B.TextoutW(0, 0, Text);
      TextBlueToAlpha(B, Color);
    end
    else
    begin
      StockCanvas := StockBitmap.Canvas;
      StockCanvas.Lock;
      try
        StockCanvas.Font := Font;
        StockCanvas.Font.Size := Font.Size shl AALevel;
{$IFDEF PLATFORM_INDEPENDENT}
        Sz := StockCanvas.TextExtent(PaddedText);
{$ELSE}
        Windows.GetTextExtentPoint32W(StockCanvas.Handle, PWideChar(PaddedText),
          Length(PaddedText), Sz);
{$ENDIF}
        Sz.Cx := (Sz.cx shr AALevel + 1) shl AALevel;
        Sz.Cy := (Sz.cy shr AALevel + 1) shl AALevel;
        B2 := TBitmap32.Create;
        try
          B2.SetSize(Sz.Cx, Sz.Cy);
          B2.Clear(0);
          B2.Font := StockCanvas.Font;
          B2.Font.Color := clWhite;
          B2.TextoutW(0, 0, Text);
          B.SetSize(Sz.cx shr AALevel, Sz.cy shr AALevel);
          TextScaleDown(B, B2, AALevel, Color);
        finally
          B2.Free;
        end;
      finally
        StockCanvas.Unlock;
      end;
    end;

    B.DrawMode := dmBlend;
    B.MasterAlpha := Alpha;
    B.CombineMode := CombineMode;

    B.DrawTo(Self, X, Y);
  finally
    B.Free;
  end;
  SetFontAntialiasing(Font, DEFAULT_QUALITY);
end;

// -------------------------------------------------------------------

function TBitmap32.CanvasAllocated: Boolean;
begin
  Result := (FBackend as ICanvasSupport).CanvasAllocated;
end;

procedure TBitmap32.DeleteCanvas;
begin
  if Supports(Backend, ICanvasSupport) then
    (FBackend as ICanvasSupport).DeleteCanvas;
end;


{ TCustomBackend }

constructor TCustomBackend.Create;
begin
  RefCounted := True;
  _AddRef;
  inherited;
end;

constructor TCustomBackend.Create(Owner: TCustomBitmap32);
begin
  FOwner := Owner;
  Create;
   if Assigned(Owner) then
    Owner.Backend := Self;
end;

destructor TCustomBackend.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCustomBackend.Clear;
var
  Width, Height: Integer;
begin
  if Assigned(FOwner) then
    ChangeSize(FOwner.FWidth, FOwner.FHeight, 0, 0, False)
  else
    ChangeSize(Width, Height, 0, 0, False);
end;

procedure TCustomBackend.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{$IFDEF BITS_GETTER}
function TCustomBackend.GetBits: PColor32Array;
begin
  Result := FBits;
end;
{$ENDIF}

procedure TCustomBackend.ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  try
    Changing;

    FinalizeSurface;

    Width := 0;
    Height := 0;

    if (NewWidth > 0) and (NewHeight > 0) then
      InitializeSurface(NewWidth, NewHeight, ClearBuffer);

    Width := NewWidth;
    Height := NewHeight;
  finally
    Changed;
  end;
end;

procedure TCustomBackend.Assign(Source: TPersistent);
var
  SrcBackend: TCustomBackend;
begin
  if Source is TCustomBackend then
  begin
    if Assigned(FOwner) then
    begin
      SrcBackend := TCustomBackend(Source);

      ChangeSize(
        FOwner.FWidth, FOwner.FHeight,
        SrcBackend.FOwner.Width, SrcBackend.FOwner.Height,
        False
      );

      if not SrcBackend.Empty then
        MoveLongword(
          SrcBackend.Bits[0], Bits[0],
          SrcBackend.FOwner.Width * SrcBackend.FOwner.Height
        );
    end;
  end
  else
    inherited;
end;

function TCustomBackend.Empty: Boolean;
begin
  Result := False;
end;

procedure TCustomBackend.FinalizeSurface;
begin
  // descendants override this method
end;

procedure TCustomBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  // descendants override this method
end;

{ TCustomSampler }

function TCustomSampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := GetSampleFixed(X * FixedOne, Y * FixedOne);
end;

function TCustomSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := GetSampleFloat(X * FixedToFloat, Y * FixedToFloat);
end;

function TCustomSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Result := GetSampleFixed(Fixed(X), Fixed(Y));
end;

procedure TCustomSampler.PrepareSampling;
begin
  // descendants override this method
end;

procedure TCustomSampler.FinalizeSampling;
begin
  // descendants override this method
end;

function TCustomSampler.HasBounds: Boolean;
begin
  Result := False;
end;

function TCustomSampler.GetSampleBounds: TFloatRect;
const
  InfRect: TFloatRect = (Left: -Infinity; Top: -Infinity; Right: Infinity; Bottom: Infinity);
begin
  Result := InfRect;
end;


{ TCustomResampler }

procedure TCustomResampler.AssignTo(Dst: TPersistent);
begin
  if Dst is TCustomResampler then
    SmartAssign(Self, Dst)
  else
    inherited;
end;

procedure TCustomResampler.Changed;
begin
  if Assigned(FBitmap) then FBitmap.Changed;
end;

constructor TCustomResampler.Create;
begin
  inherited;
  FPixelAccessMode := pamSafe;
end;

constructor TCustomResampler.Create(ABitmap: TCustomBitmap32);
begin
  Create;
  FBitmap := ABitmap;
  if Assigned(ABitmap) then ABitmap.Resampler := Self;
end;

function TCustomResampler.GetSampleBounds: TFloatRect;
begin
  Result := FloatRect(FBitmap.ClipRect);
  if PixelAccessMode = pamTransparentEdge then
    InflateRect(Result, 1, 1);
end;

function TCustomResampler.GetWidth: TFloat;
begin
  Result := 0;
end;

function TCustomResampler.HasBounds: Boolean;
begin
  Result := FPixelAccessMode <> pamWrap;
end;

procedure TCustomResampler.PrepareSampling;
begin
  FClipRect := FBitmap.ClipRect;
end;

procedure TCustomResampler.SetPixelAccessMode(
  const Value: TPixelAccessMode);
begin
  if FPixelAccessMode <> Value then
  begin
    FPixelAccessMode := Value;
    Changed;
  end;
end;

initialization
  SetGamma;
  StockBitmap := TBitmap.Create;
  StockBitmap.Width := 8;
  StockBitmap.Height := 8;

finalization
  StockBitmap.Free;

end.
