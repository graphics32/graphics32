unit GR32.SVG.Types;

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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Math,
  GR32,
  GR32_Transforms,
  GR32_Math,
  GR32_LowLevel,
  GR32.SVG.Utf8;


//------------------------------------------------------------------------------
//
//      Color space stuff
//
//------------------------------------------------------------------------------

// HSLtoRGB overload for float Alpha in the range [0.0..1.0]
function HSLtoRGB(H, S, L, A: Single): TColor32;


//------------------------------------------------------------------------------
//
//      SvgColorNameToColor
//
//------------------------------------------------------------------------------
// SVG color name -> TColor32
//------------------------------------------------------------------------------
function SvgColorNameToColor(const AName: TValuePUtf8Char; ADefault: TColor32): TColor32;


//------------------------------------------------------------------------------
//
//      Common enums
//
//------------------------------------------------------------------------------
type
  TSvgBlendMode = (
    bmNormal,
    bmMultiply,
    bmScreen,
    bmOverlay,
    bmDarken,
    bmLighten,
    bmColorDodge,
    bmColorBurn,
    bmHardLight,
    bmSoftLight,
    bmDifference,
    bmExclusion
  );

  TSvgIsolation = (
    isoAuto,
    isoIsolate
  );

  TSvgCompositeOperator = (
    coOver,
    coIn,
    coOut,
    coAtop,
    coXor,
    coArithmetic,
    coLighter
  );

  TSvgFeColorMatrixType = (
    cmMatrix,
    cmSaturate,
    cmHueRotate,
    cmLuminanceToAlpha
  );

  TSvgComponentTransferType = (
    ctIdentity,
    ctTable,
    ctDiscrete,
    ctLinear,
    ctGamma
  );

  TSvgMorphologyOperator = (
    moErode,
    moDilate
  );

  { TSvgStitchTiles defines tile stitching options for feTurbulence }
  TSvgStitchTiles = (
    stNoStitch,
    stStitch
  );

  { TSvgTurbulenceType defines turbulence or fractal noise algorithm for feTurbulence }
  TSvgTurbulenceType = (
    ttTurbulence,
    ttFractalNoise
  );

  { TSvgChannelSelector defines RGBA channel selection for feDisplacementMap }
  TSvgChannelSelector = (
    csR,
    csG,
    csB,
    csA
  );

  { TSvgFeatureKeyword defines SVG 1.1/1.2 feature URIs for conditional processing evaluation }
  TSvgFeatureKeyword = (
    fkNone,
    fkSvg,                     // http://www.w3.org/TR/SVG11/feature#SVG
    fkSvgStatic,               // http://www.w3.org/TR/SVG11/feature#SVG-static
    fkCoreAttribute,           // http://www.w3.org/TR/SVG11/feature#CoreAttribute
    fkStructure,               // http://www.w3.org/TR/SVG11/feature#Structure
    fkBasicStructure,          // http://www.w3.org/TR/SVG11/feature#BasicStructure
    fkContainerAttribute,      // http://www.w3.org/TR/SVG11/feature#ContainerAttribute
    fkConditionalProcessing,   // http://www.w3.org/TR/SVG11/feature#ConditionalProcessing
    fkImage,                   // http://www.w3.org/TR/SVG11/feature#Image
    fkStyle,                   // http://www.w3.org/TR/SVG11/feature#Style
    fkViewportAttribute,       // http://www.w3.org/TR/SVG11/feature#ViewportAttribute
    fkShape,                   // http://www.w3.org/TR/SVG11/feature#Shape
    fkGradient,                // http://www.w3.org/TR/SVG11/feature#Gradient
    fkPattern,                 // http://www.w3.org/TR/SVG11/feature#Pattern
    fkClip,                    // http://www.w3.org/TR/SVG11/feature#Clip
    fkMask,                    // http://www.w3.org/TR/SVG11/feature#Mask
    fkFilter,                  // http://www.w3.org/TR/SVG11/feature#Filter
    fkBasicFilter,             // http://www.w3.org/TR/SVG11/feature#BasicFilter
    fkMarker,                  // http://www.w3.org/TR/SVG11/feature#Marker
    fkExtensibility,           // http://www.w3.org/TR/SVG11/feature#Extensibility
    fkOrgW3cSvgStatic,         // org.w3c.svg.static
    fkSvg12Static              // http://www.w3.org/Graphics/SVG/feature/1.2/#SVG-static
  );
  
  TSvgTextAnchor = (
    taStart,
    taMiddle,
    taEnd
  );

  TSvgTextAlignmentHorizontal = (
    taHorNone,
    taHorLeft,
    taHorCenter,
    taHorRight,
    taHorJustify
  );

//------------------------------------------------------------------------------
//
//      String to enum value
//
//------------------------------------------------------------------------------
function ParseSvgBlendMode(const AName: TValuePUtf8Char): TSvgBlendMode; overload;
function ParseSvgBlendMode(const AName: UTF8String): TSvgBlendMode; overload;
function ParseSvgIsolation(const AName: TValuePUtf8Char): TSvgIsolation; overload;
function ParseSvgIsolation(const AName: UTF8String): TSvgIsolation; overload;
function ParseSvgCompositeOperator(const AName: TValuePUtf8Char): TSvgCompositeOperator; overload;
function ParseSvgCompositeOperator(const AName: UTF8String): TSvgCompositeOperator; overload;
function ParseSvgFeColorMatrixType(const AName: TValuePUtf8Char): TSvgFeColorMatrixType; overload;
function ParseSvgFeColorMatrixType(const AName: UTF8String): TSvgFeColorMatrixType; overload;
function ParseSvgComponentTransferType(const AName: TValuePUtf8Char): TSvgComponentTransferType; overload;
function ParseSvgComponentTransferType(const AName: UTF8String): TSvgComponentTransferType; overload;
function ParseSvgMorphologyOperator(const AName: TValuePUtf8Char): TSvgMorphologyOperator; overload;
function ParseSvgMorphologyOperator(const AName: UTF8String): TSvgMorphologyOperator; overload;
function ParseSvgStitchTiles(const AName: TValuePUtf8Char): TSvgStitchTiles; overload;
function ParseSvgStitchTiles(const AName: UTF8String): TSvgStitchTiles; overload;
function ParseSvgTurbulenceType(const AName: TValuePUtf8Char): TSvgTurbulenceType; overload;
function ParseSvgTurbulenceType(const AName: UTF8String): TSvgTurbulenceType; overload;
function ParseSvgChannelSelector(const AName: TValuePUtf8Char): TSvgChannelSelector; overload;
function ParseSvgChannelSelector(const AName: UTF8String): TSvgChannelSelector; overload;
function ParseSvgTextAnchor(const AName: TValuePUtf8Char): TSvgTextAnchor; overload;
function ParseSvgTextAnchor(const AName: UTF8String): TSvgTextAnchor; overload; deprecated;


//------------------------------------------------------------------------------
//
//      Enum value to string (for debug)
//
//------------------------------------------------------------------------------
function SvgBlendModeToString(AMode: TSvgBlendMode): string;
function SvgIsolationToString(AIsolation: TSvgIsolation): string;
function SvgCompositeOperatorToString(AOp: TSvgCompositeOperator): string;
function FeColorMatrixTypeToString(AType: TSvgFeColorMatrixType): string;
function ComponentTransferTypeToString(AType: TSvgComponentTransferType): string;
function MorphologyOperatorToString(AOp: TSvgMorphologyOperator): string;
function StitchTilesToString(AStitch: TSvgStitchTiles): string;
function TurbulenceTypeToString(AType: TSvgTurbulenceType): string;
function ChannelSelectorToString(ASelector: TSvgChannelSelector): string;
function SvgTextAnchorToString(AAnchor: TSvgTextAnchor): string;


//------------------------------------------------------------------------------
//
//      Conditional Processing Helpers
//
//------------------------------------------------------------------------------
// IsSupportedSvgFeature tests whether a feature URI is supported using
// TSvgKeywordDictionary.
//------------------------------------------------------------------------------
function IsSupportedSvgFeature(const AFeatureURI: TValuePUtf8Char): Boolean; overload;
{$if defined(UNIT_TEST)}
function IsSupportedSvgFeature(const AFeatureURI: UTF8String): Boolean; overload;
{$ifend}

// System language tag management and RFC 3066 / BCP 47 language matching
function GetSystemLanguage: UTF8String;
procedure SetSystemLanguage(const ALang: UTF8String);
function MatchLanguageTag(ASystemLang, ALangRange: TValuePUtf8Char): Boolean; overload;
{$if defined(UNIT_TEST)}
function MatchLanguageTag(const ASystemLang, ALangRange: UTF8String): Boolean; overload;
{$ifend}

var
  // GlobalSystemLanguage: Current system language.
  // Tested again the 'systemlanguage' switch condition.
  GlobalSystemLanguage: UTF8String = 'en';


//------------------------------------------------------------------------------
//
//      Misc. globals
//
//------------------------------------------------------------------------------
var
  // FormatSettings with '.' decimal separator
  SvgFormatSettings: TFormatSettings;


//------------------------------------------------------------------------------
//
//      TSvgLength
//
//------------------------------------------------------------------------------
// SVG length unit type
//------------------------------------------------------------------------------
type
  TSvgUnitType = (
    suPx,
    suPt,
    suMm,
    suCm,
    suIn,
    suPc,
    suPercent,
    suEm,
    suEx
  );

  TSvgLength = record
    Value: Single;
    UnitType: TSvgUnitType;
    function ToPixels(const ARefSize: Single = 100.0; const ADpi: Single = 96.0; const AFontSize: Single = 16.0): Single;
    class function Create(AValue: Single; AUnit: TSvgUnitType = suPx): TSvgLength; static;
    class function Parse(const AStr: TValuePUtf8Char): TSvgLength; overload; static;
{$if defined(UNIT_TEST)}
    class function Parse(const AStr: UTF8String): TSvgLength; overload; static; deprecated;
{$ifend}
    class function ParseAndSkip(var AStr: TValuePUtf8Char): TSvgLength; overload; static;
    class function Parse(AStr: TValuePointer): TSvgLength; overload; static;
  end;


//------------------------------------------------------------------------------
//
//      TSvgColor
//
//------------------------------------------------------------------------------
// SVG color
//------------------------------------------------------------------------------
type
  TSvgColor = record
  public type
    TColorKind = (ckUnspecified, ckNone, ckColor, ckCurrentColor);
  private
    FColor: TColor32;
    FKind: TColorKind;
  private
    function GetIsSet: Boolean; inline;
    function GetIsNone: Boolean; inline;
    function GetIsCurrentColor: Boolean; inline;
    procedure SetColor(const Value: TColor32); inline;
    function GetIsColor: Boolean; inline;
    function GetIsVisible: Boolean; inline;
  public
    // Constructors
    class function Create(AColor: TColor32): TSvgColor; static;
    class function None: TSvgColor; static;
    class function CurrentColor: TSvgColor; static;
    class function Unset: TSvgColor; static;


    class function Parse(AColorStr: TValuePUtf8Char): TSvgColor; overload; static;
{$if defined(UNIT_TEST)}
    class function Parse(const AStr: UTF8String): TSvgColor; overload; static;
{$ifend}

    function ToColor32: TColor32;

    property Color: TColor32 read FColor write SetColor;
    property Kind: TColorKind read FKind;

    property IsSet: Boolean read GetIsSet;
    property IsColor: Boolean read GetIsColor;
    property IsNone: Boolean read GetIsNone;
    property IsVisible: Boolean read GetIsVisible;
    property IsCurrentColor: Boolean read GetIsCurrentColor;
  end;


//------------------------------------------------------------------------------
//
//      TSvgAlign
//
//------------------------------------------------------------------------------
// SVG alignment
//------------------------------------------------------------------------------
type
  TSvgAlign = (
    saNone,
    saXMinYMin,
    saXMidYMin,
    saXMaxYMin,
    saXMinYMid,
    saXMidYMid,
    saXMaxYMid,
    saXMinYMax,
    saXMidYMax,
    saXMaxYMax
  );

  TSvgMeetOrSlice = (
    msMeet,
    msSlice
  );


//------------------------------------------------------------------------------
//
//      TSvgPreserveAspectRatio
//
//------------------------------------------------------------------------------
// SVG aspect ratio
//------------------------------------------------------------------------------
type
  TSvgPreserveAspectRatio = record
    Align: TSvgAlign;
    MeetOrSlice: TSvgMeetOrSlice;
    class function Default: TSvgPreserveAspectRatio; static;
    class function Parse(AValue: TValuePUtf8Char): TSvgPreserveAspectRatio; static;
  end;


//------------------------------------------------------------------------------
//
//      TSvgViewBox
//
//------------------------------------------------------------------------------
// SVG view box
//------------------------------------------------------------------------------
type
  TSvgViewBox = record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
    IsDefined: Boolean;
    function IsValid: Boolean;
    class function Create(AX, AY, AWidth, AHeight: Single): TSvgViewBox; static;
    class function Parse(AValue: TValuePUtf8Char): TSvgViewBox; static;
    function GetTransform(const ATargetRect: TFloatRect; const AAspect: TSvgPreserveAspectRatio): TFloatMatrix;
  end;


//------------------------------------------------------------------------------
//
//      TSvgKeywordDictionary<T>
//
//------------------------------------------------------------------------------
// Generic Keywork to Value dictionary
//------------------------------------------------------------------------------
type
  TSvgKeywordDictionary<T> = record
  private type
    TKeyword = record
      Keyword: UTF8String;
      Value: T;
    end;
  private
    FLengths: TArray<integer>;          // Array of Keywords[] indices, indexed
                                        // by keyword length-1.
                                        // 0 means no keywords for that length.

    FKeywords: TArray<TArray<TKeyword>>;// Sparse array of keyword arrays.
                                        // All keywords within a keyword array
                                        // has the same length and are sorted
                                        // alphabetically, case insensitive.

  private
    class function CompareKeywordPointers(P1, P2: PUtf8Char; Len: Integer): Integer; static;
  public
    // Add a keyword to the dictionary
    procedure Add(const AKeyword: UTF8String; AValue: T);
    // Lookup a keyword in the dictionary. Return Value if found, Default(T) otherwise.
    function Lookup(const AKeyword: TValuePUtf8Char; var AValue: T): boolean; overload;
    function Lookup(const AKeyword: TValuePUtf8Char): T; overload;
  end;


//------------------------------------------------------------------------------
//
//      TFloatMatrixHelper
//
//------------------------------------------------------------------------------
//  Note that while TFloatMatrixHelper CAN be used independently:
//
//    var Helper: TFloatMatrixHelper;
//    Helper.Matrix := IdentityMatrix;
//    Helper.Scale(a, b);
//    ...
//
//  it is actually meant to be used as a type cast helper for TFloatMatrix:
//
//    var MatrixA: TFloatMatrix;
//    var MatrixB: TFloatMatrix;
//    ...
//    TFloatMatrixHelper(MatrixA).Scale(a, b);
//    MatrixB := MatrixB * TFloatMatrixHelper(MatrixA);
//    ...
//------------------------------------------------------------------------------
type
  TFloatMatrixHelper = record
    // Beware that Matrix is Row-Major!
    Matrix: TFloatMatrix;

    // Note: operator Multiply(Left, Right) internally calls Mult(Right, Left) so
    // the result matches normal algebraic expectations: matRes := matA x matB
    class operator Multiply(const Left, Right: TFloatMatrixHelper): TFloatMatrixHelper; overload; {$IF DEFINED(StaticOperators)}static;{$IFEND}
    class operator Multiply(const Left: TFloatMatrixHelper; const Right: TFloatMatrix): TFloatMatrixHelper; overload; {$IF DEFINED(StaticOperators)}static;{$IFEND}
    class operator Multiply(const Left: TFloatMatrix; const Right: TFloatMatrixHelper): TFloatMatrix; overload; {$IF DEFINED(StaticOperators)}static;{$IFEND}

    procedure Translate(Dx, Dy: TFloat);
    procedure Rotate(Alpha: TFloat); overload;
    procedure Rotate(Cx, Cy, Alpha: TFloat); overload;
    procedure Skew(Fx, Fy: TFloat);
    procedure Scale(Sx, Sy: TFloat); overload;
    procedure Scale(Value: TFloat); overload;
    function TransformPoint(const P: TFloatPoint): TFloatPoint;
    function IsIdentity: boolean;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      Blend modes
//
//------------------------------------------------------------------------------
type
  TBlendModeName = record
    Name: UTF8String;
    Value: TSvgBlendMode;
  end;

var
  SvgBlendModeDictionary: TSvgKeywordDictionary<TSvgBlendMode>;

const
  sBlendModes: array[0..11] of TBlendModeName = (
    (Name: ''; Value: bmNormal),
    (Name: 'multiply'; Value: bmMultiply),
    (Name: 'screen'; Value: bmScreen),
    (Name: 'overlay'; Value: bmOverlay),
    (Name: 'darken'; Value: bmDarken),
    (Name: 'lighten'; Value: bmLighten),
    (Name: 'color-dodge'; Value: bmColorDodge),
    (Name: 'color-burn'; Value: bmColorBurn),
    (Name: 'hard-light'; Value: bmHardLight),
    (Name: 'soft-light'; Value: bmSoftLight),
    (Name: 'difference'; Value: bmDifference),
    (Name: 'exclusion'; Value: bmExclusion)
  );

function ParseSvgBlendMode(const AName: UTF8String): TSvgBlendMode;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgBlendMode(Name);
end;

function ParseSvgBlendMode(const AName: TValuePUtf8Char): TSvgBlendMode;
begin
  if (not SvgBlendModeDictionary.Lookup(AName, Result)) then
    Result := bmNormal;
end;

function SvgBlendModeToString(AMode: TSvgBlendMode): string;
begin
  case AMode of
    bmMultiply: Result := 'multiply';
    bmScreen: Result := 'screen';
    bmOverlay: Result := 'overlay';
    bmDarken: Result := 'darken';
    bmLighten: Result := 'lighten';
    bmColorDodge: Result := 'color-dodge';
    bmColorBurn: Result := 'color-burn';
    bmHardLight: Result := 'hard-light';
    bmSoftLight: Result := 'soft-light';
    bmDifference: Result := 'difference';
    bmExclusion: Result := 'exclusion';
  else
    Result := 'normal';
  end;
end;


//------------------------------------------------------------------------------
//
//      Composite operators
//
//------------------------------------------------------------------------------
type
  TCompositeOperatorName = record
    Name: UTF8String;
    Value: TSvgCompositeOperator;
  end;

var
  SvgCompositeOperatorDictionary: TSvgKeywordDictionary<TSvgCompositeOperator>;

const
  sCompositeOperators: array[0..6] of TCompositeOperatorName = (
    (Name: 'over'; Value: coOver),
    (Name: 'in'; Value: coIn),
    (Name: 'out'; Value: coOut),
    (Name: 'atop'; Value: coAtop),
    (Name: 'xor'; Value: coXor),
    (Name: 'arithmetic'; Value: coArithmetic),
    (Name: 'lighter'; Value: coLighter)
  );

function ParseSvgCompositeOperator(const AName: UTF8String): TSvgCompositeOperator;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgCompositeOperator(Name);
end;

function ParseSvgCompositeOperator(const AName: TValuePUtf8Char): TSvgCompositeOperator;
begin
  if (not SvgCompositeOperatorDictionary.Lookup(AName, Result)) then
    Result := coOver;
end;

function SvgCompositeOperatorToString(AOp: TSvgCompositeOperator): string;
begin
  case AOp of
    coIn: Result := 'in';
    coOut: Result := 'out';
    coAtop: Result := 'atop';
    coXor: Result := 'xor';
    coArithmetic: Result := 'arithmetic';
    coLighter: Result := 'lighter';
  else
    Result := 'over';
  end;
end;


//------------------------------------------------------------------------------
//
//      Isolation
//
//------------------------------------------------------------------------------
function ParseSvgIsolation(const AName: UTF8String): TSvgIsolation;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgIsolation(Name);
end;

function ParseSvgIsolation(const AName: TValuePUtf8Char): TSvgIsolation;
var
  Name: TValuePUtf8Char;
begin
  Name := AName;
  Name.Trim;
  if (Name.StartsText('isolate')) then
    Result := isoIsolate
  else
    Result := isoAuto;
end;

function SvgIsolationToString(AIsolation: TSvgIsolation): string;
begin
  case AIsolation of
    isoIsolate: Result := 'isolate';
  else
    Result := 'auto';
  end;
end;


//------------------------------------------------------------------------------
//
//      feColorMatrix
//
//------------------------------------------------------------------------------
function ParseSvgFeColorMatrixType(const AName: UTF8String): TSvgFeColorMatrixType;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgFeColorMatrixType(Name);
end;

function ParseSvgFeColorMatrixType(const AName: TValuePUtf8Char): TSvgFeColorMatrixType;
begin
  Result := cmMatrix;
  case AName.Len of
    8:
      if AName.CompareText('saturate') then
        Result := cmSaturate;

    9:
      if AName.CompareText('huerotate') then
        Result := cmHueRotate;

    16:
      if AName.CompareText('luminancetoalpha') then
        Result := cmLuminanceToAlpha;
  end;
end;

function FeColorMatrixTypeToString(AType: TSvgFeColorMatrixType): string;
begin
  case AType of
    cmSaturate: Result := 'saturate';
    cmHueRotate: Result := 'hueRotate';
    cmLuminanceToAlpha: Result := 'luminanceToAlpha';
  else
    Result := 'matrix';
  end;
end;


//------------------------------------------------------------------------------
//
//      feMorphology
//
//------------------------------------------------------------------------------
function ParseSvgMorphologyOperator(const AName: UTF8String): TSvgMorphologyOperator;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgMorphologyOperator(Name);
end;

function ParseSvgMorphologyOperator(const AName: TValuePUtf8Char): TSvgMorphologyOperator;
begin
  Result := moErode;
  if AName.CompareText('dilate') then
    Result := moDilate;
end;

function MorphologyOperatorToString(AOp: TSvgMorphologyOperator): string;
begin
  case AOp of
    moDilate: Result := 'dilate';
  else
    Result := 'erode';
  end;
end;


//------------------------------------------------------------------------------
//
//      feTurbulence
//
//------------------------------------------------------------------------------
function ParseSvgStitchTiles(const AName: UTF8String): TSvgStitchTiles;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgStitchTiles(Name);
end;

function ParseSvgStitchTiles(const AName: TValuePUtf8Char): TSvgStitchTiles;
begin
  Result := stNoStitch;
  if AName.CompareText('stitch') then
    Result := stStitch;
end;

function StitchTilesToString(AStitch: TSvgStitchTiles): string;
begin
  case AStitch of
    stStitch: Result := 'stitch';
  else
    Result := 'noStitch';
  end;
end;

function ParseSvgTurbulenceType(const AName: UTF8String): TSvgTurbulenceType;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgTurbulenceType(Name);
end;

function ParseSvgTurbulenceType(const AName: TValuePUtf8Char): TSvgTurbulenceType;
begin
  Result := ttTurbulence;
  if AName.CompareText('fractalNoise') then
    Result := ttFractalNoise;
end;

function TurbulenceTypeToString(AType: TSvgTurbulenceType): string;
begin
  case AType of
    ttFractalNoise: Result := 'fractalNoise';
  else
    Result := 'turbulence';
  end;
end;


//------------------------------------------------------------------------------
//
//      feComponentTransfer
//
//------------------------------------------------------------------------------
function ParseSvgComponentTransferType(const AName: UTF8String): TSvgComponentTransferType;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgComponentTransferType(Name);
end;

function ParseSvgComponentTransferType(const AName: TValuePUtf8Char): TSvgComponentTransferType;
begin
  Result := ctIdentity;
  case AName.Len of
    5:
      if AName.CompareText('table') then
        Result := ctTable
      else
      if AName.CompareText('gamma') then
        Result := ctGamma;

    6:
      if AName.CompareText('linear') then
        Result := ctLinear;

    8:
      if AName.CompareText('discrete') then
        Result := ctDiscrete
      else
      if AName.CompareText('identity') then
        Result := ctIdentity;
  end;
end;

function ComponentTransferTypeToString(AType: TSvgComponentTransferType): string;
begin
  case AType of
    ctTable: Result := 'table';
    ctDiscrete: Result := 'discrete';
    ctLinear: Result := 'linear';
    ctGamma: Result := 'gamma';
  else
    Result := 'identity';
  end;
end;


//------------------------------------------------------------------------------
//
//      feDisplacementMap
//
//------------------------------------------------------------------------------
function ParseSvgChannelSelector(const AName: UTF8String): TSvgChannelSelector;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgChannelSelector(Name);
end;

function ParseSvgChannelSelector(const AName: TValuePUtf8Char): TSvgChannelSelector;
begin
  // SVG spec default for xChannelSelector and yChannelSelector is 'A'
  Result := csA;
  if AName.CompareText('R') then
    Result := csR
  else
  if AName.CompareText('G') then
    Result := csG
  else
  if AName.CompareText('B') then
    Result := csB
  else
  if AName.CompareText('A') then
    Result := csA;
end;

function ChannelSelectorToString(ASelector: TSvgChannelSelector): string;
begin
  case ASelector of
    csR: Result := 'R';
    csG: Result := 'G';
    csB: Result := 'B';
  else
    Result := 'A';
  end;
end;


//------------------------------------------------------------------------------
//
//      Feature Dictionary & Conditional Processing
//
//------------------------------------------------------------------------------
type
  TFeatureKeywordName = record
    Name: UTF8String;
    Value: TSvgFeatureKeyword;
  end;

var
  SvgFeatureKeywordDictionary: TSvgKeywordDictionary<TSvgFeatureKeyword>;

const
  sFeatureKeywords: array[0..20] of TFeatureKeywordName = (
    (Name: 'http://www.w3.org/TR/SVG11/feature#SVG'; Value: fkSvg),
    (Name: 'http://www.w3.org/TR/SVG11/feature#SVG-static'; Value: fkSvgStatic),
    (Name: 'http://www.w3.org/TR/SVG11/feature#CoreAttribute'; Value: fkCoreAttribute),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Structure'; Value: fkStructure),
    (Name: 'http://www.w3.org/TR/SVG11/feature#BasicStructure'; Value: fkBasicStructure),
    (Name: 'http://www.w3.org/TR/SVG11/feature#ContainerAttribute'; Value: fkContainerAttribute),
    (Name: 'http://www.w3.org/TR/SVG11/feature#ConditionalProcessing'; Value: fkConditionalProcessing),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Image'; Value: fkImage),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Style'; Value: fkStyle),
    (Name: 'http://www.w3.org/TR/SVG11/feature#ViewportAttribute'; Value: fkViewportAttribute),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Shape'; Value: fkShape),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Gradient'; Value: fkGradient),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Pattern'; Value: fkPattern),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Clip'; Value: fkClip),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Mask'; Value: fkMask),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Filter'; Value: fkFilter),
    (Name: 'http://www.w3.org/TR/SVG11/feature#BasicFilter'; Value: fkBasicFilter),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Marker'; Value: fkMarker),
    (Name: 'http://www.w3.org/TR/SVG11/feature#Extensibility'; Value: fkExtensibility),
    (Name: 'org.w3c.svg.static'; Value: fkOrgW3cSvgStatic),
    (Name: 'http://www.w3.org/Graphics/SVG/feature/1.2/#SVG-static'; Value: fkSvg12Static)
  );

function IsSupportedSvgFeature(const AFeatureURI: TValuePUtf8Char): Boolean;
var
  Val: TSvgFeatureKeyword;
begin
  if SvgFeatureKeywordDictionary.Lookup(AFeatureURI, Val) then
    Result := (Val <> fkNone)
  else
    Result := False;
end;

{$if defined(UNIT_TEST)}
function IsSupportedSvgFeature(const AFeatureURI: UTF8String): Boolean;
begin
  Result := IsSupportedSvgFeature(TValuePUtf8Char.FromString(AFeatureURI));
end;
{$ifend}

function GetSystemLanguage: UTF8String;
begin
  Result := GlobalSystemLanguage;
end;

procedure SetSystemLanguage(const ALang: UTF8String);
begin
  GlobalSystemLanguage := ALang;
end;

function MatchLanguageTag(ASystemLang, ALangRange: TValuePUtf8Char): Boolean;
begin
  ASystemLang.Trim;
  ALangRange.Trim;

  if (ALangRange.Len = 0) or (ASystemLang.Len = 0) or (ALangRange.Text^ = '*') or (ALangRange.CompareText(ASystemLang)) then
    Exit(True);

  // Range 'en' matches system tag 'en-US' (prefix check followed by '-')
  if (ASystemLang.Len > ALangRange.Len) and (ASystemLang.StartsText(ALangRange)) and
     (ASystemLang.Text[ALangRange.Len] = '-') then
    Exit(True);

  // Range 'en-US' matches system tag 'en' (system tag is prefix of range)
  if (ALangRange.Len > ASystemLang.Len) and (ALangRange.StartsText(ASystemLang)) and
     (ALangRange.Text[ASystemLang.Len] = '-') then
    Exit(True);

  Result := False;
end;

{$if defined(UNIT_TEST)}
function MatchLanguageTag(const ASystemLang, ALangRange: UTF8String): Boolean;
begin
  Result := MatchLanguageTag(TValuePUtf8Char.FromString(ASystemLang), TValuePUtf8Char.FromString(ALangRange));
end;
{$ifend}

//------------------------------------------------------------------------------
//
//      text-anchor
//
//------------------------------------------------------------------------------
function ParseSvgTextAnchor(const AName: UTF8String): TSvgTextAnchor;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgTextAnchor(Name);
end;

function ParseSvgTextAnchor(const AName: TValuePUtf8Char): TSvgTextAnchor;
begin
  if AName.CompareText('middle') then
    Result := taMiddle
  else
  if AName.CompareText('end') then
    Result := taEnd
  else
    Result := taStart;
end;

function SvgTextAnchorToString(AAnchor: TSvgTextAnchor): string;
begin
  case AAnchor of
    taMiddle: Result := 'middle';
    taEnd: Result := 'end';
  else
    Result := 'start';
  end;
end;


//------------------------------------------------------------------------------
//
//      HSL color space
//
//------------------------------------------------------------------------------
function HSLtoRGB(H, S, L, A: Single): TColor32;
begin
  Result := GR32.HSLtoRGB(
    EnsureRange(H, 0.0, 1.0),
    EnsureRange(S, 0.0, 1.0),
    EnsureRange(L, 0.0, 1.0),
    Clamp(Round(A * 255.0))
  );
end;


//------------------------------------------------------------------------------
//
//      TSvgKeywordDictionary<T>
//
//------------------------------------------------------------------------------

// CompareKeywordPointers performs a case-insensitive UTF-8 comparison of two
// equal-length string buffers.
class function TSvgKeywordDictionary<T>.CompareKeywordPointers(P1, P2: PUtf8Char; Len: Integer): Integer;
var
  i: Integer;
  c1, c2: Byte;
begin
  for i := 0 to Len - 1 do
  begin
    c1 := Byte(P1[i]);
    c2 := Byte(P2[i]);
    if (c1 <> c2) then
    begin
      if (c1 in [65..90]) <> (c2 in [65..90]) then
        c1 := c1 xor $20;
      if (c1 <> c2) then
        Exit(Integer(c1) - Integer(c2));
    end;
  end;
  Result := 0;
end;

procedure TSvgKeywordDictionary<T>.Add(const AKeyword: UTF8String; AValue: T);
var
  Len: integer;
  Index: integer;
  L, H, Mid: integer;
  Cmp: integer;
  NewItem: TKeyword;
begin
  Len := Length(AKeyword);
  if (Len = 0) then
    exit;

  // Make room in length index.
  // Remember that the first entry in the index is for Length=1!
  if (Len > Length(FLengths)) then
  begin
    if (Len < 16) then
      SetLength(FLengths, 16)
    else
      SetLength(FLengths, Len * 2);
  end;

  // Get the keyword list for this length
  Index := FLengths[Len-1];

  // Do we have a list allocated for this length?
  if (Index = 0) then
  begin
    // Allocate a new keyword list
    Index := Length(FKeywords)+1; // 0=no entry, so first index is 1
    SetLength(FKeywords, Index);
    // Update the index
    FLengths[Len-1] := Index;
  end;

  Dec(Index); // Normalize index

  NewItem.Keyword := AKeyword;
  NewItem.Value := AValue;

  // Find insertion position using binary search to keep keywords sorted alphabetically
  L := 0;
  H := High(FKeywords[Index]);
  while (L <= H) do
  begin
    Mid := L + (H - L) div 2;
    Cmp := CompareKeywordPointers(PUtf8Char(AKeyword), PUtf8Char(FKeywords[Index, Mid].Keyword), Len);
    if (Cmp = 0) then
    begin
      FKeywords[Index, Mid].Value := AValue;
      exit;
    end;
    if (Cmp < 0) then
      H := Mid - 1
    else
      L := Mid + 1;
  end;

  // Insert the new keyword at sorted index L
  Insert(NewItem, FKeywords[Index], L);
end;

function TSvgKeywordDictionary<T>.Lookup(const AKeyword: TValuePUtf8Char): T;
begin
  if (not Lookup(AKeyword, Result)) then
    Result := Default(T);
end;

function TSvgKeywordDictionary<T>.Lookup(const AKeyword: TValuePUtf8Char; var AValue: T): boolean;
var
  Index: integer;
  L, H, Mid: integer;
  Cmp: integer;
begin
  Result := False;
  if (AKeyword.Len = 0) then
    exit;

  if (AKeyword.Len > Length(FLengths)) then
    exit;

  // Get the keyword list for this length
  Index := FLengths[AKeyword.Len-1];

  // Do we have a list allocated for this length?
  if (Index = 0) then
    exit;

  Dec(Index); // Normalize index

  // Find the keyword using binary search in the sorted keyword list
  L := 0;
  H := High(FKeywords[Index]);
  while (L <= H) do
  begin
    Mid := L + (H - L) div 2;
    Cmp := CompareKeywordPointers(AKeyword.Text, PUtf8Char(FKeywords[Index, Mid].Keyword), AKeyword.Len);
    if (Cmp = 0) then
    begin
      AValue := FKeywords[Index, Mid].Value;
      Exit(True);
    end else
    if (Cmp < 0) then
      H := Mid - 1
    else
      L := Mid + 1;
  end;
end;


//------------------------------------------------------------------------------
//
//      SvgColorNameToColor
//
//------------------------------------------------------------------------------
type
  TColorName = record
    Name: UTF8String;
    Value: TColor32;
  end;

var
  SvgColorNameDictionary: TSvgKeywordDictionary<TColor32>;

const
  sColorNames: array[0..147] of TColorName = (
    (Name: 'red'; Value: clRed32),
    (Name: 'tan'; Value: clTan32),
    (Name: 'aqua'; Value: clAqua32),
    (Name: 'blue'; Value: clBlue32),
    (Name: 'cyan'; Value: clAqua32),
    (Name: 'gold'; Value: clGold32),
    (Name: 'gray'; Value: clGray32),
    (Name: 'grey'; Value: clGrey32),
    (Name: 'lime'; Value: clLime32),
    (Name: 'navy'; Value: clNavy32),
    (Name: 'peru'; Value: clPeru32),
    (Name: 'pink'; Value: clPink32),
    (Name: 'plum'; Value: clPlum32),
    (Name: 'snow'; Value: clSnow32),
    (Name: 'teal'; Value: clTeal32),
    (Name: 'azure'; Value: clAzure32),
    (Name: 'beige'; Value: clBeige32),
    (Name: 'black'; Value: clBlack32),
    (Name: 'brown'; Value: clBrown32),
    (Name: 'coral'; Value: clCoral32),
    (Name: 'green'; Value: clGreen32),
    (Name: 'ivory'; Value: clIvory32),
    (Name: 'khaki'; Value: clKhaki32),
    (Name: 'linen'; Value: clLinen32),
    (Name: 'olive'; Value: clOlive32),
    (Name: 'wheat'; Value: clWheat32),
    (Name: 'white'; Value: clWhite32),
    (Name: 'bisque'; Value: clBisque32),
    (Name: 'indigo'; Value: clIndigo32),
    (Name: 'maroon'; Value: clMaroon32),
    (Name: 'orange'; Value: clOrange32),
    (Name: 'orchid'; Value: clOrchid32),
    (Name: 'purple'; Value: clPurple32),
    (Name: 'salmon'; Value: clSalmon32),
    (Name: 'sienna'; Value: clSienna32),
    (Name: 'silver'; Value: clSilver32),
    (Name: 'tomato'; Value: clTomato32),
    (Name: 'violet'; Value: clViolet32),
    (Name: 'yellow'; Value: clYellow32),
    (Name: 'crimson'; Value: clCrimson32),
    (Name: 'darkred'; Value: clDarkRed32),
    (Name: 'dimgray'; Value: clDimGray32),
    (Name: 'dimgrey'; Value: clDimGray32),
    (Name: 'fuchsia'; Value: clFuchsia32),
    (Name: 'hotpink'; Value: clHotPink32),
    (Name: 'magenta'; Value: clFuchsia32),
    (Name: 'oldlace'; Value: clOldLace32),
    (Name: 'skyblue'; Value: clSkyblue32),
    (Name: 'thistle'; Value: clThistle32),
    (Name: 'cornsilk'; Value: clCornSilk32),
    (Name: 'darkblue'; Value: clDarkBlue32),
    (Name: 'darkcyan'; Value: clDarkCyan32),
    (Name: 'darkgray'; Value: clDarkGray32),
    (Name: 'darkgrey'; Value: clDarkGrey32),
    (Name: 'deeppink'; Value: clDeepPink32),
    (Name: 'honeydew'; Value: clHoneyDew32),
    (Name: 'lavender'; Value: clLavender32),
    (Name: 'moccasin'; Value: clMoccasin32),
    (Name: 'seagreen'; Value: clSeaGreen32),
    (Name: 'seashell'; Value: clSeaShell32),
    (Name: 'aliceblue'; Value: clAliceBlue32),
    (Name: 'burlywood'; Value: clBurlyWood32),
    (Name: 'cadetblue'; Value: clCadetblue32),
    (Name: 'chocolate'; Value: clChocolate32),
    (Name: 'darkgreen'; Value: clDarkGreen32),
    (Name: 'darkkhaki'; Value: clDarkKhaki32),
    (Name: 'firebrick'; Value: clFireBrick32),
    (Name: 'gainsboro'; Value: clGainsBoro32),
    (Name: 'goldenrod'; Value: clGoldenRod32),
    (Name: 'indianred'; Value: clIndianRed32),
    (Name: 'lawngreen'; Value: clLawnGreen32),
    (Name: 'lightblue'; Value: clLightBlue32),
    (Name: 'lightcyan'; Value: clLightCyan32),
    (Name: 'lightgray'; Value: clLightGray32),
    (Name: 'lightgrey'; Value: clLightGrey32),
    (Name: 'lightpink'; Value: clLightPink32),
    (Name: 'limegreen'; Value: clLimeGreen32),
    (Name: 'mintcream'; Value: clMintCream32),
    (Name: 'mistyrose'; Value: clMistyRose32),
    (Name: 'olivedrab'; Value: clOliveDrab32),
    (Name: 'orangered'; Value: clOrangeRed32),
    (Name: 'palegreen'; Value: clPaleGreen32),
    (Name: 'peachpuff'; Value: clPeachPuff32),
    (Name: 'rosybrown'; Value: clRosyBrown32),
    (Name: 'royalblue'; Value: clRoyalBlue32),
    (Name: 'slateblue'; Value: clSlateBlue32),
    (Name: 'slategray'; Value: clSlateGray32),
    (Name: 'slategrey'; Value: clSlateGrey32),
    (Name: 'steelblue'; Value: clSteelblue32),
    (Name: 'turquoise'; Value: clTurquoise32),
    (Name: 'aquamarine'; Value: clAquamarine32),
    (Name: 'blueviolet'; Value: clBlueViolet32),
    (Name: 'chartreuse'; Value: clChartReuse32),
    (Name: 'darkorange'; Value: clDarkOrange32),
    (Name: 'darkorchid'; Value: clDarkOrchid32),
    (Name: 'darksalmon'; Value: clDarkSalmon32),
    (Name: 'darkviolet'; Value: clDarkViolet32),
    (Name: 'dodgerblue'; Value: clDodgerBlue32),
    (Name: 'ghostwhite'; Value: clGhostWhite32),
    (Name: 'lightcoral'; Value: clLightCoral32),
    (Name: 'lightgreen'; Value: clLightGreen32),
    (Name: 'mediumblue'; Value: clMediumBlue32),
    (Name: 'papayawhip'; Value: clPapayaWhip32),
    (Name: 'powderblue'; Value: clPowderBlue32),
    (Name: 'sandybrown'; Value: clSandyBrown32),
    (Name: 'whitesmoke'; Value: clWhitesmoke32),
    (Name: 'darkmagenta'; Value: clDarkMagenta32),
    (Name: 'deepskyblue'; Value: clDeepSkyBlue32),
    (Name: 'floralwhite'; Value: clFloralWhite32),
    (Name: 'forestgreen'; Value: clForestGreen32),
    (Name: 'greenyellow'; Value: clGreenYellow32),
    (Name: 'lightsalmon'; Value: clLightSalmon32),
    (Name: 'lightyellow'; Value: clLightYellow32),
    (Name: 'navajowhite'; Value: clNavajoWhite32),
    (Name: 'saddlebrown'; Value: clSaddleBrown32),
    (Name: 'springgreen'; Value: clSpringgreen32),
    (Name: 'transparent'; Value: clNone32),
    (Name: 'yellowgreen'; Value: clYellowgreen32),
    (Name: 'antiquewhite'; Value: clAntiqueWhite32),
    (Name: 'darkseagreen'; Value: clDarkSeaGreen32),
    (Name: 'lemonchiffon'; Value: clLemonChiffon32),
    (Name: 'lightskyblue'; Value: clLightSkyblue32),
    (Name: 'mediumorchid'; Value: clMediumOrchid32),
    (Name: 'mediumpurple'; Value: clMediumPurple32),
    (Name: 'midnightblue'; Value: clMidnightBlue32),
    (Name: 'darkgoldenrod'; Value: clDarkGoldenRod32),
    (Name: 'darkslateblue'; Value: clDarkSlateBlue32),
    (Name: 'darkslategray'; Value: clDarkSlateGray32),
    (Name: 'darkslategrey'; Value: clDarkSlateGrey32),
    (Name: 'darkturquoise'; Value: clDarkTurquoise32),
    (Name: 'lavenderblush'; Value: clLavenderBlush32),
    (Name: 'lightseagreen'; Value: clLightSeagreen32),
    (Name: 'palegoldenrod'; Value: clPaleGoldenRod32),
    (Name: 'paleturquoise'; Value: clPaleTurquoise32),
    (Name: 'palevioletred'; Value: clPaleVioletred32),
    (Name: 'blanchedalmond'; Value: clBlancheDalmond32),
    (Name: 'cornflowerblue'; Value: clCornFlowerBlue32),
    (Name: 'darkolivegreen'; Value: clDarkOliveGreen32),
    (Name: 'lightslategray'; Value: clLightSlategray32),
    (Name: 'lightslategrey'; Value: clLightSlategrey32),
    (Name: 'lightsteelblue'; Value: clLightSteelblue32),
    (Name: 'mediumseagreen'; Value: clMediumSeaGreen32),
    (Name: 'mediumslateblue'; Value: clMediumSlateBlue32),
    (Name: 'mediumturquoise'; Value: clMediumTurquoise32),
    (Name: 'mediumvioletred'; Value: clMediumVioletRed32),
    (Name: 'mediumaquamarine'; Value: clMediumAquamarine32),
    (Name: 'mediumspringgreen'; Value: clMediumSpringGreen32),
    (Name: 'lightgoldenrodyellow'; Value: clLightGoldenRodYellow32)
  );

function SvgColorNameToColor(const AName: TValuePUtf8Char; ADefault: TColor32): TColor32;
begin
  if (not SvgColorNameDictionary.Lookup(AName, Result)) then
    Result := ADefault;
end;


//------------------------------------------------------------------------------
//
//      TSvgLength
//
//------------------------------------------------------------------------------
class function TSvgLength.Create(AValue: Single; AUnit: TSvgUnitType): TSvgLength;
begin
  Result.Value := AValue;
  Result.UnitType := AUnit;
end;

function TSvgLength.ToPixels(const ARefSize, ADpi, AFontSize: Single): Single;
begin
  case UnitType of
    suPx: Result := Value;
    suPt: Result := Value * (ADpi / 72.0);
    suMm: Result := Value * (ADpi / 25.4);
    suCm: Result := Value * (ADpi / 2.54);
    suIn: Result := Value * ADpi;
    suPc: Result := Value * (ADpi / 6.0);
    suPercent: Result := Value * 0.01 * ARefSize;
    suEm: Result := Value * AFontSize;
    suEx: Result := Value * AFontSize * 0.5;
  else
    Result := Value;
  end;
end;

class function TSvgLength.Parse(AStr: TValuePointer): TSvgLength;
begin
  Result := Parse(TValuePUtf8Char(AStr));
end;

class function TSvgLength.ParseAndSkip(var AStr: TValuePUtf8Char): TSvgLength;
var
  Temp: TValuePUtf8Char;
  Suffix: TValuePUtf8Char;
  Value: Double;
begin
  Result.UnitType := suPx;
  Result.Value := 0;

  // Skip leading spaces
  AStr.Trim;
  if (AStr.Len = 0) then
    exit;

  // Find end of value = start of suffix
  Suffix := AStr;
  while (Suffix.Len > 0) and ((Suffix.Text^ in ['0'..'9', '.', '-', '+']) or ((Suffix.Text^ in ['e', 'E']) and not (Suffix.Text[1] in ['m', 'M', 'x', 'X']))) do
    Suffix.Skip;

  // Convert value
  Temp := AStr;
  Temp.Len := AStr.Len - Suffix.Len;
  if Temp.TryToFloat(Value) then
    Result.Value := Value;

  AStr.Skip(AStr.Len - Suffix.Len);

  // Skip leading spaces
  AStr.Trim;

  // Match and skip AStr
  case AStr.Text^ of
    #0: Result.UnitType := suPx;

    '%': begin Result.UnitType := suPercent; AStr.Skip; end;

    'c':
      case AStr.Text[1] of
        'm': begin Result.UnitType := suCm; AStr.Skip(2); end;
      end;

    'e':
      case AStr.Text[1] of
        'm': begin Result.UnitType := suEm; AStr.Skip(2); end;
        'x': begin Result.UnitType := suEx; AStr.Skip(2); end;
      end;

    'm':
      case AStr.Text[1] of
        'm': begin Result.UnitType := suMm; AStr.Skip(2); end;
      end;

    'i':
      case AStr.Text[1] of
        'n': begin Result.UnitType := suIn; AStr.Skip(2); end;
      end;

    'p':
      case AStr.Text[1] of
        'c': begin Result.UnitType := suPc; AStr.Skip(2); end;
        't': begin Result.UnitType := suPt; AStr.Skip(2); end;
        'x': begin Result.UnitType := suPx; AStr.Skip(2); end;
      end;
  end;

  // Skip trailing spaces
  AStr.Trim;
end;

class function TSvgLength.Parse(const AStr: TValuePUtf8Char): TSvgLength;
var
  Temp: TValuePUtf8Char;
begin
  Temp := AStr;
  Result := ParseAndSkip(Temp);
end;

{$if defined(UNIT_TEST)}
class function TSvgLength.Parse(const AStr: UTF8String): TSvgLength;
begin
  Result := Parse(TValuePUtf8Char.FromString(AStr));
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      TSvgColor
//
//------------------------------------------------------------------------------
class function TSvgColor.Create(AColor: TColor32): TSvgColor;
begin
  Result.FColor := AColor;
  Result.FKind := ckColor;
end;

class function TSvgColor.None: TSvgColor;
begin
  Result.FColor := clNone32;
  Result.FKind := ckNone;
end;


class function TSvgColor.CurrentColor: TSvgColor;
begin
  Result.FColor := clBlack32;
  Result.FKind := ckCurrentColor;
end;

class function TSvgColor.Unset: TSvgColor;
begin
  Result.FColor := clNone32;
  Result.FKind := ckUnspecified;
end;

function TSvgColor.GetIsColor: Boolean;
begin
  Result := (Kind = ckColor);
end;

function TSvgColor.GetIsCurrentColor: Boolean;
begin
  Result := (Kind = ckCurrentColor);
end;

function TSvgColor.GetIsNone: Boolean;
begin
  Result := (Kind = ckNone);
end;

function TSvgColor.GetIsSet: Boolean;
begin
  Result := (Kind <> ckUnspecified);
end;

function TSvgColor.GetIsVisible: Boolean;
begin
  Result := (Kind = ckCurrentColor) or ((Kind = ckColor) and (TColor32Entry(FColor).A <> 0));
end;

class function TSvgColor.Parse(AColorStr: TValuePUtf8Char): TSvgColor;

  function ParseHexByte(Twins: boolean = False): Byte;
  begin
    // First digit
    case AColorStr.Text^ of
      '0'..'9': Result := Ord(AColorStr.Text^) - Ord('0');
      'a'..'f': Result := Ord(AColorStr.Text^) - Ord('a') + 10;
      'A'..'F': Result := Ord(AColorStr.Text^) - Ord('A') + 10;
    else
      Exit(0);
    end;
    AColorStr.Skip;

    if Twins then
    begin
      Result := Result shl 4 + Result;
      exit;
    end;

    if (AColorStr.Len = 0) then
      exit;
    // Optional second digit
    case AColorStr.Text^ of
      '0'..'9': Result := Result shl 4 + Ord(AColorStr.Text^) - Ord('0');
      'a'..'f': Result := Result shl 4 + Ord(AColorStr.Text^) - Ord('a') + 10;
      'A'..'F': Result := Result shl 4 + Ord(AColorStr.Text^) - Ord('A') + 10;
    else
      exit;
    end;
    AColorStr.Skip;
  end;

type
  TColorRGB = record
    r, g, b, a: Byte;
  end;

  TColorHSL = record
    h, s, l, a: Single;
  end;
var
  Value: TValuePUtf8Char;
  HasComponentColor: boolean;
  HasComponentAlpha: boolean;
  n: Single;
  ColorRGB: TColorRGB;
  ColorHSL: TColorHSL;
begin
  // Trim
  AColorStr.Trim;

  if (AColorStr.Len = 0) then
    Exit(None);

  if (AColorStr.CompareText('none')) then
    Exit(None);

  if (AColorStr.CompareText('currentcolor')) then
    Exit(CurrentColor);

  if (AColorStr.Text^ = '#') then
  begin
    AColorStr.Skip;

    case AColorStr.Len of
      3: // RGB -> RRGGBB
        begin
          ColorRGB.r := ParseHexByte(True);
          ColorRGB.g := ParseHexByte(True);
          ColorRGB.b := ParseHexByte(True);
          Exit(Create(Color32(ColorRGB.r, ColorRGB.g, ColorRGB.b, 255)));
        end;

      4: // RGBA -> RRGGBBAA
        begin
          ColorRGB.r := ParseHexByte(True);
          ColorRGB.g := ParseHexByte(True);
          ColorRGB.b := ParseHexByte(True);
          ColorRGB.a := ParseHexByte(True);
          Exit(Create(Color32(ColorRGB.r, ColorRGB.g, ColorRGB.b, ColorRGB.a)));
        end;

      6: // RRGGBB
        begin
          ColorRGB.r := ParseHexByte;
          ColorRGB.g := ParseHexByte;
          ColorRGB.b := ParseHexByte;
          Exit(Create(Color32(ColorRGB.r, ColorRGB.g, ColorRGB.b, 255)));
        end;

      8: // RRGGBBAA
        begin
          ColorRGB.r := ParseHexByte;
          ColorRGB.g := ParseHexByte;
          ColorRGB.b := ParseHexByte;
          ColorRGB.a := ParseHexByte;
          Exit(Create(Color32(ColorRGB.r, ColorRGB.g, ColorRGB.b, ColorRGB.a)));
        end;
    end;
    Exit(None); // Invalid
  end;

  // Smallest possible 'rgb' string is rgb(0,0,0) -> length=10
  // Must be rgb(...) or rgba(...)
  if (AColorStr.Len >= 10) then
  begin
    ColorRGB.a := 255;

    (*
    ** RGB
    *)
    if (AColorStr.StartsText('rgb', True)) then
    begin
      HasComponentAlpha := AColorStr.StartsText('a(', True);
      HasComponentColor := HasComponentAlpha or AColorStr.StartsText('(', True);

      if (HasComponentColor) then
      begin
        ColorRGB.r := 0;
        ColorRGB.g := 0;
        ColorRGB.b := 0;
        ColorRGB.a := 255;
        // R
        Value := AColorStr.Split([' ', ','], True);
        Value.Trim;
        if (not Value.TryToPercentOf(n, 255, True)) then
          Exit(None);
        ColorRGB.r := Clamp(Round(n));

        // G
        Value := AColorStr.Split([' ', ','], True);
        Value.Trim;
        if (not Value.TryToPercentOf(n, 255, True)) then
          Exit(None);
        ColorRGB.g := Clamp(Round(n));

        // B
        Value := AColorStr.Split([' ', ','], True);
        Value.Trim;
        if (not Value.TryToPercentOf(n, 255, True)) then
          Exit(None);
        ColorRGB.b := Clamp(Round(n));

        // A
        Value := AColorStr.Split([' ', ',', '/'], True);
        Value.Trim;
        if (Value.Len > 0) and (Value.TryToPercent(n)) then
          ColorRGB.a := Clamp(Round(n * 255));

        Result := Create(Color32(ColorRGB.r, ColorRGB.g, ColorRGB.b, ColorRGB.a));
        exit;
      end;

    end else
    (*
    ** HSL
    *)
    if (AColorStr.StartsText('hsl', True)) then
    begin
      HasComponentAlpha := AColorStr.StartsText('a(', True);
      HasComponentColor := HasComponentAlpha or AColorStr.StartsText('(', True);

      if (HasComponentColor) then
      begin
        ColorHSL.h := 0;
        ColorHSL.s := 1;
        ColorHSL.l := 1;
        ColorHSL.a := 1;
        // H
        Value := AColorStr.Split([' ', ','], True);
        Value.Trim;
        if (Value.TryToFloat(ColorHSL.h, True)) then
        begin
          Value.Trim;
          case Value.Len of
            3:
              if (Value.CompareText('rad')) then
                ColorHSL.h := RadToDeg(ColorHSL.h) / 360
              else
                ColorHSL.h := ColorHSL.h / 360; // Default is degrees

            4:
              if (Value.CompareText('grad')) then
                ColorHSL.h := ColorHSL.h / 400
              else
              if (Value.CompareText('turn')) then
                ColorHSL.h := ColorHSL.h
              else
                ColorHSL.h := ColorHSL.h / 360; // Default is degrees
          else
            ColorHSL.h := ColorHSL.h / 360; // Default is degrees
          end;
        end else
          Exit(None);

        // S
        Value := AColorStr.Split([' ', ','], True);
        Value.Trim;
        if (not Value.TryToFloat(ColorHSL.s)) then
          Exit(None);
        ColorHSL.s := ColorHSL.s * 0.01; // Value is always percent (0..100) but '%' is optional

        // L
        Value := AColorStr.Split([' ', ','], True);
        Value.Trim;
        if (not Value.TryToFloat(ColorHSL.l)) then
          Exit(None);
        ColorHSL.l := ColorHSL.l * 0.01;

        // A
        Value := AColorStr.Split([' ', ',', '/'], True);
        Value.Trim;
        if (Value.TryToPercent(n)) then
          ColorHSL.a := n;

        Result := Create(HSLtoRGB(ColorHSL.h, ColorHSL.s, ColorHSL.l, ColorHSL.a));
        exit;
      end;

    end;
  end;

  Result := Create(SvgColorNameToColor(AColorStr, clBlack32));
end;

procedure TSvgColor.SetColor(const Value: TColor32);
begin
  FColor := Value;
end;

function TSvgColor.ToColor32: TColor32;
begin
  if (IsNone) then
    Result := clNone32
  else
    Result := FColor;
end;

{$if defined(UNIT_TEST)}
class function TSvgColor.Parse(const AStr: UTF8String): TSvgColor;
begin
  Result := Parse(TValuePUtf8Char.FromString(AStr));
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      TSvgPreserveAspectRatio
//
//------------------------------------------------------------------------------
class function TSvgPreserveAspectRatio.Default: TSvgPreserveAspectRatio;
begin
  Result.Align := saXMidYMid;
  Result.MeetOrSlice := msMeet;
end;

class function TSvgPreserveAspectRatio.Parse(AValue: TValuePUtf8Char): TSvgPreserveAspectRatio;
var
  Align: TValuePUtf8Char;
begin
  Result := Default;

  AValue.Trim;
  if (AValue.Len = 0) then
    exit;

  Align := AValue.Split(' ', True);

  case Align.Text^ of
    'n', 'N':
      if Align.CompareText('none') then
        Result.Align := saNone;

    'x', 'X':
      if (Align.Len = 8) then
      begin
        case Align.Text[2] of
          'i': // xmi*
            case Align.Text[7] of
              'd', 'D':
                if Align.CompareText('xminymid') then
                  Result.Align := saXMinYMid
                else
                if Align.CompareText('xmidymid') then
                  Result.Align := saXMidYMid;
              'n', 'N':
                if Align.CompareText('xminymin') then
                  Result.Align := saXMinYMin
                else
                if Align.CompareText('xmidymin') then
                  Result.Align := saXMidYMin;
              'x', 'X':
                if Align.CompareText('xminymax') then
                  Result.Align := saXMinYMax
                else
                if Align.CompareText('xmidymax') then
                  Result.Align := saXMidYMax;
            end;
          'a': //xmaxym??
            case Align.Text[7] of
              'd', 'D':
                if Align.CompareText('xmaxymid') then
                  Result.Align := saXMaxYMid;
              'n', 'N':
                if Align.CompareText('xmaxymin') then
                  Result.Align := saXMaxYMin;
              'x', 'X':
                if Align.CompareText('xmaxymax') then
                  Result.Align := saXMaxYMax;
            end;
        end;

      end;
  end;

  if AValue.CompareText('slice') then
    Result.MeetOrSlice := msSlice
  else
  if AValue.CompareText('meet') then
    Result.MeetOrSlice := msMeet;
end;

{ TFloatMatrixHelper }

class operator TFloatMatrixHelper.Multiply(const Left, Right: TFloatMatrixHelper): TFloatMatrixHelper;
begin
  Result.Matrix := Mult(Right.Matrix, Left.Matrix);
end;

class operator TFloatMatrixHelper.Multiply(const Left: TFloatMatrixHelper; const Right: TFloatMatrix): TFloatMatrixHelper;
begin
  Result.Matrix := Mult(Right, Left.Matrix);
end;

function TFloatMatrixHelper.IsIdentity: boolean;
begin
  Result := IsIdentityMatrix(Matrix);
end;

class operator TFloatMatrixHelper.Multiply(const Left: TFloatMatrix; const Right: TFloatMatrixHelper): TFloatMatrix;
begin
  Result := Mult(Right.Matrix, Left);
end;

procedure TFloatMatrixHelper.Rotate(Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);

  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := -S;
  M[0, 1] := S;   M[1, 1] := C;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Rotate(Cx, Cy, Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  if (Cx <> 0) or (Cy <> 0) then
    Translate(-Cx, -Cy);

  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);

  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := -S;
  M[0, 1] := S;   M[1, 1] := C;
  Matrix := Mult(M, Matrix);

  if (Cx <> 0) or (Cy <> 0) then
    Translate(Cx, Cy);
end;

procedure TFloatMatrixHelper.Scale(Sx, Sy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := Sx;
  M[1, 1] := Sy;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Scale(Value: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := Value;
  M[1, 1] := Value;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Skew(Fx, Fy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Translate(Dx, Dy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[2, 0] := Dx;
  M[2, 1] := Dy;
  Matrix := Mult(M, Matrix);
end;

function TFloatMatrixHelper.TransformPoint(const P: TFloatPoint): TFloatPoint;
var
  vIn, vOut: TVector3f;
begin
  vIn[0] := P.X;
  vIn[1] := P.Y;
  vIn[2] := 1.0;
  vOut := VectorTransform(Matrix, vIn);
  Result.X := vOut[0];
  Result.Y := vOut[1];
end;


//------------------------------------------------------------------------------
//
//      TSvgViewBox
//
//------------------------------------------------------------------------------
class function TSvgViewBox.Create(AX, AY, AWidth, AHeight: Single): TSvgViewBox;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.IsDefined := True;
end;

function TSvgViewBox.IsValid: Boolean;
begin
  Result := IsDefined and (Width > 0) and (Height > 0);
end;

class function TSvgViewBox.Parse(AValue: TValuePUtf8Char): TSvgViewBox;
type
  TValues = array[0..3] of Single;
var
  Value: TValuePUtf8Char;
  Values: TValues;
  i: Integer;
begin
  Result.IsDefined := False;
  Values := Default(TValues);
  AValue.Trim;
  i := 0;
  while (AValue.Len > 0) and (i <= High(Values)) do
  begin
    Value := AValue.Split([' ', ','], True);
    if (Value.Len = 0) then
      exit;
    if (not Value.TryToFloat(Values[i], True)) then
      exit;
    Inc(i);
  end;
  Result := Create(Values[0], Values[1], Values[2], Values[3]);
end;

function TSvgViewBox.GetTransform(const ATargetRect: TFloatRect; const AAspect: TSvgPreserveAspectRatio): TFloatMatrix;
var
  targetW, targetH: Single;
  sx, sy, scale: Single;
  tx, ty: Single;
  helper: TFloatMatrixHelper;
begin
  if not IsValid then
    Exit(IdentityMatrix);

  targetW := ATargetRect.Right - ATargetRect.Left;
  targetH := ATargetRect.Bottom - ATargetRect.Top;

  if (targetW <= 0) or (targetH <= 0) then
    Exit(IdentityMatrix);

  sx := targetW / Width;
  sy := targetH / Height;

  if AAspect.Align = saNone then
  begin
    helper.Matrix := IdentityMatrix;
    helper.Translate(-X, -Y);
    helper.Scale(sx, sy);
    helper.Translate(ATargetRect.Left, ATargetRect.Top);
    Exit(helper.Matrix);
  end;

  if AAspect.MeetOrSlice = msMeet then
    scale := Min(sx, sy)
  else
    scale := Max(sx, sy);

  tx := ATargetRect.Left - X * scale;
  ty := ATargetRect.Top - Y * scale;

  case AAspect.Align of
    saXMidYMin, saXMidYMid, saXMidYMax:
      tx := tx + (targetW - Width * scale) * 0.5;
    saXMaxYMin, saXMaxYMid, saXMaxYMax:
      tx := tx + (targetW - Width * scale);
  end;

  case AAspect.Align of
    saXMinYMid, saXMidYMid, saXMaxYMid:
      ty := ty + (targetH - Height * scale) * 0.5;
    saXMinYMax, saXMidYMax, saXMaxYMax:
      ty := ty + (targetH - Height * scale);
  end;

  helper.Matrix := IdentityMatrix;
  helper.Scale(scale, scale);
  helper.Translate(tx, ty);
  Result := helper.Matrix;
end;

procedure InitializeKeywordDictionaries;
var
  i: integer;
begin
  for i := 0 to High(sColorNames) do
    SvgColorNameDictionary.Add(sColorNames[i].Name, sColorNames[i].Value);

  for i := 0 to High(sBlendModes) do
    SvgBlendModeDictionary.Add(sBlendModes[i].Name, sBlendModes[i].Value);

  for i := 0 to High(sCompositeOperators) do
    SvgCompositeOperatorDictionary.Add(sCompositeOperators[i].Name, sCompositeOperators[i].Value);

  for i := 0 to High(sFeatureKeywords) do
    SvgFeatureKeywordDictionary.Add(sFeatureKeywords[i].Name, sFeatureKeywords[i].Value);
end;

initialization
{$IFDEF FPC}
  SvgFormatSettings := DefaultFormatSettings;
{$ELSE}
  SvgFormatSettings := FormatSettings;
{$ENDIF}
  SvgFormatSettings.DecimalSeparator := '.';

  InitializeKeywordDictionaries;
end.
