unit GR32.SVG.Tree;

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
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Generics.Collections,
  GR32, GR32_Transforms, GR32_Polygons, GR32_VectorUtils,
  GR32.SVG.Types,
  GR32.SVG.Utf8;

type
  TSvgNodeClass = class of TSvgNode;

  TSvgSpreadMethod = (smPad, smReflect, smRepeat);
  TSvgGradientUnits = (guObjectBoundingBox, guUserSpaceOnUse);

  TSvgGradientStop = record
    Offset: Single;
    Color: TSvgColor;
    Opacity: Single;
    class function Create(AOffset: Single; AColor: TSvgColor; AOpacity: Single = 1.0): TSvgGradientStop; static;
  end;

  TSvgFill = record
  private type
    TSvgFillProperties = set of (fpColor, fpOpacity, fpFillRule);
  private
    FSpecified: TSvgFillProperties;
    FColor: TSvgColor;
    FOpacity: Single;
    FFillRule: TPolyFillMode;
    FUrl: string;
    procedure SetColor(const Value: TSvgColor);
    procedure SetFillRule(const Value: TPolyFillMode);
    procedure SetOpacity(const Value: Single);
    procedure SetUrl(const Value: string);
  public

    procedure ApplySpecified(var ADest: TSvgFill);

    property Color: TSvgColor read FColor write SetColor;
    property Opacity: Single read FOpacity write SetOpacity;
    property FillRule: TPolyFillMode read FFillRule write SetFillRule;
    property Url: string read FUrl write SetUrl;

    class function Default: TSvgFill; static;
  end;


  TSvgStroke = record
  private type
    TSvgStrokeProperties = set of (spColor, spWidth, spOpacity, spJoinStyle, spEndStyle, spMiterLimit, spDashArray, spDashOffset);
  private
    FSpecified: TSvgStrokeProperties;
    FColor: TSvgColor;
    FWidth: TSvgLength;
    FOpacity: Single;
    FJoinStyle: TJoinStyle;
    FEndStyle: TEndStyle;
    FMiterLimit: Single;
    FDashArray: TArrayOfFloat;
    FDashOffset: Single;
    FUrl: string;
  private
    procedure SetColor(const Value: TSvgColor);
    procedure SetDashArray(const Value: TArrayOfFloat);
    procedure SetDashOffset(const Value: Single);
    procedure SetEndStyle(const Value: TEndStyle);
    procedure SetJoinStyle(const Value: TJoinStyle);
    procedure SetMiterLimit(const Value: Single);
    procedure SetOpacity(const Value: Single);
    procedure SetUrl(const Value: string);
    procedure SetWidth(const Value: TSvgLength);
  public
    procedure ApplySpecified(var ADest: TSvgStroke);

    property Color: TSvgColor read FColor write SetColor;
    property Width: TSvgLength read FWidth write SetWidth;
    property Opacity: Single read FOpacity write SetOpacity;
    property JoinStyle: TJoinStyle read FJoinStyle write SetJoinStyle;
    property EndStyle: TEndStyle read FEndStyle write SetEndStyle;
    property MiterLimit: Single read FMiterLimit write SetMiterLimit;
    property DashArray: TArrayOfFloat read FDashArray write SetDashArray;
    property DashOffset: Single read FDashOffset write SetDashOffset;
    property Url: string read FUrl write SetUrl;

    class function Default: TSvgStroke; static;
  end;

  TSvgNode = class(TObject)
  private
    FID: string;
    FCssClassName: string;
    FStyleAttr: string; // Stores raw inline style="..." string for deferred cascade evaluation
    FTransform: TFloatMatrix;
    FVisible: Boolean;
    FParent: TSvgNode;
    FFill: TSvgFill;
    FStroke: TSvgStroke;
    FResolving: Boolean;
  protected
    function GetIsRenderable: Boolean; virtual;
    function DumpNode(Indent: Integer = 0): string; virtual;
    function DumpChildren(Indent: Integer = 0): string; virtual;
  public
    constructor Create(AParent: TSvgNode = nil); virtual;
    destructor Destroy; override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; virtual;
    function FindNodeById(const AId: string): TSvgNode; virtual;
    procedure Render(ACanvas: TObject); virtual;
    function GetObjectBoundingBox: TFloatRect; virtual;
    procedure ParseAttribute(const AName, AValue: string); virtual;
    procedure ParseStyleAttribute(const AStyleStr: string);
    function Dump(Indent: Integer = 0): string;
    property ID: string read FID write FID;
    property CssClassName: string read FCssClassName write FCssClassName;
    property StyleAttr: string read FStyleAttr write FStyleAttr;
    property Transform: TFloatMatrix read FTransform write FTransform;
    property Visible: Boolean read FVisible write FVisible;
    property Parent: TSvgNode read FParent write FParent;
    property Fill: TSvgFill read FFill write FFill;
    property Stroke: TSvgStroke read FStroke write FStroke;
    property IsRenderable: Boolean read GetIsRenderable;
  end;

  TSvgGroupNode = class(TSvgNode)
  private
    FChildren: TObjectList<TSvgNode>;
    FOpacity: Single;
    FClipPathID: string;
    FMaskID: string;
  protected
    function DumpNode(Indent: Integer = 0): string; override;
    function DumpChildren(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    destructor Destroy; override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    function FindNodeById(const AId: string): TSvgNode; override;
    function GetObjectBoundingBox: TFloatRect; override;
    procedure AddChild(AChild: TSvgNode);
    procedure ParseAttribute(const AName, AValue: string); override;
    property Children: TObjectList<TSvgNode> read FChildren;
    property Opacity: Single read FOpacity write FOpacity;
    property ClipPathID: string read FClipPathID write FClipPathID;
    property MaskID: string read FMaskID write FMaskID;
  end;

  TSvgGradientNode = class(TSvgGroupNode)
  private
    FStops: TList<TSvgGradientStop>;
    FSpreadMethod: TSvgSpreadMethod;
    FGradientUnits: TSvgGradientUnits;
    FHref: string;
  protected
    function GetIsRenderable: Boolean; override;
    function DumpNode(Indent: Integer = 0): string; override;
    function DumpChildren(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    destructor Destroy; override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure AddStop(const AStop: TSvgGradientStop);
    procedure InheritFrom(ParentGradient: TSvgGradientNode); virtual;
    procedure ParseAttribute(const AName, AValue: string); override;
    property Stops: TList<TSvgGradientStop> read FStops;
    property SpreadMethod: TSvgSpreadMethod read FSpreadMethod write FSpreadMethod;
    property GradientUnits: TSvgGradientUnits read FGradientUnits write FGradientUnits;
    property Href: string read FHref write FHref;
  end;

  TSvgLinearGradientNode = class(TSvgGradientNode)
  private
    FX1: TSvgLength;
    FY1: TSvgLength;
    FX2: TSvgLength;
    FY2: TSvgLength;
  protected
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure InheritFrom(ParentGradient: TSvgGradientNode); override;
    procedure ParseAttribute(const AName, AValue: string); override;
    property X1: TSvgLength read FX1 write FX1;
    property Y1: TSvgLength read FY1 write FY1;
    property X2: TSvgLength read FX2 write FX2;
    property Y2: TSvgLength read FY2 write FY2;
  end;

  TSvgRadialGradientNode = class(TSvgGradientNode)
  private type
    TSvgRadialGradientProperties = set of (gpFocalX, gpFocalY);
  private
    FSpecified: TSvgRadialGradientProperties;
    FCx: TSvgLength;
    FCy: TSvgLength;
    FR: TSvgLength;
    FFx: TSvgLength;
    FFy: TSvgLength;
    function GetFx: TSvgLength;
    function GetFy: TSvgLength;
    procedure SetFx(const Value: TSvgLength);
    procedure SetFy(const Value: TSvgLength);
  protected
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;

    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure InheritFrom(ParentGradient: TSvgGradientNode); override;
    procedure ParseAttribute(const AName, AValue: string); override;

    property Cx: TSvgLength read FCx write FCx;
    property Cy: TSvgLength read FCy write FCy;
    property R: TSvgLength read FR write FR;
    // Focal point; Falls back to Center if not specified
    property Fx: TSvgLength read GetFx write SetFx;
    property Fy: TSvgLength read GetFy write SetFy;
  end;

  TSvgClipPathNode = class(TSvgGroupNode)
  private
    FClipPathUnits: TSvgGradientUnits;
  protected
    function GetIsRenderable: Boolean; override;
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure ParseAttribute(const AName, AValue: string); override;
    property ClipPathUnits: TSvgGradientUnits read FClipPathUnits write FClipPathUnits;
  end;

  TSvgMaskNode = class(TSvgGroupNode)
  private
    FX: TSvgLength;
    FY: TSvgLength;
    FWidth: TSvgLength;
    FHeight: TSvgLength;
    FMaskUnits: TSvgGradientUnits;
    FMaskContentUnits: TSvgGradientUnits;
  protected
    function GetIsRenderable: Boolean; override;
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure ParseAttribute(const AName, AValue: string); override;
    property X: TSvgLength read FX write FX;
    property Y: TSvgLength read FY write FY;
    property Width: TSvgLength read FWidth write FWidth;
    property Height: TSvgLength read FHeight write FHeight;
    property MaskUnits: TSvgGradientUnits read FMaskUnits write FMaskUnits;
    property MaskContentUnits: TSvgGradientUnits read FMaskContentUnits write FMaskContentUnits;
  end;

  TSvgPatternNode = class(TSvgGroupNode)
  private
    FX: TSvgLength;
    FY: TSvgLength;
    FWidth: TSvgLength;
    FHeight: TSvgLength;
    FPatternUnits: TSvgGradientUnits;
    FPatternContentUnits: TSvgGradientUnits;
    FPatternTransform: TFloatMatrix;
    FViewBox: TSvgViewBox;
    FPreserveAspectRatio: TSvgPreserveAspectRatio;
    FHref: string;
  protected
    function GetIsRenderable: Boolean; override;
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure InheritFrom(ParentPattern: TSvgPatternNode); virtual;
    procedure ParseAttribute(const AName, AValue: string); override;
    property X: TSvgLength read FX write FX;
    property Y: TSvgLength read FY write FY;
    property Width: TSvgLength read FWidth write FWidth;
    property Height: TSvgLength read FHeight write FHeight;
    property PatternUnits: TSvgGradientUnits read FPatternUnits write FPatternUnits;
    property PatternContentUnits: TSvgGradientUnits read FPatternContentUnits write FPatternContentUnits;
    property PatternTransform: TFloatMatrix read FPatternTransform write FPatternTransform;
    property ViewBox: TSvgViewBox read FViewBox write FViewBox;
    property PreserveAspectRatio: TSvgPreserveAspectRatio read FPreserveAspectRatio write FPreserveAspectRatio;
    property Href: string read FHref write FHref;
  end;

  TSvgDocumentNode = class(TSvgGroupNode)
  private
    FWidth: TSvgLength;
    FHeight: TSvgLength;
    FViewBox: TSvgViewBox;
    FPreserveAspectRatio: TSvgPreserveAspectRatio;
  protected
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure ResolveUseNodes;
    procedure ResolveGradients;
    procedure ResolvePatterns;
    procedure ParseAttribute(const AName, AValue: string); override;
    property Width: TSvgLength read FWidth write FWidth;
    property Height: TSvgLength read FHeight write FHeight;
    property ViewBox: TSvgViewBox read FViewBox write FViewBox;
    property PreserveAspectRatio: TSvgPreserveAspectRatio read FPreserveAspectRatio write FPreserveAspectRatio;
  end;

  TSvgPathNode = class(TSvgNode)
  private
    FPathData: TArrayOfArrayOfFloatPoint;
  protected
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    function GetObjectBoundingBox: TFloatRect; override;
    property PathData: TArrayOfArrayOfFloatPoint read FPathData write FPathData;
  end;

  TSvgDefsNode = class(TSvgGroupNode)
  protected
    function GetIsRenderable: Boolean; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
  end;

  TSvgUseNode = class(TSvgGroupNode)
  private
    FHref: string;
    FX: Single;
    FY: Single;
  protected
    function DumpNode(Indent: Integer = 0): string; override;
  public
    constructor Create(AParent: TSvgNode = nil); override;
    function Clone(AParent: TSvgNode = nil): TSvgNode; override;
    procedure ParseAttribute(const AName, AValue: string); override;
    property Href: string read FHref write FHref;
    property X: Single read FX write FX;
    property Y: Single read FY write FY;
  end;

// Primitive Shape Converters
function CreateRectPath(X, Y, Width, Height, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
function CreateCirclePath(Cx, Cy, Radius: Single): TArrayOfArrayOfFloatPoint;
function CreateEllipsePath(Cx, Cy, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
function CreateLinePath(X1, Y1, X2, Y2: Single): TArrayOfArrayOfFloatPoint;
function CreatePolylinePath(const APointsStr: string; AClosed: Boolean): TArrayOfArrayOfFloatPoint;

function ParseStrokeDashArray(const AStr: string): TArrayOfFloat;

// Transform Parser
function ParseSvgTransform(const AStr: string): TFloatMatrix;

// XML Parsing
function ParseSvgXml(AText: TValuePUtf8Char): TSvgDocumentNode; overload;
function ParseSvgXml(AText: TValuePUtf8Char; var AErrorMessage: string): TSvgDocumentNode; overload;
function ParseSvgXml(AText: PAnsiChar; TextLen: NativeInt): TSvgDocumentNode; overload;
function ParseSvgXml(AText: PAnsiChar; TextLen: NativeInt; var AErrorMessage: string): TSvgDocumentNode; overload;
function ParseSvgXml(const AText: UTF8String): TSvgDocumentNode; overload;
function ParseSvgXml(const AText: UTF8String; var AErrorMessage: string): TSvgDocumentNode; overload;

implementation

uses
  Types,
  Math,
  GR32_Paths,
  GR32.SVG.Path,
  GR32.SVG.Xml,
  GR32.SVG.Css;

type
  TSvgTagKeyword = (tagNone, tagSvg, tagG, tagUse, tagDefs, tagStop, tagMask, tagPath, tagRect, tagLine, tagStyle, tagCircle,
    tagLineargradient, tagRadialgradient, tagClippath, tagPattern, tagEllipse, tagPolyline, tagPolygon);

const
  sSvgTagKeywords: array[TSvgTagKeyword] of AnsiString = (
    '', 'Svg', 'G', 'Use', 'Defs', 'Stop', 'Mask', 'Path', 'Rect', 'Line', 'Style', 'Circle',
    'Lineargradient', 'Radialgradient', 'Clippath', 'Pattern', 'Ellipse', 'Polyline', 'Polygon'
  );

var
  SvgKeywordDictionary: TSvgKeywordDictionary<TSvgTagKeyword>;


type
  TSvgStopTagKeyword = (stopTagNone, stopTagOffset, stopTagStopColor, stopTagStopOpacity, stopTagStyle);

const
  sSvgStopTagKeywords: array[TSvgStopTagKeyword] of AnsiString = ('', 'offset', 'stop-color', 'stop-opacity', 'style');

var
  SvgStopTagDictionary: TSvgKeywordDictionary<TSvgStopTagKeyword>;

function FloatToString(Value: Single; const PostFix: string = ''): string;
begin
  Result := Format('%.3f', [Value], SvgFormatSettings);
  while (Result[Length(Result)] = '0') do
    SetLength(Result, Length(Result)-1);
  if (Result[Length(Result)] = '.') then
    SetLength(Result, Length(Result)-1);
  if (PostFix <> '') then
    Result := Result + PostFix;
end;

function SvgLengthToString(const L: TSvgLength): string;
begin
  case L.UnitType of
    suPercent: Result := FloatToString(L.Value, '%');
    suPt: Result := FloatToString(L.Value, 'pt');
    suMm: Result := FloatToString(L.Value, 'mm');
    suCm: Result := FloatToString(L.Value, 'cm');
    suIn: Result := FloatToString(L.Value, 'in');
    suPc: Result := FloatToString(L.Value, 'pc');
    suEm: Result := FloatToString(L.Value, 'em');
    suEx: Result := FloatToString(L.Value, 'ex');
  else
    Result := FloatToString(L.Value, 'px');
  end;
end;

function SvgColorToString(const C: TSvgColor): string;
begin
  if C.IsNone then
    Result := 'none'
  else
  if C.IsCurrentColor then
    Result := 'currentColor'
  else
    Result := '#' + IntToHex(C.Color, 8);
end;

function TSvgNode.DumpNode(Indent: Integer): string;
begin
  Result := StringOfChar(' ', Indent * 2) + ClassName;
  if FID <> '' then
    Result := Result + Format(' (id="%s")', [FID]);
end;

function TSvgNode.Dump(Indent: Integer): string;
begin
  Result := DumpNode(Indent);
  Result := Result + DumpChildren(Indent);
end;

function TSvgNode.DumpChildren(Indent: Integer): string;
begin
end;

function TSvgGroupNode.DumpNode(Indent: Integer): string;
begin
  Result := inherited DumpNode(Indent);
  if FOpacity < 1.0 then
    Result := Result + Format(' (opacity=%s)', [FloatToString(FOpacity)]);
end;

function TSvgGroupNode.DumpChildren(Indent: Integer): string;
var
  i: Integer;
begin
  Result := Result + inherited DumpChildren(Indent);
  for i := 0 to FChildren.Count - 1 do
    Result := Result + sLineBreak + FChildren[i].Dump(Indent + 1);
end;

function TSvgDocumentNode.DumpNode(Indent: Integer): string;
begin
  Result := inherited DumpNode(Indent);
  Result := Result + Format(' (width=%s, height=%s)', [SvgLengthToString(FWidth), SvgLengthToString(FHeight)]);
end;

function TSvgGradientNode.DumpChildren(Indent: Integer): string;
var
  indentStr: string;
  i: Integer;
  stop: TSvgGradientStop;
begin
  indentStr := StringOfChar(' ', (Indent + 1) * 2);
  for i := 0 to FStops.Count - 1 do
  begin
    stop := FStops[i];
    Result := Result + sLineBreak + indentStr +
      Format('TSvgGradientStop (offset=%s, color=%s, opacity=%s)',
        [FloatToString(stop.Offset), SvgColorToString(stop.Color), FloatToString(stop.Opacity)]);
  end;
end;

function TSvgGradientNode.DumpNode(Indent: Integer): string;
var
  spreadStr, unitsStr: string;
begin
  Result := inherited DumpNode(Indent);
  case FSpreadMethod of
    smReflect: spreadStr := 'smReflect';
    smRepeat: spreadStr := 'smRepeat';
  else
    spreadStr := 'smPad';
  end;
  case FGradientUnits of
    guUserSpaceOnUse: unitsStr := 'guUserSpaceOnUse';
  else
    unitsStr := 'guObjectBoundingBox';
  end;
  Result := Result + Format(' (spreadMethod=%s, units=%s)', [spreadStr, unitsStr]);
end;

function TSvgLinearGradientNode.DumpNode(Indent: Integer): string;
begin
  Result := inherited DumpNode(Indent);
  Result := Result + Format(' (x1=%s, y1=%s, x2=%s, y2=%s)',
    [SvgLengthToString(FX1), SvgLengthToString(FY1), SvgLengthToString(FX2), SvgLengthToString(FY2)]);
end;

function TSvgRadialGradientNode.DumpNode(Indent: Integer): string;
begin
  Result := inherited DumpNode(Indent);
  Result := Result + Format(' (cx=%s, cy=%s, r=%s, fx=%s, fy=%s)',
    [SvgLengthToString(FCx), SvgLengthToString(FCy), SvgLengthToString(FR), SvgLengthToString(Fx), SvgLengthToString(Fy)]);
end;

function TSvgPathNode.DumpNode(Indent: Integer): string;
begin
  Result := inherited DumpNode(Indent);
  if not FFill.Color.IsNone then
    Result := Result + Format(' (fill=%s)', [SvgColorToString(FFill.Color)]);
  if FFill.Url <> '' then
    Result := Result + Format(' (fillUrl=%s)', [FFill.Url]);
  if not FStroke.Color.IsNone then
    Result := Result + Format(' (stroke=%s)', [SvgColorToString(FStroke.Color)]);
end;

function TSvgUseNode.DumpNode(Indent: Integer): string;
begin
  Result := inherited DumpNode(Indent);
  if FHref <> '' then
    Result := Result + Format(' (href="%s")', [FHref]);
end;

function TSvgClipPathNode.DumpNode(Indent: Integer): string;
var
  unitsStr: string;
begin
  Result := inherited DumpNode(Indent);
  case FClipPathUnits of
    guObjectBoundingBox: unitsStr := 'guObjectBoundingBox';
  else
    unitsStr := 'guUserSpaceOnUse';
  end;
  Result := Result + Format(' (clipPathUnits=%s)', [unitsStr]);
end;

function TSvgMaskNode.DumpNode(Indent: Integer): string;
var
  maskUnitsStr, contentUnitsStr: string;
begin
  Result := inherited DumpNode(Indent);
  case FMaskUnits of
    guUserSpaceOnUse: maskUnitsStr := 'guUserSpaceOnUse';
  else
    maskUnitsStr := 'guObjectBoundingBox';
  end;
  case FMaskContentUnits of
    guObjectBoundingBox: contentUnitsStr := 'guObjectBoundingBox';
  else
    contentUnitsStr := 'guUserSpaceOnUse';
  end;
  Result := Result + Format(' (x=%s, y=%s, width=%s, height=%s, maskUnits=%s, maskContentUnits=%s)',
    [SvgLengthToString(FX), SvgLengthToString(FY), SvgLengthToString(FWidth), SvgLengthToString(FHeight), maskUnitsStr, contentUnitsStr]);
end;

function TSvgPatternNode.DumpNode(Indent: Integer): string;
var
  patUnitsStr, contentUnitsStr: string;
begin
  Result := inherited DumpNode(Indent);
  case FPatternUnits of
    guUserSpaceOnUse: patUnitsStr := 'guUserSpaceOnUse';
  else
    patUnitsStr := 'guObjectBoundingBox';
  end;
  case FPatternContentUnits of
    guObjectBoundingBox: contentUnitsStr := 'guObjectBoundingBox';
  else
    contentUnitsStr := 'guUserSpaceOnUse';
  end;
  Result := Result + Format(' (x=%s, y=%s, width=%s, height=%s, patternUnits=%s, patternContentUnits=%s)',
    [SvgLengthToString(FX), SvgLengthToString(FY), SvgLengthToString(FWidth), SvgLengthToString(FHeight), patUnitsStr, contentUnitsStr]);
  if FHref <> '' then
    Result := Result + ' (href="' + FHref + '")';
end;



{ TSvgGradientStop }

class function TSvgGradientStop.Create(AOffset: Single; AColor: TSvgColor; AOpacity: Single): TSvgGradientStop;
begin
  Result.Offset := EnsureRange(AOffset, 0.0, 1.0);
  Result.Color := AColor;
  Result.Opacity := EnsureRange(AOpacity, 0.0, 1.0);
end;

{ TSvgFill }

procedure TSvgFill.ApplySpecified(var ADest: TSvgFill);
begin
  if fpColor in FSpecified then
  begin
    ADest.Color := FColor;
    ADest.Url := FUrl;
  end;

  if fpOpacity in FSpecified then
    ADest.Opacity := FOpacity;

  if fpFillRule in FSpecified then
    ADest.FillRule := FFillRule;
end;

class function TSvgFill.Default: TSvgFill;
begin
  Result.FColor := TSvgColor.Create(clBlack32);
  Result.FOpacity := 1.0;
  Result.FFillRule := pfWinding;
  Result.FUrl := '';
  Result.FSpecified := [];
end;

procedure TSvgFill.SetColor(const Value: TSvgColor);
begin
  FColor := Value;
  Include(FSpecified, fpColor);
end;

procedure TSvgFill.SetFillRule(const Value: TPolyFillMode);
begin
  FFillRule := Value;
  Include(FSpecified, fpFillRule);
end;

procedure TSvgFill.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
  Include(FSpecified, fpOpacity);
end;

procedure TSvgFill.SetUrl(const Value: string);
begin
  FUrl := Value;
  Include(FSpecified, fpColor);
end;

{ TSvgStroke }

procedure TSvgStroke.ApplySpecified(var ADest: TSvgStroke);
begin
  if (spColor in FSpecified) then
  begin
    ADest.Color := FColor;
    ADest.Url := FUrl;
  end;

  if (spWidth in FSpecified) then
    ADest.Width := FWidth;

  if (spOpacity in FSpecified) then
    ADest.Opacity := FOpacity;

  if (spJoinStyle in FSpecified) then
    ADest.JoinStyle := FJoinStyle;

  if (spEndStyle in FSpecified) then
    ADest.EndStyle := FEndStyle;

  if (spMiterLimit in FSpecified) then
    ADest.MiterLimit := FMiterLimit;

  if (spDashArray in FSpecified) then
    ADest.DashArray := FDashArray;

  if (spDashOffset in FSpecified) then
    ADest.DashOffset := FDashOffset;
end;

class function TSvgStroke.Default: TSvgStroke;
begin
  Result.FColor := TSvgColor.None;
  Result.FWidth := TSvgLength.Create(1.0, suPx);
  Result.FOpacity := 1.0;
  Result.FJoinStyle := jsMiter;
  Result.FEndStyle := esButt;
  Result.FMiterLimit := 4.0;
  Result.FDashArray := nil;
  Result.FDashOffset := 0.0;
  Result.FUrl := '';
  Result.FSpecified := [];
end;

procedure TSvgStroke.SetColor(const Value: TSvgColor);
begin
  FColor := Value;
  Include(FSpecified, spColor);
end;

procedure TSvgStroke.SetDashArray(const Value: TArrayOfFloat);
begin
  FDashArray := Value;
  Include(FSpecified, spDashArray);
end;

procedure TSvgStroke.SetDashOffset(const Value: Single);
begin
  FDashOffset := Value;
  Include(FSpecified, spDashOffset);
end;

procedure TSvgStroke.SetEndStyle(const Value: TEndStyle);
begin
  FEndStyle := Value;
  Include(FSpecified, spEndStyle);
end;

procedure TSvgStroke.SetJoinStyle(const Value: TJoinStyle);
begin
  FJoinStyle := Value;
  Include(FSpecified, spJoinStyle);
end;

procedure TSvgStroke.SetMiterLimit(const Value: Single);
begin
  FMiterLimit := Value;
  Include(FSpecified, spMiterLimit);
end;

procedure TSvgStroke.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
  Include(FSpecified, spOpacity);
end;

procedure TSvgStroke.SetUrl(const Value: string);
begin
  FUrl := Value;
  Include(FSpecified, spColor);
end;

procedure TSvgStroke.SetWidth(const Value: TSvgLength);
begin
  FWidth := Value;
  Include(FSpecified, spWidth);
end;

{ TSvgNode }

function TSvgNode.GetIsRenderable: Boolean;
begin
  Result := True;
end;

constructor TSvgNode.Create(AParent: TSvgNode);
begin
  inherited Create;
  FParent := AParent;
  FTransform := IdentityMatrix;
  FVisible := True;
  FCssClassName := '';
  FResolving := False;
  if AParent <> nil then
  begin
    FFill := AParent.Fill;
    FStroke := AParent.Stroke;
    FFill.FSpecified := [];
    FStroke.FSpecified := [];
  end else
  begin
    FFill := TSvgFill.Default;
    FStroke := TSvgStroke.Default;
  end;
end;

destructor TSvgNode.Destroy;
begin
  inherited Destroy;
end;

function TSvgNode.Clone(AParent: TSvgNode): TSvgNode;
begin
  Result := TSvgNodeClass(ClassType).Create(AParent);
  Result.FID := FID;
  Result.FCssClassName := FCssClassName;
  Result.FStyleAttr := FStyleAttr;
  Result.FTransform := FTransform;
  Result.FVisible := FVisible;
  Result.FResolving := False;

  FFill.ApplySpecified(Result.FFill);
  FStroke.ApplySpecified(Result.FStroke);
end;

function TSvgNode.GetObjectBoundingBox: TFloatRect;
begin
  { Calculates the tight axis-aligned bounding box of the node in local untransformed coordinates.
    Base TSvgNode implementation returns an empty rectangle (0,0,0,0). }
  Result := FloatRect(0, 0, 0, 0);
end;

function TSvgNode.FindNodeById(const AId: string): TSvgNode;
var
  cleanId: string;
begin
  cleanId := AId;
  if (cleanId <> '') and (cleanId[1] = '#') then
    Delete(cleanId, 1, 1);

  if FID = cleanId then
    Exit(Self);
  Result := nil;
end;

procedure TSvgNode.Render(ACanvas: TObject);
begin
  // Base implementation does nothing
end;

procedure TSvgNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
  valFloat: Single;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if lowerName = 'id' then
    FID := lowerVal
  else
  if (lowerName = 'class') or (lowerName = 'classname') then
    FCssClassName := lowerVal
  else
  if lowerName = 'transform' then
    FTransform := ParseSvgTransform(lowerVal)
  else
  if (lowerName = 'display') or (lowerName = 'visibility') then
  begin
    lowerVal := LowerCase(lowerVal);
    if (lowerVal = 'none') or (lowerVal = 'hidden') then
      FVisible := False
    else if (lowerVal = 'inline') or (lowerVal = 'visible') then
      FVisible := True;
  end else
  if lowerName = 'fill' then
  begin
    if LowerCase(lowerVal) = 'none' then
      FFill.Color := TSvgColor.None
    else
    if Pos('url(', LowerCase(lowerVal)) = 1 then
      FFill.Url := lowerVal
    else
      FFill.Color := TSvgColor.Parse(lowerVal);
  end else
  if lowerName = 'fill-opacity' then
  begin
    if TryStrToFloat(lowerVal, valFloat, SvgFormatSettings) then
      FFill.Opacity := EnsureRange(valFloat, 0.0, 1.0);
  end else
  if lowerName = 'fill-rule' then
  begin
    if LowerCase(lowerVal) = 'evenodd' then
      FFill.FillRule := pfAlternate
    else
      FFill.FillRule := pfWinding;
  end else
  if lowerName = 'stroke' then
  begin
    if LowerCase(lowerVal) = 'none' then
      FStroke.Color := TSvgColor.None
    else
    if Pos('url(', LowerCase(lowerVal)) = 1 then
      FStroke.Url := lowerVal
    else
      FStroke.Color := TSvgColor.Parse(lowerVal);
  end else
  if lowerName = 'stroke-opacity' then
  begin
    if TryStrToFloat(lowerVal, valFloat, SvgFormatSettings) then
      FStroke.Opacity := EnsureRange(valFloat, 0.0, 1.0);
  end else
  if lowerName = 'stroke-width' then
  begin
    FStroke.Width := TSvgLength.Parse(lowerVal);
  end else
  if lowerName = 'stroke-linecap' then
  begin
    lowerVal := LowerCase(lowerVal);
    if lowerVal = 'round' then
      FStroke.EndStyle := esRound
    else
    if lowerVal = 'square' then
      FStroke.EndStyle := esSquare
    else
      FStroke.EndStyle := esButt;
  end else
  if lowerName = 'stroke-linejoin' then
  begin
    lowerVal := LowerCase(lowerVal);
    if lowerVal = 'round' then
      FStroke.JoinStyle := jsRound
    else
    if lowerVal = 'bevel' then
      FStroke.JoinStyle := jsBevel
    else
      FStroke.JoinStyle := jsMiter;
  end else
  if lowerName = 'stroke-miterlimit' then
  begin
    if TryStrToFloat(lowerVal, valFloat, SvgFormatSettings) then
      FStroke.MiterLimit := valFloat;
  end else
  if lowerName = 'stroke-dasharray' then
  begin
    FStroke.DashArray := ParseStrokeDashArray(lowerVal);
  end else
  if lowerName = 'stroke-dashoffset' then
  begin
    FStroke.DashOffset := TSvgLength.Parse(UTF8String(lowerVal)).ToPixels;
  end else
  if lowerName = 'style' then
    // Defer inline style parsing so stylesheet rules (classes/IDs) apply first during cascade evaluation
    FStyleAttr := lowerVal;
end;

procedure TSvgNode.ParseStyleAttribute(const AStyleStr: string);
var
  declarations: TStringList;
  decl: string;
  colonPos: Integer;
  k, v: string;
  i: Integer;
begin
  declarations := TStringList.Create;
  try
    declarations.Delimiter := ';';
    declarations.StrictDelimiter := True;
    declarations.DelimitedText := AStyleStr;
    for i := 0 to declarations.Count - 1 do
    begin
      decl := Trim(declarations[i]);
      if decl = '' then Continue;
      colonPos := Pos(':', decl);
      if colonPos > 0 then
      begin
        k := Trim(Copy(decl, 1, colonPos - 1));
        v := Trim(Copy(decl, colonPos + 1, Length(decl) - colonPos));
        ParseAttribute(k, v);
      end;
    end;
  finally
    declarations.Free;
  end;
end;

{ TSvgGroupNode }

constructor TSvgGroupNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FChildren := TObjectList<TSvgNode>.Create(True);
  FOpacity := 1.0;
  FClipPathID := '';
  FMaskID := '';
end;

destructor TSvgGroupNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

function TSvgGroupNode.Clone(AParent: TSvgNode): TSvgNode;
var
  groupRes: TSvgGroupNode;
  i: Integer;
begin
  groupRes := TSvgGroupNode(inherited Clone(AParent));
  groupRes.FOpacity := FOpacity;
  groupRes.FClipPathID := FClipPathID;
  groupRes.FMaskID := FMaskID;
  for i := 0 to FChildren.Count - 1 do
    groupRes.AddChild(FChildren[i].Clone(groupRes));
  Result := groupRes;
end;

function TSvgGroupNode.GetObjectBoundingBox: TFloatRect;
var
  i, k: Integer;
  child: TSvgNode;
  childBox: TFloatRect;
  first: Boolean;
  pts: array[0..3] of TFloatPoint;
  pt: TFloatPoint;
  transHelper: TFloatMatrixHelper;
begin
  { Calculates the object bounding box for a group node by uniting the bounding boxes
    of all renderable child nodes. Child node transformations are applied to transform
    child bounds into parent group coordinate space. }
  first := True;
  Result := FloatRect(0, 0, 0, 0);

  for i := 0 to FChildren.Count - 1 do
  begin
    child := FChildren[i];
    if (child <> nil) and child.IsRenderable and child.Visible then
    begin
      childBox := child.GetObjectBoundingBox;
      if (childBox.Right > childBox.Left) or (childBox.Bottom > childBox.Top) then
      begin
        if not IsIdentityMatrix(child.Transform) then
        begin
          transHelper.Matrix := child.Transform;
          pts[0] := transHelper.TransformPoint(FloatPoint(childBox.Left, childBox.Top));
          pts[1] := transHelper.TransformPoint(FloatPoint(childBox.Right, childBox.Top));
          pts[2] := transHelper.TransformPoint(FloatPoint(childBox.Right, childBox.Bottom));
          pts[3] := transHelper.TransformPoint(FloatPoint(childBox.Left, childBox.Bottom));

          for k := 0 to 3 do
          begin
            pt := pts[k];
            if first then
            begin
              Result := FloatRect(pt.X, pt.Y, pt.X, pt.Y);
              first := False;
            end
            else
            begin
              if pt.X < Result.Left then Result.Left := pt.X;
              if pt.X > Result.Right then Result.Right := pt.X;
              if pt.Y < Result.Top then Result.Top := pt.Y;
              if pt.Y > Result.Bottom then Result.Bottom := pt.Y;
            end;
          end;
        end
        else
        begin
          if first then
          begin
            Result := childBox;
            first := False;
          end
          else
          begin
            if childBox.Left < Result.Left then Result.Left := childBox.Left;
            if childBox.Right > Result.Right then Result.Right := childBox.Right;
            if childBox.Top < Result.Top then Result.Top := childBox.Top;
            if childBox.Bottom > Result.Bottom then Result.Bottom := childBox.Bottom;
          end;
        end;
      end;
    end;
  end;
end;

function TSvgGroupNode.FindNodeById(const AId: string): TSvgNode;
var
  i: Integer;
  found: TSvgNode;
begin
  Result := inherited FindNodeById(AId);
  if Result <> nil then
    Exit;

  for i := 0 to FChildren.Count - 1 do
  begin
    found := FChildren[i].FindNodeById(AId);
    if found <> nil then
      Exit(found);
  end;
  Result := nil;
end;

procedure TSvgGroupNode.AddChild(AChild: TSvgNode);
begin
  if AChild <> nil then
  begin
    AChild.Parent := Self;
    FChildren.Add(AChild);
  end;
end;

procedure TSvgGroupNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
  valFloat: Single;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if lowerName = 'opacity' then
  begin
    if TryStrToFloat(lowerVal, valFloat, SvgFormatSettings) then
      FOpacity := EnsureRange(valFloat, 0.0, 1.0);
  end
  else if lowerName = 'clip-path' then
    FClipPathID := lowerVal
  else if lowerName = 'mask' then
    FMaskID := lowerVal
  else
    inherited ParseAttribute(AName, AValue);
end;

{ TSvgGradientNode }

function TSvgGradientNode.GetIsRenderable: Boolean;
begin
  Result := False;
end;

constructor TSvgGradientNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FStops := TList<TSvgGradientStop>.Create;
  FSpreadMethod := smPad;
  FGradientUnits := guObjectBoundingBox;
  FHref := '';
end;

destructor TSvgGradientNode.Destroy;
begin
  FStops.Free;
  inherited Destroy;
end;

function TSvgGradientNode.Clone(AParent: TSvgNode): TSvgNode;
var
  gradRes: TSvgGradientNode;
  i: Integer;
begin
  gradRes := TSvgGradientNode(inherited Clone(AParent));
  gradRes.FSpreadMethod := FSpreadMethod;
  gradRes.FGradientUnits := FGradientUnits;
  gradRes.FHref := FHref;
  for i := 0 to FStops.Count - 1 do
    gradRes.AddStop(FStops[i]);
  Result := gradRes;
end;

procedure TSvgGradientNode.AddStop(const AStop: TSvgGradientStop);
begin
  FStops.Add(AStop);
end;

procedure TSvgGradientNode.InheritFrom(ParentGradient: TSvgGradientNode);
var
  i: Integer;
begin
  if ParentGradient = nil then
    Exit;
  if FStops.Count = 0 then
  begin
    for i := 0 to ParentGradient.FStops.Count - 1 do
      FStops.Add(ParentGradient.FStops[i]);
  end;
  if IsIdentityMatrix(FTransform) and not IsIdentityMatrix(ParentGradient.FTransform) then
    FTransform := ParentGradient.FTransform;
end;

procedure TSvgGradientNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if (lowerName = 'href') or (lowerName = 'xlink:href') then
    FHref := lowerVal
  else
  if lowerName = 'spreadmethod' then
  begin
    lowerVal := LowerCase(lowerVal);
    if lowerVal = 'reflect' then
      FSpreadMethod := smReflect
    else
    if lowerVal = 'repeat' then
      FSpreadMethod := smRepeat
    else
      FSpreadMethod := smPad;
  end else
  if lowerName = 'gradientunits' then
  begin
    if LowerCase(lowerVal) = 'userspaceonuse' then
      FGradientUnits := guUserSpaceOnUse
    else
      FGradientUnits := guObjectBoundingBox;
  end else
  if (lowerName = 'gradienttransform') or (lowerName = 'transform') then
    FTransform := ParseSvgTransform(lowerVal)
  else
    inherited ParseAttribute(AName, AValue);
end;

{ TSvgLinearGradientNode }

constructor TSvgLinearGradientNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FX1 := TSvgLength.Create(0.0, suPercent);
  FY1 := TSvgLength.Create(0.0, suPercent);
  FX2 := TSvgLength.Create(100.0, suPercent);
  FY2 := TSvgLength.Create(0.0, suPercent);
end;

function TSvgLinearGradientNode.Clone(AParent: TSvgNode): TSvgNode;
var
  linRes: TSvgLinearGradientNode;
begin
  linRes := TSvgLinearGradientNode(inherited Clone(AParent));
  linRes.FX1 := FX1;
  linRes.FY1 := FY1;
  linRes.FX2 := FX2;
  linRes.FY2 := FY2;
  Result := linRes;
end;

procedure TSvgLinearGradientNode.InheritFrom(ParentGradient: TSvgGradientNode);
var
  parentLin: TSvgLinearGradientNode;
begin
  inherited InheritFrom(ParentGradient);
  if ParentGradient is TSvgLinearGradientNode then
  begin
    parentLin := TSvgLinearGradientNode(ParentGradient);
    FX1 := parentLin.FX1;
    FY1 := parentLin.FY1;
    FX2 := parentLin.FX2;
    FY2 := parentLin.FY2;
  end;
end;

procedure TSvgLinearGradientNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if lowerName = 'x1' then FX1 := TSvgLength.Parse(lowerVal)
  else if lowerName = 'y1' then FY1 := TSvgLength.Parse(lowerVal)
  else if lowerName = 'x2' then FX2 := TSvgLength.Parse(lowerVal)
  else if lowerName = 'y2' then FY2 := TSvgLength.Parse(lowerVal)
  else inherited ParseAttribute(AName, AValue);
end;

{ TSvgRadialGradientNode }

constructor TSvgRadialGradientNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FCx := TSvgLength.Create(50.0, suPercent);
  FCy := TSvgLength.Create(50.0, suPercent);
  FR := TSvgLength.Create(50.0, suPercent);
  FFx := TSvgLength.Create(50.0, suPercent);
  FFy := TSvgLength.Create(50.0, suPercent);
  FSpecified := [];
end;

function TSvgRadialGradientNode.GetFx: TSvgLength;
begin
  if (gpFocalX in FSpecified) then
    Result := FFx
  else
    Result := FCx;
end;

function TSvgRadialGradientNode.GetFy: TSvgLength;
begin
  if (gpFocalY in FSpecified) then
    Result := FFy
  else
    Result := FCy;
end;

function TSvgRadialGradientNode.Clone(AParent: TSvgNode): TSvgNode;
var
  radRes: TSvgRadialGradientNode;
begin
  radRes := TSvgRadialGradientNode(inherited Clone(AParent));
  radRes.FCx := FCx;
  radRes.FCy := FCy;
  radRes.FR := FR;
  radRes.FFx := FFx;
  radRes.FFy := FFy;
  radRes.FSpecified := FSpecified;
  Result := radRes;
end;

procedure TSvgRadialGradientNode.InheritFrom(ParentGradient: TSvgGradientNode);
var
  parentRad: TSvgRadialGradientNode;
begin
  inherited InheritFrom(ParentGradient);
  if ParentGradient is TSvgRadialGradientNode then
  begin
    parentRad := TSvgRadialGradientNode(ParentGradient);
    FCx := parentRad.FCx;
    FCy := parentRad.FCy;
    FR := parentRad.FR;
    FFx := parentRad.FFx;
    FFy := parentRad.FFy;
    FSpecified := parentRad.FSpecified;
  end;
end;

procedure TSvgRadialGradientNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  // Note: We go through property setters to get FSpecified updated
  if lowerName = 'cx' then
    Cx := TSvgLength.Parse(lowerVal)
  else
  if lowerName = 'cy' then
    Cy := TSvgLength.Parse(lowerVal)
  else
  if lowerName = 'r' then
    R := TSvgLength.Parse(lowerVal)
  else
  if lowerName = 'fx' then
    Fx := TSvgLength.Parse(lowerVal)
  else
  if lowerName = 'fy' then
    Fy := TSvgLength.Parse(lowerVal)
  else
    inherited ParseAttribute(AName, AValue);
end;

procedure TSvgRadialGradientNode.SetFx(const Value: TSvgLength);
begin
  FFx := Value;
  Include(FSpecified, gpFocalX);
end;

procedure TSvgRadialGradientNode.SetFy(const Value: TSvgLength);
begin
  FFy := Value;
  Include(FSpecified, gpFocalY);
end;

{ TSvgClipPathNode }

function TSvgClipPathNode.GetIsRenderable: Boolean;
begin
  Result := False;
end;

constructor TSvgClipPathNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FClipPathUnits := guUserSpaceOnUse;
end;

function TSvgClipPathNode.Clone(AParent: TSvgNode): TSvgNode;
var
  clipRes: TSvgClipPathNode;
begin
  clipRes := TSvgClipPathNode(inherited Clone(AParent));
  clipRes.FClipPathUnits := FClipPathUnits;
  Result := clipRes;
end;

procedure TSvgClipPathNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if lowerName = 'clippathunits' then
  begin
    if LowerCase(lowerVal) = 'objectboundingbox' then
      FClipPathUnits := guObjectBoundingBox
    else
      FClipPathUnits := guUserSpaceOnUse;
  end
  else
    inherited ParseAttribute(AName, AValue);
end;

{ TSvgMaskNode }

function TSvgMaskNode.GetIsRenderable: Boolean;
begin
  Result := False;
end;

constructor TSvgMaskNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FX := TSvgLength.Create(-10.0, suPercent);
  FY := TSvgLength.Create(-10.0, suPercent);
  FWidth := TSvgLength.Create(120.0, suPercent);
  FHeight := TSvgLength.Create(120.0, suPercent);
  FMaskUnits := guObjectBoundingBox;
  FMaskContentUnits := guUserSpaceOnUse;
end;

function TSvgMaskNode.Clone(AParent: TSvgNode): TSvgNode;
var
  maskRes: TSvgMaskNode;
begin
  maskRes := TSvgMaskNode(inherited Clone(AParent));
  maskRes.FX := FX;
  maskRes.FY := FY;
  maskRes.FWidth := FWidth;
  maskRes.FHeight := FHeight;
  maskRes.FMaskUnits := FMaskUnits;
  maskRes.FMaskContentUnits := FMaskContentUnits;
  Result := maskRes;
end;

procedure TSvgMaskNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if lowerName = 'x' then FX := TSvgLength.Parse(lowerVal)
  else if lowerName = 'y' then FY := TSvgLength.Parse(lowerVal)
  else if lowerName = 'width' then FWidth := TSvgLength.Parse(lowerVal)
  else if lowerName = 'height' then FHeight := TSvgLength.Parse(lowerVal)
  else if lowerName = 'maskunits' then
  begin
    if LowerCase(lowerVal) = 'userspaceonuse' then FMaskUnits := guUserSpaceOnUse
    else FMaskUnits := guObjectBoundingBox;
  end
  else if lowerName = 'maskcontentunits' then
  begin
    if LowerCase(lowerVal) = 'objectboundingbox' then FMaskContentUnits := guObjectBoundingBox
    else FMaskContentUnits := guUserSpaceOnUse;
  end
  else inherited ParseAttribute(AName, AValue);
end;

{ TSvgPatternNode }

function TSvgPatternNode.GetIsRenderable: Boolean;
begin
  Result := False;
end;

constructor TSvgPatternNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FX := TSvgLength.Create(0.0, suPx);
  FY := TSvgLength.Create(0.0, suPx);
  FWidth := TSvgLength.Create(0.0, suPx);
  FHeight := TSvgLength.Create(0.0, suPx);
  FPatternUnits := guObjectBoundingBox;
  FPatternContentUnits := guUserSpaceOnUse;
  FPatternTransform := IdentityMatrix;
  FViewBox.IsDefined := False;
  FPreserveAspectRatio := TSvgPreserveAspectRatio.Default;
  FHref := '';
end;

function TSvgPatternNode.Clone(AParent: TSvgNode): TSvgNode;
var
  patRes: TSvgPatternNode;
begin
  patRes := TSvgPatternNode(inherited Clone(AParent));
  patRes.FX := FX;
  patRes.FY := FY;
  patRes.FWidth := FWidth;
  patRes.FHeight := FHeight;
  patRes.FPatternUnits := FPatternUnits;
  patRes.FPatternContentUnits := FPatternContentUnits;
  patRes.FPatternTransform := FPatternTransform;
  patRes.FViewBox := FViewBox;
  patRes.FPreserveAspectRatio := FPreserveAspectRatio;
  patRes.FHref := FHref;
  Result := patRes;
end;

procedure TSvgPatternNode.InheritFrom(ParentPattern: TSvgPatternNode);
var
  i: Integer;
begin
  if ParentPattern = nil then Exit;

  if (FWidth.Value = 0) and (FWidth.UnitType = suPx) and (ParentPattern.FWidth.Value > 0) then
    FWidth := ParentPattern.FWidth;
  if (FHeight.Value = 0) and (FHeight.UnitType = suPx) and (ParentPattern.FHeight.Value > 0) then
    FHeight := ParentPattern.FHeight;
  if not FViewBox.IsDefined and ParentPattern.FViewBox.IsDefined then
    FViewBox := ParentPattern.FViewBox;

  if (Children.Count = 0) and (ParentPattern.Children.Count > 0) then
  begin
    for i := 0 to ParentPattern.Children.Count - 1 do
      AddChild(ParentPattern.Children[i].Clone(Self));
  end;
end;

procedure TSvgPatternNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if (lowerName = 'href') or (lowerName = 'xlink:href') then
    FHref := lowerVal
  else if lowerName = 'x' then FX := TSvgLength.Parse(lowerVal)
  else if lowerName = 'y' then FY := TSvgLength.Parse(lowerVal)
  else if lowerName = 'width' then FWidth := TSvgLength.Parse(lowerVal)
  else if lowerName = 'height' then FHeight := TSvgLength.Parse(lowerVal)
  else if lowerName = 'patternunits' then
  begin
    if LowerCase(lowerVal) = 'userspaceonuse' then FPatternUnits := guUserSpaceOnUse
    else FPatternUnits := guObjectBoundingBox;
  end
  else if lowerName = 'patterncontentunits' then
  begin
    if LowerCase(lowerVal) = 'objectboundingbox' then FPatternContentUnits := guObjectBoundingBox
    else FPatternContentUnits := guUserSpaceOnUse;
  end
  else if lowerName = 'patterntransform' then
    FPatternTransform := ParseSvgTransform(lowerVal)
  else if lowerName = 'viewbox' then
    FViewBox := TSvgViewBox.Parse(lowerVal)
  else if lowerName = 'preserveaspectratio' then
    FPreserveAspectRatio := TSvgPreserveAspectRatio.Parse(lowerVal)
  else inherited ParseAttribute(AName, AValue);
end;

{ TSvgDocumentNode }

constructor TSvgDocumentNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FWidth := TSvgLength.Create(100.0, suPercent);
  FHeight := TSvgLength.Create(100.0, suPercent);
  FViewBox.IsDefined := False;
  FPreserveAspectRatio := TSvgPreserveAspectRatio.Default;
end;

function TSvgDocumentNode.Clone(AParent: TSvgNode): TSvgNode;
var
  docRes: TSvgDocumentNode;
begin
  docRes := TSvgDocumentNode(inherited Clone(AParent));
  docRes.FWidth := FWidth;
  docRes.FHeight := FHeight;
  docRes.FViewBox := FViewBox;
  docRes.FPreserveAspectRatio := FPreserveAspectRatio;
  Result := docRes;
end;

procedure TSvgDocumentNode.ResolveUseNodes;
const
  // Maximum recursion depth limit to prevent stack overflow from deep <use> structures
  MaxUseDepth = 32;

  procedure ProcessNode(ANode: TSvgNode; ADepth: Integer);
  var
    i: Integer;
    group: TSvgGroupNode;
    useNode: TSvgUseNode;
    targetNode, clonedNode: TSvgNode;
    transHelper: TFloatMatrixHelper;
    targetId: string;
  begin
    // Abort if node is nil, already being resolved (cycle detected), or max depth reached
    if (ANode = nil) or ANode.FResolving or (ADepth > MaxUseDepth) then
      Exit;

    // Mark current node as active in the resolution call stack
    ANode.FResolving := True;
    try
      if ANode is TSvgUseNode then
      begin
        useNode := TSvgUseNode(ANode);
        // Only attempt expansion if useNode has a non-empty href and no children cloned yet
        if (useNode.Href <> '') and (useNode.Children.Count = 0) then
        begin
          targetId := useNode.Href;
          // Strip leading '#' from element ID reference if present
          if (targetId <> '') and (targetId[1] = '#') then
            Delete(targetId, 1, 1);

          if targetId <> '' then
          begin
            targetNode := FindNodeById(targetId);
            // W3C SVG Circular Reference Prevention:
            // Only clone target if targetNode exists and is not currently being resolved (O(1), zero-allocation)
            if (targetNode <> nil) and not targetNode.FResolving then
            begin
              // Keep targetNode.FResolving = True active while cloning AND resolving the cloned subtree
              targetNode.FResolving := True;
              try
                clonedNode := targetNode.Clone(useNode);
                if (useNode.X <> 0) or (useNode.Y <> 0) then
                begin
                  transHelper.Matrix := clonedNode.Transform;
                  transHelper.Translate(useNode.X, useNode.Y);
                  clonedNode.Transform := transHelper.Matrix;
                end;
                useNode.AddChild(clonedNode);

                // Recursively resolve cloned subtree while targetNode remains marked as resolving
                ProcessNode(clonedNode, ADepth + 1);
              finally
                targetNode.FResolving := False;
              end;
            end;
          end;
        end;
      end else
      if ANode is TSvgGroupNode then
      begin
        // Recurse into children of regular container groups
        group := TSvgGroupNode(ANode);
        for i := 0 to group.Children.Count - 1 do
          ProcessNode(group.Children[i], ADepth + 1);
      end;
    finally
      // Reset resolving flag upon exiting node resolution traversal
      ANode.FResolving := False;
    end;
  end;

begin
  ProcessNode(Self, 0);
end;

procedure TSvgDocumentNode.ResolveGradients;

  procedure ProcessNode(ANode: TSvgNode);
  var
    i: Integer;
    group: TSvgGroupNode;
    gradNode, targetGrad: TSvgGradientNode;
    parentTarget: TSvgNode;
  begin
    if ANode is TSvgGradientNode then
    begin
      gradNode := TSvgGradientNode(ANode);
      if gradNode.Href <> '' then
      begin
        parentTarget := FindNodeById(gradNode.Href);
        if parentTarget is TSvgGradientNode then
        begin
          targetGrad := TSvgGradientNode(parentTarget);
          gradNode.InheritFrom(targetGrad);
        end;
      end;
    end;

    if ANode is TSvgGroupNode then
    begin
      group := TSvgGroupNode(ANode);
      for i := 0 to group.Children.Count - 1 do
        ProcessNode(group.Children[i]);
    end;
  end;

begin
  ProcessNode(Self);
end;

procedure TSvgDocumentNode.ResolvePatterns;

  procedure ProcessNode(ANode: TSvgNode);
  var
    i: Integer;
    group: TSvgGroupNode;
    patNode, targetPat: TSvgPatternNode;
    parentTarget: TSvgNode;
  begin
    if ANode is TSvgPatternNode then
    begin
      patNode := TSvgPatternNode(ANode);
      if patNode.Href <> '' then
      begin
        parentTarget := FindNodeById(patNode.Href);
        if parentTarget is TSvgPatternNode then
        begin
          targetPat := TSvgPatternNode(parentTarget);
          patNode.InheritFrom(targetPat);
        end;
      end;
    end;

    if ANode is TSvgGroupNode then
    begin
      group := TSvgGroupNode(ANode);
      for i := 0 to group.Children.Count - 1 do
        ProcessNode(group.Children[i]);
    end;
  end;

begin
  ProcessNode(Self);
end;

procedure TSvgDocumentNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if lowerName = 'width' then
    FWidth := TSvgLength.Parse(lowerVal)
  else if lowerName = 'height' then
    FHeight := TSvgLength.Parse(lowerVal)
  else if lowerName = 'viewbox' then
    FViewBox := TSvgViewBox.Parse(lowerVal)
  else if lowerName = 'preserveaspectratio' then
    FPreserveAspectRatio := TSvgPreserveAspectRatio.Parse(lowerVal)
  else
    inherited ParseAttribute(AName, AValue);
end;

{ TSvgPathNode }

constructor TSvgPathNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FPathData := nil;
end;

function TSvgPathNode.Clone(AParent: TSvgNode): TSvgNode;
var
  pathRes: TSvgPathNode;
  i: Integer;
begin
  pathRes := TSvgPathNode(inherited Clone(AParent));
  SetLength(pathRes.FPathData, Length(FPathData));
  for i := 0 to High(FPathData) do
    pathRes.FPathData[i] := Copy(FPathData[i], 0, Length(FPathData[i]));
  Result := pathRes;
end;

function TSvgPathNode.GetObjectBoundingBox: TFloatRect;
var
  i, j: Integer;
  pt: TFloatPoint;
  first: Boolean;
begin
  { Calculates the tight axis-aligned bounding box of path vertices in local coordinates. }
  first := True;
  Result := FloatRect(0, 0, 0, 0);

  for i := 0 to High(FPathData) do
  begin
    for j := 0 to High(FPathData[i]) do
    begin
      pt := FPathData[i][j];
      if first then
      begin
        Result := FloatRect(pt.X, pt.Y, pt.X, pt.Y);
        first := False;
      end
      else
      begin
        if pt.X < Result.Left then Result.Left := pt.X;
        if pt.X > Result.Right then Result.Right := pt.X;
        if pt.Y < Result.Top then Result.Top := pt.Y;
        if pt.Y > Result.Bottom then Result.Bottom := pt.Y;
      end;
    end;
  end;
end;

{ TSvgDefsNode }

function TSvgDefsNode.GetIsRenderable: Boolean;
begin
  Result := False;
end;

constructor TSvgDefsNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
end;

{ TSvgUseNode }

constructor TSvgUseNode.Create(AParent: TSvgNode);
begin
  inherited Create(AParent);
  FHref := '';
  FX := 0;
  FY := 0;
end;

function TSvgUseNode.Clone(AParent: TSvgNode): TSvgNode;
var
  useRes: TSvgUseNode;
begin
  useRes := TSvgUseNode(inherited Clone(AParent));
  useRes.FHref := FHref;
  useRes.FX := FX;
  useRes.FY := FY;
  Result := useRes;
end;

procedure TSvgUseNode.ParseAttribute(const AName, AValue: string);
var
  lowerName, lowerVal: string;
  valFloat: Single;
begin
  lowerName := LowerCase(Trim(AName));
  lowerVal := Trim(AValue);

  if (lowerName = 'href') or (lowerName = 'xlink:href') then
    FHref := lowerVal
  else if lowerName = 'x' then
  begin
    if TryStrToFloat(lowerVal, valFloat, SvgFormatSettings) then
      FX := valFloat;
  end
  else if lowerName = 'y' then
  begin
    if TryStrToFloat(lowerVal, valFloat, SvgFormatSettings) then
      FY := valFloat;
  end
  else
    inherited ParseAttribute(AName, AValue);
end;

function ParseStopStyle(AStyle: TValuePUtf8Char; var AOpacity: Single): TSvgColor;
var
  n: Double;
  OneProp: TValuePUtf8Char;
begin
  Result := TSvgColor.Create(clBlack32);
  AOpacity := 1.0;

  OneProp := AStyle.Split(';', True);
  OneProp.Trim; // Trim spaces after ';'

  while (OneProp.Len > 0) do
  begin
    if OneProp.StartsText('stop-color:', True) then
    begin
      Result := TSvgColor.Parse(OneProp);
    end else
    if OneProp.StartsText('stop-opacity:', True) then
    begin
      n := 1.0;
      GetExtended(OneProp.Text, OneProp.Len, n);
      AOpacity := n;
    end;

    // Move on to the next property
    OneProp := AStyle.Split(';', True);
    OneProp.Trim;
  end;
end;

{ Primitive Shape Converters }

function CreateRectPath(X, Y, Width, Height, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
var
  path: TFlattenedPath;
begin
  Result := nil;
  if (Width <= 0) or (Height <= 0) then Exit;

  if (Rx <= 0) and (Ry <= 0) then
  begin
    path := TFlattenedPath.Create;
    try
      path.Rectangle(FloatRect(X, Y, X + Width, Y + Height));
      Result := path.Path;
    finally
      path.Free;
    end;
    Exit;
  end;

  if Rx <= 0 then Rx := Ry;
  if Ry <= 0 then Ry := Rx;
  Rx := Min(Rx, Width * 0.5);
  Ry := Min(Ry, Height * 0.5);

  path := TFlattenedPath.Create;
  try
    path.MoveTo(X + Rx, Y);
    path.LineTo(X + Width - Rx, Y);
    path.EllipticalArc(FloatPoint(X + Width - Rx, Y + Ry), Rx, Ry, 0, -Pi * 0.5, 0);
    path.LineTo(X + Width, Y + Height - Ry);
    path.EllipticalArc(FloatPoint(X + Width - Rx, Y + Height - Ry), Rx, Ry, 0, 0, Pi * 0.5);
    path.LineTo(X + Rx, Y + Height);
    path.EllipticalArc(FloatPoint(X + Rx, Y + Height - Ry), Rx, Ry, 0, Pi * 0.5, Pi);
    path.LineTo(X, Y + Ry);
    path.EllipticalArc(FloatPoint(X + Rx, Y + Ry), Rx, Ry, 0, Pi, Pi * 1.5);
    path.EndPath(True);
    Result := path.Path;
  finally
    path.Free;
  end;
end;

function CreateCirclePath(Cx, Cy, Radius: Single): TArrayOfArrayOfFloatPoint;
var
  path: TFlattenedPath;
begin
  Result := nil;
  if Radius <= 0 then Exit;
  path := TFlattenedPath.Create;
  try
    path.Circle(Cx, Cy, Radius);
    path.EndPath(True);
    Result := path.Path;
  finally
    path.Free;
  end;
end;

function CreateEllipsePath(Cx, Cy, Rx, Ry: Single): TArrayOfArrayOfFloatPoint;
var
  path: TFlattenedPath;
begin
  Result := nil;
  if (Rx <= 0) or (Ry <= 0) then Exit;
  path := TFlattenedPath.Create;
  try
    path.Ellipse(Cx, Cy, Rx, Ry);
    path.EndPath(True);
    Result := path.Path;
  finally
    path.Free;
  end;
end;

function CreateLinePath(X1, Y1, X2, Y2: Single): TArrayOfArrayOfFloatPoint;
var
  path: TFlattenedPath;
begin
  path := TFlattenedPath.Create;
  try
    path.MoveTo(X1, Y1);
    path.LineTo(X2, Y2);
    path.EndPath(False);
    Result := path.Path;
  finally
    path.Free;
  end;
end;

function CreatePolylinePath(const APointsStr: string; AClosed: Boolean): TArrayOfArrayOfFloatPoint;
var
  scannerStr: string;
  i, len: Integer;
  pts: array of TFloatPoint;
  ptCount: Integer;
  sVal1, sVal2: Single;
  numStr: string;
  startPos: Integer;

  function ReadNextNum(var AIndex: Integer; out AValue: Single): Boolean;
  begin
    while (AIndex <= len) and (scannerStr[AIndex] in [' ', #9, #10, #13, ',']) do
      Inc(AIndex);
    if AIndex > len then Exit(False);

    startPos := AIndex;
    if scannerStr[AIndex] in ['+', '-'] then Inc(AIndex);
    while (AIndex <= len) and (scannerStr[AIndex] in ['0'..'9', '.']) do Inc(AIndex);
    if (AIndex <= len) and (scannerStr[AIndex] in ['e', 'E']) and
       (AIndex < len) and (scannerStr[AIndex + 1] in ['0'..'9', '+', '-']) then
    begin
      Inc(AIndex);
      if scannerStr[AIndex] in ['+', '-'] then Inc(AIndex);
      while (AIndex <= len) and (scannerStr[AIndex] in ['0'..'9']) do Inc(AIndex);
    end;

    if AIndex = startPos then Exit(False);
    numStr := Copy(scannerStr, startPos, AIndex - startPos);
    Result := TryStrToFloat(numStr, AValue, SvgFormatSettings);
  end;

var
  path: TFlattenedPath;
  idx: Integer;
begin
  Result := nil;
  scannerStr := APointsStr;
  len := Length(scannerStr);
  i := 1;
  ptCount := 0;
  SetLength(pts, 16);

  while ReadNextNum(i, sVal1) do
  begin
    if not ReadNextNum(i, sVal2) then Break;
    if ptCount >= Length(pts) then
      SetLength(pts, Length(pts) * 2);
    pts[ptCount] := FloatPoint(sVal1, sVal2);
    Inc(ptCount);
  end;

  if ptCount < 2 then Exit;

  path := TFlattenedPath.Create;
  try
    path.MoveTo(pts[0]);
    for idx := 1 to ptCount - 1 do
      path.LineTo(pts[idx]);
    path.EndPath(AClosed);
    Result := path.Path;
  finally
    path.Free;
  end;
end;

function ParseStrokeDashArray(const AStr: string): TArrayOfFloat;
var
  s, numStr: string;
  len, i, count, startPos, dIdx: Integer;
  valLength: TSvgLength;
  valPixels: Single;
  tempArr: TArrayOfFloat;
begin
  Result := nil;
  s := LowerCase(Trim(AStr));
  if (s = '') or (s = 'none') then
    Exit;

  len := Length(s);
  i := 1;
  count := 0;
  SetLength(tempArr, 8);

  while i <= len do
  begin
    while (i <= len) and (s[i] in [' ', #9, #10, #13, ',']) do
      Inc(i);
    if i > len then Break;

    startPos := i;
    if s[i] in ['+', '-'] then Inc(i);
    while (i <= len) and (s[i] in ['0'..'9', '.']) do Inc(i);
    if (i <= len) and (s[i] in ['e', 'E']) and
       (i < len) and (s[i + 1] in ['0'..'9', '+', '-']) then
    begin
      Inc(i);
      if s[i] in ['+', '-'] then Inc(i);
      while (i <= len) and (s[i] in ['0'..'9']) do Inc(i);
    end;
    // Consume unit suffix if present (px, pt, mm, cm, in, pc, %)
    while (i <= len) and (s[i] in ['a'..'z', '%']) do Inc(i);

    if i > startPos then
    begin
      numStr := Copy(s, startPos, i - startPos);
      valLength := TSvgLength.Parse(UTF8String(numStr));
      valPixels := valLength.ToPixels;
      if valPixels < 0 then
        valPixels := 0;

      if count >= Length(tempArr) then
        SetLength(tempArr, Length(tempArr) * 2);
      tempArr[count] := valPixels;
      Inc(count);
    end;
  end;

  if count = 0 then
    Exit;

  // Per W3C SVG 1.1 Specification Section 11.4:
  // If an odd number of values is provided, then the list of values is repeated to yield an even number of values.
  if Odd(count) then
  begin
    SetLength(Result, count * 2);
    for dIdx := 0 to count - 1 do
    begin
      Result[dIdx] := tempArr[dIdx];
      Result[dIdx + count] := tempArr[dIdx];
    end;
  end else
  begin
    SetLength(Result, count);
    for dIdx := 0 to count - 1 do
      Result[dIdx] := tempArr[dIdx];
  end;
end;

{ Transform Parser }

function ParseSvgTransform(const AStr: string): TFloatMatrix;
var
  s, cmdStr, paramsStr: string;
  i, len, pStart, pEnd: Integer;
  cmdHelper: TFloatMatrixHelper;
  params: array of Single;
  pCount: Integer;

  procedure ExtractParams(const AParamsText: string);
  var
    pIdx, pLen, startPos: Integer;
    val: Single;
    numStr: string;
  begin
    pLen := Length(AParamsText);
    pIdx := 1;
    pCount := 0;
    SetLength(params, 6);
    while pIdx <= pLen do
    begin
      while (pIdx <= pLen) and (AParamsText[pIdx] in [' ', #9, #10, #13, ',']) do
        Inc(pIdx);
      if pIdx > pLen then Break;

      startPos := pIdx;
      if AParamsText[pIdx] in ['+', '-'] then Inc(pIdx);
      while (pIdx <= pLen) and (AParamsText[pIdx] in ['0'..'9', '.']) do Inc(pIdx);
      if (pIdx <= pLen) and (AParamsText[pIdx] in ['e', 'E']) and
         (pIdx < pLen) and (AParamsText[pIdx + 1] in ['0'..'9', '+', '-']) then
      begin
        Inc(pIdx);
        if AParamsText[pIdx] in ['+', '-'] then Inc(pIdx);
        while (pIdx <= pLen) and (AParamsText[pIdx] in ['0'..'9']) do Inc(pIdx);
      end;

      if pIdx > startPos then
      begin
        numStr := Copy(AParamsText, startPos, pIdx - startPos);
        if TryStrToFloat(numStr, val, SvgFormatSettings) then
        begin
          if pCount >= Length(params) then
            SetLength(params, Length(params) * 2);
          params[pCount] := val;
          Inc(pCount);
        end;
      end;
    end;
  end;

var
  mMat: TFloatMatrix;
begin
  Result := IdentityMatrix;
  s := Trim(AStr);
  len := Length(s);
  i := 1;

  while i <= len do
  begin
    while (i <= len) and (s[i] in [' ', #9, #10, #13, ',']) do
      Inc(i);
    if i > len then Break;

    pStart := Pos('(', Copy(s, i, len - i + 1));
    if pStart = 0 then Break;
    pStart := i + pStart - 1;

    cmdStr := LowerCase(Trim(Copy(s, i, pStart - i)));
    pEnd := Pos(')', Copy(s, pStart, len - pStart + 1));
    if pEnd = 0 then Break;
    pEnd := pStart + pEnd - 1;

    paramsStr := Copy(s, pStart + 1, pEnd - pStart - 1);
    ExtractParams(paramsStr);

    cmdHelper.Matrix := IdentityMatrix;

    if cmdStr = 'translate' then
    begin
      if pCount >= 2 then
        cmdHelper.Translate(params[0], params[1])
      else if pCount = 1 then
        cmdHelper.Translate(params[0], 0);
    end
    else if cmdStr = 'scale' then
    begin
      if pCount >= 2 then
        cmdHelper.Scale(params[0], params[1])
      else if pCount = 1 then
        cmdHelper.Scale(params[0], params[0]);
    end
    else if cmdStr = 'rotate' then
    begin
      if pCount >= 3 then
        cmdHelper.Rotate(params[1], params[2], params[0])
      else if pCount >= 1 then
        cmdHelper.Rotate(params[0]);
    end
    else if cmdStr = 'skewx' then
    begin
      if pCount >= 1 then
        cmdHelper.Skew(Tan(DegToRad(params[0])), 0);
    end
    else if cmdStr = 'skewy' then
    begin
      if pCount >= 1 then
        cmdHelper.Skew(0, Tan(DegToRad(params[0])));
    end
    else if cmdStr = 'matrix' then
    begin
      if pCount >= 6 then
      begin
        mMat[0, 0] := params[0];
        mMat[0, 1] := params[1];
        mMat[0, 2] := 0;
        mMat[1, 0] := params[2];
        mMat[1, 1] := params[3];
        mMat[1, 2] := 0;
        mMat[2, 0] := params[4];
        mMat[2, 1] := params[5];
        mMat[2, 2] := 1;
        cmdHelper.Matrix := mMat;
      end;
    end;

    // In SVG, transform functions in a transform list are applied right-to-left (innermost to outermost).
    // Pre-multiplying cmdHelper.Matrix onto Result achieves the correct right-to-left evaluation order.
    Result := Mult(Result, cmdHelper.Matrix);

    i := pEnd + 1;
  end;
end;

{ XML Parsing }

function ParseSvgXml(AText: TValuePUtf8Char; var AErrorMessage: string): TSvgDocumentNode;
var
  cssStyleSheet: TSvgCssStyleSheet;

  procedure ParseAttributes(ANode: TSvgNode; var AParser: TXmlParser);
  var
    attrName, attrVal: string;
  begin
    while AParser.ParseNext = xtAttribute do
    begin
      attrName := AParser.Name.ToString;
      attrVal := TValuePUtf8Char(AParser.Value).ToString;
      ANode.ParseAttribute(attrName, attrVal);
    end;
  end;

  function ParseSubtree(var AParser: TXmlParser; AParent: TSvgNode): TSvgNode;
  var
    tagKeyword: TSvgTagKeyword;
    StopTag: TSvgStopTagKeyword;
    tagName: string;
    node: TSvgNode;
    groupNode: TSvgGroupNode;
    pathNode: TSvgPathNode;
    docNode: TSvgDocumentNode;
    useNode: TSvgUseNode;
    linGrad: TSvgLinearGradientNode;
    radGrad: TSvgRadialGradientNode;
    clipNode: TSvgClipPathNode;
    maskNode: TSvgMaskNode;
    parentGrad: TSvgGradientNode;
    startDepth: Byte;
    n: Double;
    x, y, w, h, rx, ry, cx, cy, r, x1, y1, x2, y2, stopOffset, stopOpacity: Single;
    ptsStr, dStr, cssText: string;
    childNode: TSvgNode;
    rawCss: RawUtf8;
    stopVal: TSvgGradientStop;
    Color: TSvgColor;
  begin
    Result := nil;
    node := nil;
    if (AParser.Kind <> xtElementStart) then
      exit;
    startDepth := AParser.Depth;

    if (SvgKeywordDictionary.Lookup(AParser.Name, tagKeyword)) then
      case tagKeyword of
        tagSvg:
          begin
            docNode := TSvgDocumentNode.Create(AParent);
            node := docNode;
            ParseAttributes(node, AParser);
          end;

        tagG:
          begin
            groupNode := TSvgGroupNode.Create(AParent);
            node := groupNode;
            ParseAttributes(node, AParser);
          end;

        tagUse:
          begin
            useNode := TSvgUseNode.Create(AParent);
            node := useNode;
            ParseAttributes(node, AParser);
          end;

        tagDefs:
          begin
            node := TSvgDefsNode.Create(AParent);
            ParseAttributes(node, AParser);
          end;

        tagStop:
          begin
            if (AParent <> nil) and (AParent is TSvgGradientNode) then
            begin
              parentGrad := TSvgGradientNode(AParent);
              stopOffset := 0.0;
              stopOpacity := 1.0;
              Color := TSvgColor.Create(clBlack32);
              while AParser.ParseNext = xtAttribute do
              begin
                if (SvgStopTagDictionary.Lookup(AParser.Name, StopTag)) then
                  case StopTag of
                    stopTagOffset:
                      begin
                        n := stopOffset;
                        if (AParser.Value.Len > 0) and (PAnsiChar(AParser.Value.Buffer)[AParser.Value.Len-1] = '%') then
                        begin
                          if (GetExtended(AParser.Value.Buffer, AParser.Value.Len-1, n)) then
                            n := n * 0.01;
                        end else
                          GetExtended(AParser.Value.Buffer, AParser.Value.Len, n);
                        stopOffset := n;
                      end;

                    stopTagStopColor:
                      Color := TSvgColor.Parse(TValuePUtf8Char(AParser.Value));

                    stopTagStopOpacity:
                      begin
                        n := stopOpacity;
                        GetExtended(AParser.Value.Buffer, AParser.Value.Len, n);
                        stopOpacity := n;
                      end;

                    stopTagStyle:
                      Color := ParseStopStyle(TValuePUtf8Char(AParser.Value), stopOpacity);
                  end;
              end;
              stopVal := TSvgGradientStop.Create(stopOffset, Color, stopOpacity);
              parentGrad.AddStop(stopVal);
            end else
              while AParser.ParseNext = xtAttribute do ;

            if AParser.Kind = xtElementEnd then
              AParser.ParseNext;
            Exit(nil);
          end;

        tagMask:
          begin
            maskNode := TSvgMaskNode.Create(AParent);
            node := maskNode;
            ParseAttributes(node, AParser);
          end;

        tagPath:
          begin
            pathNode := TSvgPathNode.Create(AParent);
            node := pathNode;
            dStr := '';
            while AParser.ParseNext = xtAttribute do
            begin
              if LowerCase(AParser.Name.ToString) = 'd' then
                dStr := TValuePUtf8Char(AParser.Value).ToString
              else
                pathNode.ParseAttribute(AParser.Name.ToString, TValuePUtf8Char(AParser.Value).ToString);
            end;
            if dStr <> '' then
              pathNode.PathData := SvgPathDataToPoints(dStr);
          end;

        tagRect:
          begin
            pathNode := TSvgPathNode.Create(AParent);
            node := pathNode;
            x := 0; y := 0; w := 0; h := 0; rx := 0; ry := 0;
            while AParser.ParseNext = xtAttribute do
            begin
              if LowerCase(AParser.Name.ToString) = 'x' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, x, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'y' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, y, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'width' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, w, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'height' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, h, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'rx' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, rx, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'ry' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, ry, SvgFormatSettings)
              else pathNode.ParseAttribute(AParser.Name.ToString, TValuePUtf8Char(AParser.Value).ToString);
            end;
            pathNode.PathData := CreateRectPath(x, y, w, h, rx, ry);
          end;

        tagLine:
          begin
            pathNode := TSvgPathNode.Create(AParent);
            node := pathNode;
            x1 := 0; y1 := 0; x2 := 0; y2 := 0;
            while AParser.ParseNext = xtAttribute do
            begin
              if LowerCase(AParser.Name.ToString) = 'x1' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, x1, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'y1' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, y1, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'x2' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, x2, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'y2' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, y2, SvgFormatSettings)
              else pathNode.ParseAttribute(AParser.Name.ToString, TValuePUtf8Char(AParser.Value).ToString);
            end;
            pathNode.PathData := CreateLinePath(x1, y1, x2, y2);
          end;

        tagStyle:
          begin
            AParser.ConsumeText(rawCss);
            cssText := string(rawCss);
            if (cssText <> '') and (cssStyleSheet <> nil) then
              cssStyleSheet.ParseCss(cssText);
            Exit(nil);
          end;

        tagCircle:
          begin
            pathNode := TSvgPathNode.Create(AParent);
            node := pathNode;
            cx := 0; cy := 0; r := 0;
            while AParser.ParseNext = xtAttribute do
            begin
              if LowerCase(AParser.Name.ToString) = 'cx' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, cx, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'cy' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, cy, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'r' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, r, SvgFormatSettings)
              else pathNode.ParseAttribute(AParser.Name.ToString, TValuePUtf8Char(AParser.Value).ToString);
            end;
            pathNode.PathData := CreateCirclePath(cx, cy, r);
          end;

        tagLineargradient:
          begin
            linGrad := TSvgLinearGradientNode.Create(AParent);
            node := linGrad;
            ParseAttributes(node, AParser);
          end;

        tagRadialgradient:
          begin
            radGrad := TSvgRadialGradientNode.Create(AParent);
            node := radGrad;
            ParseAttributes(node, AParser);
          end;

        tagClippath:
          begin
            clipNode := TSvgClipPathNode.Create(AParent);
            node := clipNode;
            ParseAttributes(node, AParser);
          end;

        tagPattern:
          begin
            node := TSvgPatternNode.Create(AParent);
            ParseAttributes(node, AParser);
          end;

        tagEllipse:
          begin
            pathNode := TSvgPathNode.Create(AParent);
            node := pathNode;
            cx := 0; cy := 0; rx := 0; ry := 0;
            while AParser.ParseNext = xtAttribute do
            begin
              if LowerCase(AParser.Name.ToString) = 'cx' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, cx, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'cy' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, cy, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'rx' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, rx, SvgFormatSettings)
              else if LowerCase(AParser.Name.ToString) = 'ry' then TryStrToFloat(TValuePUtf8Char(AParser.Value).ToString, ry, SvgFormatSettings)
              else pathNode.ParseAttribute(AParser.Name.ToString, TValuePUtf8Char(AParser.Value).ToString);
            end;
            pathNode.PathData := CreateEllipsePath(cx, cy, rx, ry);
          end;

        tagPolyline, tagPolygon:
          begin
            pathNode := TSvgPathNode.Create(AParent);
            node := pathNode;
            ptsStr := '';
            while AParser.ParseNext = xtAttribute do
            begin
              if LowerCase(AParser.Name.ToString) = 'points' then
                ptsStr := TValuePUtf8Char(AParser.Value).ToString
              else
                pathNode.ParseAttribute(AParser.Name.ToString, TValuePUtf8Char(AParser.Value).ToString);
            end;
            pathNode.PathData := CreatePolylinePath(ptsStr, (tagKeyword = tagPolygon));
          end;
      end;

    if node = nil then
    begin
      node := TSvgNode.Create(AParent);
      ParseAttributes(node, AParser);
    end;

    // CSS Specificity Cascade Hierarchy (W3C SVG 1.1 / CSS2):
    // 1. XML Presentation Attributes (e.g., fill="green") are parsed first (lowest priority).
    // 2. CSS Stylesheet Rules (* -> tag -> .class -> #id) are applied next via ApplyToNode,
    //    allowing stylesheet selectors to override presentation attributes.
    // 3. Inline style="..." attributes (specificity 1000) are parsed LAST, ensuring inline
    //    styles override stylesheet rules and presentation attributes (highest priority).
    if cssStyleSheet <> nil then
    begin
      tagName := LowerCase(AParser.Name.ToString);
      cssStyleSheet.ApplyToNode(node, tagName, node.CssClassName, node.ID);
    end;

    // Apply deferred inline style attribute after stylesheet rules to enforce inline specificity dominance
    if (node <> nil) and (node.StyleAttr <> '') then
      node.ParseStyleAttribute(node.StyleAttr);

    if node is TSvgGroupNode then
    begin
      groupNode := TSvgGroupNode(node);
      while AParser.Kind not in [xtEof, xtError] do
      begin
        if (AParser.Kind = xtElementEnd) and (AParser.Depth < startDepth) then
        begin
          AParser.ParseNext;
          Break;
        end;
        if AParser.Kind = xtElementStart then
        begin
          childNode := ParseSubtree(AParser, groupNode);
          if childNode <> nil then
            groupNode.AddChild(childNode);
        end
        else
          AParser.ParseNext;
      end;
    end;

    if not (node is TSvgGroupNode) then
    begin
      while (AParser.Kind not in [xtEof, xtError]) and (AParser.Depth >= startDepth) do
        AParser.ParseNext;
      if AParser.Kind = xtElementEnd then
        AParser.ParseNext;
    end;

    Result := node;
  end;

var
  parser: TXmlParser;
  rootNode: TSvgNode;
  docRes: TSvgDocumentNode;
begin
  Result := nil;
  if (AText.Text = nil) or (AText.Len <= 0) then
  begin
    AErrorMessage := 'No document';
    Exit;
  end;

  parser.Init(AText.Text, AText.Len, [xpoNoException]);

  cssStyleSheet := TSvgCssStyleSheet.Create;
  try

    while parser.ParseNext not in [xtEof, xtError] do
    begin
      if parser.Kind = xtElementStart then
      begin
        rootNode := ParseSubtree(parser, nil);

        if rootNode is TSvgDocumentNode then
          docRes := TSvgDocumentNode(rootNode)
        else
        if rootNode is TSvgGroupNode then
        begin
          docRes := TSvgDocumentNode.Create(nil);
          docRes.AddChild(rootNode);
        end else
        if rootNode <> nil then
        begin
          docRes := TSvgDocumentNode.Create(nil);
          docRes.AddChild(rootNode);
        end else
          docRes := nil;

        if docRes <> nil then
        begin
          docRes.ResolveGradients;
          docRes.ResolvePatterns;
          docRes.ResolveUseNodes;
          Exit(docRes);
        end;
      end;
    end;

  finally
    cssStyleSheet.Free;
  end;

  if (parser.Kind = xtError) then
    AErrorMessage := Format('%d: %s', [parser.LastErrorLine, XML_ERROR[parser.LastError]])
  else
    AErrorMessage := '';
end;

function ParseSvgXml(AText: TValuePUtf8Char): TSvgDocumentNode;
var
  ErrorMessage: string;
begin
  Result := ParseSvgXml(AText, ErrorMessage);
end;

function ParseSvgXml(AText: PAnsiChar; TextLen: NativeInt; var AErrorMessage: string): TSvgDocumentNode;
var
  UTF8: TValuePUtf8Char;
begin
  UTF8.Text := AText;
  UTF8.Len := TextLen;
  Result := ParseSvgXml(UTF8, AErrorMessage);
end;

function ParseSvgXml(AText: PAnsiChar; TextLen: NativeInt): TSvgDocumentNode;
var
  ErrorMessage: string;
  UTF8: TValuePUtf8Char;
begin
  UTF8.Text := AText;
  UTF8.Len := TextLen;
  Result := ParseSvgXml(UTF8, ErrorMessage);
end;

function ParseSvgXml(const AText: UTF8String): TSvgDocumentNode;
begin
  Result := ParseSvgXml(PAnsiChar(AText), Length(AText));
end;

function ParseSvgXml(const AText: UTF8String; var AErrorMessage: string): TSvgDocumentNode;
begin
  Result := ParseSvgXml(PAnsiChar(AText), Length(AText), AErrorMessage);
end;

procedure InitializeKeywordDictionaries;
var
  Tag: TSvgTagKeyword;
  StopTag: TSvgStopTagKeyword;
begin
  for Tag := Low(TSvgTagKeyword) to High(TSvgTagKeyword) do
    SvgKeywordDictionary.Add(sSvgTagKeywords[Tag], Tag);

  for StopTag := Low(TSvgStopTagKeyword) to High(TSvgStopTagKeyword) do
    SvgStopTagDictionary.Add(sSvgStopTagKeywords[StopTag], StopTag);
end;

initialization
  InitializeKeywordDictionaries;
end.
