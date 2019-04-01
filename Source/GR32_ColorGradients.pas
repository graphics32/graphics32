unit GR32_ColorGradients;

(* ***** BEGIN LICENSE BLOCK ***************************************************
* Version: MPL 1.1 or LGPL 2.1 with linking exception                          *
*                                                                              *
* The contents of this file are subject to the Mozilla Public License Version  *
* 1.1 (the "License"); you may not use this file except in compliance with     *
* the License. You may obtain a copy of the License at                         *
* http://www.mozilla.org/MPL/                                                  *
*                                                                              *
* Software distributed under the License is distributed on an "AS IS" basis,   *
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License     *
* for the specific language governing rights and limitations under the         *
* License.                                                                     *
*                                                                              *
* Alternatively, the contents of this file may be used under the terms of the  *
* Free Pascal modified version of the GNU Lesser General Public License        *
* Version 2.1 (the "FPC modified LGPL License"), in which case the provisions  *
* of this license are applicable instead of those above.                       *
* Please see the file LICENSE.txt for additional information concerning this   *
* license.                                                                     *
*                                                                              *
* The Original Code is Color Gradients for Graphics32                          *
*                                                                              *
* The Initial Developer of the Original Code is Angus Johnson                  *
*                                                                              *
* Portions created by the Initial Developer are Copyright (C) 2008-2012        *
* the Initial Developer. All Rights Reserved.                                  *
*                                                                              *
* Contributor(s): Christian Budde <Christian@aixcoustic.com>                   *
*                                                                              *
* ***** END LICENSE BLOCK *****************************************************)

interface

{$I GR32.inc}

uses
  Types, Classes, SysUtils, Math, GR32, GR32_Polygons,
  GR32_VectorUtils, GR32_Bindings;

type
  TColor32GradientStop = record
    Offset: TFloat; //expected range between 0.0 and 1.0
    Color32: TColor32;
  end;
  TArrayOfColor32GradientStop = array of TColor32GradientStop;

  TColor32FloatPoint = record
    Point: TFloatPoint;
    Color32: TColor32;
  end;
  TArrayOfColor32FloatPoint = array of TColor32FloatPoint;

  TColor32LookupTable = class(TPersistent)
  private
    FGradientLUT: PColor32Array;
    FOrder: Byte;
    FMask: Cardinal;
    FSize: Cardinal;
    FOnOrderChanged: TNotifyEvent;
    procedure SetOrder(const Value: Byte);
    function GetColor32(Index: Integer): TColor32; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure SetColor32(Index: Integer; const Value: TColor32);
  protected
    procedure OrderChanged;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Order: Byte = 9); virtual;
    destructor Destroy; override;

    property Order: Byte read FOrder write SetOrder;
    property Size: Cardinal read FSize;
    property Mask: Cardinal read FMask;
    property Color32[Index: Integer]: TColor32 read GetColor32 write SetColor32;
    property Color32Ptr: PColor32Array read FGradientLUT;

    property OnOrderChanged: TNotifyEvent read FOnOrderChanged write FOnOrderChanged;
  end;

  TColor32Gradient = class(TInterfacedPersistent, IStreamPersist)
  private
    FGradientColors: TArrayOfColor32GradientStop;
    FOnGradientColorsChanged: TNotifyEvent;
    function GetGradientEntry(Index: Integer): TColor32GradientStop;
    function GetGradientCount: Integer; {$IFDEF USEINLINING}inline;{$ENDIF}
    function GetStartColor: TColor32;
    function GetEndColor: TColor32;
    procedure SetEndColor(const Value: TColor32);
    procedure SetStartColor(const Value: TColor32);
  protected
    procedure GradientColorsChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Color: TColor32); overload;
    constructor Create(StartColor, EndColor: TColor32); overload;
    constructor Create(const GradientColors: TArrayOfColor32GradientStop); overload;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure ClearColorStops; overload;
    procedure ClearColorStops(Color: TColor32); overload;
    procedure AddColorStop(Offset: TFloat; Color: TColor32); overload; virtual;
    procedure AddColorStop(ColorStop: TColor32GradientStop); overload; virtual;
    procedure SetColors(const GradientColors: array of const); overload;
    procedure SetColors(const GradientColors: TArrayOfColor32GradientStop); overload;
    procedure SetColors(const GradientColors: TArrayOfColor32); overload;
    procedure SetColors(const Palette: TPalette32); overload;
    function GetColorAt(Offset: TFloat): TColor32;

    procedure FillColorLookUpTable(var ColorLUT: array of TColor32); overload;
    procedure FillColorLookUpTable(ColorLUT: PColor32Array; Count: Integer); overload;
    procedure FillColorLookUpTable(ColorLUT: TColor32LookupTable); overload;
    property GradientEntry[Index: Integer]: TColor32GradientStop read GetGradientEntry;
    property GradientCount: Integer read GetGradientCount;
    property StartColor: TColor32 read GetStartColor write SetStartColor;
    property EndColor: TColor32 read GetEndColor write SetEndColor;
    property OnGradientColorsChanged: TNotifyEvent
      read FOnGradientColorsChanged write FOnGradientColorsChanged;
  end;

  TCustomSparsePointGradientSampler = class(TCustomSampler)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetColor(Index: Integer): TColor32; virtual; abstract;
    function GetPoint(Index: Integer): TFloatPoint; virtual; abstract;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; virtual; abstract;
    procedure SetColor(Index: Integer; const Value: TColor32); virtual; abstract;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); virtual; abstract;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); virtual; abstract;
  public
    function GetSampleFixed(X, Y: TFixed): TColor32; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;

    procedure SetPoints(Points: TArrayOfFloatPoint); virtual; abstract;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); overload; virtual; abstract;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); overload; virtual; abstract;

    property Color[Index: Integer]: TColor32 read GetColor write SetColor;
    property Point[Index: Integer]: TFloatPoint read GetPoint write SetPoint;
    property ColorPoint[Index: Integer]: TColor32FloatPoint read GetColorPoint write SetColorPoint;
    property Count: Integer read GetCount;
  end;

  TBarycentricGradientSampler = class(TCustomSparsePointGradientSampler)
  protected
    FColorPoints: array [0 .. 2] of TColor32FloatPoint;
    FDists: array [0 .. 1] of TFloatPoint;
    function GetCount: Integer; override;
    function GetColor(Index: Integer): TColor32; override;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; override;
    function GetPoint(Index: Integer): TFloatPoint; override;
    procedure SetColor(Index: Integer; const Value: TColor32); override;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); override;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateBarycentricCoordinates(X, Y: TFloat; out U, V, W: TFloat); {$IFDEF USEINLINING} inline; {$ENDIF}
  public
    constructor Create(P1, P2, P3: TColor32FloatPoint); overload; virtual;
    function IsPointInTriangle(X, Y: TFloat): Boolean; overload;
    function IsPointInTriangle(const Point: TFloatPoint): Boolean; overload;

    procedure SetPoints(Points: TArrayOfFloatPoint); override;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); override;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); override;

    procedure PrepareSampling; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
    function GetSampleFloatInTriangle(X, Y: TFloat): TColor32;
  end;

  TBilinearGradientSampler = class(TCustomSparsePointGradientSampler)
  protected
    FColorPoints: array [0 .. 3] of TColor32FloatPoint;
    FDists: array [0 .. 2] of TFloatPoint;
    FDot: TFloat;
    FBiasK0: TFloat;
    FBiasU: TFloat;
    FK2Sign: Integer;
    FK2Value: TFloat;
    function GetCount: Integer; override;
    function GetColor(Index: Integer): TColor32; override;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; override;
    function GetPoint(Index: Integer): TFloatPoint; override;
    procedure SetColor(Index: Integer; const Value: TColor32); override;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); override;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure SetPoints(Points: TArrayOfFloatPoint); override;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); override;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); override;

    procedure PrepareSampling; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TCustomArbitrarySparsePointGradientSampler = class(TCustomSparsePointGradientSampler)
  private
    FColorPoints: TArrayOfColor32FloatPoint;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetCount: Integer; override;
    function GetColor(Index: Integer): TColor32; override;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; override;
    function GetPoint(Index: Integer): TFloatPoint; override;
    procedure SetColor(Index: Integer; const Value: TColor32); override;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); override;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); override;
  public
    procedure Add(Point: TFloatPoint; Color: TColor32); overload; virtual;
    procedure Add(const ColorPoint: TColor32FloatPoint); overload; virtual;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); override;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); override;
    procedure SetPoints(Points: TArrayOfFloatPoint); override;
    procedure Clear; virtual;
  end;

  TInvertedDistanceWeightingSampler = class(TCustomArbitrarySparsePointGradientSampler)
  private
    FDists: TArrayOfFloat;
    FUsePower: Boolean;
    FPower: TFloat;
    FScaledPower: TFloat;
  public
    constructor Create; virtual;
    procedure PrepareSampling; override;
    procedure FinalizeSampling; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;

    property Power: TFloat read FPower write FPower;
  end;

  TVoronoiSampler = class(TCustomArbitrarySparsePointGradientSampler)
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TGourandShadedDelaunayTrianglesSampler = class(TCustomArbitrarySparsePointGradientSampler)
  private
    FTriangles: TArrayOfTriangleVertexIndices;
    FBarycentric: array of TBarycentricGradientSampler;
  public
    procedure PrepareSampling; override;
    procedure FinalizeSampling; override;
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TCustomGradientSampler = class(TCustomSampler)
  private
    FGradient: TColor32Gradient;
    FWrapMode: TWrapMode;
    procedure SetGradient(const Value: TColor32Gradient);
    procedure SetWrapMode(const Value: TWrapMode);
  protected
    FInitialized: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
    procedure GradientChangedHandler(Sender: TObject);
    procedure GradientSamplerChanged; //de-initializes sampler
    procedure WrapModeChanged; virtual;
    procedure UpdateInternals; virtual; abstract;

    property Initialized: Boolean read FInitialized;
  public
    constructor Create(WrapMode: TWrapMode = wmMirror); overload; virtual;
    constructor Create(ColorGradient: TColor32Gradient); overload; virtual;
    destructor Destroy; override;

    procedure PrepareSampling; override;
    function GetSampleInt(X, Y: Integer): TColor32; override;
    function GetSampleFixed(X, Y: TFixed): TColor32; override;

    property Gradient: TColor32Gradient read FGradient write SetGradient;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode;
  end;

  TCustomGradientLookUpTableSampler = class(TCustomGradientSampler)
  private
    FGradientLUT: TColor32LookupTable;
    FLutPtr: PColor32Array;
    FLutMask: Integer;
    FWrapProc: TWrapProc;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure WrapModeChanged; override;
    procedure UpdateInternals; override;

    property LutPtr: PColor32Array read FLutPtr;
    property LutMask: Integer read FLutMask;
    property WrapProc: TWrapProc read FWrapProc;
  public
    constructor Create(WrapMode: TWrapMode = wmMirror); override;
    destructor Destroy; override;
  end;

  TCustomCenterLutGradientSampler = class(TCustomGradientLookUpTableSampler)
  private
    FCenter: TFloatPoint;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Transform(var X, Y: TFloat); virtual;
  public
    constructor Create(WrapMode: TWrapMode = wmMirror); override;

    property Center: TFloatPoint read FCenter write FCenter;
  end;

  TConicGradientSampler = class(TCustomCenterLutGradientSampler)
  private
    FScale: TFloat;
    FAngle: TFloat;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;

    property Angle: TFloat read FAngle write FAngle;
  end;

  TCustomCenterRadiusLutGradientSampler = class(TCustomCenterLutGradientSampler)
  private
    FRadius: TFloat;
    procedure SetRadius(const Value: TFloat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure RadiusChanged; virtual;
  public
    constructor Create(WrapMode: TWrapMode = wmMirror); override;

    property Radius: TFloat read FRadius write SetRadius;
  end;

  TRadialGradientSampler = class(TCustomCenterRadiusLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TCustomCenterRadiusAngleLutGradientSampler = class(TCustomCenterRadiusLutGradientSampler)
  private
    FAngle: TFloat;
    FSinCos: TFloatPoint;
    procedure SetAngle(const Value: TFloat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AngleChanged; virtual;
    procedure RadiusChanged; override;
    procedure Transform(var X, Y: TFloat); override;
  public
    constructor Create(WrapMode: TWrapMode = wmMirror); override;

    property Angle: TFloat read FAngle write SetAngle;
  end;

  TDiamondGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TXGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
    function GetEndPoint: TFloatPoint;
    function GetStartPoint: TFloatPoint;
    procedure SetEndPoint(const Value: TFloatPoint);
    procedure SetStartPoint(const Value: TFloatPoint);
  protected
    procedure UpdateInternals; override;
  public
    procedure SimpleGradient(const StartPoint: TFloatPoint; StartColor: TColor32;
      const EndPoint: TFloatPoint; EndColor: TColor32); virtual;
    procedure SetPoints(const StartPoint, EndPoint: TFloatPoint); virtual;

    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  public
    property StartPoint: TFloatPoint read GetStartPoint write SetStartPoint;
    property EndPoint: TFloatPoint read GetEndPoint write SetEndPoint;
  end;

  TLinearGradientSampler = class(TXGradientSampler);

  TXYGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TXYSqrtGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)
  private
    FScale: TFloat;
  protected
    procedure UpdateInternals; override;
  public
    function GetSampleFloat(X, Y: TFloat): TColor32; override;
  end;

  TCustomSparsePointGradientPolygonFiller = class(TCustomPolygonFiller)
  protected
    function GetCount: Integer; virtual; abstract;
    function GetColor(Index: Integer): TColor32; virtual; abstract;
    function GetPoint(Index: Integer): TFloatPoint; virtual; abstract;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; virtual; abstract;
    procedure SetColor(Index: Integer; const Value: TColor32); virtual; abstract;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); virtual; abstract;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); virtual; abstract;
  public
    procedure SetPoints(Points: TArrayOfFloatPoint); virtual; abstract;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); overload; virtual; abstract;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); overload; virtual; abstract;

    property Color[Index: Integer]: TColor32 read GetColor write SetColor;
    property Point[Index: Integer]: TFloatPoint read GetPoint write SetPoint;
    property ColorPoint[Index: Integer]: TColor32FloatPoint read GetColorPoint write SetColorPoint;
    property Count: Integer read GetCount;
  end;

  TBarycentricGradientPolygonFiller = class(TCustomSparsePointGradientPolygonFiller)
  protected
    FColorPoints: array [0 .. 2] of TColor32FloatPoint;
    FDists: array [0 .. 1] of TFloatPoint;
    function GetCount: Integer; override;
    function GetColor(Index: Integer): TColor32; override;
    function GetPoint(Index: Integer): TFloatPoint; override;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; override;
    procedure SetColor(Index: Integer; const Value: TColor32); override;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); override;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); override;
    function GetFillLine: TFillLineEvent; override;
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    class function Linear3PointInterpolation(A, B, C: TColor32;
      WeightA, WeightB, WeightC: Single): TColor32;
  public
    procedure BeginRendering; override;

    procedure SetPoints(Points: TArrayOfFloatPoint); override;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); overload; override;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); overload; override;
  end;

  TCustomArbitrarySparsePointGradientPolygonFiller = class(TCustomSparsePointGradientPolygonFiller)
  private
    FColorPoints: TArrayOfColor32FloatPoint;
  protected
    function GetCount: Integer; override;
    function GetColor(Index: Integer): TColor32; override;
    function GetColorPoint(Index: Integer): TColor32FloatPoint; override;
    function GetPoint(Index: Integer): TFloatPoint; override;
    procedure SetColor(Index: Integer; const Value: TColor32); override;
    procedure SetColorPoint(Index: Integer; const Value: TColor32FloatPoint); override;
    procedure SetPoint(Index: Integer; const Value: TFloatPoint); override;
  public
    procedure Add(const Point: TFloatPoint; Color: TColor32); overload; virtual;
    procedure Add(const ColorPoint: TColor32FloatPoint); overload; virtual;
    procedure SetColorPoints(ColorPoints: TArrayOfColor32FloatPoint); override;
    procedure SetColorPoints(Points: TArrayOfFloatPoint; Colors: TArrayOfColor32); override;
    procedure SetPoints(Points: TArrayOfFloatPoint); override;
    procedure Clear; virtual;
  end;

  TGourandShadedDelaunayTrianglesPolygonFiller = class(TCustomArbitrarySparsePointGradientPolygonFiller)
  private
    FTriangles: TArrayOfTriangleVertexIndices;
    FBarycentric: array of TBarycentricGradientSampler;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLine3(Dst: PColor32; DstX, DstY, Count: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLine(Dst: PColor32; DstX, DstY, Count: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    procedure BeginRendering; override;
  end;

  TCustomGradientPolygonFiller = class(TCustomPolygonFiller)
  private
    FGradient: TColor32Gradient;
    FOwnsGradient: Boolean;
    FWrapMode: TWrapMode;
    FWrapProc: TWrapProc;
    procedure SetWrapMode(const Value: TWrapMode);
  protected
    procedure GradientColorsChangedHandler(Sender: TObject);
    procedure FillLineNone(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineSolid(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure GradientFillerChanged; virtual;
    procedure WrapModeChanged; virtual;
  public
    constructor Create; overload;
    constructor Create(ColorGradient: TColor32Gradient); overload; virtual;
    destructor Destroy; override;

    property Gradient: TColor32Gradient read FGradient;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode;
  end;

  TCustomGradientLookupTablePolygonFiller = class(TCustomGradientPolygonFiller)
  private
    FLUTNeedsUpdate: Boolean;
    FOwnsLUT: Boolean;
    FGradientLUT: TColor32LookupTable;
    FUseLookUpTable: Boolean;
    function GetLUTNeedsUpdate: Boolean;
    procedure SetUseLookUpTable(const Value: Boolean);
    procedure SetGradientLUT(const Value: TColor32LookupTable);
  protected
    procedure GradientFillerChanged; override;
    procedure UseLookUpTableChanged; virtual;
    procedure LookUpTableChangedHandler(Sender: TObject);

    property LookUpTableNeedsUpdate: Boolean read GetLUTNeedsUpdate;
  public
    constructor Create; reintroduce; overload;
    constructor Create(LookupTable: TColor32LookupTable); overload; virtual;
    destructor Destroy; override;

    property GradientLUT: TColor32LookupTable read FGradientLUT write SetGradientLUT;
    property UseLookUpTable: Boolean read FUseLookUpTable write SetUseLookUpTable;
  end;

  TCustomLinearGradientPolygonFiller = class(TCustomGradientLookupTablePolygonFiller)
  private
    FIncline: TFloat;
    FStartPoint: TFloatPoint;
    FEndPoint: TFloatPoint;
    procedure SetStartPoint(const Value: TFloatPoint);
    procedure SetEndPoint(const Value: TFloatPoint);

    procedure UpdateIncline;
  protected
    procedure EndPointChanged;
    procedure StartPointChanged;
  public
    procedure SimpleGradient(const StartPoint: TFloatPoint; StartColor: TColor32;
      const EndPoint: TFloatPoint; EndColor: TColor32); virtual;
    procedure SimpleGradientX(const StartX: TFloat; StartColor: TColor32;
      const EndX: TFloat; EndColor: TColor32);
    procedure SimpleGradientY(const StartY: TFloat; StartColor: TColor32;
      const EndY: TFloat; EndColor: TColor32);
    procedure SetPoints(const StartPoint, EndPoint: TFloatPoint); virtual;

    property StartPoint: TFloatPoint read FStartPoint write SetStartPoint;
    property EndPoint: TFloatPoint read FEndPoint write SetEndPoint;
  end;

  TLinearGradientPolygonFiller = class(TCustomLinearGradientPolygonFiller)
  private
    function ColorStopToScanLine(Index: Integer; Y: Integer): TFloat;
  protected
    function GetFillLine: TFillLineEvent; override;

    procedure FillLineNegative(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLinePositive(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineVertical(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineVerticalExtreme(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);

    procedure FillLineVerticalPad(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineVerticalPadExtreme(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineVerticalWrap(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineHorizontalPadPos(Dst: PColor32;
      DstX, DstY, Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineHorizontalPadNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineHorizontalWrapNeg(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);
    procedure FillLineHorizontalWrapPos(Dst: PColor32; DstX, DstY,
      Length: Integer; AlphaValues: PColor32;
      CombineMode: TCombineMode);

    procedure UseLookUpTableChanged; override;
    procedure WrapModeChanged; override;
  public
    constructor Create(ColorGradient: TColor32Gradient); overload; override;
    constructor Create(ColorGradient: TColor32Gradient; UseLookupTable: Boolean); overload; virtual;

    procedure BeginRendering; override; //flags initialized
  end;

  TCustomRadialGradientPolygonFiller = class(TCustomGradientLookupTablePolygonFiller)
  private
    FEllipseBounds: TFloatRect;
    procedure SetEllipseBounds(const Value: TFloatRect);
  protected
    procedure EllipseBoundsChanged; virtual; abstract;
  public
    property EllipseBounds: TFloatRect read FEllipseBounds write SetEllipseBounds;
  end;

  TRadialGradientPolygonFiller = class(TCustomRadialGradientPolygonFiller)
  private
    FCenter: TFloatPoint;
    FRadius: TFloatPoint;
    FRadScale: TFloat;
    FRadXInv: TFloat;

    procedure SetCenter(const Value: TFloatPoint);
    procedure SetRadius(const Value: TFloatPoint);
    procedure UpdateEllipseBounds;
    procedure UpdateRadiusScale;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure EllipseBoundsChanged; override;
    procedure FillLinePad(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineRepeat(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
    procedure FillLineReflect(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(Radius: TFloatPoint); overload;
    constructor Create(BoundingBox: TFloatRect); overload;
    constructor Create(Radius, Center: TFloatPoint); overload;
    procedure BeginRendering; override;

    property Radius: TFloatPoint read FRadius write SetRadius;
    property Center: TFloatPoint read FCenter write SetCenter;
  end;

  TSVGRadialGradientPolygonFiller = class(TCustomRadialGradientPolygonFiller)
  private
    FOffset: TFloatPoint;
    FRadius: TFloatPoint;
    FCenter: TFloatPoint;
    FFocalPt: TFloatPoint;
    FVertDist: TFloat;

    FFocalPointNative: TFloatPoint;

    procedure SetFocalPoint(const Value: TFloatPoint);
    procedure InitMembers;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure EllipseBoundsChanged; override;
    procedure FillLineEllipse(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    constructor Create(EllipseBounds: TFloatRect); overload;
    constructor Create(EllipseBounds: TFloatRect; FocalPoint: TFloatPoint); overload;
    procedure BeginRendering; override;

    procedure SetParameters(EllipseBounds: TFloatRect); overload;
    procedure SetParameters(EllipseBounds: TFloatRect; FocalPoint: TFloatPoint); overload;

    property FocalPoint: TFloatPoint read FFocalPointNative write SetFocalPoint;
  end;

  function Color32FloatPoint(Color: TColor32; Point: TFloatPoint): TColor32FloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function Color32FloatPoint(Color: TColor32; X, Y: TFloat): TColor32FloatPoint; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
  function Color32GradientStop(Offset: TFloat; Color: TColor32): TColor32GradientStop; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

const
  FID_LINEAR3 = 0;
  FID_LINEAR4 = 1;

var
  GradientRegistry: TFunctionRegistry;

implementation

uses
  GR32_LowLevel, GR32_System, GR32_Math, GR32_Geometry, GR32_Blend;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  RCStrWrongFormat = 'Wrong format';
  RCStrOnlyExactly3Point = 'Only exactly 3 points expected!';
  RCStrPointCountMismatch = 'Point count mismatch';
  RCStrNoTColor32LookupTable = 'No TColor32LookupTable object specified';
  RCStrNoTColor32Gradient = 'No TColor32Gradient specified';
  RCStrNoLookupTablePassed = 'No lookup table passed!';

const
  CFloatTolerance = 0.001;
  clNone32: TColor32 = $00000000;

procedure FillLineAlpha(var Dst, AlphaValues: PColor32; Count: Integer;
  Color: TColor32; CombineMode: TCombineMode); {$IFDEF USEINLINING}inline;{$ENDIF}
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := 0 to Count - 1 do
  begin
    BlendMemEx(Color, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

function Color32FloatPoint(Color: TColor32; Point: TFloatPoint): TColor32FloatPoint;
begin
  Result.Point := Point;
  Result.Color32 := Color;
end;

function Color32FloatPoint(Color: TColor32; X, Y: TFloat): TColor32FloatPoint;
begin
  Result.Point := FloatPoint(X, Y);
  Result.Color32 := Color;
end;

function Color32GradientStop(Offset: TFloat; Color: TColor32): TColor32GradientStop;
begin
  Result.Offset := Offset;
  Result.Color32 := Color;
end;

type
  TLinear3PointInterpolation = function (A, B, C: TColor32; WA, WB, WC: Single): TColor32;
  TLinear4PointInterpolation = function (A, B, C, D: TColor32; WA, WB, WC, WD: Single): TColor32;

{ Linear interpolation of several (3, 4) colors }

var
  Linear3PointInterpolationProc: TLinear3PointInterpolation;
  Linear4PointInterpolationProc: TLinear4PointInterpolation;

function Linear3PointInterpolation_Pas(A, B, C: TColor32; WA, WB, WC: Single): TColor32;
var
  Clr: TColor32Entry absolute Result;
begin
  Clr.B := Clamp(Round(
    WA * TColor32Entry(A).B +
    WB * TColor32Entry(B).B +
    WC * TColor32Entry(C).B));
  Clr.G := Clamp(Round(
    WA * TColor32Entry(A).G +
    WB * TColor32Entry(B).G +
    WC * TColor32Entry(C).G));
  Clr.R := Clamp(Round(
    WA * TColor32Entry(A).R +
    WB * TColor32Entry(B).R +
    WC * TColor32Entry(C).R));
  Clr.A := Clamp(Round(
    WA * TColor32Entry(A).A +
    WB * TColor32Entry(B).A +
    WC * TColor32Entry(C).A));
end;

function Linear4PointInterpolation_Pas(A, B, C, D: TColor32; WA, WB, WC,
  WD: Single): TColor32;
var
  Clr: TColor32Entry absolute Result;
begin
  Clr.B := Clamp(Round(
    WA * TColor32Entry(A).B +
    WB * TColor32Entry(B).B +
    WC * TColor32Entry(C).B +
    WD * TColor32Entry(D).B));
  Clr.G := Clamp(Round(
    WA * TColor32Entry(A).G +
    WB * TColor32Entry(B).G +
    WC * TColor32Entry(C).G +
    WD * TColor32Entry(D).G));
  Clr.R := Clamp(Round(
    WA * TColor32Entry(A).R +
    WB * TColor32Entry(B).R +
    WC * TColor32Entry(C).R +
    WD * TColor32Entry(D).R));
  Clr.A := Clamp(Round(
    WA * TColor32Entry(A).A +
    WB * TColor32Entry(B).A +
    WC * TColor32Entry(C).A +
    WD * TColor32Entry(D).A));
end;

{$IFNDEF OMIT_SSE2}

{$IFNDEF PUREPASCAL}
function Linear3PointInterpolation_SSE2(A, B, C: TColor32; WA, WB, WC: Single): TColor32;
asm
{$IFDEF TARGET_X86}
        PXOR      XMM3,XMM3
        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM3
        PUNPCKLWD XMM0,XMM3
        CVTDQ2PS  XMM0,XMM0
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM3
        PUNPCKLWD XMM1,XMM3
        CVTDQ2PS  XMM1,XMM1
        MOVD      XMM2,ECX
        PUNPCKLBW XMM2,XMM3
        PUNPCKLWD XMM2,XMM3
        CVTDQ2PS  XMM2,XMM2

        MOV       EAX, WA
        MOV       EDX, WB
        MOV       ECX, WC
        MOVD      XMM4,EAX
        SHUFPS    XMM4,XMM4,0
        MOVD      XMM5,EDX
        SHUFPS    XMM5,XMM5,0
        MOVD      XMM6,ECX
        SHUFPS    XMM6,XMM6,0

        MULPS     XMM0,XMM4
        MULPS     XMM1,XMM5
        MULPS     XMM2,XMM6
        ADDPS     XMM0,XMM1
        ADDPS     XMM0,XMM2
        CVTPS2DQ  XMM0,XMM0
        PACKSSDW  XMM0,XMM3
        PACKUSWB  XMM0,XMM3
        MOVD      EAX,XMM0
{$ENDIF}
{$IFDEF TARGET_X64}
        MOVQ      XMM0,XMM3
        SHUFPS    XMM0,XMM0,0
        MOVD      XMM1,WB
        SHUFPS    XMM1,XMM1,0
        MOVD      XMM2,WC
        SHUFPS    XMM2,XMM2,0

        PXOR      XMM3,XMM3
        MOVD      XMM4,ECX
        PUNPCKLBW XMM4,XMM3
        PUNPCKLWD XMM4,XMM3
        CVTDQ2PS  XMM4,XMM4
        MOVD      XMM5,EDX
        PUNPCKLBW XMM5,XMM3
        PUNPCKLWD XMM5,XMM3
        CVTDQ2PS  XMM5,XMM5
        MOVD      XMM6,R8D
        PUNPCKLBW XMM6,XMM3
        PUNPCKLWD XMM6,XMM3
        CVTDQ2PS  XMM6,XMM6

        MULPS     XMM0,XMM4
        MULPS     XMM1,XMM5
        MULPS     XMM2,XMM6
        ADDPS     XMM0,XMM1
        ADDPS     XMM0,XMM2
        CVTPS2DQ  XMM0,XMM0
        PACKSSDW  XMM0,XMM3
        PACKUSWB  XMM0,XMM3
        MOVD      EAX,XMM0
{$ENDIF}
end;

function Linear4PointInterpolation_SSE2(A, B, C, D: TColor32; WA, WB, WC, WD: Single): TColor32;
asm
{$IFDEF TARGET_X86}
        PXOR      XMM7,XMM7

        MOVD      XMM0,EAX
        PUNPCKLBW XMM0,XMM7
        PUNPCKLWD XMM0,XMM7
        CVTDQ2PS  XMM0,XMM0
        MOVD      XMM1,EDX
        PUNPCKLBW XMM1,XMM7
        PUNPCKLWD XMM1,XMM7
        CVTDQ2PS  XMM1,XMM1

        MOV       EAX, WA
        MOVD      XMM4,EAX
        SHUFPS    XMM4,XMM4,0
        MULPS     XMM0,XMM4

        MOV       EDX, WB
        MOVD      XMM5,EDX
        SHUFPS    XMM5,XMM5,0
        MULPS     XMM1,XMM5
        ADDPS     XMM0,XMM1

        MOVD      XMM2,ECX
        PUNPCKLBW XMM2,XMM7
        PUNPCKLWD XMM2,XMM7
        CVTDQ2PS  XMM2,XMM2
        MOVD      XMM3,D
        PUNPCKLBW XMM3,XMM7
        PUNPCKLWD XMM3,XMM7
        CVTDQ2PS  XMM3,XMM3

        MOV       EAX, WC
        MOVD      XMM4,EAX
        SHUFPS    XMM4,XMM4,0
        MULPS     XMM2,XMM4

        MOV       EDX, WD
        MOVD      XMM5,EDX
        SHUFPS    XMM5,XMM5,0
        MULPS     XMM3,XMM5
        ADDPS     XMM2,XMM3
        ADDPS     XMM0,XMM2

        CVTPS2DQ  XMM0,XMM0
        PACKSSDW  XMM0,XMM7
        PACKUSWB  XMM0,XMM7
        MOVD      EAX,XMM0
{$ENDIF}
{$IFDEF TARGET_X64}
        PXOR      XMM7,XMM7

        MOVD      XMM0,A
        PUNPCKLBW XMM0,XMM7
        PUNPCKLWD XMM0,XMM7
        CVTDQ2PS  XMM0,XMM0
        MOVD      XMM1,B
        PUNPCKLBW XMM1,XMM7
        PUNPCKLWD XMM1,XMM7
        CVTDQ2PS  XMM1,XMM1

        MOV       EAX, WA
        MOVD      XMM4,EAX
        SHUFPS    XMM4,XMM4,0
        MULPS     XMM0,XMM4

        MOV       EDX, WB
        MOVD      XMM5,EDX
        SHUFPS    XMM5,XMM5,0
        MULPS     XMM1,XMM5
        ADDPS     XMM0,XMM1

        MOVD      XMM2,C
        PUNPCKLBW XMM2,XMM7
        PUNPCKLWD XMM2,XMM7
        CVTDQ2PS  XMM2,XMM2
        MOVD      XMM3,D
        PUNPCKLBW XMM3,XMM7
        PUNPCKLWD XMM3,XMM7
        CVTDQ2PS  XMM3,XMM3

        MOV       EAX, WC
        MOVD      XMM4,EAX
        SHUFPS    XMM4,XMM4,0
        MULPS     XMM2,XMM4

        MOV       EDX, WD
        MOVD      XMM5,EDX
        SHUFPS    XMM5,XMM5,0
        MULPS     XMM3,XMM5
        ADDPS     XMM2,XMM3
        ADDPS     XMM0,XMM2

        CVTPS2DQ  XMM0,XMM0
        PACKSSDW  XMM0,XMM7
        PACKUSWB  XMM0,XMM7
        MOVD      EAX,XMM0
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}


{ TColor32LookupTable }

constructor TColor32LookupTable.Create(Order: Byte);
begin
  inherited Create;
  FOrder := Order;
  OrderChanged;
end;

destructor TColor32LookupTable.Destroy;
begin
{$WARNINGS OFF}
  FreeMem(FGradientLUT);
{$WARNINGS ON}
  inherited;
end;

procedure TColor32LookupTable.AssignTo(Dest: TPersistent);
begin
  if Dest is TColor32LookupTable then
    with TColor32LookupTable(Dest) do
    begin
      FOrder := Self.FOrder;
      OrderChanged;
      Move(Self.FGradientLUT^, FGradientLUT^, FSize * SizeOf(TColor32));
    end
  else
    inherited;
end;

function TColor32LookupTable.GetColor32(Index: Integer): TColor32;
begin
  Result := FGradientLUT^[Index and FMask];
end;

procedure TColor32LookupTable.OrderChanged;
begin
  FSize := 1 shl FOrder;
  FMask := FSize - 1;
{$WARNINGS OFF}
  ReallocMem(FGradientLUT, FSize * SizeOf(TColor32));
{$WARNINGS ON}
  if Assigned(FOnOrderChanged) then
    FOnOrderChanged(Self);
end;

procedure TColor32LookupTable.SetColor32(Index: Integer; const Value: TColor32);
begin
  if (Index < 0) or (Index > Integer(FMask)) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    FGradientLUT^[Index] := Value;
end;

procedure TColor32LookupTable.SetOrder(const Value: Byte);
begin
  if FOrder <> Value then
  begin
    FOrder := Value;
    OrderChanged;
  end;
end;


{ TColor32Gradient; }

constructor TColor32Gradient.Create(Color: TColor32);
begin
  Create(Color, Color);
end;

constructor TColor32Gradient.Create(StartColor, EndColor: TColor32);
var
  Temp: TArrayOfColor32GradientStop;
begin
  // simple gradient using 2 color stops
  SetLength(Temp, 2);
  Temp[0].Offset := 0;
  Temp[0].Color32 := StartColor;
  Temp[1].Offset := 1;
  Temp[1].Color32 := EndColor;

  Create(Temp);
end;

constructor TColor32Gradient.Create(const GradientColors: TArrayOfColor32GradientStop);
begin
  inherited Create;
  SetColors(GradientColors);
end;

procedure TColor32Gradient.AssignTo(Dest: TPersistent);
begin
  if Dest is TColor32Gradient then
    TColor32Gradient(Dest).SetColors(Self.FGradientColors)
  else
    inherited;
end;

procedure TColor32Gradient.AddColorStop(ColorStop: TColor32GradientStop);
begin
  AddColorStop(ColorStop.Offset, ColorStop.Color32);
end;

procedure TColor32Gradient.AddColorStop(Offset: TFloat; Color: TColor32);
var
  Index, OldCount: Integer;
begin
  OldCount := Length(FGradientColors);
  Index := 0;

  // navigate to index where the color stop shall be inserted
  while (Index < OldCount) and (Offset >= FGradientColors[Index].Offset) do
    Inc(Index);

  SetLength(FGradientColors, OldCount + 1);

  // move existing color stops to make space for the new color stop
  if (Index < OldCount) then
    Move(FGradientColors[Index], FGradientColors[Index + 1],
      (OldCount - Index) * SizeOf(TColor32GradientStop));

  // finally insert new color stop
  FGradientColors[Index].Offset := Offset;
  FGradientColors[Index].Color32 := Color;
  GradientColorsChanged;
end;

procedure TColor32Gradient.ClearColorStops(Color: TColor32);
begin
  SetLength(FGradientColors, 0);
  FGradientColors[0].Offset := 0;
  FGradientColors[0].Color32 := Color;
  GradientColorsChanged;
end;

procedure TColor32Gradient.ClearColorStops;
begin
  SetLength(FGradientColors, 0);
  GradientColorsChanged;
end;

procedure TColor32Gradient.SetColors(const GradientColors: array of const);
var
  Index: Integer;
  Scale: TFloat;
begin
  if High(GradientColors) < 0 then
  begin
    // no colors specified
    if Length(FGradientColors) > 0 then
      ClearColorStops;
  end else
  begin
    SetLength(FGradientColors, High(GradientColors) + 1);

    if High(GradientColors) >= 1 then
    begin
      // several colors (at least 2)
      Scale := 1 / (Length(GradientColors) - 1);
      for Index := 0 to Length(GradientColors) - 1 do
      begin
        Assert(GradientColors[Index].VType = vtInteger);
        FGradientColors[Index].Color32 := GradientColors[Index].VInteger;
        FGradientColors[Index].Offset := Index * Scale;
      end;
    end
    else
    begin
      // only 1 color
      Assert(GradientColors[0].VType = vtInteger);
      FGradientColors[0].Color32 := GradientColors[0].VInteger;
      FGradientColors[0].Offset := 0;
    end;

    GradientColorsChanged;
  end;
end;

procedure TColor32Gradient.SetColors(const GradientColors: TArrayOfColor32GradientStop);
var
  Index: Integer;
begin
  if Length(GradientColors) = 0 then
  begin
    if Length(FGradientColors) > 0 then
      ClearColorStops;
  end else
  begin
    SetLength(FGradientColors, Length(GradientColors));
    for Index := 0 to Length(GradientColors) - 1 do
      FGradientColors[Index] := GradientColors[Index];
    GradientColorsChanged;
  end;
end;

procedure TColor32Gradient.SetColors(const GradientColors: TArrayOfColor32);
var
  Index: Integer;
  Scale: TFloat;
begin
  if Length(GradientColors) = 0 then
  begin
    // no colors specified
    if Length(FGradientColors) > 0 then
      ClearColorStops;
  end else
  begin
    SetLength(FGradientColors, Length(GradientColors));

    if Length(GradientColors) > 1 then
    begin
      // several colors (at least 2)
      Scale := 1 / (Length(GradientColors) - 1);
      for Index := 0 to Length(GradientColors) - 1 do
      begin
        FGradientColors[Index].Color32 := GradientColors[Index];
        FGradientColors[Index].Offset := Index * Scale;
      end;
    end
    else
    begin
      // only 1 color
      FGradientColors[0].Color32 := GradientColors[0];
      FGradientColors[0].Offset := 0;
    end;

    GradientColorsChanged;
  end;
end;

procedure TColor32Gradient.SetColors(const Palette: TPalette32);
var
  Index: Integer;
  Scale: TFloat;
begin
  // TPalette32 contains 256 colors
  SetLength(FGradientColors, Length(Palette));

  Scale := 1 / (Length(Palette) - 1);
  for Index := 0 to Length(Palette) - 1 do
  begin
    FGradientColors[Index].Color32 := Palette[Index];
    FGradientColors[Index].Offset := Index * Scale;
  end;

  GradientColorsChanged;
end;

procedure TColor32Gradient.SetStartColor(const Value: TColor32);
var
  HasChanged: Boolean;
begin
  HasChanged := False;
  if Length(FGradientColors) = 0 then
  begin
    SetLength(FGradientColors, 1);
    HasChanged := True;
  end;
  if FGradientColors[0].Offset <> 0 then
  begin
    FGradientColors[0].Offset := 0;
    HasChanged := True;
  end;
  if FGradientColors[0].Color32 <> Value then
  begin
    FGradientColors[0].Color32 := Value;
    HasChanged := True;
  end;
  if HasChanged then
    GradientColorsChanged;
end;

procedure TColor32Gradient.SetEndColor(const Value: TColor32);
var
  HasChanged: Boolean;
begin
  HasChanged := False;
  if Length(FGradientColors) = 1 then
  begin
    SetLength(FGradientColors, 2);
    HasChanged := True;
  end;
  if FGradientColors[High(FGradientColors)].Offset <> 1 then
  begin
    FGradientColors[High(FGradientColors)].Offset := 1;
    HasChanged := True;
  end;
  if FGradientColors[High(FGradientColors)].Color32 <> Value then
  begin
    FGradientColors[High(FGradientColors)].Color32 := Value;
    HasChanged := True;
  end;
  if HasChanged then
    GradientColorsChanged;
end;

function TColor32Gradient.GetGradientCount: Integer;
begin
  Result := Length(FGradientColors);
end;

function TColor32Gradient.GetGradientEntry(Index: Integer): TColor32GradientStop;
begin
  if Index > Length(FGradientColors) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else
    Result := FGradientColors[Index];
end;

function TColor32Gradient.GetStartColor: TColor32;
begin
  if Length(FGradientColors) = 0 then
    Result := clNone32
  else
    Result := FGradientColors[0].Color32;
end;

function TColor32Gradient.GetEndColor: TColor32;
var
  Count: Integer;
begin
  Count := Length(FGradientColors);
  if Count = 0 then
    Result := clNone32
  else
    Result := FGradientColors[Count - 1].Color32;
end;

function TColor32Gradient.GetColorAt(Offset: TFloat): TColor32;
var
  Index, Count: Integer;
begin
  Count := GradientCount;
  if (Count = 0) or (Offset <= FGradientColors[0].Offset) then
    Result := StartColor
  else if (Offset >= FGradientColors[Count - 1].Offset) then
    Result := EndColor
  else
  begin
    Index := 1;

    // find color index for a given offset (between 0 and 1)
    while (Index < Count) and (Offset > FGradientColors[Index].Offset) do
      Inc(Index);

    // calculate new offset (between two colors before and at 'Index')
    Offset := (Offset - FGradientColors[Index - 1].Offset) /
      (FGradientColors[Index].Offset - FGradientColors[Index - 1].Offset);

    // check if offset is out of bounds
    if Offset <= 0 then
      Result := FGradientColors[Index - 1].Color32
    else if Offset >= 1 then
      Result := FGradientColors[Index].Color32
    else
    begin
      // interpolate color
      Result := CombineReg(FGradientColors[Index].Color32,
        FGradientColors[Index - 1].Color32, Round($FF * Offset));
      EMMS;
    end;
  end;
end;

procedure TColor32Gradient.FillColorLookUpTable(ColorLUT: TColor32LookupTable);
begin
  FillColorLookUpTable(ColorLUT.Color32Ptr, ColorLUT.Size);
end;

procedure TColor32Gradient.FillColorLookUpTable(var ColorLUT: array of TColor32);
begin
{$WARNINGS OFF}
  FillColorLookUpTable(@ColorLUT[0], Length(ColorLUT));
{$WARNINGS ON}
end;

procedure TColor32Gradient.FillColorLookUpTable(ColorLUT: PColor32Array;
  Count: Integer);
var
  LutIndex, StopIndex, GradCount: Integer;
  RecalculateScale: Boolean;
  Fraction, LocalFraction, Delta, Scale: TFloat;
begin
  GradCount := GradientCount;

  //check trivial case
  if (GradCount < 2) or (Count < 2) then
  begin
    for LutIndex := 0 to Count - 1 do
      ColorLUT^[LutIndex] := StartColor;
    Exit;
  end;

  // set first (start) and last (end) color
  ColorLUT^[0] := StartColor;
  ColorLUT^[Count - 1] := EndColor;
  Delta := 1 / Count;
  Fraction := Delta;

  LutIndex := 1;
  while Fraction <= FGradientColors[0].Offset do
  begin
    ColorLUT^[LutIndex] := ColorLUT^[0];
    Fraction := Fraction + Delta;
    Inc(LutIndex);
  end;

  Scale := 1;
  StopIndex := 1;
  RecalculateScale := True;
  for LutIndex := LutIndex to Count - 2 do
  begin
    // eventually search next stop
    while (Fraction > FGradientColors[StopIndex].Offset) do
    begin
      Inc(StopIndex);
      if (StopIndex >= GradCount) then
        Break;
      RecalculateScale := True;
    end;

    // eventually fill remaining LUT
    if StopIndex = GradCount then
    begin
      for StopIndex := LutIndex to Count - 2 do
        ColorLUT^[StopIndex] := ColorLUT^[Count - 1];
      Break;
    end;

    // eventually recalculate scale
    if RecalculateScale then
      Scale := 1 / (FGradientColors[StopIndex].Offset -
        FGradientColors[StopIndex - 1].Offset);

    // calculate current color
    LocalFraction := (Fraction - FGradientColors[StopIndex - 1].Offset) * Scale;
    if LocalFraction <= 0 then
      ColorLUT^[LutIndex] := FGradientColors[StopIndex - 1].Color32
    else if LocalFraction >= 1 then
      ColorLUT^[LutIndex] := FGradientColors[StopIndex].Color32
    else
    begin
      ColorLUT^[LutIndex] := CombineReg(FGradientColors[StopIndex].Color32,
        FGradientColors[StopIndex - 1].Color32, Round($FF * LocalFraction));
      EMMS;
    end;
    Fraction := Fraction + Delta;
  end;
end;

procedure TColor32Gradient.GradientColorsChanged;
begin
  if Assigned(FOnGradientColorsChanged) then
    FOnGradientColorsChanged(Self);
end;

procedure TColor32Gradient.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ChunkName: array [0..3] of AnsiChar;
  ValueInt: Integer;
  ValueFloat: Single;
begin
  // read simple header
  Stream.Read(ChunkName, 4);
  if ChunkName <> 'Grad' then
    raise Exception.Create(RCStrWrongFormat);
  Stream.Read(ValueInt, 4);
  SetLength(FGradientColors, ValueInt);

  // read data
  for Index := 0 to Length(FGradientColors) - 1 do
  begin
    ValueFloat := FGradientColors[Index].Offset;
    Stream.Read(ValueFloat, 4);
    ValueInt := FGradientColors[Index].Color32;
    Stream.Read(ValueInt, 4);
  end;

  GradientColorsChanged;
end;

procedure TColor32Gradient.SaveToStream(Stream: TStream);
var
  Index: Integer;
  ChunkName: array [0..3] of AnsiChar;
  ValueInt: Integer;
  ValueFloat: Single;
begin
  // write simple header
  ChunkName := 'Grad';
  Stream.Write(ChunkName, 4);
  ValueInt := Length(FGradientColors);
  Stream.Write(ValueInt, 4);

  // write data
  for Index := 0 to Length(FGradientColors) - 1 do
  begin
    ValueFloat := FGradientColors[Index].Offset;
    Stream.Write(ValueFloat, 4);
    ValueInt := FGradientColors[Index].Color32;
    Stream.Write(ValueInt, 4);
  end;
end;


{ TCustomSparsePointGradientSampler }

function TCustomSparsePointGradientSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := GetSampleFloat(X * FixedToFloat, Y * FixedToFloat);
end;

function TCustomSparsePointGradientSampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := GetSampleFloat(X, Y);
end;


{ TBarycentricGradientSampler }

constructor TBarycentricGradientSampler.Create(P1, P2, P3: TColor32FloatPoint);
begin
  FColorPoints[0] := P1;
  FColorPoints[1] := P2;
  FColorPoints[2] := P3;
  inherited Create;
end;

procedure TBarycentricGradientSampler.AssignTo(Dest: TPersistent);
begin
  if Dest is TBarycentricGradientSampler then
    with TBarycentricGradientSampler(Dest) do
      FColorPoints := Self.FColorPoints
  else
    inherited;
end;

function TBarycentricGradientSampler.GetColor(Index: Integer): TColor32;
begin
  if Index in [0 .. 2] then
    Result := FColorPoints[Index].Color32
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBarycentricGradientSampler.GetColorPoint(
  Index: Integer): TColor32FloatPoint;
begin
  if Index in [0 .. 2] then
    Result := FColorPoints[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBarycentricGradientSampler.GetCount: Integer;
begin
  Result := 3;
end;

function TBarycentricGradientSampler.GetPoint(Index: Integer): TFloatPoint;
begin
  if Index in [0 .. 2] then
    Result := FColorPoints[Index].Point
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientSampler.CalculateBarycentricCoordinates(
  X, Y: TFloat; out U, V, W: TFloat);
var
  Temp: TFloatPoint;
begin
  Temp.X := X - FColorPoints[2].Point.X;
  Temp.Y := Y - FColorPoints[2].Point.Y;
  U := FDists[0].Y * Temp.X + FDists[0].X * Temp.Y;
  V := FDists[1].Y * Temp.X + FDists[1].X * Temp.Y;
  W := 1 - U - V;
end;

function TBarycentricGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  U, V, W: TFloat;
begin
  CalculateBarycentricCoordinates(X, Y, U, V, W);
  Result := Linear3PointInterpolationProc(FColorPoints[0].Color32,
    FColorPoints[1].Color32, FColorPoints[2].Color32, U, V, W);
end;

function TBarycentricGradientSampler.GetSampleFloatInTriangle(X,
  Y: TFloat): TColor32;
var
  U, V, W: TFloat;
begin
  CalculateBarycentricCoordinates(X, Y, U, V, W);
  if U < 0 then
  begin
    U := (V + W);
    V := V / U;
    W := W / U;
    U := 0;
  end;
  if V < 0 then
  begin
    V := (U + W);
    U := U / V;
    W := W / V;
    V := 0;
  end;
  if V < 0 then
  begin
    W := (U + V);
    U := U / W;
    V := V / W;
    W := 0;
  end;

  Result := Linear3PointInterpolationProc(FColorPoints[0].Color32,
    FColorPoints[1].Color32, FColorPoints[2].Color32, U, V, W);
end;

function TBarycentricGradientSampler.IsPointInTriangle(
  const Point: TFloatPoint): Boolean;
var
  U, V, W: TFloat;
begin
  CalculateBarycentricCoordinates(Point.X, Point.Y, U, V, W);
  Result := (U >= 0) and (V >= 0) and (W >= 0);
end;

function TBarycentricGradientSampler.IsPointInTriangle(X, Y: TFloat): Boolean;
var
  U, V, W: TFloat;
begin
  CalculateBarycentricCoordinates(X, Y, U, V, W);
  Result := (U >= 0) and (V >= 0) and (W >= 0);
end;

procedure TBarycentricGradientSampler.PrepareSampling;
var
  NormScale: TFloat;
begin
  NormScale := 1 / ((FColorPoints[1].Point.Y - FColorPoints[2].Point.Y) *
    (FColorPoints[0].Point.X - FColorPoints[2].Point.X) +
    (FColorPoints[2].Point.X - FColorPoints[1].Point.X) *
    (FColorPoints[0].Point.Y - FColorPoints[2].Point.Y));

  FDists[0].X := NormScale * (FColorPoints[2].Point.X - FColorPoints[1].Point.X);
  FDists[0].Y := NormScale * (FColorPoints[1].Point.Y - FColorPoints[2].Point.Y);
  FDists[1].X := NormScale * (FColorPoints[0].Point.X - FColorPoints[2].Point.X);
  FDists[1].Y := NormScale * (FColorPoints[2].Point.Y - FColorPoints[0].Point.Y);
end;

procedure TBarycentricGradientSampler.SetColor(Index: Integer;
  const Value: TColor32);
begin
  if Index in [0 .. 2] then
    FColorPoints[Index].Color32 := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientSampler.SetColorPoint(Index: Integer;
  const Value: TColor32FloatPoint);
begin
  if Index in [0 .. 2] then
    FColorPoints[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientSampler.SetColorPoints(
  ColorPoints: TArrayOfColor32FloatPoint);
begin
  if Length(ColorPoints) <> 3 then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0] := ColorPoints[0];
  FColorPoints[1] := ColorPoints[1];
  FColorPoints[2] := ColorPoints[2];
end;

procedure TBarycentricGradientSampler.SetColorPoints(Points: TArrayOfFloatPoint;
  Colors: TArrayOfColor32);
begin
  if (Length(Points) <> 3) or (Length(Colors) <> 3) then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0] := Color32FloatPoint(Colors[0], Points[0]);
  FColorPoints[1] := Color32FloatPoint(Colors[1], Points[1]);
  FColorPoints[2] := Color32FloatPoint(Colors[2], Points[2]);
end;

procedure TBarycentricGradientSampler.SetPoint(Index: Integer;
  const Value: TFloatPoint);
begin
  if Index in [0 .. 2] then
    FColorPoints[Index].Point := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientSampler.SetPoints(Points: TArrayOfFloatPoint);
begin
  if Length(Points) <> 3 then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0].Point := Points[0];
  FColorPoints[1].Point := Points[1];
  FColorPoints[2].Point := Points[2];
end;


{ TBilinearGradientSampler }

procedure TBilinearGradientSampler.AssignTo(Dest: TPersistent);
begin
  if Dest is TBilinearGradientSampler then
    with TBilinearGradientSampler(Dest) do
      FColorPoints := Self.FColorPoints
  else
    inherited;
end;

function TBilinearGradientSampler.GetColor(Index: Integer): TColor32;
begin
  if Index in [0 .. 3] then
    Result := FColorPoints[Index].Color32
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBilinearGradientSampler.GetColorPoint(
  Index: Integer): TColor32FloatPoint;
begin
  if Index in [0 .. 3] then
    Result := FColorPoints[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBilinearGradientSampler.GetCount: Integer;
begin
  Result := 4;
end;

function TBilinearGradientSampler.GetPoint(Index: Integer): TFloatPoint;
begin
  if Index in [0 .. 3] then
    Result := FColorPoints[Index].Point
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBilinearGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  u, v, t, k0, k1: Double;
begin
  k1 := FDot + X * FDists[2].Y - Y * FDists[2].X;
  k0 := FBiasK0 + X * FDists[0].Y - Y * FDists[0].X;
  t := Sqr(k1) - 2 * k0 * FK2Value;

  if FK2Value = 0 then
    v := -k0 / k1
  else
    v := (FK2Sign * Sqrt(Abs(t)) - k1) / FK2Value;
  u := (X - FBiasU - FDists[1].X * v) / (FDists[0].X + FDists[2].X * v);

  Result := Linear4PointInterpolationProc(FColorPoints[0].Color32,
    FColorPoints[1].Color32, FColorPoints[2].Color32, FColorPoints[3].Color32,
    (1 - u) * (1 - v), u * (1 - v), u * v, (1 - u) * v);
end;

procedure TBilinearGradientSampler.PrepareSampling;
var
  v, i, j: Integer;
  Orientation: array [0 .. 3] of Boolean;
  Indices: array [0 .. 1] of Integer;
  TempPoint: TColor32FloatPoint;
begin
  Orientation[0] := (FColorPoints[0].Point.X - FColorPoints[3].Point.X) *
    (FColorPoints[1].Point.Y - FColorPoints[0].Point.Y) -
    (FColorPoints[0].Point.Y - FColorPoints[3].Point.Y) *
    (FColorPoints[1].Point.X - FColorPoints[0].Point.X) < 0;
  Orientation[1] := (FColorPoints[1].Point.X - FColorPoints[0].Point.X) *
    (FColorPoints[2].Point.Y - FColorPoints[1].Point.Y) -
    (FColorPoints[1].Point.Y - FColorPoints[0].Point.Y) *
    (FColorPoints[2].Point.X - FColorPoints[1].Point.X) < 0;
  Orientation[2] := (FColorPoints[2].Point.X - FColorPoints[1].Point.X) *
    (FColorPoints[3].Point.Y - FColorPoints[2].Point.Y) -
    (FColorPoints[2].Point.Y - FColorPoints[1].Point.Y) *
    (FColorPoints[3].Point.X - FColorPoints[2].Point.X) < 0;
  Orientation[3] := (FColorPoints[3].Point.X - FColorPoints[2].Point.X) *
    (FColorPoints[0].Point.Y - FColorPoints[3].Point.Y) -
    (FColorPoints[3].Point.Y - FColorPoints[2].Point.Y) *
    (FColorPoints[0].Point.X - FColorPoints[3].Point.X) < 0;

  if Orientation[0] then v := -1 else v := 1;
  if Orientation[1] then Dec(v) else Inc(v);
  if Orientation[2] then Dec(v) else Inc(v);
  if Orientation[3] then Dec(v) else Inc(v);
  FK2Sign := Sign(v);

  if v = 0 then
  begin
    // correct complex case
    i := 0;
    j := 0;
    repeat
      if Orientation[j] then
      begin
        Indices[i] := j;
        Inc(i);
      end;
      Inc(j);
    until i = 2;

    // exchange color points
    TempPoint := FColorPoints[Indices[0]];
    FColorPoints[Indices[0]] := FColorPoints[Indices[1]];
    FColorPoints[Indices[1]] := TempPoint;

    FK2Sign := 1;
  end;

  FDists[0].X := FColorPoints[1].Point.X - FColorPoints[0].Point.X;
  FDists[0].Y := FColorPoints[1].Point.Y - FColorPoints[0].Point.Y;
  FDists[1].X := FColorPoints[3].Point.X - FColorPoints[0].Point.X;
  FDists[1].Y := FColorPoints[3].Point.Y - FColorPoints[0].Point.Y;
  FDists[2].X := FColorPoints[0].Point.X - FColorPoints[1].Point.X +
    FColorPoints[2].Point.X - FColorPoints[3].Point.X;
  FDists[2].Y := FColorPoints[0].Point.Y - FColorPoints[1].Point.Y +
    FColorPoints[2].Point.Y - FColorPoints[3].Point.Y;
  FK2Value := 2 * (FDists[2].X * FDists[1].Y - FDists[2].Y * FDists[1].X);

  FDot := FDists[0].X * FDists[1].Y - FDists[0].Y * FDists[1].X -
    FColorPoints[0].Point.X * FDists[2].Y + FColorPoints[0].Point.Y * FDists[2].X;
  FBiasK0 := FColorPoints[0].Point.Y * FDists[0].X -
    FColorPoints[0].Point.X * FDists[0].Y;
  FBiasU := FColorPoints[0].Point.X;
end;

procedure TBilinearGradientSampler.SetColor(Index: Integer;
  const Value: TColor32);
begin
  if Index in [0 .. 3] then
    FColorPoints[Index].Color32 := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBilinearGradientSampler.SetColorPoint(Index: Integer;
  const Value: TColor32FloatPoint);
begin
  if Index in [0 .. 3] then
    FColorPoints[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBilinearGradientSampler.SetColorPoints(
  ColorPoints: TArrayOfColor32FloatPoint);
begin
  if Length(ColorPoints) <> 4 then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0] := ColorPoints[0];
  FColorPoints[1] := ColorPoints[1];
  FColorPoints[2] := ColorPoints[2];
  FColorPoints[3] := ColorPoints[3];
end;

procedure TBilinearGradientSampler.SetColorPoints(Points: TArrayOfFloatPoint;
  Colors: TArrayOfColor32);
begin
  if (Length(Points) <> 3) or (Length(Colors) <> 3) then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0] := Color32FloatPoint(Colors[0], Points[0]);
  FColorPoints[1] := Color32FloatPoint(Colors[1], Points[1]);
  FColorPoints[2] := Color32FloatPoint(Colors[2], Points[2]);
  FColorPoints[3] := Color32FloatPoint(Colors[3], Points[3]);
end;

procedure TBilinearGradientSampler.SetPoint(Index: Integer;
  const Value: TFloatPoint);
begin
  if Index in [0 .. 3] then
    FColorPoints[Index].Point := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBilinearGradientSampler.SetPoints(Points: TArrayOfFloatPoint);
begin
  if Length(Points) <> 4 then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0].Point := Points[0];
  FColorPoints[1].Point := Points[1];
  FColorPoints[2].Point := Points[2];
  FColorPoints[3].Point := Points[3];
end;


{ TCustomArbitrarySparsePointGradientSampler }

procedure TCustomArbitrarySparsePointGradientSampler.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomArbitrarySparsePointGradientSampler then
    with TCustomArbitrarySparsePointGradientSampler(Dest) do
    begin
      FColorPoints := Self.FColorPoints;
    end
  else
    inherited;
end;

procedure TCustomArbitrarySparsePointGradientSampler.Add(Point: TFloatPoint;
  Color: TColor32);
var
  Index: Integer;
begin
  Index := Length(FColorPoints);
  SetLength(FColorPoints, Index + 1);
  FColorPoints[Index].Point := Point;
  FColorPoints[Index].Color32 := Color;
end;

procedure TCustomArbitrarySparsePointGradientSampler.Add(
  const ColorPoint: TColor32FloatPoint);
var
  Index: Integer;
begin
  Index := Length(FColorPoints);
  SetLength(FColorPoints, Index + 1);
  FColorPoints[Index].Point := ColorPoint.Point;
  FColorPoints[Index].Color32 := ColorPoint.Color32;
end;

procedure TCustomArbitrarySparsePointGradientSampler.Clear;
begin
  SetLength(FColorPoints, 0);
end;

function TCustomArbitrarySparsePointGradientSampler.GetColor(
  Index: Integer): TColor32;
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    Result := FColorPoints[Index].Color32
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomArbitrarySparsePointGradientSampler.GetColorPoint(
  Index: Integer): TColor32FloatPoint;
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    Result := FColorPoints[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomArbitrarySparsePointGradientSampler.GetCount: Integer;
begin
  Result := Length(FColorPoints);
end;

function TCustomArbitrarySparsePointGradientSampler.GetPoint(
  Index: Integer): TFloatPoint;
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    Result := FColorPoints[Index].Point
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientSampler.SetColor(Index: Integer;
  const Value: TColor32);
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    FColorPoints[Index].Color32 := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientSampler.SetColorPoint(
  Index: Integer; const Value: TColor32FloatPoint);
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    FColorPoints[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientSampler.SetPoint(Index: Integer;
  const Value: TFloatPoint);
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    FColorPoints[Index].Point := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientSampler.SetColorPoints(
  ColorPoints: TArrayOfColor32FloatPoint);
var
  Index: Integer;
begin
  SetLength(FColorPoints, Length(ColorPoints));
  for Index := 0 to High(FColorPoints) do
    FColorPoints[Index] := ColorPoints[Index];
end;

procedure TCustomArbitrarySparsePointGradientSampler.SetColorPoints(
  Points: TArrayOfFloatPoint; Colors: TArrayOfColor32);
var
  Index: Integer;
begin
  if Length(Points) <> Length(Colors) then
    raise Exception.Create(RCStrPointCountMismatch);

  SetLength(FColorPoints, Length(Points));
  for Index := 0 to High(FColorPoints) do
  begin
    FColorPoints[Index].Point := Points[Index];
    FColorPoints[Index].Color32 := Colors[Index];
  end;
end;

procedure TCustomArbitrarySparsePointGradientSampler.SetPoints(
  Points: TArrayOfFloatPoint);
var
  Index: Integer;
begin
  if Length(FColorPoints) <> Length(Points) then
    raise Exception.Create(RCStrPointCountMismatch);

  for Index := 0 to High(Points) do
    FColorPoints[Index].Point := Points[Index];
end;


{ TInvertedDistanceWeightingSampler }

constructor TInvertedDistanceWeightingSampler.Create;
begin
  inherited;
  FPower := 2;
  FScaledPower := 0.5 * FPower;
end;

procedure TInvertedDistanceWeightingSampler.FinalizeSampling;
begin
  inherited;
  Finalize(FDists);
end;

function TInvertedDistanceWeightingSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  Index: Integer;
  Temp, DistSum, Scale: Double;
  R, G, B, A: TFloat;
begin
  if Count = 1 then
  begin
    Result := FColorPoints[0].Color32;
    Exit;
  end;

  with FColorPoints[0] do
    Temp := Sqr(X - Point.X) + Sqr(Y - Point.Y);
  if FUsePower then
    Temp := Math.Power(Temp, FScaledPower);
  FDists[0] := 1 / Max(1, Temp);
  DistSum := FDists[0];
  for Index := 1 to Count - 1 do
    with FColorPoints[Index] do
    begin
      Temp := Sqr(X - Point.X) + Sqr(Y - Point.Y);
      if FUsePower then
        Temp := Math.Power(Temp, FScaledPower);
      FDists[Index] := 1 / Max(1, Temp);
      DistSum := DistSum + FDists[Index];
    end;

  Assert(DistSum <> 0);
  DistSum := 1 / DistSum;
  Scale := FDists[0] * DistSum;

  case Count of
    3:
      begin
        // optimization for 3-Point interpolation
        Result := Linear3PointInterpolationProc(FColorPoints[0].Color32,
          FColorPoints[1].Color32, FColorPoints[2].Color32, FDists[0] * DistSum,
          FDists[1] * DistSum, FDists[2] * DistSum);
        Exit;
      end;
    4:
      begin
        // optimization for 4-Point interpolation
        Result := Linear4PointInterpolationProc(FColorPoints[0].Color32,
          FColorPoints[1].Color32, FColorPoints[2].Color32,
          FColorPoints[3].Color32, FDists[0] * DistSum, FDists[1] * DistSum,
          FDists[2] * DistSum, FDists[3] * DistSum);
        Exit;
      end;
  end;

  // general n-Point interpolation
  R := Scale * TColor32Entry(FColorPoints[0].Color32).R;
  G := Scale * TColor32Entry(FColorPoints[0].Color32).G;
  B := Scale * TColor32Entry(FColorPoints[0].Color32).B;
  A := Scale * TColor32Entry(FColorPoints[0].Color32).A;
  for Index := 1 to Count - 1 do
  begin
    Scale := FDists[Index] * DistSum;
    R := R + Scale * TColor32Entry(FColorPoints[Index].Color32).R;
    G := G + Scale * TColor32Entry(FColorPoints[Index].Color32).G;
    B := B + Scale * TColor32Entry(FColorPoints[Index].Color32).B;
    A := A + Scale * TColor32Entry(FColorPoints[Index].Color32).A;
  end;

  Result := Color32(Clamp(Round(R)), Clamp(Round(G)), Clamp(Round(B)),
    Clamp(Round(A)));
end;

procedure TInvertedDistanceWeightingSampler.PrepareSampling;
begin
  SetLength(FDists, Count);
  FUsePower := FPower <> 2;
  FScaledPower := 0.5 * FPower;
  inherited;
end;


{ TVoronoiSampler }

function TVoronoiSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  Index, NearestIndex: Integer;
  Distance: TFloat;
  NearestDistance: TFloat;
begin
  NearestIndex := 0;
  NearestDistance := Sqr(X - FColorPoints[0].Point.X) + Sqr(Y - FColorPoints[0].Point.Y);
  for Index := 1 to High(FColorPoints) do
  begin
    Distance := Sqr(X - FColorPoints[Index].Point.X) + Sqr(Y - FColorPoints[Index].Point.Y);
    if Distance < NearestDistance then
    begin
      NearestDistance := Distance;
      NearestIndex := Index;
    end;
  end;
  Result := FColorPoints[NearestIndex].Color32;
end;


{ TDelaunaySampler }

procedure FastMergeSortX(const Values: TArrayOfColor32FloatPoint;
  out Indexes: TArrayOfInteger; out Bounds: TFloatRect);
var
  Temp: TArrayOfInteger;

  procedure Merge(I1, I2, J1, J2: Integer);
  var
    I, J, K: Integer;
  begin
    if Values[Indexes[I2]].Point.X < Values[Indexes[J1]].Point.X then
      Exit;
    I := I1;
    J := J1;
    K := 0;
    repeat
      if Values[Indexes[I]].Point.X < Values[Indexes[J]].Point.X then
      begin
        Temp[K] := Indexes[I];
        Inc(I);
      end
      else
      begin
        Temp[K] := Indexes[J];
        Inc(J);
      end;
      Inc(K);
    until (I > I2) or (J > J2);

    while I <= I2 do
    begin
      Temp[K] := Indexes[I];
      Inc(I); Inc(K);
    end;
    while J <= J2 do
    begin
      Temp[K] := Indexes[J];
      Inc(J); Inc(K);
    end;
    for I := 0 to K - 1 do
    begin
      Indexes[I + I1] := Temp[I];
    end;
  end;

  procedure Recurse(I1, I2: Integer);
  var
    I, IX: Integer;
  begin
    if I1 = I2 then
      Indexes[I1] := I1
    else if Indexes[I1] = Indexes[I2] then
    begin
      if Values[I1].Point.X <= Values[I2].Point.X then
      begin
        for I := I1 to I2 do Indexes[I] := I;
      end
      else
      begin
        IX := I1 + I2;
        for I := I1 to I2 do Indexes[I] := IX - I;
      end;
    end
    else
    begin
      IX := (I1 + I2) div 2;
      Recurse(I1, IX);
      Recurse(IX + 1, I2);
      Merge(I1, IX, IX + 1, I2);
    end;
  end;

var
  I, Index, S: Integer;
begin
  SetLength(Temp, Length(Values));
  SetLength(Indexes, Length(Values));

  Index := 0;
  S := Math.Sign(Values[1].Point.X - Values[0].Point.X);
  if S = 0 then S := 1;

  Indexes[0] := 0;

  // initialize bounds
  Bounds.Left := Values[0].Point.X;
  Bounds.Top := Values[0].Point.Y;
  Bounds.Right := Bounds.Left;
  Bounds.Bottom := Bounds.Top;

  for I := 1 to High(Values) do
  begin
    if Math.Sign(Values[I].Point.X - Values[I - 1].Point.X) = -S then
    begin
      S := -S;
      Inc(Index);
    end;

    // determine bounds
    if Values[I].Point.X < Bounds.Left then
      Bounds.Left := Values[I].Point.X;
    if Values[I].Point.Y < Bounds.Top then
      Bounds.Top := Values[I].Point.Y;
    if Values[I].Point.X > Bounds.Right then
      Bounds.Right := Values[I].Point.X;
    if Values[I].Point.Y > Bounds.Bottom then
      Bounds.Bottom := Values[I].Point.Y;

    Indexes[I] := Index;
  end;

  Recurse(0, High(Values));
end;

function DelaunayTriangulation(Points: TArrayOfColor32FloatPoint): TArrayOfTriangleVertexIndices;
var
  Complete: array of Byte;
  Edges: array of array [0 .. 1] of Integer;
  ByteIndex, Bit: Byte;
  MaxVerticesCount, EdgeCount, MaxEdgeCount, MaxTriangleCount: Integer;

  // For super triangle
  ScaledDeltaMax: TFloat;
  Mid: TFloatPoint;
  Bounds: TFloatRect;

  // General Variables
  SortedVertexIndices: TArrayOfInteger;
  TriangleCount, VertexCount, I, J, K: Integer;
  CenterX, CenterY, RadSqr: TFloat;
  Inside: Boolean;
const
  CSuperTriangleCount = 3; // -> super triangle
  CTolerance = 0.000001;

  function InCircle(Pt, Pt1, Pt2, Pt3: TFloatPoint): Boolean;
  // Return TRUE if the point Pt(x, y) lies inside the circumcircle made up by
  // points Pt1(x, y) Pt2(x, y) Pt3(x, y)
  // The circumcircle centre is returned in (CenterX, CenterY) and the radius r
  // NOTE: A point on the edge is inside the circumcircle
  var
    M1, M2, MX1, MY1, MX2, MY2: Double;
    DeltaX, DeltaY, DeltaRadSqr, AbsY1Y2, AbsY2Y3: Double;
  begin
    AbsY1Y2 := Abs(Pt1.Y - Pt2.Y);
    AbsY2Y3 := Abs(Pt2.Y - Pt3.Y);

    // check for coincident points
    if (AbsY1Y2 < CTolerance) and (AbsY2Y3 < CTolerance) then
    begin
      Result := False;
      Exit;
    end;

    if AbsY1Y2 < CTolerance then
    begin
      M2 := -(Pt3.X - Pt2.X) / (Pt3.Y - Pt2.Y);
      MX2 := (Pt2.X + Pt3.X) * 0.5;
      MY2 := (Pt2.Y + Pt3.Y) * 0.5;
      CenterX := (Pt2.X + Pt1.X) * 0.5;
      CenterY := M2 * (CenterX - MX2) + MY2;
    end
    else if AbsY2Y3 < CTolerance then
    begin
      M1 := -(Pt2.X - Pt1.X) / (Pt2.Y - Pt1.Y);
      MX1 := (Pt1.X + Pt2.X) * 0.5;
      MY1 := (Pt1.Y + Pt2.Y) * 0.5;
      CenterX := (Pt3.X + Pt2.X) * 0.5;
      CenterY := M1 * (CenterX - MX1) + MY1;
    end
    else
    begin
      M1 := -(Pt2.X - Pt1.X) / (Pt2.Y - Pt1.Y);
      M2 := -(Pt3.X - Pt2.X) / (Pt3.Y - Pt2.Y);
      if Abs(M1 - M2) < CTolerance then
      begin
        Result := False;
        Exit;
      end;
      MX1 := (Pt1.X + Pt2.X) * 0.5;
      MX2 := (Pt2.X + Pt3.X) * 0.5;
      MY1 := (Pt1.Y + Pt2.Y) * 0.5;
      MY2 := (Pt2.Y + Pt3.Y) * 0.5;
      CenterX := (M1 * MX1 - M2 * Mx2 + My2 - MY1) / (M1 - M2);
      if (AbsY1Y2 > AbsY2Y3) then
        CenterY := M1 * (CenterX - MX1) + MY1
      else
        CenterY := M2 * (CenterX - MX2) + MY2;
    end;

    DeltaX := Pt2.X - CenterX;
    DeltaY := Pt2.Y - CenterY;
    RadSqr := DeltaX * DeltaX + DeltaY * DeltaY;
    DeltaX := Pt.X - CenterX;
    DeltaY := Pt.Y - CenterY;
    DeltaRadSqr := Sqr(DeltaX) + Sqr(DeltaY);

    Result := (DeltaRadSqr - RadSqr) <= CTolerance;
  end;

begin
  VertexCount := Length(Points);
  MaxVerticesCount := VertexCount + CSuperTriangleCount;

  // Sort points by x value and find maximum and minimum vertex bounds.
  FastMergeSortX(Points, SortedVertexIndices, Bounds);

  SetLength(Points, MaxVerticesCount);
  MaxTriangleCount := 2 * (MaxVerticesCount - 1);
  SetLength(Result, MaxTriangleCount);
  MaxEdgeCount := 3 * (MaxVerticesCount - 1);
  SetLength(Edges, MaxEdgeCount);
  SetLength(Complete, (MaxTriangleCount + 7) shr 3);

  // This is to allow calculation of the bounding triangle
  with Bounds do
  begin
    ScaledDeltaMax := 30 * Max(Right - Left, Bottom - Top);
    Mid := FloatPoint((Left + Right) * 0.5, (Top + Bottom) * 0.5);
  end;

  // Set up the super triangle
  // This is a triangle which encompasses all the sample points. The super
  // triangle coordinates are added to the end of the vertex list. The super
  // triangle is the first triangle in the triangle list.
  Points[VertexCount].Point := FloatPoint(Mid.X - ScaledDeltaMax, Mid.Y - ScaledDeltaMax);
  Points[VertexCount + 1].Point := FloatPoint(Mid.X + ScaledDeltaMax, Mid.Y);
  Points[VertexCount + 2].Point := FloatPoint(Mid.X, Mid.Y + ScaledDeltaMax);

  Result[0, 0] := VertexCount;
  Result[0, 1] := VertexCount + 1;
  Result[0, 2] := VertexCount + 2;

  Complete[0] := 0;
  TriangleCount := 1;

  // Include each point one at a time into the existing mesh
  for I := 0 to VertexCount - 1 do
  begin
    EdgeCount := 0;

    // Set up the edge buffer.
    // If the point [x, y] lies inside the circumcircle then the hree edges of
    // that triangle are added to the edge buffer.
    J := 0;
    repeat
      if Complete[J shr 3] and (1 shl (J and $7)) = 0 then
      begin
        Inside := InCircle(Points[SortedVertexIndices[I]].Point,
          Points[Result[J, 0]].Point, Points[Result[J, 1]].Point,
          Points[Result[J, 2]].Point);

        ByteIndex := J shr 3;
        Bit := 1 shl (J and $7);
        if (CenterX < Points[SortedVertexIndices[I]].Point.X) and
          ((Sqr(Points[SortedVertexIndices[I]].Point.X - CenterX)) > RadSqr) then
          Complete[ByteIndex] := Complete[ByteIndex] or Bit
        else
          if Inside then
          begin
            Edges[EdgeCount + 0, 0] := Result[J, 0];
            Edges[EdgeCount + 0, 1] := Result[J, 1];
            Edges[EdgeCount + 1, 0] := Result[J, 1];
            Edges[EdgeCount + 1, 1] := Result[J, 2];
            Edges[EdgeCount + 2, 0] := Result[J, 2];
            Edges[EdgeCount + 2, 1] := Result[J, 0];
            EdgeCount := EdgeCount + 3;
            Assert(EdgeCount <= MaxEdgeCount);

            TriangleCount := TriangleCount - 1;
            Result[J] := Result[TriangleCount];

            Complete[ByteIndex] := (Complete[ByteIndex] and (not Bit))
              or (Complete[TriangleCount shr 3] and Bit);
            Continue;
          end;
      end;
      J := J + 1;
    until J >= TriangleCount;

    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    for J := 0 to EdgeCount - 2 do
    begin
      if (Edges[J, 0] <> -1) or (Edges[J, 1] <> -1) then
      begin
        for K := J + 1 to EdgeCount - 1 do
        begin
          if (Edges[K, 0] <> -1) or (Edges[K, 1] <> -1) then
          begin
            if (Edges[J, 0] = Edges[K, 1]) and
              (Edges[J, 1] = Edges[K, 0]) then
            begin
              Edges[J, 0] := -1;
              Edges[J, 1] := -1;
              Edges[K, 1] := -1;
              Edges[K, 0] := -1;
            end;
          end;
        end;
      end;
    end;

    // Form new triangles for the current point.
    // Skipping over any tagged edges. All edges are arranged in clockwise
    // order.
    for J := 0 to EdgeCount - 1 do
    begin
      if (Edges[J, 0] <> -1) or (Edges[J, 1] <> -1) then
      begin
        Result[TriangleCount, 0] := Edges[J, 0];
        Result[TriangleCount, 1] := Edges[J, 1];
        Result[TriangleCount, 2] := SortedVertexIndices[I];
        ByteIndex := TriangleCount shr 3;
        Bit := 1 shl (TriangleCount and $7);
        Complete[ByteIndex] := Complete[ByteIndex] and not Bit;
        Inc(TriangleCount);
        Assert(TriangleCount <= MaxTriangleCount);
      end;
    end;
  end;

  // Remove triangles with supertriangle vertices
  // These are triangles which have a vertex number greater than VertexCount
  I := 0;
  repeat
    if (Result[I, 0] >= VertexCount) or
      (Result[I, 1] >= VertexCount) or
      (Result[I, 2] >= VertexCount) then
    begin
      TriangleCount := TriangleCount - 1;
      Result[I, 0] := Result[TriangleCount, 0];
      Result[I, 1] := Result[TriangleCount, 1];
      Result[I, 2] := Result[TriangleCount, 2];
      I := I - 1;
    end;
    I := I + 1;
  until I >= TriangleCount;

  SetLength(Points, Length(Points) - 3);
  SetLength(Result, TriangleCount);
end;

procedure TGourandShadedDelaunayTrianglesSampler.PrepareSampling;
var
  Index: Integer;
begin
  inherited;

  // perform triangulation
  FTriangles := DelaunayTriangulation(FColorPoints);

  // setup internal barycentric samplers
  SetLength(FBarycentric, Length(FTriangles));
  for Index := 0 to Length(FTriangles) - 1 do
  begin
    FBarycentric[Index] := TBarycentricGradientSampler.Create(
      FColorPoints[FTriangles[Index, 0]], FColorPoints[FTriangles[Index, 1]],
      FColorPoints[FTriangles[Index, 2]]);
    FBarycentric[Index].PrepareSampling;
  end;
  SetLength(FTriangles, 0);
end;

function TGourandShadedDelaunayTrianglesSampler.GetSampleFloat(X, Y: TFloat): TColor32;
var
  Index: Integer;
  U, V, W: TFloat;
  Dist, MinDist: TFloat;
  MinIndex: Integer;
begin
  if Length(FBarycentric) = 0 then
  begin
    Result := clRed32;
    Exit;
  end;

  // check first barycentric interpolator
  FBarycentric[0].CalculateBarycentricCoordinates(X, Y, U, V, W);
  if (U >= 0) and (V >= 0) and (W >= 0) then
  begin
    Result := Linear3PointInterpolationProc(FBarycentric[0].Color[0],
      FBarycentric[0].Color[1], FBarycentric[0].Color[2], U, V, W);
    Exit;
  end;

  // calculate minimum distance
  MinDist := Sqr(U - 0.5) + Sqr(V - 0.5) + Sqr(W - 0.5);
  MinIndex := 0;

  for Index := 1 to High(FBarycentric) do
  begin
    // check barycentric interpolator
    FBarycentric[Index].CalculateBarycentricCoordinates(X, Y, U, V, W);
    if (U >= 0) and (V >= 0) and (W >= 0) then
    begin
      Result := Linear3PointInterpolationProc(FBarycentric[Index].Color[0],
        FBarycentric[Index].Color[1], FBarycentric[Index].Color[2], U, V, W);
      Exit;
    end;

    // calculate distance and eventually update minimum distance
    Dist := Sqr(U - 0.5) + Sqr(V - 0.5) + Sqr(W - 0.5);
    if Dist < MinDist then
    begin
      MinDist := Dist;
      MinIndex := Index;
    end;
  end;

  FBarycentric[MinIndex].CalculateBarycentricCoordinates(X, Y, U, V, W);
  Result := Linear3PointInterpolationProc(FBarycentric[MinIndex].Color[0],
    FBarycentric[MinIndex].Color[1], FBarycentric[MinIndex].Color[2], U, V, W);
end;

procedure TGourandShadedDelaunayTrianglesSampler.FinalizeSampling;
var
  Index: Integer;
begin
  inherited;
  for Index := 0 to Length(FBarycentric) - 1 do
  begin
    FBarycentric[Index].FinalizeSampling;
    FBarycentric[Index].Free;
  end;
end;


{ TCustomGradientSampler }

constructor TCustomGradientSampler.Create(WrapMode: TWrapMode);
begin
  inherited Create;
  FGradient := TColor32Gradient.Create(clNone32);
  FGradient.OnGradientColorsChanged := GradientChangedHandler;
  FWrapMode := WrapMode;
  WrapModeChanged;
end;

constructor TCustomGradientSampler.Create(ColorGradient: TColor32Gradient);
begin
  Create;

  if Assigned(ColorGradient) then
    FGradient.Assign(ColorGradient);
end;

destructor TCustomGradientSampler.Destroy;
begin
  FreeAndNil(FGradient);
  inherited;
end;

procedure TCustomGradientSampler.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomGradientSampler then
    with TCustomGradientSampler(Dest) do
    begin
      FGradient.Assign(Self.FGradient);
      FInitialized := False;
      FWrapMode := Self.WrapMode;
    end
  else
    inherited;
end;

procedure TCustomGradientSampler.SetGradient(const Value: TColor32Gradient);
begin
  if not Assigned(Value) then
    FGradient.ClearColorStops
  else
    Value.AssignTo(Self);
  GradientSamplerChanged;
end;

procedure TCustomGradientSampler.SetWrapMode(const Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    WrapModeChanged;
  end;
end;

procedure TCustomGradientSampler.WrapModeChanged;
begin
end;

function TCustomGradientSampler.GetSampleFixed(X, Y: TFixed): TColor32;
begin
  Result := GetSampleFloat(X * FixedToFloat, Y * FixedToFloat);
end;

function TCustomGradientSampler.GetSampleInt(X, Y: Integer): TColor32;
begin
  Result := GetSampleFloat(X, Y);
end;

procedure TCustomGradientSampler.GradientChangedHandler(Sender: TObject);
begin
  GradientSamplerChanged;
end;

procedure TCustomGradientSampler.GradientSamplerChanged;
begin
  FInitialized := False;
end;

procedure TCustomGradientSampler.PrepareSampling;
begin
  inherited;

  if not FInitialized then
  begin
    UpdateInternals;
    FInitialized := True;
  end;
end;


{ TCustomGradientLookUpTableSampler }

procedure TCustomGradientLookUpTableSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomGradientLookUpTableSampler then
    with TCustomGradientLookUpTableSampler(Dest) do
    begin
      FGradientLUT.Assign(Self.FGradientLUT);
      FWrapProc := Self.FWrapProc;
    end
end;

constructor TCustomGradientLookUpTableSampler.Create(WrapMode: TWrapMode = wmMirror);
begin
  FGradientLUT := TColor32LookupTable.Create;
  inherited Create(WrapMode);
end;

destructor TCustomGradientLookUpTableSampler.Destroy;
begin
  FGradientLUT.Free;
  inherited;
end;

procedure TCustomGradientLookUpTableSampler.UpdateInternals;
begin
  FGradient.FillColorLookUpTable(FGradientLUT);
  FLutPtr := FGradientLUT.Color32Ptr;
  FLutMask := FGradientLUT.Mask;
  FWrapProc := GetWrapProc(WrapMode, FGradientLUT.Mask);
end;

procedure TCustomGradientLookUpTableSampler.WrapModeChanged;
begin
  inherited;
  FWrapProc := GetWrapProc(WrapMode);
end;


{ TCustomCenterLutGradientSampler }

constructor TCustomCenterLutGradientSampler.Create(WrapMode: TWrapMode = wmMirror);
begin
  inherited Create(WrapMode);
  FCenter := FloatPoint(0, 0);
end;

procedure TCustomCenterLutGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomCenterLutGradientSampler then
    TCustomCenterLutGradientSampler(Dest).FCenter := Self.FCenter;
end;

procedure TCustomCenterLutGradientSampler.Transform(var X, Y: TFloat);
begin
  X := X - FCenter.X;
  Y := Y - FCenter.Y;
  inherited;
end;

{ TConicGradientSampler }

procedure TConicGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TConicGradientSampler then
    TConicGradientSampler(Dest).FAngle := Self.FAngle;
end;

function TConicGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(FScale * Abs(FAngle + ArcTan2(Y, X))),
    FLutMask)];
end;

procedure TConicGradientSampler.UpdateInternals;
begin
  inherited;
  FLutMask := FGradientLUT.Mask;
  FScale := FLutMask / Pi;
end;


{ TCustomCenterRadiusLutGradientSampler }

constructor TCustomCenterRadiusLutGradientSampler.Create(WrapMode: TWrapMode = wmMirror);
begin
  inherited Create(WrapMode);
  FRadius := 1;
  RadiusChanged;
end;

procedure TCustomCenterRadiusLutGradientSampler.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomCenterRadiusLutGradientSampler then
    TCustomCenterRadiusLutGradientSampler(Dest).FRadius := Self.FRadius;
end;

procedure TCustomCenterRadiusLutGradientSampler.RadiusChanged;
begin
  FInitialized := False;
end;

procedure TCustomCenterRadiusLutGradientSampler.SetRadius(
  const Value: TFloat);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    RadiusChanged;
  end;
end;


{ TRadialGradientSampler }

function TRadialGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FGradientLUT.Color32Ptr^[
    FWrapProc(Round(Sqrt(Sqr(X) + Sqr(Y)) * FScale), FLutMask)];
end;

procedure TRadialGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{ TCustomCenterRadiusAngleLutGradientSampler }

constructor TCustomCenterRadiusAngleLutGradientSampler.Create(WrapMode: TWrapMode = wmMirror);
begin
  inherited Create(WrapMode);
  FAngle := 0;
  FSinCos.X := 1;
  FSinCos.Y := 0;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.AssignTo(
  Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomCenterRadiusAngleLutGradientSampler then
    with TCustomCenterRadiusAngleLutGradientSampler(Dest) do
    begin
      FAngle := Self.FAngle;
      FSinCos := Self.FSinCos;
    end;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.RadiusChanged;
begin
  inherited;
  FInitialized := False;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.AngleChanged;
begin
  GR32_Math.SinCos(FAngle, FSinCos.X, FSinCos.Y);
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.SetAngle(
  const Value: TFloat);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    AngleChanged;
  end;
end;

procedure TCustomCenterRadiusAngleLutGradientSampler.Transform(var X,
  Y: TFloat);
var
  Temp: TFloat;
begin
  X := X - FCenter.X;
  Y := Y - FCenter.Y;

  Temp := X * FSinCos.X + Y * FSinCos.Y;
  Y := X * FSinCos.Y - Y * FSinCos.X;
  X := Temp;
end;


{ TDiamondGradientSampler }

function TDiamondGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(Max(Abs(X), Abs(Y)) * FScale), FLutMask)];
end;

procedure TDiamondGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{ TXGradientSampler }

function TXGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(X * FScale), FLutMask)];
end;

function TXGradientSampler.GetStartPoint: TFloatPoint;
begin
  Result := FCenter;
end;

function TXGradientSampler.GetEndPoint: TFloatPoint;
var
  X, Y: TFloat;
begin
  GR32_Math.SinCos(Angle - 0.5 * Pi, X, Y);
  Result := FloatPoint(FCenter.X + X, FCenter.Y + Y);
end;

procedure TXGradientSampler.SetEndPoint(const Value: TFloatPoint);
begin
  SetPoints(StartPoint, Value);
end;

procedure TXGradientSampler.SetPoints(const StartPoint, EndPoint: TFloatPoint);
begin
  FCenter := StartPoint;
  Radius := Distance(EndPoint, StartPoint);
  Angle := 0.5 * Pi + GetAngleOfPt2FromPt1(EndPoint, StartPoint);
end;

procedure TXGradientSampler.SetStartPoint(const Value: TFloatPoint);
begin
  SetPoints(Value, EndPoint);
end;

procedure TXGradientSampler.SimpleGradient(
  const StartPoint: TFloatPoint; StartColor: TColor32;
  const EndPoint: TFloatPoint; EndColor: TColor32);
begin
  SetPoints(StartPoint, EndPoint);
  if Assigned(FGradient) then
  begin
    FGradient.ClearColorStops;
    FGradient.StartColor := StartColor;
    FGradient.EndColor := EndColor;
  end;
end;

procedure TXGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{ TXYGradientSampler }

function TXYGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round((Abs(X) * Abs(Y)) * FScale), FLutMask)];
end;

procedure TXYGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / Sqr(FRadius);
end;


{ TXYSqrtGradientSampler }

function TXYSqrtGradientSampler.GetSampleFloat(X, Y: TFloat): TColor32;
begin
  Transform(X, Y);
  Result := FLutPtr^[FWrapProc(Round(Sqrt(Abs(X) * Abs(Y)) * FScale), FLutMask)];
end;

procedure TXYSqrtGradientSampler.UpdateInternals;
begin
  inherited;
  FScale := FLutMask / FRadius;
end;


{TCustomGradientPolygonFiller}

constructor TCustomGradientPolygonFiller.Create;
begin
  Create(TColor32Gradient.Create(clNone32));
  FGradient.OnGradientColorsChanged := GradientColorsChangedHandler;
  FOwnsGradient := True;
  FWrapMode := wmClamp;
  FWrapProc := Clamp;
end;

constructor TCustomGradientPolygonFiller.Create(ColorGradient: TColor32Gradient);
begin
  FOwnsGradient := False;
  FGradient := ColorGradient;
  inherited Create;
  FWrapMode := wmClamp;
  FWrapProc := Clamp;
end;

destructor TCustomGradientPolygonFiller.Destroy;
begin
  if Assigned(FGradient) then
    if FOwnsGradient then
      FGradient.Free
    else
      FGradient.OnGradientColorsChanged := nil;
  inherited;
end;

procedure TCustomGradientPolygonFiller.FillLineNone(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
begin
  // do nothing!
end;

procedure TCustomGradientPolygonFiller.FillLineSolid(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
begin
  FillLineAlpha(Dst, AlphaValues, Length, FGradient.StartColor, CombineMode);
end;

procedure TCustomGradientPolygonFiller.GradientColorsChangedHandler(
  Sender: TObject);
begin
  GradientFillerChanged;
end;

procedure TCustomGradientPolygonFiller.GradientFillerChanged;
begin
  // do nothing
end;

procedure TCustomGradientPolygonFiller.SetWrapMode(const Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    WrapModeChanged;
  end;
end;

procedure TCustomGradientPolygonFiller.WrapModeChanged;
begin
  FWrapProc := GetWrapProc(FWrapMode);
end;


{ TBarycentricGradientPolygonFiller }

procedure TBarycentricGradientPolygonFiller.BeginRendering;
var
  NormScale: TFloat;
begin
  inherited;
  NormScale := 1 / ((FColorPoints[1].Point.Y - FColorPoints[2].Point.Y) *
    (FColorPoints[0].Point.X - FColorPoints[2].Point.X) +
    (FColorPoints[2].Point.X - FColorPoints[1].Point.X) *
    (FColorPoints[0].Point.Y - FColorPoints[2].Point.Y));

  FDists[0].X := NormScale * (FColorPoints[2].Point.X - FColorPoints[1].Point.X);
  FDists[0].Y := NormScale * (FColorPoints[1].Point.Y - FColorPoints[2].Point.Y);
  FDists[1].X := NormScale * (FColorPoints[0].Point.X - FColorPoints[2].Point.X);
  FDists[1].Y := NormScale * (FColorPoints[2].Point.Y - FColorPoints[0].Point.Y);
end;

procedure TBarycentricGradientPolygonFiller.FillLine(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  Temp, DotY1, DotY2: TFloat;
  Barycentric: array [0..1] of TFloat;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  Temp := DstY - FColorPoints[2].Point.Y;
  DotY1 := FDists[0].X * Temp;
  DotY2 := FDists[1].X * Temp;
  for X := DstX to DstX + Length - 1 do
  begin
    Temp := (X - FColorPoints[2].Point.X);
    Barycentric[0] := FDists[0].Y * Temp + DotY1;
    Barycentric[1] := FDists[1].Y * Temp + DotY2;

    Color32 := Linear3PointInterpolationProc(FColorPoints[0].Color32,
      FColorPoints[1].Color32, FColorPoints[2].Color32,
      Barycentric[0], Barycentric[1], 1 - Barycentric[1] - Barycentric[0]);

    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TBarycentricGradientPolygonFiller.GetColor(Index: Integer): TColor32;
begin
  if Index in [0 .. 2] then
    Result := FColorPoints[Index].Color32
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBarycentricGradientPolygonFiller.GetColorPoint(
  Index: Integer): TColor32FloatPoint;
begin
  if Index in [0 .. 2] then
    Result := FColorPoints[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TBarycentricGradientPolygonFiller.GetCount: Integer;
begin
  Result := 3;
end;

function TBarycentricGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLine;
end;

function TBarycentricGradientPolygonFiller.GetPoint(
  Index: Integer): TFloatPoint;
begin
  if Index in [0 .. 2] then
    Result := FColorPoints[Index].Point
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

class function TBarycentricGradientPolygonFiller.Linear3PointInterpolation(
  A, B, C: TColor32; WeightA, WeightB, WeightC: Single): TColor32;
begin
  Result := Linear3PointInterpolationProc(A, B, C, WeightA, WeightB, WeightC);
end;

procedure TBarycentricGradientPolygonFiller.SetColor(Index: Integer;
  const Value: TColor32);
begin
  if Index in [0 .. 2] then
    FColorPoints[Index].Color32 := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientPolygonFiller.SetColorPoints(
  ColorPoints: TArrayOfColor32FloatPoint);
begin
  if Length(ColorPoints) <> 3 then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0] := ColorPoints[0];
  FColorPoints[1] := ColorPoints[1];
  FColorPoints[2] := ColorPoints[2];
end;

procedure TBarycentricGradientPolygonFiller.SetColorPoint(Index: Integer;
  const Value: TColor32FloatPoint);
begin
  if Index in [0 .. 2] then
    FColorPoints[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientPolygonFiller.SetColorPoints(
  Points: TArrayOfFloatPoint; Colors: TArrayOfColor32);
begin
  if (Length(Points) <> 3) or (Length(Colors) <> 3) then
    raise Exception.Create(RCStrOnlyExactly3Point);

  FColorPoints[0] := Color32FloatPoint(Colors[0], Points[0]);
  FColorPoints[1] := Color32FloatPoint(Colors[1], Points[1]);
  FColorPoints[2] := Color32FloatPoint(Colors[2], Points[2]);
end;

procedure TBarycentricGradientPolygonFiller.SetPoint(Index: Integer;
  const Value: TFloatPoint);
begin
  if Index in [0 .. 2] then
    FColorPoints[Index].Point := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TBarycentricGradientPolygonFiller.SetPoints(
  Points: TArrayOfFloatPoint);
var
  Index: Integer;
begin
  if Length(Points) <> 3 then
    raise Exception.Create(RCStrOnlyExactly3Point);

  for Index := 0 to 2 do
    FColorPoints[Index].Point := Points[Index];
end;


{ TCustomArbitrarySparsePointGradientPolygonFiller }

procedure TCustomArbitrarySparsePointGradientPolygonFiller.Add(
  const Point: TFloatPoint;
  Color: TColor32);
var
  Index: Integer;
begin
  Index := Length(FColorPoints);
  SetLength(FColorPoints, Index + 1);
  FColorPoints[Index].Point := Point;
  FColorPoints[Index].Color32 := Color;
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.Add(
  const ColorPoint: TColor32FloatPoint);
var
  Index: Integer;
begin
  Index := Length(FColorPoints);
  SetLength(FColorPoints, Index + 1);
  FColorPoints[Index].Point := ColorPoint.Point;
  FColorPoints[Index].Color32 := ColorPoint.Color32;
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.Clear;
begin
  SetLength(FColorPoints, 0);
end;

function TCustomArbitrarySparsePointGradientPolygonFiller.GetColor(
  Index: Integer): TColor32;
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    Result := FColorPoints[Index].Color32
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomArbitrarySparsePointGradientPolygonFiller.GetColorPoint(
  Index: Integer): TColor32FloatPoint;
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    Result := FColorPoints[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomArbitrarySparsePointGradientPolygonFiller.GetCount: Integer;
begin
  Result := Length(FColorPoints);
end;

function TCustomArbitrarySparsePointGradientPolygonFiller.GetPoint(
  Index: Integer): TFloatPoint;
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    Result := FColorPoints[Index].Point
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.SetColor(Index: Integer;
  const Value: TColor32);
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    FColorPoints[Index].Color32 := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.SetColorPoint(
  Index: Integer; const Value: TColor32FloatPoint);
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    FColorPoints[Index] := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.SetPoint(Index: Integer;
  const Value: TFloatPoint);
begin
  if (Index >= 0) and (Index < Length(FColorPoints)) then
    FColorPoints[Index].Point := Value
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.SetColorPoints(
  ColorPoints: TArrayOfColor32FloatPoint);
var
  Index: Integer;
begin
  SetLength(FColorPoints, Length(ColorPoints));
  for Index := 0 to High(FColorPoints) do
    FColorPoints[Index] := ColorPoints[Index];
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.SetColorPoints(
  Points: TArrayOfFloatPoint; Colors: TArrayOfColor32);
var
  Index: Integer;
begin
  if Length(Points) <> Length(Colors) then
    raise Exception.Create(RCStrPointCountMismatch);

  SetLength(FColorPoints, Length(Points));
  for Index := 0 to High(FColorPoints) do
  begin
    FColorPoints[Index].Point := Points[Index];
    FColorPoints[Index].Color32 := Colors[Index];
  end;
end;

procedure TCustomArbitrarySparsePointGradientPolygonFiller.SetPoints(
  Points: TArrayOfFloatPoint);
var
  Index: Integer;
begin
  if Length(FColorPoints) <> Length(Points) then
    raise Exception.Create(RCStrPointCountMismatch);

  for Index := 0 to High(Points) do
    FColorPoints[Index].Point := Points[Index];
end;


{ TGourandShadedDelaunayTrianglesPolygonFiller }

procedure TGourandShadedDelaunayTrianglesPolygonFiller.BeginRendering;
var
  Index: Integer;
begin
  inherited;

  // perform triangulation
  FTriangles := DelaunayTriangulation(FColorPoints);

  // setup internal barycentric samplers
  SetLength(FBarycentric, Length(FTriangles));
  for Index := 0 to Length(FTriangles) - 1 do
  begin
    FBarycentric[Index] := TBarycentricGradientSampler.Create(
      FColorPoints[FTriangles[Index, 0]], FColorPoints[FTriangles[Index, 1]],
      FColorPoints[FTriangles[Index, 2]]);
    FBarycentric[Index].PrepareSampling;
  end;
  SetLength(FTriangles, 0);
end;

procedure TGourandShadedDelaunayTrianglesPolygonFiller.FillLine3(Dst: PColor32;
  DstX, DstY, Count: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := DstX to DstX + Count - 1 do
  begin
    BlendMemEx(FBarycentric[0].GetSampleFloat(X, DstY), Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TGourandShadedDelaunayTrianglesPolygonFiller.FillLine(Dst: PColor32;
  DstX, DstY, Count: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  Index: Integer;
  U, V, W: TFloat;
  Dist, MinDist: TFloat;
  MinIndex: Integer;

  X: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;

label
  DrawColor;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  for X := DstX to DstX + Count - 1 do
  begin
    // check first barycentric interpolator
    FBarycentric[0].CalculateBarycentricCoordinates(X, DstY, U, V, W);
    if (U >= 0) and (V >= 0) and (W >= 0) then
    begin
      Color32 := Linear3PointInterpolationProc(FBarycentric[0].Color[0],
        FBarycentric[0].Color[1], FBarycentric[0].Color[2], U, V, W);
      goto DrawColor;
    end;

    // calculate minimum distance
    MinDist := Sqr(U - 0.5) + Sqr(V - 0.5) + Sqr(W - 0.5);
    MinIndex := 0;

    for Index := 1 to High(FBarycentric) do
    begin
      // check barycentric interpolator
      FBarycentric[Index].CalculateBarycentricCoordinates(X, DstY, U, V, W);
      if (U >= 0) and (V >= 0) and (W >= 0) then
      begin
        Color32 := Linear3PointInterpolationProc(FBarycentric[Index].Color[0],
          FBarycentric[Index].Color[1], FBarycentric[Index].Color[2], U, V, W);
        goto DrawColor;
      end;

      // calculate distance and eventually update minimum distance
      Dist := Sqr(U - 0.5) + Sqr(V - 0.5) + Sqr(W - 0.5);
      if Dist < MinDist then
      begin
        MinDist := Dist;
        MinIndex := Index;
      end;
    end;

    FBarycentric[MinIndex].CalculateBarycentricCoordinates(X, DstY, U, V, W);
    Color32 := Linear3PointInterpolationProc(FBarycentric[MinIndex].Color[0],
      FBarycentric[MinIndex].Color[1], FBarycentric[MinIndex].Color[2], U, V, W);

DrawColor:
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TGourandShadedDelaunayTrianglesPolygonFiller.GetFillLine: TFillLineEvent;
begin
  case Count of
    0 .. 2:
      raise Exception.Create('Too few color points available');
    3:
      Result := FillLine3;
  else
    Result := FillLine;
  end;
end;


{ TCustomGradientLookupTablePolygonFiller }

constructor TCustomGradientLookupTablePolygonFiller.Create;
begin
  inherited Create;

  FUseLookUpTable := True;

  // eventually create lookup table if not specified otherwise
  if not Assigned(FGradientLUT) then
  begin
    FGradientLUT := TColor32LookupTable.Create;
    FGradientLUT.OnOrderChanged := LookUpTableChangedHandler;
    FOwnsLUT := True;
  end;
end;

constructor TCustomGradientLookupTablePolygonFiller.Create(
  LookupTable: TColor32LookupTable);
begin
  if not Assigned(LookupTable) then
    raise Exception.Create(RCStrNoLookupTablePassed);

  FGradientLUT := LookupTable;
  FUseLookUpTable := True;
  FOwnsLUT := False;
  FGradient := nil;
  FOwnsGradient := False;
  FWrapMode := wmClamp;
  FWrapProc := Clamp;
end;

destructor TCustomGradientLookupTablePolygonFiller.Destroy;
begin
  if FOwnsLUT and Assigned(FGradientLUT) then
    FGradientLUT.Free;
  inherited;
end;

function TCustomGradientLookupTablePolygonFiller.GetLUTNeedsUpdate: Boolean;
begin
  Result := FLUTNeedsUpdate or (FUseLookUpTable and (not FOwnsLUT));
end;

procedure TCustomGradientLookupTablePolygonFiller.GradientFillerChanged;
begin
  FLUTNeedsUpdate := True;
end;

procedure TCustomGradientLookupTablePolygonFiller.SetGradientLUT(
  const Value: TColor32LookupTable);
begin
  if FGradientLUT <> Value then
  begin
    // check whether current look up table is owned and eventually free it
    if FOwnsLUT then
      FGradientLUT.Free;

    // set link to passed look up table
    FGradientLUT := Value;

    // if no look up table is specified don't use a look up table
    if not Assigned(FGradientLUT) then
      UseLookUpTable := False;
  end;
end;

procedure TCustomGradientLookupTablePolygonFiller.SetUseLookUpTable(
  const Value: Boolean);
begin
  if FUseLookUpTable <> Value then
  begin
    FUseLookUpTable := Value;
    UseLookUpTableChanged;
  end;
end;

procedure TCustomGradientLookupTablePolygonFiller.UseLookUpTableChanged;
begin
  if FUseLookUpTable then
    if not Assigned(FGradientLUT) then
    begin
      FGradientLUT := TColor32LookupTable.Create;
      FGradientLUT.OnOrderChanged := LookUpTableChangedHandler;
      FOwnsLUT := True;
    end
    else
  else
    if FOwnsLUT then
    begin
      if Assigned(FGradientLUT) then
        FreeAndNil(FGradientLUT);
      FOwnsLUT := False;
    end
end;

procedure TCustomGradientLookupTablePolygonFiller.LookUpTableChangedHandler(Sender: TObject);
begin
  FLUTNeedsUpdate := True;
end;


{ TCustomLinearGradientPolygonFiller }

procedure TCustomLinearGradientPolygonFiller.SetStartPoint(
  const Value: TFloatPoint);
begin
  if (FStartPoint.X <> Value.X) or (FStartPoint.Y <> Value.Y) then
  begin
    FStartPoint := Value;
    StartPointChanged;
  end;
end;

procedure TCustomLinearGradientPolygonFiller.SimpleGradient(
  const StartPoint: TFloatPoint; StartColor: TColor32;
  const EndPoint: TFloatPoint; EndColor: TColor32);
begin
  SetPoints(StartPoint, EndPoint);
  if Assigned(FGradient) then
  begin
    FGradient.ClearColorStops;
    FGradient.StartColor := StartColor;
    FGradient.EndColor := EndColor;
  end;
end;

procedure TCustomLinearGradientPolygonFiller.SimpleGradientX(
  const StartX: TFloat; StartColor: TColor32; const EndX: TFloat;
  EndColor: TColor32);
begin
  SimpleGradient(
    FloatPoint(StartX, 0), StartColor,
    FloatPoint(EndX, 0), EndColor);
end;

procedure TCustomLinearGradientPolygonFiller.SimpleGradientY(
  const StartY: TFloat; StartColor: TColor32; const EndY: TFloat;
  EndColor: TColor32);
begin
  SimpleGradient(
    FloatPoint(0, StartY), StartColor,
    FloatPoint(0, EndY), EndColor);
end;

procedure TCustomLinearGradientPolygonFiller.SetEndPoint(
  const Value: TFloatPoint);
begin
  if (FEndPoint.X <> Value.X) or (FEndPoint.Y <> Value.Y) then
  begin
    FEndPoint := Value;
    EndPointChanged;
  end;
end;

procedure TCustomLinearGradientPolygonFiller.SetPoints(const StartPoint,
  EndPoint: TFloatPoint);
begin
  FStartPoint := StartPoint;
  FEndPoint := EndPoint;
  GradientFillerChanged;
  UpdateIncline;
end;

procedure TCustomLinearGradientPolygonFiller.StartPointChanged;
begin
  GradientFillerChanged;
  UpdateIncline;
end;

procedure TCustomLinearGradientPolygonFiller.EndPointChanged;
begin
  GradientFillerChanged;
  UpdateIncline;
end;

procedure TCustomLinearGradientPolygonFiller.UpdateIncline;
begin
  if (FEndPoint.X - FStartPoint.X) <> 0 then
    FIncline := (FEndPoint.Y - FStartPoint.Y) / (FEndPoint.X - FStartPoint.X)
  else
  if (FEndPoint.Y - FStartPoint.Y) <> 0 then
    FIncline := 1 / (FEndPoint.Y - FStartPoint.Y);
end;


{ TLinearGradientPolygonFiller }

constructor TLinearGradientPolygonFiller.Create(
  ColorGradient: TColor32Gradient);
begin
  Create(ColorGradient, True);
end;

constructor TLinearGradientPolygonFiller.Create(
  ColorGradient: TColor32Gradient; UseLookupTable: Boolean);
begin
  // create lookup table (and set 'own' & 'use' flags)
  FGradientLUT := TColor32LookupTable.Create;
  FGradientLUT.OnOrderChanged := LookUpTableChangedHandler;
  FOwnsLUT := True;
  FUseLookUpTable := UseLookupTable;

  inherited Create(ColorGradient);

  FGradient.OnGradientColorsChanged := GradientColorsChangedHandler;
end;

function TLinearGradientPolygonFiller.ColorStopToScanLine(Index,
  Y: Integer): TFloat;
var
  Offset: array [0 .. 1] of TFloat;
begin
  Offset[0] := FGradient.FGradientColors[Index].Offset;
  Offset[1] := 1 - Offset[0];
  Result := Offset[1] * FStartPoint.X + Offset[0] * FEndPoint.X + FIncline *
    (Offset[1] * (FStartPoint.Y - Y) + Offset[0] * (FEndPoint.Y - Y));
end;

procedure TLinearGradientPolygonFiller.UseLookUpTableChanged;
begin
  inherited;

  // perfect gradients are only implementd for WrapMode = wmClamp
  if (not FUseLookUpTable) and (WrapMode in [wmRepeat, wmMirror]) then
    WrapMode := wmClamp;
end;

procedure TLinearGradientPolygonFiller.WrapModeChanged;
begin
  inherited;

  // perfect gradients are only implementd for WrapMode = wmClamp
  if (not FUseLookUpTable) and (WrapMode in [wmRepeat, wmMirror]) then
    UseLookUpTable := True;
end;

function TLinearGradientPolygonFiller.GetFillLine: TFillLineEvent;
var
  GradientCount: Integer;
begin
  if Assigned(FGradient) then
    GradientCount := FGradient.GradientCount
  else
    GradientCount := FGradientLUT.Size;

  case GradientCount of
    0:
      Result := FillLineNone;
    1:
      Result := FillLineSolid;
    else
      if FUseLookUpTable then
        case FWrapMode of
          wmClamp:
            if FStartPoint.X = FEndPoint.X then
              if FStartPoint.Y = FEndPoint.Y then
                Result := FillLineVerticalPadExtreme
              else
                Result := FillLineVerticalPad
            else
            if FStartPoint.X < FEndPoint.X then
              Result := FillLineHorizontalPadPos
            else
              Result := FillLineHorizontalPadNeg;
          wmMirror, wmRepeat:
            if FStartPoint.X = FEndPoint.X then
              Result := FillLineVerticalWrap
            else
            if FStartPoint.X < FEndPoint.X then
              Result := FillLineHorizontalWrapPos
            else
              Result := FillLineHorizontalWrapNeg;
        end
      else
      if FStartPoint.X = FEndPoint.X then
        if FStartPoint.Y = FEndPoint.Y then
          Result := FillLineVerticalExtreme
        else
          Result := FillLineVertical
      else
      if FStartPoint.X < FEndPoint.X then
        Result := FillLinePositive
      else
        Result := FillLineNegative;
  end;
end;

procedure TLinearGradientPolygonFiller.FillLineVertical(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  Color32 := FGradient.GetColorAt((DstY - FStartPoint.Y) * FIncline);

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLineVerticalExtreme(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  if DstY < FStartPoint.Y then
    Color32 := FGradient.StartColor
  else
    Color32 := FGradient.EndColor;

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLinePositive(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index: Integer;
  IntScale, IntValue: Integer;
  Colors: array [0..1] of TColor32;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  XPos: array [0..2] of Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;

  // set first offset/position
  XOffset[0] := ColorStopToScanLine(0, DstY);
  XPos[0] := Round(XOffset[0]);
  XPos[2] := DstX + Length;

  // check if only a solid start color should be drawn.
  if XPos[0] >= XPos[2] - 1 then
  begin
    FillLineSolid(Dst, DstX, DstY, Length, AlphaValues, CombineMode);
    Exit;
  end;

  // set start color
  Colors[0] := FGradient.FGradientColors[0].Color32;

  // eventually draw solid start color
  FillLineAlpha(Dst, AlphaValues, XPos[0] - DstX, Colors[0], CombineMode);

  Index := 1;
  repeat
    // set start position to be at least DstX
    if XPos[0] < DstX then
      XPos[0] := DstX;

    // set destination color and offset
    Colors[1] := FGradient.FGradientColors[Index].Color32;
    XOffset[1] := ColorStopToScanLine(Index, DstY);

    // calculate destination pixel position
    XPos[1] := Round(XOffset[1]);
    if XPos[1] > XPos[2] then
      XPos[1] := XPos[2];

    // check whether
    if XPos[1] > XPos[0] then
    begin
      Scale := 1 / (XOffset[1] - XOffset[0]);
      IntScale := Round($7FFFFFFF * Scale);
      IntValue := Round($7FFFFFFF * (XPos[0] - XOffset[0]) * Scale);

      for X := XPos[0] to XPos[1] - 1 do
      begin
        BlendMemEx(CombineReg(Colors[1], Colors[0], IntValue shr 23),
          Dst^, AlphaValues^);
        IntValue := IntValue + IntScale;

        Inc(Dst);
        Inc(AlphaValues);
      end;
      EMMS;
    end;

    // check whether further drawing is still necessary
    if XPos[1] = XPos[2] then
      Exit;

    Inc(Index);

    XPos[0] := XPos[1];
    XOffset[0] := XOffset[1];
    Colors[0] := Colors[1];
  until (Index = FGradient.GradientCount);

  if XPos[0] < DstX then
    XPos[0] := DstX;

  FillLineAlpha(Dst, AlphaValues, XPos[2] - XPos[0], Colors[0], CombineMode);
end;


procedure TLinearGradientPolygonFiller.FillLineNegative(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index: Integer;
  IntScale, IntValue: Integer;
  Colors: array [0..1] of TColor32;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  XPos: array [0..2] of Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  Index := FGradient.GradientCount - 1;

  // set first offset/position
  XOffset[0] := ColorStopToScanLine(Index, DstY);
  XPos[0] := Round(XOffset[0]);
  XPos[2] := DstX + Length;

  // set start color
  Colors[0] := FGradient.FGradientColors[Index].Color32;

  // check if only a solid start color should be drawn.
  if XPos[0] >= XPos[2] - 1 then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, Colors[0], CombineMode);
    Exit;
  end;

  // eventually draw solid start color
  FillLineAlpha(Dst, AlphaValues, XPos[0] - DstX, Colors[0], CombineMode);

  Dec(Index);
  repeat
    // set start position to be at least DstX
    if XPos[0] < DstX then
      XPos[0] := DstX;

    // set destination color and offset
    Colors[1] := FGradient.FGradientColors[Index].Color32;
    XOffset[1] := ColorStopToScanLine(Index, DstY);

    // calculate destination pixel position
    XPos[1] := Round(XOffset[1]);
    if XPos[1] > XPos[2] then
      XPos[1] := XPos[2];

    // check whether next color needs to be drawn
    if XPos[1] > XPos[0] then
    begin
      Scale := 1 / (XOffset[1] - XOffset[0]);
      IntScale := Round($7FFFFFFF * Scale);
      IntValue := Round($7FFFFFFF * (XPos[0] - XOffset[0]) * Scale);

      for X := XPos[0] to XPos[1] - 1 do
      begin
        BlendMemEx(CombineReg(Colors[1], Colors[0], IntValue shr 23),
          Dst^, AlphaValues^);
        IntValue := IntValue + IntScale;

        Inc(Dst);
        Inc(AlphaValues);
      end;
      EMMS;
    end;

    // check whether further drawing is still necessary
    if XPos[1] = XPos[2] then
      Exit;

    Dec(Index);

    XPos[0] := XPos[1];
    XOffset[0] := XOffset[1];
    Colors[0] := Colors[1];
  until (Index < 0);

  if XPos[0] < DstX then
    XPos[0] := DstX;

  FillLineAlpha(Dst, AlphaValues, XPos[2] - XPos[0], Colors[0], CombineMode);
end;

procedure TLinearGradientPolygonFiller.FillLineVerticalPad(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  Color32 := FGradientLUT.Color32Ptr^[FWrapProc(Round(FGradientLUT.Mask *
    (DstY - FStartPoint.Y) * FIncline), FGradientLUT.Mask)];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLineVerticalPadExtreme(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  if DstY < FStartPoint.Y then
    Color32 := FGradientLUT.Color32Ptr^[0]
  else
    Color32 := FGradientLUT.Color32Ptr^[FGradientLUT.Mask];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLineVerticalWrap(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  X := Round(FGradientLUT.Mask * (DstY - FStartPoint.Y) * FIncline);
  Color32 := FGradientLUT.Color32Ptr^[FWrapProc(X, Integer(FGradientLUT.Mask))];

  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(Color32, Dst^, AlphaValues^);
    Inc(Dst);
    Inc(AlphaValues);
  end;
  EMMS;
end;

procedure TLinearGradientPolygonFiller.FillLineHorizontalPadPos(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X, XPos, Count, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;
  ColorLUT := FGradientLUT.Color32Ptr;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[0], CombineMode);
    Exit;
  end;

  Mask := FGradientLUT.Mask;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask], CombineMode);
    Exit;
  end;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(ColorLUT^[FWrapProc(Round((X - XOffset[0]) * Scale), Mask)],
      Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientPolygonFiller.FillLineHorizontalPadNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X, XPos, Count, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;

  XPos := Round(XOffset[0]);
  Count := Round(XOffset[1]) - XPos;

  Mask := FGradientLUT.Mask;
  ColorLUT := FGradientLUT.Color32Ptr;

  // check if only a solid start color should be drawn.
  if XPos >= DstX + Length then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask], CombineMode);
    Exit;
  end;

  // check if only a solid end color should be drawn.
  if XPos + Count < DstX then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[0], CombineMode);
    Exit;
  end;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(ColorLUT^[FWrapProc(Round((XOffset[1] - X) * Scale), Mask)],
      Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientPolygonFiller.FillLineHorizontalWrapPos(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X, Index, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  XOffset[0] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  XOffset[1] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round((X - XOffset[0]) * Scale);
    BlendMemEx(ColorLUT^[FWrapProc(Index, Mask)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientPolygonFiller.FillLineHorizontalWrapNeg(
  Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X, Index, Mask: Integer;
  ColorLUT: PColor32Array;
  Scale: TFloat;
  XOffset: array [0..1] of TFloat;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  XOffset[0] := FEndPoint.X + (FEndPoint.Y - DstY) * FIncline;
  XOffset[1] := FStartPoint.X + (FStartPoint.Y - DstY) * FIncline;
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  Scale := Mask / (XOffset[1] - XOffset[0]);
  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round((XOffset[1] - X) * Scale);
    BlendMemEx(ColorLUT^[FWrapProc(Index, Mask)], Dst^, AlphaValues^);
    EMMS;

    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TLinearGradientPolygonFiller.BeginRendering;
begin
  if LookUpTableNeedsUpdate then
  begin
    if FUseLookUpTable then
    begin
      if not Assigned(FGradientLUT) then
        raise Exception.Create(RCStrNoTColor32LookupTable);

      if Assigned(FGradient) then
        FGradient.FillColorLookUpTable(FGradientLUT);
    end
    else
      if not Assigned(FGradient) then
        raise Exception.Create(RCStrNoTColor32Gradient);
    inherited;
  end;
end;


{ TCustomRadialGradientPolygonFiller }

procedure TCustomRadialGradientPolygonFiller.SetEllipseBounds(
  const Value: TFloatRect);
begin
  if (FEllipseBounds.Left <> Value.Left) or (FEllipseBounds.Top <> Value.Top) or
    (FEllipseBounds.Right <> Value.Right) or
    (FEllipseBounds.Bottom <> Value.Bottom) then
  begin
    FEllipseBounds := Value;
    EllipseBoundsChanged;
  end;
end;


{ TRadialGradientPolygonFiller }

constructor TRadialGradientPolygonFiller.Create(Radius: TFloatPoint);
begin
  inherited Create;
  FRadius := Radius;
  UpdateEllipseBounds;
  UpdateRadiusScale;
end;

constructor TRadialGradientPolygonFiller.Create(Radius, Center: TFloatPoint);
begin
  inherited Create;
  FRadius := Radius;
  FCenter := Center;
  UpdateEllipseBounds;
  UpdateRadiusScale;
end;

constructor TRadialGradientPolygonFiller.Create(BoundingBox: TFloatRect);
begin
  Create(FloatPoint(0.5 * (BoundingBox.Right - BoundingBox.Left),
    0.5 * (BoundingBox.Bottom - BoundingBox.Top)),
    FloatPoint(0.5 * (BoundingBox.Right + BoundingBox.Left),
    0.5 * (BoundingBox.Bottom + BoundingBox.Top)));
end;

procedure TRadialGradientPolygonFiller.EllipseBoundsChanged;
begin
  with FEllipseBounds do
  begin
    FCenter := FloatPoint((Left + Right) * 0.5, (Top + Bottom) * 0.5);
    FRadius.X := Round((Right - Left) * 0.5);
    FRadius.Y := Round((Bottom - Top) * 0.5);
  end;

  UpdateRadiusScale;
end;

procedure TRadialGradientPolygonFiller.SetCenter(const Value: TFloatPoint);
begin
  if (FCenter.X <> Value.X) or (FCenter.Y <> Value.Y) then
  begin
    FCenter := Value;
    UpdateEllipseBounds;
  end;
end;

procedure TRadialGradientPolygonFiller.SetRadius(const Value: TFloatPoint);
begin
  if (FRadius.X <> Value.X) or (FRadius.Y <> Value.Y) then
  begin
    FRadius := Value;
    UpdateRadiusScale;
    UpdateEllipseBounds;
  end;
end;

procedure TRadialGradientPolygonFiller.UpdateEllipseBounds;
begin
  with FEllipseBounds do
  begin
    Left := FCenter.X - FRadius.X;
    Top := FCenter.X + FRadius.X;
    Right := FCenter.Y - FRadius.Y;
    Bottom := FCenter.Y + FRadius.Y;
  end;
end;

procedure TRadialGradientPolygonFiller.UpdateRadiusScale;
begin
  FRadScale := FRadius.X / FRadius.Y;
  FRadXInv := 1 / FRadius.X;
end;

procedure TRadialGradientPolygonFiller.BeginRendering;
begin
  if LookUpTableNeedsUpdate then
  begin
    if FUseLookUpTable then
    begin
      if not Assigned(FGradientLUT) then
        raise Exception.Create(RCStrNoTColor32LookupTable);

      if Assigned(FGradient) then
        FGradient.FillColorLookUpTable(FGradientLUT);
    end
    else
      if not Assigned(FGradient) then
        raise Exception.Create(RCStrNoTColor32Gradient);
    inherited;
  end;
end;

function TRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  case FWrapMode of
    wmClamp:
      Result := FillLinePad;
    wmMirror:
      Result := FillLineReflect;
    wmRepeat:
      Result := FillLineRepeat;
  end;
end;

procedure TRadialGradientPolygonFiller.FillLinePad(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Index, Count, Mask: Integer;
  SqrRelRad, RadMax: TFloat;
  ColorLUT: PColor32Array;
  YDist, SqrInvRadius: TFloat;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  // small optimization
  Index := Ceil(FCenter.X - FRadius.X);
  if Index > DstX then
  begin
    Count := Min((Index - DstX), Length);
    FillLineAlpha(Dst, AlphaValues, Count, ColorLUT^[Mask], CombineMode);
    Length := Length - Count;
    if Length = 0 then
      Exit;
    DstX := Index;
  end;

  // further optimization
  if Abs(DstY - FCenter.Y) > FRadius.Y then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask], CombineMode);
    Exit;
  end;

  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  RadMax := (Sqr(FRadius.X) + YDist) * SqrInvRadius;

  for X := DstX to DstX + Length - 1 do
  begin
    SqrRelRad := (Sqr(X - FCenter.X) + YDist) * SqrInvRadius;
    if SqrRelRad > RadMax then
      Index := Mask
    else
      Index := Min(Round(Mask * FastSqrt(SqrRelRad)), Mask);

    Color32 := ColorLUT^[Index];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TRadialGradientPolygonFiller.FillLineReflect(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X, Index, Mask, DivResult: Integer;
  SqrInvRadius: TFloat;
  YDist: TFloat;
  ColorLUT: PColor32Array;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;

  for X := DstX to DstX + Length - 1 do
  begin
    Index := Round(Mask * FastSqrt((Sqr(X - FCenter.X) + YDist)
      * SqrInvRadius));
    DivResult := DivMod(Index, FGradientLUT.Size, Index);
    if Odd(DivResult) then
      Index := Mask - Index;
    Color32 := ColorLUT^[Index];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure TRadialGradientPolygonFiller.FillLineRepeat(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X, Mask: Integer;
  YDist, SqrInvRadius: TFloat;
  ColorLUT: PColor32Array;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  SqrInvRadius := Sqr(FRadXInv);
  YDist := Sqr((DstY - FCenter.Y) * FRadScale);
  Mask := Integer(FGradientLUT.Mask);
  ColorLUT := FGradientLUT.Color32Ptr;
  for X := DstX to DstX + Length - 1 do
  begin
    Color32 := ColorLUT^[Round(Mask * FastSqrt((Sqr(X - FCenter.X) + YDist) *
      SqrInvRadius)) mod FGradientLUT.Size];
    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

{ TSVGRadialGradientPolygonFiller }

constructor TSVGRadialGradientPolygonFiller.Create(EllipseBounds: TFloatRect);
begin
  inherited Create;
  SetParameters(EllipseBounds);
end;

constructor TSVGRadialGradientPolygonFiller.Create(EllipseBounds: TFloatRect;
  FocalPoint: TFloatPoint);
begin
  inherited Create;
  SetParameters(EllipseBounds, FocalPoint);
end;

procedure TSVGRadialGradientPolygonFiller.EllipseBoundsChanged;
begin
  GradientFillerChanged;
end;

procedure TSVGRadialGradientPolygonFiller.SetFocalPoint(const Value: TFloatPoint);
begin
  if (FFocalPointNative.X <> Value.X) and (FFocalPointNative.Y <> Value.Y) then
  begin
    FFocalPointNative := Value;
    GradientFillerChanged;
  end;
end;

procedure TSVGRadialGradientPolygonFiller.SetParameters(
  EllipseBounds: TFloatRect);
begin
  FEllipseBounds := EllipseBounds;
  FFocalPointNative := FloatPoint(
    0.5 * (FEllipseBounds.Left + FEllipseBounds.Right),
    0.5 * (FEllipseBounds.Top + FEllipseBounds.Bottom));
  GradientFillerChanged;
end;

procedure TSVGRadialGradientPolygonFiller.SetParameters(
  EllipseBounds: TFloatRect; FocalPoint: TFloatPoint);
begin
  FEllipseBounds := EllipseBounds;
  FFocalPointNative := FocalPoint;
  GradientFillerChanged;
end;

procedure TSVGRadialGradientPolygonFiller.InitMembers;
var
  X, Y: TFloat;
  Temp: TFloat;
begin
  FRadius.X := (FEllipseBounds.Right - FEllipseBounds.Left) * 0.5;
  FRadius.Y := (FEllipseBounds.Bottom - FEllipseBounds.Top) * 0.5;
  FCenter.X := (FEllipseBounds.Right + FEllipseBounds.Left) * 0.5;
  FCenter.Y := (FEllipseBounds.Bottom + FEllipseBounds.Top) * 0.5;
  FOffset.X := FEllipseBounds.Left;
  FOffset.Y := FEllipseBounds.Top;

  // make FFocalPoint relative to the ellipse midpoint ...
  FFocalPt.X := FFocalPointNative.X - FCenter.X;
  FFocalPt.Y := FFocalPointNative.Y - FCenter.Y;

  // make sure the focal point stays within the bounding ellipse ...
  if Abs(FFocalPt.X) < CFloatTolerance then
  begin
    X := 0;
    if FFocalPt.Y < 0 then
      Y := -1
    else
      Y := 1;
  end
  else
  begin
    Temp := FRadius.X * FFocalPt.Y / (FRadius.Y * FFocalPt.X);
    X := 1 / FastSqrtBab1(1 + Sqr(Temp));
    Y := Temp * X;
  end;
  if FFocalPt.X < 0 then
  begin
    X := -X;
    Y := -Y;
  end;
  X := X * FRadius.X;
  Y := Y * FRadius.Y;
  if (Y * Y + X * X) < (Sqr(FFocalPt.X) + Sqr(FFocalPt.Y)) then
  begin
    FFocalPt.X := 0.999 * X;
    FFocalPt.Y := 0.999 * Y;
  end;

  // Because the slope of vertical lines is infinite, we need to find where a
  // vertical line through the FocalPoint intersects with the Ellipse, and
  // store the distances from the focal point to these 2 intersections points
  FVertDist := FRadius.Y * FastSqrtBab1(1 - Sqr(FFocalPt.X) / Sqr(FRadius.X));
end;

procedure TSVGRadialGradientPolygonFiller.BeginRendering;
begin
  if LookUpTableNeedsUpdate then
  begin
    if FUseLookUpTable then
    begin
      if not Assigned(FGradientLUT) then
        raise Exception.Create(RCStrNoTColor32LookupTable);

      if Assigned(FGradient) then
        FGradient.FillColorLookUpTable(FGradientLUT);
    end
    else
      if not Assigned(FGradient) then
        raise Exception.Create(RCStrNoTColor32Gradient);
    inherited;
  end;
  InitMembers;
end;

function TSVGRadialGradientPolygonFiller.GetFillLine: TFillLineEvent;
begin
  Result := FillLineEllipse;
end;

procedure TSVGRadialGradientPolygonFiller.FillLineEllipse(Dst: PColor32;
  DstX, DstY, Length: Integer; AlphaValues: PColor32;
  CombineMode: TCombineMode);
var
  X, Mask: Integer;
  ColorLUT: PColor32Array;
  Rad, Rad2, X2, Y2: TFloat;
  m, b, Qa, Qb, Qc, Qz, XSqr: Double;
  RelPos: TFloatPoint;
  Color32: TColor32;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  if (FRadius.X = 0) or (FRadius.Y = 0) then
    Exit;

  ColorLUT := FGradientLUT.Color32Ptr;

  RelPos.Y := DstY - FCenter.Y - FFocalPt.Y;
  Mask := Integer(FGradientLUT.Mask);

  // check if out of bounds (vertically)
  if (DstY < FOffset.Y) or (DstY >= (FRadius.Y * 2) + 1 + FOffset.Y) then
  begin
    FillLineAlpha(Dst, AlphaValues, Length, ColorLUT^[Mask], CombineMode);
    Exit;
  end;

  for X := DstX to DstX + Length - 1 do
  begin
    // check if out of bounds (horizontally)
    if (X < FOffset.X) or (X >= (FRadius.X * 2) + 1 + FOffset.X) then
      Color32 := ColorLUT^[Mask]
    else
    begin
      RelPos.X := X - FCenter.X - FFocalPt.X;

      if Abs(RelPos.X) < CFloatTolerance then //ie on the vertical line (see above)
      begin
        Assert(Abs(X - FCenter.X) <= FRadius.X);

        Rad := Abs(RelPos.Y);
        if Abs(Abs(X - FCenter.X)) <= FRadius.X then
        begin
          if RelPos.Y < 0 then
            Rad2 := Abs(-FVertDist - FFocalPt.Y)
          else
            Rad2 := Abs( FVertDist - FFocalPt.Y);
          if Rad >= Rad2 then
            Color32 := ColorLUT^[Mask]
          else
            Color32 := ColorLUT^[Round(Mask * Rad / Rad2)];
        end else
          Color32 := ColorLUT^[Mask];
      end
      else
      begin
        m := RelPos.Y / RelPos.X;
        b := FFocalPt.Y - m * FFocalPt.X;
        XSqr := Sqr(FRadius.X);

        // apply quadratic equation ...
        Qa := 2 * (Sqr(FRadius.Y) + XSqr * m * m);
        Qb := XSqr * 2 * m * b;
        Qc := XSqr * (b * b - Sqr(FRadius.Y));
        Qz := Qb * Qb - 2 * Qa * Qc;

        if Qz >= 0 then
        begin
          Qz := FastSqrtBab2(Qz);
          Qa := 1 / Qa;
          X2 := (-Qb + Qz) * Qa;
          if (FFocalPt.X > X2) = (RelPos.X > 0) then
            X2 := -(Qb + Qz) * Qa;
          Y2 := m * X2 + b;
          Rad := Sqr(RelPos.X) + Sqr(RelPos.Y);
          Rad2 := Sqr(X2 - FFocalPt.X) + Sqr(Y2 - FFocalPt.Y);

          if Rad >= Rad2 then
            Color32 := ColorLUT^[Mask]
          else
            Color32 := ColorLUT^[Round(Mask * FastSqrtBab1(Rad / Rad2))];
        end else
          Color32 := ColorLUT^[Mask]
      end;
    end;

    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure RegisterBindings;
begin
  GradientRegistry := NewRegistry('GR32_ColorGradients bindings');
  GradientRegistry.RegisterBinding(FID_LINEAR3, @@Linear3PointInterpolationProc);
  GradientRegistry.RegisterBinding(FID_LINEAR4, @@Linear4PointInterpolationProc);

  // pure pascal
  GradientRegistry.Add(FID_LINEAR3, @Linear3PointInterpolation_Pas);
  GradientRegistry.Add(FID_LINEAR4, @Linear4PointInterpolation_Pas);

{$IFNDEF PUREPASCAL}
{$IFNDEF OMIT_SSE2}
  GradientRegistry.Add(FID_LINEAR3, @Linear3PointInterpolation_SSE2, [ciSSE2]);
  GradientRegistry.Add(FID_LINEAR4, @Linear4PointInterpolation_SSE2, [ciSSE2]);
{$ENDIF}
{$ENDIF}

  GradientRegistry.RebindAll;
end;

initialization
  RegisterBindings;

end.
