unit GR32.Paint.Tool.Brush;

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
 * The Original Code is Paint tools for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander, anders@melander.dk
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$INCLUDE GR32.inc}

uses
  Classes,
  Controls,
  GR32,
  GR32.Paint.Host.API,
  GR32.Paint.Tool,
  GR32.Paint.Tool.API,
  GR32.Paint.Brush;

//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolBrush
//
//------------------------------------------------------------------------------
type
  IBitmap32PaintToolBrush = interface
    ['{9AF10CB0-6D29-48A6-8DE6-F19CDEEBAFB2}']
    function CreateBrush(Color: TColor32): TCustomPaintBrush;
  end;

  TBitmap32PaintToolBrush = class abstract(TCustomBitmap32PaintTool, IBitmap32PaintToolBrush)
  strict private
    FBrushLineState: TPaintBrushLineState;
    FLastPos: TFloatPoint;
    FBrush: TCustomPaintBrush;
    FBrushLine: TPaintBrushLine;
    FHasVectorCursor: boolean;
    FCursorSize: Single;
    FBrushSize: Single;
  strict protected
    // IBitmap32PaintToolBrush
    function CreateBrush(Color: TColor32): TCustomPaintBrush; virtual; abstract;

  strict protected
    function GetCaption: string; override;

    procedure BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); override;
    procedure ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); override;
    procedure EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState); override;

    function GetCursor(out Cursor: TCursor): boolean; override;

  strict protected
    procedure GetVectorCursor;
    function CreateVectorCursor: boolean; virtual;
    function GetStep: integer; virtual;

    property Brush: TCustomPaintBrush read FBrush;
    property Step: integer read GetStep;

  public
    constructor Create(const APaintHost: IBitmap32PaintHost); override;
    destructor Destroy; override;

    property BrushSize: Single read FBrushSize write FBrushSize;
  end;

  TBitmap32PaintToolBrushClass = class of TBitmap32PaintToolBrush;

resourcestring
  sBitmap32PaintToolBrushCaption = 'Brush';


//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolCircularBrush
//
//------------------------------------------------------------------------------
type
  TBitmapBrushClass = class of TBitmapPaintBrush;

  TBitmap32PaintToolCircularBrush = class(TBitmap32PaintToolBrush)
  strict private
    FBlendFunc: TPixelCombineEvent;
    FAntiAlias: boolean;
    FHardness: Single;
    FStep: integer;
  strict protected
    procedure BlendWrapper(F: TColor32; var B: TColor32; M: Cardinal);
    function GetStep: integer; override;
    function CreateBrush(Color: TColor32): TCustomPaintBrush; override;
    function GetBrushClass: TBitmapBrushClass; virtual;
  public
    constructor Create(const APaintHost: IBitmap32PaintHost); override;

    property AntiAlias: boolean read FAntiAlias write FAntiAlias;
    // [0..100]
    property Hardness: Single read FHardness write FHardness;

    // [1..]
    property Step: integer read FStep write FStep;
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolSmudgeBrush
//
//------------------------------------------------------------------------------
type
  TBitmap32PaintToolSmudgeBrush = class(TBitmap32PaintToolCircularBrush)
  strict private
    FPressure: integer;
  strict protected
    function GetCaption: string; override;
    function GetStep: integer; override;
    function GetBrushClass: TBitmapBrushClass; override;
    function GetCursor(out Cursor: TCursor): boolean; override;
    function CreateBrush(Color: TColor32): TCustomPaintBrush; override;
  public
    constructor Create(const APaintHost: IBitmap32PaintHost); override;

    // [0..100]
    property Pressure: integer read FPressure write FPressure;
  end;



resourcestring
  sBitmap32PaintToolSmudgeCaption = 'Smudge';


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  Types,
  SysUtils,
  GR32_VectorUtils,
  GR32_Polygons,
  GR32.Blur;

//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolBrush
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintToolBrush.Create(const APaintHost: IBitmap32PaintHost);
begin
  inherited;

  FBrushSize := 15;
end;

destructor TBitmap32PaintToolBrush.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolBrush.CreateVectorCursor: boolean;
var
  Radius: Single;
  Steps: integer;
  Stipple: TArrayOfColor32;
  Polygon: TArrayOfFixedPoint;
  FeatureVectorCursor: IBitmap32PaintFeatureVectorCursor;
begin
  Result := False;

  if (not Supports(PaintHost, IBitmap32PaintFeatureVectorCursor, FeatureVectorCursor)) then
    exit;

  FCursorSize := BrushSize;

  Radius := FCursorSize / 2;

  if (Radius > 1) then
  begin
    Steps := Round(Radius * Abs(PaintHost.Magnification)) + 4;
    if Odd(Steps) then
      Inc(Steps);

    Polygon := FloatPointToFixedPoint(Circle(FloatPoint(0, 0), Radius, Steps));

    SetLength(Stipple, 6);
    Stipple[0] := $80FFFFFF;
    Stipple[1] := $80FFFFFF;
    Stipple[2] := $00000000;
    Stipple[3] := $80000000;
    Stipple[4] := $80000000;
    Stipple[5] := $00000000;

    Result := FeatureVectorCursor.SetToolVectorCursor(Polygon, GR32.Point(0, 0), 0, Stipple);
  end;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolBrush.GetCaption: string;
begin
  Result := sBitmap32PaintToolBrushCaption;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolBrush.GetCursor(out Cursor: TCursor): boolean;
begin
  Cursor := crCross;
  Result := True;
  GetVectorCursor;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolBrush.GetStep: integer;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintToolBrush.GetVectorCursor;
begin
  FHasVectorCursor := CreateVectorCursor;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintToolBrush.BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
  inherited;

  // Note: TRemapTransformation requires the width/height of the map to be greater than one!

  if ([ssLeft, ssRight] * Context.MouseParams.ShiftState <> []) and (Context.Buffer.Width > 1) and (Context.Buffer.Height > 1) then
  begin
    FLastPos := Context.MouseParams.BitmapPosFloat;

    FBrush := CreateBrush(ActiveColor(Context.MouseParams.ShiftState));
    FBrush.BeginBrush(Context.Buffer);

    ASSERT(FBrushLine = nil);
    FBrushLine := TPaintBrushLine.Create(Context.Buffer, FBrush, Step);
    FBrushLine.MoveTo(FLastPos.X, FLastPos.Y);

    FBrushLineState.Valid := False;

    PaintHost.Changed(Caption);
  end else
    ToolState := tsAbort;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintToolBrush.ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
  if ([ssLeft, ssRight] * Context.MouseParams.ShiftState <> []) then
  begin
    // Erase old line (via update of background)
    if (FBrushLineState.Valid) then
    begin
      Context.Buffer.BeginMeasuring(nil);
      try

        // Replay stroke without actually modifying anything
        FBrushLine.RestoreState(FBrushLineState);
        FBrushLine.LineTo(FLastPos.X, FLastPos.Y);

      finally
        Context.Buffer.EndMeasuring;
      end;
    end;

    // Save brush state so we can replay the stroke we're about to make when
    // the old line should be erased (via background repaint).
    FBrushLine.SaveState(FBrushLineState);

    FLastPos := Context.MouseParams.BitmapPosFloat;

    FBrushLine.LineTo(FLastPos.X, FLastPos.Y);

    PaintHost.Changed(Caption);
  end;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintToolBrush.EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
begin
  (*
  ** []                   Replace
  ** [ssShift]            Transparent (blend)
  *)
  if (FLastPos <> Context.MouseParams.BitmapPosFloat) then
  begin
    FBrushLine.LineTo(Context.MouseParams.BitmapPosFloat.X, Context.MouseParams.BitmapPosFloat.Y);
    PaintHost.Changed(Caption);
  end;

  FreeAndNil(FBrushLine);

  FBrush.EndBrush;
  FreeAndNil(FBrush);
end;



//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolCircularBrush
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintToolCircularBrush.Create(const APaintHost: IBitmap32PaintHost);
begin
  inherited;

  FAntiAlias := True;
  FHardness := 75;
  FStep := 1;
end;

//------------------------------------------------------------------------------
function TBitmap32PaintToolCircularBrush.CreateBrush(Color: TColor32): TCustomPaintBrush;
var
  Feather, Radius: Single;
  BrushCircle: TArrayOfFloatPoint;
//  Blender: IBitmap32Blender;
begin
  // +2 to make room for antialiasing
  Result := GetBrushClass.Create(Ceil(BrushSize)+2, Ceil(BrushSize)+2);

  TBitmapPaintBrush(Result).Bitmap.Clear(0);

  if (AntiAlias) then
    Feather := BrushSize - BrushSize / 100 * Hardness
  else
    Feather := 0;

  Radius := (BrushSize - Feather) / 2;

  BrushCircle := Circle(Result.Width/2-0.5, Result.Height/2-0.5, Radius);
  PolygonFS(TBitmapPaintBrush(Result).Bitmap, BrushCircle, Color);

  if (Feather > 0) then
    Blur32(TBitmapPaintBrush(Result).Bitmap, Feather);

(* Maybe later...
  DrawCircle(TBitmapPaintBrush(Result).Bitmap, Result.Width/2-0.5, Result.Height/2-0.5, Radius, Color, Color, 0, Feather, AntiAlias, True);
*)

(*
  Blender := BitmapEditorBlendService.BlenderByID(SettingValues['Blend']);
  if (Blender <> nil) then
  begin
    Blender.GetBlendFunc(FBlendFunc);

    if (Assigned(FBlendFunc)) then
      TBitmapPaintBrush(Result).BlendFunc := BlendWrapper
    else
      TBitmapPaintBrush(Result).BlendFunc := nil;
  end else
*)
    TBitmapPaintBrush(Result).BlendFunc := nil;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintToolCircularBrush.BlendWrapper(F: TColor32; var B: TColor32; M: Cardinal);
begin
  if (Assigned(FBlendFunc)) then
    FBlendFunc(F, B, M or $0000FF00); // Set source master alpha to 255
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolCircularBrush.GetBrushClass: TBitmapBrushClass;
begin
  Result := TBitmapPaintBrush;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolCircularBrush.GetStep: integer;
begin
  Result := FStep;
end;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolSmudgeBrush
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintToolSmudgeBrush.Create(const APaintHost: IBitmap32PaintHost);
begin
  inherited;

  FPressure := 50;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolSmudgeBrush.CreateBrush(
  Color: TColor32): TCustomPaintBrush;
begin
  Result := inherited CreateBrush(Color or $FF000000);
  TSmudgePaintBrush(Result).Pressure := FPressure;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolSmudgeBrush.GetBrushClass: TBitmapBrushClass;
begin
  Result := TSmudgePaintBrush;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolSmudgeBrush.GetCaption: string;
begin
  Result := sBitmap32PaintToolSmudgeCaption;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolSmudgeBrush.GetCursor(out Cursor: TCursor): boolean;
begin
  Cursor := crCross;
  Result := True;
  GetVectorCursor;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintToolSmudgeBrush.GetStep: integer;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------

end.
