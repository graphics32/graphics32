unit GR32.Paint.API;

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
  GR32;


//------------------------------------------------------------------------------
//
//      Paint tool settings
//
//------------------------------------------------------------------------------
// Not presently implemented
//------------------------------------------------------------------------------
type
  ISettingValues = interface
    ['{FD5DCE91-F768-480E-B907-FBAEAA08FEF8}']
    function GetValue(const Name: string): Variant;
    procedure SetValue(const Name: string; const Value: Variant);

    property Values[const Name: string]: Variant read GetValue write SetValue; default;
  end;


//------------------------------------------------------------------------------
//
//      Paint tool mouse event params
//
//------------------------------------------------------------------------------
type
  TBitmap32PaintToolMouseParams = record
    MouseMessageTime: Cardinal;         // Timestamp
    ScreenPos: TPoint;                  // Screen coordinates
    ViewPortPos: TPoint;                // Viewport coordinates
    BitmapPos: TPoint;                  // Bitmap coordinates, rounded down
    BitmapPosSnap: TPoint;              // Bitmap coordinates, snapped to nearest.
    BitmapPosFloat: TFloatPoint;        // Fractional bitmap coordinates
    ShiftState: TShiftState;            // Mouse/keyboard shift state
  end;

  PBitmap32PaintToolMouseParams = ^TBitmap32PaintToolMouseParams;

type
  IBitmap32PaintToolContext = interface
    ['{BE0ECC3B-F27B-4689-A7A8-55958EA4047C}']
    function GetMouseParams: PBitmap32PaintToolMouseParams;
    property MouseParams: PBitmap32PaintToolMouseParams read GetMouseParams;

    function GetBuffer: TBitmap32;
    property Buffer: TBitmap32 read GetBuffer;
  end;


//------------------------------------------------------------------------------
//
//      IBitmap32PaintHost
//
//------------------------------------------------------------------------------
// Provides an interface between the host application and the paint tools.
//------------------------------------------------------------------------------
type
  IBitmap32PaintHost = interface
    ['{D51C1D86-A06D-4E3C-95B4-9F70ADD7A9DF}']
    function GetColorPrimary: TColor32;
    procedure SetColorPrimary(const Value: TColor32);
    function GetColorSecondary: TColor32;
    procedure SetColorSecondary(const Value: TColor32);
    property ColorPrimary: TColor32 read GetColorPrimary write SetColorPrimary;
    property ColorSecondary: TColor32 read GetColorSecondary write SetColorSecondary;

    function GetMagnification: Single;
    procedure SetMagnification(const Value: Single);
    property Magnification: Single read GetMagnification write SetMagnification;

    function ViewPortToScreen(const APoint: TPoint): TPoint;
    function ScreenToViewPort(const APoint: TPoint): TPoint;
    function ViewPortToBitmap(const APoint: TPoint; SnapToNearest: boolean = True): TPoint;
    function BitmapToViewPort(const APoint: TPoint): TPoint;

    function GetToolSettings(const AToolKey: string): ISettingValues;

    function CreateToolContext(const ViewPortPos: TPoint; SnapMouse: boolean): IBitmap32PaintToolContext;

    // SetToolVectorCursor: Called from tools to set complex cursors.
    function SetToolVectorCursor(const Polygon: TArrayOfFixedPoint; HotspotX, HotspotY: integer; Color: TColor32; const OutlinePattern: TArrayOfColor32): boolean;
    // Changed: Called from tools. Used for undo management.
    procedure Changed(const Action: string);
  end;


//------------------------------------------------------------------------------
//
//      IBitmap32PaintExtension
//
//------------------------------------------------------------------------------
// Base interface for bitmap tools (tools, actions, filters, color pickers, etc)
//------------------------------------------------------------------------------
type
  IBitmap32PaintExtension = interface
    ['{17AF7811-406F-4C58-906C-C3E6E3FC67D2}']
    function GetIsVisible: boolean;
    function GetIsEnabled: boolean;
    function GetCaption: string;
    function GetHint: string;
    function GetDescription: string;
    function GetAttribution: string;

    // Clear instructs the extension to delete all allocated resources.
    procedure Clear;
    // Reset instructs the extension to reset its setting to their default values.
    procedure Reset;

    property IsVisible: boolean read GetIsVisible;
    property IsEnabled: boolean read GetIsEnabled;
    property Caption: string read GetCaption;
    property Hint: string read GetHint;
    property Description: string read GetDescription;
    property Attribution: string read GetAttribution;
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolContext
//
//------------------------------------------------------------------------------
// Minimal, example implementation of IBitmap32PaintToolContext
//------------------------------------------------------------------------------
type
  TBitmap32PaintToolContext = class(TInterfacedObject, IBitmap32PaintToolContext)
  private
    FMouseParams: TBitmap32PaintToolMouseParams;
    FBuffer: TBitmap32;
  private
    // IBitmap32PaintToolContext
    function GetMouseParams: PBitmap32PaintToolMouseParams;
    function GetBuffer: TBitmap32;
  public
    constructor Create(ABuffer: TBitmap32);
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolContext
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintToolContext.Create(ABuffer: TBitmap32);
begin
  inherited Create;
  FBuffer := ABuffer;
  FMouseParams := Default(TBitmap32PaintToolMouseParams);
end;

function TBitmap32PaintToolContext.GetMouseParams: PBitmap32PaintToolMouseParams;
begin
  Result := @FMouseParams;
end;

function TBitmap32PaintToolContext.GetBuffer: TBitmap32;
begin
  Result := FBuffer;
end;

//------------------------------------------------------------------------------

end.
