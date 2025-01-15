unit GR32.Paint.Host.API;

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
  GR32_Layers,
  GR32.Paint.Tool.API;


//------------------------------------------------------------------------------
//
//      Paint tool settings
//
//------------------------------------------------------------------------------
// Not presently implemented.
// Moved to GR32.Paint.Settings.API
//------------------------------------------------------------------------------
type
  ISettingValues = interface
  end;


//------------------------------------------------------------------------------
//
//      IBitmap32PaintHost
//
//------------------------------------------------------------------------------
// Handles coordinate space conversion, colors, layers, cursor, etc.
//------------------------------------------------------------------------------
// Provides the paint tools with an interface to the host application environment.
//------------------------------------------------------------------------------
type
  IBitmap32PaintHost = interface
    ['{D51C1D86-A06D-4E3C-95B4-9F70ADD7A9DF}']
    function GetPaintLayer: TBitmapLayer;
    procedure SetPaintLayer(const Value: TBitmapLayer);
    property PaintLayer: TBitmapLayer read GetPaintLayer write SetPaintLayer;

    // TODO : Move to PaintController
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
    function ViewPortToBitmap(const APoint: TPoint; SnapToNearest: boolean = True): TPoint; overload;
    function ViewPortToBitmap(const APoint: TFloatPoint): TFloatPoint; overload;
    function BitmapToViewPort(const APoint: TPoint): TPoint;

    function GetToolSettings(const AToolKey: string): ISettingValues;

    // CreateToolContext creates a new tool context object.
    // It is the responsibility of the caller to initialize and maintain the returned object with context values.
    function CreateToolContext(const APaintTool: IBitmap32PaintTool): IBitmap32PaintToolContext;

    // Cursor management
    procedure ShowToolCursor(Show: Boolean; OnlyUpdateVectorCursor: boolean = False);
    procedure SetToolCursor(NewCursor: TCursor);
    // SetToolVectorCursor: Called from tools to set complex cursors.
    function SetToolVectorCursor(const Polygon: TArrayOfFixedPoint; const Hotspot: TPoint; Color: TColor32 = clTrBlack32; const StipplePattern: TArrayOfColor32 = []): boolean;
    procedure MoveToolVectorCursor(const APos: TPoint);

    // Changed: Called from tools. Used for undo management.
    procedure Changed(const Action: string);
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

end.
