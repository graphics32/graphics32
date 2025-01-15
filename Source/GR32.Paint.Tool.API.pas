unit GR32.Paint.Tool.API;

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

type
  // TBitmap32PaintToolFeatures:
  TBitmap32PaintToolFeature = (
    betfMouseCapture            // Tool captures the mouse during operation.
                                // If this flag is not set then the framework will reset
                                // mouse capture to the image control during operation.
  );
  TBitmap32PaintToolFeatures = set of TBitmap32PaintToolFeature;


  // TBitmap32PaintToolState:
  //
  // - tsContinue       Tool is performing the requested operation
  //
  // - tsAbort          Tool has rejected the requested operation
  //
  // - tsComplete       Tool has completed the requested operation
  //
  TBitmap32PaintToolState = (tsContinue, tsAbort, tsComplete);


//------------------------------------------------------------------------------
//
//      Paint tool mouse event parameters
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
  IBitmap32PaintTool = interface;

  IBitmap32PaintToolContext = interface
    ['{BE0ECC3B-F27B-4689-A7A8-55958EA4047C}']
    function GetPaintTool: IBitmap32PaintTool;
    property PaintTool: IBitmap32PaintTool read GetPaintTool;

    function GetBuffer: TBitmap32;
    property Buffer: TBitmap32 read GetBuffer;

    function GetMouseParams: PBitmap32PaintToolMouseParams;
    // MouseParams is a pointer to avoid copy-on-read overhead.
    // The value is static and is valid for the lifetime of the context object.
    property MouseParams: PBitmap32PaintToolMouseParams read GetMouseParams;

    procedure Update(const ViewPortPos: TPoint; SnapMouse: boolean);
  end;


//------------------------------------------------------------------------------
//
//      IBitmap32PaintTool
//
//------------------------------------------------------------------------------
// A mouse-operated drawing tool. E.g. Pen, Brush, Line, Circle, Selection, etc.
//------------------------------------------------------------------------------
  IBitmap32PaintTool = interface(IBitmap32PaintExtension)
    ['{712D1D8A-5C4B-43C7-A16A-8D8BBA9A6818}']
    /// <summary>Activate is called when the tool is selected.</summary>
    /// <comments>Set Continue to False to cancel the activation.</comments>
    procedure Activate(var Continue: boolean);
    /// <summary>Deactivate is called when tool is deselected.</summary>
    procedure Deactivate;

    /// <summary>BeginTool is called before BeginAction, just before IBitmap32PaintHost.BeginUpdate is called.</summary>
    /// <comments>Set Continue to False to abort the operation.</comments>
    procedure BeginTool(var Continue: boolean);
    /// <summary>EndTool is called after EndAction, just before IBitmap32PaintHost.EndUpdate is called.</summary>
    /// <comments>EndTool is called even if the operation is aborted prematurely.</comments>
    procedure EndTool;

    // BeginAction, ContinueAction and EndAction is called when the tool has
    // been selected and the user presses, moves and releases the mouse.
    // Set State to tsAbort to cancel the operation.
    procedure BeginAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
    procedure ContinueAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
    procedure EndAction(const Context: IBitmap32PaintToolContext; var ToolState: TBitmap32PaintToolState);
    procedure CallbackAction(Buffer: TBitmap32; Data: pointer; var Result: boolean);

    // MouseDown, MouseMove and MouseUp is called when the tool has been selected
    // and the user presses, moves and releases the mouse.
    procedure MouseDown(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
    procedure MouseMove(const Context: IBitmap32PaintToolContext);
    procedure MouseUp(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
    procedure MouseEnter;
    procedure MouseLeave;

    // TODO : Not yet implemented in controller layer
    procedure KeyDown(var Key: Word; Shift: TShiftState);
    procedure KeyUp(var Key: Word; Shift: TShiftState);

    /// <summary>GetCursor returns the cursor used by the tool.</summary>
    /// <comments>The tool should use IBitmap32PaintHost.RegisterCursor to register custom cursors.</comments>
    function GetCursor(out Cursor: TCursor): boolean;

    // SnapMouse specifies if bitmap coordinates should always be rounded (SnapMouse=True) or
    // truncated (SnapMouse=False) when converting mouse position from viewport to bitmap
    // coordinates.
    // If SnapMouse=True then the coordinates specified in Context.BitmapPos passed to BeginAction,
    // ContinueAction and EndAction will be snapped coordinates. I.e. the same value as Context.BitmapPosSnap.
    // Tools that operate on pixels usually need SnapMouse=False while tools that operate
    // on areas usually need SnapMouse=True.
    function GetSnapMouse: boolean;
    property SnapMouse: boolean read GetSnapMouse;

    function GetToolFeatures: TBitmap32PaintToolFeatures;
    property ToolFeatures: TBitmap32PaintToolFeatures read GetToolFeatures;
  end;

//------------------------------------------------------------------------------
//
//      Global settings
//
//------------------------------------------------------------------------------
var
  // Snap to: rect, 45deg, etc.
  Bitmap32PaintToolKeyStateSnap: TShiftState = [ssShift]; // PhotoShop: [ssShift]
  // StartPos is center of: rect, circle, etc.
  Bitmap32PaintToolKeyStateCenter: TShiftState = [ssAlt]; // PhotoShop: [ssAlt]

  // Generic modifier (action depends on tool)
  Bitmap32PaintToolKeyStateAlternate: TShiftState = [ssCtrl]; // Must be different from the two above


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

end.
