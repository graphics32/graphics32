unit GR32.Paint.Controller.API;

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
  Controls,
  GR32,
  GR32.Paint.Tool.API;

//------------------------------------------------------------------------------
//
//      IBitmap32PaintController
//
//------------------------------------------------------------------------------
// Manages paint tool selection and tool operation state
//------------------------------------------------------------------------------
type
  IBitmap32PaintController = interface
    ['{707D785B-8B5B-48C0-8DC8-A3AF168041ED}']

    function BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
    function ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
    procedure EndOperation(Complete: boolean);

    // Mouse events forwarded from the mouse controller.
    procedure MouseDown(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
    procedure MouseMove(const Context: IBitmap32PaintToolContext);
    procedure MouseUp(const Context: IBitmap32PaintToolContext; Button: TMouseButton);

    procedure MouseEnter;
    procedure MouseExit;

    // CreateToolContext creates a new tool context object.
    // It is the responsibility of the caller to initialize and maintain the returned object with context values.
    // The creation of the actual context object is delegated to the paint host.
    function CreateToolContext: IBitmap32PaintToolContext;

    function GetPaintTool: IBitmap32PaintTool;
    procedure SetPaintTool(const Value: IBitmap32PaintTool);
    property PaintTool: IBitmap32PaintTool read GetPaintTool write SetPaintTool;

    function GetActivePaintTool: IBitmap32PaintTool;
    property ActivePaintTool: IBitmap32PaintTool read GetActivePaintTool;

    function GetActivePaintToolContext: IBitmap32PaintToolContext;
    property ActivePaintToolContext: IBitmap32PaintToolContext read GetActivePaintToolContext;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

end.
