unit GR32.Paint.MouseController.API;

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
  GR32_Layers;

//------------------------------------------------------------------------------
//
//      IBitmap32PaintMouseController
//
//------------------------------------------------------------------------------
// Processes mouse events and pass them on to IBitmap32PaintController.
// Also responsible for constructing and updating the tool context.
//------------------------------------------------------------------------------
type
  IBitmap32PaintMouseController = interface
    ['{9A67AF87-7C80-47F9-9BBD-D554EEB0F8DF}']
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure MouseEnter;
    procedure MouseExit;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

end.
