unit GR32.Paint.ToolContext;

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
  GR32.Paint.Tool.API;


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
    FPaintHost: IBitmap32PaintHost;
    FPaintTool: IBitmap32PaintTool;
    FBuffer: TBitmap32;
    FMouseParams: TBitmap32PaintToolMouseParams;
  private
    // IBitmap32PaintToolContext
    function GetPaintTool: IBitmap32PaintTool;
    function GetBuffer: TBitmap32;
    function GetMouseParams: PBitmap32PaintToolMouseParams;
    procedure Update(const ViewPortPos: TPoint; SnapMouse: boolean);
  public
    constructor Create(const APaintHost: IBitmap32PaintHost; const APaintTool: IBitmap32PaintTool; ABuffer: TBitmap32);
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Types;

//------------------------------------------------------------------------------
//
//      TBitmap32PaintToolContext
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintToolContext.Create(const APaintHost: IBitmap32PaintHost; const APaintTool: IBitmap32PaintTool; ABuffer: TBitmap32);
begin
  inherited Create;
  FPaintHost := APaintHost;
  FPaintTool := APaintTool;
  FBuffer := ABuffer;
  FMouseParams := Default(TBitmap32PaintToolMouseParams);
end;

function TBitmap32PaintToolContext.GetPaintTool: IBitmap32PaintTool;
begin
  Result := FPaintTool;
end;

function TBitmap32PaintToolContext.GetBuffer: TBitmap32;
begin
  Result := FBuffer;
end;

function TBitmap32PaintToolContext.GetMouseParams: PBitmap32PaintToolMouseParams;
begin
  Result := @FMouseParams;
end;

procedure TBitmap32PaintToolContext.Update(const ViewPortPos: TPoint; SnapMouse: boolean);
begin
  FMouseParams.ViewPortPos := ViewPortPos;
  FMouseParams.ScreenPos := FPaintHost.ViewPortToScreen(ViewPortPos);

  FMouseParams.BitmapPosFloat := FPaintHost.ViewPortToBitmap(FloatPoint(ViewPortPos));

  // If SnapMouse=True then we only return the snapped coordinates
  FMouseParams.BitmapPos := FPaintHost.ViewPortToBitmap(ViewPortPos, SnapMouse);
  FMouseParams.BitmapPosSnap := FPaintHost.ViewPortToBitmap(ViewPortPos, True);
end;

//------------------------------------------------------------------------------

end.
