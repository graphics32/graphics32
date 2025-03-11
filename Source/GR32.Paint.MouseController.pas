unit GR32.Paint.MouseController;

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
  GR32.Paint.Host.API,
  GR32.Paint.Tool.API,
  GR32.Paint.Controller.API,
  GR32.Paint.MouseController.API;

//------------------------------------------------------------------------------
//
//      TBitmap32PaintMouseController
//
//------------------------------------------------------------------------------
// An example implementation of a mouse controller.
// The mouse controller processes mouse event and passes them on to the
// controller.
//------------------------------------------------------------------------------
type
  TBitmap32PaintMouseController = class(TInterfacedObject, IBitmap32PaintMouseController)
  strict private
    FController: IBitmap32PaintController;
    FPaintHost: IBitmap32PaintHost;

  strict private
    // IBitmap32PaintMouseController
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    procedure MouseEnter;
    procedure MouseExit;

  strict private
    // Mouse state
    FMouseShift: TShiftState;
    FLastMousePos: TPoint;
    FLastMouseMessageTime: Cardinal;

  public
    constructor Create(const APaintHost: IBitmap32PaintHost; const APaintController: IBitmap32PaintController);
    destructor Destroy; override;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$if defined(MSWINDOWS)}
  Windows,
{$ifend}
{$if defined(UseInlining)}
  Types,
{$ifend}
  SysUtils,
  GR32_System;

{$if not defined(MSWINDOWS)}
type
  TMouseMovePoint = record
    x, y: integer;
    time: cardinal;
  end;

const
  MaxMouseMovePointCount = 1;

  GMMP_USE_DISPLAY_POINTS = 1;

function GetMessageTime: integer;
begin
  Result := GR32_System.GetTickCount;
end;

function GetMouseMovePointsEx(cbSize: Cardinal; var lppt, lpptBuf: TMouseMovePoint; nBufPoints: Integer; resolution: Cardinal): Integer;
begin
  Result := -1;
end;
{$ifend}

//------------------------------------------------------------------------------
//
//      TBitmap32PaintMouseController
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintMouseController.Create(const APaintHost: IBitmap32PaintHost; const APaintController: IBitmap32PaintController);
begin
  inherited Create;

  FPaintHost := APaintHost;
  FController := APaintController;
end;

destructor TBitmap32PaintMouseController.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintMouseController.HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
var
  ToolContext: IBitmap32PaintToolContext;
begin
  if (Layer <> FPaintHost.PaintLayer) then
    exit;

  // Save double-click state for use in MouseMove, MouseUp
  FMouseShift := Shift * [ssDouble];

  // Save time of last mouse down message (for use in mouse movement history)
  FLastMouseMessageTime := Cardinal(GetMessageTime);

  ToolContext := FController.CreateToolContext;
  if (ToolContext = nil) then
    exit;

  ToolContext.Update(GR32.Point(X, Y), ToolContext.PaintTool.SnapMouse);

  ToolContext.MouseParams.ShiftState := Shift;
  ToolContext.MouseParams.MouseMessageTime := FLastMouseMessageTime;

  // Save last mouse pos in screen coordinates for use with GetMouseMovePointsEx stuff
  FLastMousePos := ToolContext.MouseParams.ScreenPos;

  FController.MouseDown(ToolContext, Button);

  // Prevent nested operations. Happens if you start an operation with mbLeft and
  // then press mbRight during the operation.
  if (FController.ActivePaintTool <> nil) then
    exit;

  // BeginOperation will save ToolContext as FActivePaintToolContext if the operation is accepted
  if (not FController.BeginOperation(ToolContext)) then
    FMouseShift := [];
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintMouseController.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
const
  MaxMouseMovePointCount = 64;
var
  MouseMovePoint: TMouseMovePoint;
  MouseMovePoints: array[0..MaxMouseMovePointCount-1] of TMouseMovePoint;
  MouseMovePointCount: integer;
  MouseMovePointIndex: integer;
  ScreenPos: TPoint;
  LastMouseMessageTime: Cardinal;
  LastViewPortPos: TPoint;
  LastShiftState: TShiftState;
begin
  if (Layer <> FPaintHost.PaintLayer) then
    exit;

  if (FController.ActivePaintTool = nil) then
    exit;

  Assert(FController.ActivePaintToolContext <> nil);

  LastMouseMessageTime := Cardinal(GetMessageTime);

  ScreenPos := FPaintHost.ViewPortToScreen(GR32.Point(X, Y));

  // Ignore if mouse didn't move and shift state didn't change.
  // Note: There is an ABA race condition here if mouse moves from A to B to A.
  if (ScreenPos.X = FLastMousePos.X) and (ScreenPos.Y = FLastMousePos.Y) and (Shift = FController.ActivePaintToolContext.MouseParams.ShiftState) then
    exit;

  // Fetch history of mouse movement.
  // Get mouse coordinates up to and including the current movement message.
  // Note: GetMouseMovePointsEx is theoretically subject to ABA race condition.
  MouseMovePoint := Default(TMouseMovePoint);
  MouseMovePoint.x := ScreenPos.X and $0000FFFF; // Ensure that this number will pass through.
  MouseMovePoint.y := ScreenPos.Y and $0000FFFF;
  Cardinal(MouseMovePoint.time) := LastMouseMessageTime;

  MouseMovePointCount := GetMouseMovePointsEx(SizeOf(MouseMovePoint), MouseMovePoint, MouseMovePoints[0], MaxMouseMovePointCount, GMMP_USE_DISPLAY_POINTS);

  if (MouseMovePointCount = -1) then
  begin
    // If no history was retrieved we just store the current position in the history and proceed with that.
    MouseMovePointCount := 1;
    MouseMovePoints[0].x := ScreenPos.X;
    MouseMovePoints[0].y := ScreenPos.Y;
    Cardinal(MouseMovePoints[0].time) := LastMouseMessageTime;
  end else
  begin
    // Discard history older than last point we processed. Entries are stored most recent first.
    MouseMovePointIndex := 0;
    while (MouseMovePointIndex < MouseMovePointCount) do
    begin
      // Handle negative coordinates - required for multi monitor
      // TODO : Better handling of this; See GetMouseMovePointsEx documentation
      if (DWORD(MouseMovePoints[MouseMovePointIndex].x) >= $8000) then
        Dec(MouseMovePoints[MouseMovePointIndex].x, $00010000);

      if (DWORD(MouseMovePoints[MouseMovePointIndex].y) >= $8000) then
        Dec(MouseMovePoints[MouseMovePointIndex].y, $00010000);

      if (Cardinal(MouseMovePoints[MouseMovePointIndex].time) < FLastMouseMessageTime) or
        ((Cardinal(MouseMovePoints[MouseMovePointIndex].time) = FLastMouseMessageTime) and
         (MouseMovePoints[MouseMovePointIndex].x = FLastMousePos.X) and (MouseMovePoints[MouseMovePointIndex].y = FLastMousePos.Y)) then
      begin
        MouseMovePointCount := MouseMovePointIndex+1;
        break;
      end;

      Inc(MouseMovePointIndex);
    end;
  end;

  FLastMousePos := ScreenPos;
  FLastMouseMessageTime := LastMouseMessageTime;
  LastViewPortPos := FController.ActivePaintToolContext.MouseParams.ViewPortPos;
  LastShiftState := FController.ActivePaintToolContext.MouseParams.ShiftState;

  FController.ActivePaintToolContext.MouseParams.ShiftState := Shift + FMouseShift;

  MouseMovePointIndex := MouseMovePointCount-1;

  while (MouseMovePointIndex >= 0) do
  begin
    ScreenPos.X := MouseMovePoints[MouseMovePointIndex].x;
    ScreenPos.Y := MouseMovePoints[MouseMovePointIndex].y;

    FController.ActivePaintToolContext.Update(FPaintHost.ScreenToViewPort(ScreenPos), FController.ActivePaintTool.SnapMouse);

    FController.ActivePaintToolContext.MouseParams.MouseMessageTime := Cardinal(MouseMovePoints[MouseMovePointIndex].time);

    if (FController.ActivePaintToolContext.MouseParams.ViewPortPos <> LastViewPortPos) or (FController.ActivePaintToolContext.MouseParams.ShiftState <> LastShiftState) then
    begin
      FController.MouseMove(FController.ActivePaintToolContext);

      if (FController.ActivePaintTool <> nil) then
      begin
        if (not FController.ContinueOperation(FController.ActivePaintToolContext)) then
        begin
          FMouseShift := [];
          break;
        end;
      end;
    end;

    Dec(MouseMovePointIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintMouseController.HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  // TCustomImage32.DblClick calls MouseUp(mbLeft, [], 0, 0) - ignore this
  if (Button = mbLeft) and (Shift = []) and (X = 0) and (Y = 0) then
    exit;

  if (FController.ActivePaintTool = nil) then
    exit;

  FController.ActivePaintToolContext.Update(GR32.Point(X, Y), FController.ActivePaintTool.SnapMouse);
  FController.ActivePaintToolContext.MouseParams.ShiftState := Shift + FMouseShift;
  FController.ActivePaintToolContext.MouseParams.MouseMessageTime := Cardinal(GetMessageTime);

  FLastMousePos := FController.ActivePaintToolContext.MouseParams.ScreenPos;

  FController.MouseUp(FController.ActivePaintToolContext, Button);

  if (FController.ActivePaintTool <> nil) then
  begin
    FController.EndOperation(True);
    FMouseShift := [];
  end;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintMouseController.MouseEnter;
begin
  FController.MouseEnter;
end;

procedure TBitmap32PaintMouseController.MouseExit;
begin
  FController.MouseExit;
end;

//------------------------------------------------------------------------------

end.
