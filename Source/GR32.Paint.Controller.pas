unit GR32.Paint.Controller;

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
  GR32_System,
  GR32_Image,
  GR32_Layers,
  GR32.Paint.API,
  GR32.Paint.Tool,
  GR32.Paint.Tool.API,
  GR32.Paint.Controller.API;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintController
//
//------------------------------------------------------------------------------
// An example implementation of a paint controller using TImage32 or TImgView32.
//------------------------------------------------------------------------------
type
  TBitmap32PaintController = class(TInterfacedObject, IBitmap32PaintController)
  private
    FImage: TCustomImage32;
    FPaintHost: IBitmap32PaintHost;

  private
    FPaintTool: IBitmap32PaintTool; // The selected tool
    FActivePaintTool: IBitmap32PaintTool; // The tool that is currently handling mouse messages. Nil if none.
    FActivePaintToolContext: IBitmap32PaintToolContext;

    // Update batching
    FUpdateCount: integer;
    FLockCount: integer;
    FModified: boolean;

  private
    // IBitmap32PaintController
    function BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
    function ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
    procedure EndOperation(Complete: boolean);

    procedure BeginUpdate;
    procedure Changed;
    procedure EndUpdate;

    procedure BeginLockUpdate;
    procedure EndLockUpdate;

    function GetPaintTool: IBitmap32PaintTool;
    procedure SetPaintTool(const Value: IBitmap32PaintTool);

    function GetActivePaintTool: IBitmap32PaintTool;
    function GetActivePaintToolContext: IBitmap32PaintToolContext;
  private
    FUpdateTimer: TStopwatch;

  private
    procedure UpdateToolCursor;
    procedure SetToolCursor(NewCursor: TCursor);

    procedure SetActivePaintTool(const Value: IBitmap32PaintTool);

  public
    constructor Create(AImage: TCustomImage32; const APaintHost: IBitmap32PaintHost = nil);
    destructor Destroy; override;
  end;


//------------------------------------------------------------------------------
//
//      Global settings
//
//------------------------------------------------------------------------------
var
  // Max time between repaints during MouseMove/ContinueOperation
  Bitmap32PaintControllerMaxUpdateInterval: Cardinal = 50; // mS (zero for continuous update)


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Windows,
  SysUtils,
  GR32.Paint.Host;

type
  TImage32Cracker = class(TCustomImage32);

//------------------------------------------------------------------------------
//
//      TBitmap32PaintController
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintController.Create(AImage: TCustomImage32; const APaintHost: IBitmap32PaintHost);
begin
  inherited Create;

  FImage := AImage;

  if (APaintHost = nil) then
    // Embed
    FPaintHost := TBitmap32PaintHost.Create(FImage)
  else
    // Inject
    FPaintHost := APaintHost;
end;

destructor TBitmap32PaintController.Destroy;
begin
  SetPaintTool(nil);

  FPaintTool := nil; // In case the above failed
  FActivePaintTool := nil;
  FActivePaintToolContext := nil;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintController.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBitmap32PaintController.Changed;
begin
  if (FLockCount > 0) then
    exit;
  BeginUpdate;
  FModified := True;
  EndUpdate;
end;

procedure TBitmap32PaintController.EndUpdate;
begin
  if (FUpdateCount = 1) then
  begin
    FImage.Changed;

    FModified := False;
  end;
  Dec(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintController.BeginLockUpdate;
begin
  Inc(FLockCount);
end;

procedure TBitmap32PaintController.EndLockUpdate;
begin
  Dec(FLockCount);
end;

//------------------------------------------------------------------------------

function TBitmap32PaintController.GetActivePaintTool: IBitmap32PaintTool;
begin
  Result := FActivePaintTool;
end;

function TBitmap32PaintController.GetActivePaintToolContext: IBitmap32PaintToolContext;
begin
  Result := FActivePaintToolContext;
end;

procedure TBitmap32PaintController.SetActivePaintTool(const Value: IBitmap32PaintTool);
begin
  FActivePaintToolContext := nil;
  FActivePaintTool := Value;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintController.GetPaintTool: IBitmap32PaintTool;
begin
  Result := FPaintTool;
end;

procedure TBitmap32PaintController.SetPaintTool(const Value: IBitmap32PaintTool);
var
  Continue: boolean;
begin

  if (Value = FPaintTool) then
    exit;

  if (FActivePaintTool <> nil) then
    EndOperation(False);

  // Only activate new tool if we managed to deactivate old tool - or if there was no old tool
  if (FActivePaintTool = nil) then
  begin
    // Hide old cursor
    ShowCursor(False);

    if (FPaintTool <> nil) then
    begin
      FPaintTool.Deactivate;
      FPaintTool := nil;
    end;

    if (Value <> nil) then
    begin
      Continue := True;
      Value.Activate(Continue);

      if (Continue) then
      begin
        FPaintTool := Value;

        // Display new cursor
        UpdateToolCursor;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TBitmap32PaintController.SetToolCursor(NewCursor: TCursor);

  procedure UpdateCursor;
  var
    p: TPoint;
  begin
    GetCursorPos(p);
    SetCursorPos(p.X, p.Y);
  end;

begin
  if (FImage.Cursor <> NewCursor) then
  begin
    FImage.Cursor := NewCursor;

    // CM_CURSORCHANGED should force the cursor to update, but doesn't
    // ... so we have to resort to this ugly hack:
    UpdateCursor;
  end;
end;

procedure TBitmap32PaintController.UpdateToolCursor;
var
  NewCursor: TCursor;
  ParentForm: TWinControl;
begin
  if (FPaintTool = nil) or (not FPaintTool.GetCursor(NewCursor)) then
  begin
    ParentForm := FImage;
    while (ParentForm.Parent <> nil) do
      ParentForm := ParentForm.Parent;

    NewCursor := ParentForm.Cursor;
  end;

  SetToolCursor(NewCursor);
end;

//------------------------------------------------------------------------------

function TBitmap32PaintController.BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
var
  ToolState: TBitmap32PaintToolState;
  Continue: boolean;
begin
  Assert(FPaintTool <> nil);
  Continue := True;

  FPaintTool.BeginTool(Continue);
  try
    if (not Continue) then
    begin
      ToolState := tsAbort;
      Exit;
    end;

    if (FActivePaintTool = nil) then
      BeginUpdate;
    try

      ToolState := tsContinue;
      try

        FPaintTool.BeginAction(Context, ToolState);

      except
        ToolState := tsAbort;
        FPaintTool.EndAction(Context, ToolState);
        raise;
      end;

      case ToolState of
        tsComplete:
          ;

        tsAbort:
          ;

        tsContinue:
          begin
            SetActivePaintTool(FPaintTool);
            FActivePaintToolContext := Context;

            // Reacquire capture in case tool did something stupid that caused us to lose it (e.g. Move Select tool)
            if (not (betfMouseCapture in FPaintTool.ToolFeatures)) and (GetCapture <> FImage.Handle) then
              SetCapture(FImage.Handle);
          end;
      end;

    finally
      if (ToolState <> tsContinue) then
        EndUpdate;
    end;
  finally
    Result := (ToolState = tsContinue);

    if (not Result) then
    begin
      FPaintTool.EndTool;
      SetActivePaintTool(nil);
    end;
  end;

  FImage.Update;
end;

function TBitmap32PaintController.ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
var
  ToolState: TBitmap32PaintToolState;
begin
  if (FActivePaintTool = nil) then
    raise Exception.Create('Operation is not in progress');

  Result := False;

  ToolState := tsContinue;
  try

    try
      FActivePaintTool.ContinueAction(FActivePaintToolContext, ToolState);

      // Note: EndOperation may be called from Tool.ContinueAction so we must
      // not assume that the tool is still active when ContinueAction returns.
      if (FActivePaintTool = nil) then
        exit;

    except
      ToolState := tsAbort;
      raise;
    end;

    case ToolState of
      tsComplete:
        ;

      tsAbort:
        ;

      tsContinue:
        begin
          Result := True;

          // Reacquire capture in case tool did something stupid that caused us to lose it (e.g. Move Select tool)
          if (not (betfMouseCapture in FPaintTool.ToolFeatures)) and (GetCapture <> FImage.Handle) then
            SetCapture(FImage.Handle);
        end;
    end;

  finally
    if (ToolState <> tsContinue) and (FActivePaintTool <> nil) then
    begin
      EndUpdate;
      FActivePaintTool.EndTool;
      SetActivePaintTool(nil);
    end;
  end;

  // Repaint immediately to avoid lag caused by continous mouse messages during the operation.
  // The WM_PAINT messages are only generated once the message queue is otherwise empty or
  // UpdateWindow is called.
  if (not TImage32Cracker(FImage).CacheValid) or (not TImage32Cracker(FImage).BufferValid) then
  begin
    // Buffer has been invalidated. Limit how long we wait for an update.

    if (Bitmap32PaintControllerMaxUpdateInterval = 0) then
      // No wait; Update immediately
      FImage.Update
    else
    if (FUpdateTimer.IsRunning) then
    begin
      // Already waiting; Have we waited long enough?
      if (FUpdateTimer.ElapsedMilliseconds > Bitmap32PaintControllerMaxUpdateInterval) then
      begin
        FImage.Update;
        FUpdateTimer.Stop;
      end;
    end else
      // Not already waiting; Start timer
      FUpdateTimer := TStopwatch.StartNew;
  end else
    // Nothing to update; Don't wait.
    FUpdateTimer.Stop;
end;

procedure TBitmap32PaintController.EndOperation(Complete: boolean);
var
  ToolState: TBitmap32PaintToolState;
begin
  if (FActivePaintTool = nil) then
    exit; // TODO : Is this an error condition?

  // Ensure mouse capture is released (this takes care of right-button which TImage32 doesn't handle properly)
  if (FActivePaintTool = nil) and (GetCapture = FImage.Handle) then
    ReleaseCapture;

  if (Complete) then
    ToolState := tsComplete
  else
    ToolState := tsAbort;
  try

    try

      FActivePaintTool.EndAction(FActivePaintToolContext, ToolState);

      if (FActivePaintTool = nil) then
        exit;

    except
      ToolState := tsAbort;
      raise;
    end;

    case ToolState of
      tsComplete:
        ;

      tsAbort:
        ;
    end;

  finally
    if (ToolState <> tsContinue) and (FActivePaintTool <> nil) then
    begin
      EndUpdate;
      FActivePaintTool.EndTool;
      SetActivePaintTool(nil);
    end;
  end;

  FImage.Changed;
end;

//------------------------------------------------------------------------------

end.
