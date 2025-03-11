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
  GR32.Paint.Host.API,
  GR32.Paint.Tool.API,
  GR32.Paint.Controller.API;


//------------------------------------------------------------------------------
//
//      TCustomBitmap32PaintController
//
//------------------------------------------------------------------------------
// Sample paint controller base class.
//------------------------------------------------------------------------------
type
  TCustomBitmap32PaintController = class(TInterfacedObject, IBitmap32PaintController)
  strict private
    FPaintHost: IBitmap32PaintHost;
    // Cached feature capabilities
    FFeatureCursor: IBitmap32PaintFeatureCursor;
    FFeatureVectorCursor: IBitmap32PaintFeatureVectorCursor;

  strict protected
    property PaintHost: IBitmap32PaintHost read FPaintHost;

  strict private
    // Tool
    FPaintTool: IBitmap32PaintTool; // The selected tool
    FActivePaintTool: IBitmap32PaintTool; // The tool that is currently handling mouse messages. Nil if none.
    FActivePaintToolContext: IBitmap32PaintToolContext;

  strict protected
    procedure SetActivePaintTool(const Value: IBitmap32PaintTool);
    property PaintTool: IBitmap32PaintTool read FPaintTool;
    property ActivePaintTool: IBitmap32PaintTool read FActivePaintTool;
    property ActivePaintToolContext: IBitmap32PaintToolContext read FActivePaintToolContext;

  strict protected
    // Cursor
    procedure ShowToolCursor(AShow, ATransientChange: Boolean);
    procedure UpdateToolCursor;

  strict protected
    // IBitmap32PaintController
    function BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
    function ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
    procedure EndOperation(Complete: boolean);

    procedure MouseDown(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
    procedure MouseMove(const Context: IBitmap32PaintToolContext);
    procedure MouseUp(const Context: IBitmap32PaintToolContext; Button: TMouseButton);

    procedure MouseEnter;
    procedure MouseExit;

    function CreateToolContext: IBitmap32PaintToolContext;

    function GetPaintTool: IBitmap32PaintTool;
    procedure SetPaintTool(const Value: IBitmap32PaintTool);

    function GetActivePaintTool: IBitmap32PaintTool;
    function GetActivePaintToolContext: IBitmap32PaintToolContext;

  public
    constructor Create(const APaintHost: IBitmap32PaintHost);
    destructor Destroy; override;
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintController
//
//------------------------------------------------------------------------------
// An example paint controller optimized for use with TImage32 and TImgView32.
//------------------------------------------------------------------------------
type
  TBitmap32PaintController = class(TCustomBitmap32PaintController, IBitmap32PaintController)
  strict private
    FImage: TCustomImage32;

  strict private
    // Update optimization
    FUpdateTimer: TStopwatch;

  strict protected
    function GetHasCapture: boolean;
    procedure SetHasCapture(const Value: boolean);

  strict protected
    // IBitmap32PaintController
    function BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
    function ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
    procedure EndOperation(Complete: boolean);
    property HasCapture: boolean read GetHasCapture write SetHasCapture;
  public
    constructor Create(AImage: TCustomImage32; const APaintHost: IBitmap32PaintHost = nil);
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
{$if defined(MSWINDOWS)}
  Windows,
{$ifend}
  SysUtils,
  GR32.Paint.Host;

type
  TImage32Cracker = class(TCustomImage32);

//------------------------------------------------------------------------------
//
//      TCustomBitmap32PaintController
//
//------------------------------------------------------------------------------
constructor TCustomBitmap32PaintController.Create(const APaintHost: IBitmap32PaintHost);
begin
  inherited Create;

  FPaintHost := APaintHost;

  // Cache feature capabilities so we don't have to resolve these continously
  if (not Supports(FPaintHost, IBitmap32PaintFeatureCursor, FFeatureCursor)) then
    FFeatureCursor := nil;

  if (not Supports(FPaintHost, IBitmap32PaintFeatureVectorCursor, FFeatureVectorCursor)) then
    FFeatureVectorCursor := nil;
end;

destructor TCustomBitmap32PaintController.Destroy;
begin
  SetPaintTool(nil);

  FPaintTool := nil; // In case the above failed
  FActivePaintTool := nil;
  FActivePaintToolContext := nil;

  inherited;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintController.CreateToolContext: IBitmap32PaintToolContext;
var
  Tool: IBitmap32PaintTool;
begin
  if (FActivePaintTool <> nil) then
    Tool := FActivePaintTool
  else
    Tool := FPaintTool;

  if (Tool <> nil) then
    Result := FPaintHost.CreateToolContext(Tool)
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintController.GetActivePaintTool: IBitmap32PaintTool;
begin
  Result := FActivePaintTool;
end;

function TCustomBitmap32PaintController.GetActivePaintToolContext: IBitmap32PaintToolContext;
begin
  Result := FActivePaintToolContext;
end;

procedure TCustomBitmap32PaintController.SetActivePaintTool(const Value: IBitmap32PaintTool);
begin
  FActivePaintToolContext := nil;
  FActivePaintTool := Value;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintController.GetPaintTool: IBitmap32PaintTool;
begin
  Result := FPaintTool;
end;

procedure TCustomBitmap32PaintController.SetPaintTool(const Value: IBitmap32PaintTool);
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
    ShowToolCursor(False, False);

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

procedure TCustomBitmap32PaintController.ShowToolCursor(AShow, ATransientChange: boolean);
begin
  if (FFeatureCursor <> nil) then
    FFeatureCursor.ShowToolCursor(AShow, ATransientChange);
end;

procedure TCustomBitmap32PaintController.UpdateToolCursor;
var
  NewCursor: TCursor;
begin
  if (FFeatureCursor = nil) then
    exit;

  // Note: Calling FPaintTool.GetCursor will automatically create
  // a complex cursor, if the tool supplies one.

  if (FPaintTool = nil) or (not FPaintTool.GetCursor(NewCursor)) then
    NewCursor := crDefault;

  FFeatureCursor.SetToolCursor(NewCursor);

  ShowToolCursor(True, False);
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintController.BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
var
  ToolState: TBitmap32PaintToolState;
  Continue: boolean;
begin
  Assert(FPaintTool <> nil);
  Continue := True;
  Result := False;

  FPaintTool.BeginTool(Continue);
  try

    if (not Continue) then
    begin
      ToolState := tsAbort;
      Exit;
    end;

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
          Result := True;
        end;
    end;

  finally
    if (not Result) then
    begin
      FPaintTool.EndTool;
      SetActivePaintTool(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TCustomBitmap32PaintController.ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
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
        Result := True;
    end;

  finally
    if (not Result) and (FActivePaintTool <> nil) then
    begin
      FActivePaintTool.EndTool;
      SetActivePaintTool(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintController.EndOperation(Complete: boolean);
var
  ToolState: TBitmap32PaintToolState;
begin
  if (FActivePaintTool = nil) then
    exit; // TODO : Is this an error condition?

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
      FActivePaintTool.EndTool;
      SetActivePaintTool(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintController.MouseEnter;
begin
  // Enable vector cursor
  ShowToolCursor(True, True);
end;

procedure TCustomBitmap32PaintController.MouseExit;
begin
  // Disable vector cursor
  ShowToolCursor(False, True);
end;

//------------------------------------------------------------------------------

procedure TCustomBitmap32PaintController.MouseDown(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
begin
  if (Context.PaintTool <> nil) then
    Context.PaintTool.MouseDown(Context, Button);
end;

procedure TCustomBitmap32PaintController.MouseMove(const Context: IBitmap32PaintToolContext);
begin
  if (Context.PaintTool <> nil) then
    Context.PaintTool.MouseMove(Context);

  // Update cursor layer with most recent position
  if (FFeatureVectorCursor <> nil) then
    FFeatureVectorCursor.MoveToolVectorCursor(Context.MouseParams.ViewPortPos);
end;

procedure TCustomBitmap32PaintController.MouseUp(const Context: IBitmap32PaintToolContext; Button: TMouseButton);
begin
  if (Context.PaintTool <> nil) then
    Context.PaintTool.MouseUp(Context, Button);
end;


//------------------------------------------------------------------------------
//
//      TBitmap32PaintController
//
//------------------------------------------------------------------------------
constructor TBitmap32PaintController.Create(AImage: TCustomImage32; const APaintHost: IBitmap32PaintHost);
var
  PaintHost: IBitmap32PaintHost;
begin
  if (APaintHost = nil) then
    // Embed
    PaintHost := TBitmap32PaintHost.Create(AImage)
  else
    // Inject
    PaintHost := APaintHost;

  inherited Create(PaintHost);

  FImage := AImage;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintController.BeginOperation(const Context: IBitmap32PaintToolContext): boolean;
begin
  Result := inherited;

  if (Result) then
  begin
    // Reacquire capture in case tool did something stupid that caused us to lose it (e.g. Move Select tool)
    if (not (betfMouseCapture in PaintTool.ToolFeatures)) and (not HasCapture) then
      HasCapture := True;
  end;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintController.ContinueOperation(const Context: IBitmap32PaintToolContext): boolean;
begin
  Result := inherited;

  if (Result) then
  begin
    // Reacquire capture in case tool did something stupid that caused us to lose it (e.g. Move Select tool)
    if (not (betfMouseCapture in PaintTool.ToolFeatures)) and (not HasCapture) then
      HasCapture := True;
  end;

  // Repaint ASAP to avoid lag caused by continous mouse messages during the operation.
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

//------------------------------------------------------------------------------

procedure TBitmap32PaintController.EndOperation(Complete: boolean);
begin
  inherited;

  // Ensure mouse capture is released (this takes care of right-button which TImage32 doesn't handle properly)
  // TODO : I'm not sure that this is necessary anymore but there's no harm in it
  if (ActivePaintTool = nil) and (HasCapture) then
    HasCapture := False;
end;

//------------------------------------------------------------------------------

function TBitmap32PaintController.GetHasCapture: boolean;
begin
{$if defined(MSWINDOWS)}
  Result := (GetCapture = FImage.Handle);
{$else}
  Result := (GetCaptureControl = FImage);
{$ifend}
end;

procedure TBitmap32PaintController.SetHasCapture(const Value: boolean);
begin
{$if defined(MSWINDOWS)}
  if (Value) then
    SetCapture(FImage.Handle)
  else
    ReleaseCapture;
{$else}
  if (Value) then
    SetCaptureControl(FImage)
  else
    SetCaptureControl(nil);
{$ifend}
end;

//------------------------------------------------------------------------------

end.
