unit GR32.Design.DebugVisualizers;

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
 * The Original Code is Delphi debugger support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$if defined(FPC)}
  {$Message Fatal 'This unit does not support FreePascal'}
{$ifend}

{$if (CompilerVersion < 25.0)}
  {$Message Fatal 'Delphi 10.2 Tokyo and later required'}
{$ifend}

uses
  Windows, SysUtils, Classes, Forms, Controls,
  ToolsAPI,
  GR32.Design.BitmapEditor;

//------------------------------------------------------------------------------
//
//      TBitmap32DebuggerVisualizerBase
//
//------------------------------------------------------------------------------
// Abstract base class for Graphics32 visualizers, implementing common debugger
// visualizer and thread notification routines.
//------------------------------------------------------------------------------
type
  TGraphics32DebuggerVisualizer = class abstract(TInterfacedObject,
    IOTADebuggerVisualizer,
    IOTAThreadNotifier,
    IOTAThreadNotifier160)
  protected
    FCompleted: Boolean;
    FDeferredError: Boolean;
    FDeferredResult: string;
    FNotifierIndex: Integer;

    // Thread evaluation helper to evaluate expression in debuggee process
    function EvaluateExpression(const AThread: IOTAThread; const AExpression: string): string;

    // Expression string cleaning helper
    function CleanExpressionResult(const AValue: string): string;

  private
    // IOTAThreadNotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: Cardinal; ReturnCode: Integer); overload;
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);

    // IOTAThreadNotifier160
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer); overload;

  protected
    // IOTADebuggerVisualizer
    function GetSupportedTypeCount: Integer; virtual; abstract;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean); virtual; abstract;
    function GetVisualizerIdentifier: string; virtual; abstract;
    function GetVisualizerName: string; virtual; abstract;
    function GetVisualizerDescription: string; virtual; abstract;
  end;


//------------------------------------------------------------------------------
//
//      TGraphics32ExternalViewerUpdater
//
//------------------------------------------------------------------------------
// External viewer updater interface returned to IDE to receive lifetime updates
//------------------------------------------------------------------------------
type
  TGraphics32ExternalViewerUpdater = class(TInterfacedObject,
    IOTADebuggerVisualizerExternalViewerUpdater)
  private
    FClosedProc: TOTAVisualizerClosedProcedure;
    FViewerForm: TCustomForm;

    procedure FormCloseHandler(Sender: TObject; var Action: TCloseAction);

  private
    // IOTADebuggerVisualizerExternalViewerUpdater
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
  public
    constructor Create(AForm: TCustomForm);
    destructor Destroy; override;
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32DebuggerVisualizer
//
//------------------------------------------------------------------------------
// Base class for TBitmap32 visualizers.
//------------------------------------------------------------------------------
type
  TBitmap32DebuggerVisualizer = class abstract(TGraphics32DebuggerVisualizer)
  protected
    // IOTADebuggerVisualizer
    function GetSupportedTypeCount: Integer; override;
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendants: Boolean); override;
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32ValueReplacer
//
//------------------------------------------------------------------------------
// Value replacer visualizer: shows bitmap dimensions e.g. TBitmap32 [800x600].
//------------------------------------------------------------------------------
type
  TBitmap32ValueReplacer = class(TBitmap32DebuggerVisualizer,
    IOTADebuggerVisualizerValueReplacer)
  private class var
    FValueReplacer: IOTADebuggerVisualizer;
  private
    // IOTADebuggerVisualizerValueReplacer
    function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;

  protected
    // IOTADebuggerVisualizer
    function GetVisualizerIdentifier: string; override;
    function GetVisualizerName: string; override;
    function GetVisualizerDescription: string; override;

  public
    class procedure Register(const DebuggerServices: IOTADebuggerServices); static;
    class procedure Unregister(const DebuggerServices: IOTADebuggerServices); static;
  end;


//------------------------------------------------------------------------------
//
//      TBitmap32Viewer
//
//------------------------------------------------------------------------------
// TBitmap32 viewer: opens TPictureEditorForm from GR32.Design.BitmapEditor
//------------------------------------------------------------------------------
type
  TBitmap32Viewer = class(TBitmap32DebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer)
  private class var
    FViewer: IOTADebuggerVisualizer;
  private
    // IOTADebuggerVisualizerExternalViewer
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string;
      SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;

  protected
    // IOTADebuggerVisualizer
    function GetVisualizerIdentifier: string; override;
    function GetVisualizerName: string; override;
    function GetVisualizerDescription: string; override;

  public
    class procedure Register(const DebuggerServices: IOTADebuggerServices); static;
    class procedure Unregister(const DebuggerServices: IOTADebuggerServices); static;
  end;


//------------------------------------------------------------------------------
//
//      IDE registration
//
//------------------------------------------------------------------------------
procedure RegisterDebugVisualizers;
procedure UnregisterDebugVisualizers;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32,
  GR32_Backends_Generic;

resourcestring
  sBitmap32ValueReplacerName = 'Graphics32 Bitmap Value Replacer';
  sBitmap32ValueReplacerDesc = 'Displays the dimensions of a TCustomBitmap32 / TBitmap32 instance';
  sBitmap32CustomViewerName  = 'Graphics32 Bitmap Visualizer';
  sBitmap32CustomViewerDesc  = 'Displays a preview of a TCustomBitmap32 / TBitmap32 image in the Bitmap Editor';
  sBitmap32CustomViewerMenu  = 'Show Bitmap32 Image';
  sBitmap32EmptyInfo         = '(empty)';


//------------------------------------------------------------------------------
//
//      TGraphics32DebuggerVisualizer
//
//------------------------------------------------------------------------------
function TGraphics32DebuggerVisualizer.CleanExpressionResult(const AValue: string): string;
var
  Len: Integer;
begin
  Result := Trim(AValue);
  Len := Length(Result);
  // Strip surrounding single quotes if present (e.g. '800' -> 800)
  if (Len >= 2) and (Result[1] = '''') and (Result[Len] = '''') then
    Result := Copy(Result, 2, Len - 2);
end;

function TGraphics32DebuggerVisualizer.EvaluateExpression(const AThread: IOTAThread;
  const AExpression: string): string;
var
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr: TOTAAddress;
  ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
  WaitCount: Integer;
begin
  Result := '';
  if AThread = nil then
    Exit;

  if not Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    Exit;

  repeat
    Done := True;
    EvalRes := AThread.Evaluate(AExpression, @ResultStr, Length(ResultStr),
      CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);

    case EvalRes of
      erOK:
        Result := CleanExpressionResult(ResultStr);

      erDeferred:
        begin
          FCompleted := False;
          FDeferredResult := '';
          FDeferredError := False;
          FNotifierIndex := AThread.AddNotifier(Self);
          try
            WaitCount := 0;
            // Pump debug events with a timeout guard (~5 seconds) to prevent IDE lockups
            while not FCompleted and (WaitCount < 500) do
            begin
              DebugSvcs.ProcessDebugEvents;
              Sleep(10);
              Inc(WaitCount);
            end;
          finally
            AThread.RemoveNotifier(FNotifierIndex);
            FNotifierIndex := -1;
          end;

          if not FDeferredError then
          begin
            if FDeferredResult <> '' then
              Result := CleanExpressionResult(FDeferredResult)
            else
              Result := CleanExpressionResult(ResultStr);
          end;
        end;

      erBusy:
        begin
          DebugSvcs.ProcessDebugEvents;
          Done := False;
        end;
    end;
  until Done;
end;

procedure TGraphics32DebuggerVisualizer.AfterSave;
begin
end;

procedure TGraphics32DebuggerVisualizer.BeforeSave;
begin
end;

procedure TGraphics32DebuggerVisualizer.Destroyed;
begin
end;

procedure TGraphics32DebuggerVisualizer.Modified;
begin
end;

procedure TGraphics32DebuggerVisualizer.ModifyComplete(const ExprStr, ResultStr: string;
  ReturnCode: Integer);
begin
end;

procedure TGraphics32DebuggerVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin
end;

procedure TGraphics32DebuggerVisualizer.EvaluateComplete(const ExprStr, ResultStr: string;
  CanModify: Boolean; ResultAddress, ResultSize: Cardinal; ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress),
    LongWord(ResultSize), ReturnCode);
end;

procedure TGraphics32DebuggerVisualizer.EvaluateComplete(const ExprStr, ResultStr: string;
  CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := (ReturnCode <> 0);
end;


//------------------------------------------------------------------------------
//
//      TBitmap32DebuggerVisualizer
//
//------------------------------------------------------------------------------
function TBitmap32DebuggerVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 2;
end;

procedure TBitmap32DebuggerVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  AllDescendants := False;
  case Index of
    0: TypeName := 'TCustomBitmap32';
    1: TypeName := 'TBitmap32';
  else
    TypeName := 'TCustomBitmap32';
  end;
end;


//------------------------------------------------------------------------------
//
//      TBitmap32ValueReplacer
//
//------------------------------------------------------------------------------
function TBitmap32ValueReplacer.GetVisualizerIdentifier: string;
begin
  Result := 'Graphics32.Bitmap32ValueReplacer';
end;

function TBitmap32ValueReplacer.GetVisualizerName: string;
begin
  Result := sBitmap32ValueReplacerName;
end;

class procedure TBitmap32ValueReplacer.Register(const DebuggerServices: IOTADebuggerServices);
begin
  if (FValueReplacer = nil) then
  begin
    FValueReplacer := TBitmap32ValueReplacer.Create;
    DebuggerServices.RegisterDebugVisualizer(FValueReplacer);
  end;
end;

class procedure TBitmap32ValueReplacer.Unregister(const DebuggerServices: IOTADebuggerServices);
begin
  if (FValueReplacer <> nil) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(FValueReplacer);
    FValueReplacer := nil;
  end;
end;

function TBitmap32ValueReplacer.GetVisualizerDescription: string;
begin
  Result := sBitmap32ValueReplacerDesc;
end;

function TBitmap32ValueReplacer.GetReplacementValue(const Expression, TypeName,
  EvalResult: string): string;
var
  DebugSvcs: IOTADebuggerServices;
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  WidthStr, HeightStr: string;
  W, H: Integer;
begin
  Result := EvalResult;

  // If the instance pointer is nil ($0) or uninitialized, keep default EvalResult
  if (EvalResult = 'nil') or (EvalResult = '$0') or (EvalResult = '') then
    Exit;

  if not Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    Exit;

  CurProcess := DebugSvcs.CurrentProcess;
  if CurProcess = nil then
    Exit;

  CurThread := CurProcess.CurrentThread;
  if CurThread = nil then
    Exit;

  WidthStr := EvaluateExpression(CurThread, Expression + '.Width');
  HeightStr := EvaluateExpression(CurThread, Expression + '.Height');

  W := StrToIntDef(WidthStr, -1);
  H := StrToIntDef(HeightStr, -1);

  if (W >= 0) and (H >= 0) then
  begin
    if (W = 0) or (H = 0) then
      Result := Format('%s %s [%d x %d]', [TypeName, sBitmap32EmptyInfo, W, H])
    else
      Result := Format('%s [%d x %d]', [TypeName, W, H]);
  end;
end;


//------------------------------------------------------------------------------
//
//      TGraphics32ExternalViewerUpdater
//
//------------------------------------------------------------------------------
constructor TGraphics32ExternalViewerUpdater.Create(AForm: TCustomForm);
begin
  inherited Create;
  FViewerForm := AForm;
  if FViewerForm <> nil then
    TForm(FViewerForm).OnClose := FormCloseHandler;
end;

destructor TGraphics32ExternalViewerUpdater.Destroy;
begin
  if FViewerForm <> nil then
  begin
    TForm(FViewerForm).OnClose := nil;
    FreeAndNil(FViewerForm);
  end;
  inherited;
end;

procedure TGraphics32ExternalViewerUpdater.FormCloseHandler(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FViewerForm := nil;
  if Assigned(FClosedProc) then
    FClosedProc;
end;

procedure TGraphics32ExternalViewerUpdater.CloseVisualizer;
begin
  if FViewerForm <> nil then
  begin
    FViewerForm.Close;
    FViewerForm := nil;
  end;
end;

procedure TGraphics32ExternalViewerUpdater.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  // Process is running or out of scope
end;

procedure TGraphics32ExternalViewerUpdater.RefreshVisualizer(const Expression,
  TypeName, EvalResult: string);
begin
  // Refresh request from debugger
end;

procedure TGraphics32ExternalViewerUpdater.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;


//------------------------------------------------------------------------------
//
//      TBitmap32Viewer
//
//------------------------------------------------------------------------------
function TBitmap32Viewer.GetVisualizerIdentifier: string;
begin
  Result := 'Graphics32.Bitmap32CustomViewer';
end;

function TBitmap32Viewer.GetVisualizerName: string;
begin
  Result := sBitmap32CustomViewerName;
end;

class procedure TBitmap32Viewer.Register(const DebuggerServices: IOTADebuggerServices);
begin
  if (FViewer = nil) then
  begin
    FViewer := TBitmap32Viewer.Create;
    DebuggerServices.RegisterDebugVisualizer(FViewer);
  end;
end;

class procedure TBitmap32Viewer.Unregister(const DebuggerServices: IOTADebuggerServices);
begin
  if (FViewer <> nil) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(FViewer);
    FViewer := nil;
  end;
end;

function TBitmap32Viewer.GetVisualizerDescription: string;
begin
  Result := sBitmap32CustomViewerDesc;
end;

function TBitmap32Viewer.GetMenuText: string;
begin
  Result := sBitmap32CustomViewerMenu;
end;

function TBitmap32Viewer.Show(const Expression, TypeName, EvalResult: string;
  SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  DebuggerServices: IOTADebuggerServices;
  CurrentProcess: IOTAProcess;
  CurrentThread: IOTAThread;
  sWidth, sHeight, sBits: string;
  Widht, Height: Integer;
  BitsAddrInt: UInt64;
  PixelByteSize: NativeUInt;
  LocalBitmap: TBitmap32;
  EditorForm: TPictureEditorForm;
begin
  Result := nil;

  // If object is nil, there is nothing to view
  if (EvalResult = 'nil') or (EvalResult = '$0') or (EvalResult = '') then
    Exit;

  if not Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
    Exit;

  CurrentProcess := DebuggerServices.CurrentProcess;
  if CurrentProcess = nil then
    Exit;

  CurrentThread := CurrentProcess.CurrentThread;
  if CurrentThread = nil then
    Exit;

  // Retrieve dimensions and Bits memory address from debuggee
  sWidth := EvaluateExpression(CurrentThread, Expression + '.Width');
  sHeight := EvaluateExpression(CurrentThread, Expression + '.Height');
  // Evaluate as NativeUInt to extract a clean numeric address
  sBits := EvaluateExpression(CurrentThread, 'NativeUInt(' + Expression + '.Bits)');
  if sBits = '' then
    sBits := EvaluateExpression(CurrentThread, Expression + '.Bits');

  Widht := StrToIntDef(sWidth, 0);
  Height := StrToIntDef(sHeight, 0);

  if (Widht = 0) or (Height = 0) then
    exit;

  // Parse hexadecimal or decimal address
  sBits := Trim(sBits);
  if (Length(sBits) >= 2) and (Copy(sBits, 1, 2) = '0x') then
    sBits := '$' + Copy(sBits, 3, Length(sBits));

  BitsAddrInt := 0;
  if (sBits <> '') and (sBits <> 'nil') and (sBits <> '$0') then
    BitsAddrInt := StrToUInt64Def(sBits, 0);

  if (BitsAddrInt = 0) then
    exit;

  LocalBitmap := TBitmap32.Create(TMemoryBackend);
  try
    LocalBitmap.SetSize(Widht, Height, False);
    PixelByteSize := LocalBitmap.ByteCount;

    // Safely read pixel memory directly from the debuggee process
    var Buffer: pointer := LocalBitmap.Bits;
    CurrentProcess.ReadProcessMemory(TOTAAddress(BitsAddrInt), PixelByteSize, Buffer^);

    // Display the image non-modally in TPictureEditorForm managed by the updater
    EditorForm := TPictureEditorForm.Create(Application);
    try
      EditorForm.Caption := Format('%s - TBitmap32 viewer', [Expression]);
      EditorForm.ReadOnly := True; // We cannot modify the bitmap - yet

      EditorForm.LoadFromImage(LocalBitmap);
      EditorForm.Show;

      if (SuggestedLeft >= 0) then
      begin
        if (SuggestedLeft + EditorForm.Width < EditorForm.Monitor.Width) or (SuggestedLeft - EditorForm.Width < 0) then
          EditorForm.Left := SuggestedLeft // Right
        else
          EditorForm.Left := SuggestedLeft - EditorForm.Width; // Left
      end;

      if (SuggestedTop >= 0) then
      begin
        if (SuggestedTop + EditorForm.Height < EditorForm.Monitor.Height) or (SuggestedTop - EditorForm.Height < 0) then
          EditorForm.Top := SuggestedTop // Below
        else
          EditorForm.Top := SuggestedTop - EditorForm.Height; // Above
      end;

      Result := TGraphics32ExternalViewerUpdater.Create(EditorForm);

    except
      EditorForm.Free;
      raise;
    end;
  finally
    LocalBitmap.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//      IDE registration
//
//------------------------------------------------------------------------------
procedure RegisterDebugVisualizers;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    TBitmap32ValueReplacer.Register(DebuggerServices);
    TBitmap32Viewer.Register(DebuggerServices);
  end;
end;

procedure UnregisterDebugVisualizers;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    TBitmap32ValueReplacer.Unregister(DebuggerServices);
    TBitmap32Viewer.Unregister(DebuggerServices);
  end;
end;

initialization

finalization
  UnregisterDebugVisualizers;

end.
