unit GR32_Bindings;

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
 * The Original Code is Run-time Function Bindings for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson
 * mattias@centaurix.com
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Generics.Collections,
  Classes,
  GR32.CPUID;

//------------------------------------------------------------------------------
//
//      CPU feature convenience aliases
//
//------------------------------------------------------------------------------
// For use in CPU dispatch bindings
// For the most common usage, these alioases avoids the need to reference the
// GR32.CPUID unit directly.
//------------------------------------------------------------------------------
type
  TCPU = GR32.CPUID.TCPU;
  TInstructionSupport = GR32.CPUID.TInstructionSupport;
  TCPUInstructionSet = GR32.CPUID.TCPUInstructionSet;

const
  isPascal = GR32.CPUID.TCPUInstructionSet.isPascal;
  isAssembler = GR32.CPUID.TCPUInstructionSet.isAssembler;
  isReference = GR32.CPUID.TCPUInstructionSet.isReference;
  isMMX = GR32.CPUID.TCPUInstructionSet.isMMX;
  isExMMX = GR32.CPUID.TCPUInstructionSet.isExMMX;
  isSSE = GR32.CPUID.TCPUInstructionSet.isSSE;
  isSSE2 = GR32.CPUID.TCPUInstructionSet.isSSE2;
  isSSE3 = GR32.CPUID.TCPUInstructionSet.isSSE3;
  isSSSE3 = GR32.CPUID.TCPUInstructionSet.isSSSE3;
  isSSE41 = GR32.CPUID.TCPUInstructionSet.isSSE41;
  isSSE42 = GR32.CPUID.TCPUInstructionSet.isSSE42;
  isAVX = GR32.CPUID.TCPUInstructionSet.isAVX;
  isAVX2 = GR32.CPUID.TCPUInstructionSet.isAVX2;
  isAVX512f = GR32.CPUID.TCPUInstructionSet.isAVX512f;


//------------------------------------------------------------------------------
//
//      TFunctionInfo
//
//------------------------------------------------------------------------------
// Describes a function implementation.
//------------------------------------------------------------------------------
type
  TFunctionInfo = record
    FunctionID: NativeInt;      // Either an ID or a pointer
    Proc: Pointer;              // Pointer to the implementing function
    InstructionSupport: TInstructionSupport; // The CPU features required by this implementation
    Priority: Integer;          // Function priority; Smaller is better. Used by default TFunctionPriority callback
    Flags: Cardinal;            // Optional, user defined flags for use in a custom TFunctionPriority callback
  end;
  PFunctionInfo = ^TFunctionInfo;


//------------------------------------------------------------------------------
//
//      TFunctionPriority
//
//------------------------------------------------------------------------------
// Delegate used when evaluating a binding resolution.
//------------------------------------------------------------------------------
type
  TFunctionPriority = function(Info: PFunctionInfo): Integer;


//------------------------------------------------------------------------------
//
//      TFunctionRegistry
//
//------------------------------------------------------------------------------
// This class fascilitates a registry that allows multiple function to be
// registered together with information about their CPU requirements and
// an additional 'flags' parameter. Functions that share the same FunctionID
// can be assigned to a function variable through the rebind methods.
// A priority callback function is used to assess the most optimal function.
//------------------------------------------------------------------------------
const
  BindingPriorityDefault = 0;   // Default priority
  BindingPriorityBetter = -1;   // Negative = Better
  BindingPriorityWorse = 1;     // Positive = Worse

type
  TFunctionRegistry = class(TPersistent)
  private type
    PFunctionBinding = ^TFunctionBinding;
    TFunctionBinding = record
      FunctionID: NativeInt;      // Either an ID or a pointer
      BindVariable: PPointer;     // Pointer to the function delegate
    end;

  type
    TFunctionInfoList = TList<TFunctionInfo>;
    TFunctionBindingList = TList<TFunctionBinding>;
{$IFDEF FPC}
    TFunctionInfoListCracker = class(TFunctionInfoList);
    TFunctionBindingListCracker = class(TFunctionBindingList);
{$ENDIF}

  private class var
    FBindingRegistries: TObjectList<TFunctionRegistry>;

  private
    FItems: TFunctionInfoList;
    FBindings: TFunctionBindingList;
    FName: string;
    FNeedRebind: boolean;

    procedure SetName(const Value: string);

    class function NewRegistry(const Name: string): TFunctionRegistry;
    class destructor Destroy;
  protected
    function FindBinding(BindVariable: PPointer): NativeInt;
    function FindFunctionInfo(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): PFunctionInfo; overload;
    function FindFunctionInfo(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): PFunctionInfo; overload;

  public const
    INVALID_PRIORITY: Integer = MaxInt;
    BEST_PRIORITY: integer = -MaxInt;
    WORST_PRIORITY: integer = MaxInt-1;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;


    // Register function bindings;
    // Identify bound function using function IDs
    procedure RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer); overload;
    // Identify bound function using pointer to binding variable
    procedure RegisterBinding(BindVariable: PPointer); overload;

    // Register function binding implementations;
    // Identify bound function using function IDs
    procedure Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer = BindingPriorityDefault; Flags: Cardinal = 0); overload;
    // Identify bound function using pointer to binding variable
    procedure Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer = BindingPriorityDefault; Flags: Cardinal = 0); overload;

    // Function rebinding support
    procedure RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority = nil); overload;
    procedure RebindAll(PriorityCallback: TFunctionPriority = nil); overload;
    function Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): boolean; overload;
    function Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): boolean; overload;

    function FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): Pointer; overload;
    function FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): Pointer; overload;
  published
    property Name: string read FName write SetName;
  end;

const
  INVALID_PRIORITY: Integer = MaxInt deprecated 'Use TFunctionRegistry.INVALID_PRIORITY';


//------------------------------------------------------------------------------
//
//      NewRegistry
//
//------------------------------------------------------------------------------
// Create a new binding registry
//------------------------------------------------------------------------------
function NewRegistry(const Name: string = ''): TFunctionRegistry;


//------------------------------------------------------------------------------

function DefaultPriorityProc(Info: PFunctionInfo): Integer;

var
  DefaultPriority: TFunctionPriority = DefaultPriorityProc;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32_System;

//------------------------------------------------------------------------------

function NewRegistry(const Name: string): TFunctionRegistry;
begin
  Result := TFunctionRegistry.NewRegistry(Name);
end;

//------------------------------------------------------------------------------

function DefaultPriorityProc(Info: PFunctionInfo): Integer;
begin
  if (Info.InstructionSupport <= GR32_System.CPU.InstructionSupport) then
    Result := Info.Priority
  else
    Result := TFunctionRegistry.INVALID_PRIORITY;
end;


//------------------------------------------------------------------------------
//
//      TFunctionRegistry
//
//------------------------------------------------------------------------------
constructor TFunctionRegistry.Create;
begin
  FItems := TFunctionInfoList.Create;
  FBindings := TFunctionBindingList.Create;
end;

destructor TFunctionRegistry.Destroy;
begin
  Clear;
  FItems.Free;
  FBindings.Free;
  inherited;
end;

class destructor TFunctionRegistry.Destroy;
begin
  FBindingRegistries.Free;
  FBindingRegistries := nil;
end;

class function TFunctionRegistry.NewRegistry(const Name: string): TFunctionRegistry;
begin
  if (FBindingRegistries = nil) then
    FBindingRegistries := TObjectList<TFunctionRegistry>.Create;

  Result := TFunctionRegistry.Create;
  FBindingRegistries.Add(Result);

  Result.Name := Name;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer; Flags: Cardinal);
var
  FunctionID: NativeInt;
begin
  FunctionID := FindBinding(BindVariable);
  Assert(FunctionID <> -1, 'Binding not registered');
  Add(FunctionID, Proc, InstructionSupport, Priority, Flags);
end;

procedure TFunctionRegistry.Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer; Flags: Cardinal);
var
  Info: TFunctionInfo;
begin
  Info := Default(TFunctionInfo);
  Info.FunctionID := FunctionID;
  Info.Proc := Proc;
  Info.InstructionSupport := InstructionSupport;
  Info.Flags := Flags;
  Info.Priority := Priority;

  FItems.Add(Info);

  FNeedRebind := True;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.Clear;
begin
  FItems.Clear;
  FBindings.Clear;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindBinding(BindVariable: PPointer): NativeInt;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FBindings.Count - 1 do
    if (FBindings[i].BindVariable = BindVariable) then
    begin
      Result := FBindings[i].FunctionID;
      break;
    end;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindFunctionInfo(BindVariable: PPointer; PriorityCallback: TFunctionPriority): PFunctionInfo;
var
  FunctionID: NativeInt;
begin
  FunctionID := FindBinding(BindVariable);
  Assert(FunctionID <> -1, 'Binding not registered');
  Result := FindFunctionInfo(FunctionID, PriorityCallback);
end;

function TFunctionRegistry.FindFunctionInfo(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): PFunctionInfo;
var
  i, MinPriority, Priority: Integer;
  Info: PFunctionInfo;
begin
  if not Assigned(PriorityCallback) then
    PriorityCallback := DefaultPriority;

  Result := nil;

  MinPriority := INVALID_PRIORITY;

  for i := FItems.Count - 1 downto 0 do
  begin
{$IFNDEF FPC}
    Info := @FItems.List[i];
{$ELSE}
    Info := @(TFunctionInfoListCracker(FItems).FItems[i]);
{$ENDIF}

    if (Info.FunctionID = FunctionID) then
    begin
      Priority := PriorityCallback(Info);

      // For functions with equal priority we use the one that has the highest
      // instruction support (e.g. ASM trumps Pascal, SSE trumps MMX, AVX
      // trumps SSE, etc).
      if (Priority < MinPriority) or ((Result <> nil) and (Priority = MinPriority) and (UInt64(Info.InstructionSupport) > UInt64(Result.InstructionSupport))) then
      begin
        Result := Info;
        MinPriority := Priority;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority): Pointer;
var
  Info: PFunctionInfo;
begin
  Info := FindFunctionInfo(BindVariable, PriorityCallback);
  if (Info <> nil) then
    Result := Info.Proc
  else
    Result := nil;
end;

function TFunctionRegistry.FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): Pointer;
var
  Info: PFunctionInfo;
begin
  Info := FindFunctionInfo(FunctionID, PriorityCallback);
  if (Info <> nil) then
    Result := Info.Proc
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority): boolean;
var
  FunctionID: NativeInt;
begin
  FunctionID := FindBinding(BindVariable);
  Assert(FunctionID <> -1, 'Binding not registered');
  Result := Rebind(FunctionID, PriorityCallback);
end;

function TFunctionRegistry.Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): boolean;
var
  P: PFunctionBinding;
  i: Integer;
begin
  Result := False;
  for i := 0 to FBindings.Count - 1 do
  begin
{$IFNDEF FPC}
    P := @FBindings.List[i];
{$ELSE}
    P := @(TFunctionBindingListCracker(FBindings).FItems[i]);
{$ENDIF}

    if (P.FunctionID = FunctionID) then
    begin
      P.BindVariable^ := FindFunction(FunctionID, PriorityCallback);
      Result := (P.BindVariable^ <> nil);
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority);
begin
  if AForce then
    FNeedRebind := True;
  RebindAll(PriorityCallback);
end;

procedure TFunctionRegistry.RebindAll(PriorityCallback: TFunctionPriority);
var
  i: Integer;
  P: PFunctionBinding;
begin
  if (not Assigned(PriorityCallback)) and (not FNeedRebind) then
    exit;

  for i := 0 to FBindings.Count - 1 do
  begin
{$IFNDEF FPC}
    P := @FBindings.List[i];
{$ELSE}
    P := @(TFunctionBindingListCracker(FBindings).FItems[i]);
{$ENDIF}
    P.BindVariable^ := FindFunction(P.FunctionID, PriorityCallback);
  end;

  FNeedRebind := False;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.RegisterBinding(BindVariable: PPointer);
begin
  RegisterBinding(NativeInt(BindVariable), BindVariable);
end;

procedure TFunctionRegistry.RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer);
var
  Binding: TFunctionBinding;
begin
  Binding := Default(TFunctionBinding);
  Binding.FunctionID := FunctionID;
  Binding.BindVariable := BindVariable;

  FBindings.Add(Binding);

  FNeedRebind := True;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.SetName(const Value: string);
begin
  FName := Value;
end;

//------------------------------------------------------------------------------

end.
