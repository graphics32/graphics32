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

{$I GR32.inc}

uses
  Classes,
{$IFNDEF PUREPASCAL}
  GR32.CPUID,
{$ENDIF}
  GR32_System;

type
  TFunctionName = type string;
  TFunctionID = type Integer;

  PFunctionInfo = ^TFunctionInfo;
  TFunctionInfo = record
    FunctionID: NativeInt; // Either an ID or a pointer
    Proc: Pointer;
    InstructionSupport: TInstructionSupport;
    Flags: Integer;
    Priority: Integer; // Smaller is better
  end;

  TFunctionPriority = function (Info: PFunctionInfo): Integer;

  PFunctionBinding = ^TFunctionBinding;
  TFunctionBinding = record
    FunctionID: NativeInt;
    BindVariable: PPointer;
  end;

  { TFunctionRegistry }
  { This class fascilitates a registry that allows multiple function to be
    registered together with information about their CPU requirements and
    an additional 'flags' parameter. Functions that share the same FunctionID
    can be assigned to a function variable through the rebind methods.
    A priority callback function is used to assess the most optimal function. }
  TFunctionRegistry = class(TPersistent)
  private
    FItems: TList;
    FBindings: TList;
    FName: string;
    FNeedRebind: boolean;
    procedure SetName(const Value: string);
    function GetItems(Index: Integer): PFunctionInfo;
    procedure SetItems(Index: Integer; const Value: PFunctionInfo);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;

    // FunctionID: Identify bound function using function IDs
    // FunctionProc: Identify bound function using function pointer

    procedure Add(FunctionID: NativeInt; Proc: Pointer; CPUFeatures: TCPUFeatures; Flags: Integer = 0; Priority: Integer = 0); overload; deprecated;
    procedure Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport = []; Flags: Integer = 0; Priority: Integer = 0); overload;
    procedure Add(FunctionID: NativeInt; Proc: Pointer; Flags: Integer; Priority: Integer = 0); overload;
    procedure Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport = []; Flags: Integer = 0; Priority: Integer = 0); overload;
    procedure Add(BindVariable: PPointer; Proc: Pointer; Flags: Integer; Priority: Integer = 0); overload;

    // Function rebinding support
    procedure RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer); overload;
    procedure RegisterBinding(BindVariable: PPointer); overload;
    procedure RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority = nil); overload;
    procedure RebindAll(PriorityCallback: TFunctionPriority = nil); overload;
    function Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): boolean; overload;
    function Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): boolean; overload;

    function FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): Pointer; overload;
    function FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): Pointer; overload;

    // TODO : Items is useless as we have no way to determine the size of the list
    property Items[Index: Integer]: PFunctionInfo read GetItems write SetItems;
  published
    property Name: string read FName write SetName;
  end;

function NewRegistry(const Name: string = ''): TFunctionRegistry;

function DefaultPriorityProc(Info: PFunctionInfo): Integer;

var
  DefaultPriority: TFunctionPriority = DefaultPriorityProc;

const
  INVALID_PRIORITY: Integer = MaxInt;

implementation

uses
  Math;

var
  Registers: TList;

function NewRegistry(const Name: string): TFunctionRegistry;
begin
  if Registers = nil then
    Registers := TList.Create;
  Result := TFunctionRegistry.Create;
  {$IFDEF NEXTGEN}
  Result.__ObjAddRef;
  {$ENDIF}
  Result.Name := Name;
  Registers.Add(Result);
end;

function DefaultPriorityProc(Info: PFunctionInfo): Integer;
begin
  if (Info^.InstructionSupport <= GR32_System.CPU.InstructionSupport) then
    Result := Info^.Priority
  else
    Result := INVALID_PRIORITY;
end;

{ TFunctionRegistry }

procedure TFunctionRegistry.Add(BindVariable: PPointer; Proc: Pointer; Flags, Priority: Integer);
begin
  Add(NativeInt(BindVariable), Proc, Flags, Priority);
end;

procedure TFunctionRegistry.Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Flags, Priority: Integer);
begin
  Add(NativeInt(BindVariable), Proc, CPU.InstructionSupport, Flags, Priority);
end;

procedure TFunctionRegistry.Add(FunctionID: NativeInt; Proc: Pointer; CPUFeatures: TCPUFeatures; Flags: Integer; Priority: Integer);
begin
  Add(FunctionID, Proc, CPU.InstructionSupport, Flags, Priority);
end;

procedure TFunctionRegistry.Add(FunctionID: NativeInt; Proc: Pointer; Flags: Integer; Priority: Integer = 0);
const
  Nothing: TInstructionSupport = [];
begin
  Add(FunctionID, Proc, Nothing, Flags, Priority);
end;

procedure TFunctionRegistry.Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Flags: Integer; Priority: Integer);
var
  Info: PFunctionInfo;
begin
  New(Info);
  Info^.FunctionID := FunctionID;
  Info^.Proc := Proc;
  Info^.InstructionSupport := InstructionSupport;
  Info^.Flags := Flags;
  Info^.Priority := Priority;
  FItems.Add(Info);

  FNeedRebind := True;
end;

procedure TFunctionRegistry.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Dispose(PFunctionInfo(FItems[I]));
  FItems.Clear;
  for I := 0 to FBindings.Count - 1 do
    Dispose(PFunctionBinding(FBindings[I]));
  FBindings.Clear;
end;

constructor TFunctionRegistry.Create;
begin
  FItems := TList.Create;
  FBindings := TList.Create;
end;

destructor TFunctionRegistry.Destroy;
begin
  Clear;
  FItems.Free;
  FBindings.Free;
  inherited;
end;

function TFunctionRegistry.FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority): Pointer;
begin
  Result := FindFunction(NativeInt(BindVariable), PriorityCallback);
end;

function TFunctionRegistry.FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): Pointer;
var
  I, MinPriority, P: Integer;
  Info: PFunctionInfo;
begin
  if not Assigned(PriorityCallback) then
    PriorityCallback := DefaultPriority;
  Result := nil;
  MinPriority := INVALID_PRIORITY;
  for I := FItems.Count - 1 downto 0 do
  begin
    Info := FItems[I];

    if (Info^.FunctionID = FunctionID) then
    begin
      P := PriorityCallback(Info);

      if (P < MinPriority) then
      begin
        Result := Info^.Proc;
        MinPriority := P;
      end;
    end;
  end;
end;

function TFunctionRegistry.GetItems(Index: Integer): PFunctionInfo;
begin
  Result := FItems[Index];
end;

function TFunctionRegistry.Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority): boolean;
begin
  Result := Rebind(NativeInt(BindVariable), PriorityCallback);
end;

function TFunctionRegistry.Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): boolean;
var
  P: PFunctionBinding;
  I: Integer;
begin
  Result := False;
  for I := 0 to FBindings.Count - 1 do
  begin
    P := PFunctionBinding(FBindings[I]);

    if (P^.FunctionID = FunctionID) then
    begin
      P^.BindVariable^ := FindFunction(FunctionID, PriorityCallback);
      Result := (P^.BindVariable^ <> nil);
      break;
    end;
  end;
end;

procedure TFunctionRegistry.RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority);
begin
  if AForce then
    FNeedRebind := True;
  RebindAll(PriorityCallback);
end;

procedure TFunctionRegistry.RebindAll(PriorityCallback: TFunctionPriority);
var
  I: Integer;
  P: PFunctionBinding;
begin
  if (not Assigned(PriorityCallback)) and (not FNeedRebind) then
    exit;

  for I := 0 to FBindings.Count - 1 do
  begin
    P := PFunctionBinding(FBindings[I]);
    P^.BindVariable^ := FindFunction(P^.FunctionID, PriorityCallback);
  end;

  FNeedRebind := False;
end;

procedure TFunctionRegistry.RegisterBinding(BindVariable: PPointer);
begin
  RegisterBinding(NativeInt(BindVariable), BindVariable);
end;

procedure TFunctionRegistry.RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer);
var
  Binding: PFunctionBinding;
begin
  New(Binding);
  Binding^.FunctionID := FunctionID;
  Binding^.BindVariable := BindVariable;
  FBindings.Add(Binding);

  FNeedRebind := True;
end;

procedure TFunctionRegistry.SetItems(Index: Integer;
  const Value: PFunctionInfo);
begin
  FItems[Index] := Value;
  FNeedRebind := True;
end;

procedure TFunctionRegistry.SetName(const Value: string);
begin
  FName := Value;
end;

procedure FreeRegisters;
var
  I: Integer;
begin
  if Assigned(Registers) then
  begin
    for I := Registers.Count - 1 downto 0 do
      TFunctionRegistry(Registers[I]).Free;
    Registers.Free;
    Registers := nil;
  end;
end;

initialization

finalization
  FreeRegisters;

end.
