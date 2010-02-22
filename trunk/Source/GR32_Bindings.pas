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
 * The Original Code is MicroTiles Repaint Optimizer Extension for Graphics32
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

uses
  Classes, GR32_System;

type
  TFunctionName = type string;
  TFunctionID = type Integer;

  PFunctionInfo = ^TFunctionInfo;
  TFunctionInfo = record
    FunctionID: Integer;
    Proc: Pointer;
    CPUFeatures: TCPUFeatures;
    Flags: Integer;
  end;

  TFunctionPriority = function (Info: PFunctionInfo): Integer;

  PFunctionBinding = ^TFunctionBinding;
  TFunctionBinding = record
    FunctionID: Integer;
    BindVariable: PPointer;
  end;

  { TFunctionRegistry }
  TFunctionRegistry = class(TPersistent)
  private
    FItems: TList;
    FBindings: TList;
    FName: string;
    procedure SetName(const Value: string);
    function GetItems(Index: Integer): PFunctionInfo;
    procedure SetItems(Index: Integer; const Value: PFunctionInfo);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;

    procedure Add(FunctionID: Integer; Proc: Pointer; CPUFeatures: TCPUFeatures = []; Flags: Integer = 0);

    // function rebinding support
    procedure RegisterBinding(FunctionID: Integer; BindVariable: PPointer);
    procedure RebindAll(PriorityCallback: TFunctionPriority = nil);
    procedure Rebind(FunctionID: Integer; PriorityCallback: TFunctionPriority = nil);

    function FindFunction(FunctionID: Integer; PriorityCallback: TFunctionPriority = nil): Pointer;
    property Items[Index: Integer]: PFunctionInfo read GetItems write SetItems;
  published
    property Name: string read FName write SetName;
  end;

function NewRegistry(const Name: string = ''): TFunctionRegistry;
// function FindRegistry(const Name: string): TFunctionRegistry;

function DefaultPriorityProc(Info: PFunctionInfo): Integer;

var
  DefaultPriority: TFunctionPriority = DefaultPriorityProc;

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
  Result.Name := Name;
  Registers.Add(Result);
end;

function DefaultPriorityProc(Info: PFunctionInfo): Integer;
begin
  Result := IfThen(Info.CPUFeatures <= GR32_System.CPUFeatures, 0, -1);
end;

{ TFunctionRegistry }

procedure TFunctionRegistry.Add(FunctionID: Integer; Proc: Pointer;
  CPUFeatures: TCPUFeatures; Flags: Integer);
var
  Info: PFunctionInfo;
begin
  New(Info);
  Info.FunctionID := FunctionID;
  Info.Proc := Proc;
  Info.CPUFeatures := CPUFeatures;
  Info.Flags := Flags;
  FItems.Add(Info);
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

function TFunctionRegistry.FindFunction(FunctionID: Integer;
  PriorityCallback: TFunctionPriority): Pointer;
var
  I, MaxPriority, P: Integer;
  Info: PFunctionInfo;
begin
  if not Assigned(PriorityCallback) then PriorityCallback := DefaultPriority;
  Result := nil;
  MaxPriority := 0; // negative values => not supported
  for I := 0 to FItems.Count - 1 do
  begin
    Info := FItems[I];
    if (Info.FunctionID = FunctionID) then
    begin
      P := PriorityCallback(Info);
      if P >= MaxPriority then
      begin
        Result := Info.Proc;
        MaxPriority := P;
      end;
    end;
  end;
end;

function TFunctionRegistry.GetItems(Index: Integer): PFunctionInfo;
begin
  Result := FItems[Index];
end;

procedure TFunctionRegistry.Rebind(FunctionID: Integer;
  PriorityCallback: TFunctionPriority);
var
  P: PFunctionBinding;
  I: Integer;
begin
  for I := 0 to FBindings.Count - 1 do
  begin
    P := PFunctionBinding(FBindings[I]);
    if P.FunctionID = FunctionID then
      P.BindVariable^ := FindFunction(FunctionID, PriorityCallback);
  end;
end;

procedure TFunctionRegistry.RebindAll(PriorityCallback: TFunctionPriority);
var
  I: Integer;
  P: PFunctionBinding;
begin
  for I := 0 to FBindings.Count - 1 do
  begin
    P := PFunctionBinding(FBindings[I]);
    P.BindVariable^ := FindFunction(P.FunctionID, PriorityCallback);
  end;
end;

procedure TFunctionRegistry.RegisterBinding(FunctionID: Integer;
  BindVariable: PPointer);
var
  Binding: PFunctionBinding;
begin
  New(Binding);
  Binding.FunctionID := FunctionID;
  Binding.BindVariable := BindVariable;
  FBindings.Add(Binding);
end;

procedure TFunctionRegistry.SetItems(Index: Integer;
  const Value: PFunctionInfo);
begin
  FItems[Index] := Value;
end;

procedure TFunctionRegistry.SetName(const Value: string);
begin
  FName := Value;
end;

procedure FreeRegisters;
var
  I: Integer;
begin
  for I := Registers.Count - 1 downto 0 do
    TFunctionRegistry(Registers[I]).Free;
  Registers.Free;
end;

initialization

finalization
  FreeRegisters;

end.
