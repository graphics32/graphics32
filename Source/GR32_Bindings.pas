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
{$if not defined(FRAMEWORK_LCL)}
  System.Generics.Collections,
  System.Classes,
{$else}
  Generics.Collections,
  Classes,
{$ifend}
  GR32.CPUID;

//------------------------------------------------------------------------------
//
//      CPU feature convenience aliases
//
//------------------------------------------------------------------------------
// For use in CPU dispatch bindings
// For the most common usage, these aliases avoids the need to reference the
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


const
  BindingPriorityDefault = 0;   // Default priority
  BindingPriorityBetter = -1;   // Negative = Better
  BindingPriorityWorse = 1;     // Positive = Worse


//------------------------------------------------------------------------------
//
//      IFunctionInfo
//
//------------------------------------------------------------------------------
// Interface that describes a function implementation.
//------------------------------------------------------------------------------
type
  IBindingInfo = interface;

  IFunctionInfo = interface
    function GetBinding: IBindingInfo;
    function GetEnabled: boolean;
    procedure SetEnabled(Value: boolean);
    function GetProc: Pointer;
    function GetInstructionSupport: TInstructionSupport;
    function GetPriority: Integer;
    procedure SetPriority(Value: Integer);
    function GetFlags: Cardinal;
    procedure DoSetFlags(const Value: Cardinal);
    function GetName: string;
    procedure DoSetName(const Value: string);

    function SetFlags(const Value: Cardinal): IFunctionInfo; experimental; // Fluid API; Do not use
    function SetName(const Value: string): IFunctionInfo; experimental; // Fluid API; Do not use


    // Binding: The binding this function implements
    property Binding: IBindingInfo read GetBinding;

    // Enabled: Used to temporaily enable or disable an implementation. Default True.
    property Enabled: boolean read GetEnabled write SetEnabled;

    // Proc: Pointer to the implementing function
    property Proc: Pointer read GetProc;

    // InstructionSupport: The CPU features required by this implementation
    property InstructionSupport: TInstructionSupport read GetInstructionSupport;

    // Priority: Function priority; Smaller is better. Used by default TFunctionPriority callback
    property Priority: Integer read GetPriority write SetPriority;

    // Flags: Optional, user defined flags for use in a custom TFunctionPriority callback
    property Flags: Cardinal read GetFlags write DoSetFlags;

    // Name: Optional, implementation name
    property Name: string read GetName write DoSetName;
  end;


//------------------------------------------------------------------------------
//
//      TFunctionPriority
//
//------------------------------------------------------------------------------
// Delegate used when evaluating a binding resolution.
//------------------------------------------------------------------------------
  TFunctionPriority = function(const Info: IFunctionInfo): Integer;


//------------------------------------------------------------------------------
//
//      IBindingInfo
//
//------------------------------------------------------------------------------
// Interface that provides access to the function binding meta data.
//
// A binding represents a single function. This function can have one or more
// different implementations. The function registry Rebind function selects
// among these implementation and chooses the one best suitable for the current
// host system.
//------------------------------------------------------------------------------
  IBindingInfo = interface
    function GetFunctionID: NativeInt;
    function GetBindVariable: PPointer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetNeedRebind: boolean;
    procedure SetNeedRebind(Value: boolean);

    // FunctionID: Either an ID or a pointer which uniquely identifies the binding
    property FunctionID: NativeInt read GetFunctionID;

    // BindVariable: Pointer to the function delegate
    property BindVariable: PPointer read GetBindVariable;

    // NeedRebind: Indicates if the binding's implementation has been modified
    // so the binding needs to be rebound.
    property NeedRebind: boolean read GetNeedRebind write SetNeedRebind;

    // Name: Optional, binding name
    property Name: string read GetName write SetName;

    // Register function binding implementations;
    function Add(AProc: Pointer; AInstructionSupport: TInstructionSupport; APriority: Integer = BindingPriorityDefault): IFunctionInfo;

    function FindImplementation(const Name: string): IFunctionInfo; overload;
    function FindImplementation(Proc: pointer): IFunctionInfo; overload;

    // List of functions implementing this binding.
    function GetEnumerator: TEnumerator<IFunctionInfo>;

    function FindFunction(PriorityCallback: TFunctionPriority = nil): Pointer;
    function Rebind(PriorityCallback: TFunctionPriority = nil; AForce: boolean = False): boolean;
  end;


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
type
  TFunctionRegistry = class(TPersistent)
  private type
    TBindingInfoList = TDictionary<NativeInt, IBindingInfo>;

  private class var
    FBindingRegistries: TObjectList<TFunctionRegistry>;

  private
    FBindings: TBindingInfoList;
    FName: string;

    class function NewRegistry(const Name: string): TFunctionRegistry;
    class destructor Destroy;

  protected
    function BindVariableToFunctionID(BindVariable: PPointer): NativeInt;
    function GetBinding(const Name: string): IBindingInfo; overload;
    function GetBinding(BindVariable: PPointer): IBindingInfo; overload;
    function GetBinding(FunctionID: NativeInt): IBindingInfo; overload;
    function GetNeedRebind: boolean;

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
    function RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer; const Name: string = ''): IBindingInfo; overload;
    // Identify bound function using pointer to binding variable
    function RegisterBinding(BindVariable: PPointer; const Name: string = ''): IBindingInfo; overload;

    // Register function binding implementations;
    // Identify bound function using function IDs
    function Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer = BindingPriorityDefault; Flags: Cardinal = 0): IFunctionInfo; overload; deprecated 'Use Bindings[].Add';
    // Identify bound function using pointer to binding variable
    function Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer = BindingPriorityDefault; Flags: Cardinal = 0): IFunctionInfo; overload; deprecated 'Use Bindings[].Add';

    // Function rebinding support
    procedure RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority = nil); overload; deprecated 'Use RebindAll(PriorityCallback, AForce)';
    procedure RebindAll(PriorityCallback: TFunctionPriority = nil; AForce: boolean = False); overload;
    function Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): boolean; overload; deprecated 'Use Bindings[].Rebind';
    function Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): boolean; overload; deprecated 'Use Bindings[].Rebind';

    function FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): Pointer; overload; deprecated 'Use Bindings[].FindFunction';
    function FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): Pointer; overload; deprecated 'Use Bindings[].FindFunction';

    function FindImplementation(const Name: string): IFunctionInfo; overload; deprecated 'Use Bindings[].FindImplementation';
    function FindImplementation(Proc: pointer): IFunctionInfo; overload; deprecated 'Use Bindings[].FindImplementation';

    function FindBinding(const Name: string): IBindingInfo; overload;
    function FindBinding(BindVariable: PPointer): IBindingInfo; overload;
    function FindBinding(FunctionID: NativeInt): IBindingInfo; overload;

    property Bindings[BindVariable: PPointer]: IBindingInfo read GetBinding; default;
{$if (not defined(FPC)) and (not defined(BCB))}
    property Bindings[FunctionID: NativeInt]: IBindingInfo read GetBinding; default;
    property Bindings[const Name: string]: IBindingInfo read GetBinding; default;
{$else} // Lazarus 2.6/FPC 3.0 broke support for overloaded properties. See FPC #15384
    property BindingsByName[const Name: string]: IBindingInfo read GetBinding;
    property BindingsByID[FunctionID: NativeInt]: IBindingInfo read GetBinding;
{$ifend}


    // List of bindings in this registry.
    function GetEnumerator: TEnumerator<IBindingInfo>;

    property Name: string read FName write FName;
    // NeedRebind: True if any binding has been modified and needs rebinding
    property NeedRebind: boolean read GetNeedRebind;
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

function DefaultPriorityProc(const Info: IFunctionInfo): Integer;

var
  DefaultPriority: TFunctionPriority = DefaultPriorityProc;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$if not defined(FRAMEWORK_LCL)}
  System.Math,
  System.SysUtils,
{$else}
  Math,
  SysUtils,
{$ifend}
  GR32_System;

//------------------------------------------------------------------------------

function NewRegistry(const Name: string): TFunctionRegistry;
begin
  Result := TFunctionRegistry.NewRegistry(Name);
end;

//------------------------------------------------------------------------------

function DefaultPriorityProc(const Info: IFunctionInfo): Integer;
begin
  if (Info.InstructionSupport <= GR32_System.CPU.InstructionSupport) then
    Result := Info.Priority
  else
    Result := TFunctionRegistry.INVALID_PRIORITY;
end;


//------------------------------------------------------------------------------
//
//      IFunctionInfo
//
//------------------------------------------------------------------------------
type
  TFunctionInfo = class(TInterfacedObject, IFunctionInfo)
  private
    FBinding: pointer;// weak reference to a IBindingInfo
    FEnabled: boolean;
    FProc: Pointer;
    FInstructionSupport: TInstructionSupport;
    FPriority: Integer;
    FFlags: Cardinal;
    FName: string;
  private
    // IFunctionInfo
    function GetBinding: IBindingInfo;
    function GetEnabled: boolean;
    procedure SetEnabled(Value: boolean);
    function GetProc: Pointer;
    function GetInstructionSupport: TInstructionSupport;
    function GetPriority: Integer;
    procedure SetPriority(Value: Integer);
    function GetFlags: Cardinal;
    procedure DoSetFlags(const Value: Cardinal);
    function GetName: string;
    procedure DoSetName(const Value: string);
    function SetFlags(const Value: Cardinal): IFunctionInfo;
    function SetName(const Value: string): IFunctionInfo;
  private
    property Binding: IBindingInfo read GetBinding;
  public
    constructor Create(const ABinding: IBindingInfo; AProc: Pointer; AInstructionSupport: TInstructionSupport; APriority: Integer);
  end;

//------------------------------------------------------------------------------

constructor TFunctionInfo.Create(const ABinding: IBindingInfo; AProc: Pointer; AInstructionSupport: TInstructionSupport; APriority: Integer);
begin
  inherited Create;
  FBinding := pointer(ABinding);
  FEnabled := True;
  FProc := AProc;
  FInstructionSupport := AInstructionSupport;
  FPriority := APriority;
end;

//------------------------------------------------------------------------------

function TFunctionInfo.GetBinding: IBindingInfo;
begin
  Result := IBindingInfo(FBinding);
end;

function TFunctionInfo.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

function TFunctionInfo.GetFlags: Cardinal;
begin
  Result := FFlags;
end;

function TFunctionInfo.GetInstructionSupport: TInstructionSupport;
begin
  Result := FInstructionSupport;
end;

function TFunctionInfo.GetName: string;
begin
  Result := FName;
  if (Result = '') then
{$if defined(TARGET_x86)} // Issue 362: Older versions of Delphi (XE4 at least) lacks the IntToHex(Value) overload
    Result := '@'+IntToHex(UInt32(Self), 8);
{$elseif defined(TARGET_x64)}
    Result := '@'+IntToHex(UInt64(Self), 16);
{$else}
    Result := '@'+IntToHex(NativeUInt(Self));
{$ifend}
end;

function TFunctionInfo.GetPriority: Integer;
begin
  Result := FPriority;
end;

function TFunctionInfo.GetProc: Pointer;
begin
  Result := FProc;
end;

procedure TFunctionInfo.DoSetFlags(const Value: Cardinal);
begin
  FFlags := Value;
  Binding.NeedRebind := True;
end;

procedure TFunctionInfo.DoSetName(const Value: string);
begin
  FName := Value;
end;

procedure TFunctionInfo.SetEnabled(Value: boolean);
begin
  FEnabled := True;
  Binding.NeedRebind := True;
end;

function TFunctionInfo.SetFlags(const Value: Cardinal): IFunctionInfo;
begin
  FFlags := Value;
  Binding.NeedRebind := True;
  Result := Self;
end;

function TFunctionInfo.SetName(const Value: string): IFunctionInfo;
begin
  FName := Value;
  Binding.NeedRebind := True;
  Result := Self;
end;


procedure TFunctionInfo.SetPriority(Value: Integer);
begin
  FPriority := Value;
  Binding.NeedRebind := True;
end;

//------------------------------------------------------------------------------
//
//      IBindingInfo
//
//------------------------------------------------------------------------------
type
  TBindingInfo = class(TInterfacedObject, IBindingInfo)
  private type
    TFunctionInfoList = TList<IFunctionInfo>;

  private
    FNeedRebind: boolean;
    FFunctionID: NativeInt;      // Either an ID or a pointer
    FBindVariable: PPointer;     // Pointer to the function delegate
    FName: string;
    FFunctions: TFunctionInfoList;

  private
    function FindBestFunctionInfo(PriorityCallback: TFunctionPriority = nil): IFunctionInfo;

  private
    // IBindingInfo
    function GetFunctionID: NativeInt;
    function GetBindVariable: PPointer;
    function GetNeedRebind: boolean;
    procedure SetNeedRebind(Value: boolean);
    function GetName: string;
    procedure SetName(const Value: string);
    function Add(AProc: Pointer; AInstructionSupport: TInstructionSupport; APriority: Integer): IFunctionInfo;
    function FindImplementation(const Name: string): IFunctionInfo; overload;
    function FindImplementation(Proc: pointer): IFunctionInfo; overload;
    function GetEnumerator: TEnumerator<IFunctionInfo>;
    function FindFunction(PriorityCallback: TFunctionPriority = nil): Pointer;
    function Rebind(PriorityCallback: TFunctionPriority = nil; AForce: boolean = False): boolean;

  public
    constructor Create(AFunctionID: NativeInt; ABindVariable: PPointer);
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------

constructor TBindingInfo.Create(AFunctionID: NativeInt; ABindVariable: PPointer);
begin
  inherited Create;
  FFunctions := TFunctionInfoList.Create;
  FFunctionID := AFunctionID;
  FBindVariable := ABindVariable;
  FNeedRebind := True;
end;

destructor TBindingInfo.Destroy;
begin
  FFunctions.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TBindingInfo.FindBestFunctionInfo(PriorityCallback: TFunctionPriority = nil): IFunctionInfo;
var
  MinPriority, Priority: Integer;
  FunctionInfo: IFunctionInfo;
begin
  if not Assigned(PriorityCallback) then
    PriorityCallback := DefaultPriority;

  Result := nil;

  MinPriority := TFunctionRegistry.INVALID_PRIORITY;

  for FunctionInfo in FFunctions do
  begin
    if (not FunctionInfo.Enabled) then
      continue;

    Priority := PriorityCallback(FunctionInfo);

    // For functions with equal priority we use the one that has the highest
    // instruction support (e.g. ASM trumps Pascal, SSE trumps MMX, AVX
    // trumps SSE, etc).
    if (Priority < MinPriority) or ((Result <> nil) and (Priority = MinPriority) and (UInt64(FunctionInfo.InstructionSupport) > UInt64(Result.InstructionSupport))) then
    begin
      Result := FunctionInfo;
      MinPriority := Priority;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TBindingInfo.FindFunction(PriorityCallback: TFunctionPriority): Pointer;
var
  Info: IFunctionInfo;
begin
  Info := FindBestFunctionInfo(PriorityCallback);
  if (Info <> nil) then
    Result := Info.Proc
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TBindingInfo.Rebind(PriorityCallback: TFunctionPriority; AForce: boolean): boolean;
begin
  if (not AForce) and (not FNeedRebind) then
    Exit(False);

  FBindVariable^ := FindFunction(PriorityCallback);
  Result := (FBindVariable^ <> nil);

  FNeedRebind := True;
end;

//------------------------------------------------------------------------------

function TBindingInfo.FindImplementation(Proc: pointer): IFunctionInfo;
var
  FunctionInfo: IFunctionInfo;
begin
  for FunctionInfo in FFunctions do
    if (FunctionInfo.Proc = Proc) then
      Exit(FunctionInfo);
  Result := nil;
end;

function TBindingInfo.FindImplementation(const Name: string): IFunctionInfo;
var
  FunctionInfo: IFunctionInfo;
begin
  for FunctionInfo in FFunctions do
    if (FunctionInfo.Name = Name) then
      Exit(FunctionInfo);
  Result := nil;
end;

//------------------------------------------------------------------------------

function TBindingInfo.Add(AProc: Pointer; AInstructionSupport: TInstructionSupport; APriority: Integer): IFunctionInfo;
begin
  Result := TFunctionInfo.Create(Self, AProc, AInstructionSupport, APriority);

  // We need to get the last first when enumerating, so the list must be in reverse insertion order
  FFunctions.Insert(0, Result);

  FNeedRebind := True;
end;

//------------------------------------------------------------------------------

function TBindingInfo.GetFunctionID: NativeInt;
begin
  Result := FFunctionID;
end;

function TBindingInfo.GetBindVariable: PPointer;
begin
  Result := FBindVariable;
end;

function TBindingInfo.GetName: string;
begin
  Result := FName;
  if (Result = '') then
{$if defined(TARGET_x86)} // Issue 362: Older versions of Delphi (XE4 at least) lacks the IntToHex(Value) overload
    Result := '@'+IntToHex(UInt32(Self), 8);
{$elseif defined(TARGET_x64)}
    Result := '@'+IntToHex(UInt64(Self), 16);
{$else}
    Result := '@'+IntToHex(NativeUInt(Self));
{$ifend}
end;

function TBindingInfo.GetNeedRebind: boolean;
begin
  Result := FNeedRebind;
end;

procedure TBindingInfo.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TBindingInfo.SetNeedRebind(Value: boolean);
begin
  FNeedRebind := Value;
end;

//------------------------------------------------------------------------------

function TBindingInfo.GetEnumerator: TEnumerator<IFunctionInfo>;
begin
  Result := FFunctions.GetEnumerator;
end;


//------------------------------------------------------------------------------
//
//      TFunctionRegistry
//
//------------------------------------------------------------------------------
constructor TFunctionRegistry.Create;
begin
  FBindings := TBindingInfoList.Create;
end;

destructor TFunctionRegistry.Destroy;
begin
  Clear;
  FBindings.Free;
  inherited;
end;

//------------------------------------------------------------------------------

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

function TFunctionRegistry.Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer; Flags: Cardinal): IFunctionInfo;
var
  BindingInfo: IBindingInfo;
  FunctionID: NativeInt;
begin
  BindingInfo := FindBinding(BindVariable);

  if (BindingInfo = nil) then
  begin
    FunctionID := BindVariableToFunctionID(BindVariable);
    if (FunctionID <> -1) then
      BindingInfo := FindBinding(FunctionID);
  end;

{$if defined(BINDING_AUTO_REGISTER)}

  // Auto-register the binding if it isn't already registered
  if (BindingInfo = nil) then
    BindingInfo := RegisterBinding(BindVariable);

{$else}

  if (BindingInfo = nil) then
    raise Exception.CreateFmt('Binding %p not registered', [BindVariable]);

{$ifend}

  Result := BindingInfo.Add(Proc, InstructionSupport, Priority);
  Result.Flags := Flags;
end;

function TFunctionRegistry.Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer; Flags: Cardinal): IFunctionInfo;
var
  BindingInfo: IBindingInfo;
begin
  BindingInfo := GetBinding(FunctionID);

  Result := BindingInfo.Add(Proc, InstructionSupport, Priority);
  Result.Flags := Flags;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.Clear;
begin
  FBindings.Clear;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.BindVariableToFunctionID(BindVariable: PPointer): NativeInt;
var
  BindingInfo: IBindingInfo;
begin
  Result := -1;
  for BindingInfo in Self do
    if (BindingInfo.BindVariable = BindVariable) then
    begin
      Result := BindingInfo.FunctionID;
      break;
    end;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority): Pointer;
begin
  Result := Bindings[BindVariable].FindFunction(PriorityCallback);
end;

function TFunctionRegistry.FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): Pointer;
begin
  Result := GetBinding(FunctionID).FindFunction(PriorityCallback);
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.GetBinding(const Name: string): IBindingInfo;
begin
  Result := FindBinding(Name);
  if (Result = nil) then
    raise Exception.CreateFmt('Binding "%s" not registered', [Name]);
end;

function TFunctionRegistry.GetBinding(BindVariable: PPointer): IBindingInfo;
begin
  Result := FindBinding(BindVariable);
  if (Result = nil) then
    raise Exception.CreateFmt('Binding "%p" not registered', [BindVariable]);
end;

function TFunctionRegistry.GetBinding(FunctionID: NativeInt): IBindingInfo;
begin
  Result := FindBinding(FunctionID);
  if (Result = nil) then
    raise Exception.CreateFmt('Binding "%d" not registered', [FunctionID]);
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindBinding(const Name: string): IBindingInfo;
var
  BindingInfo: IBindingInfo;
begin
  for BindingInfo in Self do
    if (BindingInfo.Name = Name) then
      Exit(BindingInfo);
  Result := nil;
end;

function TFunctionRegistry.FindBinding(BindVariable: PPointer): IBindingInfo;
var
  BindingInfo: IBindingInfo;
begin
  for BindingInfo in Self do
    if (BindingInfo.BindVariable = BindVariable) then
      Exit(BindingInfo);
  Result := nil;
end;

function TFunctionRegistry.FindBinding(FunctionID: NativeInt): IBindingInfo;
begin
  if (not FBindings.TryGetValue(FunctionID, Result)) then
    Result := nil;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindImplementation(const Name: string): IFunctionInfo;
var
  BindingInfo: IBindingInfo;
begin
  for BindingInfo in Self do
  begin
    Result := BindingInfo.FindImplementation(Name);
    if (Result <> nil) then
      exit;
  end;
  Result := nil;
end;

function TFunctionRegistry.FindImplementation(Proc: pointer): IFunctionInfo;
var
  BindingInfo: IBindingInfo;
begin
  for BindingInfo in Self do
  begin
    Result := BindingInfo.FindImplementation(Proc);
    if (Result <> nil) then
      exit;
  end;
  Result := nil;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.GetEnumerator: TEnumerator<IBindingInfo>;
begin
  Result := FBindings.Values.GetEnumerator;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.GetNeedRebind: boolean;
var
  BindingInfo: IBindingInfo;
begin
  for BindingInfo in Self do
    if (BindingInfo.NeedRebind) then
      Exit(True);
  Result := False;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority): boolean;
begin
  Result := Bindings[BindVariable].Rebind(PriorityCallback);
end;

function TFunctionRegistry.Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority): boolean;
begin
  Result := GetBinding(FunctionID).Rebind(PriorityCallback);
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority);
begin
  RebindAll(PriorityCallback, AForce);
end;

procedure TFunctionRegistry.RebindAll(PriorityCallback: TFunctionPriority; AForce: boolean);
var
  BindingInfo: IBindingInfo;
begin
  for BindingInfo in Self do
    BindingInfo.Rebind(PriorityCallback, AForce);
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.RegisterBinding(BindVariable: PPointer; const Name: string): IBindingInfo;
begin
  Result := RegisterBinding(NativeInt(BindVariable), BindVariable, Name);
end;

function TFunctionRegistry.RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer; const Name: string): IBindingInfo;
begin
  Result := TBindingInfo.Create(FunctionID, BindVariable);
  FBindings.Add(FunctionID, Result);
  Result.Name := Name;
end;

//------------------------------------------------------------------------------

end.
