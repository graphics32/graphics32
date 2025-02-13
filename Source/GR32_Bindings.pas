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
// For internal use only.
//------------------------------------------------------------------------------
type
  TFunctionInfo = record
    FunctionID: NativeInt;      // Either an ID or a pointer
    Proc: Pointer;              // Pointer to the implementing function
    InstructionSupport: TInstructionSupport; // The CPU features required by this implementation
    Priority: Integer;          // Function priority; Smaller is better. Used by default TFunctionPriority callback
    Flags: Cardinal;            // Optional, user defined flags for use in a custom TFunctionPriority callback
    Name: string;               // Optional, implementation name
  end;
  PFunctionInfo = ^TFunctionInfo;

//------------------------------------------------------------------------------
//
//      IFunctionInfo
//
//------------------------------------------------------------------------------
// Interface that provides access to the function implementation meta data.
// The interface is only valid while the implementation it represents is
// registered.
//------------------------------------------------------------------------------
type
  IFunctionInfo = interface
    function GetFunctionID: NativeInt;
    function GetProc: Pointer;
    function GetInstructionSupport: TInstructionSupport;
    function GetPriority: Integer;
    function GetFlags: Cardinal;
    procedure SetFlags(const Value: Cardinal);
    function GetName: string;
    procedure SetName(const Value: string);

    property FunctionID: NativeInt read GetFunctionID;
    property Proc: Pointer read GetProc;
    property InstructionSupport: TInstructionSupport read GetInstructionSupport;
    property Priority: Integer read GetPriority;
    property Flags: Cardinal read GetFlags write SetFlags;
    property Name: string read GetName write SetName;
  end;

  IFunctionInfoEnumerator = interface
    function GetCurrent: IFunctionInfo;
    function MoveNext: Boolean;
    property Current: IFunctionInfo read GetCurrent;
  end;

//------------------------------------------------------------------------------
//
//      IBindingInfo
//
//------------------------------------------------------------------------------
// Interface that provides access to the function binding meta data.
// The interface is only valid while the binding it represents is registered.
//------------------------------------------------------------------------------
type
  IBindingInfo = interface
    function GetFunctionID: NativeInt;
    function GetBindVariable: PPointer;
    function GetName: string;
    procedure SetName(const Value: string);

    property FunctionID: NativeInt read GetFunctionID;
    property BindVariable: PPointer read GetBindVariable;
    property Name: string read GetName write SetName;

    // List of functions implementing this binding.
    function GetEnumerator: IFunctionInfoEnumerator;
  end;

  IBindingEnumerator = interface
    function GetCurrent: IBindingInfo;
    function MoveNext: Boolean;
    property Current: IBindingInfo read GetCurrent;
  end;

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
    TFunctionBinding = record
      FunctionID: NativeInt;      // Either an ID or a pointer
      BindVariable: PPointer;     // Pointer to the function delegate
      Name: string;
    end;
    PFunctionBinding = ^TFunctionBinding;

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

    class function NewRegistry(const Name: string): TFunctionRegistry;
    class destructor Destroy;
  protected
    function DoFindBinding(BindVariable: PPointer): NativeInt;
    function FindFunctionInfo(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): PFunctionInfo; overload;
    function FindFunctionInfo(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): PFunctionInfo; overload;

    function GetFunctionInfo(Index: integer): PFunctionInfo;
    function GetFunctionBinding(Index: integer): PFunctionBinding;

    property FunctionInfo[Index: integer]: PFunctionInfo read GetFunctionInfo;
    property FunctionBinding[Index: integer]: PFunctionBinding read GetFunctionBinding;
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
    procedure RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer; const Name: string = ''); overload;
    // Identify bound function using pointer to binding variable
    procedure RegisterBinding(BindVariable: PPointer; const Name: string = ''); overload;

    // Register function binding implementations;
    // Identify bound function using function IDs
    function Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer = BindingPriorityDefault; Flags: Cardinal = 0): IFunctionInfo; overload;
    // Identify bound function using pointer to binding variable
    function Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer = BindingPriorityDefault; Flags: Cardinal = 0): IFunctionInfo; overload;

    // Function rebinding support
    procedure RebindAll(AForce: boolean; PriorityCallback: TFunctionPriority = nil); overload;
    procedure RebindAll(PriorityCallback: TFunctionPriority = nil); overload;
    function Rebind(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): boolean; overload;
    function Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): boolean; overload;

    function FindFunction(FunctionID: NativeInt; PriorityCallback: TFunctionPriority = nil): Pointer; overload;
    function FindFunction(BindVariable: PPointer; PriorityCallback: TFunctionPriority = nil): Pointer; overload;

    function FindBinding(const Name: string): IBindingInfo; overload;
    function FindBinding(BindVariable: PPointer): IBindingInfo; overload;
    function FindBinding(FunctionID: NativeInt): IBindingInfo; overload;

    function FindImplementation(const Name: string): IFunctionInfo; overload;
    function FindImplementation(Proc: pointer): IFunctionInfo; overload;

    // List of bindings in this registry.
    function GetEnumerator: IBindingEnumerator;

    property Name: string read FName write FName;
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
  SysUtils,
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
//      IFunctionInfo
//
//------------------------------------------------------------------------------
type
  TFunctionInfoWrapper = class(TInterfacedObject, IFunctionInfo)
  private
    FFunctionInfo: PFunctionInfo;
  private
    // IFunctionInfo
    function GetFunctionID: NativeInt;
    function GetProc: Pointer;
    function GetInstructionSupport: TInstructionSupport;
    function GetPriority: Integer;
    function GetFlags: Cardinal;
    procedure SetFlags(const Value: Cardinal);
    function GetName: string;
    procedure SetName(const Value: string);
  public
    constructor Create(AFunctionInfo: PFunctionInfo);
  end;

constructor TFunctionInfoWrapper.Create(AFunctionInfo: PFunctionInfo);
begin
  inherited Create;
  FFunctionInfo := AFunctionInfo;
end;

function TFunctionInfoWrapper.GetFlags: Cardinal;
begin
  Result := FFunctionInfo.Flags;
end;

function TFunctionInfoWrapper.GetFunctionID: NativeInt;
begin
  Result := FFunctionInfo.FunctionID;
end;

function TFunctionInfoWrapper.GetInstructionSupport: TInstructionSupport;
begin
  Result := FFunctionInfo.InstructionSupport;
end;

function TFunctionInfoWrapper.GetName: string;
begin
  Result := FFunctionInfo.Name;
  if (Result = '') then
    Result := '@'+IntToHex(NativeInt(FFunctionInfo));
end;

function TFunctionInfoWrapper.GetPriority: Integer;
begin
  Result := FFunctionInfo.Priority;
end;

function TFunctionInfoWrapper.GetProc: Pointer;
begin
  Result := FFunctionInfo.Proc;
end;

procedure TFunctionInfoWrapper.SetFlags(const Value: Cardinal);
begin
  FFunctionInfo.Flags := Value;
end;

procedure TFunctionInfoWrapper.SetName(const Value: string);
begin
  FFunctionInfo.Name := Value;
end;

//------------------------------------------------------------------------------
//
//      IBindingInfo
//
//------------------------------------------------------------------------------
type
  TBindingInfoWrapper = class(TInterfacedObject, IBindingInfo)
  private type
    TFunctionInfoEnumerator = class(TInterfacedObject, IFunctionInfoEnumerator)
    private
      FFunctionRegistry: TFunctionRegistry;
      FBindingInfo: TFunctionRegistry.PFunctionBinding;
      FIndex: integer;
      FCurrent: PFunctionInfo;
    private
      // IFunctionInfoEnumerator
      function GetCurrent: IFunctionInfo;
      function MoveNext: Boolean;
    public
      constructor Create(AFunctionRegistry: TFunctionRegistry; ABindingInfo: TFunctionRegistry.PFunctionBinding);
    end;

  private
    FFunctionRegistry: TFunctionRegistry;
    FBindingInfo: TFunctionRegistry.PFunctionBinding;
  private
    // IBindingInfo
    function GetFunctionID: NativeInt;
    function GetBindVariable: PPointer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetEnumerator: IFunctionInfoEnumerator;
  public
    constructor Create(AFunctionRegistry: TFunctionRegistry; ABindingInfo: TFunctionRegistry.PFunctionBinding);
  end;

constructor TBindingInfoWrapper.Create(AFunctionRegistry: TFunctionRegistry; ABindingInfo: TFunctionRegistry.PFunctionBinding);
begin
  inherited Create;
  FFunctionRegistry := AFunctionRegistry;
  FBindingInfo := ABindingInfo;
end;

function TBindingInfoWrapper.GetFunctionID: NativeInt;
begin
  Result := FBindingInfo.FunctionID;
end;

function TBindingInfoWrapper.GetBindVariable: PPointer;
begin
  Result := FBindingInfo.BindVariable;
end;

function TBindingInfoWrapper.GetName: string;
begin
  Result := FBindingInfo.Name;
  if (Result = '') then
    Result := '@'+IntToHex(NativeInt(FBindingInfo));
end;

procedure TBindingInfoWrapper.SetName(const Value: string);
begin
  FBindingInfo.Name := Value;
end;

function TBindingInfoWrapper.GetEnumerator: IFunctionInfoEnumerator;
begin
  Result := TFunctionInfoEnumerator.Create(FFunctionRegistry, FBindingInfo);
end;

//------------------------------------------------------------------------------
// TBindingInfoWrapper.TFunctionInfoEnumerator
//------------------------------------------------------------------------------
constructor TBindingInfoWrapper.TFunctionInfoEnumerator.Create(AFunctionRegistry: TFunctionRegistry;
  ABindingInfo: TFunctionRegistry.PFunctionBinding);
begin
  inherited Create;
  FFunctionRegistry := AFunctionRegistry;
  FBindingInfo := ABindingInfo;
  FIndex := -1;
end;

function TBindingInfoWrapper.TFunctionInfoEnumerator.GetCurrent: IFunctionInfo;
begin
  if (FCurrent <> nil) then
    Result := TFunctionInfoWrapper.Create(FCurrent)
  else
    Result := nil;
end;

function TBindingInfoWrapper.TFunctionInfoEnumerator.MoveNext: Boolean;
var
  Info: PFunctionInfo;
begin
  FCurrent := nil;
  Result := False;
  while (FIndex < FFunctionRegistry.FItems.Count-1) do
  begin
    Inc(FIndex);
    Info := FFunctionRegistry.FunctionInfo[FIndex];

    if (Info.FunctionID = FBindingInfo.FunctionID) then
    begin
      FCurrent := Info;
      Result := True;
      break;
    end;
  end;
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

function TFunctionRegistry.GetFunctionInfo(Index: integer): PFunctionInfo;
begin
{$IFNDEF FPC}
  Result := @FItems.List[Index];
{$ELSE}
  Result := @(TFunctionInfoListCracker(FItems).FItems[Index]);
{$ENDIF}
end;

function TFunctionRegistry.GetFunctionBinding(Index: integer): PFunctionBinding;
begin
{$IFNDEF FPC}
  Result := @FBindings.List[Index];
{$ELSE}
  Result := @(TFunctionBindingListCracker(FBindings).FItems[i]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.Add(BindVariable: PPointer; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer; Flags: Cardinal): IFunctionInfo;
var
  FunctionID: NativeInt;
begin
  FunctionID := DoFindBinding(BindVariable);
  Assert(FunctionID <> -1, 'Binding not registered');
  Result := Add(FunctionID, Proc, InstructionSupport, Priority, Flags);
end;

function TFunctionRegistry.Add(FunctionID: NativeInt; Proc: Pointer; InstructionSupport: TInstructionSupport; Priority: Integer; Flags: Cardinal): IFunctionInfo;
var
  Info: TFunctionInfo;
  Index: integer;
begin
  Info := Default(TFunctionInfo);
  Info.FunctionID := FunctionID;
  Info.Proc := Proc;
  Info.InstructionSupport := InstructionSupport;
  Info.Flags := Flags;
  Info.Priority := Priority;

  Index := FItems.Add(Info);

  Result := TFunctionInfoWrapper.Create(FunctionInfo[Index]);

  FNeedRebind := True;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.Clear;
begin
  FItems.Clear;
  FBindings.Clear;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.DoFindBinding(BindVariable: PPointer): NativeInt;
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
  FunctionID := DoFindBinding(BindVariable);
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
    Info := FunctionInfo[i];

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

function TFunctionRegistry.FindBinding(const Name: string): IBindingInfo;
var
  i: Integer;
  Info: PFunctionBinding;
begin
  Result := nil;

  for i := FBindings.Count - 1 downto 0 do
  begin
    Info := FunctionBinding[i];

    if (Info.Name = Name) then
    begin
      Result := TBindingInfoWrapper.Create(Self, Info);
      break;
    end;
  end;
end;

function TFunctionRegistry.FindBinding(BindVariable: PPointer): IBindingInfo;
var
  i: Integer;
  Info: PFunctionBinding;
begin
  Result := nil;

  for i := FBindings.Count - 1 downto 0 do
  begin
    Info := FunctionBinding[i];

    if (Info.BindVariable = BindVariable) then
    begin
      Result := TBindingInfoWrapper.Create(Self, Info);
      break;
    end;
  end;
end;

function TFunctionRegistry.FindBinding(FunctionID: NativeInt): IBindingInfo;
var
  i: Integer;
  Info: PFunctionBinding;
begin
  Result := nil;

  for i := FBindings.Count - 1 downto 0 do
  begin
    Info := FunctionBinding[i];

    if (Info.FunctionID = FunctionID) then
    begin
      Result := TBindingInfoWrapper.Create(Self, Info);
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.FindImplementation(const Name: string): IFunctionInfo;
var
  i: Integer;
  Info: PFunctionInfo;
begin
  Result := nil;

  for i := FItems.Count - 1 downto 0 do
  begin
    Info := FunctionInfo[i];

    if (Info.Name = Name) then
    begin
      Result := TFunctionInfoWrapper.Create(Info);
      break;
    end;
  end;
end;

function TFunctionRegistry.FindImplementation(Proc: pointer): IFunctionInfo;
var
  i: Integer;
  Info: PFunctionInfo;
begin
  Result := nil;

  for i := FItems.Count - 1 downto 0 do
  begin
    Info := FunctionInfo[i];

    if (Info.Proc = Proc) then
    begin
      Result := TFunctionInfoWrapper.Create(Info);
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

type
  TBindingEnumerator = class(TInterfacedObject, IBindingEnumerator)
  private
    FFunctionRegistry: TFunctionRegistry;
    FIndex: integer;
    FCurrent: TFunctionRegistry.PFunctionBinding;
  private
    // IBindingEnumerator
    function GetCurrent: IBindingInfo;
    function MoveNext: Boolean;
  public
    constructor Create(AFunctionRegistry: TFunctionRegistry);
  end;

constructor TBindingEnumerator.Create(AFunctionRegistry: TFunctionRegistry);
begin
  inherited Create;
  FFunctionRegistry := AFunctionRegistry;
end;

function TBindingEnumerator.GetCurrent: IBindingInfo;
begin
  if (FCurrent <> nil) then
    Result := TBindingInfoWrapper.Create(FFunctionRegistry, FCurrent)
  else
    Result := nil;
end;

function TBindingEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < FFunctionRegistry.FBindings.Count-1);
  if (Result) then
    FCurrent := FFunctionRegistry.FunctionBinding[FIndex]
  else
    FCurrent := nil;
end;

function TFunctionRegistry.GetEnumerator: IBindingEnumerator;
begin
  Result := TBindingEnumerator.Create(Self);
end;

//------------------------------------------------------------------------------

function TFunctionRegistry.Rebind(BindVariable: PPointer; PriorityCallback: TFunctionPriority): boolean;
var
  FunctionID: NativeInt;
begin
  FunctionID := DoFindBinding(BindVariable);
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
    P := FunctionBinding[i];

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
    P := FunctionBinding[i];
    P.BindVariable^ := FindFunction(P.FunctionID, PriorityCallback);
  end;

  FNeedRebind := False;
end;

//------------------------------------------------------------------------------

procedure TFunctionRegistry.RegisterBinding(BindVariable: PPointer; const Name: string);
begin
  RegisterBinding(NativeInt(BindVariable), BindVariable, Name);
end;

procedure TFunctionRegistry.RegisterBinding(FunctionID: NativeInt; BindVariable: PPointer; const Name: string);
var
  Binding: TFunctionBinding;
begin
  Binding := Default(TFunctionBinding);
  Binding.FunctionID := FunctionID;
  Binding.BindVariable := BindVariable;
  Binding.Name := Name;

  FBindings.Add(Binding);

  FNeedRebind := True;
end;

//------------------------------------------------------------------------------

end.
