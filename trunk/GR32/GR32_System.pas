unit GR32_System;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Andre Beckedorf
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$I GR32_Uses.inc}, SysUtils;

type
  TPerfTimer = class
  private
{$IFDEF UNIX}
  {$IFDEF FPC}
    FStart: Int64;
  {$ENDIF}
  {$IFDEF CLX}
    FStart: timespec;
  {$ENDIF}
{$ENDIF}
{$IFDEF Windows}
    FFrequency, FPerformanceCountStart, FPerformanceCountStop: Int64;
{$ENDIF}
  public
    procedure Start;
    function ReadNanoseconds: String;
    function ReadMilliseconds: String;
    function ReadValue: Int64;
  end;

{ Pseudo GetTickCount implementation for Linux - for compatibility
  This works for basic time testing, however, it doesnt work like its
  Windows counterpart, ie. it doesnt return the number of milliseconds since
  system boot. Will definitely overflow. }
function GetTickCount: Cardinal;

{ Returns the number of processors configured by the operating system. }
function GetProcessorCount: Cardinal;
//(*
{ HasMMX returns 'true' if CPU supports MMX instructions }
function HasMMX: Boolean;
{ HasEMMX returns 'true' if CPU supports the Extended MMX (aka Integer SSE) instructions }
function HasEMMX: Boolean;
{ Has3DNow returns 'true' if CPU supports 3DNow! instructions }
function Has3DNow: Boolean;
{ Has3DNowExt returns 'true' if CPU supports 3DNow! Extended instructions }
function Has3DNowExt: Boolean;
{ HasSSE returns 'true' if CPU supports SSE instructions }
function HasSSE: Boolean;
{ HasSSE2 returns 'true' if CPU supports SSE2 instructions }
function HasSSE2: Boolean;
//*)
type

  // TCPUInstructionSet, defines specific CPU technologies

  {$IFDEF TARGET_x86}
    TCPUInstructionSet = (ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);
  {$ELSE}
    TCPUInstructionSet = (ciGeneric); // pascal only
  {$ENDIF}

  PCPUFeatures = ^TCPUFeatures;
  TCPUFeatures = set of TCPUInstructionSet;

  TFunctionInfo = record
    Address: Pointer;
    Requires: TCPUFeatures;
  end;

  PArrayOfFunctionInfo = ^TArrayOfFunctionInfo;
  TArrayOfFunctionInfo = array [0..0] of TFunctionInfo;

  TFunctionTemplate = packed record
    FunctionVar: PPointer;
    FunctionProcs: PArrayOfFunctionInfo;
    Count: Integer;
  end;

  PFunctionTemplates = ^TFunctionTemplates;
  TFunctionTemplates = array of TFunctionTemplate;

{ General function that sets up the correct function depending on detected CPU
  features and present implementations }
function SetupFunction(const Procs : array of TFunctionInfo): Pointer;

{ General function that returns whether a particular instrucion set is
  supported for the current CPU or not }
function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;

procedure Rebind(Templates: array of TFunctionTemplate;
  Requirements: PCPUFeatures = nil);

var
  GlobalPerfTimer: TPerfTimer;
  CPUFeatures: TCPUFeatures;

procedure InitCPUFeatures;

implementation

{$IFNDEF CLX}
uses
  Messages, Forms, Classes;
{$ENDIF}

var
  CPUFeaturesInitialized : Boolean = False;

{$IFDEF UNIX}
{$IFDEF FPC}
function GetTickCount: Cardinal;
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
end;

function TPerfTimer.ReadNanoseconds: String;
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := IntToStr( ( (Int64(t.tv_sec) * 1000000) + t.tv_usec ) div 1000 );
end;

function TPerfTimer.ReadMilliseconds: String;
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := IntToStr( ( (Int64(t.tv_sec) * 1000000) + t.tv_usec ) * 1000 );
end;

function TPerfTimer.ReadValue: Int64;
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
  Result := Result div 1000;
end;

procedure TPerfTimer.Start;
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  FStart := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
end;

{$ENDIF}
{$IFDEF CLX}
function GetTickCount: Cardinal;
var
  val: timespec;
begin
  clock_gettime(CLOCK_REALTIME, val);
  Result := val.tv_sec * 1000 + val.tv_nsec div 1000000;
end;

function TPerfTimer.ReadNanoseconds: String;
var
  val: timespec;
begin
  clock_gettime(CLOCK_REALTIME, val);
  Result := IntToStr(((val.tv_sec * 1000000000) + val.tv_nsec) -
                     ((FStart.tv_sec * 1000000000) + FStart.tv_nsec));
end;

function TPerfTimer.ReadMilliseconds: String;
var
  val: timespec;
begin
  clock_gettime(CLOCK_REALTIME, val);
  Result := IntToStr(((val.tv_sec * 1000) + val.tv_nsec div 1000000) -
                     ((FStart.tv_sec * 1000) + FStart.tv_nsec div 1000000));
end;

function TPerfTimer.ReadValue: Int64;
var
  val: timespec;
begin
  clock_gettime(CLOCK_REALTIME, val);
  Result := ((val.tv_sec * 1000000000) + val.tv_nsec) -
            ((FStart.tv_sec * 1000000000) + FStart.tv_nsec);
end;

procedure TPerfTimer.Start;
begin
  clock_gettime(CLOCK_REALTIME, FStart);
end;
{$ENDIF}
{$ENDIF}
{$IFDEF Windows}
function GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;

function TPerfTimer.ReadNanoseconds: String;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Result := IntToStr(Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency));
end;

function TPerfTimer.ReadMilliseconds: String;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Result := FloatToStr(Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency) / 1000);
end;

function TPerfTimer.ReadValue: Int64;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);

  Result := Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency);
end;

procedure TPerfTimer.Start;
begin
  QueryPerformanceCounter(FPerformanceCountStart);
end;
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF FPC}
function GetProcessorCount: Cardinal;
begin
  Result := 1;
end;
{$ENDIF}
{$IFDEF CLX}
function GetProcessorCount: Cardinal;
begin
  Result := get_nprocs_conf;
end;
{$ENDIF}
{$ENDIF}
{$IFDEF Windows}
function GetProcessorCount: Cardinal;
var
  lpSysInfo: TSystemInfo;
begin
  GetSystemInfo(lpSysInfo);
  Result := lpSysInfo.dwNumberOfProcessors;
end;
{$ENDIF}


{$IFDEF TARGET_x86}

function HasMMX: Boolean;
begin
  Result := HasInstructionSet(ciMMX);
end;

function HasEMMX: Boolean;
begin
  Result := HasInstructionSet(ciEMMX);
end;

function HasSSE: Boolean;
begin
  Result := HasInstructionSet(ciSSE);
end;

function HasSSE2: Boolean;
begin
  Result := HasInstructionSet(ciSSE2);
end;

function Has3DNow: Boolean;
begin
  Result := HasInstructionSet(ci3DNow);
end;

function Has3DNowExt: Boolean;
begin
  Result := HasInstructionSet(ci3DNowExt);
end;

const
  CPUISChecks: Array[TCPUInstructionSet] of Cardinal =
    ($800000,  $400000, $2000000, $4000000, $80000000, $40000000);
    {ciMMX  ,  ciEMMX,  ciSSE   , ciSSE2  , ci3DNow ,  ci3DNowExt}

function CPUID_Available: Boolean;
asm
        MOV       EDX,False
        PUSHFD
        POP       EAX
        MOV       ECX,EAX
        XOR       EAX,$00200000
        PUSH      EAX
        POPFD
        PUSHFD
        POP       EAX
        XOR       ECX,EAX
        JZ        @1
        MOV       EDX,True
@1:     PUSH      EAX
        POPFD
        MOV       EAX,EDX
end;

function CPU_Signature: Integer;
asm
        PUSH    EBX
        MOV     EAX,1
        DW      $A20F   // CPUID
        POP     EBX
end;

function CPU_Features: Integer;
asm
        PUSH    EBX
        MOV     EAX,1
        DW      $A20F   // CPUID
        POP     EBX
        MOV     EAX,EDX
end;

function CPU_ExtensionsAvailable: Boolean;
asm
        PUSH    EBX
        MOV     @Result, True
        MOV     EAX, $80000000
        DW      $A20F   // CPUID
        CMP     EAX, $80000000
        JBE     @NOEXTENSION
        JMP     @EXIT
      @NOEXTENSION:
        MOV     @Result, False
      @EXIT:
        POP     EBX
end;

function CPU_ExtFeatures: Integer;
asm
        PUSH    EBX
        MOV     EAX, $80000001
        DW      $A20F   // CPUID
        POP     EBX
        MOV     EAX,EDX
end;

function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
// Must be implemented for each target CPU on which specific functions rely
begin

  {
  //Simulation of other i386 types, example feature set shown
  if [InstructionSet] <= [ciMMX, ciEMMX, ciSSE] then
    Result := True
  else
  begin
    Result := False;
    Exit;
  end;
  }

  Result := False;
  if not CPUID_Available then Exit;                   // no CPUID available
  if CPU_Signature shr 8 and $0F < 5 then Exit;       // not a Pentium class

  case InstructionSet of
    ci3DNow, ci3DNowExt:
      if not CPU_ExtensionsAvailable or (CPU_ExtFeatures and CPUISChecks[InstructionSet] = 0) then
        Exit;
    ciEMMX:
      begin
        // check for SSE, necessary for Intel CPUs because they don't implement the
        // extended info
        if (CPU_Features and CPUISChecks[ciSSE] = 0) and
          (not CPU_ExtensionsAvailable or (CPU_ExtFeatures and CPUISChecks[ciEMMX] = 0)) then
          Exit;
      end;
  else
    if CPU_Features and CPUISChecks[InstructionSet] = 0 then
      Exit; // return -> instruction set not supported
  end;

  Result := True;
end;

{$ELSE}

function HasMMX: Boolean;
begin
  Result := False;
end;

function HasEMMX: Boolean;
begin
  Result := False;
end;

function HasSSE: Boolean;
begin
  Result := False;
end;

function HasSSE2: Boolean;
begin
  Result := False;
end;

function Has3DNow: Boolean;
begin
  Result := False;
end;

function Has3DNowExt: Boolean;
begin
  Result := False;
end;

function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
// Generic
begin
  Result := (InstructionSet = ciGeneric);
end;

{$ENDIF}

function SetupFunction(const Procs : array of TFunctionInfo): Pointer;
var
  I: Integer;
begin
  //Assumes ordering after estimated performance, fastest last in Procs array
  for I := High(Procs) downto Low(Procs) do
     with Procs[I] do
        if Requires <= CPUFeatures then
        begin
          Result := Address;
          if Assigned(Result) then
            Exit;
        end;

  if Length(Procs) = 0 then
    raise Exception.Create('Cannot initialize empty array.');

  //Try to link generic
  if not Assigned(Result) then
    Result := Procs[0].Address;

  if not Assigned(Result) then
    raise Exception.Create('Invalid Function Info (address is nil)');

end;

function BindFunction(const Procs : array of TFunctionInfo;
  Requirements: TCPUFeatures): Pointer;
var
  I: Integer;
begin
  for I := High(Procs) downto Low(Procs) do
     with Procs[I] do
        if Requires <= Requirements then
        begin
          Result := Address;
          if Assigned(Result) then
            Exit;
        end;

  if Length(Procs) = 0 then
    raise Exception.Create('Cannot initialize empty array.');

  //Try to link generic
  if not Assigned(Result) then
    Result := Procs[0].Address;

  if not Assigned(Result) then
    raise Exception.Create('Invalid Function Info (address is nil)');
end;


procedure Rebind(Templates: array of TFunctionTemplate;
  Requirements: PCPUFeatures = nil);
var
  I: Integer;
begin
  //if nil then assume actually detected features are to be used
  if Requirements = nil then
    Requirements := @CPUFeatures;

  for I := Low(Templates) to High(Templates) do
  begin
    with Templates[I] do
      FunctionVar^ := BindFunction(Slice(FunctionProcs^, Count), Requirements^);
  end;
end;

procedure InitCPUFeatures;
var
  I: TCPUInstructionSet;
begin
  if CPUFeaturesInitialized then Exit;

  CPUFeatures := [];
  for I := Low(TCPUInstructionSet) to High(TCPUInstructionSet) do
    if HasInstructionSet(I) then CPUFeatures := CPUFeatures + [I];

  CPUFeaturesInitialized := True;
end;

initialization
  InitCPUFeatures;
  GlobalPerfTimer := TPerfTimer.Create;

finalization
  GlobalPerfTimer.Free;

end.
