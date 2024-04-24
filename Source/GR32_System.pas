unit GR32_System;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf, Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$IFNDEF PUREPASCAL}
uses
  GR32.CPUID;
{$ENDIF}

//------------------------------------------------------------------------------
//
//      Performance timer
//
//------------------------------------------------------------------------------
type
  TPerfTimer = class
  private
{$if defined(Windows)}
    FFrequency, FPerformanceCountStart, FPerformanceCountStop: Int64;
{$elseif defined(UNIX)}
  {$IFDEF FPC}
    FStart: Int64;
  {$ENDIF}
{$ifend}
  public
    procedure Start;
    function ReadNanoseconds: string;
    function ReadMilliseconds: string;
    function ReadSeconds: string;

    function ReadValue: Int64;
  end;

var
  GlobalPerfTimer: TPerfTimer;


//------------------------------------------------------------------------------
//
//      Portable GetTickCount
//
//------------------------------------------------------------------------------
{ Pseudo GetTickCount implementation for Linux - for compatibility
  This works for basic time testing, however, it doesnt work like its
  Windows counterpart, ie. it doesnt return the number of milliseconds since
  system boot. Will definitely overflow. }
function GetTickCount: UInt64;


//------------------------------------------------------------------------------
//
//      Processor and core management
//
//------------------------------------------------------------------------------
{ Returns the number of processors configured by the operating system. }
function GetProcessorCount: Cardinal;

// Set process affinity to exclude efficiency cores
function SetPerformanceAffinityMask(Force: boolean = False): boolean;
procedure RestoreAffinityMask;


//------------------------------------------------------------------------------
//
//      Legacy CPU features
//
//------------------------------------------------------------------------------
(*
** Legacy HasInstructionSet and CPUFeatures functions
*)
type
{$IFNDEF PUREPASCAL}
  { TCPUFeature, previously TCPUInstructionSet, defines specific CPU technologies }
  TCPUFeature = (ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);
{$ELSE}
  TCPUFeature = (ciDummy);
  {$DEFINE NO_REQUIREMENTS}
{$ENDIF}

  PCPUFeatures = ^TCPUFeatures;
  TCPUFeatures = set of TCPUFeature;

{ General function that returns whether a particular instruction set is
  supported for the current CPU or not }
function HasInstructionSet(const InstructionSet: TCPUFeature): Boolean; deprecated 'Use CPU.InstructionSupport instead';
function CPUFeatures: TCPUFeatures; deprecated 'Use CPU.InstructionSupport instead';

{$IFNDEF PUREPASCAL}
const
  InstructionSetMap: array[TCPUFeature] of TCPUInstructionSet = (isMMX, isExMMX, isSSE, isSSE2, is3DNow, isEx3DNow);
{$ELSE}
type
  TCPUInstructionSet = (siDummy);
const
  InstructionSetMap: array[TCPUFeature] of TCPUInstructionSet = (siDummy);
type
  TInstructionSupport = set of TCPUInstructionSet;
{$ENDIF}

// Migration support: TCPUFeatures->TInstructionSupport
function CPUFeaturesToInstructionSupport(CPUFeatures: TCPUFeatures): TInstructionSupport;



//------------------------------------------------------------------------------
//
//      CPU features
//
//------------------------------------------------------------------------------
// For use in CPU dispatch bindings
//------------------------------------------------------------------------------
(*
** GR32.CPUID CPU feature detection
*)
// Convenience aliases. For the most common usage, this avoids the need to use GR32.CPUID directly.
{$IFNDEF PUREPASCAL}
type
  TCPU = GR32.CPUID.TCPU;
  TInstructionSupport = GR32.CPUID.TInstructionSupport;
  TCPUInstructionSet = GR32.CPUID.TCPUInstructionSet;

const
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
{$ELSE}
type
  TCPU = record
    InstructionSupport: TInstructionSupport;
  end;
{$ENDIF}

var
  CPU: TCPU;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$ifndef FPC}
  System.Diagnostics,
{$endif}
{$ifdef WINDOWS}
  Windows,
{$endif}
  SysUtils,
  Classes;

//------------------------------------------------------------------------------
//
//      GetTickCount
//
//------------------------------------------------------------------------------
{$if not defined(FPC)}
var
  TickCounter: TStopwatch;

function GetTickCount: UInt64;
begin
  Result := UInt64(TickCounter.ElapsedMilliseconds);
end;
{$else}
function GetTickCount: UInt64;
begin
  Result := SysUtils.GetTickCount64;
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      Performance timer
//
//------------------------------------------------------------------------------
{$if defined(Windows)}
function TPerfTimer.ReadNanoseconds: string;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  Result := IntToStr(Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency));
end;

function TPerfTimer.ReadMilliseconds: string;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  Result := FloatToStrF(1000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency, ffFixed, 15, 3);
end;

function TPerfTimer.ReadSeconds: String;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Result := FloatToStrF((FPerformanceCountStop - FPerformanceCountStart) / FFrequency, ffFixed, 15, 3);
end;

function TPerfTimer.ReadValue: Int64;
begin
  QueryPerformanceCounter(FPerformanceCountStop);
  QueryPerformanceFrequency(FFrequency);
  Assert(FFrequency > 0);

  Result := Round(1000000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency);
end;

procedure TPerfTimer.Start;
begin
  QueryPerformanceCounter(FPerformanceCountStart);
end;
{$elseif defined(UNIX)}
{$IFDEF FPC}
function TPerfTimer.ReadNanoseconds: string;
begin
  Result := IntToStr(ReadValue);
end;

function TPerfTimer.ReadMilliseconds: string;
begin
  Result := IntToStr(ReadValue div 1000);
end;

function TPerfTimer.ReadSeconds: string;
begin
  Result := IntToStr(ReadValue div 1000000);
end;

function TPerfTimer.ReadValue: Int64;
begin
  Result := GetTickCount - FStart;
end;

procedure TPerfTimer.Start;
begin
  FStart := GetTickCount;
end;
{$ENDIF}
{$ifend}


//------------------------------------------------------------------------------
//
//      Processor and core management
//
//------------------------------------------------------------------------------
function GetProcessorCount: Cardinal;
{$IFNDEF FPC}
begin
  Result := CPUCount;
end;
{$ELSE FPC}
{$if defined(Windows)}
var
  lpSysInfo: TSystemInfo;
begin
  GetSystemInfo(lpSysInfo);
  Result := lpSysInfo.dwNumberOfProcessors;
end;
{$elseif defined(UNIX)}
begin
  Result := 1;
end;
{$ifend}
{$ENDIF}

//------------------------------------------------------------------------------

{$if (defined(Windows)) and (not defined(FPC))}
function SetPerformanceAffinityMask(Force: boolean): boolean;
type
  // Declaration in Delphi 11 lacks EfficiencyClass
  TProcessorRelationship = record
    Flags: BYTE;
    EfficiencyClass: BYTE;
    Reserved: array[0..19] of BYTE;
    GroupCount: WORD;
    GroupMask: array[0..0] of GROUP_AFFINITY;
  end;

var
  ProcessHandle: THandle;
  ProcessMask, SystemMask: NativeUInt;
  NewMask: NativeUInt;
  Size: Cardinal;
  ProcessorInfoBuffer: TBytes;
  ProcessorInfo: PSystemLogicalProcessorInformationEx;
  EfficiencyMap: array[Byte] of KAFFINITY;
  CoreMask: ^KAFFINITY;
  i: integer;
begin
  Result := False;

  // TProcessorRelationship.EfficiencyClass requires Windows 10
  if (not CheckWin32Version(10, 0)) then
    exit;

  ProcessHandle := GetCurrentProcess();

  GetProcessAffinityMask(ProcessHandle, ProcessMask, SystemMask);

  // Punt if mask has already been modified
  if (not Force) and (ProcessMask <> SystemMask) then
    exit;

  Size := 0;
  if (not GetLogicalProcessorInformationEx(RelationProcessorCore, nil, Size)) then
    if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
      exit;

  SetLength(ProcessorInfoBuffer, Size);
  ProcessorInfo := @ProcessorInfoBuffer[0];

  if (not GetLogicalProcessorInformationEx(RelationProcessorCore, PSystemLogicalProcessorInformation(ProcessorInfo), Size)) then
    exit;

  ZeroMemory(@EfficiencyMap, SizeOf(EfficiencyMap));

  // For each efficiency class create a core mask
  while (Size > 0) do
  begin
    if (ProcessorInfo.Relationship = RelationProcessorCore) then
    begin
      CoreMask := @EfficiencyMap[TProcessorRelationship(ProcessorInfo.Processor).EfficiencyClass];

      for i := 0 to ProcessorInfo.Processor.GroupCount-1 do
        CoreMask^ := CoreMask^ or ProcessorInfo.Processor.GroupMask[i].Mask;
    end;

    Dec(Size, ProcessorInfo.Size);
    Inc(PByte(ProcessorInfo), ProcessorInfo.Size);
  end;

  // Create a mask for performance cores
  NewMask := 0;
  i := 0;
  while (i < High(EfficiencyMap)) do
  begin
    if (EfficiencyMap[i] <> 0) then
    begin
      // Assume the first performance class is "efficiency". Skip it.
      Inc(i);

      while (i <= High(EfficiencyMap)) do
      begin
        NewMask := NewMask or EfficiencyMap[i];
        Inc(i);
      end;

      break;
    end;
    Inc(i);
  end;

  // Set the new mask
  NewMask := SystemMask and NewMask;
  if (NewMask <> 0) and (NewMask <> ProcessMask) then
  begin
    SetProcessAffinityMask(ProcessHandle, NewMask);
    Result := True;
  end;
end;

procedure RestoreAffinityMask;
var
  ProcessHandle: THandle;
  ProcessMask, SystemMask: NativeUInt;
begin
  ProcessHandle := GetCurrentProcess();

  GetProcessAffinityMask(ProcessHandle, ProcessMask, SystemMask);

  if (ProcessMask <> SystemMask) then
    SetProcessAffinityMask(ProcessHandle, SystemMask);
end;

{$else}
function SetPerformanceAffinityMask(Force: boolean): boolean;
begin
  Result := False;
end;

procedure RestoreAffinityMask;
begin
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      Legacy CPU features
//
//------------------------------------------------------------------------------

function CPUFeaturesToInstructionSupport(CPUFeatures: TCPUFeatures): TInstructionSupport;
var
  InstructionSet: TCPUFeature;
begin
  Result := [];
  for InstructionSet in CPUFeatures do
    Include(Result, InstructionSetMap[InstructionSet]);
end;

//------------------------------------------------------------------------------

function HasInstructionSet(const InstructionSet: TCPUFeature): Boolean;
begin
{$IFNDEF PUREPASCAL}
  Result := (InstructionSetMap[InstructionSet] in CPU.InstructionSupport);
{$ELSE}
  Result := False;
{$ENDIF}
end;

//------------------------------------------------------------------------------

function CPUFeatures: TCPUFeatures;
var
  InstructionSet: TCPUFeature;
begin
  Result := [];
  for InstructionSet := Low(TCPUFeature) to High(TCPUFeature) do
    if (InstructionSetMap[InstructionSet] in CPU.InstructionSupport) then
      Include(Result, InstructionSet);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
{$if not defined(FPC)}
  TickCounter := TStopwatch.StartNew;
{$ifend}

{$IFNDEF PUREPASCAL}
  CPU := TCPU.GetCPUInfo;
{$ELSE}
  CPU := Default(TCPU);
{$ENDIF}
  GlobalPerfTimer := TPerfTimer.Create;

finalization
  GlobalPerfTimer.Free;

end.
