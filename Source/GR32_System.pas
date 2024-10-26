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

{$include GR32.inc}

uses
{$ifndef FPC}
  System.Diagnostics,
{$endif}
  GR32.CPUID,
  Types; // Not really needed in this unit but we do need something in this uses list


//------------------------------------------------------------------------------
//
//      Delphi compatible TStopwatch-lookalike
//
//------------------------------------------------------------------------------
// Differences from the Delphi TStopwatch:
// - Does not have the Elapsed:TTimeSpan property.
//------------------------------------------------------------------------------
{$if not defined(FPC)}
type
  TStopwatch = System.Diagnostics.TStopWatch;
{$else}
type
  TStopwatch = record
  strict private
    class var FFrequency: Int64;
    class var FIsHighResolution: Boolean;
    class var FTickFrequency: Double;
  strict private
    FElapsed: Int64;
    FRunning: Boolean;
    FStartTimeStamp: Int64;
    function GetElapsedDateTimeTicks: Int64;
    function GetElapsedMilliseconds: Int64;
    function GetElapsedTicks: Int64;
    class constructor Create;
  public
    class function Create: TStopwatch; static;
    class function GetTimeStamp: Int64; static;
    procedure Reset;
    procedure Start;
    class function StartNew: TStopwatch; static;
    procedure Stop;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    class property Frequency: Int64 read FFrequency;
    class property IsHighResolution: Boolean read FIsHighResolution;
    property IsRunning: Boolean read FRunning;
  end;
{$ifend}

{$ifndef FPC}
type
  TStopwatchHelper = record helper for TStopwatch
{$endif}
  const
    TicksPerMicrosecond = 10; // 1 tick = 100ns
    TicksPerNanosecond = TicksPerMicrosecond / 1000;
    TicksPerMillisecond = 1000 * Int64(TicksPerMicrosecond);
    TicksPerSecond = 1000 * Int64(TicksPerMillisecond);
{$ifndef FPC}
  end;
{$endif}


//------------------------------------------------------------------------------
//
//      Performance timer
//
//------------------------------------------------------------------------------
// Obsolete; Use TStopWatch instead.
//------------------------------------------------------------------------------
type
  TPerfTimer = class
  private
    FStopwatch: TStopwatch;
  public
    procedure Start;
    function ReadNanoseconds: string;
    function ReadMilliseconds: string;
    function ReadSeconds: string;

    function ReadValue: Int64;
  end deprecated 'Use TStopwatch';

var
  {$WARN SYMBOL_DEPRECATED OFF}{$ifdef FPC}{$push}{$endif}
  GlobalPerfTimer: TPerfTimer deprecated 'Use TStopwatch';
  {$ifndef FPC}{$WARN SYMBOL_DEPRECATED DEFAULT}{$else}{$pop}{$endif}


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
// Legacy HasInstructionSet and CPUFeatures functions
//------------------------------------------------------------------------------
type
  { TCPUFeature, previously TCPUInstructionSet, defines specific CPU technologies }
  TCPUFeature = (ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);
  TCPUFeatures = set of TCPUFeature;
  PCPUFeatures = ^TCPUFeatures;

{ General function that returns whether a particular instruction set is
  supported for the current CPU or not }
function HasInstructionSet(const InstructionSet: TCPUFeature): Boolean; deprecated 'Use CPU.InstructionSupport instead';
function CPUFeatures: TCPUFeatures; deprecated 'Use CPU.InstructionSupport instead';

const
  InstructionSetMap: array[TCPUFeature] of TCPUInstructionSet = (isMMX, isExMMX, isSSE, isSSE2, is3DNow, isEx3DNow);

// Migration support: TCPUFeatures->TInstructionSupport
function CPUFeaturesToInstructionSupport(CPUFeatures: TCPUFeatures): TInstructionSupport; deprecated;



//------------------------------------------------------------------------------
//
//      CPU features
//
//------------------------------------------------------------------------------
// For use in CPU dispatch bindings
//------------------------------------------------------------------------------
var
  CPU: TCPU;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  SysUtils,
  Classes;

//------------------------------------------------------------------------------
//
//      GetTickCount
//
//------------------------------------------------------------------------------
{$ifndef FPC}
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
{$endif}


//------------------------------------------------------------------------------
//
//      TStopwatch
//
//------------------------------------------------------------------------------
{$if defined(FPC)}

class constructor TStopwatch.Create;
begin
{$if defined(MSWINDOWS)}
  if not QueryPerformanceFrequency(FFrequency) then
  begin // Never happens on XP and later
    FIsHighResolution := False;
    FFrequency := TicksPerSecond;
    FTickFrequency := 1.0;
  end else
  begin
    FIsHighResolution := True;
    FTickFrequency := 10000000.0 / FFrequency;
  end;
{$elseif defined(POSIX)}
  FIsHighResolution := True;
  FFrequency := 10000000; // 100 ns resolution
  FTickFrequency := 10000000.0 / FFrequency;
{$ifend}
end;

class function TStopwatch.Create: TStopwatch;
begin
  Result.Reset;
end;

function TStopwatch.GetElapsedDateTimeTicks: Int64;
begin
  Result := ElapsedTicks;
  if FIsHighResolution then
    Result := Trunc(Result * FTickFrequency);
end;

function TStopwatch.GetElapsedMilliseconds: Int64;
begin
  Result := GetElapsedDateTimeTicks div TicksPerMillisecond;

end;

function TStopwatch.GetElapsedTicks: Int64;
begin
  Result := FElapsed;
  if FRunning then
    Result := Result + GetTimeStamp - FStartTimeStamp;
end;

class function TStopwatch.GetTimeStamp: Int64;
{$if defined(POSIX) and not defined(MACOS)}
var
  res: timespec;
{$ifend}
begin
{$if defined(MSWINDOWS)}
  if FIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    // TODO : This looks wrong. GetTickCount always returns ms
    Result := GetTickCount64 * UInt64(TicksPerMillisecond);
{$elseif defined(MACOS)}
  Result := Int64(AbsoluteToNanoseconds(mach_absolute_time) div 100);
{$elseif defined(POSIX)}
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 100;
{$ifend}
end;

procedure TStopwatch.Reset;
begin
  FElapsed := 0;
  FRunning := False;
  FStartTimeStamp := 0;
end;

procedure TStopwatch.Start;
begin
  if not FRunning then
  begin
    FStartTimeStamp := GetTimeStamp;
    FRunning := True;
  end;
end;

class function TStopwatch.StartNew: TStopwatch;
begin
  Result.Reset;
  Result.Start;
end;

procedure TStopwatch.Stop;
begin
  if FRunning then
  begin
    FElapsed := FElapsed + GetTimeStamp - FStartTimeStamp;
    FRunning := False;
  end;
end;

{$ifend}


//------------------------------------------------------------------------------
//
//      Performance timer
//
//------------------------------------------------------------------------------
function TPerfTimer.ReadNanoseconds: string;
begin
  Result := IntToStr(Round(FStopwatch.ElapsedTicks / {$ifndef FPC}FStopwatch.{$endif}TicksPerNanosecond));
end;

function TPerfTimer.ReadMilliseconds: string;
begin
  Result := FloatToStrF(FStopwatch.ElapsedTicks / {$ifndef FPC}FStopwatch.{$endif}TicksPerMillisecond, ffFixed, 15, 3);
end;

function TPerfTimer.ReadSeconds: String;
begin
  Result := FloatToStrF(FStopwatch.ElapsedTicks / {$ifndef FPC}FStopwatch.{$endif}TicksPerSecond, ffFixed, 15, 3);
end;

function TPerfTimer.ReadValue: Int64;
begin
  Result := FStopwatch.ElapsedTicks;
end;

procedure TPerfTimer.Start;
begin
  FStopwatch := TStopwatch.StartNew;
end;


//------------------------------------------------------------------------------
//
//      Processor and core management
//
//------------------------------------------------------------------------------
function GetProcessorCount: Cardinal;
{$ifndef FPC}
begin
  Result := CPUCount;
end;
{$else}
{$if defined(MSWINDOWS)}
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
{$endif}

//------------------------------------------------------------------------------

{$if (defined(MSWINDOWS)) and (not defined(FPC))}
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
{$ifndef FPC}
  TickCounter := TStopwatch.StartNew;
{$endif}

  CPU := TCPU.GetCPUInfo;

{$WARN SYMBOL_DEPRECATED OFF}{$ifdef FPC}{$push}{$endif}
  GlobalPerfTimer := TPerfTimer.Create;
{$ifndef FPC}{$WARN SYMBOL_DEPRECATED DEFAULT}{$else}{$pop}{$endif}

finalization
{$WARN SYMBOL_DEPRECATED OFF}{$ifdef FPC}{$push}{$endif}
  GlobalPerfTimer.Free;
{$ifndef FPC}{$WARN SYMBOL_DEPRECATED DEFAULT}{$else}{$pop}{$endif}

end.
