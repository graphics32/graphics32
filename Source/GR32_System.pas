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
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Andre Beckedorf
 *  Michael Hansen <dyster_tid@hotmail.com>
 *    - CPU type & feature-set aware function binding
 *    - Runtime function template and extension binding system
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType,
  {$IFDEF Windows}
    Windows,
  {$ENDIF}
  {$IFDEF UNIX}
    Unix, BaseUnix,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}
{$IFNDEF PUREPASCAL}
  GR32.CPUID,
{$ENDIF}
  SysUtils;

type
  TPerfTimer = class
  private
{$IFDEF UNIX}
  {$IFDEF FPC}
    FStart: Int64;
  {$ENDIF}
{$ENDIF}
{$IFDEF Windows}
    FFrequency, FPerformanceCountStart, FPerformanceCountStop: Int64;
{$ENDIF}
  public
    procedure Start;
    function ReadNanoseconds: string;
    function ReadMilliseconds: string;
    function ReadSeconds: string;

    function ReadValue: Int64;
  end;

var
  GlobalPerfTimer: TPerfTimer;

{ Pseudo GetTickCount implementation for Linux - for compatibility
  This works for basic time testing, however, it doesnt work like its
  Windows counterpart, ie. it doesnt return the number of milliseconds since
  system boot. Will definitely overflow. }
function GetTickCount: Cardinal;

{ Returns the number of processors configured by the operating system. }
function GetProcessorCount: Cardinal;


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
const
  siDummy = ciDummy;
  InstructionSetMap: array[TCPUInstructionSet] of TCPUInstructionSet = (siDummy);
type
  TInstructionSupport = set of TCPUInstructionSet;
{$ENDIF}

// Migration support: TCPUFeatures->TInstructionSupport
function CPUFeaturesToInstructionSupport(CPUFeatures: TCPUFeatures): TInstructionSupport;



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


implementation

uses
  Classes;

{$IFDEF UNIX}
{$IFDEF FPC}
function GetTickCount: Cardinal;
var
  t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
end;


{ TPerfTimer }

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
{$ENDIF}

{$IFDEF Windows}
function GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;


{ TPerfTimer }

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
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF FPC}
function GetProcessorCount: Cardinal;
begin
  Result := 1;
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

function CPUFeaturesToInstructionSupport(CPUFeatures: TCPUFeatures): TInstructionSupport;
var
  InstructionSet: TCPUFeature;
begin
  Result := [];
  for InstructionSet in CPUFeatures do
    Include(Result, InstructionSetMap[InstructionSet]);
end;

function HasInstructionSet(const InstructionSet: TCPUFeature): Boolean;
begin
{$IFNDEF PUREPASCAL}
  Result := (InstructionSetMap[InstructionSet] in CPU.InstructionSupport);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function CPUFeatures: TCPUFeatures;
var
  InstructionSet: TCPUFeature;
begin
  Result := [];
  for InstructionSet := Low(TCPUFeature) to High(TCPUFeature) do
    if HasInstructionSet(InstructionSet) then
      Include(Result, InstructionSet);
end;

initialization
{$IFNDEF PUREPASCAL}
  CPU := TCPU.GetCPUInfo;
{$ELSE}
  CPU := Default(TCPU);
{$ENDIF}
  GlobalPerfTimer := TPerfTimer.Create;

finalization
  GlobalPerfTimer.Free;

end.
