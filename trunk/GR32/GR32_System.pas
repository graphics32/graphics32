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

type

   { TCPUInstructionSet, defines specific CPU technologies }
  {$IFDEF TARGET_x86}
    TCPUInstructionSet = (ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);
  {$ELSE}
    { target specific set not defined, force pascal only, ciDummy is simply
      there because enum types can't be empty - in this mode TCPUFeatures = []
      and TCPUFeatures = [ciDummy] are treated the same way. }
    TCPUInstructionSet = (ciDummy);
    {$DEFINE NO_REQUIREMENTS}
  {$ENDIF}

  PCPUFeatures = ^TCPUFeatures;
  TCPUFeatures = set of TCPUInstructionSet;

  TFunctionInfo = packed record
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

  PFunctionTemplateArray = ^TFunctionTemplateArray;
  TFunctionTemplateArray = array [0..0] of TFunctionTemplate;

  PTemplatesHandle = ^TTemplatesHandle;
  TTemplatesHandle = type Integer;

  TFunctionTemplateGroup = packed record
    Count: Integer;
    Templates : PFunctionTemplateArray;
    Name: String;
    HandlePtr: PTemplatesHandle;
  end;

{ General function that returns whether a particular instrucion set is
  supported for the current CPU or not }
function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
function CPUFeatures: TCPUFeatures;

{ General functions that sets up the correct function(s) depending on detected CPU
  features, present implementations and/or compiler directives }

procedure RebindSystem(Requirements: TCPUFeatures);
procedure RebindTemplates(Handle: TTemplatesHandle; Requirements: TCPUFeatures);
procedure RebindFunction(var FunctionPtr: Pointer; Requirements: TCPUFeatures);

{ The template register routine; used internally in GR32 for setting up
  default function template layer - should only be used if you intend to use
  the setup and extension system for managing your own native units.
  If you want to use extensions or alternative routines for GR32 native
  routines, use extensions }

procedure RegisterTemplates(var Handle: TTemplatesHandle;
  var TemplateGroup: array of TFunctionTemplate; const TemplateName: String);

{ The register and remove routines; use these if you want to replace GR32
  native functions with your own versions }

procedure RegisterExtension(var Handle: TTemplatesHandle;
  var TemplateGroup: array of TFunctionTemplate; const TemplateName: String);
procedure RemoveExtension(Handle: TTemplatesHandle);


var
  GlobalPerfTimer: TPerfTimer;

  //Function template registers - DO NOT ALTER THESE DIRECTLY!!
  DefaultTemplateGroups : array of TFunctionTemplateGroup;
  ExtensionTemplateGroups : array of TFunctionTemplateGroup;

implementation

{$IFNDEF CLX}
uses
  Messages, Forms, Classes, TypInfo;
{$ENDIF}

var
  CPUFeaturesInitialized : Boolean = False;
  CPUFeaturesData: TCPUFeatures;

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

function TPerfTimer.ReadNanoseconds: String;
var
  t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := IntToStr( ( (Int64(t.tv_sec) * 1000000) + t.tv_usec ) div 1000 );
end;

function TPerfTimer.ReadMilliseconds: String;
var
  t : timeval;
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
var
  t : timeval;
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
  Result := FloatToStrF(1000 * (FPerformanceCountStop - FPerformanceCountStart) / FFrequency, ffFixed, 15, 3);
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

function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;
begin
  Result := True;
end;

{$ENDIF}
procedure BindTemplate(var Template: TFunctionTemplate; Requirements: TCPUFeatures);
var
  I: Integer;
  InputNotAssigned: Boolean;
begin
  InputNotAssigned := Assigned(Template.FunctionVar);

  {$IFDEF NO_REQUIREMENTS}
  Requirements := [];
  {$ENDIF}

  with Template do
  for I := Count - 1 downto 0 do
    with FunctionProcs^[I] do
       if Requires <= Requirements then
       begin
         FunctionVar^ := Address;
         if Assigned(FunctionVar) then
           Exit;
       end;

  if InputNotAssigned then
  begin
    { Only raise exceptions if FunctionPtr was nil initially
      - we only reach this situation on initialization binding phases }

    if Template.Count = 0 then
      raise Exception.Create('Cannot initialize function based on empty template!');

    if not Assigned(Template.FunctionVar) then
      raise Exception.Create('Invalid Function Info (address is nil)');
  end;

end;

procedure BindTemplatesArray(Templates: PFunctionTemplateArray; Count: Integer; Requirements: TCPUFeatures);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    BindTemplate(Templates^[I], Requirements);
end;

procedure RebindSystem(Requirements: TCPUFeatures);
var
  I: Integer;
begin
  //Bind default
  for I := Low(DefaultTemplateGroups) to High(DefaultTemplateGroups) do
    with DefaultTemplateGroups[I] do
      BindTemplatesArray(Templates, Count, Requirements);

  //Bind eventual extension overlays
  for I := Low(ExtensionTemplateGroups) to High(ExtensionTemplateGroups) do
    with ExtensionTemplateGroups[I] do
      BindTemplatesArray(Templates, Count, Requirements);
end;

procedure RebindTemplates(Handle: TTemplatesHandle; Requirements: TCPUFeatures);
var
  I, J, P: Integer;
  ExtPtr: PPointer;
begin
  if (Handle >= 0) and (Handle <= High(DefaultTemplateGroups)) then
  begin
    //Bind default
    with DefaultTemplateGroups[Handle] do
      BindTemplatesArray(Templates, Count, Requirements);

    //Simple search for possible extension overlays
    for I := Low(ExtensionTemplateGroups) to High(ExtensionTemplateGroups) do
    begin
      with ExtensionTemplateGroups[I] do
      for J := 0 to Count - 1 do
      begin
        ExtPtr := Templates^[J].FunctionVar;
        for P := 0 to DefaultTemplateGroups[Handle].Count - 1 do
        begin
          if ExtPtr = DefaultTemplateGroups[Handle].Templates^[P].FunctionVar then
            BindTemplate(Templates^[J], Requirements);
        end;
      end;
    end;
  end;
end;

procedure RebindFunction(var FunctionPtr: Pointer; Requirements: TCPUFeatures);
var
  I, J: Integer;
begin
  //Bind Default
  for I := Low(DefaultTemplateGroups) to High(DefaultTemplateGroups) do
  with DefaultTemplateGroups[I] do
  begin
    for J := 0 to Count - 1 do
    begin
      if FunctionPtr = Templates^[J].FunctionVar^ then
      begin
        BindTemplate(Templates^[J], Requirements);
        Exit;
      end;
    end;
  end;

  //Bind eventual extension
  for I := Low(ExtensionTemplateGroups) to High(ExtensionTemplateGroups) do
  with ExtensionTemplateGroups[I] do
  begin
    for J := 0 to Count - 1 do
    begin
      if FunctionPtr = Templates^[J].FunctionVar^ then
      begin
        BindTemplate(Templates^[J], Requirements);
        Exit;
      end;
    end;
  end;
end;

procedure _RegisterTemplates(var Handle: TTemplatesHandle;
  var TemplateGroup: array of TFunctionTemplate; const TemplateName: String;
  AsExtension: Boolean);
var
  PDstTemplateGroup: ^TFunctionTemplateGroup;
begin

  if Length(TemplateGroup) > 0 then
  begin
    if AsExtension then
    begin
      Handle := Length(ExtensionTemplateGroups);
      SetLength(ExtensionTemplateGroups, Handle + 1);
      PDstTemplateGroup := @ExtensionTemplateGroups[Handle];
    end
    else
    begin
      Handle := Length(DefaultTemplateGroups);
      SetLength(DefaultTemplateGroups, Handle + 1);
      PDstTemplateGroup := @DefaultTemplateGroups[Handle];
    end;

    with PDstTemplateGroup^ do
    begin
      Templates := @TemplateGroup;
      Count := High(TemplateGroup) - Low(TemplateGroup) + 1;
      Name := TemplateName;
      HandlePtr := @Handle;
      BindTemplatesArray(Templates, Count, CPUFeatures);
    end;
  end
  else
    raise Exception.Create('Cannot register empty TemplateGroup!');
end;

procedure RegisterTemplates(var Handle: TTemplatesHandle;
  var TemplateGroup: array of TFunctionTemplate; const TemplateName: String);
begin
  _RegisterTemplates(Handle, TemplateGroup, TemplateName, False);
end;

procedure RegisterExtension(var Handle: TTemplatesHandle;
  var TemplateGroup: array of TFunctionTemplate; const TemplateName: String);
begin
  _RegisterTemplates(Handle, TemplateGroup, TemplateName, True);
end;

procedure RemoveExtension(Handle: TTemplatesHandle);
var
  I: Integer;
begin
  if (Handle >= 0) and (Handle <= High(ExtensionTemplateGroups)) then
  begin
    if ExtensionTemplateGroups[Handle].HandlePtr^ <> Handle then
      raise Exception.Create('Internal Error - Extension handle mismatch!');

    ExtensionTemplateGroups[Handle].HandlePtr^ := -1; //reset handle

    if Handle = High(ExtensionTemplateGroups) then
      SetLength(ExtensionTemplateGroups, Length(ExtensionTemplateGroups) - 1)
    else
    begin
      for I := Handle to High(ExtensionTemplateGroups) - 1 do
      begin
        ExtensionTemplateGroups[I] := ExtensionTemplateGroups[I + 1];
        ExtensionTemplateGroups[I].HandlePtr^ := I;
      end;
      SetLength(ExtensionTemplateGroups, Length(ExtensionTemplateGroups) - 1);
    end;
    RebindSystem(CPUFeatures);
  end
  else
    raise Exception.Create('Invalid Extension Handle!');
end;

procedure InitCPUFeaturesData;
var
  I: TCPUInstructionSet;
begin
  if CPUFeaturesInitialized then Exit;

  CPUFeaturesData := [];
  for I := Low(TCPUInstructionSet) to High(TCPUInstructionSet) do
    if HasInstructionSet(I) then CPUFeaturesData := CPUFeaturesData + [I];

  CPUFeaturesInitialized := True;
end;

function CPUFeatures: TCPUFeatures;
begin
  if not CPUFeaturesInitialized then
    InitCPUFeaturesData;
  Result := CPUFeaturesData;
end;

initialization
  InitCPUFeaturesData;
  GlobalPerfTimer := TPerfTimer.Create;

finalization
  GlobalPerfTimer.Free;
  DefaultTemplateGroups := nil;
  ExtensionTemplateGroups := nil;

end.
