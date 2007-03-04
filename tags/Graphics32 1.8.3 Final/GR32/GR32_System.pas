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
  SysUtils,
  {$IFDEF CLX}
  Qt, Types {$IFDEF LINUX}, Libc {$ELSE}, Windows{$ENDIF}
  {$ELSE}
  Windows
  {$ENDIF};

type
  TPerfTimer = class
  private
{$IFDEF LINUX}
    FStart: timespec;
{$ELSE}
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

type
  TCPUInstructionSet = (ciMMX, ciEMMX, ciSSE, ciSSE2, ci3DNow, ci3DNowExt);

{ General function that returns whether a particular instrucion set is
  supported for the current CPU or not }
function HasInstructionSet(const InstructionSet: TCPUInstructionSet): Boolean;

const
  CPUISChecks: Array[TCPUInstructionSet] of Cardinal =
    ($800000,  $400000, $2000000, $4000000, $80000000, $40000000);
//   ciMMX  ,  ciEMMX,  ciSSE   , ciSSE2  , ci3DNow ,  ci3DNowExt

var
  GlobalPerfTimer: TPerfTimer;

{$IFNDEF CLX}
{ Internal support for Windows XP themes }
var
  USE_THEMES: Boolean = False;
  SCROLLBAR_THEME: THandle = 0;
  GLOBALS_THEME: THandle = 0;

const
  THEMEMGR_VERSION                     = 1;
  WM_THEMECHANGED                      = $031A;

{ "Scrollbar" Parts & States }
  { SCROLLBARPARTS }
  SBP_ARROWBTN                         = 1;
  SBP_THUMBBTNHORZ                     = 2;
  SBP_THUMBBTNVERT                     = 3;
  SBP_LOWERTRACKHORZ                   = 4;
  SBP_UPPERTRACKHORZ                   = 5;
  SBP_LOWERTRACKVERT                   = 6;
  SBP_UPPERTRACKVERT                   = 7;
  SBP_GRIPPERHORZ                      = 8;
  SBP_GRIPPERVERT                      = 9;
  SBP_SIZEBOX                          = 10;

  { ARROWBTNSTATES }
  ABS_UPNORMAL                         = 1;
  ABS_UPHOT                            = 2;
  ABS_UPPRESSED                        = 3;
  ABS_UPDISABLED                       = 4;
  ABS_DOWNNORMAL                       = 5;
  ABS_DOWNHOT                          = 6;
  ABS_DOWNPRESSED                      = 7;
  ABS_DOWNDISABLED                     = 8;
  ABS_LEFTNORMAL                       = 9;
  ABS_LEFTHOT                          = 10;
  ABS_LEFTPRESSED                      = 11;
  ABS_LEFTDISABLED                     = 12;
  ABS_RIGHTNORMAL                      = 13;
  ABS_RIGHTHOT                         = 14;
  ABS_RIGHTPRESSED                     = 15;
  ABS_RIGHTDISABLED                    = 16;

  { SCROLLBARSTATES }
  SCRBS_NORMAL                         = 1;
  SCRBS_HOT                            = 2;
  SCRBS_PRESSED                        = 3;
  SCRBS_DISABLED                       = 4;

  { SIZEBOXSTATES }
  SZB_RIGHTALIGN                       = 1;
  SZB_LEFTALIGN                        = 2;

{ Access to uxtheme.dll }

type
  HIMAGELIST = THandle;
  HTHEME = THandle;
  _MARGINS = record
    cxLeftWidth: Integer;      // width of left border that retains its size
    cxRightWidth: Integer;     // width of right border that retains its size
    cyTopHeight: Integer;      // height of top border that retains its size
    cyBottomHeight: Integer;   // height of bottom border that retains its size
  end;
  MARGINS = _MARGINS;
  PMARGINS = ^MARGINS;
  TMargins = MARGINS;

var
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
  CloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;
  DrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const Rect: TRect; pClipRect: PRect): HRESULT; stdcall;
  DrawThemeEdge: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge,
    uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;
  GetThemeColor: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall;
  GetThemeMetric: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;
  GetThemeMargins: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer; prc: PRECT;
    var pMargins: MARGINS): HRESULT; stdcall;
  SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
  IsThemeActive: function: BOOL; stdcall;
  IsAppThemed: function: BOOL; stdcall;
  EnableTheming: function(fEnable: BOOL): HRESULT; stdcall;
{$ENDIF}

implementation

{$IFNDEF CLX}
uses
  Messages, Forms, Classes;
{$ENDIF}

{$IFDEF LINUX}
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
{$ELSE}
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

{$IFDEF LINUX}
function GetProcessorCount: Cardinal;
begin
  Result := get_nprocs_conf;
end;
{$ELSE}
function GetProcessorCount: Cardinal;
var
  lpSysInfo: TSystemInfo;
begin
  GetSystemInfo(lpSysInfo);
  Result := lpSysInfo.dwNumberOfProcessors;
end;
{$ENDIF}

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

{$IFNDEF CLX}
const
  UXTHEME_DLL = 'uxtheme.dll';

var
  DllHandle: THandle;

procedure FreeXPThemes;
begin
  if DllHandle <> 0 then
  begin
    if not IsLibrary then
      FreeLibrary(DllHandle);
      
    DllHandle := 0;
    OpenThemeData := nil;
    CloseThemeData := nil;
    DrawThemeBackground := nil;
    DrawThemeEdge := nil;
    GetThemeColor := nil;
    GetThemeMetric := nil;
    GetThemeMargins := nil;
    SetWindowTheme := nil;
    IsThemeActive := nil;
    IsAppThemed := nil;
    EnableTheming := nil;
  end;
end;

function InitXPThemes: Boolean;
begin
  if DllHandle = 0 then
  begin
    DllHandle := LoadLibrary(UXTHEME_DLL);
    if DllHandle > 0 then
    begin
      OpenThemeData := GetProcAddress(DllHandle, 'OpenThemeData');
      CloseThemeData := GetProcAddress(DllHandle, 'CloseThemeData');
      DrawThemeBackground := GetProcAddress(DllHandle, 'DrawThemeBackground');
      DrawThemeEdge := GetProcAddress(DllHandle, 'DrawThemeEdge');
      GetThemeColor := GetProcAddress(DllHandle, 'GetThemeColor');
      GetThemeMetric := GetProcAddress(DllHandle, 'GetThemeMetric');
      GetThemeMargins := GetProcAddress(DllHandle, 'GetThemeMargins');
      SetWindowTheme := GetProcAddress(DllHandle, 'SetWindowTheme');
      IsThemeActive := GetProcAddress(DllHandle, 'IsThemeActive');
      IsAppThemed := GetProcAddress(DllHandle, 'IsAppThemed');
      EnableTheming := GetProcAddress(DllHandle, 'EnableTheming');
      if (@OpenThemeData = nil) or (@CloseThemeData = nil) or (@IsThemeActive = nil) or
        (@IsAppThemed = nil) or (@EnableTheming = nil) then FreeXPThemes;
    end;
  end;
  Result := DllHandle > 0;
end;

function UseXPThemes: Boolean;
begin
  Result := (DllHandle > 0) and IsAppThemed and IsThemeActive;
end;

type
  TThemeNexus = class
  private
    FWindowHandle: HWND;
  protected
    procedure WndProc(var Message: TMessage);
    procedure OpenVisualStyles;
    procedure CloseVisualStyles;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{$IFDEF XPTHEMES}
var
  ThemeNexus: TThemeNexus;
{$ENDIF}

{ TThemeNexus }

procedure TThemeNexus.CloseVisualStyles;
begin
  if not IsLibrary and UseXPThemes then
  begin
    if SCROLLBAR_THEME <> 0 then
    begin
      CloseThemeData(SCROLLBAR_THEME);
      SCROLLBAR_THEME := 0;
    end;
    if GLOBALS_THEME <> 0 then
    begin
      CloseThemeData(GLOBALS_THEME);
      GLOBALS_THEME := 0;
    end;
  end;
  FreeXPThemes;
end;

constructor TThemeNexus.Create;
begin
  FWindowHandle := {$IFDEF COMPILER6}Classes.{$ENDIF}AllocateHWnd(WndProc);
  OpenVisualStyles;
end;

destructor TThemeNexus.Destroy;
begin
  CloseVisualStyles;
  {$IFDEF COMPILER6}Classes.{$ENDIF}DeallocateHWnd(FWindowHandle);
  inherited;
end;

procedure TThemeNexus.OpenVisualStyles;
begin
  USE_THEMES := False;
  if InitXPThemes then
  begin
    USE_THEMES := UseXPThemes;
    if USE_THEMES then
    begin
      SCROLLBAR_THEME := OpenThemeData(FWindowHandle, 'SCROLLBAR');
      GLOBALS_THEME := OpenThemeData(FWindowHandle, 'GLOBALS');
    end;
  end;
end;

procedure TThemeNexus.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_THEMECHANGED:
      begin
        CloseVisualStyles;
        OpenVisualStyles;
      end;
  end;
  with Message do Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;
{$ENDIF}

initialization
{$IFNDEF CLX}
  {$IFDEF XPTHEMES}
  ThemeNexus := TThemeNexus.Create;
  {$ENDIF}
{$ENDIF}
  GlobalPerfTimer := TPerfTimer.Create;

finalization
  GlobalPerfTimer.Free;
{$IFNDEF CLX}
  {$IFDEF XPTHEMES}
  ThemeNexus.Free;
  {$ENDIF}
{$ENDIF}

end.
