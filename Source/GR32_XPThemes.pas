unit GR32_XPThemes;

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
 * Andre Beckedorf
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
  SysUtils;


{$IFDEF Windows}

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

{$IFDEF Windows}

uses
  Messages, Forms, Classes;

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
  FWindowHandle := Classes.AllocateHWnd(WndProc);
  OpenVisualStyles;
end;

destructor TThemeNexus.Destroy;
begin
  CloseVisualStyles;
  Classes.DeallocateHWnd(FWindowHandle);
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

{$IFDEF XPTHEMES}
initialization
  ThemeNexus := TThemeNexus.Create;

finalization
  ThemeNexus.Free;
{$ENDIF}

{$ENDIF}

end.
