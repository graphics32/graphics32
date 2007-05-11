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
 *
 * ***** END LICENSE BLOCK ***** *)

{ List of platform specific units included in uses clause by default }

{$IFDEF FPC}
  LCLIntf, LCLType,
  {$IFDEF Windows}
    Windows
  {$ENDIF}
  {$IFDEF UNIX}
    Unix, BaseUnix
  {$ENDIF}
{$ELSE}
  {$IFDEF CLX}
    Qt, Types,
    {$IFDEF LINUX}
      Libc
    {$ELSE}
      Windows
    {$ENDIF}
  {$ELSE}
    Windows
  {$ENDIF}
{$ENDIF}
