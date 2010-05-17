unit GR32_Dsgn_Misc;

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
 * The Initial Developers of the Original Code are
 * Mattias Andersson <mattias@centaurix.com>
 * Andre Beckedorf <andre@metaexception.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits,{$ELSE}
  DesignIntf, DesignEditors,{$ENDIF}
  Classes, TypInfo, GR32_Containers;

type
  TCustomClassProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  protected
    class function GetClassList: TClassList; virtual;
    procedure SetClassName(const CustomClass: string); virtual; {$IFNDEF BCB} abstract; {$ENDIF}
    function GetObject: TObject; virtual; {$IFNDEF BCB} abstract; {$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TKernelClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    procedure SetClassName(const CustomClass: string); override;
    function GetObject: TObject; override;
  end;

  TResamplerClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TClassList; override;
    procedure SetClassName(const CustomClass: string); override;
    function GetObject: TObject; override;
  end;

implementation

uses GR32, GR32_Resamplers;
                        
{ TCustomClassProperty }

function TCustomClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paReadOnly] +
    [paValueList, paRevertable, paVolatileSubProperties];
  if not HasSubProperties then Exclude(Result, paSubProperties);
end;

class function TCustomClassProperty.GetClassList: TClassList;
begin
  Result := nil;
end;

function TCustomClassProperty.GetValue: string;
begin
  if PropCount > 0 then
    Result := GetObject.ClassName
  else
    Result := '';
end;

procedure TCustomClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
      Proc(L.Items[I].ClassName);
end;

function TCustomClassProperty.HasSubProperties: Boolean;
begin
  if PropCount > 0 then
    Result := GetTypeData(GetObject.ClassInfo)^.PropCount > 0
  else
    Result := False;
end;

procedure TCustomClassProperty.SetValue(const Value: string);
var
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) and Assigned(L.Find(Value)) then
    SetClassName(Value)
  else SetStrValue('');
  Modified;
end;

{$IFDEF BCB}
class function TCustomClassProperty.GetClassList: TClassList;
begin
  Result := nil;
end;

procedure TCustomClassProperty.SetClassName(const CustomClass: string);
begin
end;

function TCustomClassProperty.GetObject: TObject;
begin
  Result := nil;
end;
{$ENDIF}

{ TKernelClassProperty }

class function TKernelClassProperty.GetClassList: TClassList;
begin
  Result := KernelList;
end;

function TKernelClassProperty.GetObject: TObject;
begin
  Result := TKernelResampler(GetComponent(0)).Kernel;
end;

procedure TKernelClassProperty.SetClassName(const CustomClass: string);
begin
  TKernelResampler(GetComponent(0)).KernelClassName := CustomClass;
end;

{ TResamplerClassProperty }

class function TResamplerClassProperty.GetClassList: TClassList;
begin
  Result := ResamplerList;
end;

function TResamplerClassProperty.GetObject: TObject;
begin
  Result := TBitmap32(GetComponent(0)).Resampler;
end;

procedure TResamplerClassProperty.SetClassName(
  const CustomClass: string);
begin
  TBitmap32(GetComponent(0)).ResamplerClassName := CustomClass;
end;

end.

