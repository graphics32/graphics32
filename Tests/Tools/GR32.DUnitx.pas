unit GR32.DUnitx;

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
 * The Original Code is DUnitX utilities for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I ..\..\Source\GR32.inc}

uses
  DUnitX.TestFramework,
  GR32_Bindings;

type
  TestCaseSourceAttribute = class(CustomTestCaseSourceAttribute)
  private
    FRegistry: TFunctionRegistry;
    FBindingName: string;
  protected
    function GetCaseInfoArray: TestCaseInfoArray; override;
  public
    constructor Create(const ARegistry: string; const ABindingName: string);
    property Registry: TFunctionRegistry read FRegistry;
    property BindingName: string read FBindingName;
  end;

implementation

uses
  SysUtils,
  Classes,
{$IFDEF FPC}
  Generics.Collections;
{$ELSE}
  System.Generics.Collections;
{$ENDIF}

{ TestCaseSourceAttribute }

constructor TestCaseSourceAttribute.Create(const ARegistry: string; const ABindingName: string);
begin
  inherited Create;
  FRegistry := TFunctionRegistry.FindRegistry(ARegistry);
  FBindingName := ABindingName;
  if (FRegistry = nil) then
    raise Exception.CreateFmt('A binding registry with the name "%s" was not found', [ARegistry]);
end;

function TestCaseSourceAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  Binding: IBindingInfo;
  Impl: IFunctionInfo;
  List: TList<IFunctionInfo>;
  i: integer;
begin
  Binding := FRegistry.FindBinding(FBindingName);
  if Binding = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  List := TList<IFunctionInfo>.Create;
  try
    for Impl in Binding do
      List.Add(Impl);

    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
    begin
      Result[i].Name := List[i].Name;
      SetLength(Result[i].Values, 1);
      Result[i].Values[0] := List[i].Name;
    end;
  finally
    List.Free;
  end;
end;

end.
