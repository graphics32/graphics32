unit GR32_Dsgn_Misc;

interface

uses
  DesignIntf, DesignEditors, Classes;

type
	TKernelClassProperty = class(TClassProperty)
  public
		function GetAttributes : TPropertyAttributes; override;
		procedure GetValues(Proc : TGetStrProc); override;
		function GetValue: string; override;
		procedure SetValue(const Value: string); override;
	end;

	TResamplerClassProperty = class(TClassProperty)
  public
		function GetAttributes : TPropertyAttributes; override;
		procedure GetValues(Proc : TGetStrProc); override;
		function GetValue: string; override;
		procedure SetValue(const Value: string); override;
	end;

implementation

uses GR32, GR32_Resamplers;


{ TKernelClassProperty }

function TKernelClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paValueList];
end;

function TKernelClassProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TKernelClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  S: TStrings;
begin
  S := GetKernelClassNames;
  try
    for I := 0 to S.Count - 1 do
      Proc(S.Strings[I]);
  finally
    S.Free;
  end;
end;

procedure TKernelClassProperty.SetValue(const Value: string);
var
	RC: TCustomKernelClass;
begin
	RC := FindKernelClass(Value);
	if Assigned(RC) then
		SetStrValue(RC.ClassName)
	else SetStrValue('');
	Modified;
end;


{ TResamplerClassProperty }

function TResamplerClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paValueList];
end;

function TResamplerClassProperty.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TResamplerClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  S: TStrings;
begin
  S := GetResamplerClassNames;
  try
    for I := 0 to S.Count - 1 do
      Proc(S.Strings[I]);
  finally
    S.Free;
  end;
end;

procedure TResamplerClassProperty.SetValue(const Value: string);
var
	RC: TCustomResamplerClass;
begin
	RC := FindResamplerClass(Value);
	if Assigned(RC) then
		SetStrValue(RC.ClassName)
	else SetStrValue('');
	Modified;
end;

end.
