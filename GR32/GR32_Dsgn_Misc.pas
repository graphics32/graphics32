unit GR32_Dsgn_Misc;

interface

uses
  DesignIntf, DesignEditors, Classes, TypInfo;

type
	TKernelClassProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		function GetValue: string; override;
		procedure SetValue(const Value: string); override;
	end;

	TResamplerClassProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		function GetValue: string; override;
		procedure SetValue(const Value: string); override;
	end;

implementation

uses GR32, GR32_Resamplers;

{ TKernelClassProperty }

function TKernelClassProperty.HasSubProperties: Boolean;
begin
  if PropCount > 0 then
    Result := GetTypeData(TBitmap32KernelResampler(GetComponent(0)).Kernel.ClassInfo)^.PropCount > 0
  else
    Result := False;
end;

function TKernelClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paRevertable, paVolatileSubProperties];
end;

function TKernelClassProperty.GetValue: string;
begin
  if PropCount > 0 then
    Result := TBitmap32KernelResampler(GetComponent(0)).KernelClassName
  else
    Result := '';
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
    TBitmap32KernelResampler(GetComponent(0)).KernelClassName := Value
	else SetStrValue('');
	Modified;
end;


{ TResamplerClassProperty }

function TResamplerClassProperty.HasSubProperties: Boolean;
begin
  if PropCount > 0 then
    Result := GetTypeData(TBitmap32(GetComponent(0)).Resampler.ClassInfo)^.PropCount > 0
  else
    Result := False;
end;

function TResamplerClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] +
    [paValueList, paRevertable, paVolatileSubProperties];
end;

function TResamplerClassProperty.GetValue: string;
begin
  if PropCount > 0 then
    Result := TBitmap32(GetComponent(0)).ResamplerClassName
  else
    Result := '';
end;

procedure TResamplerClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  S: TStrings;
begin
  S := GetResamplerClassNames;
  try
    for I := 0 to S.Count - 1 do
      Proc(S[I]);
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
    TBitmap32(GetComponent(0)).ResamplerClassName := Value
	else
    SetStrValue('');

	Modified;
end;

end.
