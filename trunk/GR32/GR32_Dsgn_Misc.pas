unit GR32_Dsgn_Misc;

interface

uses
  DesignIntf, DesignEditors, Classes, TypInfo;

type
  TCustomClassProperty = class(TClassProperty)
  protected
    class function GetClassList: TList; virtual; abstract;
    function HasSubProperties: Boolean;
    procedure SetClassName(const CustomClass: string); virtual; abstract;
    function GetObject: TObject; virtual; abstract;
  public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure SetValue(const Value: string); override;
		function GetValue: string; override;
  end;

	TKernelClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TList; override;
    procedure SetClassName(const CustomClass: string); override;
    function GetObject: TObject; override;
	end;

	TResamplerClassProperty = class(TCustomClassProperty)
  protected
    class function GetClassList: TList; override;
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
  S: TStrings;
begin
  S := GetCustomClassNames(GetClassList);
  try
    for I := 0 to S.Count - 1 do
      Proc(S[I]);
  finally
    S.Free;
  end;
end;

function TCustomClassProperty.HasSubProperties: Boolean;
begin
  if PropCount > 0 then
    Result := GetTypeData(GetObject.ClassInfo)^.PropCount > 0
  else
    Result := False;
end;

procedure TCustomClassProperty.SetValue(const Value: string);
begin
	if Assigned(FindCustomClass(Value, GetClassList)) then
    SetClassName(Value)
	else SetStrValue('');
	Modified;
end;

{ TKernelClassProperty }

class function TKernelClassProperty.GetClassList: TList;
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

class function TResamplerClassProperty.GetClassList: TList;
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
