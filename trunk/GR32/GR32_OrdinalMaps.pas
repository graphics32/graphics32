unit GR32_IntegerMaps;

interface

uses
  GR32;

type
  TArrayOfWord = array of Word;
  TArrayOfInteger = array of Integer;

  TWordMap = class(TCustomMap)
  private
    FBits: TArrayOfWord;
    function GetValPtr(X, Y: Integer): PWord;
    function GetValue(X, Y: Integer): Word;
    procedure SetValue(X, Y: Integer; const Value: Word);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    function Empty: Boolean; override;
    procedure Clear(FillValue: Word);
    property ValPtr[X, Y: Integer]: PWord read GetValPtr;
    property Value[X, Y: Integer]: Word read GetValue write SetValue; default;
  end;

  TIntegerMap = class(TCustomMap)
  private
    FBits: TArrayOfInteger;
    function GetValPtr(X, Y: Integer): PInteger;
    function GetValue(X, Y: Integer): Integer;
    procedure SetValue(X, Y: Integer; const Value: Integer);
  protected
    procedure ChangeSize(var Width, Height: Integer; NewWidth, NewHeight: Integer); override;
  public
    function Empty: Boolean; override;
    procedure Clear(FillValue: Integer);
    property ValPtr[X, Y: Integer]: PInteger read GetValPtr;
    property Value[X, Y: Integer]: Integer read GetValue write SetValue; default;
  end;

implementation

uses
  GR32_LowLevel;

{ TWordMap }

procedure TWordMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  SetLength(FBits, NewWidth * NewHeight);
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TWordMap.Clear(FillValue: Word);
begin
  FillWord(FBits[0], Width * Height, FillValue);
  Changed;
end;

function TWordMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TWordMap.GetValPtr(X, Y: Integer): PWord;
begin
  Result := @FBits[X + Y * Width];
end;

function TWordMap.GetValue(X, Y: Integer): Word;
begin
  Result := FBits[X + Y * Width];
end;

procedure TWordMap.SetValue(X, Y: Integer; const Value: Word);
begin
  FBits[X + Y * Width] := Value;
end;

{ TIntegerMap }

procedure TIntegerMap.ChangeSize(var Width, Height: Integer; NewWidth,
  NewHeight: Integer);
begin
  SetLength(FBits, NewWidth * NewHeight);
  Width := NewWidth;
  Height := NewHeight;
end;

procedure TIntegerMap.Clear(FillValue: Integer);
begin
  FillLongword(FBits[0], Width * Height, FillValue);
  Changed;
end;

function TIntegerMap.Empty: Boolean;
begin
  Result := not Assigned(FBits);
end;

function TIntegerMap.GetValPtr(X, Y: Integer): PInteger;
begin
  Result := @FBits[X + Y * Width];
end;

function TIntegerMap.GetValue(X, Y: Integer): Integer;
begin
  Result := FBits[X + Y * Width];
end;

procedure TIntegerMap.SetValue(X, Y: Integer; const Value: Integer);
begin
  FBits[X + Y * Width] := Value;
end;

end.
