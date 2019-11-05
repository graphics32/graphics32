unit GR32_Gamma;

interface

uses
  GR32;

{ Gamma bias for line/pixel antialiasing }

type
  TGammaTable8Bit = array [Byte] of Byte;

var
  GAMMA_VALUE: Double;
  GAMMA_ENCODING_TABLE: TGammaTable8Bit;
  GAMMA_DECODING_TABLE: TGammaTable8Bit;

const
  DEFAULT_GAMMA: Double = 1.6;

// set gamma
procedure SetGamma; overload;  {$IFDEF USEINLINING} inline; {$ENDIF}
procedure SetGamma(Gamma: Double); overload;  {$IFDEF USEINLINING} inline; {$ENDIF}
procedure SetGamma(Gamma: Double; var GammaTable: TGammaTable8Bit); overload;

procedure Set_sRGB; overload;
procedure Set_sRGB(var GammaTable: TGammaTable8Bit); overload;
procedure SetInv_sRGB(var GammaTable: TGammaTable8Bit);

// apply gamma
function ApplyGamma(Color: TColor32): TColor32; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function ApplyInvGamma(Color: TColor32): TColor32; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function ApplyCustomGamma(Color: TColor32; GammaTable: TGammaTable8Bit): TColor32; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

procedure ApplyGamma(Color: PColor32Array; Length: Integer); overload;
procedure ApplyInvGamma(Color: PColor32Array; Length: Integer); overload;
procedure ApplyCustomGamma(Color: PColor32Array; Length: Integer; GammaTable: TGammaTable8Bit); overload;

procedure ApplyGamma(Bitmap: TBitmap32); overload;
procedure ApplyInvGamma(Bitmap: TBitmap32); overload;
procedure ApplyCustomGamma(Bitmap: TBitmap32; GammaTable: TGammaTable8Bit); overload;
procedure ApplyCustomGamma(Bitmap: TBitmap32; Gamma: Double); overload;

implementation

uses
  Math;

function ApplyGamma(Color: TColor32): TColor32;
begin
  TColor32Entry(Result).R := GAMMA_ENCODING_TABLE[TColor32Entry(Color).R];
  TColor32Entry(Result).G := GAMMA_ENCODING_TABLE[TColor32Entry(Color).G];
  TColor32Entry(Result).B := GAMMA_ENCODING_TABLE[TColor32Entry(Color).B];
end;

function ApplyInvGamma(Color: TColor32): TColor32;
begin
  TColor32Entry(Result).R := GAMMA_DECODING_TABLE[TColor32Entry(Color).R];
  TColor32Entry(Result).G := GAMMA_DECODING_TABLE[TColor32Entry(Color).G];
  TColor32Entry(Result).B := GAMMA_DECODING_TABLE[TColor32Entry(Color).B];
end;

function ApplyCustomGamma(Color: TColor32; GammaTable: TGammaTable8Bit): TColor32;
begin
  TColor32Entry(Result).R := GammaTable[TColor32Entry(Color).R];
  TColor32Entry(Result).G := GammaTable[TColor32Entry(Color).G];
  TColor32Entry(Result).B := GammaTable[TColor32Entry(Color).B];
end;


procedure ApplyGamma(Color: PColor32Array; Length: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Length - 1 do
  begin
    PColor32Entry(Color)^.R := GAMMA_ENCODING_TABLE[PColor32Entry(Color)^.R];
    PColor32Entry(Color)^.G := GAMMA_ENCODING_TABLE[PColor32Entry(Color)^.G];
    PColor32Entry(Color)^.B := GAMMA_ENCODING_TABLE[PColor32Entry(Color)^.B];
    Inc(Color);
  end;
end;

procedure ApplyInvGamma(Color: PColor32Array; Length: Integer);
var
  Index: Integer;
begin
  for Index := 0 to Length - 1 do
  begin
    PColor32Entry(Color)^.R := GAMMA_DECODING_TABLE[PColor32Entry(Color)^.R];
    PColor32Entry(Color)^.G := GAMMA_DECODING_TABLE[PColor32Entry(Color)^.G];
    PColor32Entry(Color)^.B := GAMMA_DECODING_TABLE[PColor32Entry(Color)^.B];
    Inc(Color);
  end;
end;

procedure ApplyCustomGamma(Color: PColor32Array; Length: Integer; GammaTable: TGammaTable8Bit);
var
  Index: Integer;
begin
  for Index := 0 to Length - 1 do
  begin
    PColor32Entry(Color)^.R := GammaTable[PColor32Entry(Color)^.R];
    PColor32Entry(Color)^.G := GammaTable[PColor32Entry(Color)^.G];
    PColor32Entry(Color)^.B := GammaTable[PColor32Entry(Color)^.B];
    Inc(Color);
  end;
end;


procedure ApplyGamma(Bitmap: TBitmap32);
begin
  ApplyGamma(Bitmap.Bits, Bitmap.Width * Bitmap.Height);
end;

procedure ApplyInvGamma(Bitmap: TBitmap32);
begin
  ApplyInvGamma(Bitmap.Bits, Bitmap.Width * Bitmap.Height);
end;

procedure ApplyCustomGamma(Bitmap: TBitmap32; GammaTable: TGammaTable8Bit);
begin
  ApplyCustomGamma(Bitmap.Bits, Bitmap.Width * Bitmap.Height, GammaTable);
end;

procedure ApplyCustomGamma(Bitmap: TBitmap32; Gamma: Double);
var
  GammaTable: TGammaTable8Bit;
begin
  if GAMMA_VALUE = Gamma then
    ApplyGamma(Bitmap.Bits, Bitmap.Width * Bitmap.Height)
  else
  begin
    SetGamma(Gamma, GammaTable);
    ApplyCustomGamma(Bitmap.Bits, Bitmap.Width * Bitmap.Height, GammaTable);
  end;
end;


{ Gamma / Pixel Shape Correction table }

procedure SetGamma;
begin
  SetGamma(DEFAULT_GAMMA);
end;

procedure SetGamma(Gamma: Double);
begin
  GAMMA_VALUE := Gamma;

  // calculate default gamma tables
  SetGamma(1 / Gamma, GAMMA_ENCODING_TABLE);
  SetGamma(Gamma, GAMMA_DECODING_TABLE);
end;

procedure SetGamma(Gamma: Double; var GammaTable: TGammaTable8Bit);
var
  i: Integer;
begin
  for i := 0 to $FF do
    GammaTable[i] := Round($FF * Power(i * COne255th, Gamma));
end;

procedure Set_sRGB;
begin
  Set_sRGB(GAMMA_ENCODING_TABLE);
  SetInv_sRGB(GAMMA_DECODING_TABLE);
end;

procedure Set_sRGB(var GammaTable: TGammaTable8Bit);
var
  i: Integer;
  Value: Double;
const
  CExp = 1 / 2.4;
begin
  for i := 0 to $FF do
  begin
    Value := i * COne255th;
    if (Value < 0.0031308) then
      GammaTable[i] := Round($FF * Value * 12.92)
    else
      GammaTable[i] := Round($FF * (1.055 * Power(Value, CExp) - 0.055));
  end;
end;

procedure SetInv_sRGB(var GammaTable: TGammaTable8Bit);
var
  i: Integer;
  Value: Double;
begin
  for i := 0 to $FF do
  begin
    Value := i * COne255th;
    if (Value < 0.004045) then
      GammaTable[i] := Round($FF * Value / 12.92)
    else
      GammaTable[i] := Round($FF * Power((Value + 0.055) / 1.055, 2.4));
  end;
end;

end.
