unit GR32_Gamma;

interface

{ Gamma bias for line/pixel antialiasing }

var
  GAMMA_TABLE: array [Byte] of Byte;

procedure SetGamma(Gamma: Single = 1.6);

implementation

uses
  Math, GR32;

{ Gamma / Pixel Shape Correction table }

procedure SetGamma(Gamma: Single);
var
  i: Integer;
begin
  for i := 0 to $FF do
    GAMMA_TABLE[i] := Round($FF * Power(i * COne255th, Gamma));
end;

end.

