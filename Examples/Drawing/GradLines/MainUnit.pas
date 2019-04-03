unit MainUnit;

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
 * The Original Code is Gradient Lines Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF} SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, 
  {$IFDEF COMPILERXE2_UP}Types, {$ENDIF}
  GR32, GR32_Blend, GR32_Image, GR32_LowLevel;

type
  TVector2f = record
    X, Y: Single;
  end;

  TLine = class
  public
    Bitmap: TBitmap32;
    P1, P2: TVector2f;     // positions
    V1, V2: TVector2f;     // velocities
    C1, C2, C3: TColor32;  // colors that define gradient pattern
    t1, t2, t3: Single;
    MaxVelocity: Single;
    constructor Create(ABitmap: TBitmap32);
    procedure Advance(DeltaT: Single);
    function GetLength: Single;
    procedure Paint;
  end;

  { TFormGradientLines }

  TFormGradientLines = class(TForm)
    BtnAddOne: TButton;
    BtnAddTen: TButton;
    BtnClear: TButton;
    LblTotal: TLabel;
    Memo: TMemo;
    PaintBox: TPaintBox32;
    PnlTotalLines: TPanel;
    RgpDraw: TRadioGroup;
    RgpFade: TRadioGroup;
    RepaintOpt: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RepaintOptClick(Sender: TObject);
    procedure BtnAddOneClick(Sender: TObject);
    procedure BtnAddTenClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure RgpFadeClick(Sender: TObject);
    procedure RgpDrawClick(Sender: TObject);
  protected
    Lines: array of TLine;
    P: TPoint; // mouse shift
    M: Boolean; // mouse down flag
    FadeCount: Integer;
    Pass: Integer;
    DrawPasses: Integer;
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
  public
    procedure AddLine;
    procedure AddLines(N: Integer);
  end;

var
  FormGradientLines: TFormGradientLines;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses Math;

function VectorAdd(const A, B: TVector2f): TVector2f;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function VectorSub(const A, B: TVector2f): TVector2f;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function VectorLen(const A: TVector2f): Single;
begin
  Result := SqRt(SqR(A.X) + SqR(A.Y));
end;

function VectorDot(const A, B: TVector2f): Single;
begin
  Result := A.X * B.X + A.Y * B.Y;
end;

function VectorScale(const A: TVector2f; Factor: Single): TVector2f;
begin
  Result.X := A.X * Factor;
  Result.Y := A.Y * Factor;
end;

{ TLine }

constructor TLine.Create(ABitmap: TBitmap32);
begin
  Bitmap := ABitmap;
  MaxVelocity := 1;
end;

procedure TLine.Advance(DeltaT: Single);
const
  COne400 : Single = 1 / 400;
  COne300 : Single = 1 / 300;

  procedure AdvancePoint(var P, V: TVector2f; t: Single);
  begin
    { apply velocities }
    P := VectorAdd(P, VectorScale(V, t));

    { reflect from walls }
    if P.X < 0 then
    begin
      P.X := 0;
      V.X := -V.X;
    end;
    if P.X >= FormGradientLines.PaintBox.Width then
    begin
      P.X := FormGradientLines.PaintBox.Width - 1;
      V.X := - V.X;
    end;
    if P.Y < 0 then
    begin
      P.Y := 0;
      V.Y := -V.Y;
    end;
    if P.Y >= FormGradientLines.PaintBox.Height then
    begin
      P.Y := FormGradientLines.PaintBox.Height - 1;
      V.Y := - V.Y
    end;

    { change velocity a little bit }
    V.X := V.X + t * (Random - 0.5) * 0.25;
    V.Y := V.Y + t * (Random - 0.5) * 0.25;

    { limit velocity }
    if VectorLen(V) > MaxVelocity then
      V := VectorScale(V, 1 / VectorLen(V));
  end;

begin
  AdvancePoint(P1, V1, DeltaT);
  AdvancePoint(P2, V2, DeltaT);

  C1 := HSLtoRGB(t1, Sin(t1 * 0.55) * 0.4 + 0.6, 0.5);
  C1 := SetAlpha(C1, Round(Sin(t1) * 25 + 50));
  t1 := t1 + Random * COne300;

  C2 := HSLtoRGB(t2, Sin(t2 * 0.55) * 0.4 + 0.6, 0.5);
  C2 := SetAlpha(C2, Round(Sin(t2) * 25 + 50));
  t2 := t2 + Random * COne400;

  C3 := HSLtoRGB(t3, Sin(t3 * 0.55) * 0.4 + 0.6, 0.5);
  C3 := SetAlpha(C3, Round(Sin(t3) * 25 + 50));
  t3 := t3 + Random * COne400;
end;

function TLine.GetLength: Single;
begin
  Result := VectorLen(VectorSub(P1, P2));
end;

procedure TLine.Paint;
var
  L: Single;
begin
  // this shows how to draw a gradient line
  L := GetLength;
  if L < 1 then Exit;
  Bitmap.SetStipple([C1, C2, C3]);
  Bitmap.StippleStep := 2 / L; {2 = 3 - 1 = Number of colors in a pattern - 1}
  Bitmap.StippleCounter := 0;
  Bitmap.LineFSP(P1.X, P1.Y, P2.X, P2.Y);
end;

{ TFormGradientLines }

procedure TFormGradientLines.FormCreate(Sender: TObject);
begin
  FadeCount := 0;
  DrawPasses := 2;
  Application.OnIdle := AppEventsIdle;
end;

procedure TFormGradientLines.AddLine;
var
  L: TLine;
begin
  SetLength(Lines, Length(Lines) + 1);
  L := TLine.Create(PaintBox.Buffer);
  Lines[High(Lines)] := L;
  L.t1 := Random * 3;
  L.t2 := Random * 3;
  L.t3 := Random * 3;
  L.P1.X := Random(PaintBox.Buffer.Width div 2 - 1);
  L.P2.X := Random(PaintBox.Buffer.Width div 2 - 1);
  L.P1.Y := Random(PaintBox.Buffer.Height div 2 - 1);
  L.P2.Y := Random(PaintBox.Buffer.Height div 2 - 1);
  PnlTotalLines.Caption := IntToStr(Length(Lines));
end;

procedure TFormGradientLines.AddLines(N: Integer);
var
  Index: Integer;
begin
  for Index := 0 to N - 1 do AddLine;
end;

procedure TFormGradientLines.AppEventsIdle(Sender: TObject; var Done: Boolean);
var
  I, J: Integer;
  P: PColor32;
begin
  for J := 0 to DrawPasses - 1 do
    for I := 0 to High(Lines) do
    begin
      Lines[I].Advance(1);
      Lines[I].Paint;
    end;

  if FadeCount > 0 then
  begin
    if Pass = 0 then with PaintBox.Buffer do
    begin
      P := @Bits[0];
      for I := 0 to Width * Height -1 do
      begin
        BlendMem($10000000, P^);
        Inc(P);
      end;
      EMMS;
    end;
    Dec(Pass);
    if (Pass < 0) or (Pass > FadeCount) then Pass := FadeCount;

    // we're doing unsafe operations above, so force a complete invalidation
    // so that wrong output of repaint optimizer doesn't show.
    PaintBox.ForceFullInvalidate;
  end
  else
    PaintBox.Invalidate;
end;

procedure TFormGradientLines.BtnAddOneClick(Sender: TObject);
begin
  AddLine;
end;

procedure TFormGradientLines.BtnAddTenClick(Sender: TObject);
begin
  AddLines(10);
end;

procedure TFormGradientLines.BtnClearClick(Sender: TObject);
var
  Index: Integer;
begin
  for Index := High(Lines) downto 0 do Lines[Index].Free;
  Lines := nil;
  PaintBox.Buffer.Clear;
  PnlTotalLines.Caption := '0';
end;
 
procedure TFormGradientLines.RgpFadeClick(Sender: TObject);
const
  FC: array [0..2] of Integer = (0, 7, 1);
begin
  FadeCount := FC[RgpFade.ItemIndex];
end;

procedure TFormGradientLines.RgpDrawClick(Sender: TObject);
begin
  DrawPasses := (RgpDraw.ItemIndex + 1) * 3 - 2;
end;

procedure TFormGradientLines.RepaintOptClick(Sender: TObject);
begin
  if RepaintOpt.Checked then
    PaintBox.RepaintMode := rmOptimizer
  else
    PaintBox.RepaintMode := rmFull;
end;

end.
