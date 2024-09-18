unit amEasing;

(*
 * Copyright © 2006 Anders Melander
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *)

// -----------------------------------------------------------------------------
//
//      Easing / Tweening animation
//
// -----------------------------------------------------------------------------
// See:
// - Robert Penner's Easing Functions
//   http://robertpenner.com/easing/
//
// - Motion, Tweening, and Easing
//   http://robertpenner.com/easing/penner_chapter7_tweening.pdf
//
// - Easing Functions Cheat Sheet
//   http://easings.net
// -----------------------------------------------------------------------------

// WEAKPACKAGEUNIT so we can include the unit in a design time package.
{$WEAKPACKAGEUNIT ON}

interface

type
  // Ease function prototype.
  // Value: [0..1]
  // Result: [0..1]
  TEaseFunc = function(Value: Double): Double;

  // Ease/Tween performer prototype.
  // Value: [0..1]
  TEasePerformer = reference to procedure(Value: Double; var Continue: boolean);


// Tween/Easing engine
procedure AnimatedTween(EaseFunc: TEaseFunc; Duration: integer; Performer: TEasePerformer; Throttle: integer = 0; InitialThrottle: boolean = False);

(*
        Example of usage:

        Move the position of a button using the Bounce/Elastic animation.

            AnimatedTween(EaseOutElastic, 2000,
              procedure(Value: Double; var Continue: boolean)
              begin
                // Move from Left=20 to Left=100
                Button1.Left := 20 + Trunc(Value * 80);
              end, 40);

        The animation will take 2 seconds.
        Each frame/step will take a minimum of 40 mS.
        The animation will perform up to 50 frames or steps (50 = 2000/40) with
        a maximum frame rate of 25 fps (25 = 1000/40)
*)

// -----------------------------------------------------------------------------
//
//      Easing functions
//
// -----------------------------------------------------------------------------
type
  TEaseLinear = class
  public
    class function Ease(Value: Double): Double; static;
  end;

  TEaseSine = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseCubic = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseQuadratic = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseQuartic = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseQuintic = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseCircular = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseElastic = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseExponential = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseBack = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseOut2(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

  TEaseBounce = class
  public
    class function EaseIn(Value: Double): Double; static;
    class function EaseOut(Value: Double): Double; static;
    class function EaseInOut(Value: Double): Double; static;
  end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

implementation

uses
  Math,
  Windows,
  System.Diagnostics;

// -----------------------------------------------------------------------------

class function TEaseLinear.Ease(Value: Double): Double;
begin
  Result := Value;
end;

// Modeled after quarter-cycle of sine wave
class function TEaseSine.EaseIn(Value: Double): Double;
begin
  Result := Sin((Value - 1) * Pi/2) + 1;
end;

// Modeled after quarter-cycle of sine wave (different phase)
class function TEaseSine.EaseOut(Value: Double): Double;
begin
  Result := Sin(Value * Pi/2);
end;

// Modeled after half sine wave
class function TEaseSine.EaseInOut(Value: Double): Double;
begin
  Result := 0.5 * (1 - Cos(Value * Pi));
end;

// Modeled after the parabola
// y = x^2
class function TEaseQuadratic.EaseIn(Value: Double): Double;
begin
  Result := Value * Value;
end;

// Modeled after the parabola
// y = -x^2 + 2x
class function TEaseQuadratic.EaseOut(Value: Double): Double;
begin
  Result := -(Value * (Value - 2));
end;

// Modeled after the piecewise quadratic
// y = (1/2)((2x)^2)             ; [0, 0.5)
// y = -(1/2)((2x-1)*(2x-3) - 1) ; [0.5, 1]
class function TEaseQuadratic.EaseInOut(Value: Double): Double;
begin
  if (Value < 0.5) then
    Result := 2 * Value * Value
  else
    Result := (-2 * Value * Value) + (4 * Value) - 1;
end;

// Modeled after the cubic
// y = x^3
class function TEaseCubic.EaseIn(Value: Double): Double;
begin
  Result := Value * Value * Value;
end;

// Modeled after the cubic
// y = (x - 1)^3 + 1
class function TEaseCubic.EaseOut(Value: Double): Double;
begin
  Value := Value - 1;
  Result := Value * Value * Value + 1;
end;

// Modeled after the piecewise cubic
// y = (1/2)((2x)^3)       ; [0, 0.5)
// y = (1/2)((2x-2)^3 + 2) ; [0.5, 1]
class function TEaseCubic.EaseInOut(Value: Double): Double;
begin
  if (Value < 0.5) then
    Result := 4 * Value * Value * Value
  else
  begin
    Value := (2 * Value) - 2;
    Result := 0.5 * Value * Value * Value + 1;
  end;
end;

// Modeled after the quartic
// y = x^4
class function TEaseQuartic.EaseIn(Value: Double): Double;
begin
  Result := Value * Value * Value * Value;
end;

// Modeled after the quartic
// y = 1 - (x - 1)^4
class function TEaseQuartic.EaseOut(Value: Double): Double;
begin
  Value := Value - 1;
  Result := 1 - (Value * Value * Value * Value);
end;

// Modeled after the piecewise quartic
// y = (1/2)((2x)^4)        ; [0, 0.5)
// y = -(1/2)((2x-2)^4 - 2) ; [0.5, 1]
class function TEaseQuartic.EaseInOut(Value: Double): Double;
begin
  if (Value < 0.5) then
    Result := 8 * Value * Value * Value * Value
  else
  begin
    Value := Value - 1;
    Result := -8 * Value * Value * Value * Value + 1;
  end;
end;

// Modeled after the quintic
// y = x^5
class function TEaseQuintic.EaseIn(Value: Double): Double;
begin
  Result := Value * Value * Value * Value * Value;
end;

// Modeled after the quintic
// y = (x - 1)^5 + 1
class function TEaseQuintic.EaseOut(Value: Double): Double;
begin
  Value := Value - 1;
  Result := Value * Value * Value * Value * Value + 1;
end;

// Modeled after the piecewise quintic
// y = (1/2)((2x)^5)       ; [0, 0.5)
// y = (1/2)((2x-2)^5 + 2) ; [0.5, 1]
class function TEaseQuintic.EaseInOut(Value: Double): Double;
begin
  if (Value < 0.5) then
    Result := 16 * Value * Value * Value * Value * Value
  else
  begin
    Value := 2 * Value - 2;
    Result := 0.5 * Value * Value * Value * Value * Value + 1;
  end;
end;

// Modeled after shifted quadrant IV of unit circle
class function TEaseCircular.EaseIn(Value: Double): Double;
begin
  Result := 1 - Sqrt(1 - Value * Value);
end;

// Modeled after shifted quadrant II of unit circle
class function TEaseCircular.EaseOut(Value: Double): Double;
begin
  Result := Sqrt((2 - Value) * Value);
end;

// Modeled after the piecewise circular function
// y = (1/2)(1 - sqrt(1 - 4x^2))           ; [0, 0.5)
// y = (1/2)(sqrt(-(2x - 3)*(2x - 1)) + 1) ; [0.5, 1]
class function TEaseCircular.EaseInOut(Value: Double): Double;
begin
  if (Value < 0.5) then
    Result := 0.5 * (1 - Sqrt(1 - 4 * Value * Value))
  else
    Result := 0.5 * (Sqrt(-(2 * Value - 3) * (2 * Value - 1)) + 1);
end;

// Modeled after the damped sine wave
// y = sin(13pi/2*x)*pow(2, 10 * (x - 1))
class function TEaseElastic.EaseIn(Value: Double): Double;
begin
  if (Value = 0) then
    Exit(0);

  if (Value = 1) then
    Exit(1);

  Result := Sin(13 * Pi/2 * Value) * Math.Power(2, 10 * (Value-1));
end;

// Modeled after the damped sine wave
// y = sin(-13pi/2*(x + 1))*pow(2, -10x) + 1
class function TEaseElastic.EaseOut(Value: Double): Double;
begin
  if (Value = 0) then
    Exit(0);

  if (Value = 1) then
    Exit(1);

  Result := Sin(-13 * Pi/2 * (Value + 1)) * Math.Power(2, -10 * Value) + 1;
end;

// Modeled after the piecewise exponentially-damped sine wave:
// y = (1/2)*sin(13pi/2*(2*x))*pow(2, 10 * ((2*x) - 1))      ; [0,0.5)
// y = (1/2)*(sin(-13pi/2*((2x-1)+1))*pow(2,-10(2*x-1)) + 2) ; [0.5, 1]
class function TEaseElastic.EaseInOut(Value: Double): Double;
begin
  if (Value = 0) then
    Exit(0);

  if (Value = 1) then
    Exit(1);

  if (Value < 0.5) then
    Result := 0.5 * Sin(13 * Pi * Value) * Math.Power(2, 10 * (2 * Value - 1))
  else
    Result := 0.5 * (Sin(-13 * Pi/2 * ((2 * Value - 1) + 1)) * Math.Power(2, -10 * (2 * Value - 1)) + 2);
end;

// Modeled after the exponential function
// y = 2^(10(x - 1))
class function TEaseExponential.EaseIn(Value: Double): Double;
begin
  if (Value = 0) then
    Result := 0
  else
    Result := Math.Power(2, 10 * (Value-1));
end;

// Modeled after the exponential function
// y = -2^(-10x) + 1
class function TEaseExponential.EaseOut(Value: Double): Double;
begin
  if (Value = 1) then
    Result := 1
  else
    Result := Math.Power(2, -10 * Value);
end;

// Modeled after the piecewise exponential
// y = (1/2)2^(10(2x - 1))         ; [0,0.5)
// y = -(1/2)*2^(-10(2x - 1))) + 1 ; [0.5,1]
class function TEaseExponential.EaseInOut(Value: Double): Double;
begin
  if (Value = 0) then
    Result := 0
  else
  if (Value = 1) then
    Result := 1
  else
  if (Value < 0.5) then
    Result := 0.5 * Math.Power(2, 20 * Value - 10)
  else
    Result := -0.5 * Math.Power(2, -20 * Value + 10) + 1;
end;

// Modeled after the overshooting cubic
// y = x^3-x*sin(x*pi)
class function TEaseBack.EaseIn(Value: Double): Double;
const
  s = 1.70158;
begin
  Result := Value * Value * ((s + 1) * Value - s);
end;

// Modeled after the overshooting cubic
// y = (1-x)^2*((x+1)*(1-x)+x)+1
class function TEaseBack.EaseOut(Value: Double): Double;
const
  s = 1.70158;
begin
  Value := Value - 1;
  Result := Value * Value * ((s + 1) * Value + s) + 1;
end;

// Modeled after the piecewise overshooting cubic function:
// y = (1/2)*((2x)^3-(2x)*sin(2*x*pi))           ; [0, 0.5)
// y = (1/2)*(1-((1-x)^3-(1-x)*sin((1-x)*pi))+1) ; [0.5, 1]
class function TEaseBack.EaseInOut(Value: Double): Double;
const
  s = 1.70158 *  1.525;
begin
  Value := Value * 2;
  if (Value < 1) then
    Result := 0.5 * (Value * Value * ((s + 1) * Value - s))
  else
  begin
    Value := Value - 2;
    Result := 0.5 * (Value * Value * ((s + 1) * Value + s) + 2);
  end;
end;

// Modeled after the overshooting cubic
// y = 1-((1-x)^3-(1-x)*sin((1-x)*pi))
// Overshoots a bit more than EaseOutBack
class function TEaseBack.EaseOut2(Value: Double): Double;
begin
  Value := 1 - Value;
  Result := 1 - (Value * Value * Value - Value * Sin(Value * Pi));
end;

class function TEaseBounce.EaseIn(Value: Double): Double;
begin
  Result := 1 - EaseOut(1 - Value);
end;

class function TEaseBounce.EaseOut(Value: Double): Double;
begin
  if (Value < 4/11) then
    Result := (121 * Value * Value) / 16
  else
  if (Value < 8/11) then
    Result := (363/40 * Value * Value) - (99/10 * Value) + 17/5
  else
  if (Value < 9/10) then
    Result := (4356/361 * Value * Value) - (35442/1805 * Value) + 16061/1805
  else
    Result := (54/5 * Value * Value) - (513/25 * Value) + 268/25;
end;

class function TEaseBounce.EaseInOut(Value: Double): Double;
begin
  if(Value < 0.5) then
    Result := 0.5 * EaseIn(Value * 2)
  else
    Result := 0.5 * EaseOut(Value * 2 - 1) + 0.5;
end;

// -----------------------------------------------------------------------------

procedure AnimatedTween(EaseFunc: TEaseFunc; Duration: integer; Performer: TEasePerformer; Throttle: integer; InitialThrottle: boolean);
var
  Stopwatch: TStopwatch;
  Elapsed: int64;
  Value: Double;
  RemainingThrottle: int64;
  Continue: boolean;
begin
  (*
  ** Performs time controlled tweening using an easing function.
  *)

  Stopwatch := TStopwatch.StartNew;
  Elapsed := 0;
  Value := 0;
  Continue := True;

  while (Continue) and (Elapsed <= Duration) do
  begin
    // Throttle
    if ((Elapsed <> 0) or (InitialThrottle)) and (Throttle <> 0) and (Elapsed < Duration) then
    begin
      // Make sure we don't wait too long
      RemainingThrottle := Duration - Stopwatch.ElapsedMilliseconds;
      if (RemainingThrottle > 0) then
        Sleep(Min(RemainingThrottle, Throttle));

      // Calculate time elapsed during throttle
      Elapsed := Stopwatch.ElapsedMilliseconds;
    end;

    if (Elapsed < Duration) then
      // Calculate tween value...
      Value := EaseFunc(Elapsed / Duration)
    else
      Value := 1;

    // ...and Ease
    Performer(Value, Continue);

    // Calculate time elapsed during Ease
    Elapsed := Stopwatch.ElapsedMilliseconds;
  end;

  // If we exited the loop prematurely because we ran out of time then
  // give the performer a final go so we can guarantee that we will
  // reach the goal.
  if (Continue) and (Value < 1) then
    Performer(1, Continue);
end;

// -----------------------------------------------------------------------------

end.
