unit GR32.Blend.Modes;

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
 * The Original Code is Blend Modes for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes,
  Generics.Collections,
  GR32,
  GR32_LowLevel; // Inlining


//------------------------------------------------------------------------------
//
// TGraphics32Blender
//
//------------------------------------------------------------------------------
type
  TCustomGraphics32Blender = class abstract
  private
    FData: NativeInt;
  protected
    class function GetID: string; virtual;
    class function GetName: string; virtual; abstract;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; overload; virtual; abstract;
    procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); overload; virtual;
    procedure BlendEx(F: TColor32; var B: TColor32; M: Cardinal); virtual; // M contains both F and B master alpha

    procedure GetPixelCombiner(out Func: TPixelCombineEvent); virtual;

    class function ID: string; inline;
    class function Name: string; inline;

    property Data: NativeInt read FData write FData; // User defined data; Unused by library.
  end;

  TGraphics32BlenderClass = class of TCustomGraphics32Blender;


//------------------------------------------------------------------------------
//
// TGraphics32ComponentBlender
//
//------------------------------------------------------------------------------
type
  TCustomGraphics32ComponentBlender = class abstract(TCustomGraphics32Blender)
  protected
    function BlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; virtual; abstract;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; overload; override;
  end;


type
  // The component blender implements the Adobe/Photoshop composition formula.
  // The blend function is assumed to be separable.
  TGraphics32ComponentBlender = class abstract(TCustomGraphics32ComponentBlender)
  protected
    class function DoBlendComponent(fColor, fAlpha, bColor, bAlpha, rAlpha, rfAlpha, Blended: Cardinal): Cardinal; static; inline;
    class function DoBlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal; Blended: TColor32): TColor32; static;
  end;

type
  // A separable blender applies the same blend function independently to all three
  // channels/components using the BlendComponent function.
  TGraphics32SeparableBlender = class abstract(TGraphics32ComponentBlender)
  protected
    function BlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32; override;
    class function BlendComponent(F, B: Cardinal): Cardinal; virtual; abstract;
  end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderNormal
//
//------------------------------------------------------------------------------
type
  TGraphics32BlenderNormal = class(TCustomGraphics32Blender)
  protected
    class function GetID: string; override;
    class function GetName: string; override;
  public
    function Blend(F: TColor32; B: TColor32): TColor32; override;
    procedure Blend(F: TColor32; var B: TColor32; M: Cardinal); override;
    procedure GetPixelCombiner(out Func: TPixelCombineEvent); override;
  end;

const
  cBlendNormal = '';

resourcestring
  sBlendNormal = 'Normal';


//------------------------------------------------------------------------------
//
// Graphics32BlendService
//
//------------------------------------------------------------------------------
type
  IGraphics32BlendEnumerator = interface
    ['{29D43084-46D2-4EC7-A382-D31C5BC025C1}']
    function GetCurrent: TGraphics32BlenderClass;
    property Current: TGraphics32BlenderClass read GetCurrent;
    function MoveNext: Boolean;
  end;

  IGraphics32BlendGroup = interface
    ['{971D7D2A-B1C6-4F50-A0FB-A236226C8C4F}']
//    procedure Register(BlenderClass: TGraphics32BlenderClass);
    function GetEnumerator: IGraphics32BlendEnumerator;

    function GetName: string;
    property Name: string read GetName;
  end;

  IGraphics32BlendGroupsEnumerator = interface
    ['{A08FCFF6-397D-4B2D-B31C-DE8D19F4F56C}']
    function GetCurrent: IGraphics32BlendGroup;
    property Current: IGraphics32BlendGroup read GetCurrent;
    function MoveNext: Boolean;
  end;

  IGraphics32BlendGroups = interface
    ['{CA5D5CE9-09EB-4790-A7DC-88DC73368CA8}']
    procedure Register(const GroupName: string; BlenderClass: TGraphics32BlenderClass);
    function GroupByName(const GroupName: string): IGraphics32BlendGroup;
    function GetEnumerator: IGraphics32BlendGroupsEnumerator;
  end;

  IGraphics32BlendService = interface
    ['{E0C6833B-CAED-41BA-9EE3-9832243650A4}']
    procedure Register(BlenderClass: TGraphics32BlenderClass; Groups: TArray<string> = []);
    function BlenderByID(const ID: string): TGraphics32BlenderClass;
    function GetEnumerator: IGraphics32BlendEnumerator;

    function GetGroups: IGraphics32BlendGroups;
    property Groups: IGraphics32BlendGroups read GetGroups;
  end;

function Graphics32BlendService: IGraphics32BlendService;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils,
  GR32_Blend,
  // Reference the actual blend mode units so we get them registered by default
  GR32.Blend.Modes.PhotoShop,
  GR32.Blend.Modes.PorterDuff;


//------------------------------------------------------------------------------
//
// Graphics32BlendService
//
//------------------------------------------------------------------------------
type
  TGraphics32BlendEnumerator = class(TInterfacedObject, IGraphics32BlendEnumerator)
  private
    FList: TArray<TGraphics32BlenderClass>;
    FIndex: integer;
  private
    // IGraphics32BlendEnumerator
    function GetCurrent: TGraphics32BlenderClass;
    function MoveNext: Boolean;
  public
    constructor Create(const AList: TArray<TGraphics32BlenderClass>);
  end;

//------------------------------------------------------------------------------

constructor TGraphics32BlendEnumerator.Create(const AList: TArray<TGraphics32BlenderClass>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TGraphics32BlendEnumerator.GetCurrent: TGraphics32BlenderClass;
begin
  Result := FList[FIndex];
end;

function TGraphics32BlendEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < High(FList));
  if Result then
    Inc(FIndex);
end;

//------------------------------------------------------------------------------

type
  TGraphics32BlendService = class(TInterfacedObject, IGraphics32BlendService, IGraphics32BlendGroups)
  private type
    TGraphics32BlendGroup = class(TInterfacedObject, IGraphics32BlendGroup)
    private
      FName: string;
      FMembers: TList<TGraphics32BlenderClass>;
    private
      // IGraphics32BlendGroup
      function GetEnumerator: IGraphics32BlendEnumerator;
      function GetName: string;
    public
      constructor Create(const AName: string);
      destructor Destroy; override;
    end;

  private type
    TGraphics32BlendGroupsEnumerator = class(TInterfacedObject, IGraphics32BlendGroupsEnumerator)
    private
      FList: TArray<IGraphics32BlendGroup>;
      FIndex: integer;
    private
      // IGraphics32BlendEnumerator
      function GetCurrent: IGraphics32BlendGroup;
      function MoveNext: Boolean;
    public
      constructor Create(const AList: TArray<IGraphics32BlendGroup>);
    end;

  private
    FBlenders: TDictionary<string, TGraphics32BlenderClass>;
    FGroups: TDictionary<string, IGraphics32BlendGroup>;

  private
    // IGraphics32BlendGroups
    procedure Groups_Register(const GroupName: string; BlenderClass: TGraphics32BlenderClass);
    function Groups_GroupByName(const GroupName: string): IGraphics32BlendGroup;
    function Groups_GetEnumerator: IGraphics32BlendGroupsEnumerator;

    procedure IGraphics32BlendGroups.Register = Groups_Register;
    function IGraphics32BlendGroups.GroupByName = Groups_GroupByName;
    function IGraphics32BlendGroups.GetEnumerator = Groups_GetEnumerator;

  private
    // IGraphics32BlendService
    procedure Register(BlenderClass: TGraphics32BlenderClass; AGroups: TArray<string> = []);
    function BlenderByID(const ID: string): TGraphics32BlenderClass;
    function GetGroups: IGraphics32BlendGroups;
    function GetEnumerator: IGraphics32BlendEnumerator;

  public
    constructor Create;
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------

constructor TGraphics32BlendService.Create;
begin
  inherited Create;
  FBlenders := TDictionary<string, TGraphics32BlenderClass>.Create;
  FGroups := TDictionary<string, IGraphics32BlendGroup>.Create;
end;

destructor TGraphics32BlendService.Destroy;
begin
  FGroups.Free;
  FBlenders.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TGraphics32BlendService.Register(BlenderClass: TGraphics32BlenderClass; AGroups: TArray<string> = []);
var
  GroupName: string;
begin
  FBlenders.AddOrSetValue(AnsiUpperCase(BlenderClass.GetID), BlenderClass);

  for GroupName in AGroups do
    Groups_Register(GroupName, BlenderClass);
end;

function TGraphics32BlendService.BlenderByID(const ID: string): TGraphics32BlenderClass;
begin
  if (not FBlenders.TryGetValue(AnsiUpperCase(ID), Result)) then
    Result := nil;
end;

function TGraphics32BlendService.GetEnumerator: IGraphics32BlendEnumerator;
begin
  Result := TGraphics32BlendEnumerator.Create(FBlenders.Values.ToArray);
end;

function TGraphics32BlendService.GetGroups: IGraphics32BlendGroups;
begin
  Result := Self;
end;

//------------------------------------------------------------------------------

function TGraphics32BlendService.Groups_GetEnumerator: IGraphics32BlendGroupsEnumerator;
begin
  Result := TGraphics32BlendGroupsEnumerator.Create(FGroups.Values.ToArray);
end;

function TGraphics32BlendService.Groups_GroupByName(const GroupName: string): IGraphics32BlendGroup;
begin
  if (not FGroups.TryGetValue(AnsiUpperCase(GroupName), Result)) then
    Result := nil;
end;

procedure TGraphics32BlendService.Groups_Register(const GroupName: string; BlenderClass: TGraphics32BlenderClass);
var
  Group: IGraphics32BlendGroup;
begin
  if (not FGroups.TryGetValue(AnsiUpperCase(GroupName), Group)) then
  begin
    Group := TGraphics32BlendGroup.Create(GroupName);
    FGroups.Add(AnsiUpperCase(GroupName), Group);
  end;

  TGraphics32BlendGroup(Group).FMembers.Add(BlenderClass);
end;

//------------------------------------------------------------------------------

constructor TGraphics32BlendService.TGraphics32BlendGroupsEnumerator.Create(const AList: TArray<IGraphics32BlendGroup>);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TGraphics32BlendService.TGraphics32BlendGroupsEnumerator.GetCurrent: IGraphics32BlendGroup;
begin
  Result := FList[FIndex];
end;

function TGraphics32BlendService.TGraphics32BlendGroupsEnumerator.MoveNext: Boolean;
begin
  Result := (FIndex < High(FList));
  if Result then
    Inc(FIndex);
end;

//------------------------------------------------------------------------------

constructor TGraphics32BlendService.TGraphics32BlendGroup.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FMembers := TList<TGraphics32BlenderClass>.Create;
end;

destructor TGraphics32BlendService.TGraphics32BlendGroup.Destroy;
begin
  FMembers.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TGraphics32BlendService.TGraphics32BlendGroup.GetEnumerator: IGraphics32BlendEnumerator;
begin
  Result := TGraphics32BlendEnumerator.Create(FMembers.ToArray);
end;

function TGraphics32BlendService.TGraphics32BlendGroup.GetName: string;
begin
  Result := FName;
end;

//------------------------------------------------------------------------------

var
  FGraphics32BlendService: IGraphics32BlendService;

function Graphics32BlendService: IGraphics32BlendService;
begin
  if (FGraphics32BlendService = nil) then
    FGraphics32BlendService := TGraphics32BlendService.Create;

  Result := FGraphics32BlendService;
end;


//------------------------------------------------------------------------------
//
// TGraphics32Blender
//
//------------------------------------------------------------------------------
procedure TCustomGraphics32Blender.GetPixelCombiner(out Func: TPixelCombineEvent);
begin
  Func := Blend;
end;

//------------------------------------------------------------------------------

procedure TCustomGraphics32Blender.Blend(F: TColor32; var B: TColor32; M: Cardinal);
begin
  M := (M and $FF); // Foreground master alpha

  // Modulate foreground alpha
  if (M <> 255) then
{$if defined(BLEND_USE_TABLES)}
    TColor32Entry(F).A := MulDiv255Table[TColor32Entry(F).A, M];
{$else}
    TColor32Entry(F).A := Div255(TColor32Entry(F).A * M);
{$ifend}

  // Blend
  B := Blend(F, B);
end;

//------------------------------------------------------------------------------

procedure TCustomGraphics32Blender.BlendEx(F: TColor32; var B: TColor32; M: Cardinal);
var
  fM, bM: Cardinal;
begin
  (*
  ** Note:
  ** Both the Source and Destination MasterAlpha has been packed into the M
  ** parameter.
  ** The lower  8 bits contains the Foreground/Source master alpha, the next 8 bits the
  ** Background/Destination master alpha.
  *)
  fM := (M and $FF); // Foreground master alpha

  // Modulate foreground alpha
  if (fM <> 255) then
{$if defined(BLEND_USE_TABLES)}
    TColor32Entry(F).A := MulDiv255Table[TColor32Entry(F).A, fM];
{$else}
    TColor32Entry(F).A := Div255(TColor32Entry(F).A * fM);
{$ifend}

  bM := (M shr 8) and $FF; // Background master alpha

  // Modulate background alpha
  if (bM <> 255) then
{$if defined(BLEND_USE_TABLES)}
    TColor32Entry(B).A := MulDiv255Table[TColor32Entry(B).A, bM];
{$else}
    TColor32Entry(B).A := Div255(TColor32Entry(B).A * bM);
{$ifend}

  // Blend
  B := Blend(F, B);
end;

//------------------------------------------------------------------------------

class function TCustomGraphics32Blender.GetID: string;
begin
  Result := GetName;
end;

class function TCustomGraphics32Blender.ID: string;
begin
  Result := GetID;
end;

class function TCustomGraphics32Blender.Name: string;
begin
  Result := GetName;
end;


//------------------------------------------------------------------------------
//
// TCustomGraphics32ComponentBlender
//
//------------------------------------------------------------------------------
function TCustomGraphics32ComponentBlender.Blend(F, B: TColor32): TColor32;
begin
  Result := BlendComponents(F and $00FFFFFF, TColor32Entry(F).A, B and $00FFFFFF, TColor32Entry(B).A);
end;


//------------------------------------------------------------------------------
//
// TGraphics32ComponentBlender
//
//------------------------------------------------------------------------------
class function TGraphics32ComponentBlender.DoBlendComponent(fColor, fAlpha, bColor, bAlpha, rAlpha, rfAlpha, Blended: Cardinal): Cardinal;
begin
  (*
      This method implements the Adobe composition formula as used in Photoshop, Acrobat et al.

      TLDR;

        rAlpha := fAlpha + bAlpha * (1 - fAlpha);
        rColor := (1 - fAlpha / rAlpha) * bColor + (fAlpha / rAlpha) * ((1 - bAlpha) * fColor + bAlpha * Blend(fColor, bColor));

      The additional parameters are supplied by the caller to avoid calculating them here:

        rAlpha   = fAlpha + (bAlpha * (1 - fAlpha))
        rfAlpha  = fAlpha / rAlpha
        Blended  = Blend(fColor, bColor)

      Reference: https://www.w3.org/TR/compositing-1/#blending

      ------------------------------------------------------------------------------

      The algorithm was reverse engineered independently by [1] Anders melander
      and [2] Michael Hansen & Mattias Andersson, long before it became public.
      The following is from the Graphics32 newsgroup where the discovery of the
      algorithm was published (NNTP Message-ID: <ialvqd$if5$1@news.graphics32.org>).

      ------------------------------------------------------------------------------

      [1] The compositing formula used by Photoshop is [drumroll]:

          rAlpa := fAlpha + bAlpha * (1 - fAlpha);

          rColor := (1 - fAlpha / rAlpha) * bColor + (fAlpha / rAlpha) * ((1 - bAlpha) * fColor + bAlpha * Blend(fColor, bColor));

        where

          fColor          Foreground color
          fAlpha          Foreground alpha
          bColor          Background color
          bAlpha          Background alpha
          rAlpha          Result alpha
          rColor          Result color
          Blend()         The blending function (Normal, Multiply, Screen, Dodge, etc)

        So, according to the above formula, the blending code should be:

          rA := fA + bA - ba * fa div 255;
          rR := bR - bR * fA div rA + (fR - fR * bA div 255 + BlendR * bA div 255) * fA div rA;
          rG := bG - bG * fA div rA + (fG - fG * bA div 255 + BlendG * bA div 255) * fA div rA;
          rB := bB - bB * fA div rA + (fB - fB * bA div 255 + BlendB * bA div 255) * fA div rA;

        After Optimization:

          rA := fA + bA - ba * fa div 255;
          rR := bR - bR * fA div rA + (fR - (fR - BlendR) * bA div 255) * fA div rA;
          rG := bG - bG * fA div rA + (fG - (fG - BlendG) * bA div 255) * fA div rA;
          rB := bB - bB * fA div rA + (fB - (fB - BlendB) * bA div 255) * fA div rA;

      ------------------------------------------------------------------------------

      [2] A correct alpha compositing solution should use the merge formula to blend
          the RGB result of the blendmode with the background ARGB, using foreground
          alpha (eventually multiplied with masteralpha variable):

          F: Foreground
          B: Background
          C: Combined
          O: Output

          Crgb = blendmode(Frgb, Brgb)

          optional master alpha:
          Fa = Fa * Ma

          Oa = Fa + Ba * (1 - Fa)
          Orgb = (Crgb * Fa + (Brgb * Ba) * (1 - Fa)) / Oa

          Oa = Fa + Ba * (1 - Fa)
          Crgb = Frgb + (blendmode(Frgb, Brgb) - Frgb) * Ba
          Orgb = (Crgb * Fa + (Brgb * Ba) * (1 - Fa)) / Oa

      ------------------------------------------------------------------------------

      [1] As far as I can tell the foreground & blend terms are exactly the same (once
          expanded), but at first look the background term appears different:

        1) Foreground/blend term

        1.1) Mine:

          (fAlpha/rAlpha)(fColor*(1 - bAlpha) + bAlpha*Blend)           [1a]


        1.2) Yours:

          Fa*(Frgb + Ba*(Blend - Frgb))/Oa =                            [2a]

          (Fa/Oa)(Frgb + Ba*Blend - Ba*Frgb) =                          [2b]

          (Fa/Oa)(Frgb*(1 - Ba) + Ba*Blend) =                           [2c]

          (fAlpha/rAlpha)(fColor*(1 - bAlpha) + bAlpha*Blend)           [2d]

          [1a] = [2d] => Our foreground and blend terms are identical.


        2) Background term

        2.1) Mine:

          bColor*(1 - fAlpha/rAlpha)                                    [3a]

          Expanding rAlpha we get:

          bColor*(1 - fAlpha/(fAlpha + bAlpha*(1 - fAlpha))) =          [3b]

        (handwave)

          bColor*bAlpha*(fAlpha-1)/(bAlpha*(fAlpha-1)-fAlpha)           [3c]

        2.2) Yours:

          (Brgb*Ba)(1 - Fa)/Oa =                                        [4a]

          Brgb*(1 - Fa)(Ba/Oa) =                                        [4b]

          bColor*(1 - fAlpha)(bAlpha/rAlpha)                            [4c]

        Expanding rAlpha we get:

          bColor*(1 - fAlpha)(bAlpha/(fAlpha + bAlpha*(1 - fAlpha))) =  [4d]

        (handwave)

          bColor*bAlpha*(fAlpha-1)/(bAlpha*(fAlpha-1)-fAlpha))          [4e]

        [3c] = [4e] => Our background terms are identical.

        So both our compositing formulas are in fact identical.


        3) Isolating the terms we can verify that the composition formula is correct:

        Alpha:
          rAlpha := fAlpha + bAlpha*(1 - fAlpha)                        [5a]

        Foreground term:
          nS := (fAlpha/rAlpha)(fColor*(1 - bAlpha))                    [5b]

        Background term:
          nD := bColor*(1 - fAlpha/rAlpha)                              [5c]

        Blend term:
          nB := (fAlpha/rAlpha)(bAlpha*Blend)                           [5d]

        Result:
          rColor := nS + nD + nB;                                       [5e]

        3.1) For Normal blend mode, Blend(fColor, bColor) = fColor, the blend term
             reduces to:

          nB := (fAlpha/rAlpha)(bAlpha*fColor)                          [5f]

        3.1a) Solving for (fAlpha = bAlpha = 1) we get:

          rAlpha = 1 + 1*(1-1) = 1
          nS = (1/1)(fColor*(1-1)) = 0
          nD = bColor*(1-1/1) = 0
          nB = (1/1)(1*fColor) = fColor
          rColor = 0+0+fColor = fColor

                Correct.

        3.1b) Solving for (fAlpha = 1, bAlpha = 0) we get:

          rAlpha = 1 + 0*(1-1) = 1
          nS = (1/1)(fColor*(1-0)) = fColor
          nD = bColor*(1-1/1) = 0
          nB = (1/1)(0*fColor) = 0
          rColor = 0+fColor+0 = fColor

                Correct.

        3.1c) Solving for (fAlpha = 0, bAlpha = 1) we get:

          rAlpha = 0 + 1*(1-0) = 1
          nS = (0/1)(fColor*(1-1)) = 0
          nD = bColor*(1-0/1) = bColor
          nB = (0/1)(1*fColor) = 0
          rColor = 0+bColor+0 = 0

                Correct.

        3.1d) Solving for (fAlpha = 0.5, bAlpha = 0.5) we get:

          rAlpha = 0.5 + 0.5*(1-0.5) = 0.75
          nS = (0.5/0.75)(fColor*(1-0.5)) = fColor/3
          nD = bColor*(1-0.5/0.75) = bColor/3
          nB = (0.5/0.75)(0.5*fColor) = fColor/3
          rColor = fColor/3+bColor/3+fColor/3 = 2/3*fColor + 1/3*bColor

                Correct.

        3.1e) Solving for (fAlpha = 0.5, bAlpha = 1) we get:

          rAlpha = 0.5 + 1*(1-0.5) = 1
          nS = (0.5/1)(fColor*(1-1)) = 0
          nD = bColor*(1-0.5/1) = bColor/2
          nB = (0.5/1)(1*fColor) = fColor/2
          rColor = 0+bColor/2+fColor/2 = 1/2*fColor + 1/2*bColor

                Correct.

        3.2) For other blend modes, substitute Blend in [5d] with the blend result,
             rinse and repeat.

      ------------------------------------------------------------------------------
  *)

  Result := Div255((255 - rfAlpha) * bColor + rfAlpha * Div255((255 - bAlpha) * fColor + bAlpha * Blended));
end;

//------------------------------------------------------------------------------

class function TGraphics32ComponentBlender.DoBlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal; Blended: TColor32): TColor32;
var
  rAlpha, rfAlpha: Cardinal;
  f, b, n: Cardinal;
  rR, rG, rB: Cardinal;
begin
  // rAlpha = fAlpha + (bAlpha * (1- fAlpha))
{$if defined(BLEND_USE_TABLES)}
  rfAlpha := fAlpha + MulDiv255Table[bAlpha, 255 - fAlpha];
{$else}
  rAlpha := fAlpha + Div255(bAlpha * (255 - fAlpha));
{$ifend}

  if (rAlpha = 0) then
    Exit(0);

  // rfAlpha  = fAlpha / rAlpha
{$if defined(BLEND_USE_TABLES)}
  rfAlpha := DivMul255Table[rAlpha, fAlpha];
{$else}
  rfAlpha := 255 * fAlpha div rAlpha;
{$ifend}

  f := fColor  shr 16 and $FF;
  b := bColor  shr 16 and $FF;
  n := Blended shr 16 and $FF;
  rR := DoBlendComponent(f, fAlpha, b, bAlpha, rAlpha, rfAlpha, n);

  f := fColor  shr  8 and $FF;
  b := bColor  shr  8 and $FF;
  n := Blended shr  8 and $FF;
  rG := DoBlendComponent(f, fAlpha, b, bAlpha, rAlpha, rfAlpha, n);

  f := fColor         and $FF;
  b := bColor         and $FF;
  n := Blended        and $FF;
  rB := DoBlendComponent(f, fAlpha, b, bAlpha, rAlpha, rfAlpha, n);

  Result := (rAlpha shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;


//------------------------------------------------------------------------------
//
// TGraphics32SeparableBlender
//
//------------------------------------------------------------------------------
function TGraphics32SeparableBlender.BlendComponents(fColor: TColor32; fAlpha: Cardinal; bColor: TColor32; bAlpha: Cardinal): TColor32;
var
  rAlpha, rfAlpha: Cardinal;
  f, b, n: Cardinal;
  rR, rG, rB: Cardinal;
begin
  // rAlpha = fAlpha + (bAlpha * (1- fAlpha))
{$if defined(BLEND_USE_TABLES)}
  rfAlpha := fAlpha + MulDiv255Table[bAlpha, 255 - fAlpha];
{$else}
  rAlpha := fAlpha + Div255(bAlpha * (255 - fAlpha));
{$ifend}

  if (rAlpha = 0) then
    Exit(0);

  // rfAlpha  = fAlpha / rAlpha
{$if defined(BLEND_USE_TABLES)}
  rfAlpha := DivMul255Table[rAlpha, fAlpha];
{$else}
  rfAlpha := 255 * fAlpha div rAlpha;
{$ifend}

  f := fColor shr 16 and $FF;
  b := bColor shr 16 and $FF;
  n := BlendComponent(f, b);
  rR := DoBlendComponent(f, fAlpha, b, bAlpha, rAlpha, rfAlpha, n);

  f := fColor shr  8 and $FF;
  b := bColor shr  8 and $FF;
  n := BlendComponent(f, b);
  rG := DoBlendComponent(f, fAlpha, b, bAlpha, rAlpha, rfAlpha, n);

  f := fColor        and $FF;
  b := bColor        and $FF;
  n := BlendComponent(f, b);
  rB := DoBlendComponent(f, fAlpha, b, bAlpha, rAlpha, rfAlpha, n);

  Result := (rAlpha shl 24) or (rR shl 16) or (rG shl 8) or rB;
end;


//------------------------------------------------------------------------------
//
// TGraphics32BlenderNormal
//
//------------------------------------------------------------------------------
procedure TGraphics32BlenderNormal.GetPixelCombiner(out Func: TPixelCombineEvent);
begin
  Func := nil; // Signal caller to use dmBlend & cmMerge
end;

function TGraphics32BlenderNormal.Blend(F, B: TColor32): TColor32;
begin
  Result := MergeReg(F, B);
end;

procedure TGraphics32BlenderNormal.Blend(F: TColor32; var B: TColor32; M: Cardinal);
begin
  MergeRegEx(F, B, M);
end;

class function TGraphics32BlenderNormal.GetID: string;
begin
  Result := cBlendNormal; // Make this the default blend mode
end;

class function TGraphics32BlenderNormal.GetName: string;
begin
  Result := sBlendNormal;
end;


//------------------------------------------------------------------------------

initialization
  Graphics32BlendService.Register(TGraphics32BlenderNormal);

finalization
  FGraphics32BlendService := nil;
end.
