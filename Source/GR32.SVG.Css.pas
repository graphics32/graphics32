unit GR32.SVG.Css;

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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Generics.Collections,
  GR32.SVG.Utf8,
  GR32.SVG.Tree;

//------------------------------------------------------------------------------
//
//      CSS Style Parser
//
//------------------------------------------------------------------------------
(*

  StyleSheet          = { Rule } ;

  Rule                = SelectorList , "{" , DeclarationBlock , "}" ;

  SelectorList        = Selector , { "," , Selector } ;

  Selector            = UniversalSelector
                      | ClassSelector
                      | IdSelector
                      | ElementSelector ;

  UniversalSelector   = [ "*" ] ;
  ClassSelector       = "." , Identifier ;
  IdSelector          = "#" , Identifier ;
  ElementSelector     = Identifier ;

  DeclarationBlock    = [ Declaration , { ";" , Declaration } , [ ";" ] ] ;

  Declaration         = PropertyName , ":" , PropertyValue ;

  PropertyName        = Identifier ;

  PropertyValue       = { Character - ( ";" | "}" ) } ;

  Identifier          = { Character - ( WhiteSpace | "," | "{" | "}" | ":" | ";" ) } ;

  Comment             = "/*" , { Character - "*" | "*" , ( Character - "/" ) } , "*/" ;

  WhiteSpace          = " " | "\t" | "\r" | "\n" ;

*)

type
  TSvgCssSelectorKind = (skUniversal, skElement, skClass, skId, skCompound);

//------------------------------------------------------------------------------
//
//      TSvgCssSelector
//
//------------------------------------------------------------------------------
// Represents a CSS "Selector" element
//------------------------------------------------------------------------------
type
  TSvgCssSelector = record
    Kind: TSvgCssSelectorKind;
    ElementTag: AnsiString;
    ClassName: AnsiString;
    Id: AnsiString;
    Name: AnsiString;
    Specificity: Integer;
    function Matches(const AElementTag: TValuePUtf8Char; const AClassName, AElementId: AnsiString): Boolean;
    class function Parse(Value: TValuePUtf8Char): TSvgCssSelector; static;
  end;


//------------------------------------------------------------------------------
//
//      TSvgCssProperty
//
//------------------------------------------------------------------------------
// Represents a CSS "Declaration" element
//------------------------------------------------------------------------------
type
  TSvgCssProperty = record
    Name: AnsiString;
    Value: AnsiString;
    class function Create(const AName, AValue: TValuePUtf8Char): TSvgCssProperty; static;
  end;


//------------------------------------------------------------------------------
//
//      TSvgCssRule
//
//------------------------------------------------------------------------------
// Represents a CSS "Rule" element
//------------------------------------------------------------------------------
type
  TSvgCssRule = record
    Selector: TSvgCssSelector;
    Properties: TArray<TSvgCssProperty>;
    procedure AddProperty(const AName, AValue: TValuePUtf8Char);
    procedure MakeUnique;
  end;


//------------------------------------------------------------------------------
//
//      TSvgCssStyleSheet
//
//------------------------------------------------------------------------------
// Represents a CSS "StyleSheet" element
//------------------------------------------------------------------------------
type
  TSvgCssStyleSheet = class(TObject)
  private
    FRules: TList<TSvgCssRule>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseCss(const ACssText: AnsiString);
    procedure ApplyToNode(ANode: TSvgNode; const AElementTag: TValuePUtf8Char; const AClassName, AElementId: AnsiString);

    property Rules: TList<TSvgCssRule> read FRules;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  AnsiStrings;


//------------------------------------------------------------------------------
//
//      MatchClass
//
//------------------------------------------------------------------------------
// Match a single class name against a white-space separated list of class names.
//------------------------------------------------------------------------------
function MatchClass(const AClassList, ATargetClass: AnsiString): Boolean;
var
  List: TValuePUtf8Char;
  OneClass: TValuePUtf8Char;
  Target: TValuePUtf8Char;
begin
  if (AClassList = '') or (ATargetClass = '') then
    Exit(False);

  List := TValuePUtf8Char.FromString(AClassList);
  List.Trim;
  if (List.Len = 0) then
    Exit(False);

  Target := TValuePUtf8Char.FromString(ATargetClass);
  Target.Trim;
  if (Target.Len = 0) then
    Exit(False);

  if List.CompareText(Target) then
    Exit(True);

  while (List.Len > 0) do
  begin
    OneClass := List.Split(sWhiteSpaceSeparators, True);
    if OneClass.CompareText(Target) then
      Exit(True);

    List.Trim;
  end;
  Result := False;
end;


//------------------------------------------------------------------------------
//
//      TSvgCssSelector
//
//------------------------------------------------------------------------------
class function TSvgCssSelector.Parse(Value: TValuePUtf8Char): TSvgCssSelector;
var
  Part: TValuePUtf8Char;
  Ch: AnsiChar;
begin
  Value.Trim;
  Value.TrimEnd;

  Result.Kind := skUniversal;
  Result.ElementTag := '';
  Result.ClassName := '';
  Result.Id := '';
  Result.Name := '';
  Result.Specificity := 0;

  if (Value.Len = 0) or ((Value.Len = 1) and (Value.Text^ = '*')) then
    Exit;

  if (Value.Text^ = '#') then
  begin
    Result.Kind := skId;
    SetString(Result.Id, Value.Text + 1, Value.Len - 1);
    Result.Name := Result.Id;
    Result.Specificity := 100;
  end else

  if (Value.Text^ = '.') then
  begin
    Result.Kind := skClass;
    SetString(Result.ClassName, Value.Text + 1, Value.Len - 1);
    Result.Name := Result.ClassName;
    Result.Specificity := 10;
  end else

  begin
    Part := Value.Split(['.', '#'], False);
    if (Part.Len > 0) then
    begin
      Result.ElementTag := AnsiStrings.LowerCase(Part.ToUtf8);
      Inc(Result.Specificity, 1);
    end;

    while (Value.Len > 0) do
    begin
      Ch := Value.Text^;
      Value.Skip(1);
      Part := Value.Split(['.', '#'], False);
      if (Ch = '.') then
      begin
        if (Result.ClassName <> '') then
          Result.ClassName := Result.ClassName + ' ' + Part.ToUtf8
        else
          Result.ClassName := Part.ToUtf8;
        Inc(Result.Specificity, 10);
      end else
      if (Ch = '#') then
      begin
        Result.Id := Part.ToUtf8;
        Inc(Result.Specificity, 100);
      end;
    end;

    if (Result.ClassName <> '') or (Result.Id <> '') then
    begin
      Result.Kind := skCompound;
      if (Result.Id <> '') then
        Result.Name := Result.Id
      else
        Result.Name := Result.ClassName;
    end else
    begin
      Result.Kind := skElement;
      Result.Name := Result.ElementTag;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TSvgCssSelector.Matches(const AElementTag: TValuePUtf8Char; const AClassName, AElementId: AnsiString): Boolean;
var
  Value: TValuePUtf8Char;
begin
  case Kind of
    skUniversal:
      Exit(True);

    skElement:
      begin
        Value := AElementTag;
        Value.Trim;
        Exit(Value.CompareText(Name));
      end;

    skClass:
      Exit(MatchClass(AClassName, Name));

    skId:
      begin
        Value := TValuePUtf8Char.FromString(AElementId);
        Value.Trim;
        Exit(Value.CompareText(Name));
      end;

    skCompound:
      begin
        if (ElementTag <> '') then
        begin
          Value := AElementTag;
          Value.Trim;
          if not Value.CompareText(ElementTag) then
            Exit(False);
        end;

        if (Id <> '') then
        begin
          Value := TValuePUtf8Char.FromString(AElementId);
          Value.Trim;
          if not Value.CompareText(Id) then
            Exit(False);
        end;

        if (ClassName <> '') then
        begin
          if not MatchClass(AClassName, ClassName) then
            Exit(False);
        end;

        Exit(True);
      end;
  else
    Result := False;
  end;
end;


//------------------------------------------------------------------------------
//
//      TSvgCssProperty
//
//------------------------------------------------------------------------------
class function TSvgCssProperty.Create(const AName, AValue: TValuePUtf8Char): TSvgCssProperty;
begin
  Result.Name := AName.ToUtf8;
  Result.Value := AValue.ToUtf8;
end;


//------------------------------------------------------------------------------
//
//      TSvgCssRule
//
//------------------------------------------------------------------------------
procedure TSvgCssRule.AddProperty(const AName, AValue: TValuePUtf8Char);
var
  Index: Integer;
begin
  Index := Length(Properties);
  SetLength(Properties, Index + 1);
  Properties[Index] := TSvgCssProperty.Create(AName, AValue);
end;

//------------------------------------------------------------------------------

procedure TSvgCssRule.MakeUnique;
begin
  Properties := Copy(Properties);
end;


//------------------------------------------------------------------------------
//
//      TSvgCssRule
//
//------------------------------------------------------------------------------
constructor TSvgCssStyleSheet.Create;
begin
  inherited Create;
  FRules := TList<TSvgCssRule>.Create;
end;

destructor TSvgCssStyleSheet.Destroy;
begin
  FRules.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TSvgCssStyleSheet.Clear;
begin
  FRules.Clear;
end;

//------------------------------------------------------------------------------

procedure TSvgCssStyleSheet.ParseCss(const ACssText: AnsiString);

  procedure SkipWhitespaceAndComments(var Value: TValuePUtf8Char);
  begin
    // Strip C-style comments: /* ... */

    while (Value.Len > 0) do
    begin
      Value.Trim;

      if (Value.Len >= 2) and (Value.Text^ = '/') and (Value.Text[1] = '*') then
      begin
        Value.Skip(2);

        while (Value.Len > 0) do
        begin
          if (Value.Len >= 2) and (Value.Text^ = '*') and (Value.Text[1] = '/') then
          begin
            Value.Skip(2);
            break;
          end;
          Value.Skip(1);
        end;
      end else
        break;
    end;
  end;

var
  Value: TValuePUtf8Char;
  SelectorList: TValuePUtf8Char;
  DeclarationBlock: TValuePUtf8Char;
  Selector: TValuePUtf8Char;
  Declaration: TValuePUtf8Char;
  PropertyName: TValuePUtf8Char;
  Rule: TSvgCssRule;
  Selectors: TArray<TSvgCssSelector>;
  Properties: TArray<TSvgCssProperty>;
  PropCount, SelCount, i: Integer;
begin
  Value := TValuePUtf8Char.FromString(ACssText);

  (*
  ** StyleSheet = { Rule } ;
  *)
  while True do
  begin
    SkipWhitespaceAndComments(Value);
    if (Value.Len = 0) then
      break;

    (*
    ** Rule = SelectorList , "{" , DeclarationBlock , "}" ;
    *)
    // Find SelectorList
    SelectorList := Value.Split('{', True);
    SkipWhitespaceAndComments(SelectorList);
    if (SelectorList.Len = 0) then
      break; // Missing '{'; No more SelectorLists; We're done

    // Isolate DeclarationBlock
    DeclarationBlock := Value.Split('}', True);

    (*
    ** SelectorList = Selector , { "," , Selector } ;
    *)
    SetLength(Selectors, 4);
    SelCount := 0;
    while (SelectorList.Len > 0) do
    begin
      SkipWhitespaceAndComments(SelectorList);
      if (SelectorList.Len = 0) then
        break;

      Selector := SelectorList.Split(',', True);
      Selector.Trim;
      Selector.TrimEnd;
      if (Selector.Len > 0) then
      begin
        if (SelCount >= Length(Selectors)) then
          SetLength(Selectors, Length(Selectors) * 2);
        Selectors[SelCount] := TSvgCssSelector.Parse(Selector);
        Inc(SelCount);
      end;
    end;

    if (SelCount = 0) then
      continue;

    (*
    ** DeclarationBlock = [ Declaration , { ";" , Declaration } , [ ";" ] ] ;
    *)
    SetLength(Properties, 4);
    PropCount := 0;
    while (DeclarationBlock.Len > 0) do
    begin
      SkipWhitespaceAndComments(DeclarationBlock);
      if (DeclarationBlock.Len = 0) then
        Break;

      Declaration := DeclarationBlock.Split(';', True);
      SkipWhitespaceAndComments(Declaration);
      Declaration.TrimEnd;
      if (Declaration.Len = 0) then
        Continue;

      (*
      ** Declaration = PropertyName , ":" , PropertyValue ;
      *)
      PropertyName := Declaration.Split(':', True);
      PropertyName.Trim;
      PropertyName.TrimEnd;

      Declaration.Trim;
      Declaration.TrimEnd;

      if (PropertyName.Len > 0) and (Declaration.Len > 0) then
      begin
        if (PropCount >= Length(Properties)) then
          SetLength(Properties, Length(Properties) * 2);
        Properties[PropCount] := TSvgCssProperty.Create(PropertyName, Declaration);
        Inc(PropCount);
      end;
    end;

    if (PropCount = 0) then
      Continue;

    SetLength(Properties, PropCount);

    for i := 0 to SelCount - 1 do
    begin
      Rule.Selector := Selectors[i];
      // Since rules are immutable once parsed, we can safely share the same
      // property array among the different rules.
      Rule.Properties := Properties;
      FRules.Add(Rule);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSvgCssStyleSheet.ApplyToNode(ANode: TSvgNode; const AElementTag: TValuePUtf8Char; const AClassName, AElementId: AnsiString);
// CSS Specificity values based on W3C CSS2 / SVG 1.1 specification:
//   0   = Universal selector (*)
//   1   = Type / Element tag selector (e.g. 'path', 'rect')
//   10  = Class selector (e.g. '.myclass')
//   100 = ID selector (e.g. '#myid')
//
// Evaluating matching rules in ascending order of specificity ensures that
// higher-specificity selectors override lower-specificity ones regardless of
// their position in the stylesheet.
// Within the same specificity level, stylesheet document order is preserved,
// so later rules override earlier ones of equal specificity.
var
  i, j: Integer;
  Rule: TSvgCssRule;
  Keyword: TSvgAttributeKeyword;
  MatchingRules: TList<TSvgCssRule>;
begin
  if (ANode = nil) or (FRules.Count = 0) then
    Exit;

  MatchingRules := TList<TSvgCssRule>.Create;
  try
    for Rule in FRules do
    begin
      if Rule.Selector.Matches(AElementTag, AClassName, AElementId) then
        MatchingRules.Add(Rule);
    end;

    if (MatchingRules.Count = 0) then
      Exit;

    // Stable insertion sort by Selector.Specificity ascending to ensure
    // higher specificity overrides lower, and equal specificity preserves
    // document order.
    for i := 1 to MatchingRules.Count - 1 do
    begin
      Rule := MatchingRules[i];
      j := i - 1;
      while (j >= 0) and (MatchingRules[j].Selector.Specificity > Rule.Selector.Specificity) do
      begin
        MatchingRules[j + 1] := MatchingRules[j];
        Dec(j);
      end;
      MatchingRules[j + 1] := Rule;
    end;

    for Rule in MatchingRules do
    begin
      for j := 0 to High(Rule.Properties) do
      begin
        Keyword := ANode.KeywordLookup(TValuePUtf8Char.FromString(Rule.Properties[j].Name));
        if (Keyword <> attrNone) then
          ANode.ParseAttribute(Keyword, TValuePUtf8Char.FromString(Rule.Properties[j].Value));
      end;
    end;
  finally
    MatchingRules.Free;
  end;
end;

end.
