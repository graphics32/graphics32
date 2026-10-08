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
  TSvgCssCombinator = (coNone, coDescendant, coChild);

  TSvgCssSelectorComponent = record
    Kind: TSvgCssSelectorKind;
    ElementTag: UTF8String;
    ClassName: UTF8String;
    Id: UTF8String;
    Name: UTF8String;
    Specificity: Integer;
    Combinator: TSvgCssCombinator;
    function MatchesSimple(const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String): Boolean; overload;
    function MatchesSimple(ANode: TSvgNode): Boolean; overload;
  end;

//------------------------------------------------------------------------------
//
//      TSvgCssSelector
//
//------------------------------------------------------------------------------
// Represents a CSS "Selector" element
//------------------------------------------------------------------------------
type
  TSvgCssSelector = record
    Chain: TArray<TSvgCssSelectorComponent>;
    Specificity: Integer;
    function Matches(ANode: TSvgNode): Boolean; overload;
    function Matches(ANode: TSvgNode; const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String): Boolean; overload;
    function Matches(const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String): Boolean; overload;
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
    Name: UTF8String;
    Value: UTF8String;
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
  private type
    TSvgCssRuleList = TList<TSvgCssRule>;
  private
    FRules: TSvgCssRuleList;
    function GetIsEmpty: Boolean; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseCss(const ACssText: UTF8String);
    procedure ApplyToNode(ANode: TSvgNode; const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String);

    // Note: Rules might be nil if there are no rules
    property Rules: TSvgCssRuleList read FRules;
    property IsEmpty: Boolean read GetIsEmpty;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//
//      MatchClass
//
//------------------------------------------------------------------------------
// Match a single class name against a white-space separated list of class names.
//------------------------------------------------------------------------------
function MatchClass(const AClassList, ATargetClass: UTF8String): Boolean;
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
function TSvgCssSelectorComponent.MatchesSimple(const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String): Boolean;
var
  TagVal: TValuePUtf8Char;
begin
  case Kind of
    skUniversal:
      Exit(True);

    skElement:
      begin
        TagVal := AElementTag;
        TagVal.Trim;
        Exit(TagVal.CompareText(Name));
      end;

    skClass:
      Exit(MatchClass(AClassName, Name));

    skId:
      begin
        TagVal := TValuePUtf8Char.FromString(AElementId);
        TagVal.Trim;
        Exit(TagVal.CompareText(Name));
      end;

    skCompound:
      begin
        if (ElementTag <> '') then
        begin
          TagVal := AElementTag;
          TagVal.Trim;
          if not TagVal.CompareText(ElementTag) then
            Exit(False);
        end;

        if (Id <> '') then
        begin
          TagVal := TValuePUtf8Char.FromString(AElementId);
          TagVal.Trim;
          if not TagVal.CompareText(Id) then
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

function TSvgCssSelectorComponent.MatchesSimple(ANode: TSvgNode): Boolean;
begin
  if ANode = nil then
    Exit(False);
  Result := MatchesSimple(ANode.ElementTag, ANode.CssClassName, ANode.ID);
end;

//------------------------------------------------------------------------------

class function TSvgCssSelector.Parse(Value: TValuePUtf8Char): TSvgCssSelector;

  procedure ParseComponent(CompStr: TValuePUtf8Char; var Comp: TSvgCssSelectorComponent);
  var
    Part: TValuePUtf8Char;
    Ch: AnsiChar;
  begin
    CompStr.Trim;
    CompStr.TrimEnd;

    Comp.Kind := skUniversal;
    Comp.ElementTag := '';
    Comp.ClassName := '';
    Comp.Id := '';
    Comp.Name := '';
    Comp.Specificity := 0;

    if (CompStr.Len = 0) or ((CompStr.Len = 1) and (CompStr.Text^ = '*')) then
      Exit;

    if (CompStr.Text^ = '#') then
    begin
      Comp.Kind := skId;
      SetString(Comp.Id, CompStr.Text + 1, CompStr.Len - 1);
      Comp.Name := Comp.Id;
      Comp.Specificity := 100;
    end else

    if (CompStr.Text^ = '.') then
    begin
      Comp.Kind := skClass;
      SetString(Comp.ClassName, CompStr.Text + 1, CompStr.Len - 1);
      Comp.Name := Comp.ClassName;
      Comp.Specificity := 10;
    end else

    begin
      Part := CompStr.Split(['.', '#'], False);
      if (Part.Len > 0) then
      begin
        Comp.ElementTag := UTF8Tools.Lowercase(Part.ToUtf8);
        Inc(Comp.Specificity, 1);
      end;

      while (CompStr.Len > 0) do
      begin
        Ch := CompStr.Text^;
        CompStr.Skip(1);
        Part := CompStr.Split(['.', '#'], False);
        if (Ch = '.') then
        begin
          if (Comp.ClassName <> '') then
            Comp.ClassName := Comp.ClassName + ' ' + Part.ToUtf8
          else
            Comp.ClassName := Part.ToUtf8;
          Inc(Comp.Specificity, 10);
        end else
        if (Ch = '#') then
        begin
          Comp.Id := Part.ToUtf8;
          Inc(Comp.Specificity, 100);
        end;
      end;

      if (Comp.ClassName <> '') or (Comp.Id <> '') then
      begin
        Comp.Kind := skCompound;
        if (Comp.Id <> '') then
          Comp.Name := Comp.Id
        else
          Comp.Name := Comp.ClassName;
      end else
      begin
        Comp.Kind := skElement;
        Comp.Name := Comp.ElementTag;
      end;
    end;
  end;

var
  Token: TValuePUtf8Char;
  Comp: TSvgCssSelectorComponent;
  NextCombinator, CurrentCombinator: TSvgCssCombinator;
  Count: Integer;
  pStart: PUtf8Char;
begin
  Value.Trim;
  Value.TrimEnd;

  Result.Specificity := 0;

  if Value.Len = 0 then
    Exit;

  CurrentCombinator := coNone;
  Count := 0;

  while (Value.Len > 0) do
  begin
    Value.Trim;
    if Value.Len = 0 then
      Break;

    pStart := Value.Text;
    Value.SkipUntil([#9, #10, #13, ' ', '>'], False);

    Token.Text := pStart;
    Token.Len := PtrInt(Value.Text - pStart);

    NextCombinator := coDescendant;
    Value.Trim;
    if (Value.Len > 0) and (Value.Text^ = '>') then
    begin
      NextCombinator := coChild;
      Value.Skip;
      Value.Trim;
    end;

    if (Token.Len > 0) then
    begin
      ParseComponent(Token, Comp);
      Comp.Combinator := CurrentCombinator;

      SetLength(Result.Chain, Count + 1); // Assume there are ony one or two; Not worth it oversizing
      Result.Chain[Count] := Comp;
      Inc(Count);
      Inc(Result.Specificity, Comp.Specificity);
    end;

    CurrentCombinator := NextCombinator;
  end;
end;

//------------------------------------------------------------------------------

function TSvgCssSelector.Matches(ANode: TSvgNode): Boolean;
var
  DummyTag: TValuePUtf8Char;
begin
  if ANode = nil then
    Exit(False);
  DummyTag := TValuePUtf8Char.FromString(ANode.ElementTag);
  Result := Matches(ANode, DummyTag, ANode.CssClassName, ANode.ID);
end;

function TSvgCssSelector.Matches(ANode: TSvgNode; const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String): Boolean;

  function MatchChain(Node: TSvgNode; Index: Integer): Boolean;
  var
    p: TSvgNode;
    IsTarget: Boolean;
    MatchTag: TValuePUtf8Char;
    MatchClass, MatchID: UTF8String;
  begin
    if (Index < 0) then
      Exit(True);

    if (Node = nil) then
      Exit(False);

    IsTarget := (Node = ANode) and (Index = High(Chain));

    if IsTarget then
    begin
      if (AElementTag.Len > 0) then
        MatchTag := AElementTag
      else
        MatchTag := TValuePUtf8Char.FromString(Node.ElementTag);
      MatchClass := AClassName;
      MatchID := AElementId;
    end else
    begin
      MatchTag := TValuePUtf8Char.FromString(Node.ElementTag);
      MatchClass := Node.CssClassName;
      MatchID := Node.ID;
    end;

    if not Chain[Index].MatchesSimple(MatchTag, MatchClass, MatchID) then
      Exit(False);

    if Index = 0 then
      Exit(True);

    case Chain[Index].Combinator of
      coChild:
        Exit(MatchChain(Node.Parent, Index - 1));

      coDescendant:
        begin
          p := Node.Parent;
          while p <> nil do
          begin
            if MatchChain(p, Index - 1) then
              Exit(True);
            p := p.Parent;
          end;
          Exit(False);
        end;
    else
      Exit(False);
    end;
  end;

begin
  if (ANode = nil) or (Length(Chain) = 0) then
    Exit(False);

  Result := MatchChain(ANode, High(Chain));
end;

function TSvgCssSelector.Matches(const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String): Boolean;
begin
  if Length(Chain) = 0 then
    Exit(False);

  Result := Chain[High(Chain)].MatchesSimple(AElementTag, AClassName, AElementId);
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
function TSvgCssStyleSheet.GetIsEmpty: Boolean;
begin
  Result := (FRules = nil) or (FRules.Count = 0);
end;

constructor TSvgCssStyleSheet.Create;
begin
  inherited Create;
end;

destructor TSvgCssStyleSheet.Destroy;
begin
  FRules.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TSvgCssStyleSheet.Clear;
begin
  if (FRules <> nil) then
    FRules.Clear;
end;

//------------------------------------------------------------------------------

procedure TSvgCssStyleSheet.ParseCss(const ACssText: UTF8String);

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
  Clear;

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

      if (FRules = nil) then
        FRules := TSvgCssRuleList.Create;

      FRules.Add(Rule);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSvgCssStyleSheet.ApplyToNode(ANode: TSvgNode; const AElementTag: TValuePUtf8Char; const AClassName, AElementId: UTF8String);
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
  MatchingRules: TSvgCssRuleList;
begin
  if (ANode = nil) or (IsEmpty) then
    Exit;

  MatchingRules := nil;
  try
    for Rule in FRules do
    begin
      if Rule.Selector.Matches(ANode, AElementTag, AClassName, AElementId) then
      begin
        if (MatchingRules = nil) then
          MatchingRules := TSvgCssRuleList.Create;

        MatchingRules.Add(Rule);
      end;
    end;

    if (MatchingRules = nil) or (MatchingRules.Count = 0) then
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
