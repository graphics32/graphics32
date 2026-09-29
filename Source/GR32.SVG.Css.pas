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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Generics.Collections,
  GR32.SVG.Utf8,
  GR32.SVG.Tree;

type
  TSvgCssSelectorKind = (skUniversal, skElement, skClass, skId);

  TSvgCssSelector = record
    Kind: TSvgCssSelectorKind;
    Name: AnsiString;
    Specificity: Integer;
    function Matches(const AElementTag, AClassName, AElementId: AnsiString): Boolean;
    class function Parse(const ASelectorStr: AnsiString): TSvgCssSelector; static;
  end;

  TSvgCssProperty = record
    Name: AnsiString;
    Value: AnsiString;
    class function Create(const AName, AValue: AnsiString): TSvgCssProperty; static;
  end;

  TSvgCssRule = record
    Selector: TSvgCssSelector;
    Properties: TArray<TSvgCssProperty>;
    procedure AddProperty(const AName, AValue: AnsiString);
  end;

  TSvgCssStyleSheet = class(TObject)
  private
    FRules: TList<TSvgCssRule>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseCss(const ACssText: AnsiString);
    procedure ApplyToNode(ANode: TSvgNode; const AElementTag, AClassName, AElementId: AnsiString);

    property Rules: TList<TSvgCssRule> read FRules;
  end;

implementation

uses
  AnsiStrings;

{ TSvgCssSelector }

class function TSvgCssSelector.Parse(const ASelectorStr: AnsiString): TSvgCssSelector;
var
  s: AnsiString;
begin
  s := Trim(ASelectorStr);
  Result.Name := '';
  Result.Kind := skUniversal;
  Result.Specificity := 0;

  if (s = '') or (s = '*') then
  begin
    Result.Kind := skUniversal;
    Result.Specificity := 0;
    Exit;
  end;

  if s[1] = '#' then
  begin
    Result.Kind := skId;
    Result.Name := Copy(s, 2, Length(s) - 1);
    Result.Specificity := 100;
  end else
  if s[1] = '.' then
  begin
    Result.Kind := skClass;
    Result.Name := Copy(s, 2, Length(s) - 1);
    Result.Specificity := 10;
  end else
  begin
    Result.Kind := skElement;
    Result.Name := LowerCase(s);
    Result.Specificity := 1;
  end;
end;

function TSvgCssSelector.Matches(const AElementTag, AClassName, AElementId: AnsiString): Boolean;
var
  classes: TStringList;
  i: Integer;
begin
  case Kind of
    skUniversal:
      Exit(True);

    skElement:
      Exit(LowerCase(AElementTag) = Name);

    skId:
      Exit(AElementId = Name);

    skClass:
      begin
        if (AClassName = '') or (Name = '') then
          Exit(False);
        if AClassName = Name then
          Exit(True);
        // TODO
        classes := TStringList.Create;
        try
          classes.Delimiter := ' ';
          classes.DelimitedText := AClassName;
          for i := 0 to classes.Count - 1 do
            if Trim(classes[i]) = Name then
              Exit(True);
        finally
          classes.Free;
        end;
        Result := False;
      end;
  else
    Result := False;
  end;
end;

{ TSvgCssProperty }

class function TSvgCssProperty.Create(const AName, AValue: AnsiString): TSvgCssProperty;
begin
  Result.Name := Trim(AName);
  Result.Value := AnsiStrings.Trim(AValue);
end;

{ TSvgCssRule }

procedure TSvgCssRule.AddProperty(const AName, AValue: AnsiString);
var
  Len: Integer;
begin
  Len := Length(Properties);
  SetLength(Properties, Len + 1);
  Properties[len] := TSvgCssProperty.Create(AName, AValue);
end;

{ TSvgCssStyleSheet }

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

procedure TSvgCssStyleSheet.Clear;
begin
  FRules.Clear;
end;

procedure TSvgCssStyleSheet.ParseCss(const ACssText: AnsiString);
var
  i, len: Integer;
  selStr, declBlock: string;
  pOpen, pClose: Integer;
  declList: TStringList;
  declStr: AnsiString;
  colonPos: Integer;
  k, v: AnsiString;
  rule: TSvgCssRule;
  selectors: TStringList;
  selItem: AnsiString;
  sIdx, dIdx: Integer;

  function CleanCss(const AInput: AnsiString): AnsiString;
  var
    idx, inputLen: Integer;
    sb: string;
    in_C: Boolean;
  begin
    inputLen := Length(AInput);
    idx := 1;
    in_C := False;
    sb := '';
    while idx <= inputLen do
    begin
      if in_C then
      begin
        if (idx < inputLen) and (AInput[idx] = '*') and (AInput[idx + 1] = '/') then
        begin
          in_C := False;
          Inc(idx, 2);
          Continue;
        end;
        Inc(idx);
      end
      else
      begin
        if (idx < inputLen) and (AInput[idx] = '/') and (AInput[idx + 1] = '*') then
        begin
          in_C := True;
          Inc(idx, 2);
          Continue;
        end;
        sb := sb + AInput[idx];
        Inc(idx);
      end;
    end;
    Result := sb;
  end;

var
  cssClean: AnsiString;
begin
  cssClean := CleanCss(ACssText);
  len := Length(cssClean);
  i := 1;

  while i <= len do
  begin
    pOpen := Pos('{', Copy(cssClean, i, len - i + 1));
    if pOpen = 0 then Break;
    pOpen := i + pOpen - 1;

    selStr := Trim(Copy(cssClean, i, pOpen - i));

    pClose := Pos('}', Copy(cssClean, pOpen, len - pOpen + 1));
    if pClose = 0 then Break;
    pClose := pOpen + pClose - 1;

    declBlock := Copy(cssClean, pOpen + 1, pClose - pOpen - 1);

    if selStr <> '' then
    begin
      selectors := TStringList.Create;
      try
        selectors.Delimiter := ',';
        selectors.StrictDelimiter := True;
        selectors.DelimitedText := selStr;

        declList := TStringList.Create;
        try
          declList.Delimiter := ';';
          declList.StrictDelimiter := True;
          declList.DelimitedText := declBlock;

          for sIdx := 0 to selectors.Count - 1 do
          begin
            selItem := Trim(selectors[sIdx]);
            if selItem = '' then Continue;

            rule.Selector := TSvgCssSelector.Parse(selItem);
            rule.Properties := nil;

            for dIdx := 0 to declList.Count - 1 do
            begin
              declStr := Trim(declList[dIdx]);
              if declStr = '' then Continue;
              colonPos := Pos(':', declStr);
              if colonPos > 0 then
              begin
                k := Trim(Copy(declStr, 1, colonPos - 1));
                v := Trim(Copy(declStr, colonPos + 1, Length(declStr) - colonPos));
                rule.AddProperty(k, v);
              end;
            end;

            FRules.Add(rule);
          end;
        finally
          declList.Free;
        end;
      finally
        selectors.Free;
      end;
    end;

    i := pClose + 1;
  end;
end;

procedure TSvgCssStyleSheet.ApplyToNode(ANode: TSvgNode; const AElementTag, AClassName, AElementId: AnsiString);
const
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
  Specificities: array[0..3] of Integer = (0, 1, 10, 100);
var
  i, j: Integer;
  Rule: TSvgCssRule;
  Keyword: TSvgAttributeKeyword;
begin
  if (ANode = nil) or (FRules.Count = 0) then
    Exit;

  for i := Low(Specificities) to High(Specificities) do
    for Rule in FRules do
      if (Rule.Selector.Specificity = Specificities[i]) and Rule.Selector.Matches(AElementTag, AClassName, AElementId) then
        for j := 0 to High(Rule.Properties) do
        begin
          Keyword := ANode.KeywordLookup(TValuePUtf8Char.FromString(Rule.Properties[j].Name));
          if (Keyword <> attrNone) then
            ANode.ParseAttribute(Keyword, TValuePUtf8Char.FromString(Rule.Properties[j].Value));
        end;
end;

end.
