unit HTML_Tags;

{$I DocProcessor.inc}

interface

type
  TElemType = (etUnknown, etBlock, etInline, etBR);
  TClosingType = (ctUnknown, ctAlways, ctNever, ctAnchor);

  THtmlTagInfo = record
    Name: string;
    ElemType: TElemType;
    ClosingType: TClosingType;
    SimpleContent: Boolean;
  end;

function GetTagInfo(const TagName: string): THtmlTagInfo;

implementation

uses
  SysUtils;

var
  Tags: array of THtmlTagInfo;

procedure Add(const AName: string; Elem: TElemType; Closing: TClosingType; Simple: Boolean = False);
var
  L: Integer;
begin
  L := Length(Tags);
  SetLength(Tags, L + 1);
  with Tags[L] do
  begin
    Name := AName;
    ElemType := Elem;
    ClosingType := Closing;
    SimpleContent := Simple;
  end;
end;

function GetTagInfo(const TagName: string): THtmlTagInfo;
var
  I: Integer;
begin
  for I := 1 to High(Tags) do
  begin
    if SameText(Tags[I].Name, TagName) then
    begin
      Result := Tags[I];
      Exit;
    end;
  end;
  Result := Tags[0];
end;

initialization
                          //Element   //Closing  //SimpleContent
                          //type      //type     //true == same line
  Add(''                , etUnknown,  ctUnknown, False);
  Add('a'               , etInline ,  ctAnchor , True);
  Add('b'               , etInline ,  ctAlways , True);
  Add('body'            , etBlock  ,  ctAlways , False);
  Add('br'              , etBR     ,  ctNever  , True);
  Add('center'          , etBlock  ,  ctAlways , True);
  Add('div'             , etInline ,  ctAlways , False);
  Add('em'              , etInline ,  ctAlways , True);
  Add('font'            , etInline ,  ctAlways , True);
  Add('head'            , etBlock  ,  ctAlways , False);
  Add('h1'              , etBlock  ,  ctAlways , True);
  Add('h2'              , etBlock  ,  ctAlways , True);
  Add('h3'              , etBlock  ,  ctAlways , True);
  Add('h4'              , etBlock  ,  ctAlways , True);
  Add('hr'              , etBlock  ,  ctNever  , True);
  Add('html'            , etBlock  ,  ctAlways , False);
  Add('i'               , etInline ,  ctAlways , True);
  Add('img'             , etInline ,  ctNever  , False);
  Add('li'              , etBlock  ,  ctAlways , True);
  Add('link'            , etBlock  ,  ctNever  , False);
  Add('meta'            , etBlock  ,  ctNever  , False);
  Add('object'          , etInline ,  ctAlways , False);
  Add('p'               , etBlock  ,  ctAlways , True);
  Add('param'           , etBlock  ,  ctNever  , True);
  Add('strong'          , etInline ,  ctAlways , True);
  Add('span'            , etInline ,  ctAlways , True);
  Add('table'           , etBlock  ,  ctAlways , False);
  Add('tr'              , etBlock  ,  ctAlways , False);
  Add('td'              , etBlock  ,  ctAlways , False);
  Add('th'              , etBlock  ,  ctAlways , True);
  Add('pre'             , etBlock  ,  ctAlways , False);
  Add('title'           , etBlock  ,  ctAlways , True);
  Add('script'          , etBlock  ,  ctAlways , False);
  Add('style'           , etBlock  ,  ctAlways , False);
  Add('u'               , etInline ,  ctAlways , True);
  Add('ul'              , etBlock  ,  ctAlways , False);
  Add('input'           , etBlock  ,  ctNever , False);

finalization
  Tags := nil;

end.
