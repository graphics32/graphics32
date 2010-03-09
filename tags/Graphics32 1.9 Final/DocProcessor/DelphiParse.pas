unit DelphiParse;

////////////////////////////////////////////////////////////////////////////////
// Author        : Angus Johnson                                              //
// Copyright     : 2003-2009                                                  //
// Last revision : 9 Jan 2004.                                                //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TTokenKind = (tkIdentifier, tkReserved, tkAsm,
    tkText, tkSymbol, tkValue, tkComment, tkSpace, tkPrecompiler, tkEndLine);

  PToken = ^TToken;
  TToken = record
    kind: TTokenKind;
    text: string;
    XPos: integer;
    YPos: integer;
  end;

  TDelphiParser = class
  private
    fFinished: boolean;
    fMultilineComment: boolean;
    fCurlyBraceComment: boolean;
    fPrecompiler: boolean;
    fAsmBlock: boolean;
    fAsmLabel: boolean; //flags @end as Label & avoid premature end of asm block
    fStrings: TStringList;
    fReservedList: TStringList;
    fCurrent: TPoint;
    fLastX: integer;
    fCurrentLine: string; //line containing next token to be read
    fCurrentLineLen: integer;
    fLastSpecialComment: string;
    fLatestCommentLine: integer;
    fPeeked: boolean;
    fPeekTok: TToken;
    function GetIdentToken: TTokenKind;
    function GetNumberToken: TTokenKind;
    function GetSymbolToken: TTokenKind;
    function GetTextToken: TTokenKind;
    procedure TestEndCurlyBraceComment;
    procedure TestEndMultilineComment;
    procedure GetNextTokenInternal(var tok: TToken);
    function GetLastSpecialComment: string;
    function CheckSpecialComment(const tok: TToken): string;
  public
    constructor Create(aStrings: TStrings);
    destructor Destroy; override;
    procedure Reset;
    procedure NextLine;
    procedure PeekNextToken(var tok: TToken);
    procedure GetNextToken(var tok: TToken);
    property Finished: boolean read fFinished;
    property CurrentPt: TPoint read fCurrent;
    property ReservedList: TStringList read fReservedList;
    property LastSpecialComment: string read GetLastSpecialComment;
  end;

const
  SINGLEQUOTE = #39;
  
implementation

//------------------------------------------------------------------------------
// TDelphiParser ....
//------------------------------------------------------------------------------

const
  //this list is still incomplete -
  resList: array[0..97] of PChar = ('absolute','abstract','and','array','as',
    'asm','assembler','begin','case','cdecl','class','const','constructor',
    'contains', 'destructor','default','dispid','dispinterface','div','do',
    'downto','dynamic','else','end','except','export','exports','external',
    'far','file','finalization','finally','for','forward','function','goto',
    'if','implementation','in','inline','initialization','interface','is',
    'inherited','label','library','message','mod','near','nil','not','object', 'platform',
    'of','on','or','override','overload','out','package','packed','pascal','property',
    'protected','private','procedure','program','public','published','read', 'reintroduce',
    'write','raise','record','register','repeat','reintroduce','requires',
    'resourcestring','safecall','shl','shr','set','stdcall','string','then',
    'to','threadvar','try','type','unit','until','uses','var','virtual','with',
    'while','xor');

constructor TDelphiParser.Create(aStrings: TStrings);
var
  i: integer;
begin
  inherited Create;
  fReservedList := TStringList.create;
  for i := Low(resList) to High(resList) do
    fReservedList.Add(resList[i]);
  fReservedList.Sorted := true;
  
  fStrings := TStringList.create;
  fStrings.assign(aStrings);
  Reset;
end;
//------------------------------------------------------------------------------

destructor TDelphiParser.Destroy;
begin
  fStrings.free;
  fReservedList.free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.Reset;
begin
  fMultilineComment := false;
  fCurlyBraceComment := false;
  fPrecompiler := false;
  fAsmBlock := false;
  fAsmLabel := false;

  fCurrent := Point(-1,-1);
  fFinished := (fStrings = nil);
  if not fFinished then NextLine;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.NextLine;
begin
  inc(fCurrent.y);
  if fCurrent.y = fStrings.count then
  begin
    fFinished := true;
  end else
  begin
    fCurrent.x := 1;
    fLastX := 1;
    fCurrentLine := fStrings[fCurrent.y];
    fCurrentLineLen := length(fCurrentLine);
  end;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.PeekNextToken(var tok: TToken);
begin
  if fPeeked then
    tok := fPeekTok
  else
  begin
    repeat
      GetNextTokenInternal(tok);
      case tok.kind of
        tkComment: CheckSpecialComment(tok);
        tkEndline: if (fCurrent.Y > fLatestCommentLine+1) then fLastSpecialComment := '';
      end;
    until Finished or not (tok.kind in [tkSpace, tkComment, tkPrecompiler, tkEndline]);
    fPeekTok := tok;
    fPeeked := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.GetNextToken(var tok: TToken);
begin
  if fPeeked then
  begin
    tok := fPeekTok;
    fPeeked := false;
  end
  else
    repeat
      GetNextTokenInternal(tok);
      case tok.kind of
        tkComment: CheckSpecialComment(tok);
        tkEndline: if (fCurrent.Y > fLatestCommentLine+1) then fLastSpecialComment := '';
      end;
    until Finished or not (tok.kind in [tkSpace, tkComment, tkPrecompiler, tkEndline]);
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.GetNextTokenInternal(var tok: TToken);
var
  dummy: integer;
begin
  tok.XPos := fCurrent.x;
  tok.YPos := fCurrent.y;
  if fFinished then
  begin
    tok.text := '';
    tok.kind := tkEndLine;
    exit;
  end
  //end of line
  else if fCurrent.x > fCurrentLineLen then
  begin
    tok.text := '';
    tok.kind := tkEndLine;
    NextLine;
    exit;
  end;

  if fCurlyBraceComment then
  begin
    TestEndCurlyBraceComment;
    tok.Kind := tkComment;
  end
  else if fMultilineComment then
  begin
    TestEndMultilineComment;
    tok.Kind := tkComment;
  end
  //whitespace ...
  else if (fCurrentLine[fCurrent.x] < #33) then
  begin
    while (fCurrent.x <= fCurrentLineLen) and
      (fCurrentLine[fCurrent.x] < #33) do inc(fCurrent.x);
    tok.Kind := tkSpace;
  end
  //identifier
  else if upcase(fCurrentLine[fCurrent.x]) in  ['A'..'Z','_'] then
    tok.kind := GetIdentToken
  //text
  else if (fCurrentLine[fCurrent.x] in ['#',SINGLEQUOTE]) and not
    fMultilineComment and not fCurlyBraceComment then
    tok.kind := GetTextToken
  //number
  else if (fCurrentLine[fCurrent.x] in ['$','0'..'9']) then
    tok.kind := GetNumberToken
  //symbol
  else
    tok.kind := GetSymbolToken;

  tok.text := copy(fCurrentLine,fLastX,fCurrent.x-fLastX);

  //check for assembler blocks ...
  if fAsmBlock and not (tok.kind in [tkComment,tkSpace]) then
  begin
    if (lowercase(tok.text) = 'end') and not fAsmLabel then
    begin
      tok.kind := tkReserved;
      fAsmBlock := false;
    end else
      tok.kind := tkAsm;
    fAsmLabel := (tok.text = '@');
  end
  //check for reserved words ...
  else if (tok.kind = tkIdentifier) and fReservedList.Find(tok.text, dummy) then
  begin
    tok.kind := tkReserved;
    tok.text := LowerCase(tok.text);
    if tok.text = 'asm' then fAsmBlock := true;
  end;
  fLastX := fCurrent.x;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetIdentToken: TTokenKind;
const
  IdentifierChars = ['0'..'9','A'..'Z','_','a'..'z'];
begin
  if fMultilineComment or fCurlyBraceComment then
  begin
    if fPrecompiler then result := tkPrecompiler
    else result := tkComment;
  end else
    result := tkIdentifier;
  repeat
    inc(fCurrent.x);
  until (fCurrent.x > fCurrentLineLen) or not
    (fCurrentLine[fCurrent.x] in IdentifierChars);
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetNumberToken: TTokenKind;
var
  IsHex: boolean;
begin
  IsHex := (fCurrentLine[fCurrent.x] = '$');
  inc(fCurrent.x);
  if fMultilineComment or fCurlyBraceComment  then
  begin
    if fPrecompiler then result := tkPrecompiler
    else result := tkComment;
  end else
    result := tkValue;
  if IsHex then
    while (fCurrent.x <= fCurrentLineLen) and
      (fCurrentLine[fCurrent.x] in ['0'..'9','A'..'F','a'..'f']) do
        inc(fCurrent.x)
  else
    while (fCurrent.x <= fCurrentLineLen) do
      if (fCurrentLine[fCurrent.x] in ['0'..'9']) then
        inc(fCurrent.x)
      else if fCurrentLine[fCurrent.x] = '.' then
      begin
        if fCurrentLine[fCurrent.x-1] = '.' then // ..
        begin
          dec(fCurrent.x);
          break;
        end
        else inc(fCurrent.x);
      end
      else break;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.TestEndCurlyBraceComment;
begin
  while (fCurrent.x <= fCurrentLineLen) and
    (fCurrentLine[fCurrent.x] <> '}') do inc(fCurrent.x);
  if (fCurrent.x <= fCurrentLineLen) then
  begin
    fCurlyBraceComment := false;
    fPrecompiler := false;
    inc(fCurrent.x);
  end;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.TestEndMultilineComment;
begin
  while (fCurrent.x < fCurrentLineLen) do
  begin
    if (fCurrentLine[fCurrent.x] = '*') and
      (fCurrentLine[fCurrent.x +1] = ')') then
    begin
      fMultilineComment := false;
      inc(fCurrent.x,2);
      exit;
    end;
    inc(fCurrent.x);
  end;
  fCurrent.x := fCurrentLineLen +1;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetSymbolToken: TTokenKind;
begin
  if fMultilineComment or fCurlyBraceComment  then
  begin
    if fPrecompiler then result := tkPrecompiler
    else result := tkComment;
  end else
    result := tkSymbol;
  inc(fCurrent.x);
  case fCurrentLine[fCurrent.x-1] of
    '/':  if  (result <> tkComment) and
      (fCurrent.x <= fCurrentLineLen) and (fCurrentLine[fCurrent.x] = '/') then
          begin
            fCurrent.x := fCurrentLineLen+1;
            result := tkComment;
          end;
    '{':  if not fMultilineComment then
          begin
            fCurlyBraceComment := true;
            if (fCurrent.x < fCurrentLineLen) and
              (fCurrentLine[fCurrent.x] = '$') then
            begin
              fPrecompiler := true;
              result := tkPrecompiler;
            end else result := tkComment;
            TestEndCurlyBraceComment;
          end;
    '}':  if fCurlyBraceComment then
          begin
            fPrecompiler := false;
            fCurlyBraceComment := false;
          end;
    '(':  if not fCurlyBraceComment and (fCurrent.x <= fCurrentLineLen) and
            (fCurrentLine[fCurrent.x] = '*') then
          begin
            fMultilineComment := true;
            inc(fCurrent.x);
            result := tkComment;
            TestEndMultilineComment;
          end;
    '*':  if fMultilineComment and not fCurlyBraceComment and
      (fCurrent.x <= fCurrentLineLen) and (fCurrentLine[fCurrent.x] = ')') then
          begin
            fMultilineComment := false;
            inc(fCurrent.x);
          end;
    end;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetTextToken: TTokenKind;
begin
  result := tkText;
  inc(fCurrent.x); //ignore first SINGLEQUOTE or #
  if (fCurrentLine[fCurrent.x-1] = '#') then
  begin
    while (fCurrent.x <= fCurrentLineLen) and
      (fCurrentLine[fCurrent.x] in ['0'..'9']) do inc(fCurrent.x);
  end else
  begin
    while (fCurrent.x <= fCurrentLineLen) do
    begin
      if (fCurrentLine[fCurrent.x] = SINGLEQUOTE) then
      begin
        inc(fCurrent.x);
        if (fCurrent.x > fCurrentLineLen) or
          (fCurrentLine[fCurrent.x] <> SINGLEQUOTE) then break;
      end;
      inc(fCurrent.x);
    end;
  end;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetLastSpecialComment: string;
begin
  result := fLastSpecialComment;
  if (result <> '') and (result[length(result)] = ')') then
    delete(result, length(result)-1, 2);
end;
//------------------------------------------------------------------------------

function TDelphiParser.CheckSpecialComment(const tok: TToken): string;
var
  len: integer;
begin
  len := length(tok.text);
  if len < 3 then //do nothing
  else if not fMultilineComment and (tok.text[3] = '*') then
  begin
    if (fLastSpecialComment = '') then
      fLastSpecialComment := copy(tok.text,4,len-3)
    else if fLastSpecialComment <> '' then
      fLastSpecialComment := fLastSpecialComment + ' ' +copy(tok.text,4,len-3);
  end
  else if fMultilineComment and
    (tok.text[3] = '*') and (fLastSpecialComment = '') then
      fLastSpecialComment := copy(tok.text,4,len-3)
  else if fLastSpecialComment <> '' then
    fLastSpecialComment := fLastSpecialComment +' ' + trim(tok.text);
  fLatestCommentLine := fCurrent.Y;
end;
//------------------------------------------------------------------------------

end.
