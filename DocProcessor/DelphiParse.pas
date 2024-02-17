unit DelphiParse;

////////////////////////////////////////////////////////////////////////////////
// Author        : Angus Johnson                                              //
// Copyright     : 2003-2010                                                  //
// Last revision : 4 July 2010.                                               //
////////////////////////////////////////////////////////////////////////////////

{$I DocProcessor.inc}

interface

uses
  Windows, Messages, Types, SysUtils, Classes;

type
  TTokenKind = (tkIdentifier, tkReserved, tkAsm, tkText, tkSymbol, tkValue,
    tkComment, tkSpace, tkPrecompiler, tkEndLine);

  PToken = ^TToken;
  TToken = record
    Kind: TTokenKind;
    Text: string;
    XPos: Integer;
    YPos: Integer;
  end;

  TDelphiParser = class
  private
    FFinished: Boolean;
    FMultilineComment: Boolean;
    FCurlyBraceComment: Boolean;
    FPrecompiler: Boolean;
    FAsmBlock: Boolean;
    FAsmLabel: Boolean; //flags @end as Label & avoid premature end of asm block
    FStrings: TStringList;
    FReservedList: TStringList;
    FCurrent: TPoint;
    FLastX: Integer;
    FCurrentLine: string; //line containing next token to be read
    FCurrentLineLen: Integer;
    FLastSpecialComment: string;
    FLatestCommentLine: Integer;
    FPeeked: Boolean;
    FPeekTok: TToken;
    function GetIdentToken: TTokenKind;
    function GetNumberToken: TTokenKind;
    function GetSymbolToken: TTokenKind;
    function GetTextToken: TTokenKind;
    procedure TestEndCurlyBraceComment;
    procedure TestEndMultilineComment;
    procedure GetNextTokenInternal(var TOK: TToken);
    function GetLastSpecialComment: string;
    function CheckSpecialComment(const TOK: TToken): string;
  public
    constructor Create(AStrings: TStrings);
    destructor Destroy; override;

    procedure Reset;
    procedure NextLine;
    procedure PeekNextToken(var TOK: TToken);
    procedure GetNextToken(var TOK: TToken);
    property Finished: Boolean read FFinished;
    property CurrentPt: TPoint read FCurrent;
    property ReservedList: TStringList read FReservedList;
    property LatestCommentLine: integer read FLatestCommentLine;
    property LastSpecialComment: string read GetLastSpecialComment;
  end;

const
  SINGLEQUOTE = #39;
  
implementation

uses
  Utils;

//------------------------------------------------------------------------------
// TDelphiParser ....
//------------------------------------------------------------------------------

const
  //this list is still incomplete -
  CResList: array [0 .. 97] of PChar = ('absolute', 'abstract', 'and', 'array',
    'as', 'asm', 'assembler', 'begin', 'case', 'cdecl', 'class', 'const',
    'constructor', 'contains', 'destructor', 'default', 'dispid',
    'dispinterface', 'div', 'do', 'downto', 'dynamic', 'else', 'end', 'except',
    'export', 'exports', 'external', 'far', 'file', 'finalization', 'finally',
    'for', 'forward', 'function', 'goto', 'if', 'implementation', 'in',
    'inline', 'initialization', 'interface', 'is', 'inherited', 'label',
    'library', 'message', 'mod', 'near', 'nil', 'not', 'object', 'platform',
    'of', 'on', 'or', 'override', 'overload', 'out', 'package', 'packed',
    'pascal', 'property', 'protected', 'private', 'procedure', 'program',
    'public', 'published', 'read', 'reintroduce', 'write', 'raise', 'record',
    'register', 'repeat', 'reintroduce', 'requires', 'resourcestring',
    'safecall', 'shl', 'shr', 'set', 'stdcall', 'string', 'then', 'to',
    'threadvar', 'try', 'type', 'unit', 'until', 'uses', 'var', 'virtual',
    'with', 'while', 'xor');

constructor TDelphiParser.Create(AStrings: TStrings);
var
  Index: Integer;
begin
  inherited Create;
  FReservedList := TStringList.create;
  for Index := Low(CResList) to High(CResList) do
    FReservedList.Add(CResList[Index]);
  FReservedList.Sorted := True;

  FStrings := TStringList.create;
  FStrings.Assign(AStrings);
  Reset;
end;
//------------------------------------------------------------------------------

destructor TDelphiParser.Destroy;
begin
  FStrings.Free;
  FReservedList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.Reset;
begin
  FMultilineComment := False;
  FCurlyBraceComment := False;
  FPrecompiler := False;
  FAsmBlock := False;
  FAsmLabel := False;

  FCurrent := Point(-1, -1);
  FFinished := (FStrings = nil);
  if not FFinished then
    NextLine;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.NextLine;
begin
  if FCurrent.Y < FStrings.count - 1 then
  begin
    Inc(FCurrent.Y);
    FCurrent.X := 1;
    FLastX := 1;
    FCurrentLine := FStrings[FCurrent.Y];
    FCurrentLineLen := Length(FCurrentLine);
  end
  else
    FFinished := True;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.PeekNextToken(var TOK: TToken);
begin
  if FPeeked then
    TOK := FPeekTok
  else
  begin
    repeat
      GetNextTokenInternal(TOK);
      case TOK.Kind of
        tkComment: CheckSpecialComment(TOK);
        tkEndline: if (FCurrent.Y > FLatestCommentLine + 1) then FLastSpecialComment := '';
      end;
    until Finished or not (TOK.Kind in [tkSpace, tkComment, tkPrecompiler, tkEndline]);
    FPeekTok := TOK;
    FPeeked := True;
  end;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.GetNextToken(var TOK: TToken);
begin
  if FPeeked then
  begin
    TOK := FPeekTok;
    FPeeked := False;
  end
  else
    repeat
      GetNextTokenInternal(TOK);
      case TOK.Kind of
        tkComment: CheckSpecialComment(TOK);
        tkEndline: if (FCurrent.Y > FLatestCommentLine + 1) then
          FLastSpecialComment := '';
      end;
    until Finished or not (TOK.Kind in [tkSpace, tkComment, tkPrecompiler, tkEndline]);
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.GetNextTokenInternal(var TOK: TToken);
var
  Dummy: Integer;
begin
  TOK.XPos := FCurrent.X;
  TOK.YPos := FCurrent.Y;
  if FFinished then
  begin
    TOK.Text := '';
    TOK.Kind := tkEndLine;
    Exit;
  end
  //end of line
  else if FCurrent.X > FCurrentLineLen then
  begin
    TOK.Text := '';
    TOK.Kind := tkEndLine;
    NextLine;
    Exit;
  end;

  if FCurlyBraceComment then
  begin
    TestEndCurlyBraceComment;
    TOK.Kind := tkComment;
  end
  else if FMultilineComment then
  begin
    TestEndMultilineComment;
    TOK.Kind := tkComment;
  end
  //whitespace ...
  else if (FCurrentLine[FCurrent.X] < #33) then
  begin
    while (FCurrent.X <= FCurrentLineLen) and
      (FCurrentLine[FCurrent.X] < #33) do Inc(FCurrent.X);
    TOK.Kind := tkSpace;
  end
  //identifier
  else if CharInSet(UpCase(FCurrentLine[FCurrent.X]), ['A'..'Z', '_']) then
    TOK.Kind := GetIdentToken
  //Text
  else if CharInSet(FCurrentLine[FCurrent.X], ['#',SINGLEQUOTE]) and not
    FMultilineComment and not FCurlyBraceComment then
    TOK.Kind := GetTextToken
  //number
  else if CharInSet(FCurrentLine[FCurrent.X], ['$', '0'..'9']) then
    TOK.Kind := GetNumberToken
  //symbol
  else
    TOK.Kind := GetSymbolToken;

  TOK.Text := Copy(FCurrentLine, FLastX, FCurrent.X - FLastX);

  //check for assembler blocks ...
  if FAsmBlock and not (TOK.Kind in [tkComment,tkSpace]) then
  begin
    if (LowerCase(TOK.Text) = 'end') and not FAsmLabel then
    begin
      TOK.Kind := tkReserved;
      FAsmBlock := False;
    end else
      TOK.Kind := tkAsm;
    FAsmLabel := (TOK.Text = '@');
  end
  //check for reserved words ...
  else if (TOK.Kind = tkIdentifier) and FReservedList.Find(TOK.Text, Dummy) then
  begin
    TOK.Kind := tkReserved;
    TOK.Text := LowerCase(TOK.Text);
    if TOK.Text = 'asm' then
      FAsmBlock := True;
  end;
  FLastX := FCurrent.X;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetIdentToken: TTokenKind;
const
  IdentifierChars = ['0'..'9', 'A'..'Z', '_', 'a'..'z'];
begin
  if FMultilineComment or FCurlyBraceComment then
  begin
    if FPrecompiler then
      Result := tkPrecompiler
    else
      Result := tkComment;
  end else
    Result := tkIdentifier;
  repeat
    Inc(FCurrent.X);
  until (FCurrent.X > FCurrentLineLen) or not
    CharInSet(FCurrentLine[FCurrent.X], IdentifierChars);
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetNumberToken: TTokenKind;
var
  IsHex: Boolean;
begin
  IsHex := (FCurrentLine[FCurrent.X] = '$');
  Inc(FCurrent.X);
  if FMultilineComment or FCurlyBraceComment  then
  begin
    if FPrecompiler then
      Result := tkPrecompiler
    else
      Result := tkComment;
  end else
    Result := tkValue;
  if IsHex then
    while (FCurrent.X <= FCurrentLineLen) and
      CharInSet(FCurrentLine[FCurrent.X], ['0'..'9', 'A'..'F', 'a'..'f']) do
        Inc(FCurrent.X)
  else
    while (FCurrent.X <= FCurrentLineLen) do
      if CharInSet(FCurrentLine[FCurrent.X], ['0'..'9']) then
        Inc(FCurrent.X)
      else if FCurrentLine[FCurrent.X] = '.' then
      begin
        if FCurrentLine[FCurrent.X - 1] = '.' then // ..
        begin
          Dec(FCurrent.X);
          Break;
        end
        else Inc(FCurrent.X);
      end
      else Break;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.TestEndCurlyBraceComment;
begin
  while (FCurrent.X <= FCurrentLineLen) and
    (FCurrentLine[FCurrent.X] <> '}') do Inc(FCurrent.X);
  if (FCurrent.X <= FCurrentLineLen) then
  begin
    FCurlyBraceComment := False;
    FPrecompiler := False;
    Inc(FCurrent.X);
  end;
end;
//------------------------------------------------------------------------------

procedure TDelphiParser.TestEndMultilineComment;
begin
  while (FCurrent.X < FCurrentLineLen) do
  begin
    if (FCurrentLine[FCurrent.X] = '*') and
      (FCurrentLine[FCurrent.X + 1] = ')') then
    begin
      FMultilineComment := False;
      Inc(FCurrent.X, 2);
      Exit;
    end;
    Inc(FCurrent.X);
  end;
  FCurrent.X := FCurrentLineLen + 1;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetSymbolToken: TTokenKind;
begin
  if FMultilineComment or FCurlyBraceComment  then
  begin
    if FPrecompiler then
      Result := tkPrecompiler
    else
      Result := tkComment;
  end else
    Result := tkSymbol;
  Inc(FCurrent.X);
  case FCurrentLine[FCurrent.X - 1] of
    '/':  if  (Result <> tkComment) and
      (FCurrent.X <= FCurrentLineLen) and (FCurrentLine[FCurrent.X] = '/') then
          begin
            FCurrent.X := FCurrentLineLen + 1;
            Result := tkComment;
          end;
    '{':  if not FMultilineComment then
          begin
            FCurlyBraceComment := True;
            if (FCurrent.X < FCurrentLineLen) and
              (FCurrentLine[FCurrent.X] = '$') then
            begin
              FPrecompiler := True;
              Result := tkPrecompiler;
            end else Result := tkComment;
            TestEndCurlyBraceComment;
          end;
    '}':  if FCurlyBraceComment then
          begin
            FPrecompiler := False;
            FCurlyBraceComment := False;
          end;
    '(':  if not FCurlyBraceComment and (FCurrent.X <= FCurrentLineLen) and
            (FCurrentLine[FCurrent.X] = '*') then
          begin
            FMultilineComment := True;
            Inc(FCurrent.X);
            Result := tkComment;
            TestEndMultilineComment;
          end;
    '*':  if FMultilineComment and not FCurlyBraceComment and
      (FCurrent.X <= FCurrentLineLen) and (FCurrentLine[FCurrent.X] = ')') then
          begin
            FMultilineComment := False;
            Inc(FCurrent.X);
          end;
    end;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetTextToken: TTokenKind;
begin
  Result := tkText;
  Inc(FCurrent.X); //ignore first SINGLEQUOTE or #
  if (FCurrentLine[FCurrent.X - 1] = '#') then
  begin
    while (FCurrent.X <= FCurrentLineLen) and
      CharInSet(FCurrentLine[FCurrent.X], ['0'..'9']) do Inc(FCurrent.X);
  end else
  begin
    while (FCurrent.X <= FCurrentLineLen) do
    begin
      if (FCurrentLine[FCurrent.X] = SINGLEQUOTE) then
      begin
        Inc(FCurrent.X);
        if (FCurrent.X > FCurrentLineLen) or
          (FCurrentLine[FCurrent.X] <> SINGLEQUOTE) then
          Break;
      end;
      Inc(FCurrent.X);
    end;
  end;
end;
//------------------------------------------------------------------------------

function TDelphiParser.GetLastSpecialComment: string;
var
  CharCount: Integer;
begin
  Result := FLastSpecialComment;
  CharCount := Length(Result);
  if (CharCount > 1) and (Result[CharCount] = ')') and (Result[CharCount - 1] = '*') then
    Delete(Result, Length(Result) - 1, 2);
end;
//------------------------------------------------------------------------------

function TDelphiParser.CheckSpecialComment(const TOK: TToken): string;
var
  CharCount: Integer;
begin
  // modified 4 Jul 2010 - now accepts standard delphi comments (ie no longer
  // uses //* format to indicate DocProcessor comments).
  CharCount := Length(TOK.Text);
  if CharCount < 3 then //do nothing
  else if not FMultilineComment then
  begin
    if (FLastSpecialComment = '') then
      FLastSpecialComment := Copy(TOK.Text, 3, CharCount)
    else if FLastSpecialComment <> '' then
      FLastSpecialComment := FLastSpecialComment + ' ' + Copy(TOK.Text, 3, CharCount);
  end
  else if FMultilineComment and (FLastSpecialComment = '') then
    FLastSpecialComment := Copy(TOK.Text, 3, CharCount - 2)
  else if FLastSpecialComment <> '' then
    FLastSpecialComment := FLastSpecialComment + ' ' + Trim(TOK.Text);
  FLatestCommentLine := FCurrent.Y;
end;
//------------------------------------------------------------------------------

end.
