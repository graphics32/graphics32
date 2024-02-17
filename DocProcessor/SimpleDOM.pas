unit SimpleDOM;

{$I DocProcessor.inc}
{$WARN UNSAFE_CAST OFF}

interface

{-$DEFINE CODESITE}

uses
  Classes, Types, SysUtils, Contnrs,
  HTML_Tags{$IFDEF CODESITE}, CSIntf{$ENDIF};

type
  EDomError = class(Exception);

  TAttribute = class
  private
    FName: string;
    FValue: string;
  public
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  TAttributeList = class(TObjectList)
  private
    function  GetItem(const AName: string): string;
    function  GetItems(Index: Integer): TAttribute;
    procedure SetItem(const AName, AValue: string);
    procedure SetItems(Index: Integer; Attribute: TAttribute);
  public
    constructor Create;
    procedure Add(const AName, AValue: string);
    function  Has(const AName: string): Boolean;
    function  FindItem(const AName: string): TAttribute;
    procedure Remove(const AName: string);
    property Item[const AName: string]: string read GetItem write SetItem; default;
    property Items[Index: Integer]: TAttribute read GetItems write SetItems;
  end;

  TDomNode = class;

  TDomNodeList = class(TObjectList)
  private
    function GetItems(Index: Integer): TDomNode;
    procedure SetItems(Index: Integer; Value: TDomNode);
  public
    property Items[Index: Integer]: TDomNode read GetItems write SetItems;
  end;

  TDomNodeType = (ntText, ntTag, ntComment, ntPI, ntCData);

  TDomNode = class(TDomNodeList)
  private
    FAttributes: TAttributeList;
    FName: string;
    FNodeType: TDomNodeType;
    FParent: TDomNode;
    FValue: string;
    procedure SetIndex(Value: Integer);
    procedure SetParent(Value: TDomNode);
    function GetIndex: Integer;
  public
    constructor Create(AParent: TDomNode);
    destructor Destroy; override;
    function Add(const AName: string = ''): TDomNode;
    function AddObject(const AType: string): TDomNode;
    function AddObjectParam(const AName, AValue: string): TDomNode;
    function AddParse(const Text: string): TDomNode;
    function AddText(const Text: string): TDomNode;
    function AddNode(ANode: TDomNode): TDomNode;
    function Duplicate: TDomNode;
    function FindNode(const AName: string; Recursive: Boolean): TDomNode;
    function FindNodes(const AName: string; Recursive: Boolean): TDomNodeList;
    function FirstChild: TDomNode;
    function Insert(Index: Integer; const AName: string = ''): TDomNode;
    function InsertNode(Index: Integer; ANode: TDomNode): TDomNode;
    function LastChild: TDomNode;
    function NextSibling: TDomNode;
    function PrevSibling: TDomNode;
    function AsString: string;
    function GetContent: string;
    property Attributes: TAttributeList read FAttributes;
    property Index: Integer read GetIndex write SetIndex;
    property Name: string read FName write FName;
    property NodeType: TDomNodeType read FNodeType write FNodeType;
    property Parent: TDomNode read FParent write SetParent;
    property Value: string read FValue write FValue;
  end;

  TDomDocument = class(TDomNode)
  public
    constructor Create; 
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
  end;

  TDomParser = class
  protected
    Buffer: string;
    Pos: PChar;
    procedure Reset;
    procedure Parse(Dst: TDomNode);
  public
    procedure ReadFile(Dst: TDomNode; const FileName: string);
    procedure ReadStream(Dst: TDomNode; Stream: TStream);
    procedure ReadString(Dst: TDomNode; const AString: string);
    procedure WriteStream(Src: TDomNode; Stream: TStream; Level: Integer);
  end;

var
  DefaultHTMLMode: Boolean;

implementation

{ String handling }

const
  CValidWhiteSpace = [#13, #10, #9, #32]; // cr, lf, tab, space
  CValidLetter = [#$41..#$5A, #$61..#$7A, #$C0..#$D6, #$D8..#$F6, #$F8..#$FF];
  CValidDigit = ['0'..'9'];
  CValidNameChar = CValidLetter + CValidDigit + ['.', '-', '_', ':', #$B7];
  CValidNameFirst = CValidLetter + ['_', ':'];

function SubString(Start, Current: PChar): string;
var
  Size: Integer;
begin
  Size := Integer(Current) - Integer(Start);
  SetString(Result, nil, Size div sizeof(Char));
  Move(Start^, Pointer(Result)^, Size);
end;

function TrimWhiteSpace(const Src: string): string;
var
  L, R: PChar;
begin
  L := Pointer(Src);
  R := L + Length(Src) - 1;
  while AnsiChar(L^) in CValidWhiteSpace do Inc(L);
  while (R >= L) and (AnsiChar(R^) in CValidWhiteSpace) do Dec(R);
  SetString(Result, L, Integer(R) - Integer(L) + 1);
end;

procedure OmitWhiteSpace(var P: PChar);
begin
  while AnsiChar(P^) in CValidWhiteSpace do Inc(P);
end;

function ConvertWhiteSpace(const Src: string): string;
const
  CTerminators = CValidWhiteSpace + [#0];
var
  L, R: PChar;
begin
  L := Pointer(Src);
  R := L;
  SetLength(Result, 0);
  while L^ <> #0 do
  begin
    if not (AnsiChar(R^) in CTerminators) then Inc(R)
    else
    begin
      if R > L then Result := Result + SubString(L, R);
      if R^ = #0 then Exit;
      Result := Result + ' ';
      while AnsiChar(R^) in CValidWhiteSpace do Inc(R);
      L := R;
    end;
  end;
end;

function GetName(var P: PChar): string;
var
  Start: PChar;
begin
  SetLength(Result, 0);
  if AnsiChar(P^) in CValidNameFirst then
  begin
    Start := P;
    Inc(P);
    while AnsiChar(P^) in CValidNameChar do Inc(P);
    Result := SubString(Start, P);
  end
end;

function GetEq(var P: PChar): Boolean;
begin
  OmitWhiteSpace(P);
  if P^ = '=' then
  begin
    Result := True;
    Inc(P);
    OmitWhiteSpace(P);
  end
  else Result := False;
end;

function GetAttValue(var P: PChar; out Error: Boolean): string;
var
  Start: PChar;
  Terminators: set of AnsiChar;
  Qt: Char;
begin
  SetLength(Result, 0);
  Error := True;
  OmitWhiteSpace(P);
  if AnsiChar(P^) in ['''', '"'] then
  begin
    Qt := P^;
    Terminators := ['^', '<', #0] + [Qt]; // removed '&' from terminator
    Inc(P);
    Start := P;
    while not (AnsiChar(P^) in Terminators) do Inc(P);
    if P^ = Qt then
    begin
      Result := SubString(Start, P);
      Inc(P); // proceed to a next symbol after terminator
      Error := False;
    end;
  end;
end;

{ Misc. routines }

procedure DomError(const Msg: string);
begin
  raise EDomError.Create(Msg);
end;

{ TAttributeList }

procedure TAttributeList.Add(const AName, AValue: string);
var
  Attr: TAttribute;
begin
  Attr := TAttribute.Create;
  inherited Add(Attr);
  Attr.Name := AName;
  Attr.Value := AValue;
end;

constructor TAttributeList.Create;
begin
  inherited Create(True);
end;

function TAttributeList.FindItem(const AName: string): TAttribute;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if SameText(Result.Name, AName) then Exit
  end;
  Result := nil;
end;

function TAttributeList.GetItem(const AName: string): string;
var
  Attr: TAttribute;
begin
  Attr := FindItem(AName);
  if Attr <> nil then Result := Attr.Value
  else Result := '';
end;

function TAttributeList.GetItems(Index: Integer): TAttribute;
begin
  Result := TAttribute(inherited Items[Index]);
end;

function TAttributeList.Has(const AName: string): Boolean;
begin
  Result := FindItem(AName) <> nil;
end;

procedure TAttributeList.Remove(const AName: string);
begin
  inherited Remove(FindItem(AName));
end;

procedure TAttributeList.SetItem(const AName, AValue: string);
var
  Attr: TAttribute;
begin
  Attr := FindItem(AName);
  if Attr <> nil then Attr.Value := AValue
  else Add(AName, AValue);
end;

procedure TAttributeList.SetItems(Index: Integer; Attribute: TAttribute);
begin
  inherited Items[Index] := Attribute
end;

{ TDomNodeList }

function TDomNodeList.GetItems(Index: Integer): TDomNode;
begin
  Result := TDomNode(inherited Items[Index]);
end;

procedure TDomNodeList.SetItems(Index: Integer; Value: TDomNode);
begin
  inherited Items[Index] := Value;
end;

{ TDomNode }

function TDomNode.Add(const AName: string): TDomNode;
begin
  Result := TDomNode.Create(Self);
  Result.Name := AName;
  Result.NodeType := ntTag;
end;

function TDomNode.AddNode(ANode: TDomNode): TDomNode;
begin
  inherited Add(ANode);
  Result := ANode;
  ANode.FParent := Self;
end;

function TDomNode.AddObject(const AType: string): TDomNode;
begin
  Result := Add('object');
  Result.Attributes['type'] := AType;
end;

function TDomNode.AddObjectParam(const AName, AValue: string): TDomNode;
begin
  Result := Add('param');
  Result.Attributes['name'] := AName;
  Result.Attributes['value'] := AValue;
end;

function TDomNode.AddParse(const Text: string): TDomNode;
var
  P: TDomParser;
begin
  Result := nil;
  if Text = '' then Exit;
  P := TDomParser.Create;
  with P do
  try
    Buffer := Text;
    Reset;
    Parse(Self);
    Result := Self.LastChild;
  finally
    Free;
  end;
end;

function TDomNode.AddText(const Text: string): TDomNode;
begin
  Result := Add;
  Result.NodeType := ntText;
  Result.Value := Text;
end;

constructor TDomNode.Create(AParent: TDomNode);
begin
  inherited Create(True);
  FAttributes := TAttributeList.Create;
  Parent := AParent;
end;

destructor TDomNode.Destroy;
begin
  if FParent <> nil then FParent.Remove(Self);
  FAttributes.Free;
  inherited;
end;

function TDomNode.Duplicate: TDomNode;
var
  I: Integer;
begin
  Result := TDomNode.Create(nil);
  Result.Name := Name;
  Result.NodeType := NodeType;
  Result.Value := Value;
  for I := 0 to Attributes.Count - 1 do
    Result.Attributes.Add(Attributes.Items[I].Name, Attributes.Items[I].Value);
  for I := 0 to Count - 1 do
    Result.AddNode(Items[I].Duplicate).FParent := Result;
end;

function TDomNode.FindNode(const AName: string; Recursive: Boolean): TDomNode;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if SameText(Items[I].Name, AName) then Result := Items[I]
    else if Recursive then Result := Items[I].FindNode(AName, Recursive);
    if Result <> nil then Exit;
  end;
end;

function TDomNode.FindNodes(const AName: string; Recursive: Boolean): TDomNodeList;

  procedure DoFind(ParentNode: TDomNode);
  var
    I: Integer;
  begin
    for I := 0 to ParentNode.Count - 1 do
      with ParentNode do
      begin
        if SameText(Items[I].Name, AName) then Result.Add(Items[I]);
        if Recursive and (NodeType = ntTag) then DoFind(Items[I]);
      end;
  end;

begin
  Result := TDomNodeList.Create(False);
  DoFind(self);
end;

function TDomNode.FirstChild: TDomNode;
begin
  if Count > 0 then Result := Items[0]
  else Result := nil;
end;

function TDomNode.GetIndex: Integer;
begin
  if Assigned(Parent) then Result := Parent.IndexOf(Self)
  else Result := -1;
end;

function TDomNode.Insert(Index: Integer; const AName: string): TDomNode;
begin
  Result := TDomNode.Create(Self);
  Result.Name := AName;
  Result.NodeType := ntTag;
  Result.Index := Index;
end;

function TDomNode.InsertNode(Index: Integer; ANode: TDomNode): TDomNode;
begin
  inherited Insert(Index, ANode);
  Result := ANode;
  ANode.FParent := Self;  
end;

function TDomNode.LastChild: TDomNode;
begin
  if Count > 0 then Result := Items[Count - 1]
  else Result := nil;
end;

function TDomNode.NextSibling: TDomNode;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Parent) then
  begin
    I := Index;
    if I < Parent.Count - 1 then Result := Parent.Items[I + 1];
  end;
end;

function TDomNode.PrevSibling: TDomNode;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Parent) then
  begin
    I := Index;
    if I > 0 then Result := Parent.Items[I - 1];
  end;
end;

procedure TDomNode.SetIndex(Value: Integer);
begin
  // this method will generate exception if new index is invalid
  if Assigned(Parent) then Parent.Move(GetIndex, Value);
end;

procedure TDomNode.SetParent(Value: TDomNode);
begin
  if Value = FParent then Exit;
  if Assigned(FParent) then
  begin
    // extract from the previous parent;
    FParent.Extract(Self);
    FParent := nil;
  end;
  if Assigned(Value) then
  begin
    Value.AddNode(Self);
    FParent := Value;
  end;
end;

function TDomNode.AsString: string;
var
  i: integer;
  ti: THtmlTagInfo;
begin
  Result := '';
  case FNodeType of
    ntComment, ntPI, ntCData: Exit;
    ntText: begin Result := Value; Exit; end;
  end;
  ti := GetTagInfo(name);
  Result := '<' + name;
  for i := 0 to Attributes.Count -1 do
    with Attributes.Items[i] do
      Result := Format('%s %s="%s"', [Result, name, value]);

  if (ti.ClosingType = ctNever) then
    Result := Result + '/>'
  else
    Result := Result + '>' + GetContent + format('</%s>', [name]) ;
end;

function TDomNode.GetContent: string;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    Result := Result + items[i].AsString;
end;

{ TDomDocument }

constructor TDomDocument.Create;
begin
  inherited Create(nil);
end;

procedure TDomDocument.LoadFromFile(const FileName: string);
var
  P: TDomParser;
begin
  Clear;
  Attributes.Clear;
  if FileExists(FileName) then
  begin
    P := TDomParser.Create;
    try
      try
        P.ReadFile(Self, FileName);
      except
        on E: Exception do
        begin
          E.Message :=
            'The error - ' + E.Message +
            '- was encountered in'#13#10 + FileName;
          raise;
        end;
      end;
    finally
      P.Free;
    end;
  end;
end;

procedure TDomDocument.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDomDocument.SaveToStream(Stream: TStream);
var
  P: TDomParser;
  I: Integer;
begin
  P := TDomParser.Create;
  try
    for I := 0 to Count - 1 do
      P.WriteStream(Self.Items[I], Stream, -1);
  finally
    P.Free;
  end;
end;

{ TDomParser }

procedure TDomParser.Parse(Dst: TDomNode);
var
  P, P2: PChar;
  S: string;

  procedure GetTag;
  var
    AttrName, AttrVal: string;
    Err: Boolean;
    N: TDomNode;
    TagInfo: THtmlTagInfo;
    DoHTML: Boolean;
  begin
    Inc(Pos);
    if Pos^ = '/' then
    begin
      Inc(Pos);
      S := GetName(Pos);
      TagInfo := GetTagInfo(S);
      if TagInfo.ElemType <> etUnknown then
      begin
        if TagInfo.ClosingType in [ctNever, ctAnchor] then
        begin
          Inc(Pos);
          Exit;
        end;
      end;
      DomError('Unexpected end tag - ' + S);
    end;
    OmitWhiteSpace(Pos);
    N := Dst.Add(GetName(Pos));
    with N do
    begin
      {$IFDEF CODESITE}CodeSite.EnterMethod('Tag ' + Name);{$ENDIF}
      TagInfo := GetTagInfo(Name);
      DoHTML := TagInfo.ElemType <> etUnknown;
      if DoHTML then Name := LowerCase(Name);

      OmitWhiteSpace(Pos);
      if Pos^ = '/' then Exit;
      while not (AnsiChar(Pos^) in ['>', '/']) do
      begin
        // get attributes
        AttrName := GetName(Pos);
        if AttrName = '' then
          DomError('Unable to get name of the attribute');

        if DoHTML then Name := LowerCase(Name);
        if not GetEq(Pos) then
          DomError('Unable to find attribute value');

        AttrVal := GetAttValue(Pos, Err);
        if Err then
          DomError(Format('Unable to get value of the attribute %s', [AttrName]));

        Attributes.Add(AttrName, AttrVal);
        OmitWhiteSpace(Pos);
        if Pos^ = #0 then
          DomError(Format('Unexpected document end (%s)', [AttrName]));
      end;

      if (TagInfo.ClosingType in [ctNever]) then
      begin
        if Pos^ = '/' then Inc(Pos);
        if Pos^ <> '>' then
          DomError(Name + 'Improper tag closure');
        Inc(Pos);
        if StrLComp(Pos, '</', 2) = 0 then
        begin
          P2 := Pos + 2;
          S := GetName(P2);
          if S = Name then
          begin
            Pos := P2 + 1;
          end;
        end;
        Exit;
      end;

      if Pos^ = '/' then
      begin
        Inc(Pos);
        if Pos^ = '>' then
        begin
          Inc(Pos);
          Exit;
        end
        else DomError('Invalid tag');
      end
      else // if Pos^ = '>';
      begin
        Inc(Pos);

        // read child nodes
        while True do
        begin
          //OmitWhiteSpace(Pos);
          if Pos^ = #0 then DomError('Unexpected document end');

          if (Pos^ = '<') and ((Pos + 1)^ = '/') then
          begin
            // verify end tag
            Inc(Pos, 2);
            S := GetName(Pos);
            if DoHTML then S := LowerCase(S);
            if S <> Name then
              DomError('Unexpected end tag : ' + S + ' ');
            if Pos^ <> '>' then DomError('End tag contains unexpected symbols');
            Inc(Pos);
            Exit;
          end;
          Parse(N);
        end;
      end;
    end;
  end;

  procedure GetText;
  begin
    P := Pos - 1;
    if AnsiChar(P^) in CValidWhiteSpace then S := ' ' else S := '';
    Inc(P);
    while True do
    begin
      case P^ of
        '&': S := S + P^;
        #0 :  DomError('Unexpected null char (#0) in quoted text');
        '<': Break;
      else
        S := S + P^;
      end;
      Inc(P);
    end;
    {$IFDEF CODESITE}CodeSite.EnterMethod('Text ' + S);{$ENDIF}
    Pos := P;

    if GetTagInfo(Dst.Name).SimpleContent then
      Dst.AddText(ConvertWhiteSpace(S))
    else
      Dst.AddText(S);
  end;

  procedure GetPI;
  begin
    P := StrPos(Pos + 2, '?>');
    if P = nil then DomError('Unable to find end of processing instruction');
    with Dst.Add do
    begin
      NodeType := ntPI;
      Name := GetName(Pos);
      Value := SubString(Pos + 1, P);
      {$IFDEF CODESITE}CodeSite.EnterMethod('PI ' + Name);{$ENDIF}
    end;
    Pos := P + 2;
  end;

  procedure GetProlog;
  begin
    GetPI;
  end;

  procedure GetComment;
  begin
    P := StrPos(Pos + 4, '-->');
    if P = nil then DomError('Unable to find end of comment');
    with Dst.Add do
    begin
      NodeType := ntComment;
      Value := SubString(Pos + 4, P);
      {$IFDEF CODESITE}CodeSite.EnterMethod('Comment ' + Value);{$ENDIF}
    end;
    Pos := P + 3;
  end;

  procedure GetDTD;
  begin
    GetTag;
  end;

  procedure GetCDATA;
  begin
    P := StrPos(Pos + 9, ']]>');
    if P = nil then DomError('Unable to find end of CDATA section');
    with Dst.Add do
    begin
      NodeType := ntCDATA;
      Value := SubString(Pos + 9, P);
      {$IFDEF CODESITE}CodeSite.EnterMethod('CDATA ' + Value);{$ENDIF}
    end;
    Pos := P + 3;
  end;

begin
  if Pos^ = '<' then
  begin
    P := Pos + 1;
    if P^ = '?' then
    begin
      if StrLComp(P, '?xml', 3) = 0 then GetProlog
      else GetPI;
    end
    else if P^ = '!' then
    begin
      if StrLComp(P, '!--', 3) = 0 then GetComment
      else if StrLComp(P, '!DOCTYPE', 8) = 0 then GetDTD
      else if StrLComp(P, '![CDATA[', 8) = 0 then GetCDATA
      else GetTag;
    end
    else GetTag;
  end
  else GetText;
  {$IFDEF CODESITE}CodeSite.ExitMethod('');{$ENDIF}
end;

procedure TDomParser.ReadFile(Dst: TDomNode; const FileName: string);
var
  Stream: TStream;
begin
  {$IFDEF CODESITE}CodeSite.SendString('FileName', FileName);{$ENDIF}
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ReadStream(Dst, Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDomParser.ReadStream(Dst: TDomNode; Stream: TStream);
var
  Size: Integer;
  s: AnsiString;
begin
  Size := Stream.Size - Stream.Position;
  if Size = 0 then Exit;
  SetString(s, nil, Size);
  Stream.Read(Pointer(s)^, Size);
  buffer := string(s);
  Dst.Clear;
  Dst.Attributes.Clear;
  Reset;
  Parse(Dst);
end;

procedure TDomParser.ReadString(Dst: TDomNode; const AString: string);
begin
  if AString = '' then Exit;
  Buffer := AString;
  Reset;
  Dst.Clear;
  Dst.Attributes.Clear;
  Parse(Dst);
end;

procedure TDomParser.Reset;
begin
  Pos := Pointer(Buffer);
  OmitWhiteSpace(Pos);
end;

procedure TDomParser.WriteStream(Src: TDomNode; Stream: TStream; Level: Integer);
var
  I: Integer;
  TagInfo: THtmlTagInfo;
  NLS: AnsiString;

  procedure Write(const S: AnsiString);
  begin
    if (Length(NLS) > 0) and (Stream.Position > 0) then
      Stream.WriteBuffer(Pointer(NLS)^, Length(NLS));
    Stream.WriteBuffer(Pointer(S)^, Length(S));
    NLS := '';
  end;

  procedure NewLine;
  begin
    if Level > 0 then NLS := AnsiString(#13#10 + StringOfChar(' ', Level * 2))
    else NLS := AnsiString(#13#10);
  end;

begin
  case Src.NodeType of
    ntText:
      begin
        Write(AnsiString(Src.Value));
      end;

    ntComment:
      begin
        NewLine;
        Write('<!--' + AnsiString(Src.Value) + '-->');
        NewLine;
      end;

    ntPI:
      begin
        NewLine;
        Write(AnsiString('<?' + Src.Name + ' ' + Src.Value + '?>'));
        NewLine;
      end;

    ntTag:
      begin
        if SameText(Src.Attributes['id'], 'hidden') then
          Exit;

        //treat empty tags as a forced newline
        if Src.Name = '' then
        begin
          Stream.WriteBuffer(#13#10, 2);
          NewLine;
          Exit;
        end;

        TagInfo := GetTagInfo(Src.Name);
        if TagInfo.ElemType = etBlock then NewLine;

        // opening tag
        Write('<' + AnsiString(Src.Name));
        with Src.Attributes do for I := 0 to Count - 1 do
          Write(AnsiString(Format(' %s="%s"', [Items[I].Name, Items[I].Value])));

        if Src.Count = 0 then
        begin
          if TagInfo.ElemType = etUnknown then
          begin
            Write('/>');
            NewLine;
          end
          else
          begin
            Write('>');
            if TagInfo.ElemType = etBlock then NewLine;
            if TagInfo.ElemType = etBR then NewLine;
            if TagInfo.ClosingType = ctAlways then
            begin
              Write('</' + AnsiString(Src.Name) + '>');
              if TagInfo.ElemType = etBlock then NewLine;
              if TagInfo.ElemType = etBR then NewLine;
            end;
          end;
        end
        else
        begin
          Write('>');

          // child elements
          //if not TagInfo.SimpleContent then NewLine;

          for I := 0 to Src.Count - 1 do
            WriteStream(Src.Items[I], Stream, Level + 1);

          if not TagInfo.SimpleContent then NewLine;

          // closing tag
          Write('</' + AnsiString(Src.Name) + '>');

          if TagInfo.ElemType = etBlock then NewLine;
        end;
      end;
  end;
end;

end.
