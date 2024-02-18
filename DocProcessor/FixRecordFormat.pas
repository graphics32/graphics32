unit FixRecordFormat;

interface

uses
  Windows, SysUtils, Classes, Utils;

type

  TAttrib = record
    name: string;
    content: string;
  end;
  TArrayOfTAttrib = array of TAttrib;

  TElem = class;
  TArrayOfTElem = array of TElem;

  TElem = class
  public
    name    : string;
    text    : string;
    level   : integer;
    idx     : integer;
    flag    : cardinal;
    attribs : TArrayOfTAttrib;
    parent  : TElem;
    childs  : TArrayOfTElem;
    destructor Destroy; override;
    function AddChild: TElem;
    function RenderContent: string;
  end;

function FixAllRecords(const rootFolder: string): integer;
function FixRecordLayout(const filename: string): Boolean;

implementation

const
  unpairedTags: array[0..2] of string =
    ('br', 'meta', 'link');
  majorTags: array[0..3] of string =
    ('html', 'head', 'body', 'p');
var
  recordFound: Boolean;

function TElem.AddChild: TElem;
begin
  Result := TElem.Create;
  Result.parent := self;
  Result.level := level +1;
  Result.idx := Length(childs);
  setLength(childs, Result.idx +1);
  childs[Result.idx] := Result;
end;

destructor TElem.Destroy;
var
  i: integer;
begin
  for i := High(childs) downto 0 do
    childs[i].Free;
  inherited;
end;

function IsUnpairedElement(name:string): Boolean;
var
  i: integer;
begin
  name := LowerCase(name);
  for i := 0 to High(unpairedTags) do
    if SameStr(name, unpairedTags[i]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function IsMajorElement(name:string): Boolean;
var
  i: integer;
begin
  name := LowerCase(name);
  for i := 0 to High(majorTags) do
    if SameStr(name, majorTags[i]) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function PutRecordInTable(const str: string): string;
var
  i,j,len: integer;
  s: string;
begin
  len := length(str);
  Result := '<table style="padding:0 20px; border:0">'#13#10;
  //record header
  j := Pos('<br>', str);
  if j = 0 then Exit;
  result := result + '<tr><td colspan="2">'+
    copy(str, 1,j -1) + '</td></tr>'#13#10;
  //loop through each row
  while j < len - 20 do
  begin
    i := j + 6; //ie skip over <br>#13#10

    //check for comments
    j := Pos('<span class="Comment"', str, i);
    if j = i then
    begin
      j := Pos('</span>', str, i);
      result := result + '<tr><td class="field" colspan="2">' +
        copy(str, i,j-i + 7) + '</td></tr>'#13#10;
      inc(j, 7);
      continue;
    end;


    //get field
    j := Pos(':', str, i);
    if j = 0 then
    begin
      result := str;
      Exit;
    end;
    s := copy(str, i,j-i);
    if pos(',', s) > 0 then
      result := result + '<tr><td class="field" style="width=%50">' +
        s + '</td><td class="type">'
    else
      result := result + '<tr><td class="field">' +
        s + '</td><td class="type">';


    //get field's type
    i := j;
    j := Pos('<br>', str, i);
    if j = 0 then
    begin
      result := str;
      Exit;
    end;
    result := result + copy(str, i,j-i) + '</td></tr>'#13#10;
  end;
  result := result +
    '<tr><td><b>end</b>;</td><td></td></tr>'#13#10'</table>'#13#10;
end;

function TElem.RenderContent: string;
var
  i,j, recStart: integer;
  isMajorTag: Boolean;
begin
  recStart := 0;
  Result := text;
  for i := 0 to High(childs) do
    with childs[i] do
    begin
      if name = '' then
      begin
        Result := Result + RenderContent;
      end else
      begin
        isMajorTag := IsMajorElement(name);
        Result := Result + '<' + name;
        for j := 0 to High(attribs) do
          with attribs[j] do
            Result := Result + ' ' + name + '="' + content + '"';
        Result := Result + '>';
        if isMajorTag then
          Result := Result + #13#10;
        if (flag = 1) then
        begin
          recStart := Length(Result) +1;
        end;
        if IsUnpairedElement(name) then
        begin
          Result := Result + #13#10;
        end else
        begin
          Result := Result + RenderContent;

          if (recStart > 0) then
          begin
            Result := Copy(Result, 1, recStart -1) +
              PutRecordInTable(Copy(Result, recStart, 4096));
            recStart := 0;
          end;

          Result := Result + '</' + name + '>';
          if isMajorTag then
            Result := Result + #13#10;
        end;
      end;
    end;
end;

procedure SkipWhiteSpace(const s: string; var i: integer; len: integer);
begin
  while (i <= len) and (s[i] <= #32) do inc(i);
end;

function GetString(const s: string; var i: integer; len: integer): string;
var
  j: integer;
begin
  j := i;
  inc(i);
  while (i <= len) and
    CharInSet(s[i], ['a'..'z','A'..'Z','0'..'9','_']) do inc(i);
  result := Lowercase(Copy(s, j, i-j));
end;

function GetAttrib(const s: string; var i: integer;
  len: integer; elem: TElem): Boolean;
var
  j: integer;
  name, content: string;
begin
  name := GetString(s, i, len);
  result := (i < len) and (s[i] = '=') and (s[i +1] = '"');
  if not result then Exit;
  inc(i, 3); j := i -1;
  while (i <= len) and (s[i] <> '"') do inc(i);
  content := Copy(s, j, i-j);
  j := Length(elem.attribs);
  setLength(elem.attribs, j+1);
  elem.attribs[j].name := name;
  elem.attribs[j].content := content;
  inc(i); //skip end DQuote
  SkipWhiteSpace(s, i, len);
end;

procedure GetElemContents(const s: string; var i: integer;
  len: integer; elem: TElem);
var
  j: integer;
  name: string;
  subEl: TElem;
begin
  j := i;
  SkipWhiteSpace(s, i, len);
  while (i < len) do
  begin
    if (s[i] <> '<') then
    begin
      subEl := elem.AddChild;
      if (j < i) and (s[i-1] = #32) then
        j := i -1 else //allow a preceeding space char
        j := i;
      Inc(i);
      while (i < len) and (s[i] <> '<') do inc(i);
      subEl.text := Copy(s, j, i - j);

      //flag 'p' containing record defs for later
      if (elem.name = 'b') and (elem.parent.name = 'p') and
        (Length(elem.parent.attribs) > 0) and
        (elem.parent.attribs[0].content = 'Decl') then
      begin
        if (SameText(subEl.text, 'record') or
        SameText(subEl.text, 'packed record')) then
        begin
          elem.parent.flag := 1;
          recordFound := true;
        end
        else if SameText(subEl.text, 'case') then
        begin
          elem.parent.flag := 0;
          recordFound := false;
        end;
      end;

    end else
    begin
      inc(i);

      if (s[i] = '/') then //ie ending this element
      begin
        inc(i); name := GetString(s, i, len);
        if not SameStr(name, elem.name) or (s[i] <> '>') then
          i := len; //error
        inc(i);
        Exit;
      end;

      //we're starting a new subelement
      subEl := elem.AddChild;

      //get name
      subEl.name := GetString(s, i, len);

      SkipWhiteSpace(s, i, len);
      //get attribs
      while (i < len) and not CharInSet(s[i], ['/','>']) and
        GetAttrib(s, i, len, subEl) do;

      if not CharInSet(s[i], ['/','>']) then
      begin
        i := len;
      end
      else if (s[i] = '/') then
      begin
        if s[i+1] <> '>' then i := len;
        inc(i, 2);
        Exit;
      end
      else if IsUnpairedElement(subEl.name) then
      begin
        inc(i); //skip '>'
      end else
      begin
        inc(i); //skip '>'
        //this is either text or a subelement
        GetElemContents(s, i, len, subEl);
      end;
    end;
    SkipWhiteSpace(s, i, len);
  end;
end;

function Parse(s: string): string;
var
  i, len: integer;
  topElem: TElem;
begin
  recordFound := false;
  i := 1;
  len  := length(s);
  topElem := TElem.Create;
  GetElemContents(s, i, len, topElem);
  Result := topElem.RenderContent;
  topElem.Free;
end;

function FixRecordLayout(const filename: string): Boolean;
begin
  result := False;
  if not FileExists(filename) then Exit;
  with TStringList.create do
  try
    LoadFromFile(filename);
    text := Parse(text);
    if recordFound then
    begin
      result := true;
      //RenameFile(filename, ChangeFileExt(filename,'__.htm'));
      SaveToFile(filename);
    end;
  finally
    free;
  end;
end;

function FindFiles(const folder: string): integer;
var
  sr: TSearchRec;
  i, srResult: integer;
  fileList: array of string;
begin
  i := 0;
  fileList := nil;
  srResult := FindFirst(folder + '\*.htm', faAnyFile, sr);
  while srResult = 0 do
  begin
    inc(i);
    setLength(fileList, i);
    fileList[i-1] := folder + '\' + sr.Name;
    srResult := FindNext(sr);
  end;
  FindClose(sr);

  Result := 0;
  for i := 0 to High(fileList) do
    if FixRecordLayout(fileList[i]) then inc(Result);
end;

function FindInSubfolders(const folder: string): integer;
var
  sr: TSearchRec;
  srResult: integer;
begin
  Result := 0;
  srResult := FindFirst(folder + '\*.*', faDirectory, sr);
  while srResult = 0 do
  begin
    if (sr.Attr and faDirectory = 0) or
      (sr.Name[1] = '.') then //do nothing
    else if sr.Name = 'Types' then
      inc(Result, FindFiles(folder+'\Types'))
    else
      inc(Result, FindInSubfolders(folder + '\' + sr.Name));
    srResult := FindNext(sr);
  end;
  FindClose(sr);
end;

function FixAllRecords(const rootFolder: string): integer;
var
  i: integer;
  folder: string;
begin
  Result := 0;
  folder := rootFolder;
  i := Length(folder);
  if (i > 0) and (folder[i] <> '\') then
    folder := folder + '\source' else
    folder := folder + 'source';
  if DirectoryExists(folder) then
    result := FindInSubfolders(folder);
end;

end.
