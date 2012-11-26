unit Pas2Html;

// Regarding building new units from existing PAS files ...
// 1. Comments directly preceeding declarations in the header section of PAS
//    files will be imported as declaration descriptions into the help file.
// 2. Images can also be flagged for import by using <img src="filename">
//    Format. Images must be located in the PAS file's folder and the filename
//    must not contain a Path. Images will be copied to the Images folder.
// 3. Extended comments (sample code etc) can be flagged for import by using
//    the custom <include src="filename"> Format. Again the file for importing
//    must be in the PAS file's folder and the filename must not contain a path.

{$I DocProcessor.inc}

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, Controls, DelphiParse, ShellApi,
  ShlObj;

  function BuildNewUnit(const PasFilename, DestUnitFolder, ProjectFolder: TFileName): Integer;
  function DeleteFolder(const FolderName: TFileName): Boolean;

implementation

uses
  StrUtils;

const
  htmlEnd: string = #10'<p class="Body"></p>'#10#10'</body>'#10'</html>';
  cr: AnsiChar = #10;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function LevelToEllipsis(Level: Integer): string;
begin
  Result := '';
  for Level := 1 to Level do
    Result := Result + '../';
end;
//------------------------------------------------------------------------------

function HtmlStart(Level: Integer; const metaTag: string = ''): string;
const
  HtmlStart1: string =
    '<html>'#10'<head>'#10'<title>G32Version</title>'#10'<link rel="stylesheet" href="';
  HtmlStart2: string = 'styles/default.css" type="text/css">'#10;
  HtmlStart3: string = '</head>'#10'<body bgcolor="#FFFFFF">'#10;
begin
  Result := HtmlStart1 + LevelToEllipsis(Level) + HtmlStart2 + metaTag + HtmlStart3;
end;
//------------------------------------------------------------------------------

var
  GBuffer: string;

{$IFNDEF UNICODE}
function CharInSet(c: AnsiChar; chrs: TSysCharSet): boolean;
begin
  result := c in chrs;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure AddToBuffer(const Tok: TToken); overload;
var
  Len: Integer;
  AvoidSpace, ForceSpace: Boolean;
begin
  Len := Length(GBuffer);
  AvoidSpace := (Len > 0) and CharInSet(GBuffer[Len], ['^','@','(','[','.']);
  case Tok.kind of
    tkReserved:
      if (Len > 0) and not AvoidSpace then
        GBuffer := GBuffer + ' <b>' + Tok.Text + '</b>' else
        GBuffer := GBuffer + '<b>' + Tok.Text + '</b>';
    tkText:
      if Len > 0 then
        GBuffer := GBuffer + ' ''' + Tok.Text + '''' else
        GBuffer := GBuffer + '''' + Tok.Text + '''';
    tkIdentifier, tkValue, tkAsm:
      if (Len > 0) and not AvoidSpace then
        GBuffer := GBuffer + ' '+ Tok.Text else
        GBuffer := GBuffer + Tok.Text;
    tkSymbol:
      begin
        ForceSpace := (Len > 0) and CharInSet(GBuffer[Len], [':']);
        if ForceSpace then
          GBuffer := GBuffer + ' '+ Tok.Text
        else if CharInSet(Tok.Text[1], [':', ';', ',', '(', ')', ']', '^', SINGLEQUOTE, '"', '.']) then
          GBuffer := GBuffer + Tok.Text
        else
          GBuffer := GBuffer + ' '+ Tok.Text;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure AddToBuffer(const str: string); overload;
begin
  GBuffer := GBuffer + str;
end;
//------------------------------------------------------------------------------

procedure ClearBuffer;
begin
 GBuffer := '';
end;
//------------------------------------------------------------------------------

function StripSlash(const Path: AnsiString): AnsiString;
var
  Len: Integer;
begin
  Result := Path;
  Len := Length(Path);
  if (Len = 0) or (Path[Len] <> '\') then
    Exit;
  SetLength(Result, Len - 1);
end;
//------------------------------------------------------------------------------

function BrowseProc(hwnd: HWnd; uMsg: Integer; lParam, lpData: LPARAM): Integer; stdcall;
var
  sfi: TSHFileInfo;
begin
  case uMsg of
    BFFM_INITIALIZED:
      begin
        SendMessage(hwnd, BFFM_SETSTATUSTEXT,0, lpData);
        SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
      end;
    BFFM_SELCHANGED:
      begin
        ShGetFileInfo(PChar(lParam), 0, sfi,SizeOf(sfi),SHGFI_DISPLAYNAME or SHGFI_PIDL);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT,0, Integer(@sfi.szDisplayName));
      end;
  end;
  Result := 0;
end;
//------------------------------------------------------------------------------

function TrimSlash(const Path: string): string;
var
  i: Integer;
begin
  Result := Path;
  i := Length(Path);
  if (i > 0) and (Path[i] = '\') then Delete(Result, i, 1);
end;
//------------------------------------------------------------------------------

function ShellFileOperation(fromFile: string; toFile: string; Flag: Integer): Boolean;
var
  ShelliIfo: TSHFileOpStruct;
begin
  FillChar(ShelliIfo, SizeOf(ShelliIfo), 0);
  with ShelliIfo do
  begin
    wnd   := Application.Handle;
    wFunc := Flag; //FO_MOVE, FO_COPY, FO_DELETE or FO_RENAME
    pFrom := PChar(fromFile);
    pTo   := PChar(toFile);
  end;
  Result := SHFileOperation(ShelliIfo) = 0;
end;
//------------------------------------------------------------------------------

function DeleteFolder(const FolderName: TFileName): Boolean;
begin
  Result := DirectoryExists(FolderName) and
    ShellFileOperation(TrimSlash(FolderName), '', FO_DELETE);
end;
//------------------------------------------------------------------------------

function AnsiStringFromFile(const FileName: TFileName): AnsiString;
begin
  with TMemoryStream.Create do
  try
    LoadFromFile(FileName);
    SetString(Result, PAnsiChar(Memory), Size);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function StringFromFile(const FileName: TFileName): string;
begin
  Result := string(AnsiStringFromFile(FileName));
(*
  with TMemoryStream.Create do
  try
    LoadFromFile(FileName);
    SetString(Result, PChar(Memory), Size);
  finally
    Free;
  end;
*)
end;
//------------------------------------------------------------------------------

procedure AnsiStringToFile(const FileName: TFileName; StrVal: AnsiString);
begin
  with TMemoryStream.Create do
  try
    Size := Length(StrVal);
    if Size > 0 then
      Move(StrVal[1], PAnsiChar(Memory)^, Size);
    SaveToFile(FileName);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure StringToFile(const FileName: TFileName; StrVal: string);
begin
  AnsiStringToFile(FileName, AnsiString(StrVal));
(*
  with TMemoryStream.Create do
  try
    Size := Length(StrVal);
    if Size > 0 then
      Move(StrVal[1], PChar(Memory)^, Size);
    SaveToFile(FileName);
  finally
    Free;
  end;
*)
end;
//------------------------------------------------------------------------------

procedure AppendAnsiStringToFile(const FileName: TFileName; StrVal: AnsiString);
var
  i, Len, OldSize: Cardinal;
begin
  Len := Length(StrVal);
  if Len = 0 then Exit;
  with TMemoryStream.Create do
  try
    if FileExists(FileName) then LoadFromFile(FileName);
    OldSize := Size;
    if OldSize > 0 then i := SizeOf(cr) else i := 0;
    Size := OldSize + Len + i;
    if OldSize > 0 then Move(cr, (PAnsiChar(Memory)+ OldSize)^, SizeOf(cr));
    Move(StrVal[1], (PAnsiChar(Memory) + OldSize + i)^, Len);
    SaveToFile(FileName);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure AppendStringToFile(const FileName: TFileName; StrVal: string);
(*
var
  i, Len, OldSize: Cardinal;
*)
begin
  AppendAnsiStringToFile(FileName, AnsiString(StrVal));
(*
  Len := Length(StrVal);
  if Len = 0 then Exit;
  with TMemoryStream.Create do
  try
    if FileExists(FileName) then LoadFromFile(FileName);
    OldSize := Size;
    if OldSize > 0 then i := SizeOf(cr) else i := 0;
    Size := OldSize + Len + i;
    if OldSize > 0 then Move(cr, (PChar(Memory)+ OldSize)^, SizeOf(cr));
    Move(StrVal[1], (PChar(Memory) + OldSize + i)^, Len);
    SaveToFile(FileName);
  finally
    Free;
  end;
*)
end;
//------------------------------------------------------------------------------

procedure PrependAnsiStringToFile(const FileName: TFileName; StrVal: AnsiString);
var
  i, Len, OldSize: Cardinal;
begin
  Len := Length(StrVal);
  if Len = 0 then Exit;
  with TMemoryStream.Create do
  try
    if FileExists(FileName) then LoadFromFile(FileName);
    OldSize := Size;
    if OldSize > 0 then i := SizeOf(cr) else i := 0;
    Size := OldSize + Len + i;
    if OldSize > 0 then Move(PAnsiChar(Memory)^, (PAnsiChar(Memory)+ Len + i)^, OldSize);
    Move(StrVal[1], PAnsiChar(Memory)^, Len);
    if OldSize > 0 then Move(cr, (PAnsiChar(Memory)+Len)^, SizeOf(cr));
    SaveToFile(FileName);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure PrependStringToFile(const FileName: TFileName; StrVal: string);
(*
var
  i, Len, OldSize: Cardinal;
*)
begin
  PrependAnsiStringToFile(FileName, AnsiString(StrVal));
(*
  Len := Length(StrVal);
  if Len = 0 then Exit;
  with TMemoryStream.Create do
  try
    if FileExists(FileName) then LoadFromFile(FileName);
    OldSize := Size;
    if OldSize > 0 then i := SizeOf(cr) else i := 0;
    Size := OldSize + Len + i;
    if OldSize > 0 then Move(PChar(Memory)^, (PChar(Memory)+ Len + i)^, OldSize);
    Move(StrVal[1], PChar(Memory)^, Len);
    if OldSize > 0 then Move(cr, (PChar(Memory)+Len)^, SizeOf(cr));
    SaveToFile(FileName);
  finally
    Free;
  end;
*)
end;
//------------------------------------------------------------------------------

function FirstWordInStr(const s: AnsiString): AnsiString; overload;
var
  i, Len: Integer;
begin
  Len := Length(s);
  if Len = 0 then Result := ''
  else
  begin
    i := 1;
    while (i <= Len) and (s[i] in ['a'..'z','A'..'Z','0'..'9']) do
      Inc(i);
    Result := Copy(s, 1, i - 1);
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF UNICODE}
function FirstWordInStr(const s: string): string; overload;
var
  i, Len: Integer;
begin
  Len := Length(s);
  if Len = 0 then Result := ''
  else
  begin
    i := 1;
    while (i <= Len) and CharInSet(s[i], ['a'..'z','A'..'Z','0'..'9']) do
      Inc(i);
    Result := Copy(s, 1, i - 1);
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function BuildNewUnit(const PasFilename, DestUnitFolder, ProjectFolder: TFileName): Integer;
var
  i: Integer;
  PasLines: TStringlist;
  DelphiParser: TDelphiParser;
  ConstList, VarList, RoutinesList: TStringList;
  Tok: TToken;
  s: string;
  fn: TFileName;
  Comment: string;

  function MakeDescription(Level: Integer; Comment: string): string;
  var
    i,j: Integer;
    ImgFile, IncFile, IncStr: string;
    QuoteChar: Char;
  begin
    //add any 'includes' into the comment ...
    //Format <include src="filename">
    i := 1;
    while True do
    begin
      i := PosEx('<include src=', Comment, i);
      if i = 0 then
        Break;
      QuoteChar := Comment[i + 13];
      if not CharInSet(QuoteChar, ['"', '''']) then
        Break;
      j := PosEx(QuoteChar, Comment, i + 14);
      if j = 0 then
        Break;
      IncFile := Copy(Comment, i + 14, j - (i + 14));
      j := PosEx('>', Comment, j);
      if j = 0 then
        Break;
      Delete(Comment, i, j - i + 1);
      IncStr := StringFromFile(ExtractFilePath(PasFilename) + IncFile);
      IncStr := Trim(IncStr);
      //replace tabs with double-spaces ...
      IncStr := StringReplace(IncStr, #9, '  ', [rfReplaceAll]);
      Insert(IncStr, Comment, i);
      Break; //ie assumes a maximum of one 'include' statement
    end;

    //Move any images into the image folder and fixup the Path in Comment ...
    i := 1;
    while True do
    begin
      i := PosEx('<img src=', Comment, i);
      if i = 0 then
        Break;
      QuoteChar := Comment[i + 9];
      if not CharInSet(QuoteChar, ['"', '''']) then
        Break;
      j := PosEx(QuoteChar, Comment, i + 10);
      if j = 0 then Break;
      ImgFile := Copy(Comment, i + 10, j - (i + 10));
      Insert(LevelToEllipsis(Level) + 'Images/', Comment, i+10);
      CopyFile( PChar(ExtractFilePath(PasFilename) +ImgFile),
        PChar(projectFolder + 'Images/' +ImgFile), False );
      i := j+1;
    end;

    //Delete spaces that trail the <br> token ...
    i := 1;
    while True do
    begin
      i := PosEx('<br> ',Comment,i);
      if i = 0 then Break;
      Delete(Comment, i + 4, 1);
    end;
    while True do
    begin
      i := PosEx('<br/> ', Comment, i);
      if i = 0 then Break;
      Delete(Comment, i + 5, 1);
    end;

    if Comment = '' then
      Result := '<br>' else
      Result := '<p class="Body">'#10 + Comment + '</p>'#10;
  end;

  function DoConst: Boolean;
  var
    Ident: string;
  begin
    Result := False;
    with DelphiParser do
    begin
      while True do
      begin
        peekNextToken(Tok);
        if (Tok.kind <> tkIdentifier) then Break;
        GetNextToken(Tok); //gobble peek
        Ident := Tok.Text;
        Comment := LastSpecialComment;
        ClearBuffer;
        repeat
          GetNextToken(Tok);
          AddToBuffer(Tok);
        until Finished or (Tok.Text = ';');
        Result := Tok.Text = ';';
        if Result then
          ConstList.Add(
            Format('<p class="Decl">%s %s</p>'#10,[Ident, GBuffer])+
            MakeDescription(4, Comment) + '<br><br>'#10);
      end;
    end;
    //add a space between each CONST code block ...
    ConstList.Add('<br>'#10);
  end;

  function DoVars: Boolean;
  var
    Ident: string;
    HasBracket: Boolean;
  begin
    Result := False;
    with DelphiParser do
    begin
      while True do
      begin
        peekNextToken(Tok);
        if (Tok.kind <> tkIdentifier) then Exit;
        GetNextToken(Tok); //gobble peek
        Ident := Tok.Text;
        GetNextToken(Tok);
        if Tok.Text <> ':' then Exit;
        ClearBuffer;
        Comment := LastSpecialComment;
        AddToBuffer(Ident + ':');
        HasBracket := False;
        repeat
          GetNextToken(Tok);
          if (Tok.Text = '(') then HasBracket := True
          else if (Tok.Text = ')') then HasBracket := False;
          AddToBuffer(Tok);
        until Finished or (not HasBracket and (Tok.Text = ';'));
        Result := Tok.Text = ';';
        PeekNextToken(Tok);
        if Tok.Text = 'stdcall' then
        begin
          GetNextToken(Tok);
          GetNextToken(Tok);
          AddToBuffer(Tok);
        end;
        if Result then
          VarList.Add(MakeDescription(4, Comment) +
            Format('<p class="Decl">%s</p>'#10,[GBuffer]));
      end;
    end;
    //add a space between each VAR code block ...
    VarList.Add('<br>'#10);
  end;

  function DoFunction: string;
  var
    HasBracket: Boolean;
  begin
    Result := '';
    with DelphiParser do
    begin
      GetNextToken(Tok);
      if not (Tok.kind in [tkIdentifier, tkReserved]) then Exit;
      ClearBuffer;
      AddToBuffer(Tok);
      HasBracket := False;
      repeat
        GetNextToken(Tok);
        if Tok.Text = '(' then HasBracket := True
        else if Tok.Text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.Text = ':'));
      if Tok.Text <> ':' then Exit;
      repeat
        GetNextToken(Tok);
        AddToBuffer(Tok);
      until Finished or (Tok.Text = ';');
      if (Tok.Text <> ';') then Exit;
      while True do
      begin
        PeekNextToken(Tok);
        if (Tok.kind = tkReserved) and ((Tok.Text = 'overload') or
          (Tok.Text = 'override') or (Tok.Text = 'virtual') or
          (Tok.Text = 'abstract') or (Tok.Text = 'dynamic') or
          (Tok.Text = 'reintroduce') or (Tok.Text = 'inline') or
          (Tok.Text = 'stdcall')) then
            AddToBuffer(Tok) else
            Break;

        GetNextToken(Tok); //ie gobbles peek
        GetNextToken(Tok);
        if Tok.Text <> ';' then Exit;
        AddToBuffer(';');
      end;
      Result := GBuffer;
    end;
  end;

  function DoProcedure: string;
  var
    HasBracket: Boolean;
  begin
    Result := '';
    with DelphiParser do
    begin
      GetNextToken(Tok);
      if not (Tok.kind in [tkIdentifier, tkReserved]) then Exit;
      ClearBuffer;
      AddToBuffer(Tok);
      HasBracket := False;
      repeat
        GetNextToken(Tok);
        if Tok.Text = '(' then HasBracket := True
        else if Tok.Text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.Text = ';'));
      if Tok.Text <> ';' then Exit;
      while True do
      begin
        PeekNextToken(Tok);
        if (Tok.kind = tkReserved) and ((Tok.Text = 'overload') or
          (Tok.Text = 'override') or (Tok.Text = 'virtual') or
          (Tok.Text = 'abstract') or (Tok.Text = 'dynamic')or
          (Tok.Text = 'reintroduce') or (Tok.Text = 'inline') or
          (Tok.Text = 'stdcall')) then
            AddToBuffer(Tok) else
            Break;
        GetNextToken(Tok); //ie gobbles peek
        GetNextToken(Tok);
        if Tok.Text <> ';' then Exit;
        AddToBuffer(';');
      end;
      Result := GBuffer;
    end;
  end;

  function DoProperty: string;
  var
    inSqrBracket, doRead, doWrite: Boolean;

    function PropertyValid(Token: TToken): Boolean;
    const
      CValidPropertyNames: array [0..34] of string = ('absolute', 'abstract',
        'assembler', 'cdecl', 'contains', 'default', 'dispid', 'dynamic',
        'export', 'external', 'far', 'forward', 'message', 'near', 'platform',
        'on', 'override', 'overload', 'out', 'package', 'pascal', 'protected',
        'private', 'program', 'public', 'published', 'read', 'reintroduce',
        'write', 'register', 'reintroduce', 'requires', 'safecall', 'stdcall',
        'virtual'); // label ?!
    var
      Index: Integer;
    begin
      Result := (Token.Kind = tkIdentifier); // or (Tok.kind = tkReserved);
      if (not Result) and (Tok.kind = tkReserved) then
        for Index := 0 to High(CValidPropertyNames) do
          if Token.Text = CValidPropertyNames[Index] then
          begin
            Result := True;
            Exit;
          end;
    end;

  begin
    Result := '';
    ClearBuffer;
    with DelphiParser do
    begin
      GetNextToken(Tok);
      if not PropertyValid(Tok) then
        Exit;
      AddToBuffer(Tok);
      PeekNextToken(Tok);
      if Tok.Text = ';' then
      begin
        GetNextToken(Tok); //gobble the peek
        AddToBuffer(';');
        Result := GBuffer;
        Exit;              //ie just elevated the property's visibility
      end;
      inSqrBracket := False;
      doRead := False;
      doWrite := False;
      repeat
        GetNextToken(Tok);
        AddToBuffer(Tok);
        if Tok.Text = '[' then inSqrBracket := True
        else if Tok.Text = ']' then inSqrBracket := False;
      until Finished or (not inSqrBracket and (Tok.Text = ':'));
      GetNextToken(Tok);
      if not (Tok.kind in [tkIdentifier, tkReserved]) then Exit;
      AddToBuffer(Tok);
      AddToBuffer(';');
      //now skip the rest of the property stuff (ie read, write etc) ...
      repeat
        GetNextToken(Tok);
        if Tok.Text = 'read' then doRead := True
        else if Tok.Text = 'write' then doWrite := True;
      until Finished or (Tok.Text = ';');
      if Tok.Text <> ';' then Exit;

      while True do
      begin
        PeekNextToken(Tok);
        if (Tok.Text = 'default') or (Tok.Text = 'stored') then
        repeat
          GetNextToken(Tok);
        until Finished or (Tok.Text = ';')
        else Break;
      end;

      if doRead and doWrite then
        AddToBuffer(' <span class="Comment">//read and write</span>')
      else if doRead then
        AddToBuffer(' <span class="Comment">//read only</span>')
      else if doWrite then
        AddToBuffer(' <span class="Comment">//write only</span>');

      Result := GBuffer;
    end;
  end;

  function DoClass(const ClsName: string): Boolean;
  var
    s, s2, fn, Ancestor, ClassPath: string;
  begin
    with DelphiParser do
    begin
      GetNextToken(Tok);
      Result := Tok.Text = ';';
      if Result then Exit; //ie ignore forward class declarations
      if (Tok.Text = '(') then
      begin
        Ancestor := '';
        repeat
          GetNextToken(Tok);
          if Tok.Text <> ')' then Ancestor := Ancestor + Tok.Text;
        until Finished or (Tok.Text = ')');
        if Tok.Text <> ')' then Exit;
        GetNextToken(Tok);
        Result := Tok.Text = ';';
        if Result then Exit; //ie ignore forward class declaration
      end;
      if not DirectoryExists(DestUnitFolder + 'Classes') then
        MkDir(DestUnitFolder + 'Classes');
      ClassPath := DestUnitFolder + 'Classes/' + ClsName + '/';
      if not DirectoryExists(ClassPath) then
        MkDir(ClassPath);
      Ancestor := '<meta name="Ancestor" content="' + Ancestor + '">'#10;
      StringToFile(ClassPath + '_Body.htm',
        HtmlStart(5, Ancestor) + MakeDescription(5, Comment) + htmlEnd);

      repeat
        //skip private and protected class fields and methods...
        if (Tok.Text = 'private') or (Tok.Text = 'protected') then
        repeat
          GetNextToken(Tok);
        until Finished or (Tok.Text = 'public') or
          (Tok.Text = 'published') or (Tok.Text = 'end');

        if (Tok.Text = 'end') then
        begin
          GetNextToken(Tok);
          Result := Tok.Text = ';';
          Break;
        end
        else if (Tok.Text = 'public') or (Tok.Text = 'published') then
          GetNextToken(Tok);

        case Tok.kind of
          tkIdentifier:
            begin
              ClearBuffer;
              AddToBuffer(Tok);
              repeat
                GetNextToken(Tok);
                AddToBuffer(Tok);
              until Finished or (Tok.Text = ';');
              if Tok.Text <> ';' then Break;
              fn := ClassPath + 'Fields.htm';
              AppendStringToFile(fn, Format('<p class="Decl">%s</p>'#10,[GBuffer]));
              GetNextToken(Tok);
            end;
          tkReserved:
            if (Tok.Text = 'constructor') or (Tok.Text = 'destructor') or
              (Tok.Text = 'procedure') then
            begin
              s := Tok.Text;
              Comment := LastSpecialComment;
              s2 := DoProcedure;
              if s2 = '' then Exit;
              if not DirectoryExists(ClassPath + 'Methods') then
                MkDir(ClassPath + 'Methods');
              fn := ClassPath + 'Methods/' + FirstWordInStr(s2)+ '.htm';
              AppendStringToFile(fn,Format('<p class="Decl"><b>%s</b> %s</p>'#10,[s,s2]) +
                MakeDescription(6, Comment));
              if RoutinesList.IndexOf(fn) < 0 then
                RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.Text = 'function') then
            begin
              Comment := LastSpecialComment;
              s := DoFunction;
              if s = '' then Exit;
              if not DirectoryExists(ClassPath + 'Methods') then
                MkDir(ClassPath + 'Methods');
              fn := ClassPath  + 'Methods/' + FirstWordInStr(s) + '.htm';
              AppendStringToFile(fn, '<p class="Decl"><b>function</b> ' +
                  s + '</p>' +MakeDescription(6, Comment));
              if RoutinesList.IndexOf(fn) < 0 then
                RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.Text = 'class') then
            begin
              ClearBuffer;
              AddToBuffer(Tok);

              Comment := LastSpecialComment;
              GetNextToken(Tok);
              if (Tok.Text = 'procedure') or (Tok.Text = 'function') then
              begin
                if Tok.Text = 'procedure' then
                begin
                  s := 'procedure';
                  s2 := DoProcedure;
                end else if Tok.Text = 'function' then
                begin
                  s := 'function';
                  s2 := DoFunction;
                end;
                if s2 = '' then Exit;
                if not DirectoryExists(ClassPath + 'Methods') then
                  MkDir(ClassPath + 'Methods');
                fn := ClassPath  + 'Methods/' + FirstWordInStr(s2) + '.htm';
                if RoutinesList.IndexOf(fn) < 0 then
                  RoutinesList.AddObject(fn, Pointer(6));

                AppendStringToFile(fn,
                  Format('<p class="Decl"><b>class %s</b> %s</p>'#10, [s, s2])
                  + MakeDescription(6, Comment));
              end
              else if Tok.Text = 'var' then
              begin
                AddToBuffer(Tok);
                repeat
                  GetNextToken(Tok);
                  AddToBuffer(Tok);
                until Finished or (Tok.Text = ';');
                if Tok.Text <> ';' then Break;
                fn := ClassPath + 'Fields.htm';
                AppendStringToFile(fn, Format('<p class="Decl">%s</p>'#10, [GBuffer]));
              end else
                Exit;
              GetNextToken(Tok);
            end
            else if (Tok.Text = 'property') then
            begin
              Comment := LastSpecialComment;
              s := DoProperty;
              if s = '' then Exit;
              s2 := FirstWordInStr(s);
              s := HtmlStart(6) + '<p class="Decl"><b>property</b> ' +
                s + '</p>'#10 + MakeDescription(6, Comment)+ htmlEnd;
              if pos('On', s2) = 1 then
              begin
                if not DirectoryExists(ClassPath + 'Events') then
                  MkDir(ClassPath + 'Events');
                StringToFile(ClassPath + 'Events/' +s2 + '.htm', s)
              end else
              begin
                if not DirectoryExists(ClassPath + 'Properties') then
                  MkDir(ClassPath + 'Properties');
                StringToFile(ClassPath + 'Properties/' + s2 + '.htm', s);
              end;
              GetNextToken(Tok);
            end
            else Exit;
          else Exit;
        end;
      until Finished;
    end;
    if FileExists(ClassPath + 'Fields.htm') then
    begin
      PrependStringToFile(ClassPath + 'Fields.htm', HtmlStart(5));
      AppendStringToFile(ClassPath + 'Fields.htm', htmlEnd);
    end;
  end;

  function DoInterface(const InterfaceName: string): Boolean;
  var
    s, s2: string;
    fn: TFileName;
    InterfacePath: string;
  begin
    Result := False;
    with DelphiParser do
    begin
      PeekNextToken(Tok);
      if Tok.Text = ';' then
      begin
        Result := True;
        Exit; //ie forward declaration only
      end;
      ClearBuffer;
      AddToBuffer(InterfaceName + ' = <b>interface</b>');
      if Tok.Text = '(' then
      begin
        GetNextToken(Tok); //ie gobbles peek
        GetNextToken(Tok);
        s := Tok.Text;
        AddToBuffer('(' + Tok.Text + ')');
        GetNextToken(Tok);
        if Tok.Text <> ')' then
          Exit;
        PeekNextToken(Tok);
      end;
      AddToBuffer('<br>'#10);

      if Tok.Text = '[' then
      begin
        repeat
          GetNextToken(Tok);
          AddToBuffer(Tok);
        until Finished or (Tok.Text = ']');
        Result := Tok.Text = ']';
        if not Result then Exit;
      end;

      if not DirectoryExists(DestUnitFolder + 'Interfaces') then
        MkDir(DestUnitFolder + 'Interfaces');
      InterfacePath := DestUnitFolder + 'Interfaces/' + InterfaceName + '/';
      if not DirectoryExists(InterfacePath) then
        MkDir(InterfacePath);
      StringToFile(InterfacePath+ '_Body.htm',
        HtmlStart(5) + GBuffer + MakeDescription(5, Comment) + htmlEnd);

      GetNextToken(Tok);
      repeat

        if (Tok.Text = 'end') then
        begin
          GetNextToken(Tok);
          Result := Tok.Text = ';';
          Break;
        end;

        Comment := LastSpecialComment;
        case Tok.kind of
          tkReserved:
            if (Tok.Text = 'procedure') then
            begin
              s := Tok.Text;
              s2 := DoProcedure;
              if s2 = '' then Exit;
              if not DirectoryExists(InterfacePath + 'Methods') then
                MkDir(InterfacePath + 'Methods');
              fn := InterfacePath + 'Methods/'+FirstWordInStr(s2)+ '.htm';
              AppendStringToFile(fn,
                Format('<p class="Decl"><b>%s</b> %s</p>'#10,[s,s2]) +
                MakeDescription(6, Comment));
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.Text = 'function') then
            begin
              s := DoFunction;
              if s = '' then Exit;
              if not DirectoryExists(InterfacePath + 'Methods') then
                MkDir(InterfacePath + 'Methods');
              fn := InterfacePath  + 'Methods/' +FirstWordInStr(s) + '.htm';
              AppendStringToFile(fn,
                Format('<p class="Decl"><b>function</b> %s</p>'#10,[s]) +
                MakeDescription(6, Comment));

              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.Text = 'property') then
            begin
              s := DoProperty;
              if s = '' then Exit;
              s2 := FirstWordInStr(s);
              s := HtmlStart(6) + '<p class="Decl"><b>property</b> ' + s +
                '</p>'#10+ MakeDescription(6, Comment)+ htmlEnd;
              if pos('On', s2) = 1 then
              begin
                if not DirectoryExists(InterfacePath + 'Events') then
                  MkDir(InterfacePath + 'Events');
                StringToFile(InterfacePath + 'Events/' +s2 + '.htm', s)
              end else
              begin
                if not DirectoryExists(InterfacePath + 'Properties') then
                  MkDir(InterfacePath + 'Properties');
                StringToFile(InterfacePath + 'Properties/' + s2 + '.htm', s);
              end;
              GetNextToken(Tok);
            end
            else Exit;
          else Exit;
        end;
      until Finished;
    end;
  end;

  function DoTypeFunc(const FuncName: string): Boolean;
  var
    HasBracket: Boolean;
  begin
    with DelphiParser do
    begin
      ClearBuffer;
      AddToBuffer(FuncName + ' = <b>function</b>');
      HasBracket := False;
      repeat
        GetNextToken(Tok);
        if Tok.Text = '(' then HasBracket := True
        else if Tok.Text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.Text = ';'));
      Result := Tok.Text = ';';
      PeekNextToken(Tok);
      if Tok.Text = 'stdcall' then
      begin
        GetNextToken(Tok);
        GetNextToken(Tok);
        AddToBuffer(' <b>stdcall</b>;');
      end;
      if not DirectoryExists(DestUnitFolder + 'Types') then
        MkDir(DestUnitFolder + 'Types');
      StringToFile(DestUnitFolder + 'Types/' + FuncName + '.htm',
        HtmlStart(4) + '<p class="Decl">' +GBuffer + '</p>'#10 + MakeDescription(4, Comment)+ htmlEnd);
    end;
  end;

  function DoTypeProc(const ProcName: string): Boolean;
  var
    HasBracket: Boolean;
  begin
    with DelphiParser do
    begin
      ClearBuffer;
      AddToBuffer(ProcName + ' = <b>procedure</b> ');
      HasBracket := False;
      repeat
        GetNextToken(Tok);
        if Tok.Text = '(' then HasBracket := True
        else if Tok.Text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.Text = ';'));
      Result := Tok.Text = ';';
      PeekNextToken(Tok);
      if Tok.Text = 'stdcall' then
      begin
        GetNextToken(Tok);
        GetNextToken(Tok);
        AddToBuffer(' <b>stdcall</b>;');
      end;
      if not DirectoryExists(DestUnitFolder + 'Types') then
        MkDir(DestUnitFolder + 'Types');
      StringToFile(DestUnitFolder + 'Types/' + ProcName + '.htm',
        HtmlStart(4) + '<p class="Decl">' + GBuffer + '</p>'#10 +
        MakeDescription(4, Comment) + htmlEnd);
    end;
  end;

  function DoRecord(const RecordName, Ident2: string): Boolean;
  var
    inCase: Boolean;
  begin
    Result := False;
    ClearBuffer;
    with DelphiParser do
    begin
      if Ident2 = 'packed' then
      begin
        GetNextToken(Tok);
        if Tok.Text <> 'record' then Exit;
        AddToBuffer(RecordName + ' = <b>packed record</b><br>'#10);
      end
      else
        AddToBuffer(RecordName + ' = <b>record</b><br>'#10);

      inCase := False;
      repeat
        repeat
          GetNextToken(Tok);
          if (Tok.Text = ';') then
            AddToBuffer(';<br>') else
            AddToBuffer(Tok);
          if Tok.Text = 'case' then
            inCase := True
          else if inCase and (Tok.Text = 'of') then
          begin
            inCase := False;
            AddToBuffer('<br>');
          end;
        until Finished or (Tok.Text = ';') or (Tok.Text = 'end');
      until Finished or (Tok.Text = 'end');
      GetNextToken(Tok);
      Result := not Finished and (Tok.Text = ';');
      AddToBuffer(';');
      if Result then
      begin
        if not DirectoryExists(DestUnitFolder + 'Types') then
          MkDir(DestUnitFolder + 'Types');
        StringToFile(DestUnitFolder + 'Types/' + recordName + '.htm',
          HtmlStart(4) + '<p class="Decl">' + GBuffer + '</p>'#10 +
          MakeDescription(4, Comment) + htmlEnd);
      end;
    end;
  end;

  function DoGeneralType(const TypeName, Ident2: string): Boolean;
  begin
    ClearBuffer;
    with DelphiParser do
    begin
      if ReservedList.IndexOf(Ident2) >= 0 then
        AddToBuffer(TypeName + ' = <b>' + Ident2 + '</b>') else
        AddToBuffer(TypeName + ' = ' + Ident2);
      repeat
        GetNextToken(Tok);
        AddToBuffer(Tok);
      until Finished or (Tok.Text = ';') ;
      Result := not Finished;
      if not Result then Exit;
      if not DirectoryExists(DestUnitFolder + 'Types') then
        MkDir(DestUnitFolder + 'Types');
      StringToFile(DestUnitFolder + 'Types/' + TypeName + '.htm',
        HtmlStart(4) + '<p class="Decl">' + GBuffer + '</p>'#10 +
        MakeDescription(4, Comment) + htmlEnd);
    end;
  end;

  function DoType: Boolean;
  var
    Ident: String;
  begin
    Result := False;
    with DelphiParser do
    begin
      GetNextToken(Tok);

      while not Finished and (Tok.kind = tkIdentifier) do
      begin
        Ident := Tok.Text;
        GetNextToken(Tok);
        if Tok.Text <> '=' then Exit;
        GetNextToken(Tok);
        Comment := LastSpecialComment;
        if Tok.kind = tkReserved then
        begin
          if Tok.Text = 'class' then
          begin
            PeekNextToken(Tok);
            if Tok.Text = 'of' then
              Result := DoGeneralType(Ident, 'class') else
              Result := DoClass(Ident)
          end else if Tok.Text = 'function' then
            Result := DoTypeFunc(Ident)
          else if Tok.Text = 'procedure' then
            Result := DoTypeProc(Ident)
          else if (Tok.Text = 'packed') or (Tok.Text = 'record') then
            Result := DoRecord(Ident, Tok.Text)
          else if Tok.Text = 'interface' then
            Result := DoInterface(Ident)
          else
            Result := DoGeneralType(Ident, Tok.Text);
        end
        else
          Result := DoGeneralType(Ident, Tok.Text);
        PeekNextToken(Tok);
        if not Result or (Tok.kind <> tkIdentifier) then Break;
        GetNextToken(Tok);
      end;
    end;
  end;

begin
  Result := -1;
  if not DirectoryExists(DestUnitFolder) then
    MkDir(DestUnitFolder);

  StringToFile(DestUnitFolder + '_Body.htm', HtmlStart(3) + '<b>Unit</b> ' +
    ChangeFileExt(ExtractFileName(PasFilename),'') + #10'<br>'#10'<br>' + htmlEnd);

  PasLines := TStringlist.Create;
  try
    PasLines.LoadFromFile(PasFilename);

    ConstList := TStringlist.Create;
    VarList := TStringlist.Create;
    RoutinesList := TStringlist.Create;

    DelphiParser := TDelphiParser.Create(PasLines);
    try
      //find 'interface' identifier ...
      repeat
        DelphiParser.GetNextToken(Tok);
        if (Tok.kind = tkReserved) and (Tok.Text = 'interface') then
          Break;
      until DelphiParser.Finished;
      //parse the interface section ...
      if not DelphiParser.Finished then
        repeat
          DelphiParser.GetNextToken(Tok);
          Comment := DelphiParser.LastSpecialComment;

          if (Tok.kind = tkReserved) then
          begin
            if (Tok.Text = 'implementation') then
              Break
            else if (Tok.Text = 'const') then
            begin
              if not DoConst then
                Break
            end else if (Tok.Text = 'var') then
            begin
              if not DoVars then
                Break
            end else if (Tok.Text = 'type') then
            begin
              if not DoType then
                Break
            end else if (Tok.Text = 'function') then
            begin
              s := DoFunction;
              if s = '' then
                Break
              else
              begin
                if not DirectoryExists(DestUnitFolder + 'Routines') then
                  MkDir(DestUnitFolder + 'Routines');
                fn := DestUnitFolder + 'Routines/'+ FirstWordInStr(s) + '.htm';
                AppendStringToFile(fn,
                  Format('<p class="Decl"><b>function</b> %s</p>'#10,[s]) +
                  MakeDescription(4, Comment));
                if RoutinesList.IndexOf(fn) < 0 then
                  RoutinesList.AddObject(fn, Pointer(4));
              end;
            end else if (Tok.Text = 'procedure') then
            begin
              s := DoProcedure;
              if s = '' then
                Break
              else
              begin
                if not DirectoryExists(DestUnitFolder + 'Routines') then
                  MkDir(DestUnitFolder + 'Routines');
                fn := DestUnitFolder + 'Routines/' + FirstWordInStr(s) + '.htm';
                  AppendStringToFile(fn,
                    Format('<p class="Decl"><b>procedure</b> %s</p>'#10,[s]) +
                    MakeDescription(4, Comment));
                if RoutinesList.IndexOf(fn) < 0 then
                  RoutinesList.AddObject(fn, Pointer(4));
              end;
            end;
          end;
        until DelphiParser.Finished;

      if (Tok.Text <> 'implementation') then
        Result := DelphiParser.CurrentPt.Y;

      if ConstList.Count > 0 then
      begin
        if not DirectoryExists(DestUnitFolder + 'Constants') then
          MkDir(DestUnitFolder + 'Constants');
        ConstList.Insert(0, HtmlStart(4));
        ConstList.Add(htmlEnd);
        ConstList.SaveToFile(DestUnitFolder + 'Constants/constants.htm');
      end;

      if VarList.Count > 0 then
      begin
        if not DirectoryExists(DestUnitFolder + 'Variables') then
          MkDir(DestUnitFolder + 'Variables');
        VarList.Insert(0, HtmlStart(4));
        VarList.Add(htmlEnd);
        VarList.SaveToFile(DestUnitFolder + 'Variables/vars.htm');
      end;

     for i := 0 to RoutinesList.Count - 1 do
     begin
       //nb: the RoutinesList object simply stores the 'level' of the file ...
       PrependStringToFile(RoutinesList[i],
         HtmlStart(Integer(RoutinesList.Objects[i])));
       AppendStringToFile(RoutinesList[i], htmlEnd);
     end;

    finally
      DelphiParser.Free;
      RoutinesList.Free;
      ConstList.Free;
      VarList.Free;
    end;
  finally
    PasLines.Free;
  end;
end;

end.
