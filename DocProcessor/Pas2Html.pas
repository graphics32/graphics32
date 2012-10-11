unit Pas2Html;

//Regarding building new units from existing PAS files ...
//1. Comments directly preceeding declarations in the header section of PAS
//   files will be imported as declaration descriptions into the help file.
//2. Images can also be flagged for import by using <img src="filename">
//   Format. Images must be located in the PAS file's folder and the filename
//   must not contain a Path. Images will be copied to the Images folder.
//3. Extended comments (sample code etc) can be flagged for import by using
//   the custom <include src="filename"> Format. Again the file for importing
//   must be in the PAS file's folder and the filename must not contain a path.

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, DelphiParse,
  ShellApi, ShlObj, Forms;

  function GetDelphiSourceFolder(const startFolder: string): string;
  function BuildNewUnit(const PasFilename, destUnitFolder, projectFolder: AnsiString): Integer;
  function DeleteFolder(const Foldername: AnsiString): Boolean;

implementation

uses StrUtils;

const
  htmlEnd: AnsiString = #10'</body>'#10'</html>';
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

function HtmlStart(Level: Integer; const metaTag: AnsiString = ''): AnsiString;
const
  HtmlStart1: AnsiString =
    '<html>'#10'<head>'#10'<title>Untitled</title>'#10'<link rel="stylesheet" href="';
  HtmlStart2: AnsiString = 'styles/default.css" type="text/css">'#10;
  HtmlStart3: AnsiString = '</head>'#10'<body bgcolor="#FFFFFF">'#10;
begin
  Result := HtmlStart1 + LevelToEllipsis(Level) + HtmlStart2 + metaTag + HtmlStart3;
end;
//------------------------------------------------------------------------------

var
  GBuffer: AnsiString;

procedure AddToBuffer(const Tok: TToken); overload;
var
  Len: Integer;
  AvoidSpace, ForceSpace: Boolean;
begin
  Len := Length(GBuffer);
  AvoidSpace := (Len > 0) and (GBuffer[Len] in ['^','@','(','[','.']);
  case Tok.kind of
    tkReserved:
      if (Len > 0) and not AvoidSpace then
        GBuffer := GBuffer + ' <b>'+ Tok.text + '</b>' else
        GBuffer := GBuffer + '<b>'+ Tok.text + '</b>';
    tkText:
      if Len > 0 then
        GBuffer := GBuffer + ' '''+ Tok.text + '''' else
        GBuffer := GBuffer + ''''+ Tok.text + '''';
    tkIdentifier, tkValue, tkAsm:
      if (Len > 0) and not AvoidSpace then
        GBuffer := GBuffer + ' '+ Tok.text else
        GBuffer := GBuffer + Tok.text;
    tkSymbol:
      begin
        ForceSpace := (Len > 0) and (GBuffer[Len] in [':']);
        if ForceSpace then
          GBuffer := GBuffer + ' '+ Tok.text
        else if (Tok.text[1] in [':',';',',','(',')',']','^',SINGLEQUOTE,'"','.']) then
          GBuffer := GBuffer + Tok.text
        else
          GBuffer := GBuffer + ' '+ Tok.text;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure AddToBuffer(const str: AnsiString); overload;
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

procedure CoTaskMemFree(pv: Pointer); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

//------------------------------------------------------------------------------

function GetFolder(OwnerForm: TForm;
  const Caption: string; AllowCreateNew: Boolean; var Folder: string): Boolean;
var
  displayname: array[0..MAX_PATH] of char;
  bi: TBrowseInfo;
  pidl: PItemIdList;
begin
  if not assigned(OwnerForm) then
    bi.hWndOwner := 0 else
    bi.hWndOwner := OwnerForm.Handle;
  bi.pIDLRoot := nil;
  bi.pszDisplayName := PChar(@displayname[0]);
  bi.lpszTitle := PChar(Caption);
  bi.ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
  if AllowCreateNew then bi.ulFlags := bi.ulFlags or BIF_NEWDIALOGSTYLE;
  bi.lpfn := @BrowseProc;
  if Folder <> '' then Folder := StripSlash(Folder);
  bi.lParam := Integer(PAnsiChar(Folder));
  bi.iImage := 0;
  pidl := SHBrowseForFolder(bi);
  Result := pidl <> nil;
  if Result then
  try
    Result := SHGetPathFromIDList(pidl,PChar(@displayname[0]));
    Folder := displayname;
  finally
    CoTaskMemFree(pidl);
  end;
end;
//------------------------------------------------------------------------------

function GetSpecialFolder(FolderID: Integer): AnsiString;
var
  p: PItemIDList;
  Path: array[0..MAX_PATH] of char;
begin
  Result := '';
  //nb: SHGetSpecialFolderPath requires IE 4.0 or higher
  if Succeeded(SHGetSpecialFolderLocation(0, FolderID, p)) then
  try
    if SHGetPathFromIDList(p, Path) then Result := Path;
  finally
    CoTaskMemFree(p);
  end;
end;
//------------------------------------------------------------------------------

function GetDelphiSourceFolder(const startFolder: string): string;
const
  CSIDL_PROGRAM_FILES = $26;
begin
  if (startFolder = '') or not DirectoryExists(startFolder) then
    Result := GetSpecialFolder(CSIDL_PROGRAM_FILES) else
    Result := startFolder;
  if not GetFolder(application.MainForm,
    'Location of Delphi PAS Files ...', False, Result) then
    Result := '';
end;
//------------------------------------------------------------------------------

function trimSlash(const Path: string): string;
var
  i: Integer;
begin
  Result := Path;
  i := Length(Path);
  if (i > 0) and (Path[i] = '\') then Delete(Result, i, 1);
end;
//------------------------------------------------------------------------------

function GetParentFolder(const Path: string): string;
var
  i: Integer;
begin
  i := Length(Path) - 1;
  while (i > 0) and (Path[i] <> '\') do Dec(i);
  Result := Copy(Path, 1, i);
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

function DeleteFolder(const Foldername: AnsiString): Boolean;
begin
  Result := DirectoryExists(Foldername) and
    ShellFileOperation(trimSlash(Foldername), '', FO_DELETE);
end;
//------------------------------------------------------------------------------

function StringFromFile(const Filename: string): string;
begin
  with TMemoryStream.Create do
  try
    LoadFromFile(Filename);
    SetString(Result, PAnsiChar(Memory), size);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure StringToFile(const Filename, StrVal: AnsiString);
begin
  with TMemoryStream.Create do
  try
    Size := Length(StrVal);
    if size > 0 then
      Move(StrVal[1], PAnsiChar(Memory)^, size);
    SaveToFile(Filename);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure AppendStringToFile(const Filename, StrVal: AnsiString);
var
  i, Len, OldSize: cardinal;
begin
  Len := Length(StrVal);
  if Len = 0 then Exit;
  with TMemoryStream.Create do
  try
    if FileExists(Filename) then LoadFromFile(Filename);
    OldSize := Size;
    if OldSize > 0 then i := SizeOf(cr) else i := 0;
    Size := OldSize + Len + i;
    if OldSize > 0 then Move(cr, (PAnsiChar(Memory)+ OldSize)^, SizeOf(cr));
    Move(StrVal[1], (PAnsiChar(Memory)+ OldSize +i)^, Len);
    SaveToFile(Filename);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure PrependStringToFile(const Filename, StrVal: AnsiString);
var
  i, Len, OldSize: cardinal;
begin
  Len := Length(StrVal);
  if Len = 0 then Exit;
  with TMemoryStream.Create do
  try
    if FileExists(Filename) then LoadFromFile(Filename);
    OldSize := Size;
    if OldSize > 0 then i := SizeOf(cr) else i := 0;
    Size := OldSize + Len + i;
    if OldSize > 0 then Move(PAnsiChar(Memory)^, (PAnsiChar(Memory)+ Len + i)^, OldSize);
    Move(StrVal[1], PAnsiChar(Memory)^, Len);
    if OldSize > 0 then Move(cr, (PAnsiChar(Memory)+Len)^, SizeOf(cr));
    SaveToFile(Filename);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function FirstWordInStr(const s: AnsiString): AnsiString;
var
  i, Len: Integer;
begin
  Len := Length(s);
  if Len = 0 then Result := ''
  else
  begin
    i := 1;
    while (i <= Len) and (s[i] in ['a'..'z','A'..'Z','0'..'9']) do Inc(i);
    Result := Copy(s, 1, i - 1);
  end;
end;
//------------------------------------------------------------------------------

function BuildNewUnit(const PasFilename, destUnitFolder, projectFolder: AnsiString): Integer;
var
  i: Integer;
  PasLines: TStringlist;
  DelphiParser: TDelphiParser;
  ConstList, VarList, RoutinesList: TStringList;
  Tok: TToken;
  s, fn, Comment: AnsiString;

  function MakeDescription(Level: Integer; Comment: string): AnsiString;
  var
    i,j: Integer;
    ImgFile, IncFile, IncStr: string;
    QuoteChar: char;
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
      if not (QuoteChar in ['"', '''']) then
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
      i := PosEx('<img src=',Comment,i);
      if i = 0 then
        Break;
      QuoteChar := Comment[i + 9];
      if not (QuoteChar in ['"', '''']) then
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
      Delete(Comment,i+4,1);
    end;
    while True do
    begin
      i := PosEx('<br/> ',Comment,i);
      if i = 0 then Break;
      Delete(Comment,i+5,1);
    end;

    if Comment = '' then
      Result := '<br>' else
      Result := '<p class="Body">'#10 +Comment+ '</p>'#10;
  end;

  function DoConst: Boolean;
  var
    Ident: AnsiString;
  begin
    Result := False;
    with DelphiParser do
    begin
      while True do
      begin
        peekNextToken(Tok);
        if (Tok.kind <> tkIdentifier) then Break;
        GetNextToken(Tok); //gobble peek
        Ident := Tok.text;
        Comment := LastSpecialComment;
        ClearBuffer;
        repeat
          GetNextToken(Tok);
          AddToBuffer(Tok);
        until Finished or (Tok.text = ';');
        Result := Tok.text = ';';
        if Result then
          ConstList.Add(
            Format('<p class="Decl">%s %s</p>'#10,[Ident, GBuffer])+
            MakeDescription(4, Comment)+ '<br><br>'#10);
      end;
    end;
    //add a space between each CONST code block ...
    ConstList.Add('<br>'#10);
  end;

  function DoVars: Boolean;
  var
    Ident: AnsiString;
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
        Ident := Tok.text;
        GetNextToken(Tok);
        if Tok.text <> ':' then Exit;
        ClearBuffer;
        Comment := LastSpecialComment;
        AddToBuffer(Ident + ':');
        HasBracket := False;
        repeat
          GetNextToken(Tok);
          if (Tok.text = '(') then HasBracket := True
          else if (Tok.text = ')') then HasBracket := False;
          AddToBuffer(Tok);
        until Finished or (not HasBracket and (Tok.text = ';'));
        Result := Tok.text = ';';
        PeekNextToken(Tok);
        if Tok.text = 'stdcall' then
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

  function DoFunction: AnsiString;
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
        if Tok.text = '(' then HasBracket := True
        else if Tok.text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.text = ':'));
      if Tok.text <> ':' then Exit;
      repeat
        GetNextToken(Tok);
        AddToBuffer(Tok);
      until Finished or (Tok.text = ';');
      if (Tok.text <> ';') then Exit;
      while True do
      begin
        PeekNextToken(Tok);
        if (Tok.kind = tkReserved) and ((Tok.text = 'overload') or
          (Tok.text = 'override') or (Tok.text = 'virtual') or
          (Tok.text = 'abstract') or (Tok.text = 'dynamic') or
          (Tok.text = 'reintroduce') or (Tok.text = 'inline') or
          (Tok.text = 'stdcall')) then
            AddToBuffer(Tok) else
            Break;

        GetNextToken(Tok); //ie gobbles peek
        GetNextToken(Tok);
        if Tok.text <> ';' then Exit;
        AddToBuffer(';');
      end;
      Result := GBuffer;
    end;
  end;

  function DoProcedure: AnsiString;
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
        if Tok.text = '(' then HasBracket := True
        else if Tok.text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.text = ';'));
      if Tok.text <> ';' then Exit;
      while True do
      begin
        PeekNextToken(Tok);
        if (Tok.kind = tkReserved) and ((Tok.text = 'overload') or
          (Tok.text = 'override') or (Tok.text = 'virtual') or
          (Tok.text = 'abstract') or (Tok.text = 'dynamic')or
          (Tok.text = 'reintroduce') or (Tok.text = 'inline') or
          (Tok.text = 'stdcall')) then
            AddToBuffer(Tok) else
            Break;
        GetNextToken(Tok); //ie gobbles peek
        GetNextToken(Tok);
        if Tok.text <> ';' then Exit;
        AddToBuffer(';');
      end;
      Result := GBuffer;
    end;
  end;

  function DoProperty: AnsiString;
  var
    inSqrBracket, doRead, doWrite: Boolean;
  begin
    Result := '';
    ClearBuffer;
    with DelphiParser do
    begin
      GetNextToken(Tok);
      if Tok.kind <> tkIdentifier then Exit;
      AddToBuffer(Tok);
      PeekNextToken(Tok);
      if Tok.text = ';' then
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
        if Tok.text = '[' then inSqrBracket := True
        else if Tok.text = ']' then inSqrBracket := False;
      until Finished or (not inSqrBracket and (Tok.text = ':'));
      GetNextToken(Tok);
      if not (Tok.kind in [tkIdentifier, tkReserved]) then Exit;
      AddToBuffer(Tok);
      AddToBuffer(';');
      //now skip the rest of the property stuff (ie read, write etc) ...
      repeat
        GetNextToken(Tok);
        if Tok.text = 'read' then doRead := True
        else if Tok.text = 'write' then doWrite := True;
      until Finished or (Tok.text = ';');
      if Tok.text <> ';' then Exit;

      while True do
      begin
        PeekNextToken(Tok);
        if (Tok.text = 'default') or (Tok.text = 'stored') then
        repeat
          GetNextToken(Tok);
        until Finished or (Tok.text = ';')
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

  function DoClass(const ClsName: AnsiString): Boolean;
  var
    s, s2, fn, Ancestor, ClassPath: AnsiString;
  begin
    with DelphiParser do
    begin
      GetNextToken(Tok);
      Result := Tok.text = ';';
      if Result then Exit; //ie ignore forward class declarations
      if (Tok.text = '(') then
      begin
        Ancestor := '';
        repeat
          GetNextToken(Tok);
          if Tok.text <> ')' then Ancestor := Ancestor + Tok.text;
        until Finished or (Tok.text = ')');
        if Tok.text <> ')' then Exit;
        GetNextToken(Tok);
        Result := Tok.text = ';';
        if Result then Exit; //ie ignore forward class declaration
      end;
      if not DirectoryExists(destUnitFolder+ 'Classes') then
        MkDir(destUnitFolder+ 'Classes');
      ClassPath := destUnitFolder+ 'Classes/' + ClsName + '/';
      if not DirectoryExists(ClassPath) then
        MkDir(ClassPath);
      Ancestor := '<meta name="Ancestor" content="' +Ancestor + '">'#10;
      StringToFile(ClassPath+ '_Body.htm',
        HtmlStart(5, Ancestor) + MakeDescription(5, Comment) + htmlEnd);

      repeat
        //skip private and protected class fields and methods...
        if (Tok.text = 'private') or (Tok.text = 'protected') then
        repeat
          GetNextToken(Tok);
        until Finished or (Tok.text = 'public') or
          (Tok.text = 'published') or (Tok.text = 'end');

        if (Tok.text = 'end') then
        begin
          GetNextToken(Tok);
          Result := Tok.text = ';';
          Break;
        end
        else if (Tok.text = 'public') or (Tok.text = 'published') then
          GetNextToken(Tok);

        case Tok.kind of
          tkIdentifier:
            begin
              ClearBuffer;
              AddToBuffer(Tok);
              repeat
                GetNextToken(Tok);
                AddToBuffer(Tok);
              until Finished or (Tok.text = ';');
              if Tok.text <> ';' then Break;
              fn := ClassPath + 'Fields.htm';
              AppendStringToFile(fn, Format('<p class="Decl">%s</p>'#10,[GBuffer]));
              GetNextToken(Tok);
            end;
          tkReserved:
            if (Tok.text = 'constructor') or (Tok.text = 'destructor') or
              (Tok.text = 'procedure') then
            begin
              s := Tok.text;
              Comment := LastSpecialComment;
              s2 := DoProcedure;
              if s2 = '' then Exit;
              if not DirectoryExists(ClassPath + 'Methods') then
                MkDir(ClassPath + 'Methods');
              fn := ClassPath + 'Methods/'+FirstWordInStr(s2)+ '.htm';
              AppendStringToFile(fn,Format('<p class="Decl"><b>%s</b> %s</p>'#10,[s,s2]) +
                MakeDescription(6, Comment));
              if RoutinesList.IndexOf(fn) < 0 then
                RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.text = 'function') then
            begin
              Comment := LastSpecialComment;
              s := DoFunction;
              if s = '' then Exit;
              if not DirectoryExists(ClassPath + 'Methods') then
                MkDir(ClassPath + 'Methods');
              fn := ClassPath  + 'Methods/' +FirstWordInStr(s) + '.htm';
              AppendStringToFile(fn, '<p class="Decl"><b>function</b> ' +
                  s + '</p>' +MakeDescription(6, Comment));
              if RoutinesList.IndexOf(fn) < 0 then
                RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.text = 'class') then
            begin
              Comment := LastSpecialComment;
              GetNextToken(Tok);
              if Tok.text = 'procedure' then
              begin
                s := 'procedure';
                s2 := DoProcedure;
              end else if Tok.text = 'function' then
              begin
                s := 'function';
                s2 := DoFunction;
              end else Exit;
              if s2 = '' then Exit;
              if not DirectoryExists(ClassPath + 'Methods') then
                MkDir(ClassPath + 'Methods');
              fn := ClassPath  + 'Methods/' + FirstWordInStr(s2) + '.htm';
              AppendStringToFile(fn,
                Format('<p class="Decl"><b>class %s</b> %s</p>'#10, [s, s2])
                + MakeDescription(6, Comment));
              if RoutinesList.IndexOf(fn) < 0 then
                RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(Tok);
            end
            else if (Tok.text = 'property') then
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

  function DoInterface(const interfaceName: AnsiString): Boolean;
  var
    s, s2, fn, InterfacePath: AnsiString;
  begin
    with DelphiParser do
    begin
      PeekNextToken(Tok);
      if Tok.text = ';' then
      begin
        Result := True;
        Exit; //ie forward declaration only
      end;
      ClearBuffer;
      AddToBuffer(interfaceName + ' = <b>interface</b><br>'#10);
      repeat
        GetNextToken(Tok);
        AddToBuffer(Tok);
      until Finished or (Tok.text = ']');
      Result := Tok.text = ']';
      if not Result then Exit;

      if not DirectoryExists(destUnitFolder+ 'Interfaces') then
        MkDir(destUnitFolder+ 'Interfaces');
      InterfacePath := destUnitFolder+ 'Interfaces/' + interfaceName + '/';
      if not DirectoryExists(InterfacePath) then
        MkDir(InterfacePath);
      StringToFile(InterfacePath+ '_Body.htm',
        HtmlStart(5) + GBuffer + MakeDescription(5, Comment) + htmlEnd);

      GetNextToken(Tok);
      repeat

        if (Tok.text = 'end') then
        begin
          GetNextToken(Tok);
          Result := Tok.text = ';';
          Break;
        end;

        Comment := LastSpecialComment;
        case Tok.kind of
          tkReserved:
            if (Tok.text = 'procedure') then
            begin
              s := Tok.text;
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
            else if (Tok.text = 'function') then
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
            else if (Tok.text = 'property') then
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

  function DoTypeFunc(const FuncName: AnsiString): Boolean;
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
        if Tok.text = '(' then HasBracket := True
        else if Tok.text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.text = ';'));
      Result := Tok.text = ';';
      PeekNextToken(Tok);
      if Tok.text = 'stdcall' then
      begin
        GetNextToken(Tok);
        GetNextToken(Tok);
        AddToBuffer(' <b>stdcall</b>;');
      end;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types/' + FuncName + '.htm',
        HtmlStart(4) + '<p class="Decl">' +GBuffer + '</p>'#10 + MakeDescription(4, Comment)+ htmlEnd);
    end;
  end;

  function DoTypeProc(const ProcName: AnsiString): Boolean;
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
        if Tok.text = '(' then HasBracket := True
        else if Tok.text = ')' then HasBracket := False;
        AddToBuffer(Tok);
      until Finished or (not HasBracket and (Tok.text = ';'));
      Result := Tok.text = ';';
      PeekNextToken(Tok);
      if Tok.text = 'stdcall' then
      begin
        GetNextToken(Tok);
        GetNextToken(Tok);
        AddToBuffer(' <b>stdcall</b>;');
      end;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types/' + ProcName + '.htm',
        HtmlStart(4) + '<p class="Decl">' + GBuffer + '</p>'#10 +
        MakeDescription(4, Comment) + htmlEnd);
    end;
  end;

  function DoRecord(const recordName, ident2: AnsiString): Boolean;
  var
    inCase: Boolean;
  begin
    Result := False;
    ClearBuffer;
    with DelphiParser do
    begin
      if ident2 = 'packed' then
      begin
        GetNextToken(Tok);
        if Tok.text <> 'record' then Exit;
        AddToBuffer(recordName + ' = <b>packed record</b><br>'#10);
      end
      else
        AddToBuffer(recordName + ' = <b>record</b><br>'#10);

      inCase := False;
      repeat
        repeat
          GetNextToken(Tok);
          if (Tok.text = ';') then
            AddToBuffer(';<br>') else
            AddToBuffer(Tok);
          if Tok.text = 'case' then
            inCase := True
          else if inCase and (Tok.text = 'of') then
          begin
            inCase := False;
            AddToBuffer('<br>');
          end;
        until Finished or (Tok.text = ';') or (Tok.text = 'end');
      until Finished or (Tok.text = 'end');
      GetNextToken(Tok);
      Result := not Finished and (Tok.text = ';');
      AddToBuffer(';');
      if Result then
      begin
        if not DirectoryExists(destUnitFolder+ 'Types') then
          MkDir(destUnitFolder+ 'Types');
        StringToFile(destUnitFolder+ 'Types/' + recordName + '.htm',
          HtmlStart(4) + '<p class="Decl">' + GBuffer + '</p>'#10 +
          MakeDescription(4, Comment) + htmlEnd);
      end;
    end;
  end;

  function DoGeneralType(const typeName, ident2: AnsiString): Boolean;
  begin
    ClearBuffer;
    with DelphiParser do
    begin
      if ReservedList.IndexOf(ident2) >= 0 then
        AddToBuffer(typeName + ' = <b>' + ident2 + '</b>') else
        AddToBuffer(typeName + ' = ' + ident2);
      repeat
        GetNextToken(Tok);
        AddToBuffer(Tok);
      until Finished or (Tok.text = ';') ;
      Result := not Finished;
      if not Result then Exit;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types/' + typeName + '.htm',
        HtmlStart(4) + '<p class="Decl">' + GBuffer + '</p>'#10 +
        MakeDescription(4, Comment) + htmlEnd);
    end;
  end;

  function DoType: Boolean;
  var
    Ident: AnsiString;
  begin
    Result := False;
    with DelphiParser do
    begin
      GetNextToken(Tok);

      while not Finished and (Tok.kind = tkIdentifier) do
      begin
        Ident := Tok.text;
        GetNextToken(Tok);
        if Tok.text <> '=' then Exit;
        GetNextToken(Tok);
        Comment := LastSpecialComment;
        if Tok.kind = tkReserved then
        begin
          if Tok.text = 'class' then
          begin
            PeekNextToken(Tok);
            if Tok.text = 'of' then
              Result := DoGeneralType(Ident, 'class') else
              Result := DoClass(Ident)
          end else if Tok.text = 'function' then
            Result := DoTypeFunc(Ident)
          else if Tok.text = 'procedure' then
            Result := DoTypeProc(Ident)
          else if (Tok.text = 'packed') or (Tok.text = 'record') then
            Result := DoRecord(Ident, Tok.text)
          else if Tok.text = 'interface' then
            Result := DoInterface(Ident)
          else
            Result := DoGeneralType(Ident, Tok.text);
        end
        else
          Result := DoGeneralType(Ident, Tok.text);
        PeekNextToken(Tok);
        if not Result or (Tok.kind <> tkIdentifier) then Break;
        GetNextToken(Tok);
      end;
    end;
  end;

begin
  Result := -1;
  if not DirectoryExists(destUnitFolder) then
    MkDir(destUnitFolder);

  StringToFile(destUnitFolder+ '_Body.htm', HtmlStart(3) + '<b>Unit</b> ' +
    ChangeFileExt(ExtractFileName(PasFilename),'') + htmlEnd);

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
        if (Tok.kind = tkReserved) and (Tok.text = 'interface') then Break;
      until DelphiParser.Finished;
      //parse the interface section ...
      if not DelphiParser.Finished then
        repeat
          DelphiParser.GetNextToken(Tok);
          Comment := DelphiParser.LastSpecialComment;

          if (Tok.kind = tkReserved) then
          begin
            if (Tok.text = 'implementation') then Break
            else if (Tok.text = 'const') then
            begin
              if not DoConst then Break
            end else if (Tok.text = 'var') then
            begin
              if not DoVars then Break
            end else if (Tok.text = 'type') then
            begin
              if not DoType  then Break
            end else if (Tok.text = 'function') then
            begin
              s := DoFunction;
              if s = '' then
                Break
              else
              begin
                if not DirectoryExists(destUnitFolder+ 'Functions') then
                  MkDir(destUnitFolder+ 'Functions');
                fn := destUnitFolder+ 'Functions/'+ FirstWordInStr(s) + '.htm';
                AppendStringToFile(fn,
                  Format('<p class="Decl"><b>function</b> %s</p>'#10,[s]) +
                  MakeDescription(4, Comment));
                if RoutinesList.IndexOf(fn) < 0 then
                  RoutinesList.AddObject(fn, Pointer(4));
              end;
            end else if (Tok.text = 'procedure') then
            begin
              s := DoProcedure;
              if s = '' then
                Break
              else
              begin
                if not DirectoryExists(destUnitFolder+ 'Functions') then
                  MkDir(destUnitFolder+ 'Functions');
                fn := destUnitFolder+ 'Functions/' +FirstWordInStr(s) + '.htm';
                  AppendStringToFile(fn,
                    Format('<p class="Decl"><b>procedure</b> %s</p>'#10,[s]) +
                    MakeDescription(4, Comment));
                if RoutinesList.IndexOf(fn) < 0 then
                  RoutinesList.AddObject(fn, Pointer(4));
              end;
            end;
          end;
        until DelphiParser.Finished;

      if (Tok.text <> 'implementation') then
        Result := DelphiParser.CurrentPt.Y;

      if ConstList.Count > 0 then
      begin
        if not DirectoryExists(destUnitFolder+ 'Constants') then
          MkDir(destUnitFolder+ 'Constants');
        ConstList.Insert(0, HtmlStart(4));
        ConstList.Add(htmlEnd);
        ConstList.SaveToFile(destUnitFolder+ 'Constants/constants.htm');
      end;

      if VarList.Count > 0 then
      begin
        if not DirectoryExists(destUnitFolder+ 'Vars') then
          MkDir(destUnitFolder+ 'Vars');
        VarList.Insert(0, HtmlStart(4));
        VarList.Add(htmlEnd);
        VarList.SaveToFile(destUnitFolder+ 'Vars/vars.htm');
      end;

     for i := 0 to RoutinesList.Count -1 do
     begin
       //nb: the RoutinesList object simply stores the 'level' of the file ...
       PrependStringToFile(RoutinesList[i],
         HtmlStart(Integer(RoutinesList.Objects[i])));
       AppendStringToFile(RoutinesList[i], htmlEnd);
     end;

    finally
      DelphiParser.free;
      RoutinesList.Free;
      ConstList.free;
      VarList.Free;
    end;
  finally
    PasLines.Free;
  end;
end;

end.
