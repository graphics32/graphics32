unit Pas2Html;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, DelphiParse,
  ShellApi, ShlObj, Forms;

  function GetDelphiSourceFolder: string;
  function BuildNewUnit(const pasFilename, destUnitFolder: ansiString): integer;

implementation

uses StrUtils;

const
  htmlEnd: AnsiString = #10'</body>'#10'</html>';
  cr: AnsiChar = #10;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function htmlStart(level: integer; const metaTag: ansiString = ''): ansiString;
const
  htmlStart1: AnsiString = '<html>'#10'<head>'#10'<title>Untitled</title>'#10'<link rel="stylesheet" href="';
  htmlStart2: AnsiString = 'styles/default.css" type="text/css">'#10;
  htmlStart3: AnsiString = '</head>'#10'<body bgcolor="#FFFFFF">'#10;
begin
  result := '';
  for level := 1 to level do result := result + '../';
  result := htmlStart1 + result + htmlStart2 + metaTag + htmlStart3;
end;
//------------------------------------------------------------------------------

var
  buffer: AnsiString;

procedure AddToBuffer(const tok: TToken); overload;
var
  len: integer;
  avoidSpace, forceSpace: boolean;
begin
  len := length(buffer);
  avoidSpace := (len > 0) and (buffer[len] in ['^','@','(','[','.']);
  case tok.kind of
    tkReserved:
      if (len > 0) and not avoidSpace then
        buffer := buffer + ' <b>'+ tok.text +'</b>' else
        buffer := buffer + '<b>'+ tok.text +'</b>';
    tkText:
      if len > 0 then
        buffer := buffer + ' '''+ tok.text +'''' else
        buffer := buffer + ''''+ tok.text +'''';
    tkIdentifier, tkValue, tkAsm:
      if (len > 0) and not avoidSpace then
        buffer := buffer + ' '+ tok.text else
        buffer := buffer + tok.text;
    tkSymbol:
      begin
        forceSpace := (len > 0) and (buffer[len] in [':']);
        if forceSpace then
          buffer := buffer + ' '+ tok.text
        else if (tok.text[1] in [':',';',',','(',')',']','^',SINGLEQUOTE,'"','.']) then
          buffer := buffer + tok.text
        else
          buffer := buffer + ' '+ tok.text;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure AddToBuffer(const str: ansiString); overload;
begin
  buffer := buffer + str;
end;
//------------------------------------------------------------------------------

procedure ClearBuffer;
begin
 buffer := '';
end;
//------------------------------------------------------------------------------

function StripSlash(const path: ansiString): ansiString;
var
  len: integer;
begin
  result := path;
  len := length(path);
  if (len = 0) or (path[len] <> '\') then exit;
  setlength(result,len-1);
end;
//------------------------------------------------------------------------------

function BrowseProc(hwnd: HWnd; uMsg: integer; lParam, lpData: LPARAM): integer; stdcall;
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
        ShGetFileInfo(PChar(lParam), 0, sfi,sizeof(sfi),SHGFI_DISPLAYNAME or SHGFI_PIDL);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT,0, integer(@sfi.szDisplayName));
      end;
  end;
  result := 0;
end;
//------------------------------------------------------------------------------

procedure CoTaskMemFree(pv: Pointer); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

//------------------------------------------------------------------------------

function GetFolder(OwnerForm: TForm;
  const Caption: string; AllowCreateNew: boolean; var Folder: string): boolean;
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
  bi.lParam := integer(PAnsiChar(Folder));
  bi.iImage := 0;
  pidl := SHBrowseForFolder(bi);
  result := pidl <> nil;
  if result then
  try
    result := SHGetPathFromIDList(pidl,PChar(@displayname[0]));
    Folder := displayname;
  finally
    CoTaskMemFree(pidl);
  end;
end;
//------------------------------------------------------------------------------

function GetSpecialFolder(FolderID: integer): ansiString;
var
  p: PItemIDList;
  Path: array[0..MAX_PATH] of char;
begin
  result := '';
  //nb: SHGetSpecialFolderPath requires IE 4.0 or higher
  if Succeeded(SHGetSpecialFolderLocation(0, FolderID, p)) then
  try
    if SHGetPathFromIDList(p, Path) then result := path;
  finally
    CoTaskMemFree(p);
  end;
end;
//------------------------------------------------------------------------------

function GetDelphiSourceFolder: string;
const
  CSIDL_PROGRAM_FILES = $26;
begin
  result := GetSpecialFolder(CSIDL_PROGRAM_FILES);
  if not GetFolder(application.MainForm,
    'Location of Delphi PAS Files ...', false, result) then
    result := '';
end;

//------------------------------------------------------------------------------

procedure StringToFile(const filename, strVal: ansiString);
begin
  with TMemoryStream.Create do
  try
    Size := length(strVal);
    if size > 0 then
      move(strVal[1], PAnsiChar(memory)^, size);
    SaveToFile(filename);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure AppendStringToFile(const filename, strVal: ansiString);
var
  i, len, oldSize: cardinal;
begin
  len := length(strVal);
  if len = 0 then exit;
  with TMemoryStream.Create do
  try
    if FileExists(filename) then LoadFromFile(filename);
    oldSize := Size;
    if oldSize > 0 then i := sizeof(cr) else i := 0;
    Size := oldSize + len + i;
    if oldSize > 0 then move(cr, (PAnsiChar(memory)+ oldSize)^, sizeof(cr));
    move(strVal[1], (PAnsiChar(memory)+ oldSize +i)^, len);
    SaveToFile(filename);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure PrependStringToFile(const filename, strVal: ansiString);
var
  i, len, oldSize: cardinal;
begin
  len := length(strVal);
  if len = 0 then exit;
  with TMemoryStream.Create do
  try
    if FileExists(filename) then LoadFromFile(filename);
    oldSize := Size;
    if oldSize > 0 then i := sizeof(cr) else i := 0;
    Size := oldSize + len + i;
    if oldSize > 0 then move(PAnsiChar(memory)^, (PAnsiChar(memory)+ len + i)^, oldSize);
    move(strVal[1], PAnsiChar(memory)^, len);
    if oldSize > 0 then move(cr, (PAnsiChar(memory)+len)^, sizeof(cr));
    SaveToFile(filename);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function FirstWordInStr(const s: ansiString): ansiString;
var
  i, len: integer;
begin
  len := length(s);
  if len = 0 then result := ''
  else
  begin
    i := 1;
    while (i <= len) and (s[i] in ['a'..'z','A'..'Z','0'..'9']) do inc(i);
    result := copy(s, 1, i -1);
  end;
end;
//------------------------------------------------------------------------------

function BuildNewUnit(const pasFilename, destUnitFolder: ansiString): integer;
var
  i: integer;
  pasLines: TStringlist;
  DelphiParser: TDelphiParser;
  ConstList, VarList, RoutinesList: TStringList;
  tok: TToken;
  s, fn, comment: ansiString;

  function MakeDescription(comment: ansiString): ansiString;
  var
    i: integer;
  begin
    //delete spaces that trail the <br> token ...
    i := 1;
    while true do
    begin
      i := PosEx('<br> ',comment,i);
      if i = 0 then break;
      delete(comment,i+4,1);
    end;
    if comment = '' then
      result := '<br>' else
      result := '<p class="Body">'#10 +comment+'</p><br>'#10;
  end;

  function DoConst: boolean;
  var
    ident: ansiString;
  begin
    result := false;
    with DelphiParser do
    begin
      while true do
      begin
        peekNextToken(tok);
        if (tok.kind <> tkIdentifier) then break;
        GetNextToken(tok); //gobble peek
        ident := tok.text;
        comment := LastSpecialComment;
        clearBuffer;
        repeat
          GetNextToken(tok);
          AddToBuffer(tok);
        until finished or (tok.text = ';');
        result := tok.text = ';';
        if result then
          ConstList.Add(MakeDescription(comment)+
            format('<p class="Decl">%s %s</p>'#10,[ident, buffer]));
      end;
    end;
    //add a space between each CONST code block ...
    ConstList.Add('<br>'#10);
  end;

  function DoVars: boolean;
  var
    ident: ansiString;
    hasBracket: boolean;
  begin
    result := false;
    with DelphiParser do
    begin
      while true do
      begin
        peekNextToken(tok);
        if (tok.kind <> tkIdentifier) then exit;
        GetNextToken(tok); //gobble peek
        ident := tok.text;
        GetNextToken(tok);
        if tok.text <> ':' then exit;
        ClearBuffer;
        comment := LastSpecialComment;
        AddToBuffer(ident + ':');
        hasBracket := false;
        repeat
          GetNextToken(tok);
          if (tok.text = '(') then hasBracket := true
          else if (tok.text = ')') then hasBracket := false;
          AddToBuffer(tok);
        until finished or (not hasBracket and (tok.text = ';'));
        result := tok.text = ';';
        PeekNextToken(tok);
        if tok.text = 'stdcall' then
        begin
          GetNextToken(tok);
          GetNextToken(tok);
          AddToBuffer(tok);
        end;
        if result then
          VarList.Add(MakeDescription(comment) +
            format('<p class="Decl">%s</p>'#10,[buffer]));
      end;
    end;
    //add a space between each VAR code block ...
    VarList.Add('<br>'#10);
  end;

  function DoFunction: ansiString;
  var
    hasBracket: boolean;
  begin
    result := '';
    with DelphiParser do
    begin
      GetNextToken(tok);
      if not (tok.kind in [tkIdentifier, tkReserved]) then exit;
      ClearBuffer;
      AddToBuffer(tok);
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        AddToBuffer(tok);
      until finished or (not hasBracket and (tok.text = ':'));
      if tok.text <> ':' then exit;
      repeat
        GetNextToken(tok);
        AddToBuffer(tok);
      until finished or (tok.text = ';');
      if (tok.text <> ';') then exit;
      while true do
      begin
        PeekNextToken(tok);
        if (tok.kind = tkReserved) and ((tok.text = 'overload') or
          (tok.text = 'override') or (tok.text = 'virtual') or
          (tok.text = 'abstract') or (tok.text = 'dynamic') or
          (tok.text = 'reintroduce') or (tok.text = 'inline') or
          (tok.text = 'stdcall')) then
            AddToBuffer(tok) else
            break;

        GetNextToken(tok); //ie gobbles peek
        GetNextToken(tok);
        if tok.text <> ';' then exit;
        AddToBuffer(';');
      end;
      result := buffer;
    end;
  end;

  function DoProcedure: ansiString;
  var
    hasBracket: boolean;
  begin
    result := '';
    with DelphiParser do
    begin
      GetNextToken(tok);
      if not (tok.kind in [tkIdentifier, tkReserved]) then exit;
      ClearBuffer;
      AddToBuffer(tok);
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        AddToBuffer(tok);
      until finished or (not hasBracket and (tok.text = ';'));
      if tok.text <> ';' then exit;
      while true do
      begin
        PeekNextToken(tok);
        if (tok.kind = tkReserved) and ((tok.text = 'overload') or
          (tok.text = 'override') or (tok.text = 'virtual') or
          (tok.text = 'abstract') or (tok.text = 'dynamic')or
          (tok.text = 'reintroduce') or (tok.text = 'inline') or
          (tok.text = 'stdcall')) then
            AddToBuffer(tok) else
            break;
        GetNextToken(tok); //ie gobbles peek
        GetNextToken(tok);
        if tok.text <> ';' then exit;
        AddToBuffer(';');
      end;
      result := buffer;
    end;
  end;

  function DoProperty: ansiString;
  var
    inSqrBracket, doRead, doWrite: boolean;
  begin
    result := '';
    clearBuffer;
    with DelphiParser do
    begin
      GetNextToken(tok);
      if tok.kind <> tkIdentifier then exit;
      AddToBuffer(tok);
      PeekNextToken(tok);
      if tok.text = ';' then
      begin
        GetNextToken(tok); //gobble the peek
        AddToBuffer(';');
        result := buffer;
        exit;              //ie just elevated the property's visibility
      end;
      inSqrBracket := false;
      doRead := false;
      doWrite := false;
      repeat
        GetNextToken(tok);
        AddToBuffer(tok);
        if tok.text = '[' then inSqrBracket := true
        else if tok.text = ']' then inSqrBracket := false;
      until finished or (not inSqrBracket and (tok.text = ':'));
      GetNextToken(tok);
      if not (tok.kind in [tkIdentifier, tkReserved]) then exit;
      AddToBuffer(tok);
      AddToBuffer(';');
      //now skip the rest of the property stuff (ie read, write etc) ...
      repeat
        GetNextToken(tok);
        if tok.text = 'read' then doRead := true
        else if tok.text = 'write' then doWrite := true;
      until finished or (tok.text = ';');
      if tok.text <> ';' then exit;

      while true do
      begin
        PeekNextToken(tok);
        if (tok.text = 'default') or (tok.text = 'stored') then
        repeat
          GetNextToken(tok);
        until finished or (tok.text = ';')
        else break;
      end;

      if doRead and doWrite then
        AddToBuffer(' <span class="Comment">//read and write</span>')
      else if doRead then
        AddToBuffer(' <span class="Comment">//read only</span>')
      else if doWrite then
        AddToBuffer(' <span class="Comment">//write only</span>');

      result := buffer;
    end;
  end;

  function DoClass(const clsName: ansiString): boolean;
  var
    s, s2, fn, ancestor, classPath: ansiString;
  begin
    with DelphiParser do
    begin
      GetNextToken(tok);
      result := tok.text = ';';
      if result then exit; //ie ignore forward class declarations
      if (tok.text = '(') then
      begin
        ancestor := '';
        repeat
          GetNextToken(tok);
          if tok.text <> ')' then ancestor := ancestor + tok.text;
        until finished or (tok.text = ')');
        if tok.text <> ')' then exit;
        GetNextToken(tok);
        result := tok.text = ';';
        if result then exit; //ie ignore forward class declaration
      end;
      if not DirectoryExists(destUnitFolder+ 'Classes') then
        MkDir(destUnitFolder+ 'Classes');
      classPath := destUnitFolder+ 'Classes\' + clsName + '\';
      MkDir(classPath);
      ancestor := '<meta name="Ancestor" content="' +ancestor +'">'#10;
      StringToFile(classPath+'_Body.htm',
        htmlStart(5, ancestor) + MakeDescription(comment) + htmlEnd);

      repeat
        //skip private and protected class fields and methods...
        if (tok.text = 'private') or (tok.text = 'protected') then
        repeat
          GetNextToken(tok);
        until finished or (tok.text = 'public') or
          (tok.text = 'published') or (tok.text = 'end');

        if (tok.text = 'end') then
        begin
          GetNextToken(tok);
          result := tok.text = ';';
          break;
        end
        else if (tok.text = 'public') or (tok.text = 'published') then
          GetNextToken(tok);

        case tok.kind of
          tkIdentifier:
            begin
              ClearBuffer;
              AddToBuffer(tok);
              repeat
                GetNextToken(tok);
                AddToBuffer(tok);
              until finished or (tok.text = ';');
              if tok.text <> ';' then break;
              fn := classPath + 'Fields.htm';
              AppendStringToFile(fn, format('<p class="Decl">%s</p>'#10,[buffer]));
              GetNextToken(tok);
            end;
          tkReserved:
            if (tok.text = 'constructor') or (tok.text = 'destructor') or
              (tok.text = 'procedure') then
            begin
              s := tok.text;
              comment := LastSpecialComment;
              s2 := DoProcedure;
              if s2 = '' then exit;
              if not DirectoryExists(classPath +'Methods') then
                MkDir(classPath +'Methods');
              fn := classPath +'Methods\'+FirstWordInStr(s2)+'.htm';
              AppendStringToFile(fn,format('<p class="Decl"><b>%s</b> %s</p>'#10,[s,s2]) +
                MakeDescription(comment));
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(tok);
            end
            else if (tok.text = 'function') then
            begin
              comment := LastSpecialComment;
              s := DoFunction;
              if s = '' then exit;
              if not DirectoryExists(classPath +'Methods') then
                MkDir(classPath +'Methods');
              fn := classPath  +'Methods\' +FirstWordInStr(s) +'.htm';
              AppendStringToFile(fn, '<p class="Decl"><b>function</b> ' +
                  s +'</p>' +MakeDescription(comment));
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(tok);
            end
            else if (tok.text = 'class') then
            begin
              comment := LastSpecialComment;
              GetNextToken(tok);
              if tok.text = 'procedure' then
              begin
                s := 'procedure';
                s2 := DoProcedure;
              end else if tok.text = 'function' then
              begin
                s := 'function';
                s2 := DoFunction;
              end else exit;
              if s2 = '' then exit;
              if not DirectoryExists(classPath +'Methods') then
                MkDir(classPath +'Methods');
              fn := classPath  +'Methods\' +FirstWordInStr(s2) +'.htm';
              AppendStringToFile(fn,
                format('<p class="Decl"><b>class %s</b> %s</p>'#10,[s,s2])
                +MakeDescription(comment));
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(tok);
            end
            else if (tok.text = 'property') then
            begin
              comment := LastSpecialComment;
              s := DoProperty;
              if s = '' then exit;
              s2 := FirstWordInStr(s);
              s := htmlStart(6) + '<p class="Decl"><b>property</b> ' +
                s +'</p>'#10 + MakeDescription(comment)+ htmlEnd;
              if pos('On', s2) = 1 then
              begin
                if not DirectoryExists(classPath +'Events') then
                  MkDir(classPath +'Events');
                StringToFile(classPath +'Events\' +s2 +'.htm', s)
              end else
              begin
                if not DirectoryExists(classPath +'Properties') then
                  MkDir(classPath +'Properties');
                StringToFile(classPath +'Properties\' + s2 +'.htm', s);
              end;
              GetNextToken(tok);
            end
            else exit;
          else exit;
        end;
      until finished;
    end;
    if FileExists(classPath + 'Fields.htm') then
    begin
      PrependStringToFile(classPath + 'Fields.htm', htmlStart(5));
      AppendStringToFile(classPath + 'Fields.htm', htmlEnd);
    end;
  end;

  function DoInterface(const interfaceName: ansiString): boolean;
  var
    s, s2, fn, interfacePath: ansiString;
  begin
    with DelphiParser do
    begin
      PeekNextToken(tok);
      if tok.text = ';' then
      begin
        result := true;
        exit; //ie forward declaration only
      end;
      ClearBuffer;
      AddToBuffer(interfaceName + ' = <b>interface</b><br>'#10);
      repeat
        GetNextToken(tok);
        AddToBuffer(tok);
      until finished or (tok.text = ']');
      result := tok.text = ']';
      if not result then exit;

      if not DirectoryExists(destUnitFolder+ 'Interfaces') then
        MkDir(destUnitFolder+ 'Interfaces');
      interfacePath := destUnitFolder+ 'Interfaces\' + interfaceName + '\';
      MkDir(interfacePath);
      StringToFile(interfacePath+'_Body.htm',
        htmlStart(5) + buffer + MakeDescription(comment) + htmlEnd);

      GetNextToken(tok);
      repeat

        if (tok.text = 'end') then
        begin
          GetNextToken(tok);
          result := tok.text = ';';
          break;
        end;

        comment := LastSpecialComment;
        case tok.kind of
          tkReserved:
            if (tok.text = 'procedure') then
            begin
              s := tok.text;
              s2 := DoProcedure;
              if s2 = '' then exit;
              if not DirectoryExists(interfacePath +'Methods') then
                MkDir(interfacePath +'Methods');
              fn := interfacePath +'Methods\'+FirstWordInStr(s2)+'.htm';
              AppendStringToFile(fn,
                format('<p class="Decl"><b>%s</b> %s</p>'#10,[s,s2]) +
                MakeDescription(comment));
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(tok);
            end
            else if (tok.text = 'function') then
            begin
              s := DoFunction;
              if s = '' then exit;
              if not DirectoryExists(interfacePath +'Methods') then
                MkDir(interfacePath +'Methods');
              fn := interfacePath  +'Methods\' +FirstWordInStr(s) +'.htm';
              AppendStringToFile(fn,
                format('<p class="Decl"><b>function</b> %s</p>'#10,[s]) +
                MakeDescription(comment));

              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(6));
              GetNextToken(tok);
            end
            else if (tok.text = 'property') then
            begin
              s := DoProperty;
              if s = '' then exit;
              s2 := FirstWordInStr(s);
              s := htmlStart(6) + '<p class="Decl"><b>property</b> ' + s +
                '</p>'#10+ MakeDescription(comment)+ htmlEnd;
              if pos('On', s2) = 1 then
              begin
                if not DirectoryExists(interfacePath +'Events') then
                  MkDir(interfacePath +'Events');
                StringToFile(interfacePath +'Events\' +s2 +'.htm', s)
              end else
              begin
                if not DirectoryExists(interfacePath +'Properties') then
                  MkDir(interfacePath +'Properties');
                StringToFile(interfacePath +'Properties\' + s2 +'.htm', s);
              end;
              GetNextToken(tok);
            end
            else exit;
          else exit;
        end;
      until finished;
    end;
  end;

  function DoTypeFunc(const funcName: ansiString): boolean;
  var
    hasBracket: boolean;
  begin
    with DelphiParser do
    begin
      ClearBuffer;
      AddToBuffer(funcName + ' = <b>function</b>');
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        AddToBuffer(tok);
      until finished or (not hasBracket and (tok.text = ';'));
      result := tok.text = ';';
      PeekNextToken(tok);
      if tok.text = 'stdcall' then
      begin
        GetNextToken(tok);
        GetNextToken(tok);
        AddToBuffer(' <b>stdcall</b>;');
      end;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types\' + funcName + '.htm',
        htmlStart(4) + '<p class="Decl">' +buffer +'</p>'#10 + MakeDescription(comment)+ htmlEnd);
    end;
  end;

  function DoTypeProc(const procName: ansiString): boolean;
  var
    hasBracket: boolean;
  begin
    with DelphiParser do
    begin
      ClearBuffer;
      AddToBuffer(procName + ' = <b>procedure</b> ');
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        AddToBuffer(tok);
      until finished or (not hasBracket and (tok.text = ';'));
      result := tok.text = ';';
      PeekNextToken(tok);
      if tok.text = 'stdcall' then
      begin
        GetNextToken(tok);
        GetNextToken(tok);
        AddToBuffer(' <b>stdcall</b>;');
      end;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types\' + procName + '.htm',
        htmlStart(4) + '<p class="Decl">' + buffer + '</p>'#10 +
        MakeDescription(comment) + htmlEnd);
    end;
  end;

  function DoRecord(const recordName, ident2: ansiString): boolean;
  var
    inCase: boolean;
  begin
    result := false;
    ClearBuffer;
    with DelphiParser do
    begin
      if ident2 = 'packed' then
      begin
        GetNextToken(tok);
        if tok.text <> 'record' then exit;
        AddToBuffer(recordName + ' = <b>packed record</b><br>'#10);
      end
      else
        AddToBuffer(recordName + ' = <b>record</b><br>'#10);

      inCase := false;
      repeat
        repeat
          GetNextToken(tok);
          if (tok.text = ';') then
            AddToBuffer(';<br>') else
            AddToBuffer(tok);
          if tok.text = 'case' then
            inCase := true
          else if inCase and (tok.text = 'of') then
          begin
            inCase := false;
            AddToBuffer('<br>');
          end;
        until Finished or (tok.text = ';') or (tok.text = 'end');
      until Finished or (tok.text = 'end');
      GetNextToken(tok);
      result := not Finished and (tok.text = ';');
      AddToBuffer(';');
      if result then
      begin
        if not DirectoryExists(destUnitFolder+ 'Types') then
          MkDir(destUnitFolder+ 'Types');
        StringToFile(destUnitFolder+ 'Types\' + recordName + '.htm',
          htmlStart(4) +'<p class="Decl">' + buffer + '</p>'#10 +
          MakeDescription(comment) + htmlEnd);
      end;
    end;
  end;

  function DoGeneralType(const typeName, ident2: ansiString): boolean;
  begin
    ClearBuffer;
    with DelphiParser do
    begin
      if ReservedList.IndexOf(ident2) >= 0 then
        AddToBuffer(typeName + ' = <b>' + ident2 + '</b>') else
        AddToBuffer(typeName + ' = ' + ident2);
      repeat
        GetNextToken(tok);
        AddToBuffer(tok);
      until Finished or (tok.text = ';') ;
      result := not Finished;
      if not result then exit;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types\' + typeName + '.htm',
        htmlStart(4) + '<p class="Decl">' + buffer + '</p>'#10 +
        MakeDescription(comment) + htmlEnd);
    end;
  end;

  function DoType: boolean;
  var
    ident: ansiString;
  begin
    result := false;
    with DelphiParser do
    begin
      GetNextToken(tok);

      while not finished and (tok.kind = tkIdentifier) do
      begin
        ident := tok.text;
        GetNextToken(tok);
        if tok.text <> '=' then exit;
        GetNextToken(tok);
        comment := LastSpecialComment;
        if tok.kind = tkReserved then
        begin
          if tok.text = 'class' then
          begin
            PeekNextToken(tok);
            if tok.text = 'of' then
              result := DoGeneralType(ident, 'class') else
              result := DoClass(ident)
          end else if tok.text = 'function' then
            result := DoTypeFunc(ident)
          else if tok.text = 'procedure' then
            result := DoTypeProc(ident)
          else if (tok.text = 'packed') or (tok.text = 'record') then
            result := DoRecord(ident, tok.text)
          else if tok.text = 'interface' then
            result := DoInterface(ident)
          else
            result := DoGeneralType(ident, tok.text);
        end
        else
          result := DoGeneralType(ident, tok.text);
        PeekNextToken(tok);
        if not result or (tok.kind <> tkIdentifier) then break;
        GetNextToken(tok);
      end;
    end;
  end;

begin
  Result := -1;
  MkDir(destUnitFolder);

  StringToFile(destUnitFolder+'_Body.htm', htmlStart(3) + '<b>Unit</b> ' +
    ChangeFileExt(ExtractFileName(pasFilename),'') + htmlEnd);

  pasLines := TStringlist.Create;
  try
    pasLines.LoadFromFile(pasFilename);

    ConstList := TStringlist.Create;
    VarList := TStringlist.Create;
    RoutinesList := TStringlist.Create;

    DelphiParser := TDelphiParser.Create(pasLines);
    try
      //find 'interface' identifier ...
      repeat
        DelphiParser.GetNextToken(tok);
        if (tok.kind = tkReserved) and (tok.text = 'interface') then break;
      until DelphiParser.finished;
      //parse the interface section ...
      if not DelphiParser.finished then
        repeat
          DelphiParser.GetNextToken(tok);
          comment := DelphiParser.LastSpecialComment;

          if (tok.kind = tkReserved) then
          begin
            if (tok.text = 'implementation') then break
            else if (tok.text = 'const') then
            begin
              if not DoConst then break
            end else if (tok.text = 'var') then
            begin
              if not DoVars then break
            end else if (tok.text = 'type') then
            begin
              if not DoType  then break
            end else if (tok.text = 'function') then
            begin
              s := DoFunction;
              if s = '' then
                break
              else
              begin
                if not DirectoryExists(destUnitFolder+ 'Routines') then
                  MkDir(destUnitFolder+ 'Routines');
                fn := destUnitFolder+'Routines\'+ FirstWordInStr(s) +'.htm';
                AppendStringToFile(fn,
                  format('<p class="Decl"><b>function</b> %s</p>'#10,[s]) +
                  MakeDescription(comment));
                if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(4));
              end;
            end else if (tok.text = 'procedure') then
            begin
              s := DoProcedure;
              if s = '' then
                break
              else
              begin
                if not DirectoryExists(destUnitFolder+ 'Routines') then
                  MkDir(destUnitFolder+ 'Routines');
                fn := destUnitFolder+'Routines\' +FirstWordInStr(s) +'.htm';
                  AppendStringToFile(fn,
                    format('<p class="Decl"><b>procedure</b> %s</p>'#10,[s]) +
                    MakeDescription(comment));
                if RoutinesList.IndexOf(fn) < 0 then RoutinesList.AddObject(fn, Pointer(4));
              end;
            end;
          end;
        until DelphiParser.finished;

      if (tok.text <> 'implementation') then
        result := DelphiParser.CurrentPt.Y;

      if ConstList.Count > 0 then
      begin
        MkDir(destUnitFolder+ 'Constants');
        ConstList.Insert(0, htmlStart(4));
        ConstList.Add(htmlEnd);
        ConstList.SaveToFile(destUnitFolder+ 'Constants\const.htm');
      end;

      if VarList.Count > 0 then
      begin
        MkDir(destUnitFolder+ 'Vars');
        VarList.Insert(0, htmlStart(4));
        VarList.Add(htmlEnd);
        VarList.SaveToFile(destUnitFolder+ 'Vars\vars.htm');
      end;

     for i := 0 to RoutinesList.Count -1 do
     begin
       //nb: the RoutinesList object simply stores the 'level' of the file ...
       PrependStringToFile(RoutinesList[i],
         htmlStart(integer(RoutinesList.Objects[i])));
       AppendStringToFile(RoutinesList[i], htmlEnd);
     end;

    finally
      DelphiParser.free;
      RoutinesList.Free;
      ConstList.free;
      VarList.Free;
    end;
  finally
    pasLines.Free;
  end;
end;

end.
