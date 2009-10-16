unit Pas2Html;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, DelphiParse,
  ShellApi, ShlObj, Forms;

  function GetDelphiSourceFolder: string;
  function BuildNewUnit(const pasFilename, destUnitFolder: string): integer;

implementation

const
  htmlStart = '<html>'#10'<head>'#10'<title>Untitled</title>'#10'<link rel="stylesheet" '+
    'href="/styles/default.css" type="text/css">'#10'</head>'#10'<body bgcolor="#FFFFFF">'#10;
  htmlEnd = #10'</body>'#10'</html>';
  cr: AnsiChar = #10;

//------------------------------------------------------------------------------
// GetFolder functions ...
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
        ShGetFileInfo(PAnsiChar(lParam), 0, sfi,sizeof(sfi),SHGFI_DISPLAYNAME or SHGFI_PIDL);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT,0, integer(@sfi.szDisplayName));
      end;
  end;
  result := 0;
end;
//------------------------------------------------------------------------------

procedure CoTaskMemFree(pv: Pointer); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

//------------------------------------------------------------------------------

function StripSlash(const path: string): string;
var
  len: integer;
begin
  result := path;
  len := length(path);
  if (len = 0) or (path[len] <> '\') then exit;
  setlength(result,len-1);
end;
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
  bi.pszDisplayName := PAnsiChar(@displayname[0]);
  bi.lpszTitle := PAnsiChar(Caption);
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
    result := SHGetPathFromIDList(pidl,PAnsiChar(@displayname[0]));
    Folder := displayname;
  finally
    CoTaskMemFree(pidl);
  end;
end;
//------------------------------------------------------------------------------

function GetSpecialFolder(FolderID: integer): string;
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
  GetFolder(application.MainForm,'Location of Delphi PAS Files ...', false, result);
end;
//------------------------------------------------------------------------------

procedure StringToFile(const filename, strVal: string);
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

procedure AppendStringToFile(const filename, strVal: string);
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

procedure PrependStringToFile(const filename, strVal: string);
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

function FirstWordInStr(const s: string): string;
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

function BuildNewUnit(const pasFilename, destUnitFolder: string): integer;
var
  i: integer;
  pasLines: TStringlist;
  DelphiParser: TDelphiParser;
  ConstList, VarList, RoutinesList: TStringList;
  tok: TToken;
  s, fn: string;

  function DoConst: boolean;
  var
    ident, value: string;
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
        value := '';
        repeat
          GetNextToken(tok);
          if tok.kind = tkText then
            value := value + SINGLEQUOTE + tok.text + SINGLEQUOTE else
            value := value + tok.text + ' ';
        until finished or (tok.text = ';');
        result := tok.text = ';';
        if result then
          ConstList.Add(format('%s %s<br>'#10,[ident, value]));
      end;
    end;
  end;

  function DoVars: boolean;
  var
    ident, value: string;
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
        GetNextToken(tok);
        value := '';
        hasBracket := false;
        repeat
          GetNextToken(tok);
          if (tok.text = '(') then hasBracket := true
          else if (tok.text = ')') then hasBracket := false;
          if tok.kind = tkText then
            value := value + SINGLEQUOTE + tok.text + SINGLEQUOTE else
            value := value + tok.text +' ';
        until finished or (not hasBracket and (tok.text = ';'));
        result := tok.text = ';';
        PeekNextToken(tok);
        if tok.text = 'stdcall' then
        begin
          GetNextToken(tok);
          GetNextToken(tok);
          s := s + ' <b>stdcall</b>;';
        end;
        if result then
          VarList.Add(format('%s : %s<br>'#10,[ident, value]));
      end;
    end;
  end;

  function DoFunction: string;
  var
    s: string;
    hasBracket: boolean;
  begin
    result := '';
    with DelphiParser do
    begin
      GetNextToken(tok);
      if not (tok.kind in [tkIdentifier, tkReserved]) then exit;
      s := tok.text;
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
      until finished or (not hasBracket and (tok.text = ':'));
      if tok.text <> ':' then exit;
      repeat
        GetNextToken(tok);
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
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
        begin
          s := s + '<b>'+ tok.text + '</b>; ';
        end
        else break;
        GetNextToken(tok); //ie gobbles peek
        GetNextToken(tok);
        if tok.text <> ';' then exit;
      end;
      result := s;
    end;
  end;

  function DoProcedure: string;
  var
    s: string;
    hasBracket: boolean;
  begin
    result := '';
    with DelphiParser do
    begin
      GetNextToken(tok);
      if not (tok.kind in [tkIdentifier, tkReserved]) then exit;
      s := tok.text;
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
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
        begin
          s := s + ' <b>' +tok.text +'</b>;';
        end
        else break;
        GetNextToken(tok); //ie gobbles peek
        GetNextToken(tok);
        if tok.text <> ';' then exit;
      end;
      result := s;
    end;
  end;

  function DoProperty: string;
  var
    s: string;
  begin
    result := '';
    with DelphiParser do
    begin
      GetNextToken(tok);
      if tok.kind <> tkIdentifier then exit;
      s := tok.text + ' ';
      PeekNextToken(tok);
      if tok.text = ';' then
      begin
        GetNextToken(tok); //gobble the peek
        result := s +';';
        exit;
      end;
      repeat
        GetNextToken(tok);
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
      until finished or (tok.text = ':');
      GetNextToken(tok);
      if tok.kind in [tkIdentifier, tkReserved] then
        s := s + tok.text +';' else
        exit;
      //now skip the rest of the property stuff (ie read, write etc) ...
      repeat
        GetNextToken(tok);
      until finished or (tok.text = ';');
      if tok.text = ';' then
        result := s else
        exit;
      while true do
      begin
        PeekNextToken(tok);
        if (tok.text = 'default') or (tok.text = 'stored') then
        repeat
          GetNextToken(tok);
        until finished or (tok.text = ';')
        else break;
      end;
    end;
  end;

  function DoClass(const clsName: string): boolean;
  var
    s, s2, fn, classPath: string;
  begin
    with DelphiParser do
    begin
      GetNextToken(tok);
      result := tok.text = ';';
      if result then exit; //ie ignore forward class declaration
      if (tok.text = '(') then
      begin
        s := '( ';
        repeat
          GetNextToken(tok);
          if tok.kind = tkReserved then
            s := s + '<b>' +tok.text +'</b> ' else
            s := s + tok.text +' ';
        until finished or (tok.text = ')');
        if tok.text <> ')' then exit;
        GetNextToken(tok);
        result := tok.text = ';';
        if result then exit; //ie ignore forward class declaration
      end
      else s := '';
      if not DirectoryExists(destUnitFolder+ 'Classes') then
        MkDir(destUnitFolder+ 'Classes');
      classPath := destUnitFolder+ 'Classes\' + clsName + '\';
      MkDir(classPath);
      StringToFile(classPath+'_Body.htm',
        htmlStart + '<b>'+clsName +'</b> = <b>class</b>'+ s + htmlEnd);

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
              s := tok.text + ' ';
              repeat
                GetNextToken(tok);
                if tok.kind = tkReserved then
                  s := s + '<b>' +tok.text +'</b> ' else
                  s := s + tok.text +' ';
              until finished or (tok.text = ';');
              if tok.text <> ';' then break;
              fn := classPath + 'Fields.htm';
              AppendStringToFile(fn, s + '<br>'#10);
              GetNextToken(tok);
            end;
          tkReserved:
            if (tok.text = 'constructor') or (tok.text = 'destructor') or
              (tok.text = 'procedure') then
            begin
              s := tok.text;
              s2 := DoProcedure;
              if s2 = '' then exit;
              if not DirectoryExists(classPath +'Methods') then
                MkDir(classPath +'Methods');
              fn := classPath +'Methods\'+FirstWordInStr(s2)+'.htm';
              AppendStringToFile(fn, '<b>'+s +'</b> ' +s2 +'<br>'#10);
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
              GetNextToken(tok);
            end
            else if (tok.text = 'function') then
            begin
              s := DoFunction;
              if s = '' then exit;
              if not DirectoryExists(classPath +'Methods') then
                MkDir(classPath +'Methods');
              fn := classPath  +'Methods\' +FirstWordInStr(s) +'.htm';
              AppendStringToFile(fn, '<b>function</b> ' +s +'<br>'#10);
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
              GetNextToken(tok);
            end
            else if (tok.text = 'class') then
            begin
              GetNextToken(tok);
              if tok.text = 'procedure' then
              begin
                s := '<b>class procedure</b> ';
                s2 := DoProcedure;
              end else if tok.text = 'function' then
              begin
                s := '<b>class function</b> ';
                s2 := DoFunction;
              end else exit;
              if s2 = '' then exit;
              if not DirectoryExists(classPath +'Methods') then
                MkDir(classPath +'Methods');
              fn := classPath  +'Methods\' +FirstWordInStr(s2) +'.htm';
              AppendStringToFile(fn, s +s2 +'<br>'#10);
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
              GetNextToken(tok);
            end
            else if (tok.text = 'property') then
            begin
              s := DoProperty;
              if s = '' then exit;
              s2 := FirstWordInStr(s);
              s := htmlStart + '<b>property</b> ' + s + htmlEnd;
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
      PrependStringToFile(classPath + 'Fields.htm', htmlStart);
      AppendStringToFile(classPath + 'Fields.htm', htmlEnd);
    end;
  end;

  function DoInterface(const interfaceName: string): boolean;
  var
    s, s2, fn, interfacePath: string;
  begin
    with DelphiParser do
    begin
      s := interfaceName + ' = <b>interface</b><br>'#10;
      repeat
        GetNextToken(tok);
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
      until finished or (tok.text = ']');
      result := tok.text = ']';
      if not result then exit;

      if not DirectoryExists(destUnitFolder+ 'Interfaces') then
        MkDir(destUnitFolder+ 'Interfaces');
      interfacePath := destUnitFolder+ 'Interfaces\' + interfaceName + '\';
      MkDir(interfacePath);
      StringToFile(interfacePath+'_Body.htm',htmlStart + s + htmlEnd);

      GetNextToken(tok);
      repeat

        if (tok.text = 'end') then
        begin
          GetNextToken(tok);
          result := tok.text = ';';
          break;
        end;

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
              AppendStringToFile(fn, '<b>'+ s +'</b> ' +s2 +'<br>'#10);
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
              GetNextToken(tok);
            end
            else if (tok.text = 'function') then
            begin
              s := DoFunction;
              if s = '' then exit;
              if not DirectoryExists(interfacePath +'Methods') then
                MkDir(interfacePath +'Methods');
              fn := interfacePath  +'Methods\' +FirstWordInStr(s) +'.htm';
              AppendStringToFile(fn, '<b>function</b> ' +s +'<br>'#10);
              if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
              GetNextToken(tok);
            end
            else if (tok.text = 'property') then
            begin
              s := DoProperty;
              if s = '' then exit;
              s2 := FirstWordInStr(s);
              s := htmlStart + '<b>property</b> ' + s + htmlEnd;
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

  function DoTypeFunc(const funcName: string): boolean;
  var
    s: string;
    hasBracket: boolean;
  begin
    with DelphiParser do
    begin
      s := funcName + ' = <b>function</b> ';
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
      until finished or (not hasBracket and (tok.text = ';'));
      result := tok.text = ';';
      PeekNextToken(tok);
      if tok.text = 'stdcall' then
      begin
        GetNextToken(tok);
        GetNextToken(tok);
        s := s + ' <b>stdcall</b>;';
      end;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types\' + funcName + '.htm',
        htmlStart + s + htmlEnd);
    end;
  end;

  function DoTypeProc(const procName: string): boolean;
  var
    s: string;
    hasBracket: boolean;
  begin
    with DelphiParser do
    begin
      s := procName + ' = <b>procedure</b> ';
      hasBracket := false;
      repeat
        GetNextToken(tok);
        if tok.text = '(' then hasBracket := true
        else if tok.text = ')' then hasBracket := false;
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
      until finished or (not hasBracket and (tok.text = ';'));
      result := tok.text = ';';
      PeekNextToken(tok);
      if tok.text = 'stdcall' then
      begin
        GetNextToken(tok);
        GetNextToken(tok);
        s := s + ' <b>stdcall</b>;';
      end;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types\' + procName + '.htm',
        htmlStart + s + htmlEnd);
    end;
  end;

  function DoRecord(const recordName, ident2: string): boolean;
  var
    s: string;
  begin
    result := false;
    with DelphiParser do
    begin
      if ident2 = 'packed' then
      begin
        GetNextToken(tok);
        if tok.text <> 'record' then exit;
        s := recordName + ' = <b>packed record</b><br>'#10;
      end
      else s := recordName + ' = <b>record</b><br>'#10;
      repeat
        repeat
          GetNextToken(tok);
          if tok.kind = tkReserved then
            s := s + '<b>' +tok.text +'</b> ' else
            s := s + tok.text +' ';
        until Finished or (tok.text = ';') or (tok.text = 'end');
      until Finished or (tok.text = 'end');
      GetNextToken(tok);
      result := not Finished and (tok.text = ';');
      s := s + ';';
      if result then
      begin
        if not DirectoryExists(destUnitFolder+ 'Types') then
          MkDir(destUnitFolder+ 'Types');
        StringToFile(destUnitFolder+ 'Types\' + recordName + '.htm',
          htmlStart + s + htmlEnd);
      end;
    end;
  end;

  function DoGeneralType(const typeName, ident2: string): boolean;
  var
    s: string;
  begin
    with DelphiParser do
    begin
      s := typeName + ' = ' + ident2 +' ';
      repeat
        GetNextToken(tok);
        if tok.kind = tkReserved then
          s := s + '<b>' +tok.text +'</b> ' else
          s := s + tok.text +' ';
      until Finished or (tok.text = ';') ;
      result := not Finished;
      if not result then exit;
      if not DirectoryExists(destUnitFolder+ 'Types') then
        MkDir(destUnitFolder+ 'Types');
      StringToFile(destUnitFolder+ 'Types\' + typeName + '.htm',
        htmlStart + s + htmlEnd);
    end;
  end;

  function DoType: boolean;
  var
    ident: string;
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

  StringToFile(destUnitFolder+'_Body.htm',htmlStart + '<b>Unit</b> ' +
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
                AppendStringToFile(fn, '<b>function</b> ' +s +'<br>'#10);
                if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
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
                AppendStringToFile(fn, '<b>procedure</b> ' +s +'<br>'#10);
                if RoutinesList.IndexOf(fn) < 0 then RoutinesList.Add(fn);
              end;
            end;
          end;
        until DelphiParser.finished;

      if (tok.text <> 'implementation') then
        result := DelphiParser.CurrentPt.Y;

      if ConstList.Count > 0 then
      begin
        MkDir(destUnitFolder+ 'Constants');
        ConstList.Insert(0, htmlStart);
        ConstList.Add(htmlEnd);
        ConstList.SaveToFile(destUnitFolder+ 'Constants\const.htm');
      end;

      if VarList.Count > 0 then
      begin
        MkDir(destUnitFolder+ 'Vars');
        VarList.Insert(0, htmlStart);
        VarList.Add(htmlEnd);
        VarList.SaveToFile(destUnitFolder+ 'Vars\vars.htm');
      end;

     for i := 0 to RoutinesList.Count -1 do
     begin
       PrependStringToFile(RoutinesList[i], htmlStart);
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
