unit Utils;

{$I DocProcessor.inc}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Classes, SysUtils, FileCtrl, Contnrs, SimpleDOM, DocStructure,
  Forms, StdCtrls;

function DirName(const FullPath: string): string;
function FileNameNoExt(const FileName: string): string;
function GetMeta(const FileName: string; const Name: string): string;
function GetFileList(FDirectory, Filter: TFileName): TStringList;
function GetDirList(FDirectory, Filter: TFileName): TStringList;
function CompareDirectories(List: TStringList; Index1, Index2: Integer): Integer;
function GetLinkName(const Target: string): string;
function CompareLinks(List: TStringList; Index1, Index2: Integer): Integer;
function CompareElements(Item1, Item2: Pointer): Integer;
procedure RunCommandInMemo(const Command: String; AMemo: TMemo);

{$IFNDEF SUPPORTS_UNICODE}
type
  TSysCharSet = set of AnsiChar;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
{$ENDIF}

implementation

uses
  MainUnit;

const
  CDirInfoFile = 'DirInfo.xml';

function DirName(const FullPath: string): string;
begin
  Result := ExtractFileName(ExcludeTrailingBackslash(FullPath));
end;

function FileNameNoExt(const FileName: string): string;
begin
  Result := StringReplace(ExtractFileName(FileName), '/', '\', [rfReplaceAll]);
  Result := Copy(Result, 1, Length(Result) - Length(ExtractFileExt(Result)));
end;

function GetMeta(const FileName: string; const Name: string): string;
var
  Dom: TDomDocument;
  Head: TDomNode;
  Nodes: TDomNodeList;
  I: Integer;
begin
  // get a content of the META tag (in HTML head) specified by Name parameter
  Result := '';
  if not FileExists(FileName) then Exit;
  Dom := TDomDocument.Create;
  try
    Dom.LoadFromFile(FileName);
    Head := Dom.FindNode('head', True);
    if Head = nil then Exit;
    Nodes := Head.FindNodes('meta', False);
    for I := 0 to Nodes.Count - 1 do
      if SameText(Nodes.Items[I].Attributes['name'], Name) then
      begin
        Result := Nodes.Items[I].Attributes['content'];
        Exit;
      end;
  finally
    Dom.Free;
  end;
end;

function GetFileList(FDirectory, Filter: TFileName): TStringList;
var
  ARec: TSearchRec;
  Res: Integer;
begin
  if FDirectory[Length(FDirectory)] <> '\' then
    FDirectory := FDirectory + '\';

  Result := TStringList.Create;
  try
    Res := FindFirst(FDirectory + Filter, faAnyFile, ARec);
    while Res = 0 do
    begin
      if FileExists(FDirectory + ARec.Name) then
        Result.Add(FDirectory + ARec.Name);

      Res := FindNext(ARec);
    end;
    FindClose(ARec);
  except
    Result.Free;
  end;
end;

function GetDirList(FDirectory, Filter: TFileName): TStringList;
var
  ARec: TSearchRec;
  Res: Integer;
begin
  if FDirectory[Length(FDirectory)] <> '\' then
    FDirectory := FDirectory + '\';

  Result := TStringList.Create;
  try
    Res := FindFirst(FDirectory + Filter, faDirectory, ARec);
    while Res = 0 do
    begin
      if ARec.Name[1] <> '.' then
        if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(FDirectory + ARec.Name)
          and not SameText(ARec.Name, 'CVS') then
          Result.Add(FDirectory + ARec.Name);
      Res := FindNext(ARec);
    end;
    FindClose(ARec);
  except
    Result.Free;
  end;
end;

function CompareDirectories(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1: string;
  S2: string;
begin
  S1 := DirName(List[Index1]);
  S2 := DirName(List[Index2]);
  Result := AnsiCompareStr(S1, S2);
end;

function GetLinkName(const Target: string): string;
var
  I: Integer;
begin
  if Target = '' then Result := ''
  else
  begin
    I := Pos('#', Target);
    if I > 0 then Result := Copy(Target, I + 1, 1000)
    else
    begin
      I := Length(Target);
      Result := Target;
      while I > 0 do
      begin
        if CharInSet(Target[I], ['/', '\']) then
        begin
          Result := Copy(Target, I + 1, 1000);
          Break;
        end;
        Dec(I);
      end;
      Result := FileNameNoExt(Result);
      if Result = '_Body' then
        Result := DirName(ExtractFilePath(Target));
      if Result = '_Home' then Result := 'Home';
    end;
  end;
end;

function GetLinkName2(const Target: string): string;
var
  I: Integer;
begin
  I := Pos('#', Target);
  if I > 0 then
    Result := Copy(Target, 1, I - 1)
  else
    Result := Target;
end;

function CompareLinks(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareStr(GetLinkName2(List[Index1]), GetLinkName2(List[Index2]));
end;

function CompareElements(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TElement(Item1).DisplayName, TElement(Item2).DisplayName);
end;

procedure RunCommandInMemo(const Command: string; AMemo: TMemo);
const
  ReadBuffer = 2400;
var
  Security: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  BytesRead, AppRunning: DWord;
  Start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PAnsiChar;
begin
   with Security do
   begin
     nlength := SizeOf(TSecurityAttributes);
     binherithandle := true;
     lpsecuritydescriptor := nil;
   end;

  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then
  begin
    Buffer := AllocMem(ReadBuffer + 1);
    FillChar(Start,Sizeof(Start),#0);
    start.cb := SizeOf(start);
    start.hStdOutput := WritePipe;
    start.hStdInput := ReadPipe;
    start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    start.wShowWindow := SW_HIDE;

    if CreateProcess(nil, PChar(Command), @Security, @Security, true,
      NORMAL_PRIORITY_CLASS, nil, nil, start, ProcessInfo) then
    begin
      repeat
        AppRunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
        Application.ProcessMessages;
      until AppRunning <> WAIT_TIMEOUT;

      repeat
        BytesRead := 0;
        ReadFile(ReadPipe, Buffer[0], ReadBuffer, BytesRead, nil);
        Buffer[BytesRead] := #0;
        OemToAnsi(Buffer, Buffer);
        AMemo.Text := AMemo.Text + AnsiString(Buffer);
      until BytesRead < ReadBuffer;
    end;

    FreeMem(Buffer);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ReadPipe);
    CloseHandle(WritePipe);
  end;
end;

{$IFNDEF SUPPORTS_UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

end.
