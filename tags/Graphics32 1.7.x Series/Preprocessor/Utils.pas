unit Utils;

interface

uses
  Windows, Classes, SysUtils, FileCtrl, Contnrs, SimpleDOM, DocStructure;

function DirName(const FullPath: string): string;
function FileNameNoExt(const FileName: string): string;
function GetMeta(const FileName: string; const Name: string): string;
function GetFileList(FDirectory, Filter: TFileName): TStringList;
function GetDirList(FDirectory, Filter: TFileName): TStringList;
function CompareDirectories(List: TStringList; Index1, Index2: Integer): Integer;
function GetLinkName(const Target: string): string;
function CompareLinks(List: TStringList; Index1, Index2: Integer): Integer;
function CompareElements(Item1, Item2: Pointer): Integer;

implementation

uses MainUnit;

const CDirInfoFile = 'DirInfo.xml';

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
        if DirectoryExists(FDirectory + ARec.Name) and not SameText(ARec.Name, 'CVS') then
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
        if Target[I] in ['/', '\'] then
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

function CompareLinks(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareStr(GetLinkName(List[Index1]), GetLinkName(List[Index2]));
end;

function CompareElements(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TElement(Item1).DisplayName, TElement(Item2).DisplayName);
end;

end.
