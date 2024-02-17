unit MainUnit;

{$I DocProcessor.inc}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, FileCtrl, ComCtrls, Contnrs, IniFiles, ExtCtrls, ShellApi,
  Utils, SimpleDOM, DocStructure, Pas2Html, FixRecordFormat, Vcl.WinXPickers;

type
  TMainForm = class(TForm)
    BtnClose: TButton;
    BtnOpen: TButton;
    BtnParseMissing: TButton;
    BtnSaveProjectInfo: TButton;
    BtnTransform: TButton;
    CbxReportBrokenLinks: TCheckBox;
    CbxOpenAfterProcess: TCheckBox;
    CmbProjectName: TComboBox;
    EdtProjectDirectory: TEdit;
    EdtProjectTitle: TEdit;
    EdtVersionString: TEdit;
    LblProgress: TLabel;
    LblProjectDirectory: TLabel;
    LblProjectFileName: TLabel;
    LblProjectTitle: TLabel;
    LblVersionString: TLabel;
    Log: TMemo;
    OpnDlgPAS: TOpenDialog;
    PnlControl: TPanel;
    PnlLog: TPanel;
    PnlMisc: TPanel;
    PnlProgress: TPanel;
    PnlProjectInfo: TPanel;
    PnlProjectInfoHead: TPanel;
    PnlTransComp: TPanel;
    Progress: TProgressBar;
    cbxReportBrokenImages: TCheckBox;
    Label1: TLabel;
    dtpProjectBuildDate: TDateTimePicker;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnParseMissingClick(Sender: TObject);
    procedure BtnProcessClick(Sender: TObject);
    procedure BtnSaveProjectInfoClick(Sender: TObject);
    procedure BtnTransformClick(Sender: TObject);
    procedure CmbProjectNameChange(Sender: TObject);
    procedure CmbProjectNameClick(Sender: TObject);
    procedure EdtProjectDirectoryChange(Sender: TObject);
    procedure EdtProjectTitleChange(Sender: TObject);
    procedure EdtCHMCompilerChange(Sender: TObject);
    procedure EdtVersionStringChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    projectFolders: TStringList;
    ProjectDir: TFileName;
    SourceDir: TFileName;
    CompiledDir: TFileName;
    ExeFilename: TFileName;
    procedure LoadProperties;
    procedure LoadProject(const ProjName: TFileName);
    procedure SaveProject;
    function StartTransforming: Boolean;
    function GetProjectIniFilename(const folder, projectName: string): string;
  end;

var
  ExePath: string;

  NoGUI: Boolean;
  MainForm: TMainForm;
  Project: TProject;

  LogElemCnt: integer;
  LogElemsCnt: integer;

procedure LogAdd(const S: string);

implementation

const
  CDots: string = '...';
  CRLF = #13#10;

  BodyFileContent =
    '<html>'#10'<head></head>'#10'<body></body>'#10'</html>';
  indexFileContent =
    '<html>'#10+
    '<head>'#10+
    '  <meta http-equiv = "refresh" content = "0; URL=./_Body.htm"/>'#10+
    '</head>'#10+
    '<body>'#10+
    '  <p>Redirected <a href="./_Body.htm">here</a></p>'#10+
    '</body>'#10+
    '</html>';
  indexFileContent2 =
    '<html>'#10+
    '<head>'#10+
    '  <meta http-equiv = "refresh" content = "0; URL=./Docs/_Body.htm"/>'#10+
    '</head>'#10+
    '<body>'#10+
    '  <p>Redirected <a href="./Docs/_Body.htm">here</a></p>'#10+
    '</body>'#10+
    '</html>';

resourcestring
  RCStrTransformingFiles = 'Transforming Files';
  RCStrWritingProject = 'Writing Project';
  RCStrProjectContains = 'Project Contains';
  RCStrReadingFiles = 'Reading files';
  RCStrBuildingClassHierarchy = 'Building Class Hierarchy';
  RCStrDeletingDocFolder = 'Deleting Doc folder';
  RCStrTransformingFile = 'Transforming File';
  RCStrTransformingHalted = 'Transforming halted.';
  RCStrBrokenLinksFound = 'Broken links found';
  RCStrErrorDestinationFolderDoesNotExist = 'Error: destination folder does not exist.';
  RCStrStartingPas2Html = 'Starting Pas2Html';

{$R *.DFM}

procedure LogAdd(const S: string);
begin
  if NoGUI then
    Writeln(S)
  else
  with MainForm.Log do
  begin
    SelStart := Length(Text);
    SelText := S;
    SelStart := Length(Text);
  end;
end;

procedure LogNL;
begin
  if NoGUI then
    Writeln('')
  else
  with MainForm.Log do
  begin
    SelStart := Length(Text);
    SelText := CRLF;
    SelStart := Length(Text);
  end;
end;

procedure LogReplace(S: string);
begin
  if NoGUI then
    Writeln(S)
  else
  with MainForm.Log do
    Lines[Lines.Count - 1] := S;
end;

function TMainForm.GetProjectIniFilename(const folder, projectName: string): string;
begin
  Result := IncludeTrailingBackslash(folder) + projectName + '.ini';
end;

procedure TMainForm.EdtCHMCompilerChange(Sender: TObject);
begin
  //BtnCompile.Enabled := FileExists(EdtCHMCompiler.Text);
  EdtProjectTitleChange(Sender);
end;

procedure TMainForm.EdtProjectDirectoryChange(Sender: TObject);
begin
  ProjectDir := EdtProjectDirectory.Text;
  if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(ProjectDir) then
  begin
    ProjectDir := IncludeTrailingBackslash(ProjectDir);
    if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(ProjectDir + 'Source') then
    begin
      SourceDir := ProjectDir + 'Source\';
      BtnTransform.Enabled := True;
      BtnSaveProjectInfo.Enabled := True;
      if EdtProjectDirectory.Focused then Log.Lines.Clear;
      Exit;
    end;
  end;
  BtnTransform.Enabled := False;
  ProjectDir := '';
  SourceDir := '';
  if not EdtProjectDirectory.Focused then Exit;
  LogNL;
  LogAdd('Caution: no ''Source'' folder found.');
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  iniFilename: string;
begin
  projectFolders := TStringList.Create;

  EdtProjectDirectoryChange(Self);

  ExeFilename := paramstr(0);
  ExePath := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  LoadProperties;

  NoGUI := False;


  if (ParamCount > 1) then
  begin
    iniFilename := ExePath + paramstr(2);
    if not FileExists(iniFilename) then Exit;

    LoadProject(iniFilename);

    if ParamCount <= 2 then Exit;

    if FindCmdLineSwitch('nogui') then
    begin
      NoGUI := True;
      Self.Hide;
    end
    else if FindCmdLineSwitch('transform') then
    begin
      BtnTransform.Click;
      Application.Terminate;
    end;
  end else
  begin
    if (CmbProjectName.ItemIndex < 0) then Exit;
    with CmbProjectName do
      LoadProject(projectFolders[ItemIndex]+ Text + '.ini');
    if BtnTransform.Enabled then
      ActiveControl := BtnTransform;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  projectFolders.Free;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Index: Integer;
begin
  CanClose := True;
  if CmbProjectName.Items.Count <> projectFolders.Count then Exit;

  if not NoGUI and BtnSaveProjectInfo.Enabled and
    (MessageBox(self.handle, 'Save Project Information', PChar(caption),
      MB_YESNO or MB_DEFBUTTON1) = IDYES) then SaveProject;

  with TIniFile.Create(ChangeFileExt(ExeFilename, '.ini')) do
  try
    WriteInteger('Settings', 'ActiveProject', CmbProjectName.ItemIndex);
    if SectionExists('Projects') then EraseSection('Projects');
    if SectionExists('ProjectFolders') then EraseSection('ProjectFolders');
    with CmbProjectName do
      for Index := 0 to Items.Count - 1 do
      begin
        WriteString('Projects',
          'ProjectName' + IntToStr(Index + 1), Items[Index]);
        WriteString('ProjectFolders', Items[Index], projectFolders[Index]);
      end;
  finally
    Free;
  end;
end;

procedure TMainForm.SaveProject;
var
  Index: Integer;
  Ini: TIniFile;
begin
  Index := CmbProjectName.Items.IndexOf(CmbProjectName.Text);
  if Index < 0 then
  begin
    index := CmbProjectName.Items.Add(CmbProjectName.Text);
    projectFolders.Add(EdtProjectDirectory.Text);
    CmbProjectName.ItemIndex := index;
  end
  else
    projectFolders[index] := EdtProjectDirectory.Text;

  Ini := TIniFile.Create(
    GetProjectIniFilename(EdtProjectDirectory.Text, CmbProjectName.Text));
  try
    Ini.WriteString('Settings', 'ProjectName', CmbProjectName.Text);
    for Index := 0 to Self.ComponentCount - 1 do
      if Self.Components[Index].InheritsFrom(TCustomEdit) and
         not Self.Components[Index].InheritsFrom(TMemo) then
        Ini.WriteString('Settings', Copy(Self.Components[Index].Name, 4, MAXINT), TEdit(Self.Components[Index]).Text)
      else if Self.Components[Index].InheritsFrom(TCheckBox) then
        Ini.WriteBool('Settings', Copy(Self.Components[Index].Name, 4, MAXINT), TCheckBox(Self.Components[Index]).Checked)
      else if Self.Components[Index].InheritsFrom(TDateTimePicker) then
        Ini.WriteDate('Settings',
          Copy(Self.Components[Index].Name, 4, MAXINT),
          TDateTimePicker(Self.Components[Index]).Date);
  finally
    Ini.Free;
  end;
end;

type TDateTimePickerHack = class(TDateTimePicker);

function ReadValueOnly(const sectionValue: string): string;
begin
  Result := Copy(sectionValue, Pos('=', sectionValue) +1, Length(sectionValue));
end;

procedure TMainForm.LoadProperties;
var
  i: Integer;
begin
  with TIniFile.Create(ChangeFileExt(ExeFilename, '.ini')) do
  try
    ReadSectionValues('Projects', CmbProjectName.Items);
    for i := 0 to CmbProjectName.Items.Count -1 do
      CmbProjectName.Items[i] := ReadValueOnly(CmbProjectName.Items[i]);
    ReadSectionValues('ProjectFolders', projectFolders);
    for i := 0 to projectFolders.Count -1 do
      projectFolders[i] :=
        IncludeTrailingBackslash(ReadValueOnly(projectFolders[i]));

    if CmbProjectName.Items.Count = 0 then Exit;

    if CmbProjectName.Items.Count <> projectFolders.Count then
    begin
      LogAdd(CRLF +
        'There''s a problem in Properties.ini.'+CRLF+
        'The Projects and ProjectFolders section counts '+
        'don''t match.'+CRLF);
      Log.Color := $E7E7FF;
      Exit;
    end;

    i := ReadInteger('Settings', 'ActiveProject', 0);
    CmbProjectName.ItemIndex := i;
    ProjectDir := projectFolders[i];
  finally
    Free;
  end;
end;

procedure TMainForm.LoadProject(const ProjName: TFileName);
var
  Ini: TIniFile;
  Index: Integer;
  StrValue: string;
begin
  if not FileExists(ProjName) then
  begin
    LogAdd('Couldn''t find: ' + ProjName);
    Log.Color := $E7E7FF;
    Exit;
  end;
  ini := TIniFile.Create(ProjName);
  try
    StrValue := Ini.ReadString('Settings', 'ProjectName', 'MyProjectName');
    Index := CmbProjectName.Items.IndexOf(StrValue);
    if Index >= 0 then
      CmbProjectName.ItemIndex := Index
    else
    begin
      Index := CmbProjectName.Items.Add(StrValue);
      CmbProjectName.ItemIndex := index;
      projectFolders.Add(ProjName);
    end;

    BtnTransform.Enabled := false; //assume a problem

    dataTypesCaption := ini.ReadString('Settings', 'AlternateTypesName', '');
    if dataTypesCaption = '' then dataTypesCaption := DefaultTypesCaption;


    staticFuncsCaption := ini.ReadString('Settings', 'AlternateStaticFuncsName', '');
    if staticFuncsCaption = '' then staticFuncsCaption := DefaultStaticFuncsCaption;

    for Index := 0 to Self.ComponentCount - 1 do
      if Self.Components[Index].InheritsFrom(TCustomEdit) and
         not Self.Components[Index].InheritsFrom(TMemo) then
      begin
        StrValue := Ini.ReadString('Settings', Copy(Self.Components[Index].Name, 4, MAXINT), '');
        if StrValue <> '' then TEdit(Self.Components[Index]).Text := StrValue;
      end
      else if Self.Components[Index].InheritsFrom(TCheckBox) then
        TCheckBox(Self.Components[Index]).Checked :=
          Ini.ReadBool('Settings', Copy(Self.Components[Index].Name, 4, MAXINT), False)
      else if Self.Components[Index].InheritsFrom(TDateTimePicker) then
        with TDateTimePickerHack(Self.Components[Index]) do
        begin
          Date := Ini.ReadDate('Settings',
            Copy(Self.Components[Index].Name, 4, MAXINT), Date);
          if Assigned(OnChange) then OnChange(nil);
      end;
  finally
    Ini.Free;
  end;

  Log.Lines.Clear;
  BtnSaveProjectInfo.Enabled := False;

  if not BtnTransform.Enabled then
  begin
    //no project directory with source subfolder found
    LogNL;
    LogAdd('Caution: no ''Source'' folder found.');
  end;
end;

procedure TMainForm.BtnSaveProjectInfoClick(Sender: TObject);
begin
  SaveProject;
  BtnSaveProjectInfo.Enabled := False;
end;

procedure TMainForm.CmbProjectNameChange(Sender: TObject);
begin
  BtnSaveProjectInfo.Enabled :=
    CmbProjectName.Items.IndexOf(CmbProjectName.Text) < 0;
end;

procedure TMainForm.CmbProjectNameClick(Sender: TObject);
begin
  if CmbProjectName.ItemIndex >= 0 then
    with CmbProjectName do
      LoadProject(projectFolders[ItemIndex]+ Text + '.ini');
end;

procedure TMainForm.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    VK_F7: BtnTransformClick(nil);
  end;
end;

procedure TMainForm.BtnProcessClick(Sender: TObject);
begin
  if not StartTransforming then Exit;
end;

procedure WriteRedirectFile(const folder, title: string);
begin
  with TStringList.Create do
  try
    text := indexFileContent;
    SaveToFile(folder + 'Docs\index.htm');

    text := indexFileContent2;
    SaveToFile(folder + 'index.htm');
  finally
    Free;
  end;
end;

function CleanFoldername(const foldername: string): string;
var
  len: integer;
begin
  Result := foldername;
  if Length(Result) < 3 then Exit;
  if (Result[1] = '.') and
    ((Result[2] = '\') or (Result[2] = '/')) then
      Delete(Result, 1, 2);
  len := Length(Result);
  if (Result[len] = '\') or (Result[len] = '/') then
    Delete(Result, len, 1);
end;

function AddForwardSlash(const S: string): string;
var
  c: Char;
begin
  Result := S;
  if (Result = '') then Exit;
  c := Result[Length(Result)];
  if c <> '/' then  Result := Result + '/';
end;

procedure AddSubItem(var topItem: TMenuTopItem; const cap, url: string);
var
  len: integer;
begin
  len := length(topItem.subItems);
  SetLength(topItem.subItems, len +1);
  topItem.subItems[len].shortFileName := cap;
  if pos('.htm', url) = 0 then
    topItem.subItems[len].url := url + '/_Body.htm' else
    topItem.subItems[len].url := url;
end;

function FindExclude(firstNode: TDomNode; const exclude: string): Boolean;
var
  textNode: TDomNode;
begin
  Result := true;
  while Assigned(firstNode) do
  begin
    textNode := firstNode.FirstChild;
    if assigned(textNode) and (textNode.Value = exclude) then Exit;
    firstNode := firstNode.NextSibling;
  end;
  Result := false;
end;

procedure GetSubItems(const foldername: string; var topItem: TMenuTopItem);
var
  res: integer;
  sr: TSearchRec;
  cap, url: string;
  isFolder: Boolean;
  excludes: TDomDocument;
  firstExclude: TDomNode;
begin
  excludes := TDomDocument.Create;
  try
    firstExclude := nil;
    if FileExists(foldername+'\excludes.xml') then
    begin
      excludes.LoadFromFile(foldername+'\excludes.xml');
      firstExclude := excludes.FindNode('exclude', true);
    end;

    res := FindFirst(foldername+'\*.*', faAnyFile, sr);
    while res = 0 do
    begin
      if sr.Name[1] <> '.' then
      begin
        isFolder := (sr.Attr and faDirectory <> 0);
        if isFolder or (pos('.htm', sr.Name) > 0) then
        begin
          if isFolder then
            cap := sr.Name else
            cap := ChangeFileExt(sr.Name, '');
          url := AddForwardSlash(topItem.url) + sr.Name;
          if not FindExclude(firstExclude, sr.Name) then
            AddSubItem(topItem, cap, url);
        end;
      end;
      res := FindNext(sr);
    end;
  finally
    excludes.Free;
  end;
end;

function AddTopItem(const sourcePath: string; var topItems: TMenuTopItems;
  const caption: string; url: string; isFolder: Boolean): integer;
begin
  Result := length(topItems);
  SetLength(topItems, Result +1);
  if url = '' then url := './' + caption;
  topItems[Result].url := url;
  if isFolder and (Pos('.htm', url) = 0) then
  begin
    topItems[Result].shortFileName := caption;
    GetSubItems(AddForwardSlash(sourcePath)+ url, topItems[Result]);
  end else
    topItems[Result].shortFileName := ChangeFileExt(caption, '');
end;

procedure GetFilenamesInFolder(const foldername:string; sl: TStringList);
var
  res: integer;
  sr: TSearchRec;
begin
  res := FindFirst(foldername+'\*.*', faAnyFile, sr);
  while res = 0 do
  begin
    if sr.Name[1] <> '.' then
      sl.Add(foldername + '\'+sr.Name);
    res := FindNext(sr);
  end;
end;

procedure GetTopItems(node: TDomNode;
  const sourceFolder: string; var items: TMenuTopItems);
var
  cap, url: string;
  isFolder: Boolean;
begin
  node := node.FirstChild;
  while Assigned(node) do
  begin
    if (node.Name <> 'include') then
    begin
      node := node.NextSibling;
      Continue;
    end;
    cap := node.Attributes['caption'];
    url := node.Attributes['url'];
    if (Lowercase(cap) = 'index') or (cap = '_Body') then
    begin
      node := node.NextSibling;
      Continue;
    end;

    if url <> '' then
    begin
      url := CleanFoldername(url);
      if (pos('.htm', url) = 0) and not SysUtils.DirectoryExists(
        IncludeTrailingBackslash(sourceFolder) + url) then
          url := '' else
          url := './' + url;
      isFolder := url <> '';
    end else
      isFolder := SysUtils.DirectoryExists(
        IncludeTrailingBackslash(sourceFolder) + cap);
    AddTopItem(sourceFolder, items, cap, url, isFolder);

    node := node.NextSibling;
  end;
end;

function ReplaceMacros(const text: string): string; overload;
begin
    Result := StringReplace(text, '##BuildDate##', BuildDateString,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result,
      '##VersionString##', VersionString, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result,
      '##HtmlCompileDate##', FormatDateTime('d mmm yyyy', date),
        [rfReplaceAll, rfIgnoreCase]);
end;

function TMainForm.StartTransforming: Boolean;
const
  cUpdateInterval = 500;
var
  i,j: Integer;
  found: boolean;
  S, dstFolder: string;
  CompileTime: Cardinal;
  NextUpdate: Cardinal;

  Includes: TDomDocument;
  node: TDomNode;
  imageInFolder: TStringList;
  indexes: array of Boolean;
begin
  LogElemCnt := 0;
  LogElemsCnt := 0;

  Result := False;

  BuildDateString := FormatDateTime('d mmmm yyyy', dtpProjectBuildDate.Date);
  VersionString := EdtVersionString.Text;
  if ProjectDir = '' then Exit;
  ProjectDir := ExpandFileName(ProjectDir);

  if not FileExists(SourceDir + '_Body.htm') then
    with TStringList.Create do
    try
      text := BodyFileContent;
      SaveToFile(SourceDir + '_Body.htm');
    finally
      Free;
    end;

  Log.Clear;
  Log.Color := $E7FFE7;
  Progress.Position := 0;
  Enabled := False;
  LogAdd('Deleting Docs folder');
  Application.ProcessMessages;
  if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(ProjectDir + 'Docs') then
    DeleteDir(Self.Handle, ProjectDir + 'Docs');
  CompiledDir := (ProjectDir + 'Docs\');

  Log.Clear;
  Application.ProcessMessages;
  CompileTime := GetTickCount;

  Project := TProject.Create(nil, ProjectDir + 'Source');
  try
    Project.ShortFileName         := EdtProjectTitle.Text;
    Project.DestinationFolder   := ProjectDir + 'Docs';
    Project.SourceFolder        := ProjectDir + 'Source';
    Project.ImageFolder         := ProjectDir + 'Images';
    Project.ScriptFolder        := ProjectDir + 'Script';

    //get includes
    if fileExists(SourceDir + '_head_includes.xml')  then
    begin
      Project.HeadIncludes.LoadFromFile(SourceDir + '_head_includes.xml');
      Project.HeadInclude := Project.HeadIncludes.FindNode('head_include', false);
    end;

    if fileExists(SourceDir + '_body_includes.xml')  then
    begin
      Includes := TDomDocument.Create;
      try
        Includes.LoadFromFile(SourceDir + '_body_includes.xml');
        node := Includes.FindNode('body_include', true);
        if Assigned(node) then
          Project.BodyIncludes := ReplaceMacros(node.GetContent);
      finally
        Includes.Free;
      end;
    end;

    //add top level menu items listed in includes.xml
    if fileExists(SourceDir + '_top_items_includes.xml')  then
    begin
      Includes := TDomDocument.Create;
      try
        Includes.LoadFromFile(SourceDir + '_top_items_includes.xml');
        node := Includes.FindNode('includes', false);
        if Assigned(node) then
          GetTopItems(node, Project.SourceFolder, Project.MenuTopItems);
      finally
        Includes.Free;
      end;
    end;

    s := GetProjectIniFilename(EdtProjectDirectory.Text, CmbProjectName.Text);
    if not FileExists(s) then
    begin
      LogAdd('Oops, couldn''t find: ' + s);
      Log.Color := $E7E7FF;
      Exit;
    end;

    LogAdd(CRLF + 'Transforming - ' + CmbProjectName.Text + CRLF);
    LogNL;
    LogAdd(RCStrReadingFiles + ' ' + CDots);

    Project.CheckForBrokenLinks := CbxReportBrokenLinks.Checked;
    Project.CheckBrokenImages := cbxReportBrokenImages.Checked;

    Project.Read;
    Progress.Position := 2;
    LogAdd(CDots + ' done' + CRLF);
    LogAdd(RCStrProjectContains + ':' + CRLF);
    LogAdd(#9'Units       '#9 + IntToStr(Project.Units.Count)        + CRLF);
    LogAdd(#9'Classes     '#9 + IntToStr(Length(Project.Classes))    + CRLF);
    LogAdd(#9'Interfaces  '#9 + IntToStr(Length(Project.Interfaces)) + CRLF);
    LogAdd(#9'Topics      '#9 + IntToStr(Length(Project.Topics))     + CRLF);
    LogAdd(#9'HTML Files  '#9 + IntToStr(Project.Files.Count)        + CRLF);
    LogNL;
    LogAdd(RCStrBuildingClassHierarchy + ' ' + CDots);
    Project.BuildHierarchy;
    LogAdd(CDots + ' done' + CRLF);

    if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(CompiledDir) then
    begin
      LogAdd(RCStrDeletingDocFolder + ' ' + CDots);
      DeleteDirectoryTree(CompiledDir);
      LogAdd(CDots + ' done' + CRLF);
    end;

    LogAdd(RCStrTransformingFiles + ':');

    Progress.Position := 4;
    NextUpdate := 0;

    for i := 0 to Project.Files.Count - 1 do
    begin
      S := TElement(Project.Files.Objects[i]).ShortFileName;

      if GetTickCount > NextUpdate then
      begin
        LogReplace(Format(RCStrTransformingFile + ': (%d/%d) %s',
          [i + 1, Project.Files.Count, S]));
        Progress.Position := 4 + 83 * i div Project.Files.Count;
        Application.ProcessMessages;
        NextUpdate := GetTickCount + cUpdateInterval;
      end;

      try
        TElement(Project.Files.Objects[i]).Transform;
      except
        on e: Exception do
        begin
          LogAdd(CRLF + CRLF + e.Message);
          LogAdd(CRLF + RCStrTransformingHalted);
          Progress.Position := 0;
          LogNL;
          Log.Color := $E7E7FF;
          Exit;
        end;
      end;
    end;
    LogReplace(RCStrTransformingFiles + ' ' + CDots + CDots + ' done' + CRLF);
    Progress.Position := 100;

    //parse TOP LEVEL files for placeholder strings and replace
    dstFolder := IncludeTrailingBackslash(project.DestinationFolder);
    with TStringList.Create do
    try
      if FileExists(dstFolder + '_Body.htm') then
      begin
        LoadFromFile(dstFolder + '_Body.htm');
        text := StringReplace(text,
          '##BuildDate##', BuildDateString, [rfReplaceAll, rfIgnoreCase]);
        text := StringReplace(text, '##VersionString##',
          VersionString, [rfReplaceAll, rfIgnoreCase]);
        SaveToFile(dstFolder + '_Body.htm');
      end;
      for i := 0 to HIgh(Project.MenuTopItems) do
        with TMenuTopItem(Project.MenuTopItems[i]) do
        begin
          if not FileExists(dstFolder + url) then Continue;
          LoadFromFile(dstFolder + url);
          text := StringReplace(text,
            '##BuildDate##', BuildDateString, [rfReplaceAll, rfIgnoreCase]);
          text := StringReplace(text, '##VersionString##',
            VersionString, [rfReplaceAll, rfIgnoreCase]);
          SaveToFile(dstFolder + url);
        end;
    finally
      Free;
    end;


    WriteRedirectFile(ProjectDir, CmbProjectName.Text);

    LogNL;
    LogAdd('Project transformed in ');

    CompileTime := GetTickCount - CompileTime;
    LogAdd(Format('%d minutes, %d seconds' + CRLF,
      [(CompileTime div 1000) div 60, (CompileTime div 1000) mod 60]));
    Progress.Position := 0;

    if Project.BrokenLinks.Count > 0 then
    begin
      LogNL;
      LogAdd(RCStrBrokenLinksFound + ':' + CRLF);
      for i := 0 to Project.BrokenLinks.Count -1 do
        LogAdd(Project.BrokenLinks[i]);
      LogNL;
      Log.Color := $E7FFFF;
    end;

    if Project.CheckBrokenImages then
    begin
      imageInFolder := TStringList.create;
      try
        imageInFolder.CaseSensitive := true;
        GetFilenamesInFolder( Project.ImageFolder, imageInFolder);
        imageInFolder.Sort;
        SetLength(indexes, imageInFolder.Count);
        //imageInFolder.SaveToFile('c:\temp\imgs.txt');

        found := true;
        LogNL;
        LogAdd('Missing Images:'+CRLF);
        for i := 0 to Project.imageNames.Count -1 do
        begin
          j := imageInFolder.IndexOf(Project.imageNames[i]);
          if j < 0 then
          begin
            found := false;
            LogAdd('  ' + Project.imageNames[i] + CRLF);
          end else
            indexes[j] := True;
        end;
        if found then
          LogAdd('  None.'+CRLF) else
          LogAdd('  Note: Case sensitivity is important.'+CRLF);

        found := false;
        LogNL;
        LogAdd('Unused Images:'+CRLF);
        for i := 0 to High(indexes) do
          if not indexes[i] then
          begin
            found := true;
            LogAdd('  ' + ExtractFileName(imageInFolder[i]) + CRLF);
          end;
        if not found then
          LogAdd('  None.'+CRLF);

      finally
        imageInFolder.Free;
      end;
    end;
    Result := True;

    if CbxOpenAfterProcess.Checked then
      BtnOpenClick(nil);

  finally
    Enabled := True;
    Project.Free;
  end;
end;

procedure TMainForm.BtnTransformClick(Sender: TObject);
begin
  StartTransforming;
end;

procedure TMainForm.BtnOpenClick(Sender: TObject);
var
  indexFile: string;
begin
  indexFile := IncludeTrailingBackslash(
    EdtProjectDirectory.Text) + 'Docs\_Body.htm';
  if not FileExists(indexFile) then
    beep else
    ShellExecute( Self.Handle, 'open', PChar(indexFile), '', '', SW_SHOW);
end;

procedure TMainForm.BtnParseMissingClick(Sender: TObject);
var
  I, K: Integer;
  DestUnitFolder, Fn: TFileName;
  S: string;
  PasFiles: TStringList;
const
  NEW_MAIN_UNIT = 5;
  NEW_ADDITIONAL_UNIT = 6;
begin
  if SourceDir = '' then Exit;
  OpnDlgPAS.InitialDir :=
    IncludeTrailingBackslash(EdtProjectDirectory.Text) + 'source';
  if not OpnDlgPAS.Execute then exit;

  DestUnitFolder := SourceDir + 'Units\';

  Log.Clear;
  Log.Color := clWhite;

  if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(DestUnitFolder) then
  begin
    LogAdd(RCStrErrorDestinationFolderDoesNotExist + ' ' + CRLF);
    Log.Color := $E7FFE7;
    Exit;
  end;
  LogAdd(RCStrStartingPas2Html + ' ' + CDots + CRLF);
  LogNL;
  Application.ProcessMessages;

  PasFiles := TStringList.Create;
  try
    PasFiles.Duplicates := dupIgnore;
    PasFiles.Assign(OpnDlgPAS.Files);
    for I := 0 to PasFiles.Count - 1 do
    begin
      Fn := ChangeFileExt(ExtractFileName(PasFiles[I]), '');
      if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(DestUnitFolder + fn) then
      begin
        S := Format('The file %s has already been imported into the help ' +
          'source.' + #10 + 'Do you want to replace the existing contents ' +
          'with this new file?', [fn]);
        if MessageBox(Handle, PChar(S), PChar(Caption),
          MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) <> IDYES then Continue;
        DeleteFolder(DestUnitFolder + fn);
      end;

      K := BuildNewUnit(PasFiles[I],
        DestUnitFolder + fn + '\', ProjectDir);
      LogAdd('  added: ' + PasFiles[I] + CRLF);
      if K >= 0 then
        LogAdd('  (parse error at line ' + IntToStr(K + 1) +')' + CRLF);
      Application.ProcessMessages;

      //prepare to update the menu list ...
      PasFiles[I] := fn;
      PasFiles.Objects[I] := Pointer(1); //flag as updating
    end;

    //remove skipped over files ...
    for I := PasFiles.Count - 1 downto 0 do
      if PasFiles.Objects[I] = nil then
        PasFiles.Delete(I);
    PasFiles.Sorted := True; //???
    LogNL;
    LogAdd(CDots + ' done' + CRLF);
    Log.Color := $E7FFE7;
  finally
    PasFiles.Free;
  end;
end;

procedure TMainForm.EdtProjectTitleChange(Sender: TObject);
begin
  BtnSaveProjectInfo.Enabled := True;
end;

procedure TMainForm.EdtVersionStringChange(Sender: TObject);
begin
  BtnSaveProjectInfo.Enabled := True;
  if EdtVersionString.Focused then
    dtpProjectBuildDate.Date := Date;
end;

end.
