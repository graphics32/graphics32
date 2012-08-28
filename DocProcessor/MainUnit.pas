unit MainUnit;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, FileCtrl, ComCtrls, Contnrs, IniFiles, ExtCtrls, ShellApi,
  Utils, SimpleDOM, DocStructure, Pas2Html;

type
  TMainForm = class(TForm)
    BtnClose: TButton;
    BtnCompile: TButton;
    BtnOpen: TButton;
    BtnParseMissing: TButton;
    BtnProcess: TButton;
    BtnSaveProjectInfo: TButton;
    BtnTransform: TButton;
    CbxBrokenLinks: TCheckBox;
    CbxIncludeAlphabetClasses: TCheckBox;
    CbxOpenAfterProcess: TCheckBox;
    EdtCHMCompiler: TEdit;
    EdtProjectDirectory: TEdit;
    EdtProjectTitle: TEdit;
    LblCompiler: TLabel;
    LblProgress: TLabel;
    LblProjectDirectory: TLabel;
    LblProjectTitle: TLabel;
    Log: TMemo;
    PnlCompiler: TPanel;
    PnlCompilerHead: TPanel;
    PnlControl: TPanel;
    PnlLog: TPanel;
    PnlMisc: TPanel;
    PnlMiscHead: TPanel;
    PnlProgress: TPanel;
    PnlProjectInfo: TPanel;
    PnlProjectInfoHead: TPanel;
    PnlTransComp: TPanel;
    PnlTransCompHead: TPanel;
    Progress: TProgressBar;
    LblVersionString: TLabel;
    LblProjectFileName: TLabel;
    CmbProjectName: TComboBox;
    EdtVersionString: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnCompileClick(Sender: TObject);
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
  public
    ProjectDir: TFileName;
    SourceDir: TFileName;
    CompiledDir: TFileName;
    StyleFile: TFileName;
    procedure LoadProject(const ProjName: TFileName);
    procedure SaveProject;
    function StartTransforming: Boolean;
    procedure StartCompile;
    procedure WriteProject(const FileName: TFileName);
  end;

var
  ExePath: string;
  DelphiSourceFolder: string;
  NoGUI: Boolean;
  MainForm: TMainForm;
  Project: TProject;

implementation

{$R *.DFM}

function DeleteDirectoryTree(Dir: string): Boolean;
var
  CharCount: Integer;
  FileOpt : TSHFileOpStruct;
begin
  Result := False;
  CharCount := Length(Dir);
  if (CharCount > 0) and (Dir[CharCount] = '\') then Dir[CharCount] := #0;
  if not DirectoryExists(Dir) then Exit;
  fillChar(FileOpt, SizeOf(FileOpt), 0);
  FileOpt.Wnd := 0;
  FileOpt.wFunc := FO_DELETE;
  FileOpt.pFrom := PChar(Dir);
  FileOpt.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;
  Result := SHFileOperation(FileOpt) = 0;
end;

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
      SelText := #13#10;
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

procedure TMainForm.EdtCHMCompilerChange(Sender: TObject);
begin
  BtnCompile.Enabled := FileExists(EdtCHMCompiler.Text);
  EdtProjectTitleChange(Sender);
end;

procedure TMainForm.EdtProjectDirectoryChange(Sender: TObject);
begin
  ProjectDir := EdtProjectDirectory.Text;
  if DirectoryExists(ProjectDir) then
  begin
    ProjectDir := IncludeTrailingBackslash(ProjectDir);
    if DirectoryExists(ProjectDir + 'Source') then
    begin
      SourceDir := ProjectDir + 'Source\';
      BtnProcess.Enabled := True;
      BtnSaveProjectInfo.Enabled := True;
      Exit;
    end;
  end;
  BtnProcess.Enabled := False;
  ProjectDir := '';
  SourceDir := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, ActiveItem: Integer;
  sValue: string;
begin
  EdtProjectDirectoryChange(Self);

  ExePath := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));

  with TIniFile.Create(ExePath + 'properties.ini') do
  try
    i := 1;
    while ValueExists('Projects', 'ProjectName' + IntToStr(i)) do
    begin
      sValue := ReadString('Projects', 'ProjectName' + IntToStr(i), '');
      if sValue <> '' then CmbProjectName.Items.Add(sValue);
      Inc(i);
    end;
    DelphiSourceFolder := ReadString('Settings', 'DelphiSourceFolder', '');
    ActiveItem := ReadInteger('Settings', 'ActiveProject', 0);
  finally
    Free;
  end;

  NoGUI := False;

  if (ParamCount > 0) and FileExists(ExePath + paramstr(2)) then
    LoadProject(paramstr(2))
  else if (ParamCount > 0) and FileExists(ExePath + paramstr(2) + '.ini') then
    LoadProject(paramstr(2)+ '.ini')
  else if (CmbProjectName.Items.Count > 0) then
  begin
    if (ActiveItem < CmbProjectName.Items.Count) and
      FileExists(ExePath + CmbProjectName.Items[ActiveItem] + '.ini') then
        LoadProject(CmbProjectName.Items[ActiveItem] + '.ini')
    else if FileExists(ExePath + CmbProjectName.Items[0] + '.ini') then
        LoadProject(CmbProjectName.Items[0] + '.ini')
    else LoadProject('properties.ini');
  end else
    LoadProject('properties.ini');
  ActiveControl := BtnProcess;

  BtnCompile.Enabled := FileExists(EdtCHMCompiler.Text);

  if ParamCount > 2 then
  begin
    if FindCmdLineSwitch('nogui') then
    begin
      NoGUI := True;
      Self.Hide;
    end;

    if FindCmdLineSwitch('transform') then
      BtnTransform.Click
    else if FindCmdLineSwitch('compile') then
      BtnCompile.Click
    else if FindCmdLineSwitch('process') then
      BtnProcess.Click;

    Application.Terminate;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Index: Integer;
begin
  CanClose := True;
  if not NoGUI and BtnSaveProjectInfo.Enabled and
    (MessageBox(self.handle, 'Save Project Information', PChar(caption),
      MB_YESNO or MB_DEFBUTTON1) = IDYES) then SaveProject;
  with TIniFile.Create(ExePath + 'properties.ini') do
  try
    WriteInteger('Settings', 'ActiveProject', CmbProjectName.ItemIndex);
    if sectionExists('Projects') then EraseSection('Projects');
    for Index := 0 to CmbProjectName.Items.Count - 1 do
      WriteString('Projects', 'ProjectName' + IntToStr(Index + 1), CmbProjectName.Items[Index]);
  finally
    Free;
  end;
end;

procedure TMainForm.SaveProject;
var
  Index: Integer;
  Ini: TIniFile;
begin
  if CmbProjectName.Items.IndexOf(CmbProjectName.Text) < 0 then
    CmbProjectName.Items.Add(CmbProjectName.Text);

  Ini := TIniFile.Create(ExePath + CmbProjectName.Text + '.ini');
  try
    Ini.WriteString('Settings', 'ProjectName', CmbProjectName.Text);
    Ini.WriteString('Settings', 'DelphiSourceFolder', DelphiSourceFolder);
    for Index := 0 to Self.ComponentCount - 1 do
      if Self.Components[Index].InheritsFrom(TCustomEdit) and
         not Self.Components[Index].InheritsFrom(TMemo) then
        Ini.WriteString('Settings', Copy(Self.Components[Index].Name, 3, MAXINT), TEdit(Self.Components[Index]).Text)
      else if Self.Components[Index].InheritsFrom(TCheckBox) then
        Ini.WriteBool('Settings', Copy(Self.Components[Index].Name, 3, MAXINT), TCheckBox(Self.Components[Index]).Checked);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.LoadProject(const ProjName: TFileName);
var
  Ini: TIniFile;
  Index: Integer;
  SValue: string;
begin
  Ini := TIniFile.Create(ExePath + ProjName);
  try
    if ProjName = 'properties.ini' then
      SValue := Ini.ReadString('Settings', 'ProjectName', 'MyProjectName') else
      SValue := ChangeFileExt(ProjName, '');
    Index := CmbProjectName.Items.IndexOf(SValue);
    if Index >= 0 then CmbProjectName.ItemIndex := Index
    else CmbProjectName.Text := SValue;

    for Index := 0 to Self.ComponentCount - 1 do
      if Self.Components[Index].InheritsFrom(TCustomEdit) and
         not Self.Components[Index].InheritsFrom(TMemo) then
      begin
        SValue := Ini.ReadString('Settings', Copy(Self.Components[Index].Name, 3, MAXINT), '');
        if SValue <> '' then TEdit(Self.Components[Index]).Text := SValue;
      end
      else if Self.Components[Index].InheritsFrom(TCheckBox) then
        TCheckBox(Self.Components[Index]).Checked :=
          Ini.ReadBool('Settings', Copy(Self.Components[Index].Name, 3, MAXINT), False);
  finally
    Ini.Free;
  end;
  Log.Lines.Clear;
  BtnSaveProjectInfo.Enabled := False;
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
  LoadProject(CmbProjectName.Text + '.ini');
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
    VK_F8: BtnCompileClick(nil);
    VK_F9: BtnProcessClick(nil);
  end;
end;

procedure TMainForm.BtnProcessClick(Sender: TObject);
begin
  if not StartTransforming then Exit;
  LogAdd(#13#10);
  StartCompile;
end;

procedure TMainForm.StartCompile;
begin
  if FileExists(EdtCHMCompiler.Text) then
  begin
    LogAdd('Starting HTML Help compiler...'#13#10);
    RunCommandInMemo(EdtCHMCompiler.Text + ' "' + ProjectDir + CmbProjectName.Text + '.hhp"', Log);
    LogAdd('Done.'#13#10);
    LogNL;
    if CbxOpenAfterProcess.Checked then
      BtnOpenClick(nil);
    Exit;
  end
  else
    LogAdd('HTML Help compiler not found! (' + EdtCHMCompiler.Text + ')'#13#10);
  LogNL;
end;

function TMainForm.StartTransforming: Boolean;
const
  cUpdateInterval = 500;
var
  I: Integer;
  S: string;
  CompileTime: Cardinal;
  NextUpdate: Cardinal;
begin
  Result := False;
  VersionString := EdtVersionString.Text;
  if ProjectDir = '' then Exit;
  Log.Clear;
  Log.Color := clWhite;
  Progress.Position := 0;
  Enabled := False;
  Application.ProcessMessages;
  if DirectoryExists(ProjectDir + 'Docs') then RemoveDir(ProjectDir + 'Docs');
  CompiledDir := (ProjectDir + 'Docs\');
  StyleFile := ProjectDir + 'Styles\Default.css';

  Log.Color := $E7FFE7;
  Application.ProcessMessages;
  CompileTime := GetTickCount;

  DocStructure.IncludeAlphabetClasses := CbxIncludeAlphabetClasses.Checked;
  DocStructure.CheckForBrokenLinks := CbxBrokenLinks.Checked;

  Project := TProject.Create(nil, ProjectDir + 'Source');
  try
    Project.DisplayName := EdtProjectTitle.Text;
    Project.DestinationFolder := ProjectDir + 'Docs';
    Project.ImageFolder := ProjectDir + 'Images';
    Project.ScriptFolder := ProjectDir + 'Script';
    Project.StylesFolder := ProjectDir + 'Styles';
    Project.HeadSectionTemplate := ProjectDir + 'HeadSection.tmpl';
    Project.BodySectionTemplate := ProjectDir + 'BodySection.tmpl';
    LogAdd(#13#10+ 'Transforming - ' + CmbProjectName.Text+ #13#10);
    LogNL;

    LogAdd('Reading files ...');
    Project.Read;
    Progress.Position := 2;
    LogAdd('... done'#13#10);
    LogAdd('Project Contains:'#13#10);
    LogAdd(#9'Units       '#9 + IntToStr(Project.Units.Count)        + #13#10);
    LogAdd(#9'Classes     '#9 + IntToStr(Length(Project.Classes))    + #13#10);
    LogAdd(#9'Interfaces  '#9 + IntToStr(Length(Project.Interfaces)) + #13#10);
    LogAdd(#9'Topics      '#9 + IntToStr(Length(Project.Topics))     + #13#10);
    LogAdd(#9'HTML Files  '#9 + IntToStr(Project.Files.Count)        + #13#10);
    LogNL;
    LogAdd('Building Class Hierarchy ...');
    Project.BuildHierarchy;
    LogAdd('... done'#13#10);

    if DirectoryExists(CompiledDir) then
    begin
      LogAdd('Deleting Doc folder ...');
      DeleteDirectoryTree(CompiledDir);
      LogAdd('... done'#13#10);
    end;

    LogAdd('Transforming Files:');

    Progress.Position := 4;

    NextUpdate := 0;

    for I := 0 to Project.Files.Count - 1 do
    begin
      S := TElement(Project.Files.Objects[I]).DisplayName;

      if GetTickCount > NextUpdate then
      begin
        LogReplace(Format('Transforming File: (%d/%d) %s',
          [I + 1, Project.Files.Count, S]));
        Progress.Position := 4 + 83 * I div Project.Files.Count;
        Application.ProcessMessages;
        NextUpdate := GetTickCount + cUpdateInterval;
      end;

      try
        TElement(Project.Files.Objects[I]).Transform;
      except
        on e: Exception do
        begin
          LogAdd(#13#10#13#10 + e.Message);
          LogAdd(#13#10 + 'Transforming halted.');
          Progress.Position := 0;
          LogNL;
          Log.Color := $E7E7FF;
          Exit;
        end;
      end;
    end;
    LogReplace('Transforming Files ...... done'#13#10);
    LogAdd('Building TOC ...');
    Project.BuildToc(ProjectDir + CmbProjectName.Text + '.hhc');
    LogAdd('... done'#13#10);
    Progress.Position := 95;
    LogAdd('Building Index ...');
    Project.BuildIndex(ProjectDir + CmbProjectName.Text + '.hhk');
    LogAdd('... done'#13#10);
    Progress.Position := 100;

    LogAdd('Writing Project ...');
    WriteProject(ProjectDir + CmbProjectName.Text + '.hhp');
    LogAdd('... done'#13#10);
    LogNL;
    LogAdd('Project transformed in ');

    CompileTime := GetTickCount - CompileTime;
    LogAdd(Format('%d minutes, %d seconds'#13#10,
      [(CompileTime div 1000) div 60, (CompileTime div 1000) mod 60]));
    Progress.Position := 0;

    if Project.BrokenLinks.Count > 0 then
    begin
      LogNL;
      LogAdd('Broken links found:'#13#10);
      for I := 0 to Project.BrokenLinks.Count -1 do
        LogAdd(Project.BrokenLinks[i]);
      LogNL;
      Log.Color := $E7FFFF;
    end;

    Result := True;
  finally
    Enabled := True;
    Project.Free;
  end;
end;

procedure TMainForm.WriteProject(const FileName: TFileName);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('[OPTIONS]');
    Lines.Add('Compatibility=1.1 or later');
    Lines.Add('Compiled file=' + CmbProjectName.Text + '.chm');
    Lines.Add('Contents file=' + CmbProjectName.Text + '.hhc');
    Lines.Add('Default Window=Main Window');
    Lines.Add('Default topic=Docs\Overview\_Body.htm');
    Lines.Add('Display compile progress=No');
    Lines.Add('Full-text search=Yes');
    Lines.Add('Index file=' + CmbProjectName.Text + '.hhk');
    Lines.Add('Language=0x409 English (United States)');
    Lines.Add('Title=' + EdtProjectTitle.Text);
    Lines.Add('');
    Lines.Add('[WINDOWS]');
    Lines.Add(Format('Main Window="%s","%s","%s","Docs\Overview\_Body.htm","Docs\Overview\_Body.htm",,,,,0x63520,600,0x10384e,[0,0,900,680],0xb0000,,,1,,,0',
      [EdtProjectTitle.Text, CmbProjectName.Text + '.hhc', CmbProjectName.Text + '.hhk']));
    Lines.Add('');
    Lines.Add('[INFOTYPES]');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TMainForm.BtnTransformClick(Sender: TObject);
begin
  StartTransforming;
end;

procedure TMainForm.BtnCompileClick(Sender: TObject);
begin
  Log.Clear;
  StartCompile;
end;

procedure TMainForm.BtnOpenClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, 'open',
    PChar(IncludeTrailingBackslash(EdtProjectDirectory.Text) + CmbProjectName.Text + '.chm'), '', '', SW_SHOW);
end;

procedure TMainForm.BtnParseMissingClick(Sender: TObject);
var
  i, j, k, m: Integer;
  DestUnitFolder, fn: TFileName;
  s: string;
  PasFiles, MenuData: TStringList;
begin
  if SourceDir = '' then Exit;
  DelphiSourceFolder := GetDelphiSourceFolder(DelphiSourceFolder);
  if DelphiSourceFolder = '' then Exit;
  DestUnitFolder := SourceDir + 'Units\';

  Log.Clear;
  Log.Color := clWhite;

  if not DirectoryExists(DestUnitFolder) then
  begin
    LogAdd('Error: destination folder does not exist. '#13#10);
    Log.Color := $E7FFE7;
    Exit;
  end;
  LogAdd('Starting Pas2Html ...'#13#10);
  LogNL;
  Application.ProcessMessages;
  j := 0;
  PasFiles := GetFileList(DelphiSourceFolder, '*.pas');
  try
    PasFiles.Sort;
    for i := 0 to PasFiles.Count -1 do
    begin
      fn := ChangeFileExt(ExtractFileName(PasFiles[i]),'');
      if DirectoryExists(DestUnitFolder + fn) then
      begin
        s := 'The file '+ fn + ' has already been imported into the help source.'+#10+
          'Do you want to replace the existing contents with this new file?';
        if MessageBox(self.handle, PChar(s),
          PChar(caption), MB_YESNO or MB_DEFBUTTON2) <> IDYES then continue;
        DeleteFolder(DestUnitFolder + fn);
      end;
      Inc(j);
      k := BuildNewUnit(AnsiString(PasFiles[i]),
        AnsiString(DestUnitFolder + fn + '\'), ProjectDir);
      LogAdd('  added: ' + PasFiles[i] + #13#10);
      if k >= 0 then
        LogAdd('  (parse error at line ' + IntToStr(k+1) +')'#13#10);
      Application.ProcessMessages;
      //getting ready to update the menu list (ie saves redoing stuff) ...
      PasFiles[i] := fn;
      PasFiles.Objects[i] := pointer(1); //flags a new unit
    end;
    //now update the help file's dropdown menu list of units
    if (j > 0) and FileExists(ProjectDir + 'Scripts\menu_data.js') then
    begin
      MenuData := TStringList.Create;
      try
        MenuData.LoadFromFile(ProjectDir + 'Scripts\menu_data.js');
        i := MenuData.IndexOf('td_6 = "Additional Units"');
        if i > 0 then
        begin
          while MenuData[i] <> '' do Inc(i);
          //now append to the list of additional units ...
          PasFiles.Sort;
          m := j;
          for k := PasFiles.Count -1 downto 0 do
            if Assigned(PasFiles.Objects[k]) then
            begin
              MenuData.Insert(i, Format('url_6_%d = "Units/%s/_Body.htm"',[m, PasFiles[k]]));
              MenuData.Insert(i, Format('td_6_%d = "%s.pas"',[m, PasFiles[k]]));
              dec(m);
            end;
          {$IFDEF DEBUGGING}
          //MenuData.SaveToFile('c:\temp\menu_data.txt');
          {$ELSE}
          MenuData.SaveToFile(ProjectDir + 'Scripts\menu_data.js');
          {$ENDIF}
          LogNL;
          LogAdd('  ''Additional Units'' menu updated.' +#13#10);
        end;
      finally
        MenuData.Free;
      end;
    end;
  finally
    PasFiles.Free;
  end;
  LogAdd(IntToStr(j) +' units added.'#13#10);
  LogNL;
  LogAdd('... done'#13#10);
  Log.Color := $E7FFE7;
end;

procedure TMainForm.EdtProjectTitleChange(Sender: TObject);
begin
  BtnSaveProjectInfo.Enabled := True;
end;

end.
