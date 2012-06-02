unit MainUnit;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, FileCtrl, ComCtrls, Utils, SimpleDOM,
  Contnrs, DocStructure, IniFiles, ExtCtrls, ShellApi, Pas2Html;

type
  TMainForm = class(TForm)
    bClose: TButton;
    bCompile: TButton;
    bOpen: TButton;
    bParseMissing: TButton;
    bProcess: TButton;
    bSaveProjectInfo: TButton;
    bTransform: TButton;
    cbBrokenLinks: TCheckBox;
    cbIncludeAlphabetClasses: TCheckBox;
    cbOpenAfterProcess: TCheckBox;
    edCHMCompiler: TEdit;
    edProjectDirectory: TEdit;
    edProjectTitle: TEdit;
    lblCompiler: TLabel;
    lblProgress: TLabel;
    lblProjectDirectory: TLabel;
    lblProjectTitle: TLabel;
    Log: TMemo;
    pnlCompiler: TPanel;
    pnlCompilerHead: TPanel;
    pnlControl: TPanel;
    pnlLog: TPanel;
    pnlMisc: TPanel;
    pnlMiscHead: TPanel;
    pnlProgress: TPanel;
    pnlProjectInfo: TPanel;
    pnlProjectInfoHead: TPanel;
    pnlTransComp: TPanel;
    pnlTransCompHead: TPanel;
    Progress: TProgressBar;
    lblVersionString: TLabel;
    lblProjectFileName: TLabel;
    cbProjectName: TComboBox;
    edVersionString: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bCloseClick(Sender: TObject);
    procedure bCompileClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bParseMissingClick(Sender: TObject);
    procedure bProcessClick(Sender: TObject);
    procedure bSaveProjectInfoClick(Sender: TObject);
    procedure bTransformClick(Sender: TObject);
    procedure cbProjectNameChange(Sender: TObject);
    procedure cbProjectNameClick(Sender: TObject);
    procedure edProjectDirectoryChange(Sender: TObject);
    procedure edProjectTitleChange(Sender: TObject);
    procedure edCHMCompilerChange(Sender: TObject);
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
  len: Integer;
  shfo : TSHFileOpStruct;
begin
  Result := False;
  len := Length(Dir);
  if (len > 0) and (Dir[len] = '\') then Dir[len] := #0;
  if not DirectoryExists(Dir) then Exit;
  fillChar(shfo, sizeof(shfo), 0);
  shfo.Wnd := 0;
  shfo.wFunc := FO_DELETE;
  shfo.pFrom := PChar(Dir);
  shfo.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;
  Result := SHFileOperation(shfo) = 0;
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

procedure TMainForm.edCHMCompilerChange(Sender: TObject);
begin
  bCompile.Enabled := FileExists(edCHMCompiler.Text);
  edProjectTitleChange(Sender);
end;

procedure TMainForm.edProjectDirectoryChange(Sender: TObject);
begin
  ProjectDir := edProjectDirectory.Text;
  if DirectoryExists(ProjectDir) then
  begin
    ProjectDir := IncludeTrailingBackslash(ProjectDir);
    if DirectoryExists(ProjectDir + 'Source') then
    begin
      SourceDir := ProjectDir + 'Source\';
      bProcess.Enabled := True;
      bSaveProjectInfo.Enabled := true;
      Exit;
    end;
  end;
  bProcess.Enabled := False;
  ProjectDir := '';
  SourceDir := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, ActiveItem: Integer;
  sValue: string;
begin
  edProjectDirectoryChange(Self);

  ExePath := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));

  with TIniFile.Create(ExePath + 'properties.ini') do
  try
    i := 1;
    while ValueExists('Projects', 'ProjectName' + IntToStr(i)) do
    begin
      sValue := ReadString('Projects', 'ProjectName' + IntToStr(i), '');
      if sValue <> '' then cbProjectName.Items.Add(sValue);
      Inc(i);
    end;
    DelphiSourceFolder := ReadString('Settings', 'DelphiSourceFolder', '');
    ActiveItem := ReadInteger('Settings', 'ActiveProject', 0);
  finally
    Free;
  end;

  NoGUI := False;

  if (ParamCount > 0) and fileExists(ExePath + paramstr(2)) then
    LoadProject(paramstr(2))
  else if (ParamCount > 0) and fileExists(ExePath + paramstr(2) + '.ini') then
    LoadProject(paramstr(2)+ '.ini')
  else if (cbProjectName.Items.Count > 0) then
  begin
    if (ActiveItem < cbProjectName.Items.Count) and
      fileExists(ExePath + cbProjectName.Items[ActiveItem] + '.ini') then
        LoadProject(cbProjectName.Items[ActiveItem] + '.ini')
    else if fileExists(ExePath + cbProjectName.Items[0] + '.ini') then
        LoadProject(cbProjectName.Items[0] + '.ini')
    else LoadProject('properties.ini');
  end else
    LoadProject('properties.ini');
  ActiveControl := bProcess;

  bCompile.Enabled := FileExists(edCHMCompiler.Text);

  if ParamCount > 2 then
  begin
    if FindCmdLineSwitch('nogui') then
    begin
      NoGUI := True;
      Self.Hide;
    end;

    if FindCmdLineSwitch('transform') then
      bTransform.Click
    else if FindCmdLineSwitch('compile') then
      bCompile.Click
    else if FindCmdLineSwitch('process') then
      bProcess.Click;

    Application.Terminate;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: Integer;
begin
  CanClose := true;
  if not NoGUI and bSaveProjectInfo.Enabled and
    (MessageBox(self.handle, 'Save Project Information', pchar(caption),
      MB_YESNO or MB_DEFBUTTON1) = IDYES) then SaveProject;
  with TIniFile.Create(ExePath + 'properties.ini') do
  try
    WriteInteger('Settings', 'ActiveProject', cbProjectName.ItemIndex);
    if sectionExists('Projects') then EraseSection('Projects');
    for i := 0 to cbProjectName.Items.Count - 1 do
      WriteString('Projects', 'ProjectName' + IntToStr(i + 1), cbProjectName.Items[i]);
  finally
    Free;
  end;
end;

procedure TMainForm.SaveProject;
var
  I: Integer;
  Ini: TIniFile;
begin
  if cbProjectName.Items.IndexOf(cbProjectName.Text) < 0 then
    cbProjectName.Items.Add(cbProjectName.Text);

  Ini := TIniFile.Create(ExePath + cbProjectName.Text + '.ini');
  try
    Ini.WriteString('Settings', 'ProjectName', cbProjectName.Text);
    Ini.WriteString('Settings', 'DelphiSourceFolder', DelphiSourceFolder);
    for i := 0 to Self.ComponentCount-1 do
      if Self.Components[i].InheritsFrom(TCustomEdit) and
         not Self.Components[i].InheritsFrom(TMemo) then
        Ini.WriteString('Settings', Copy(Self.Components[i].Name, 3, MAXINT), TEdit(Self.Components[i]).Text)
      else if Self.Components[i].InheritsFrom(TCheckBox) then
        Ini.WriteBool('Settings', Copy(Self.Components[i].Name, 3, MAXINT), TCheckBox(Self.Components[i]).Checked);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.LoadProject(const ProjName: TFileName);
var
  Ini: TIniFile;
  i: Integer;
  SValue: string;
begin
  Ini := TIniFile.Create(ExePath + ProjName);
  try
    if ProjName = 'properties.ini' then
      SValue := Ini.ReadString('Settings', 'ProjectName', 'MyProjectName') else
      SValue := ChangeFileExt(ProjName, '');
    i := cbProjectName.Items.IndexOf(SValue);
    if i >= 0 then cbProjectName.ItemIndex := i
    else cbProjectName.Text := SValue;

    for i := 0 to Self.ComponentCount - 1 do
      if Self.Components[i].InheritsFrom(TCustomEdit) and
         not Self.Components[i].InheritsFrom(TMemo) then
      begin
        SValue := Ini.ReadString('Settings', Copy(Self.Components[i].Name, 3, MAXINT), '');
        if SValue <> '' then TEdit(Self.Components[i]).Text := SValue;
      end
      else if Self.Components[i].InheritsFrom(TCheckBox) then
        TCheckBox(Self.Components[i]).Checked :=
          Ini.ReadBool('Settings', Copy(Self.Components[i].Name, 3, MAXINT), False);
  finally
    Ini.Free;
  end;
  log.Lines.Clear;
  bSaveProjectInfo.Enabled := False;
end;

procedure TMainForm.bSaveProjectInfoClick(Sender: TObject);
begin
  SaveProject;
  bSaveProjectInfo.Enabled := False;
end;

procedure TMainForm.cbProjectNameChange(Sender: TObject);
begin
  bSaveProjectInfo.Enabled :=
    cbProjectName.Items.IndexOf(cbProjectName.Text) < 0;
end;

procedure TMainForm.cbProjectNameClick(Sender: TObject);
begin
  LoadProject(cbProjectName.Text + '.ini');
end;

procedure TMainForm.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: Close;
    VK_F7: bTransformClick(nil);
    VK_F8: bCompileClick(nil);
    VK_F9: bProcessClick(nil);
  end;
end;

procedure TMainForm.bProcessClick(Sender: TObject);
begin
  if not StartTransforming then Exit;
  LogAdd(#13#10);
  StartCompile;
end;

procedure TMainForm.StartCompile;
begin
  if FileExists(edCHMCompiler.Text) then
  begin
    LogAdd('Starting HTML Help compiler...'#13#10);
    RunCommandInMemo(edCHMCompiler.Text + ' "' + ProjectDir + cbProjectName.Text + '.hhp"', Log);
    LogAdd('Done.'#13#10);
    LogNL;
    if cbOpenAfterProcess.Checked then
      bOpenClick(nil);
    Exit;
  end
  else
    LogAdd('HTML Help compiler not found! (' + edCHMCompiler.Text + ')'#13#10);
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
  VersionString := edVersionString.Text;
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

  DocStructure.IncludeAlphabetClasses := cbIncludeAlphabetClasses.Checked;
  DocStructure.CheckForBrokenLinks := cbBrokenLinks.Checked;

  Project := TProject.Create(nil, ProjectDir + 'Source');
  try
    Project.DisplayName := edProjectTitle.Text;
    Project.DestinationFolder := ProjectDir + 'Docs';
    Project.ImageFolder := ProjectDir + 'Images';
    Project.ScriptFolder := ProjectDir + 'Script';
    Project.StylesFolder := ProjectDir + 'Styles';
    Project.HeadSectionTemplate := ProjectDir + 'HeadSection.tmpl';
    Project.BodySectionTemplate := ProjectDir + 'BodySection.tmpl';
    LogAdd(#13#10+ 'Transforming - ' + cbProjectName.Text+ #13#10);
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
    Project.BuildToc(ProjectDir + cbProjectName.Text + '.hhc');
    LogAdd('... done'#13#10);
    Progress.Position := 95;
    LogAdd('Building Index ...');
    Project.BuildIndex(ProjectDir + cbProjectName.Text + '.hhk');
    LogAdd('... done'#13#10);
    Progress.Position := 100;

    LogAdd('Writing Project ...');
    WriteProject(ProjectDir + cbProjectName.Text + '.hhp');
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

    Result := true;
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
    Lines.Add('Compiled file=' + cbProjectName.Text + '.chm');
    Lines.Add('Contents file=' + cbProjectName.Text + '.hhc');
    Lines.Add('Default Window=Main Window');
    Lines.Add('Default topic=Docs\Overview\_Body.htm');
    Lines.Add('Display compile progress=No');
    Lines.Add('Full-text search=Yes');
    Lines.Add('Index file=' + cbProjectName.Text + '.hhk');
    Lines.Add('Language=0x409 English (United States)');
    Lines.Add('Title=' + edProjectTitle.Text);
    Lines.Add('');
    Lines.Add('[WINDOWS]');
    Lines.Add(Format('Main Window="%s","%s","%s","Docs\Overview\_Body.htm","Docs\Overview\_Body.htm",,,,,0x63520,600,0x10384e,[0,0,900,680],0xb0000,,,1,,,0',
      [edProjectTitle.Text, cbProjectName.Text + '.hhc', cbProjectName.Text + '.hhk']));
    Lines.Add('');
    Lines.Add('[INFOTYPES]');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

procedure TMainForm.bTransformClick(Sender: TObject);
begin
  StartTransforming;
end;

procedure TMainForm.bCompileClick(Sender: TObject);
begin
  Log.Clear;
  StartCompile;
end;

procedure TMainForm.bOpenClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, 'open',
    PChar(IncludeTrailingBackslash(edProjectDirectory.Text) + cbProjectName.Text + '.chm'), '', '', SW_SHOW);
end;

procedure TMainForm.bParseMissingClick(Sender: TObject);
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
        if MessageBox(self.handle, pchar(s),
          pchar(caption), MB_YESNO or MB_DEFBUTTON2) <> IDYES then continue;
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

procedure TMainForm.edProjectTitleChange(Sender: TObject);
begin
  bSaveProjectInfo.Enabled := True;
end;

end.
