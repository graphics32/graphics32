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
    bTransform: TButton;
    cbBrokenLinks: TCheckBox;
    cbIncludeAlphabetClasses: TCheckBox;
    cbOpenAfterProcess: TCheckBox;
    edCHMCompiler: TEdit;
    edProjectDirectory: TEdit;
    edProjectName: TEdit;
    edProjectTitle: TEdit;
    edVersionString: TEdit;
    lblCompiler: TLabel;
    lblProgress: TLabel;
    lblProjectDirectory: TLabel;
    lblProjectFileName: TLabel;
    lblProjectTitle: TLabel;
    lblVersionString: TLabel;
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
    Shape1: TShape;
    procedure edProjectDirectoryChange(Sender: TObject);
    procedure bProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bTransformClick(Sender: TObject);
    procedure bCompileClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bParseMissingClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    ProjectDir: string;
    SourceDir: string;
    CompiledDir: string;
    StyleFile: string;
    function StartTransforming: boolean;
    procedure StartCompile;
    procedure WriteProject(const FileName: string);
  end;

var
  PropertiesFilename: string;
  DelphiSourceFolder: string;
  NoGUI: Boolean;
  MainForm: TMainForm;
  Project: TProject;

implementation

{$R *.DFM}

function DeleteDirectoryTree(dir: string): boolean;
var
  len: integer;
  shfo : TSHFileOpStruct;
begin
  result := false;
  len := length(dir);
  if (len > 0) and (dir[len] = '\') then dir[len] := #0;
  if not DirectoryExists(dir) then exit;
  fillChar(shfo, sizeof(shfo), 0);
  shfo.Wnd := 0;
  shfo.wFunc := FO_DELETE;
  shfo.pFrom := PChar(dir);
  shfo.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;
  result := SHFileOperation(shfo) = 0;
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
      Exit;
    end;
  end;
  bProcess.Enabled := False;
  ProjectDir := '';
  SourceDir := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
  SValue: String;
begin
  edProjectDirectoryChange(Self);

  if ParamCount > 1 then
    PropertiesFilename := ParamStr(1)
  else
    PropertiesFilename := ExtractFilePath(ParamStr(0)) + 'properties.ini';

  Ini := TIniFile.Create(PropertiesFilename);
  try
    for i := 0 to Self.ComponentCount - 1 do
      if Self.Components[i].InheritsFrom(TCustomEdit) and
         not Self.Components[i].InheritsFrom(TMemo) then
      begin
        SValue := Ini.ReadString('Settings', Copy(Self.Components[i].Name, 3, MAXINT), '');
        if SValue <> '' then TEdit(Self.Components[i]).Text := SValue;
      end
      else if Self.Components[i].InheritsFrom(TCheckBox) then
        TCheckBox(Self.Components[i]).Checked := Ini.ReadBool('Settings', Copy(Self.Components[i].Name, 3, MAXINT), False);

    DelphiSourceFolder := Ini.ReadString('Settings', 'DelphiSourceFolder', '');
  finally
    Ini.Free;
  end;

  NoGUI := False;

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

procedure TMainForm.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
begin
  Ini := TIniFile.Create(PropertiesFilename);
  try
    for i := 0 to Self.ComponentCount-1 do
      if Self.Components[i].InheritsFrom(TCustomEdit) and
         not Self.Components[i].InheritsFrom(TMemo) then
        Ini.WriteString('Settings', Copy(Self.Components[i].Name, 3, MAXINT), TEdit(Self.Components[i]).Text)
      else if Self.Components[i].InheritsFrom(TCheckBox) then
        Ini.WriteBool('Settings', Copy(Self.Components[i].Name, 3, MAXINT), TCheckBox(Self.Components[i]).Checked);

    Ini.WriteString('Settings', 'DelphiSourceFolder', DelphiSourceFolder);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.bCloseClick(Sender: TObject);
begin
  close;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: close;
    VK_F7: bTransformClick(nil);
    VK_F8: bCompileClick(nil);
    VK_F9: bProcessClick(nil);
  end;
end;

procedure TMainForm.bProcessClick(Sender: TObject);
begin
  if not StartTransforming then exit;
  LogAdd(#13#10);
  StartCompile;
end;

procedure TMainForm.StartCompile;
begin
  LogAdd('Starting HTML Help compiler...'#13#10);
  RunCommandInMemo(edCHMCompiler.Text + ' "' + ProjectDir + edProjectName.Text + '.hhp"', Log);
  LogAdd('Done.'#13#10);
  if cbOpenAfterProcess.Checked then
    bOpenClick(nil);
end;

function TMainForm.StartTransforming: boolean;
const
  cUpdateInterval = 500;
var
  I: Integer;
  S: string;
  CompileTime: Cardinal;
  NextUpdate: Cardinal;
begin
  result := false;
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
    LogAdd('Transforming'#13#10);

    LogAdd('Reading files ...');
    Project.Read;
    Progress.Position := 2;
    LogAdd('... done'#13#10);
    LogNL;
    LogAdd('Project Contains:'#13#10);
    LogAdd(#9'Units       '#9 + IntToStr(Project.Units.Count)        + #13#10);
    LogAdd(#9'Classes     '#9 + IntToStr(Length(Project.Classes))    + #13#10);
    LogAdd(#9'Interfaces  '#9 + IntToStr(Length(Project.Interfaces)) + #13#10);
    LogAdd(#9'Topics      '#9 + IntToStr(Length(Project.Topics))     + #13#10);
    LogAdd(#9'HTML Files  '#9 + IntToStr(Project.Files.Count)        + #13#10);
    LogNL;
    LogAdd('Restoring Class Hierarchy ...');
    Project.BuildHierarchy;
    LogAdd('... done'#13#10);

    if DirectoryExists(CompiledDir) then
    begin
      LogNL;
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
          exit;
        end;
      end;
    end;
    LogReplace('Transforming Files ...... done'#13#10);
    LogAdd('Building TOC ...');
    Project.BuildToc(ProjectDir + edProjectName.Text + '.hhc');
    LogAdd('... done'#13#10);
    Progress.Position := 95;
    LogAdd('Building Index ...');
    Project.BuildIndex(ProjectDir + edProjectName.Text + '.hhk');
    LogAdd('... done'#13#10);
    Progress.Position := 100;

    LogAdd('Writing Project ...');
    WriteProject(ProjectDir + edProjectName.Text + '.hhp');
    LogAdd('... done'#13#10);

    LogAdd('Project transformed.'#13#10);

    if Project.BrokenLinks.Count > 0 then
    begin
      LogNL;
      LogAdd('Broken links found:'#13#10);
      for I := 0 to Project.BrokenLinks.Count -1 do
        LogAdd(Project.BrokenLinks[i]);
      LogNL;
    end;

    CompileTime := GetTickCount - CompileTime;
    LogAdd(Format('Compile time: %d minutes, %d seconds'#13#10, [(CompileTime div 1000) div 60, (CompileTime div 1000) mod 60]));
    Progress.Position := 0;
    Log.Color := $E7FFE7;

    result := true;
  finally
    Enabled := True;
    Project.Free;
  end;
end;

procedure TMainForm.WriteProject(const FileName: string);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('[OPTIONS]');
    Lines.Add('Compatibility=1.1 or later');
    Lines.Add('Compiled file=' + edProjectName.Text + '.chm');
    Lines.Add('Contents file=' + edProjectName.Text + '.hhc');
    Lines.Add('Default Window=Main Window');
    Lines.Add('Default topic=Docs\Overview\_Body.htm');
    Lines.Add('Display compile progress=No');
    Lines.Add('Full-text search=Yes');
    Lines.Add('Index file=' + edProjectName.Text + '.hhk');
    Lines.Add('Language=0x409 English (United States)');
    Lines.Add('Title=' + edProjectTitle.Text);
    Lines.Add('');
    Lines.Add('[WINDOWS]');
    Lines.Add(Format('Main Window="%s","%s","%s","Docs\Overview\_Body.htm","Docs\Overview\_Body.htm",,,,,0x63520,600,0x10384e,[0,0,900,680],0xb0000,,,1,,,0',
      [edProjectTitle.Text, edProjectName.Text + '.hhc', edProjectName.Text + '.hhk']));
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
  ShellExecute(Self.Handle, 'open', PChar(IncludeTrailingBackslash(edProjectDirectory.Text) + edProjectName.Text + '.chm'), '', '', SW_SHOW);
end;

procedure TMainForm.bParseMissingClick(Sender: TObject);
var
  i,j,k,m: integer;
  destUnitFolder, fn, s: string;
  pasFiles, menuData: TStringList;
begin
  if SourceDir = '' then exit;
  DelphiSourceFolder := GetDelphiSourceFolder(DelphiSourceFolder);
  if DelphiSourceFolder = '' then exit;
  destUnitFolder := SourceDir + 'Units\';

  Log.Clear;
  Log.Color := clWhite;

  if not DirectoryExists(destUnitFolder) then
  begin
    LogAdd('Error: destination folder does not exist. '#13#10);
    Log.Color := $E7FFE7;
    exit;
  end;
  LogAdd('Starting Pas2Html ...'#13#10);
  LogNL;
  Application.ProcessMessages;
  j := 0;
  pasFiles := GetFileList(DelphiSourceFolder, '*.pas');
  try
    pasFiles.Sort;
    for i := 0 to pasFiles.Count -1 do
    begin
      fn := ChangeFileExt(ExtractFileName(pasFiles[i]),'');
      if DirectoryExists(destUnitFolder + fn) then
      begin
        s := 'The file '+ fn + ' has already been imported into the help source.'+#10+
          'Do you want to replace the existing contents with this new file?';
        if MessageBox(self.handle, pchar(s),
          pchar(caption), MB_YESNO or MB_DEFBUTTON2) <> IDYES then continue;
        DeleteFolder(destUnitFolder + fn);
      end;
      inc(j);
      k := BuildNewUnit(ansiString(pasFiles[i]),
        ansiString(destUnitFolder + fn + '\'), ProjectDir);
      LogAdd('  added: ' + pasFiles[i] + #13#10);
      if k >= 0 then
        LogAdd('  (parse error at line ' + inttostr(k+1) +')'#13#10);
      Application.ProcessMessages;
      //getting ready to update the menu list (ie saves redoing stuff) ...
      pasFiles[i] := fn;
      pasFiles.Objects[i] := pointer(1); //flags a new unit
    end;
    //now update the help file's dropdown menu list of units
    if (j > 0) and FileExists(ProjectDir + 'Scripts\menu_data.js') then
    begin
      menuData := TStringList.Create;
      try
        menuData.LoadFromFile(ProjectDir + 'Scripts\menu_data.js');
        i := menuData.IndexOf('td_6 = "Additional Units"');
        if i > 0 then
        begin
          while menuData[i] <> '' do inc(i);
          //now append to the list of additional units ...
          pasFiles.Sort;
          m := j;
          for k := pasFiles.Count -1 downto 0 do
            if assigned(pasFiles.Objects[k]) then
            begin
              menuData.Insert(i, format('url_6_%d = "Units/%s/_Body.htm"',[m, pasFiles[k]]));
              menuData.Insert(i, format('td_6_%d = "%s.pas"',[m, pasFiles[k]]));
              dec(m);
            end;
          {$IFDEF DEBUGGING}
          //menuData.SaveToFile('c:\temp\menu_data.txt');
          {$ELSE}
          menuData.SaveToFile(ProjectDir + 'Scripts\menu_data.js');
          {$ENDIF}
          LogNL;
          LogAdd('  ''Additional Units'' menu updated.' +#13#10);
        end;
      finally
        menuData.Free;
      end;
    end;
  finally
    pasFiles.Free;
  end;
  LogAdd(inttostr(j) +' units added.'#13#10);
  LogNL;
  LogAdd('... done'#13#10);
  Log.Color := $E7FFE7;
end;

end.
