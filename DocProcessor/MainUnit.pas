unit MainUnit;

{.$DEFINE DEBUGGING}

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, FileCtrl, ComCtrls, Utils, SimpleDOM,
  Contnrs, DocStructure, IniFiles, ExtCtrls, ShellApi, Pas2Html;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Log: TMemo;
    Panel2: TPanel;
    Label7: TLabel;
    Progress: TProgressBar;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    DirectoryEdit1: TEdit;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Label9: TLabel;
    Edit7: TEdit;
    CheckBox1: TCheckBox;
    Panel9: TPanel;
    Edit6: TEdit;
    Label8: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Label6: TLabel;
    Panel10: TPanel;
    bProcess: TButton;
    Panel11: TPanel;
    bTransform: TButton;
    bCompile: TButton;
    cbOpenAfterProcess: TCheckBox;
    bOpen: TButton;
    bParseMissing: TButton;
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure bProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bTransformClick(Sender: TObject);
    procedure bCompileClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bParseMissingClick(Sender: TObject);
  public
    ProjectDir: string;
    SourceDir: string;
    CompiledDir: string;
    StyleFile: string;
    procedure StartTransforming;
    procedure StartCompile;
    procedure WriteProject(const FileName: string);
  end;

var
  MainForm: TMainForm;
  Project: TProject;

implementation

{$R *.DFM}

procedure LogAdd(const S: string);
begin
  with MainForm.Log do
  begin
    SelStart := Length(Text);
    SelText := S;
    SelStart := Length(Text);
  end;
end;

procedure LogNL;
begin
  with MainForm.Log do
  begin
    SelStart := Length(Text);
    SelText := #13#10;
    SelStart := Length(Text);
  end;
end;

procedure LogReplace(S: string);
begin
  with MainForm.Log do
    Lines[Lines.Count - 1] := S;
end;

procedure TMainForm.DirectoryEdit1Change(Sender: TObject);
begin
  ProjectDir := DirectoryEdit1.Text;
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
  DirectoryEdit1Change(Self);

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'properties.ini');
  try
    for i := 0 to Self.ComponentCount - 1 do
    if Self.Components[i].InheritsFrom(TCustomEdit) and
       not Self.Components[i].InheritsFrom(TMemo) then
    begin
      SValue := Ini.ReadString('Settings', Self.Components[i].Name, '');
      if SValue <> '' then TEdit(Self.Components[i]).Text := SValue;
    end
    else if Self.Components[i].InheritsFrom(TCheckBox) then
      TCheckBox(Self.Components[i]).Checked := Ini.ReadBool('Settings', Self.Components[i].Name, False);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'properties.ini');
  try
    for i := 0 to Self.ComponentCount-1 do
    if Self.Components[i].InheritsFrom(TCustomEdit) and
       not Self.Components[i].InheritsFrom(TMemo) then
      Ini.WriteString('Settings', Self.Components[i].Name, TEdit(Self.Components[i]).Text)
    else if Self.Components[i].InheritsFrom(TCheckBox) then
      Ini.WriteBool('Settings', Self.Components[i].Name, TCheckBox(Self.Components[i]).Checked)
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.bProcessClick(Sender: TObject);
begin
  StartTransforming;
  if CheckBox1.Checked then
  begin
    LogAdd(#13#10);
    StartCompile;
  end;
end;

procedure TMainForm.StartCompile;
begin
  LogAdd('Starting HTML Help compiler...'#13#10);
  RunCommandInMemo(Edit7.Text + ' "' + ProjectDir + Edit5.Text + '"', Log);
  LogAdd('Done.'#13#10);
  if cbOpenAfterProcess.Checked then
    bOpenClick(nil);
end;

procedure TMainForm.StartTransforming;
const
  cUpdateInterval = 500;
var
  I: Integer;
  S: string;
  CompileTime: Cardinal;
  NextUpdate: Cardinal;
begin
  VersionString := Edit6.Text;
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
  Project := TProject.Create(nil, ProjectDir + 'Source');
  try
    Project.DisplayName := Edit1.Text;
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

      TElement(Project.Files.Objects[I]).Transform;
    end;

    LogReplace('Transforming Files ...... done'#13#10);
    LogAdd('Building TOC ...');
    Project.BuildToc(ProjectDir + Edit3.Text);
    LogAdd('... done'#13#10);
    Progress.Position := 95;
    LogAdd('Building Index ...');
    Project.BuildIndex(ProjectDir + Edit2.Text);
    LogAdd('... done'#13#10);
    Progress.Position := 100;

    LogAdd('Writing Project ...');
    WriteProject(ProjectDir + Edit5.Text);
    LogAdd('... done'#13#10);

    LogAdd('Project transformed.'#13#10);
    CompileTime := GetTickCount - CompileTime;
    LogAdd(Format('Compile time: %d minutes, %d seconds'#13#10, [(CompileTime div 1000) div 60, (CompileTime div 1000) mod 60]));
    Progress.Position := 0;
    Log.Color := $E7FFE7;
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
    Lines.Add('Compiled file=' + Edit4.Text);
    Lines.Add('Contents file=' + Edit3.Text);
    Lines.Add('Default Window=Main Window');
    Lines.Add('Default topic=Docs\_Body.htm');
    Lines.Add('Display compile progress=No');
    Lines.Add('Full-text search=Yes');
    Lines.Add('Index file=' + Edit2.Text);
    Lines.Add('Language=0x409 English (United States)');
    Lines.Add('Title=' + Edit1.Text);
    Lines.Add('');
    Lines.Add('[WINDOWS]');
    Lines.Add(Format('Main Window="%s","%s","%s","Docs\_Body.htm","Docs\_Body.htm",,,,,0x63520,600,0x10384e,[0,0,900,680],0xb0000,,,1,,,0',
      [Edit1.Text, Edit3.Text, Edit2.Text]));
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
  ShellExecute(Self.Handle, 'open', PAnsiChar(IncludeTrailingBackslash(DirectoryEdit1.Text) + Edit4.Text), '', '', SW_SHOW);
end;

procedure TMainForm.bParseMissingClick(Sender: TObject);
var
  i,j,k,m: integer;
  srcPasFolder, destUnitFolder, fn: string;
  pasFiles, menuData: TStringList;
begin
  if SourceDir = '' then exit;
  srcPasFolder := GetDelphiSourceFolder;
  if srcPasFolder = '' then exit;
  {$IFDEF DEBUGGING}
  destUnitFolder := 'c:\temp\';
  {$ELSE}
  destUnitFolder := SourceDir + 'Units\';
  {$ENDIF}
  Log.Clear;
  Log.Color := clWhite;

  if not DirectoryExists(destUnitFolder) then
  begin
    LogAdd('Error: destination folder does not exist. '#13#10);
    Log.Color := $E7FFE7;
    exit;
  end;

  LogAdd('Starting Pas2Html ...'#13#10);
  Application.ProcessMessages;
  j := 0;
  pasFiles := GetFileList(srcPasFolder, '*.pas');
  try
    pasFiles.Sort;
    for i := 0 to pasFiles.Count -1 do
    begin
      fn := ChangeFileExt(ExtractFileName(pasFiles[i]),'');
      if not DirectoryExists(destUnitFolder + fn) then
      begin
        inc(j);
        k := BuildNewUnit(pasFiles[i], destUnitFolder + fn + '\');
        LogAdd('  added: ' + pasFiles[i] + #13#10);
        if k >= 0 then
          LogAdd('  (parse error at line ' + inttostr(k+1) +')'#13#10);
        Application.ProcessMessages;
        //getting ready to update the menu list (ie to save redoing stuff) ...
        pasFiles[i] := fn;
        pasFiles.Objects[i] := pointer(1);
      end;
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
          menuData.SaveToFile('c:\temp\menu_data.txt');
          {$ELSE}
          menuData.SaveToFile(ProjectDir + 'Scripts\menu_data.js');
          {$ENDIF}
          LogAdd('Menu list of items updated' +#13#10);
        end;
      finally
        menuData.Free;
      end;
    end;
  finally
    pasFiles.Free;
  end;
  LogAdd('... done'#13#10);
  LogAdd(inttostr(j) +' units added.'#13#10);
  Log.Color := $E7FFE7;
end;

end.
