unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, JvToolEdit, FileCtrl, ComCtrls, Utils, SimpleDOM,
  Contnrs, DocStructure, IniFiles, JvExMask;

type
  TMainForm = class(TForm)
    DirectoryEdit1: TJvDirectoryEdit;
    Label1: TLabel;
    Process: TButton;
    Log: TMemo;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Edit4: TEdit;
    Label6: TLabel;
    Edit5: TEdit;
    Progress: TProgressBar;
    Label7: TLabel;
    Edit6: TEdit;
    Label8: TLabel;
    procedure DirectoryEdit1Change(Sender: TObject);
    procedure ProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    ProjectDir: string;
    SourceDir: string;
    CompiledDir: string;
    StyleFile: string;
    procedure StartProcessing;
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
      Process.Enabled := True;
      Exit;
    end;
  end;
  Process.Enabled := False;
  ProjectDir := '';
  SourceDir := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
  Value: String;
begin
  DirectoryEdit1Change(Self);

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'properties.ini');
  try
    for i := 0 to Self.ComponentCount-1 do
    if Self.Components[i].InheritsFrom(TCustomEdit) and
       not Self.Components[i].InheritsFrom(TMemo) then
    begin
      Value := Ini.ReadString('Settings', Self.Components[i].Name, '');
      If Value <> '' then TEdit(Self.Components[i]).Text := Value;
    end;
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
      Ini.WriteString('Settings', Self.Components[i].Name, TEdit(Self.Components[i]).Text);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.ProcessClick(Sender: TObject);
begin
  VersionString := Edit6.Text;
  StartProcessing;
end;

procedure TMainForm.StartProcessing;
var
  I: Integer;
  S: string;
begin
  if ProjectDir = '' then Exit;
  Log.Clear;
  Log.Color := clWhite;
  Progress.Position := 0;
  Enabled := False;
  Application.ProcessMessages;
  if DirectoryExists(ProjectDir + 'Compiled') then RemoveDir(ProjectDir + 'Compiled');
  CompiledDir := (ProjectDir + 'Compiled\');
  StyleFile := ProjectDir + 'Styles\Default.css';

  Project := TProject.Create(nil, ProjectDir + 'Source');
  try
    Project.DisplayName := Edit1.Text;
    Project.DestinationFolder := ProjectDir + 'Compiled';
    Project.ImageFolder := ProjectDir + 'Images';
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

    for I := 0 to Project.Files.Count - 1 do
    begin
      S := TElement(Project.Files.Objects[I]).DisplayName;
      LogReplace(Format('Transforming File: (%d/%d) %s',
        [I + 1, Project.Files.Count, S]));
      TElement(Project.Files.Objects[I]).Transform;
      Progress.Position := 4 + 83 * I div Project.Files.Count;
      Application.ProcessMessages;
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

    LogAdd('Project Transformed');
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
    Lines.Add('Default topic=Compiled\_Body.htm');
    Lines.Add('Display compile progress=No');
    Lines.Add('Full-text search=Yes');
    Lines.Add('Index file=' + Edit2.Text);
    Lines.Add('Language=0x409 English (United States)');
    Lines.Add('Title=' + Edit1.Text);
    Lines.Add('');
    Lines.Add('[WINDOWS]');
    Lines.Add(Format('Main Window="%s","%s","%s","Compiled\_Body.htm","Compiled\_Body.htm",,,,,0x23520,200,0x10384e,,0xb0000,,,1,,,0',
      [Edit1.Text, Edit3.Text, Edit2.Text]));
    Lines.Add('');
    Lines.Add('[INFOTYPES]');
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

end.
