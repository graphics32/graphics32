unit DocStructure;

{$I DocProcessor.inc}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, SysUtils, Contnrs, Windows, FileCtrl, StrUtils, SimpleDOM, Math;

type

  TMenuSubItem = record
    shortFileName : string;
    url           : string;
  end;

  TMenuTopItem = record
    shortFileName : string;
    url           : string;
    subItems      : array of TMenuSubItem;
  end;
  TMenuTopItems = array of TMenuTopItem;

  TElement = class;
  TElementClass = class of TElement;
  TElements = class;
  TProject = class;
  TGroupElement = class;
  TGroupElementClass = class of TGroupElement;

  TElements = class(TObjectList)
  private
    function GetItems(Index: Integer): TGroupElement;
    procedure SetItems(Index: Integer; Value: TGroupElement);
  public
    ElemClass: TGroupElementClass;
    groupName: string;
    altGroupName: string;
    Owner: TElement;
    constructor Create(AOwner: TElement; AElemClass: TGroupElementClass; const ADirName: string);
    procedure Add(AElement: TGroupElement);
    procedure Read;
    property Items[Index: Integer]: TGroupElement read GetItems write SetItems; default;
  end;

  TElement = class
  private
    function ExpandFileName(filename: string): string;
    procedure AddBrokenLink(const link: string);
  protected
    class function IsTopic: Boolean; virtual;
    procedure AddUnitTable(divNode: TDomNode; elements: TElements);
    procedure AddSeeAlso(Body: TDomNode; SeeAlso: TStringList);
    procedure InsertMenu(Body: TDomNode; menuIdx: integer; isTopLevel: Boolean);
    procedure InsertTopLevelMenu(menuNode: TDomNode; menuIdx: integer);
    procedure InsertNormalMenu(menuNode: TDomNode);
    procedure InsertHeading(Body: TDomNode);
  public
    FileName: string;
    Folder: string;
    ShortFileName: string;
    TitleText: string;
    Parent: TElement;
    Project: TProject;
    FolderLevel: integer;
    constructor Create(AParent: TElement; const APath: string); virtual;
    function GetDstFolder: string;
    function GetDstFile: string;
    function LinkTo(Target: TElement): string; overload;
    function LinkTo(const Target: string): string; overload;
    function PathTo(Target: TElement): string; overload;
    function PathTo(const Target: string): string; overload;
    procedure ProcessBody(Body: TDomNode; const Anchors, SeeAlso: TStringList); virtual;
    procedure Transform;
  end;

  TNodeElement = class(TElement)
  private
    Children: TObjectList;
    function GetChild(Index: Integer): TNodeElement;
  public
    constructor Create(AParent: TElement; const APath: string); override;
    constructor CreateRoot(AParent: TElement);
    destructor Destroy; override;
    function Count: Integer;
    procedure Read; virtual;
    procedure SetOrder(S: string);
    property Child[Index: Integer]: TNodeElement read GetChild;
  end;

  TGroupElement = class(TElement)
  private
    ListRef: TElements;
    ChildLists: TList;
  protected
    procedure RegList(AList: TElements);
  public
    constructor Create(AParent: TElement; const APath: string); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Read; virtual;
    function Next: TElement;
    function Prev: TElement;
  end;

  TTopicElement = class(TGroupElement)
  protected
    class function IsTopic: Boolean; override;
  public
    procedure Read; override;
  end;

  TClassElement = class(TGroupElement)
    Fields: TElements;
    Properties: TElements;
    Methods: TElements;
    Events: TElements;
    Ancestor: TClassElement;
    AncestorName: string;
    constructor Create(AParent: TElement; const APath: string); override;
    procedure ProcessBody(Body: TDomNode; const Anchors, Links: TStringList); override;
    procedure Read; override;
  end;

  TInterfaceElement = class(TClassElement)
    procedure Read; override;
  end;

  TUnitElement = class(TGroupElement)
    Interfaces: TElements;
    Classes: TElements;
    Functions: TElements;
    Types: TElements;
    Globals: TElements;
    Variables: TElements;
    Constants: TElements;
    constructor Create(AParent: TElement; const APath: string); override;
    procedure ProcessBody(Body: TDomNode; const Anchors, Links: TStringList); override;
  end;

  TIndex = class;

  TIndexEntry = class
    Keyword: string;
    Name: string;
    Targets: TStringList;
    Members: TIndex;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TIndex = class(TObjectList)
  private
    function GetItem(Index: Integer): TIndexEntry;
    procedure SetItem(Index: Integer; Value: TIndexEntry);
  public
    function Add(const AKeyword, ATarget: string): TIndexEntry;
    procedure AddMember(const AKeyword, AKeyword2, AName, ATarget: string);
    property Items[Index: Integer]: TIndexEntry read GetItem write SetItem; default;
  end;

  TClassElementArray = array of TClassElement;

  TProject = class(TGroupElement)
  public
    Units: TElements;
    Root: TNodeElement;

    Classes: array of TClassElement;
    Interfaces: array of TInterfaceElement;
    Topics: array of TTopicElement;

    Files: TStringList;
    FileElements: array of TTopicElement;
    SourceFolder: string;
    DestinationFolder: string;
    ImageFolder: string;
    ScriptFolder: string;

    HeadIncludes: TDomDocument;
    HeadInclude: TDomNode;
    BodyIncludes: string;

    MenuTopItems: TMenuTopItems;

    BrokenLinks: TStringList;
    imageNames : TStringList;

    CheckForBrokenLinks: boolean;
    CheckBrokenImages: Boolean;

    Index: TIndex;
    constructor Create(AParent: TElement; const APath: string); override;
    destructor Destroy; override;
    procedure BuildHierarchy;
    function FindClass(const AName: string): TClassElement;
    function FindInterface(const AName: string): TInterfaceElement;
    procedure ProcessBody(Body: TDomNode; const Anchors, Links: TStringList); override;
    procedure Read; override;
  end;

  TClassEntry = class
    Element: TElement;
    Children: TObjectList;
    Parent: TClassEntry;
    constructor Create(AElem: TElement);
    destructor Destroy; override;
  end;

resourcestring
  DefaultTypesCaption = 'Types';
  DefaultStaticFuncsCaption = 'Static Functions';

var
  dataTypesCaption: string = DefaultTypesCaption;
  staticFuncsCaption: string = DefaultStaticFuncsCaption;

  VersionString: string = '1.0';
  BuildDateString: string;

implementation

uses
  Utils;

resourcestring
  RCStrInvalidNodeElement = 'Invalid node element';
  RCStrInvalidElementPath = 'Invalid element path';
  RCStrCantGetDestinationFileName = 'Can''t get destination filename';
  RCStrCantGetDestinationFolder = 'Can''t get destination folder';
  RCStrInvalidTarget = 'Invalid target';
  RCStrFileSIsInvalidHead = 'File ''%s'' is invalid (missing HEAD element)';
  RCStrFileSIsInvalidBody = 'File ''%s'' is invalid (missing BODY element)';
  RCStrLoopedHierarchyRef = 'Looped hierarchy Ref';

type
  TClassLinks = class
    ShortFileName: string;
    Fields: TStringList;
    Properties: TStringList;
    Methods: TStringList;
    Events: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Get(Src: TClassElement);
    procedure RemoveDuplicatesFrom(Links: TClassLinks);
    function TotalCount: Integer;
  end;

{ TClassLinks }

constructor TClassLinks.Create;
begin
  Fields := TStringList.Create;
  Properties := TStringList.Create;
  Methods := TStringList.Create;
  Events := TStringList.Create;
end;

destructor TClassLinks.Destroy;
begin
  Events.Free;
  Methods.Free;
  Properties.Free;
  Fields.Free;
  inherited;
end;

procedure TClassLinks.Get(Src: TClassElement);
var
  I: Integer;
begin
  ShortFileName := Src.ShortFileName;
  for I := 0 to Src.Fields.Count - 1 do
    Fields.Add(Src.Fields[I].FileName);
  for I := 0 to Src.Methods.Count - 1 do
    Methods.Add(Src.Methods[I].FileName);
  for I := 0 to Src.Properties.Count - 1 do
    Properties.Add(Src.Properties[I].FileName);
  for I := 0 to Src.Events.Count - 1 do
    Events.Add(Src.Events[I].FileName);
end;

procedure TClassLinks.RemoveDuplicatesFrom(Links: TClassLinks);

  procedure CleanList(Master, Target: TStringList);
  var
    I, J: Integer;
    S: string;
  begin
    for I := 0 to Master.Count - 1 do
    begin
      S := Master[I];
      J := 0;
      while J < Target.Count do
        if SameText(GetLinkName(Target[J]), GetLinkName(S)) then Target.Delete(J)
        else Inc(J);
    end;
  end;

begin
  CleanList(Fields, Links.Fields);
  CleanList(Methods, Links.Methods);
  CleanList(Properties, Links.Properties);
  CleanList(Events, Links.Events);
end;

function TClassLinks.TotalCount: Integer;
begin
  Result := Fields.Count;
  if Properties.Count > Result then Result := Properties.Count;
  if Methods.Count > Result then Result := Methods.Count;
  if Events.Count > Result then Result := Events.Count;
end;

{ TElements }

procedure TElements.Add(AElement: TGroupElement);
begin
  inherited Add(AElement);
  AElement.ListRef := Self;
end;

constructor TElements.Create(AOwner: TElement;
  AElemClass: TGroupElementClass; const ADirName: string);
var
  aname: string;
  i: integer;
begin
  OwnsObjects := true;                           //!! IMPORTANT
  Owner := AOwner;
  ElemClass := AElemClass;
  aname := ADirName;
  i := pos(',', aname);
  if i = 0 then i := pos(';', aname);
  if i > 1 then
  begin
    groupName := Trim(copy(aname, 1, i -1));
    altGroupName := Trim(copy(aname, i +1, 255));
  end else
  begin
    groupName := ADirName;
    altGroupName := '';
  end;
end;

function TElements.GetItems(Index: Integer): TGroupElement;
begin
  Result := TGroupElement(inherited Items[Index]);
end;

procedure TElements.Read;
var
  Listing: TStringList;
  I: Integer;
  Elem: TGroupElement;
  Folder: string;
begin
  Assert(ElemClass <> nil);
  Assert(Owner <> nil);
  Clear;
  Folder := Owner.Folder + '\' + groupName;
  if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(Folder) then
  begin
    if (altGroupName = '') then Exit;
    Folder := Owner.Folder + '\' + altGroupName;
    if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(Folder) then
      Exit;
  end;

  Listing := nil;                                //!! IMPORTANT BUGFIX
  try
    if ElemClass.IsTopic then
    begin
      // read topics
      Listing := GetFileList(Folder, '*.htm*');
      for I := 0 to Listing.Count - 1 do
      begin
        Elem := ElemClass.Create(Owner, Listing[I]);
        Add(Elem);
        Elem.Read;
      end;
    end
    else
    begin
      // read non-topic element
      Listing := GetDirList(Folder, '*.*');
      for I := 0 to Listing.Count - 1 do
      begin
        Elem := ElemClass.Create(Owner, Listing[I]);
        Add(Elem);
        Elem.Read;
      end;
    end;
    Sort(CompareDisplayNames);
  finally
    Listing.Free;
  end;
end;

procedure TElements.SetItems(Index: Integer; Value: TGroupElement);
begin
  inherited Items[Index] := Value;
end;

function AddLevels(levels: integer): string;
var
  i: integer;
begin
  if levels <= 0 then Result := './'
  else
  begin
    Result := '';
    for i := 1 to levels do
      Result := Result + '../';
  end;
end;

{ TElement }

// File names should be case sensitive since the hosting web server will
// likely be running on a Linux machine where filenames are case sensitive.
function FileExistsCaseSensitive(const filename: string): Boolean;
var
  fileNameNoPath: string;
  flags: Cardinal;
  handle: THandle;
  findData: TWin32FindData;
begin
  flags := GetFileAttributes(PChar(FileName));
  Result :=
    (flags <> INVALID_FILE_ATTRIBUTES) and
    (faDirectory and flags = 0);
  if not Result then Exit;

  handle := FindFirstFile(PChar(Filename), findData);
  if handle <> INVALID_HANDLE_VALUE then
  begin
    FindClose(handle);
    fileNameNoPath := ExtractFileName(filename);
    Result := CompareStr(FindData.cFileName, fileNameNoPath) = 0;
  end;
end;

procedure TElement.AddUnitTable(divNode: TDomNode; elements: TElements);
var
  i: Integer;
begin
  elements.Sort(CompareDisplayNames);
  divNode.Attributes['class'] := 'autoTbl';
  divNode.Add('div').Add('b').AddText(elements.groupName);
  with divNode.Add('div') do
  begin
    for i := 0 to elements.Count - 1 do
    begin
      with Add('span').Add('a') do
      begin
        Attributes['href'] := PathTo(elements[I].FileName);
        AddText(GetLinkName(elements[I].FileName));
      end;
      AddText(#13#10);
    end;
  end;
  divNode.Add('br');
end;

procedure TElement.AddSeeAlso(Body: TDomNode; SeeAlso: TStringList);
var
  I, J: Integer;
  S, PrevS: string;
  E: TElement;
begin
  if not assigned(project) or (SeeAlso.Count = 0) then
    Exit;

  with Body.Add('h2') do
  begin
    Attributes['id'] := 'Auto-SeeAlso';
    AddText('See Also');
  end;
  with Body.Add('p') do
  begin
    // Attributes['id'] := 'Auto'; must be unique!
    Attributes['class'] := 'Body';

    SeeAlso.CustomSort(CompareLinks);
    PrevS := '';
    for I := 0 to SeeAlso.Count - 1 do
    begin
      S := SeeAlso[I];
      if (Pos('mailto:', S) > 0) then Continue;
      J := Pos('#', S);
      if J > 0 then SetLength(S, J -1);

      if Project.CheckForBrokenLinks and
        not FileExistsCaseSensitive(S) then
          AddBrokenLink(S);

      E := nil;
      for J := 0 to Project.Files.Count - 1 do
        if Project.Files[J] = S then
        begin
          E := TElement(Project.Files.Objects[J]);
          Break;
        end;

      if not assigned(E) then
        S := LinkTo(S)
      else if Assigned(E.Parent) and
        (E.Parent is TClassElement) and (E.Parent <> Self.Parent) then
      begin
        S := Format('<a href="%s">%s</a>',
          [PathTo(S), E.Parent.ShortFileName + '.' + GetLinkName(S)]);
      end
      else if (E = E.Project) then
        S := Format('<a href="%s"><b>Index</b></a>', [PathTo(S)])
      else
        S := LinkTo(S);

      if SameText(S, PrevS) then
        Continue;
      PrevS := S;

      if I > 0 then AddText(', ');
      AddParse(S);
    end;
  end;
end;

function GetFolderLevel(const folderPath: string): integer;
var
  i, len: integer;
  isFilePath: boolean;
begin
  result := 0;
  len := length(folderPath);
  if len = 0 then Exit;
  isFilePath := false;
  for i := len downto 1 do
  begin
    if CharInSet(folderPath[i], ['\','/']) then inc(Result)
    else if (folderPath[i] = '.') and (result = 0) then
      isFilePath := true;
  end;
  if not isFilePath and
    not CharInSet(folderPath[len], ['\','/']) then inc(Result);
end;

constructor TElement.Create(AParent: TElement; const APath: string);
begin
  Parent := AParent;
  if AParent <> nil then Project := AParent.Project;

  if Self is TNodeElement then
  begin
    if FileExists(APath) then
    begin
      FileName := APath;
      Folder := ExtractFilePath(APath);
      ShortFileName := FileNameNoExt(APath);
    end
    else if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(APath) then
    begin
      if FileExists(APath + '\_Body.htm') then
        FileName := APath + '\_Body.htm';
      Folder := APath;
      ShortFileName := GetFolderName(APath);
    end
    else raise Exception.Create(RCStrInvalidNodeElement);
  end
  else if IsTopic then
  begin
    // APath must point to *.htm* file
    if not FileExists(APath) then
      raise Exception.Create(RCStrInvalidElementPath);
    FileName := APath;
    Folder := ExtractFilePath(APath);
    ShortFileName := FileNameNoExt(APath);
  end
  else
  begin
    // A path must point to a directory with optional '_Body.htm' in it
    if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(APath) then
      Exception.Create(RCStrInvalidElementPath);
    if FileExists(APath + '\_Body.htm') then
      FileName := APath + '\_Body.htm';
    Folder := APath;
    ShortFileName := GetFolderName(APath); // ie directory's name
  end;

  if (FileName <> '') and (Project <> nil) then
  begin
    Project.Files.AddObject(FileName, Self);
    FolderLevel := GetFolderLevel(FileName);
  end;
end;

function TElement.GetDstFile: string;
begin
  if FileName = '' then
    raise Exception.Create(RCStrCantGetDestinationFileName);
  Result := Copy(FileName, Length(Project.Folder) + 2, 1000);
  Result := Project.DestinationFolder + '\' + Result;
end;

procedure TElement.AddBrokenLink(const link: string);
begin
  Project.BrokenLinks.Add('  ' + link + #13#10);
  Project.BrokenLinks.Add('    in ' + Self.FileName + #13#10);
end;

{$WARN SYMBOL_PLATFORM OFF}
function TElement.ExpandFileName(filename: string): string;
var
  i,i2, upLevels: integer;
  folder: string;
begin
  upLevels := 0;
  i := 1;
  filename := StringReplace(filename, '/', '\', [rfReplaceAll]);

  while True do
  begin
    i2 := PosEx('..', filename, i);
    if i2 = 0 then break;
    i := i2 + 2;
    if (filename[i] = '\') then
      inc(upLevels) else
      break;
  end;
  if filename[i] = '.' then inc(i);
  if (filename[i] = '\') then inc(i);
  filename := Copy(filename, i, Length(filename));

  folder := StringReplace(Self.folder, '/', '\', [rfReplaceAll]);
  folder := IncludeTrailingBackslash(folder);
  i := Length(folder);
  while upLevels > 0 do
  begin
    dec(i);
    while (i > 0) and (folder[i] <> '\') do dec(i);
    dec(upLevels);
  end;
  Result := Copy(folder, 1, i) + filename;
end;
{$WARN SYMBOL_PLATFORM ON}

function TElement.GetDstFolder: string;
begin
  if Folder = '' then
    raise Exception.Create(RCStrCantGetDestinationFolder);
  Result := Copy(Folder, Length(Project.Folder) + 1, 1000);
  Result := Project.Folder + '\' + Result;
end;

procedure TElement.InsertMenu(Body: TDomNode;
  menuIdx: integer; isTopLevel: Boolean);
var
  menuNode, node: TDomNode;
begin
  //insert menu container
  menuNode := Body.Insert(0, 'div');
  menuNode.Attributes['class'] := 'menu';
  menuNode.Attributes['id'] := 'menu';
  if isTopLevel then
    menuNode.Attributes['onmouseleave'] := 'OnMouseLeaveEvent()';
  menuNode.AddText(#13#10);

  //insert Index (which is always an ancestor)
  if isTopLevel and (menuIdx = -2) then
  begin
    node := menuNode.Add('span');
    node.Attributes['class'] := 'ancestor active';
    node.AddText('Index');
  end else
  begin
    node := menuNode.Insert(1, 'a');
    node.Attributes['class'] := 'ancestor';
    node.Attributes['href'] := PathTo(Project);
    node.AddText('Index');
  end;
  menuNode.AddText(#13#10);

  if isTopLevel then
    InsertTopLevelMenu(menuNode, menuIdx) else
    InsertNormalMenu(menuNode);

  Body.Insert(1, 'br');
  Body.Insert(1, 'br');
end;

procedure TElement.InsertTopLevelMenu(menuNode: TDomNode; menuIdx: integer);
var
  i,j: integer;
  hamberger, node, subdiv, subnode: TDomNode;
begin
  for i := 0 to High(Project.MenuTopItems) do
    with Project.MenuTopItems[i] do
    begin
      if i = menuIdx then
      begin
        node := menuNode.Add('span');
        node.Attributes['class'] := 'active';
        node.AddText(shortFileName); //TMenuTopItem
      end
      else if pos('.htm', Url) = 0 then
      begin
        subnode := menuNode.Add('span');
        subnode.Attributes['class'] := 'submenu_owner';
        subnode.Attributes['onmouseover'] := 'onSubmenuPopup(this)';
        subnode.AddText(#13#10);
        with subnode.Add('span') do
        begin
          Attributes['class'] := 'submenu_heading';
          AddText(shortFileName);
        end;
        subnode.AddText(#13#10);
        subdiv := subnode.Add('div');
        subdiv.Attributes['class'] := 'submenu_background';

        for j := 0 to High(subItems) do
          with subItems[j] do
          begin
            subdiv.AddText(#13#10);
            node := subdiv.Add('a');
            node.AddText(shortFileName);     //TMenuSubItem.shortFileName
            node.Attributes['href'] := Url;  //TMenuSubItem.Url
          end;
      end else
      begin
        node := menuNode.Add('a');
        node.AddText(shortFileName);         //TMenuTopItem.shortFileName
        node.Attributes['href'] := Url;      //TMenuTopItem.Url
      end;
      menuNode.AddText(#13#10);
    end;

  hamberger := menuNode.Add('a');
  hamberger.Attributes['class'] := 'icon_container';
  hamberger.Attributes['id'] := 'icon_container';
  hamberger.Attributes['href'] := 'javascript:void(0)';
  hamberger.Attributes['onclick'] := 'hamburger()';
  node := hamberger.Add('img');
  node.Attributes['id'] := 'menu_icon';
  node.Attributes['src'] := '../Menu/hamburger.svg';
end;

procedure TElement.InsertNormalMenu(menuNode: TDomNode);
var
  last: integer;
  element: TElement;
  node: TDomNode;
  dots :string;
begin

  last := menuNode.Count;
  node := menuNode.Insert(last, 'span');
  node.Attributes['class'] := 'active';
  node.AddText(ShortFileName);
  element := parent;
  while element <> Project do
  begin
    if element.FileName <> '' then
    begin
      node := menuNode.Insert(last, 'a');
      node.Attributes['class'] := 'ancestor';
      dots := AddLevels(FolderLevel - element.FolderLevel);
      node.Attributes['href'] := dots + ExtractFilename(element.FileName);
      node.AddText(element.ShortFileName);
    end;
    element := element.Parent;
  end;
end;

procedure TElement.InsertHeading(Body: TDomNode);
var
  S: string;
begin
  if (Parent is TClassElement) or (Parent is TInterfaceElement) then
    S := Parent.ShortFileName + '.' + ShortFileName else
    S := TitleText;
  Body.Insert(0, 'h1').AddText(S);
end;

class function TElement.IsTopic: Boolean;
begin
  Result := False;
end;

function TElement.LinkTo(Target: TElement): string;
begin
  Result := LinkTo(Target.FileName);
end;

function TElement.LinkTo(const Target: string): string;
begin
  Result := Format('<a href="%s">%s</a>', [PathTo(Target), GetLinkName(Target)]);
end;

function TElement.PathTo(Target: TElement): string;
begin
  Result := PathTo(Target.FileName);
end;

function TElement.PathTo(const Target: string): string;
var
  S: string;
begin
  if Target = '' then raise Exception.Create(RCStrInvalidTarget);
    S := StringReplace(Target, '/', '\', [rfReplaceAll]);
  S := ExtractRelativePath(FileName, S);
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

procedure TElement.ProcessBody(Body: TDomNode; const Anchors, SeeAlso: TStringList);
var
  i, menuIdx: Integer;
  isTopLevel: Boolean;
begin
  InsertHeading(Body);
  AddSeeAlso(Body, SeeAlso);

  menuIdx := -1;
  isTopLevel := FolderLevel = Project.FolderLevel;
  if Self <> Project then
  begin
    Project.Index.Add(ShortFileName, FileName);
    for I := 0 to Anchors.Count - 1 do
      Project.Index.Add(Anchors[I], FileName + '#' + Anchors[I]);

    if isTopLevel then  //get menu index
    begin
      for I := 0 to High(Project.MenuTopItems) do
        if project.MenuTopItems[i].shortFileName = ShortFileName then
        begin
          menuIdx := i;
          break;
        end;
    end;
  end;
  InsertMenu(Body, menuIdx, isTopLevel);
end;


procedure InjectHeader(head, includes: TDomNode; level: integer);
var
  s, dots: string;
  node: TDomNode;
begin
  if not Assigned(includes) then Exit;
  dots := AddLevels(level);

  node := includes.FirstChild;
  while assigned(node) do
  begin
    if node.Name = 'script' then
    begin
      s := Format('<script type="%s" src="%s%s"></script>',
        [node.Attributes['type'], dots, node.Attributes['src']]);
      head.AddText(s);
    end
    else if node.Name = 'link' then
    begin
      s := Format('<link rel="%s" type="%s" href="%s%s"/>',
        [node.Attributes['rel'], node.Attributes['type'],
        dots, node.Attributes['href']]);
      head.AddText(s);
    end else
      head.AddText(node.AsString);

    node := node.NextSibling;
  end;
end;

procedure TElement.Transform;
var
  Dom: TDomDocument;
  Head, Title, Body: TDomNode;
  DestFile, DestPath, S: string;
  SeeAlsoNodes, ImgNodes: TDomNodeList;
  Anchors, SeeAlso: TStringList;
  I: Integer;
begin
  Dom := TDomDocument.Create;
  try
    Dom.LoadFromFile(FileName);
    Head := Dom.FindNode('head', True);
    if not Assigned(Head) then
      raise Exception.CreateFmt(RCStrFileSIsInvalidHead, [FileName]);

    Title := Head.FindNode('title', False);
    if not Assigned(Title) then
    begin
      Title := Head.Add('title');
      Title.AddText(ShortFileName)
    end
    else if Title.Count = 0 then
      Title.AddText(ShortFileName);
    TitleText := Title.FirstChild.Value;

    InjectHeader(head, project.HeadInclude,
      FolderLevel - Project.FolderLevel +1);

    Body := Dom.FindNode('body', True);
    if not Assigned(Body) then
      raise Exception.CreateFmt(RCStrFileSIsInvalidBody, [FileName]);

    if FolderLevel = Project.FolderLevel then
      Body.Attributes.Add('onload', 'OnLoadEvent()');

    // fix up links and anchors and PROCESS
    Anchors := TStringList.Create;
    SeeAlso := TStringList.Create;
    try
      Anchors.Sorted := True;
      Anchors.Duplicates := dupIgnore;
      SeeAlso.Sorted := True;
      SeeAlso.Duplicates := dupIgnore;

      SeeAlsoNodes := Body.FindNodes('a', True);
      try
        SetCurrentDir(Folder);
        for I := 0 to SeeAlsoNodes.Count - 1 do
          with SeeAlsoNodes.Items[I] do
          begin
            S := Attributes['name'];
            if Length(S) > 0 then
            begin
              S := StringReplace(S, '%20', ' ', [rfReplaceAll]);
              Anchors.Add(S);
            end;
            S := Attributes['href'];
            if (Length(S) > 0) and (S[1] <> '#') then
            begin
              if Pos('http', S) = 0 then
              begin
                S := StringReplace(S, '%20', ' ', [rfReplaceAll]);
                S := ExpandFileName(S);
                if not QuickCheckLink(S) then
                  AddBrokenLink(S);
                SeeAlso.Add(S);
              end;
            end;
          end;

        SeeAlso.Sorted := False;
        SeeAlso.CustomSort(CompareLinks);

        ProcessBody(Body, Anchors, SeeAlso);

        //finally, add copyright info etc.
        Body.AddText(Project.BodyIncludes);

      finally
        SeeAlsoNodes.Free;
      end;

      if Project.CheckBrokenImages then
      begin
        ImgNodes := Body.FindNodes('img', True);
        try
          SetCurrentDir(Folder);
          for I := 0 to ImgNodes.Count - 1 do
            with ImgNodes.Items[I] do
            begin
              S := Attributes['src'];
              if pos('../Menu/', S) < 1 then
                Project.imageNames.Add(ExpandFileName(S));
            end;
        finally
          ImgNodes.Free;
        end;
      end;

    finally
      SeeAlso.Free;
      Anchors.Free;
    end;


    DestFile := GetDstFile;

    DestPath := ExtractFilePath(DestFile);
    if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(DestPath) then
      {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}ForceDirectories(DestPath);
    Dom.SaveToFile(DestFile);
  finally
    Dom.Free;
  end;
end;

{ TNodeElement }

function TNodeElement.Count: Integer;
begin
  Result := Children.Count;
end;

constructor TNodeElement.Create(AParent: TElement; const APath: string);
begin
  inherited;
  Children := TObjectList.Create;
end;

constructor TNodeElement.CreateRoot(AParent: TElement);
begin
  Parent := AParent;
  if AParent <> nil then Project := AParent.Project;
  Children := TObjectList.Create;
end;

destructor TNodeElement.Destroy;
begin
  Children.Free;
  inherited;
end;

function TNodeElement.GetChild(Index: Integer): TNodeElement;
begin
  Result := TNodeElement(Children[Index]);
end;

procedure TNodeElement.Read;
var
  Dirs, Files: TStringList;
  I: Integer;
  N: TNodeElement;
begin
  Dirs := GetDirList(Folder, '*.*');
  Files := GetFileList(Folder, '*.htm*');
  try
    if (Dirs.Count = 0) and (Files.Count = 0) then Exit;

    if FileName <> '' then
    begin
      I := Files.IndexOf(FileName);
      if I >= 0 then Files.Delete(I);
    end;
    for I := 0 to Files.Count - 1 do
    begin
      N := TNodeElement.Create(Self, Files[I]);
      Children.Add(N);
    end;
    for I := 0 to Dirs.Count - 1 do
    begin
      N := TNodeElement.Create(Self, Dirs[I]);
      Children.Add(N);
      N.Read;
    end;
  finally
    Files.Free;
    Dirs.Free;
  end;

  if (FileName <> '') and (Count > 0) then
    SetOrder(GetMeta(FileName, 'Order'));
end;

procedure TNodeElement.SetOrder(S: string);
var
  L: TList;
  I, J: Integer;
  Order: TStringList;
begin
  S := Trim(S);
  Order := TStringList.Create;
  try
    if Length(S) > 0 then
    begin
      S := StringReplace(S, ',', '","', [rfReplaceAll]);
      S := StringReplace(S, '" ', '"', [rfReplaceAll]);
      S := '"' + S + '"';
      Order.CommaText := S;
    end;

    if Order.Count <= 1 then Exit;

    L := TList.Create;
    try
      for I := 0 to Count - 1 do L.Add(Child[I]);
      Children.OwnsObjects := False;             //!! IMPORTANT
      Children.Clear;

      for I := 0 to Order.Count - 1 do
      begin
        J := 0;
        while J < L.Count do
          if SameText(TNodeElement(L[J]).ShortFileName, Order[I]) then
          begin
            Children.Add(TNodeElement(L[J]));
            L.Delete(J);
          end
          else Inc(J);
      end;

      for I := 0 to L.Count - 1 do
        Children.Add(TNodeElement(L[I]));

      Children.OwnsObjects := True;              //!! IMPORTANT
    finally
      L.Free;
    end;
  finally
    Order.Free;
  end;
end;

{ TGroupElement }

procedure TGroupElement.Clear;
var
  I: Integer;
begin
  for I := 0 to ChildLists.Count - 1 do TElements(ChildLists[I]).Clear;
end;

constructor TGroupElement.Create(AParent: TElement; const APath: string);
begin
  inherited;
  ChildLists := TList.Create;
end;

destructor TGroupElement.Destroy;
var
  I: Integer;
begin
  for I := ChildLists.Count - 1 downto 0 do
    TElements(ChildLists[I]).Free;
  ChildLists.Free;
  inherited;
end;

function TGroupElement.Next: TElement;
var
  I: Integer;
begin
  Result := nil;
  if ListRef <> nil then
  begin
    I := ListRef.IndexOf(Self);
    if I < ListRef.Count - 1 then Result := ListRef[I + 1];
  end;
end;

function TGroupElement.Prev: TElement;
var
  I: Integer;
begin
  Result := nil;
  if ListRef <> nil then
  begin
    I := ListRef.IndexOf(Self);
    if I > 0 then Result := ListRef[I - 1];
  end;
end;

procedure TGroupElement.Read;
var
  I: Integer;
begin
  for I := 0 to ChildLists.Count - 1 do TElements(ChildLists[I]).Read;
end;

procedure TGroupElement.RegList(AList: TElements);
begin
  ChildLists.Add(AList);
end;

{ TTopicElement }

class function TTopicElement.IsTopic: Boolean;
begin
  Result := True;
end;

procedure TTopicElement.Read;
var
  L: Integer;
begin
  inherited;
  L := Length(Project.Topics);
  SetLength(Project.Topics, L + 1);
  Project.Topics[L] := Self;
end;

{ TClassElement }

constructor TClassElement.Create(AParent: TElement; const APath: string);
begin
  inherited;
  Fields := TElements.Create(Self, TTopicElement, 'Fields');
  Properties := TElements.Create(Self, TTopicElement, 'Properties');
  Methods := TElements.Create(Self, TTopicElement, 'Methods, Functions');
  Events := TElements.Create(Self, TTopicElement, 'Events');
  RegList(Fields);
  RegList(Methods);
  RegList(Properties);
  RegList(Events);
end;

procedure TClassElement.ProcessBody(Body: TDomNode; const Anchors, Links: TStringList);
type
  TColumns = set of (ctFields, ctProperties, ctMethods, ctEvents);
var
  ClassLinks, AL: TClassLinks;
  Ancestors: TObjectList;
  AE: TClassElement;
  Columns: TColumns;
  ColCount, cc: Integer;
  T: TDomNode;
  I, J: Integer;
  S: string;

  procedure AddMembers(ClassLinks: TClassLinks);
  var
    I: Integer;

    function CheckLink(const Target: string): string;
    var
      P: Integer;
    begin
      P := Links.IndexOf(Target);
      if P >= 0 then Links.Delete(P);
      Result := LinkTo(Target);
    end;

  begin
    for I := 0 to ClassLinks.TotalCount - 1 do
      with T.Add('tr') do
      begin
        if ctFields in Columns then with Add('td') do
          if I < ClassLinks.Fields.Count then
            AddParse(CheckLink(ClassLinks.Fields[I]));

        if ctMethods in Columns then with Add('td') do
          if I < ClassLinks.Methods.Count then
            AddParse(CheckLink(ClassLinks.Methods[I]));

        if ctProperties in Columns then with Add('td') do
          if I < ClassLinks.Properties.Count then
            AddParse(CheckLink(ClassLinks.Properties[I]));

        if ctEvents in Columns then with Add('td') do
          if I < ClassLinks.Events.Count then
            AddParse(CheckLink(ClassLinks.Events[I]));
      end;
  end;

  procedure AddToIndex(Elements: TElements);
  var
    I: Integer;
  begin
    Assert(Assigned(Elements));
    for I := 0 to Elements.Count - 1 do
    begin
      Assert(Assigned(Elements[I]));

      Project.Index.AddMember(
        ShortFileName,
        Elements[I].ShortFileName,
        ShortFileName + '.' + Elements[I].ShortFileName,
        Elements[I].FileName);

      Project.Index.AddMember(
        Elements[I].ShortFileName,
        ShortFileName,
        ShortFileName + '.' + Elements[I].ShortFileName,
        Elements[I].FileName);
    end;
  end;

begin
  ClassLinks := TClassLinks.Create;
  Ancestors := TObjectList.Create;
  try
    ClassLinks.Get(Self);

    // get member links of ancestors
    AE := Ancestor;
    while AE <> nil do
    begin
      AL := TClassLinks.Create;
      Ancestors.Add(AL);
      AL.Get(AE);
      AE := AE.Ancestor;
    end;

    // add hierarcy section
    if AncestorName <> '' then
    begin
      S := AncestorName;
      AE := Ancestor;
      while True do
      begin
        with Body.Insert(0, 'p') do
        begin
          // Attributes['id'] := 'Auto'; // must be unique!
          Attributes['class'] := 'Hierarchy';
          if (AE <> nil) and (AE.FileName <> '') then AddParse(LinkTo(AE))
          else AddText(S);
        end;
        if AE = nil then Break;
        S := AE.AncestorName;
        AE := AE.Ancestor;
        with Body.Insert(0, 'p') do
        begin
          // Attributes['id'] := 'Auto'; // must be unique!
          Attributes['class'] := 'Hierarchy';
          AddText('&nbsp;&nbsp;&nbsp;|');
        end;
      end;
      with Body.Insert(0, 'h2') do
      begin
        // Attributes['id'] := 'Auto'; // must be unique!
        AddText('Hierarchy');
      end;
    end;


    // remove overrided duplicates
    for I := 0 to Ancestors.Count - 1 do
      ClassLinks.RemoveDuplicatesFrom(TClassLinks(Ancestors[I]));

    for I := 0 to Ancestors.Count - 2 do
      for J := I + 1 to Ancestors.Count - 1 do
        TClassLinks(Ancestors[I]).RemoveDuplicatesFrom(TClassLinks(Ancestors[J]));

    if (ClassLinks.TotalCount > 0) or (Ancestors.Count > 0) then
    begin
      // get non-empty member columns
      ColCount := 0;

      I := ClassLinks.Fields.Count;
      for J := 0 to Ancestors.Count - 1 do
        Inc(I, TClassLinks(Ancestors.Items[J]).Fields.Count);
      if I > 0 then
      begin
        Include(Columns, ctFields);
        Inc(ColCount);
      end;

      I := ClassLinks.Methods.Count;
      for J := 0 to Ancestors.Count - 1 do
        Inc(I, TClassLinks(Ancestors.Items[J]).Methods.Count);
      if I > 0 then
      begin
        Include(Columns, ctMethods);
        Inc(ColCount);
      end;

      I := ClassLinks.Properties.Count;
      for J := 0 to Ancestors.Count - 1 do
        Inc(I, TClassLinks(Ancestors.Items[J]).Properties.Count);
      if I > 0 then
      begin
        Include(Columns, ctProperties);
        Inc(ColCount);
      end;

      I := ClassLinks.Events.Count;
      for J := 0 to Ancestors.Count - 1 do
        Inc(I, TClassLinks(Ancestors.Items[J]).Events.Count);
      if I > 0 then
      begin
        Include(Columns, ctEvents);
        Inc(ColCount);
      end;

      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Reference';
        AddText('Reference');
      end;

      // Unfortunately class member lists (that usually include
      // ancestor members) really do need to be in a TABLE.
      // But if the table scrolls (see default.css), then this
      // still works  pretty well on mobile devices.

      cc := 0;
      T := Body.Add('p');
      T := T.Add('table');
      with T do
      begin
        Attributes['class'] := 'autoTbl';

        // heading row
        with Add('tr') do
        begin
          if ctFields in Columns then
            with Add('th') do
            begin
              inc(cc);
              if cc = ColCount then
              Attributes['style'] := 'width: 100%;';
              AddText('Fields');
            end;

          if ctMethods in Columns then
          begin
            with Add('th') do
            begin
              inc(cc);
              if cc = ColCount then
              Attributes['style'] := 'width: 100%;';
              AddText('Methods');
            end;
          end;

          if ctProperties in Columns then
            with Add('th') do
            begin
              inc(cc);
              if cc = ColCount then
              Attributes['style'] := 'width: 100%;';
              AddText('Properties');
            end;

          if ctEvents in Columns then
            with Add('th') do
            begin
              Attributes['style'] := 'width: 100%;';
              AddText('Events');
            end;
        end;

        if ClassLinks.TotalCount > 0 then
        begin
          if Ancestors.Count > 0 then
            with Add('tr').Add('td') do
            begin
              Attributes['colspan'] := IntToStr(ColCount);
              Attributes['class'] := 'ancestor';
              AddText('In ' + GetFolderName(Folder) + ':');
            end;
          AddMembers(ClassLinks);
        end;

        for I := 0 to Ancestors.Count - 1 do
          if TClassLinks(Ancestors[I]).TotalCount > 0 then
          begin
            with Add('tr').Add('td') do
            begin
              Attributes['colspan'] := IntToStr(ColCount);
              Attributes['class'] := 'ancestor';
              AddText('In ' + TClassLinks(Ancestors[I]).ShortFileName + ':');
            end;
            AddMembers(TClassLinks(Ancestors[I]));
          end;
      end;
    end;
  finally
    Ancestors.Free;
    ClassLinks.Free;
  end;

  // default stuff
  inherited;

  // index
  AddToIndex(Fields);
  AddToIndex(Methods);
  AddToIndex(Properties);
  AddToIndex(Events);
end;

procedure TClassElement.Read;
var
  L: Integer;
begin
  inherited;
  if FileName <> '' then AncestorName := GetMeta(FileName, 'Ancestor');
  L := Length(Project.Classes);
  SetLength(Project.Classes, L + 1);
  Project.Classes[L] := Self;
end;

{ TInterfaceElement }

procedure TInterfaceElement.Read;
var
  L: Integer;
  I: Integer;
begin
  for I := 0 to ChildLists.Count - 1 do TElements(ChildLists[I]).Read;
  if FileName <> '' then AncestorName := GetMeta(FileName, 'Ancestor');
  L := Length(Project.Interfaces);
  SetLength(Project.Interfaces, L + 1);
  Project.Interfaces[L] := Self;
end;

{ TUnitElement }

constructor TUnitElement.Create(AParent: TElement; const APath: string);
begin
  inherited;
  Interfaces := TElements.Create(Self, TInterfaceElement, 'Interfaces');
  Classes := TElements.Create(Self, TClassElement, 'Classes');
  Functions := TElements.Create(Self, TTopicElement, 'Routines, Functions');
  Types := TElements.Create(Self, TTopicElement, 'Types');
  Globals := TElements.Create(Self, TTopicElement, 'Globals');
  Variables := TElements.Create(Self, TTopicElement, 'Variables');
  Constants := TElements.Create(Self, TTopicElement, 'Constants');
  RegList(Types);
  RegList(Functions);
  RegList(Globals);
  RegList(Constants);
  RegList(Variables);
  RegList(Interfaces);
  RegList(Classes);
end;

procedure TUnitElement.ProcessBody(Body: TDomNode; const Anchors, Links: TStringList);
var
  N, I: Integer;
  Columns: TList;
  unitDiv: TDomNode;
begin
  N := Types.Count;
  if Interfaces.Count > N then N := Interfaces.Count;
  if Classes.Count > N then N := Classes.Count;
  if Functions.Count > N then N := Functions.Count;
  if Globals.Count > N then N := Globals.Count;
  if Variables.Count > N then N := Variables.Count;
  if Constants.Count > N then N := Constants.Count;

  if N > 0 then
  begin
    with Body.Add('h2') do
    begin
      Attributes['id'] := 'Auto-Contents';
      AddText('Contents');
    end;

    Columns := TList.Create;
    try
      // get non-empty columns
      if Types.Count > 0 then Columns.Add(Types);
      if Interfaces.Count > 0 then Columns.Add(Interfaces);
      if Classes.Count > 0 then Columns.Add(Classes);
      if Functions.Count > 0 then Columns.Add(Functions);
      if Globals.Count > 0 then Columns.Add(Globals);
      if Variables.Count > 0 then Columns.Add(Variables);
      if Constants.Count > 0 then Columns.Add(Constants);

      unitDiv := Body.Add('p').Add('div');
      for I := 0 to Columns.Count - 1 do
        AddUnitTable(unitDiv, TElements(Columns[I]));

    finally
      Columns.Free;
    end;
  end;

  inherited;
end;

{ TProject }

function CompareClasses(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TClassEntry(Item1).Element.ShortFileName,
    TClassEntry(Item2).Element.ShortFileName);
end;

procedure TProject.BuildHierarchy;
var
  I, J: Integer;
  AName: string;
begin
  for I := 0 to High(Classes) do
  begin
    AName := Classes[I].AncestorName;
    if AName <> '' then
      for J := 0 to High(Classes) do
        if SameText(Classes[J].ShortFileName, AName) then
        begin
          Classes[I].Ancestor := Classes[J];
          if Classes[J] = Classes[I] then
            raise Exception.Create(RCStrLoopedHierarchyRef);
          Break;
        end;
  end;

  for I := 0 to High(Interfaces) do
  begin
    AName := Interfaces[I].AncestorName;
    if AName <> '' then
      for J := 0 to High(Interfaces) do
        if SameText(Interfaces[J].AncestorName, AName) then
        begin
          Interfaces[I].Ancestor := Interfaces[J];
          Break;
        end;
  end;
end;

constructor TProject.Create(AParent: TElement; const APath: string);
begin
  inherited;
  Project := Self;
  Files := TStringList.Create;
  Index := TIndex.Create;
  Units := TElements.Create(Self, TUnitElement, 'Units');
  Root := TNodeElement.CreateRoot(Self);
  HeadIncludes := TDomDocument.Create;

  RegList(Units);
  if (FileName <> '') then
  begin
    Files.AddObject(FileName, Self);
    FolderLevel := GetFolderLevel(FileName);
  end;

  BrokenLinks := TStringList.create;

  imageNames  := TStringList.Create;
  imageNames.Sorted := True;
  imageNames.Duplicates := dupIgnore;
end;

destructor TProject.Destroy;
begin
  imageNames.Free;
  BrokenLinks.Free;
  Root.Free;
  Index.Free;
  Files.Free;
  HeadIncludes.Free;
  Topics := nil;
  Interfaces := nil;
  Classes := nil;
  Units := nil;
  inherited;
end;

function TProject.FindClass(const AName: string): TClassElement;
var
  I: Integer;
begin
  for I := 0 to High(Classes) do
  begin
    Result := Classes[I];
    if SameText(Result.ShortFileName, AName) then Exit;
  end;
  Result := nil;
end;

function TProject.FindInterface(const AName: string): TInterfaceElement;
var
  I: Integer;
begin
  for I := 0 to High(Interfaces) do
  begin
    Result := Interfaces[I];
    if SameText(Result.ShortFileName, AName) then Exit;
  end;
  Result := nil;
end;

procedure TProject.ProcessBody(Body: TDomNode; const Anchors, Links: TStringList);
var
  Elems: TElements;
  I, J: Integer;

  function AddIndexTable(const displayName: string): TDomNode;
  var
    I: Integer;
  begin
    Elems.Sort(CompareDisplayNames);
    Result := Body.Add('p');
    Result := Result.Add('div');
    with Result do
    begin
      Attributes['class'] := 'autoTbl';
      with Add('div') do
        with Add('b') do AddText(displayName);
      with Add('div') do
      begin
        for I := 0 to Elems.Count - 1 do
        begin
          with Add('span') do
          begin
            with Add('a') do
            begin
              Attributes['href'] := PathTo(Elems[I].FileName);
              AddText(GetLinkName(Elems[I].FileName));
            end;
          end;
        end;
      end;
    end;
    Body.AddText(#13#10#13#10);
  end;

begin
  Elems := TElements.Create(nil, TTopicElement, Folder);
  try
    //this is the start of the Index links
    //that are listed **after** any text found in _Body.htm in 'source'

    Elems.OwnsObjects := false;                  //!! IMPORTANT

    // units
    for I := 0 to Units.Count - 1 do
      Elems.Add(TUnitElement(Units[I]));
    if Elems.Count > 0 then
    begin
      Elems.Sort(CompareDisplayNames);
      AddIndexTable('Units');
      Elems.Clear;
    end;

    // classes
    if Length(Classes) > 0 then
    begin
      for I := 0 to High(Classes) do
        Elems.Add(Classes[I]);
      if Elems.Count > 0 then
      begin
        AddIndexTable('Classes');
        Elems.Clear;
      end;
    end;

    if Length(Interfaces) > 0 then
    begin

      for I := 0 to High(Interfaces) do
        Elems.Add(Interfaces[I]);
      if Elems.Count > 0 then
      begin
        AddIndexTable('Interfaces');
        Elems.Clear;
      end;
    end;

    // types
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Types.Count - 1 do Elems.Add(Types[J]);
    if Elems.Count > 0 then
    begin
      AddIndexTable(dataTypesCaption);
      Elems.Clear;
    end;

    // globals
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Globals.Count - 1 do Elems.Add(Globals[J]);
    if Elems.Count > 0 then
    begin
      AddIndexTable('Globals');
      Elems.Clear;
    end;

    // variables
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Variables.Count - 1 do Elems.Add(Variables[J]);
    if Elems.Count > 0 then
    begin
      AddIndexTable('Variables');
      Elems.Clear;
    end;

    // constants
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Constants.Count - 1 do Elems.Add(Constants[J]);
    if Elems.Count > 0 then
    begin
      AddIndexTable('Constants');
      Elems.Clear;
    end;

    // Functions
    for I := 0 to Units.Count - 1 do
      with TUnitElement(Units[I]) do
        for J := 0 to Functions.Count - 1 do
        begin
          Elems.Add(Functions[J]);
        end;

    if Elems.Count > 0 then
    begin
      AddIndexTable(staticFuncsCaption);
      Elems.Clear;
    end;

  finally
    Elems.Free;
  end;

  //inserts the library's desciption <H1>
  InsertHeading(Body);
  InsertMenu(Body, -2, true);
end;

procedure TProject.Read;
var
  Files, Dirs: TStringList;
  I, J: Integer;
  NE: TNodeElement;
begin
  inherited;
  Dirs := GetDirList(Folder, '*.*');
  Files := GetFileList(Folder, '*.htm*');
  try
    if Dirs.Count > 0 then
    begin
      for I := 0 to ChildLists.Count - 1 do
      begin
        J := Dirs.IndexOf(Folder + '\' + TElements(ChildLists[I]).groupName);
        if J >= 0 then
          Dirs.Delete(J);
      end;
      for I := Dirs.Count - 1 downto 0 do
        if SameText(Dirs[I], 'cvs') then
          Dirs.Delete(I);

      for I := 0 to Dirs.Count - 1 do
      begin
        NE := TNodeElement.Create(Self, Dirs[I]);
        Root.Children.Add(NE);
        NE.Read;
      end;
    end;

    for I := 0 to Files.Count - 1 do
    begin
      if ExtractFileName(Files[I]) = '_Body.htm' then Continue;
      NE := TNodeElement.Create(Self, Files[I]);
      Root.Children.Add(NE);
    end;
  finally
    Files.Free;
    Dirs.Free;
  end;

  Root.SetOrder(GetMeta(Folder + '\' + '_Body.htm', 'Order'));
end;

{ TIndexEntry }

constructor TIndexEntry.Create;
begin
  Targets := TStringList.Create;
  Members := TIndex.Create;
end;

destructor TIndexEntry.Destroy;
begin
  Members.Free;
  Targets.Free;
  inherited;
end;

{ TIndex }

function TIndex.Add(const AKeyword, ATarget: string): TIndexEntry;
var
  I: Integer;
begin
  Result := nil;
  if (AKeyword = '') then Exit;

  for I := 0 to Count - 1 do
  begin
    if Items[I].Keyword = AKeyword then
    begin
      Result := Items[I];
      Break;
    end;
  end;

  if Result = nil then
  begin
    Result := TIndexEntry.Create;
    inherited Add(Result);
    Result.Keyword := AKeyword;
  end;

  if ATarget <> '' then Result.Targets.Add(ATarget);
end;

procedure TIndex.AddMember(const AKeyword, AKeyword2, AName, ATarget: string);
var
  I: Integer;
  E: TIndexEntry;
begin
  E := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Keyword = AKeyword then
    begin
      E := Items[I];
      Break;
    end;
  end;
  if E = nil then
    E := Add(AKeyword, '');

  Assert(Assigned(E));
  E.Members.Add(AKeyword2, ATarget).Name := AName;
end;

function TIndex.GetItem(Index: Integer): TIndexEntry;
begin
  Result := TIndexEntry(inherited Items[Index]);
end;

procedure TIndex.SetItem(Index: Integer; Value: TIndexEntry);
begin
  inherited Items[Index] := Value;
end;

{ TClassEntry }

constructor TClassEntry.Create(AElem: TElement);
begin
  Element := AElem;
  Children := TObjectList.Create;
end;

destructor TClassEntry.Destroy;
begin
  Children.Free;
  inherited;
end;

{$WARN SYMBOL_PLATFORM OFF}

end.
