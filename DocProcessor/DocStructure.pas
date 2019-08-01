unit DocStructure;

{$I DocProcessor.inc}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, SysUtils, Contnrs, Windows, FileCtrl, StrUtils, SimpleDOM;

const
  cColumnCount = 5;
var
  IncludeAlphabetClasses: boolean = true;
  CheckForBrokenLinks: boolean = true;

type
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
    SubDirName: string;
    Owner: TElement;
    constructor Create(AOwner: TElement; AElemClass: TGroupElementClass; const ADirName: string);
    procedure Add(AElement: TGroupElement);
    procedure Read;
    property Items[Index: Integer]: TGroupElement read GetItems write SetItems; default;
  end;

  TElement = class
  protected
    class function IsTopic: Boolean; virtual;
    procedure AddSeeAlso(Body: TDomNode; Links: TStringList);
    procedure InsertBanner(Body: TDomNode);
    procedure InsertHeading(Body: TDomNode);
  public
    FileName: string;
    Folder: string;
    DisplayName: string;
    Parent: TElement;
    Project: TProject;
    constructor Create(AParent: TElement; const APath: string); virtual;
    function GetDstFolder: string;
    function GetDstFile: string;
    function LinkTo(Target: TElement): string; overload;
    function LinkTo(const Target: string): string; overload;
    function PathTo(Target: TElement): string; overload;
    function PathTo(const Target: string): string; overload;
    procedure Process(Head, Body: TDomNode; const Anchors, Links: TStringList); virtual;
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
    procedure Process(Head, Body: TDomNode; const Anchors, Links: TStringList); override;
    procedure Read; override;
  end;

  TInterfaceElement = class(TClassElement)
    procedure Read; override;
  end;

  TUnitElement = class(TGroupElement)
    Interfaces: TElements;
    Classes: TElements;
    Records: TElements;
    Routines: TElements;
    Types: TElements;
    Globals: TElements;
    Variables: TElements;
    Constants: TElements;
    constructor Create(AParent: TElement; const APath: string); override;
    procedure Process(Head, Body: TDomNode; const Anchors, Links: TStringList); override;
  end;

  TCaptionUrlList = class
  private
    CaptionList: TStringList;
    UrlList: TStringList;
    function GetCaption(index: Integer): string;
    function GetUrl(index: Integer): string;
  public
    procedure Add(const aCaption, aUrl: string);
    procedure Clear;
    function Count: Integer;
    constructor Create;
    destructor Destroy; override;
    property Caption[index: Integer]: string read GetCaption;
    property Url[index: Integer]: string read GetUrl;
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
  protected
    procedure AddClasses(const Classes: TClassElementArray; Body: TDomNode);
  public
    Units: TElements;
    Root: TNodeElement;

    Classes: array of TClassElement;
    Interfaces: array of TInterfaceElement;
    Topics: array of TTopicElement;

    Files: TStringList;
    FileElements: array of TTopicElement;
    DestinationFolder: string;
    ImageFolder: string;
    ScriptFolder: string;
    StylesFolder: string;

    HeadSectionTemplate: string;
    HeadSectionTemplateDOM: TDomDocument;
    BodySectionTemplate: string;
    BodySectionTemplateDOM: TDomDocument;

    BrokenLinks: TStringList;

    Index: TIndex;
    constructor Create(AParent: TElement; const APath: string); override;
    destructor Destroy; override;
    procedure BuildHierarchy;
    procedure ParseMenuData(CuList: TCaptionUrlList);
    procedure BuildIndex(const IndexFile: string);
    procedure BuildTOC(const TocFile: string);
    function FindClass(const AName: string): TClassElement;
    function FindInterface(const AName: string): TInterfaceElement;
    procedure Process(Head, Body: TDomNode; const Anchors, Links: TStringList); override;
    procedure Read; override;
  end;

  TClassEntry = class
    Element: TElement;
    Children: TObjectList;
    Parent: TClassEntry;
    constructor Create(AElem: TElement);
    destructor Destroy; override;
  end;

var
  EnglishFormatSettings: TFormatSettings;
  VersionString: string;

implementation

uses
  Utils;

resourcestring
  RCStrInvalidNodeElement = 'Invalid node element';
  RCStrInvalidElementPath = 'Invalid element path';
  RCStrTCaptionUrlListRan = 'TCaptionUrlList: range error';
  RCStrCantGetDestinationFileName = 'Can''t get destination filename';
  RCStrCantGetDestinationFolder = 'Can''t get destination folder';
  RCStrInvalidTarget = 'Invalid target';
  RCStrFileSIsInvalidHead = 'File ''%s'' is invalid (missing HEAD element)';
  RCStrFileSIsInvalidLink = 'File ''%s'' is invalid (missing LINK element)';
  RCStrFileSIsInvalidBody = 'File ''%s'' is invalid (missing BODY element)';
  RCStrLoopedHierarchyRef = 'Looped hierarchy Ref';

procedure TCaptionUrlList.Add(const aCaption, aUrl: string);
begin
  CaptionList.Add(aCaption);
  UrlList.Add(aUrl);
end;

procedure TCaptionUrlList.Clear;
begin
  CaptionList.Clear;
  UrlList.Clear;
end;

function TCaptionUrlList.Count;
begin
  Result := CaptionList.Count;
end;

function TCaptionUrlList.GetCaption(index: Integer): string;
begin
  if (index < 0) or (index >= CaptionList.Count) then
    raise Exception.Create(RCStrTCaptionUrlListRan);
  Result := CaptionList[index];
end;

function TCaptionUrlList.GetUrl(index: Integer): string;
begin
  if (index < 0) or (index >= UrlList.Count) then
    raise Exception.Create(RCStrTCaptionUrlListRan);
  Result := UrlList[index];
end;

constructor TCaptionUrlList.Create;
begin
  CaptionList := TStringList.Create;
  UrlList := TStringList.Create;
end;

destructor TCaptionUrlList.Destroy;
begin
  CaptionList.Free;
  UrlList.Free;
end;


type
  TClassLinks = class
    DisplayName: string;
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
  DisplayName := Src.DisplayName;
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

constructor TElements.Create(AOwner: TElement; AElemClass: TGroupElementClass; const ADirName: string);
begin
  OwnsObjects := true;                           //!! IMPORTANT
  Owner := AOwner;
  ElemClass := AElemClass;
  SubDirName := ADirName;
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
  Folder := Owner.Folder + '\' + SubDirName;
  if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(Folder) then Exit;
  Listing := nil;
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
    Sort(CompareElements);
  finally
    Listing.Free;
  end;
end;

procedure TElements.SetItems(Index: Integer; Value: TGroupElement);
begin
  inherited Items[Index] := Value;
end;

{ TElement }

procedure TElement.AddSeeAlso(Body: TDomNode; Links: TStringList);
var
  I, J: Integer;
  S, PrevS: string;
  E: TElement;
begin
  if Links.Count = 0 then Exit;

  with Body.Add('h2') do
  begin
    Attributes['id'] := 'Auto-SeeAlso';
    AddText('See Also');
  end;
  with Body.Add('p') do
  begin
    // Attributes['id'] := 'Auto'; must be unique!
    Attributes['class'] := 'Body';

    Links.CustomSort(CompareLinks);
    PrevS := '';
    for I := 0 to Links.Count - 1 do
    begin
      S := Links[I];
      J := Pos('#', S);
      if J > 0 then
        SetLength(S, J -1);

      if CheckForBrokenLinks and not FileExists(S) and (Pos('mailto:', S) = 0) then
      begin
        Project.BrokenLinks.Add('  ' + S + #13#10);
        Project.BrokenLinks.Add('    in ' + Self.FileName + #13#10);
      end;

      E := nil;
      if Project <> nil then
        for J := 0 to Project.Files.Count - 1 do
          if Project.Files[J] = S then
          begin
            E := TElement(Project.Files.Objects[J]);
            Break;
          end;

      if (E <> nil) and (E.Parent <> nil) and (E.Parent is TClassElement) and (E.Parent <> Self.Parent) then
      begin
        S := Format('<a href="%s">%s</a>',
          [PathTo(S), E.Parent.DisplayName + '.' + GetLinkName(S)]);
      end
      else if (E <> nil) and (E = E.Project) then
      begin
        S := Format('<a href="%s"><b>Home</b></a>', [PathTo(S)]);
      end
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
      DisplayName := FileNameNoExt(APath);
    end
    else if {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(APath) then
    begin
      if FileExists(APath + '\_Body.htm') then
        FileName := APath + '\_Body.htm';
      Folder := APath;
      DisplayName := DirName(APath);
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
    DisplayName := FileNameNoExt(APath);
  end
  else
  begin
    // A path must point to a directory with optional '_Body.htm' in it
    if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(APath) then
      Exception.Create(RCStrInvalidElementPath);
    if FileExists(APath + '\_Body.htm') then
      FileName := APath + '\_Body.htm';
    Folder := APath;
    DisplayName := DirName(APath);
    if DisplayName = '_Home' then DisplayName := 'Home';
  end;

  if (FileName <> '') and (Project <> nil) then
    Project.Files.AddObject(FileName, Self);
end;

function TElement.GetDstFile: string;
begin
  if FileName = '' then
    raise Exception.Create(RCStrCantGetDestinationFileName);
  Result := Copy(FileName, Length(Project.Folder) + 2, 1000);
  Result := Project.DestinationFolder + '\' + Result;
end;

function TElement.GetDstFolder: string;
begin
  if Folder = '' then
    raise Exception.Create(RCStrCantGetDestinationFolder);
  Result := Copy(Folder, Length(Project.Folder) + 1, 1000);
  Result := Project.Folder + '\' + Result;
end;

procedure TElement.InsertBanner(Body: TDomNode);
var
  Banner, TR, TD, A: TDomNode;
  P: TElement;

  procedure AddImage(const AName: string);
  begin
    with A.Add('img') do
    begin
      Attributes['src'] := PathTo(Project.ImageFolder + '/' + AName);
      Attributes['align'] := 'absmiddle';
    end;
  end;

begin
  Banner := Body.Insert(0, 'table');
  Banner.Attributes['class'] := 'Banner';
  Banner.Attributes['cellspacing'] := '0';
  Banner.Attributes['cellpadding'] := '0';
  Banner.Attributes['border'] := '1';
  Banner.Attributes['bordercolorlight'] := '#303080';
  Banner.Attributes['bordercolordark'] := '#7070B0';

  TR := Banner.Add('tr');

  P := Parent;
  while P <> nil do
  begin
    if P.FileName <> '' then
    begin
      TD := TR.Insert(0, 'td');
      TD.Attributes['class'] := 'Banner';
      TD.Attributes['nowrap'] := '';
      A := TD.Add('a');
      with A do
      begin
        Attributes['href'] := PathTo(P);
        Attributes['class'] := 'Banner';
        if P is TUnitElement then AddImage('_Unit.gif')
        else if P is TClassElement then AddImage('_Class.gif')
        else if P is TProject then AddImage('_Home.gif');
        if P is TProject then AddText('Home') else AddText(P.DisplayName);
      end;
    end;
    P := P.Parent;
  end;
  with TR.Add('td') do
  begin
    Attributes['class'] := 'Banner';
    Attributes['width'] := '100%';
    Attributes['align'] := 'right';
    with Add('img') do
    begin
      Attributes['src'] := PathTo(Project.ImageFolder + '/_Project_Logo.gif');
      Attributes['align'] := 'absmiddle';
    end;
  end;
end;

procedure TElement.InsertHeading(Body: TDomNode);
var
  S: string;
begin
  S := DisplayName;
  if (Parent is TClassElement) or (Parent is TInterfaceElement) then
    S := Parent.DisplayName + '.' + S;
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

procedure TElement.Process(Head, Body: TDomNode; const Anchors, Links: TStringList);
var
  I: Integer;
begin
  InsertHeading(Body);
  InsertBanner(Body);
  AddSeeAlso(Body, Links);
  if Self <> Project then
  begin
    Project.Index.Add(DisplayName, FileName);
    for I := 0 to Anchors.Count - 1 do
      Project.Index.Add(Anchors[I], FileName + '#' + Anchors[I]);
  end;
end;

procedure TElement.Transform;
var
  Dom: TDomDocument;
  Head, Title, Body, Link: TDomNode;
  DestFile, DestPath, S: string;
  LinkNodes: TDomNodeList;
  Anchors, Links: TStringList;
  I: Integer;

  procedure InjectSectionTemplate(const NameBegin, NameEnd: string;
    SrcDoc: TDomDocument; DstNode: TDomNode; const RootFolder: string);
  var
    SrcNode: TDomNode;
    SubNode: TDomNode;
    I: Integer;
    Att: TAttribute;

    procedure RewriteLocationAttribute(const Name: string; Node: TDomNode);
    begin
      Att := Node.Attributes.FindItem(Name);
      if Assigned(Att) then
        Att.Value := PathTo(RootFolder + Att.Value);
    end;

    procedure RewriteWithMacros(Node: TDomNode);
    var
      I: Integer;
      ChildNode: TDomNode;
    begin
      for I := 0 to Node.Count - 1 do
      begin
        ChildNode := Node.Items[I];
        ChildNode.Value := StringReplace(ChildNode.Value, '##BuildDate##', FormatDateTime('d-mmmm-yyyy', Now, EnglishFormatSettings), [rfReplaceAll, rfIgnoreCase]);
        ChildNode.Value := StringReplace(ChildNode.Value, '##VersionString##', VersionString, [rfReplaceAll, rfIgnoreCase]);
        // Add new macros here...
        RewriteWithMacros(ChildNode);
      end;
    end;

  begin
    SrcNode := SrcDoc.FindNode(NameBegin, True);
    if Assigned(SrcNode) then
      for I := SrcNode.Count - 1 downto 0 do
      begin
        SubNode := SrcNode.Items[I].Duplicate;
        RewriteLocationAttribute('src', SubNode);
        RewriteLocationAttribute('href', SubNode);
        RewriteWithMacros(SubNode);
        DstNode.InsertNode(0, SubNode);
      end;

    SrcNode := SrcDoc.FindNode(NameEnd, True);
    if Assigned(SrcNode) then
      for I := 0 to SrcNode.Count - 1 do
      begin
        SubNode := SrcNode.Items[I].Duplicate;
        RewriteLocationAttribute('src', SubNode);
        RewriteLocationAttribute('href', SubNode);
        RewriteWithMacros(SubNode);
        DstNode.AddNode(SubNode);
      end;
  end;

begin
  Dom := TDomDocument.Create;
  try
    Dom.LoadFromFile(FileName);
    Head := Dom.FindNode('head', True);
    if not Assigned(Head) then
      raise Exception.CreateFmt(RCStrFileSIsInvalidHead, [FileName]);
    Link := Head.FindNode('link', False);
    if not Assigned(Link) then
      raise Exception.CreateFmt(RCStrFileSIsInvalidLink, [FileName]);
    Link.Attributes['href'] := StringReplace(Link.Attributes['href'], '\', '/', [rfReplaceAll]);
    Title := Head.FindNode('title', False);
    Title.Clear;
    Title.AddText(DisplayName);
    Body := Dom.FindNode('body', True);
    if not Assigned(Body) then
      raise Exception.CreateFmt(RCStrFileSIsInvalidBody, [FileName]);

    Anchors := TStringList.Create;
    Links := TStringList.Create;
    try
      LinkNodes := Body.FindNodes('a', True);
      try
        Anchors.Sorted := True;
        Anchors.Duplicates := dupIgnore;
        Links.Sorted := True;
        Links.Duplicates := dupIgnore;

        SetCurrentDir(Folder);
        for I := 0 to LinkNodes.Count - 1 do
          with LinkNodes.Items[I] do
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
              if Pos('//', S) = 0 then
              begin
                S := StringReplace(S, '%20', ' ', [rfReplaceAll]);
                S := ExpandFileName(S);
                Links.Add(S);
              end;
            end;
          end;

        Links.Sorted := False;
        Links.CustomSort(CompareLinks);

        Process(Head, Body, Anchors, Links);
        InjectSectionTemplate('head_begin', 'head_end', Project.HeadSectionTemplateDOM, Head, ExtractFilePath(Project.HeadSectionTemplate));
        InjectSectionTemplate('body_begin', 'body_end', Project.BodySectionTemplateDOM, Body, ExtractFilePath(Project.BodySectionTemplate));
      finally
        LinkNodes.Free;
      end;

    finally
      Links.Free;
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
      Children.OwnsObjects := False;
      Children.Clear;

      for I := 0 to Order.Count - 1 do
      begin
        J := 0;
        while J < L.Count do
          if SameText(TNodeElement(L[J]).DisplayName, Order[I]) then
          begin
            Children.Add(TNodeElement(L[J]));
            L.Delete(J);
          end
          else Inc(J);
      end;

      for I := 0 to L.Count - 1 do
        Children.Add(TNodeElement(L[I]));

      Children.OwnsObjects := True;
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
  Methods := TElements.Create(Self, TTopicElement, 'Methods');
  Events := TElements.Create(Self, TTopicElement, 'Events');
  RegList(Fields);
  RegList(Methods);
  RegList(Properties);
  RegList(Events);
end;

procedure TClassElement.Process(Head, Body: TDomNode; const Anchors, Links: TStringList);
type
  TColumns = set of (ctFields, ctProperties, ctMethods, ctEvents);
var
  ClassLinks, AL: TClassLinks;
  Ancestors: TObjectList;
  AE: TClassElement;
  Columns: TColumns;
  ColCount: Integer;
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
        DisplayName,
        Elements[I].DisplayName,
        DisplayName + '.' + Elements[I].DisplayName,
        Elements[I].FileName);

      Project.Index.AddMember(
        Elements[I].DisplayName,
        DisplayName,
        DisplayName + '.' + Elements[I].DisplayName,
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

      T := Body.Add('table');
      with T do
      begin
        // Attributes['id'] := 'Auto'; // must be unique!

        // heading row
        with Add('tr') do
        begin
          if ctFields in Columns then Add('th').AddText('Fields');
          if ctMethods in Columns then Add('th').AddText('Methods');
          if ctProperties in Columns then Add('th').AddText('Properties');
          if ctEvents in Columns then Add('th').AddText('Events');
        end;

        if ClassLinks.TotalCount > 0 then
        begin
          with Add('tr').Add('td') do
          begin
            Attributes['colspan'] := IntToStr(ColCount);
            Attributes['class'] := 'White';
            AddText('In ' + DirName(Folder) + ':');
          end;
          AddMembers(ClassLinks);
        end;

        for I := 0 to Ancestors.Count - 1 do
          if TClassLinks(Ancestors[I]).TotalCount > 0 then
          begin
            with Add('tr').Add('td') do
            begin
              Attributes['colspan'] := IntToStr(ColCount);
              Attributes['class'] := 'White';
              AddText('In ' + TClassLinks(Ancestors[I]).DisplayName + ':');
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
  Records := TElements.Create(Self, TTopicElement, 'Records');
  Routines := TElements.Create(Self, TTopicElement, 'Routines');
  Types := TElements.Create(Self, TTopicElement, 'Types');
  Globals := TElements.Create(Self, TTopicElement, 'Globals');
  Variables := TElements.Create(Self, TTopicElement, 'Variables');
  Constants := TElements.Create(Self, TTopicElement, 'Constants');
  RegList(Types);
  RegList(Records);
  RegList(Routines);
  RegList(Globals);
  RegList(Constants);
  RegList(Variables);
  RegList(Interfaces);
  RegList(Classes);
end;

procedure TUnitElement.Process(Head, Body: TDomNode; const Anchors, Links: TStringList);
var
  N, I, J: Integer;
  Columns: TList;
  T: TDomNode;
begin
  N := Types.Count;
  if Records.Count > N then N := Records.Count;
  if Interfaces.Count > N then N := Interfaces.Count;
  if Classes.Count > N then N := Classes.Count;
  if Routines.Count > N then N := Routines.Count;
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
      if Records.Count > 0 then Columns.Add(Records);
      if Interfaces.Count > 0 then Columns.Add(Interfaces);
      if Classes.Count > 0 then Columns.Add(Classes);
      if Routines.Count > 0 then Columns.Add(Routines);
      if Globals.Count > 0 then Columns.Add(Globals);
      if Variables.Count > 0 then Columns.Add(Variables);
      if Constants.Count > 0 then Columns.Add(Constants);

      // add table
      T := Body.Add('table');
      with T do
      begin
        // Attributes['id'] := 'Auto'; // must be unique!
        with Add('tr') do
          for I := 0 to Columns.Count - 1 do
            Add('th').AddText(TElements(Columns[I]).SubDirName);
        for I := 0 to N - 1 do
          with Add('tr') do
            for J := 0 to Columns.Count - 1 do
            begin
              with Add('td') do
                if I < TElements(Columns[J]).Count then
                  AddParse(LinkTo(TElements(Columns[J])[I].FileName));
            end;
      end;

    finally
      Columns.Free;
    end;
  end;

  inherited;
end;

{ TProject }

function CompareClasses(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TClassEntry(Item1).Element.DisplayName,
    TClassEntry(Item2).Element.DisplayName);
end;

procedure TProject.AddClasses(const Classes: TClassElementArray; Body: TDomNode);
const
  IMG_E = '<img src="../Images/_BranchEmpty.gif" align="absmiddle">';
  IMG_E1 = '<img src="../Images/_BranchEmpty.gif" align="absmiddle" width="1" height="18">';
  IMG_V = '<img src="../Images/_BranchVert.gif" align="absmiddle">';
  IMG_R = '<img src="../Images/_BranchRight.gif" align="absmiddle">';
  IMG_VR = '<img src="../Images/_BranchVertRight.gif" align="absmiddle">';
  IMG_C = '<img src="../Images/_Class.gif" align="absmiddle">';
var
  CC: TObjectList;
  I: Integer;

  procedure AddClass(Parent: TClassEntry; Elem: TClassElement; List: TObjectList);
  var
    I: Integer;
    Entry: TClassEntry;
  begin
    Entry := TClassEntry.Create(Elem);
    Entry.Parent := Parent;
    List.Add(Entry);
    for I := 0 to High(Classes) do
      if Classes[I].Ancestor = Elem then AddClass(Entry, Classes[I], Entry.Children);
  end;

  procedure Write(Entry: TClassEntry; Level: Integer; IsLast: Boolean);
  var
    I: Integer;
    E: TClassEntry;
  begin
    with Body.Add('p') do
    begin
      // Attributes['id'] := 'Auto'; // must be unique!
      Attributes['Class'] := 'Tree';

      E := Entry;
      while E.Parent <> nil do
      begin
        if E.Parent.Children.Last = E then
        begin
          if E = Entry then AddText(IMG_R).Index := 0
          else AddText(IMG_E).Index := 0;
        end
        else
          if E = Entry then AddText(IMG_VR).Index := 0
          else AddText(IMG_V).Index := 0;
        E := E.Parent;
      end;
      AddText(IMG_E1).Index := 0;

      if Entry.Element.FileName <> '' then
        AddParse(LinkTo(Entry.Element.FileName)).AddText(IMG_C + '&nbsp;').Index := 0
      else
        AddParse(Entry.Element.DisplayName).AddText(IMG_C + '&nbsp;').Index := 0;
    end;

    Entry.Children.Sort(CompareClasses);
    for I := 0 to Entry.Children.Count - 1 do
      Write(TClassEntry(Entry.Children.Items[I]), Level + 1, I = Entry.Children.Count - 1);
  end;

begin
  CC := TObjectList.Create;
  try
    for I := 0 to High(Classes) do
      if Classes[I].Ancestor = nil then AddClass(nil, Classes[I], CC);

    CC.Sort(CompareClasses);
    for I := 0 to CC.Count - 1 do
      Write(TClassEntry(CC.Items[I]), 0, I = CC.Count - 1);
  finally
    CC.Free;
  end;
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
        if SameText(Classes[J].DisplayName, AName) then
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

procedure TProject.BuildIndex(const IndexFile: string);
var
  Dom: TDomDocument;
  Body: TDomNode;
  I, J, K: Integer;
  Base: string;

  function Link(const Target: string): string;
  begin
    Result := 'Docs\' + ExtractRelativePath(Base, Target);
  end;

begin
  Base := Folder + '\t.htm';
  Dom := TDomDocument.Create;
  try
    with Dom.Add('html') do
    begin
      Add('head');
      Body := Add('Body');
      with Body.Add('ul') do
      begin
        for I := 0 to Self.Index.Count - 1 do
          with TIndexEntry(Self.Index.Items[I]) do
          begin
            with Add('li').AddObject('text/sitemap') do
            begin
              AddObjectParam('Keyword', Keyword);
              for J := 0 to Targets.Count - 1 do
              begin
                AddObjectParam('Name', GetLinkName(Targets[J]));
                AddObjectParam('Local', Link(Targets[J]));
              end;
            end;

            if Members.Count > 0 then with Add('ul') do
              for J := 0 to Members.Count - 1 do
                with Add('li').AddObject('text/sitemap'), Members[J] do
                begin
                  AddObjectParam('Keyword', Keyword);
                  for K := 0 to Targets.Count - 1 do
                  begin
                    AddObjectParam('Name', Name);
                    AddObjectParam('Local', Link(Targets[K]));
                  end;
                end;
            end;
        end;
    end;
    Dom.SaveToFile(IndexFile);
  finally
    Dom.Free;
  end;
end;

procedure TProject.BuildTOC(const TocFile: string);
var
  Dom: TDomDocument;
  Body: TDomNode;
  Base: string;

  procedure AddNode(N: TDomNode; NE: TNodeElement);
  var
    I: Integer;
    UL: TDomNode;
  begin
    with N.Add('li').AddObject('text/sitemap') do
    begin
      AddObjectParam('Name', NE.DisplayName);
      if NE.FileName <> '' then
        AddObjectParam('Local', 'Docs\' + ExtractRelativePath(Base, NE.FileName));
      if NE.Count = 0 then AddObjectParam('ImageNumber', '11');
    end;
    if NE.Count > 0 then
    begin
      UL := N.Add('ul');
      for I := 0 to NE.Count - 1 do AddNode(UL, NE.Child[I]);
    end;
  end;

  procedure AddGroupElement(N: TDomNode; E: TGroupElement);
  var
    I: Integer;
    UL: TDomNode;

    procedure AddList(NN: TDomNode; L: TElements);
    var
      I: Integer;
      UL: TDomNode;
    begin
      Assert(L <> nil);
      if L.Count = 0 then Exit;
      if L.ElemClass = TClassElement then
        for I := 0 to L.Count - 1 do AddGroupElement(NN, L[I])
      else
      begin
        with NN.Add('li').AddObject('text/sitemap') do
        begin
          AddObjectParam('Name', L.SubDirName);
          AddObjectParam('ImageNumber', '5');
        end;
        UL := NN.Add('ul');
        for I := 0 to L.Count - 1 do
        begin
          Assert(L[I] <> nil);
          AddGroupElement(UL, L[I]);
        end;
      end;
    end;

  begin
    if E.FileName <> '' then
    begin
      if E <> Project then
        with N.Add('li').AddObject('text/sitemap') do
        begin
          AddObjectParam('Name', E.DisplayName);
          if E.FileName <> '' then
            AddObjectParam('Local', 'Docs\' + ExtractRelativePath(Base, E.FileName));
          if E.IsTopic then AddObjectParam('ImageNumber', '11');
          if E is TClassElement then AddObjectParam('ImageNumber', '11');
        end;
    end;

    if E.ChildLists.Count > 0 then
    begin
      UL := N.Add('ul');
      with UL do
      begin
        if E = Self then
          with Add('li').AddObject('text/sitemap') do
          begin
            AddObjectParam('Name', E.DisplayName);
            if E.FileName <> '' then
              AddObjectParam('Local', 'Docs\' + ExtractRelativePath(Base, E.FileName));
            AddObjectParam('ImageNumber', '21');
          end;
      end;

      if E = Self then
        for I := 0 to Root.Count - 1 do
          AddNode(UL, Root.Child[I]);

      if E = Self then
      begin
        with UL.Add('li').AddObject('text/sitemap') do
        begin
          AddObjectParam('Name', 'Reference');
        end;
        UL := UL.Add('ul');
      end;

      for I := 0 to E.ChildLists.Count - 1 do
        AddList(UL, TElements(E.ChildLists[I]));
    end;
  end;

begin
  Base := Folder + '\t.htm';
  Dom := TDomDocument.Create;
  try
    with Dom.Add('html') do
    begin
      Add('head');
      Body := Add('Body');
      with Body.AddObject('text/site properties') do
      begin
        AddObjectParam('Window Styles', '0x800227');
        AddObjectParam('Image Type', 'Folder');
      end;

      AddGroupElement(Body, Self);
    end;
    Dom.SaveToFile(TocFile);
  finally
    Dom.Free;
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
  HeadSectionTemplateDOM := TDomDocument.Create;
  BodySectionTemplateDOM := TDomDocument.Create;
  RegList(Units);
  if (FileName <> '') then Files.AddObject(FileName, Self);
  BrokenLinks := TStringList.create;
end;

destructor TProject.Destroy;
begin
  BrokenLinks.Free;
  BodySectionTemplateDOM.Free;
  HeadSectionTemplateDOM.Free;
  Root.Free;
  Index.Free;
  Files.Free;
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
    if SameText(Result.DisplayName, AName) then Exit;
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
    if SameText(Result.DisplayName, AName) then Exit;
  end;
  Result := nil;
end;

function GetParentDirectory(const path: string): string;
begin
 Result := ExpandFileName(path + '\..') + '\';
end;

procedure TProject.ParseMenuData(CuList: TCaptionUrlList);
var
  Index: Integer;
  projDir, Cap, url: string;
  StrLst: TStringList;

  function GetTextBetweenQuotes(const str: string): string;
  var
    i, j: Integer;
  begin
    i := Pos('"', str);
    j := PosEx('"', str, i+1);
    if (i = 0) or (j <= i) then Result := ''
    else SetString(Result, PChar(@str[i+1]), j-i-1);
  end;

begin
  CuList.Clear;
  ProjDir := GetParentDirectory(Folder);
  if not {$IFDEF COMPILERXE2_UP}SysUtils.{$ENDIF}DirectoryExists(ProjDir + 'Scripts') or
    not FileExists(ProjDir + 'Scripts\menu_data.js') then exit;
  StrLst := TStringList.Create;
  try
    StrLst.LoadFromFile(ProjDir + 'Scripts\menu_data.js');
    for Index := 0 to StrLst.Count - 2 do
    begin
      if (Pos('td_1', StrLst[Index]) <> 1) or (PosEx('_', StrLst[Index], 5) <> 5) then Continue;
      Cap := GetTextBetweenQuotes(StrLst[Index]);
      if Pos('url_', StrLst[Index+1]) <> 1 then Continue;
      Url := {'.\' +} GetTextBetweenQuotes(StrLst[Index + 1]);
      CuList.add(Cap, url);
    end;
  finally
    StrLst.Free;
  end;
end;

procedure TProject.Process(Head, Body: TDomNode; const Anchors, Links: TStringList);
var
  Elems: TElements;
  I, J: Integer;
  TD: TDomNode;
  CuList: TCaptionUrlList;

  procedure AddOverviewTable(CuList: TCaptionUrlList);
  var
    i: Integer;
  begin
    with Body.Add('table') do
    begin
      // Attributes['id'] := 'Auto'; // must be unique!
      with Add('tr') do
        for i := 0 to CuList.Count -1 do
          with Add('td') do
            AddText(Format('<a href="%s">%s</a>', [CuList.Url[i], CuList.Caption[i]]));
    end;
  end;

  procedure AddClassTable(const Classes: TClassElementArray; const DisplayName: string);
  var
    I, J: Integer;
  begin
    with Body.Add('table') do
    begin
      // Attributes['id'] := 'Auto'; // must be unique!
      Attributes['class'] := 'Home';
      Attributes['cellpadding'] := '0';
      Attributes['cellspacing'] := '0';
      Attributes['border'] := '0';
      with Add('tr') do
      begin
        TD := Add('td');
        TD.Attributes['class'] := 'Home';
        TD.Attributes['valign'] := 'Top';

        if IncludeAlphabetClasses then
        begin
          for I := 0 to High(Classes) do Elems.Add(TClassElement(Classes[I]));
          if Elems.Count > 0 then
          begin
            TD.Add('h2').AddText(DisplayName + ' (Alphabetical)');
            Elems.Sort(CompareElements);
            for J := 0 to Elems.Count - 1 do
              with TD.Add('p') do
              begin
                // Attributes['id'] := 'Auto'; // must be unique!
                Attributes['class'] := 'Tree';

                AddText('<img src="../Images/_BranchEmpty.gif" align="absmiddle" width="1" height="18">');
                AddParse(LinkTo(Elems[J]));
              end;
            Elems.Clear;
          end;
        end;

        // classes
        TD := Add('td');
        with TD do
        begin
          Attributes['class'] := 'Home';
          Attributes['valign'] := 'Top';
          Add('h2').AddText(DisplayName + ' (Hierarchy)');
          AddClasses(Classes, TD);
        end;
      end;
    end;
  end;

  procedure AddElems(NUM_COL: Integer);
  var
    I, J, N, R: Integer;
    Links: array of array of string;
  begin
    Elems.Sort(CompareElements);
    with Body.Add('table') do
    begin
      // Attributes['id'] := 'Auto'; // must be unique!
      SetLength(Links, NUM_COL);
      R := (Elems.Count + NUM_COL - 1) div NUM_COL;
      N := 0;
      for I := 0 to NUM_COL - 1 do
      begin
        SetLength(Links[I], R);
        for J := 0 to R - 1 do
        begin
          if N < Elems.Count then
            Links[I][J] := LinkTo(Elems[N])
          else Break;
          Inc(N);
        end;
      end;

      for J := 0 to R - 1 do
      begin
        with Add('tr') do
          for I := 0 to NUM_COL - 1 do
            if Links[I][J] <> '' then
              with Add('td') do AddText(Links[I][J]);
      end;
    end;
  end;

begin
  Elems := TElements.Create(nil, TTopicElement, Folder);
  try
    Elems.OwnsObjects := false;                  //!! IMPORTANT

    //overview items ...
    CuList := TCaptionUrlList.Create;
    try
      ParseMenuData(CuList);
      if CuList.Count > 0 then
        AddOverviewTable(CuList);
    finally
      CuList.Free;
    end;

    if Length(Classes) > 0 then
      AddClassTable(TClassElementArray(Classes), 'Classes');
    if Length(Interfaces) > 0 then
      AddClassTable(TClassElementArray(Interfaces), 'Interfaces');

    // types
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Types.Count - 1 do Elems.Add(Types[J]);
    if Elems.Count > 0 then
      begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Types'; // must be unique!
        AddText('Types');
      end;
      AddElems(cColumnCount);
      Elems.Clear;
    end;

    // Records
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Records.Count - 1 do Elems.Add(Records[J]);
    if Elems.Count > 0 then
    begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Records'; // must be unique!
        AddText('Records');
      end;
      AddElems(cColumnCount);
      Elems.Clear;
    end;

    // Functions
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Routines.Count - 1 do Elems.Add(Routines[J]);
    if Elems.Count > 0 then
    begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Functions'; // must be unique!
        AddText('Routines');
      end;
      AddElems(cColumnCount);
      Elems.Clear;
    end;

    // globals
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Globals.Count - 1 do Elems.Add(Globals[J]);
    if Elems.Count > 0 then
      begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Globals'; // must be unique!
        AddText('Globals');
      end;
      AddElems(cColumnCount);
      Elems.Clear;
    end;

    // variables
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Variables.Count - 1 do Elems.Add(Variables[J]);
    if Elems.Count > 0 then
      begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Variables'; // must be unique!
        AddText('Variables');
      end;
      AddElems(cColumnCount);
      Elems.Clear;
    end;

    // constants
    for I := 0 to Units.Count - 1 do with TUnitElement(Units[I]) do
      for J := 0 to Constants.Count - 1 do Elems.Add(Constants[J]);
    if Elems.Count > 0 then
      begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Constants'; // must be unique!
        AddText('Constants');
      end;
      AddElems(cColumnCount);
      Elems.Clear;
    end;

    // units
    for I := 0 to Units.Count - 1 do Elems.Add(TUnitElement(Units[I]));
    if Elems.Count > 0 then
    begin
      with Body.Add('h2') do
      begin
        Attributes['id'] := 'Auto-Units'; // must be unique!
        AddText('Units');
      end;
      Elems.Sort(CompareElements);
      AddElems(cColumnCount);
      Elems.Clear;
    end;
  finally
    Elems.Free;
  end;
end;

procedure TProject.Read;
var
  Files, Dirs: TStringList;
  I, J: Integer;
  NE: TNodeElement;
begin
  inherited;

  if FileExists(HeadSectionTemplate) then
    HeadSectionTemplateDOM.LoadFromFile(HeadSectionTemplate);

  if FileExists(BodySectionTemplate) then
    BodySectionTemplateDOM.LoadFromFile(BodySectionTemplate);

  Dirs := GetDirList(Folder, '*.*');
  Files := GetFileList(Folder, '*.htm*');
  try
    if Dirs.Count = 0 then Exit;
    for I := 0 to ChildLists.Count - 1 do
    begin
      J := Dirs.IndexOf(Folder + '\' + TElements(ChildLists[I]).SubDirName);
      if J >= 0 then
        Dirs.Delete(J);
    end;
    for I := Dirs.Count - 1 downto 0 do
      if SameText(Dirs[I], 'cvs') then
        Dirs.Delete(I);

    J := Files.IndexOf(Folder + '\' + '_Body.htm');
    if J >= 0 then
      Files.Delete(J);

    for I := 0 to Dirs.Count - 1 do
    begin
      NE := TNodeElement.Create(Self, Dirs[I]);
      Root.Children.Add(NE);
      NE.Read;
    end;
    for I := 0 to Files.Count - 1 do
    begin
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

initialization
  VersionString := 'v1.0';
  GetLocaleFormatSettings(2057, EnglishFormatSettings);
  
end.
