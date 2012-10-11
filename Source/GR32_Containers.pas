unit GR32_Containers;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Repaint Optimizer Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  Types,
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}
  RTLConsts,
  GR32, SysUtils, GR32_LowLevel, Classes, TypInfo;

const
  BUCKET_MASK = $FF;               
  BUCKET_COUNT = BUCKET_MASK + 1;  // 256 buckets by default

type
  PPItem = ^PItem;
  PItem = Pointer;

  PPData = ^PData;
  PData = Pointer;

  PPointerBucketItem = ^TPointerBucketItem;
  TPointerBucketItem = record
    Item: PItem;
    Data: PData;
  end;
  TPointerBucketItemArray = array of TPointerBucketItem;

  TPointerBucket = record
    Count: Integer;
    Items: TPointerBucketItemArray;
  end;
  TPointerBucketArray = array[0..BUCKET_MASK] of TPointerBucket;

  { TPointerMap } 
  { Associative pointer map
    Inspired by TBucketList, which is not available on D5/CB5, it is
    reimplemented from scratch, simple, optimized and light-weight.
    Not thread-safe. Does use exceptions only for Data property. }
  TPointerMap = class
  private
    FBuckets: TPointerBucketArray;
    FCount: Integer;
  protected
    function GetData(Item: PItem): PData;
    procedure SetData(Item: PItem; const Data: PData);
    function Exists(Item: Pointer; out BucketIndex, ItemIndex: Integer): Boolean;
    function Delete(BucketIndex, ItemIndex: Integer): PData; virtual;
  public
    destructor Destroy; override;
    function Add(NewItem: PItem): PPData; overload;
    function Add(NewItem: PItem; out IsNew: Boolean): PPData; overload;
    function Add(NewItem: PItem; NewData: PData): PPData; overload;
    function Add(NewItem: PItem; NewData: PData; out IsNew: Boolean): PPData; overload;
    function Remove(Item: PItem): PData;
    procedure Clear;
    function Contains(Item: PItem): Boolean;
    function Find(Item: PItem; out Data: PPData): Boolean;
    property Data[Item: PItem]: PData read GetData write SetData; default;
    property Count: Integer read FCount;
  end;

  { TPointerMapIterator }
  { Iterator object for the associative pointer map
    See below for usage example... }
  TPointerMapIterator = class
  private
    FSrcPointerMap: TPointerMap;
    FItem: PItem;
    FData: PData;
    FCurBucketIndex: Integer;
    FCurItemIndex: Integer;
  public
    constructor Create(SrcPointerMap: TPointerMap);
    function Next: Boolean;
    property Item: PItem read FItem;
    property Data: PData read FData;
  end;
  {
    USAGE EXAMPLE:
    --------------
    with TPointerMapIterator.Create(MyPointerMap) do
    try
      while Next do
      begin
        // do something with Item and Data here...
      end;
    finally
      Free;
    end;
  }

  PPolyRects = ^TPolyRects;
  TPolyRects = Array[0..Maxint div 32 - 1] of TRect;

  { TRectList }
  { List that holds Rects
    Do not reuse TList due to pointer structure.
    A direct structure is more memory efficient.
    stripped version of TList blatantly stolen from Classes.pas }
  TRectList = class
  private
    FList: PPolyRects;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): PRect;
    procedure Grow; virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(const Rect: TRect): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(const Rect: TRect): Integer;
    procedure Insert(Index: Integer; const Rect: TRect);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(const Rect: TRect): Integer;
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: PRect read Get; default;
    property List: PPolyRects read FList;
  end;

  { TClassList }
  { This is a class that maintains a list of classes. }
  TClassList = class(TList)
  protected
    function GetItems(Index: Integer): TClass;
    procedure SetItems(Index: Integer; AClass: TClass);
  public
    function Add(AClass: TClass): Integer;
    function Extract(Item: TClass): TClass;
    function Remove(AClass: TClass): Integer;
    function IndexOf(AClass: TClass): Integer;
    function First: TClass;
    function Last: TClass;
    function Find(AClassName: string): TClass;
    procedure GetClassNames(Strings: TStrings);
    procedure Insert(Index: Integer; AClass: TClass);
    property Items[Index: Integer]: TClass read GetItems write SetItems; default;
  end;


  PLinkedNode = ^TLinkedNode;
  TLinkedNode = record
    Prev: PLinkedNode;
    Next: PLinkedNode;
    Data: Pointer;
  end;

  TIteratorProc = procedure(Node: PLinkedNode; Index: Integer);

  TFreeDataEvent = procedure(Data: Pointer) of object;

  { TLinkedList }
  { A class for maintaining a linked list }
  TLinkedList = class
   private
     FCount: Integer;
     FHead: PLinkedNode;
     FTail: PLinkedNode;
     FOnFreeData: TFreeDataEvent;
   protected
     procedure DoFreeData(Data: Pointer); virtual;
   public
     destructor Destroy; override;
     function Add: PLinkedNode;
     procedure Remove(Node: PLinkedNode);
     function IndexOf(Node: PLinkedNode): Integer;
     function GetNode(Index: Integer): PLinkedNode;
     procedure Exchange(Node1, Node2: PLinkedNode);
     procedure InsertBefore(Node, NewNode: PLinkedNode);
     procedure InsertAfter(Node, NewNode: PLinkedNode);
     procedure Clear;
     procedure IterateList(CallBack: TIteratorProc);
     property Head: PLinkedNode read FHead write FHead;
     property Tail: PLinkedNode read FTail write FTail;
     property Count: Integer read FCount write FCount;
     property OnFreeData: TFreeDataEvent read FOnFreeData write FOnFreeData;
   end;


procedure SmartAssign(Src, Dst: TPersistent; TypeKinds: TTypeKinds = tkProperties);
procedure Advance(var Node: PLinkedNode; Steps: Integer = 1);

implementation

procedure SmartAssign(Src, Dst: TPersistent; TypeKinds: TTypeKinds = tkProperties);
var
  Count, I: Integer;
  Props: PPropList;
  SubSrc, SubDst: TPersistent;
begin
  Count := GetTypeData(Src.ClassInfo).PropCount;
  if Count = 0 then Exit;

  GetMem(Props, Count * SizeOf(PPropInfo));
  try
    // Get the property list in an unsorted fashion.
    // This is important so the order in which the properties are defined is obeyed,
    // ie. mimic how the Delphi form loader would set the properties.
    Count := GetPropList(Src.ClassInfo, TypeKinds, Props, False);

    for I := 0 to Count - 1 do
    with Props^[I]^ do
    begin
      if PropType^.Kind = tkClass then
      begin
        // TODO DVT Added cast to fix ShortString to String warnings. Need to verify is OK
        SubDst := TPersistent(GetObjectProp(Dst, string(Name)));
        if not Assigned(SubDst) then Continue;

        SubSrc := TPersistent(GetObjectProp(Src, string(Name)));
        if Assigned(SubSrc) then SubDst.Assign(SubSrc);
      end
      else
        SetPropValue(Dst, string(Name), GetPropValue(Src, string(Name), True));
    end;
  finally
    FreeMem(Props, Count * SizeOf(PPropInfo));
  end;
end;

procedure Advance(var Node: PLinkedNode; Steps: Integer);
begin
  if Steps > 0 then
  begin
    while Assigned(Node) and (Steps > 0) do
    begin
      Dec(Steps);
      Node := Node.Next;
    end;
  end
  else
  begin
    while Assigned(Node) and (Steps < 0) do
    begin
      Inc(Steps);
      Node := Node.Prev;
    end;
  end;
end;


{ TPointerMap }

function TPointerMap.Add(NewItem: PItem; NewData: PData): PPData;
var
  Dummy: Boolean;
begin
  Result := Add(NewItem, NewData, Dummy);
end;

function TPointerMap.Add(NewItem: PItem): PPData;
var
  Dummy: Boolean;
begin
  Result := Add(NewItem, nil, Dummy);
end;

function TPointerMap.Add(NewItem: PItem; out IsNew: Boolean): PPData;
begin
  Result := Add(NewItem, nil, IsNew);
end;

function TPointerMap.Add(NewItem: PItem; NewData: PData; out IsNew: Boolean): PPData;
var
  BucketIndex, ItemIndex, Capacity: Integer;
begin
  if Exists(NewItem, BucketIndex, ItemIndex) then
  begin
    IsNew := False;
    Result := @FBuckets[BucketIndex].Items[ItemIndex].Data
  end
  else
  begin
    with FBuckets[BucketIndex] do
    begin
      Capacity := Length(Items);

      // enlarge capacity if completely used
      if Count = Capacity then
      begin
        if Capacity > 64 then
          Inc(Capacity, Capacity div 4)
        else if Capacity > 8 then
          Inc(Capacity, 16)
        else
          Inc(Capacity, 4);

        SetLength(Items, Capacity);
      end;

      with Items[Count] do
      begin
        Item := NewItem;
        Data := NewData;
        Result := @Data;
      end;

      Inc(Count);
      IsNew := True;
    end;
    Inc(FCount);
  end;
end;

procedure TPointerMap.Clear;
var
  BucketIndex, ItemIndex: Integer;
begin
  FCount := 0;

  for BucketIndex := 0 to BUCKET_MASK do
  with FBuckets[BucketIndex] do
  begin
    for ItemIndex := Count - 1 downto 0 do
      Delete(BucketIndex, ItemIndex);

    Count := 0;
    SetLength(Items, 0);
  end;
end;

destructor TPointerMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TPointerMap.Delete(BucketIndex, ItemIndex: Integer): PData;
begin
  with FBuckets[BucketIndex] do
  begin
    Result := Items[ItemIndex].Data;

    if FCount = 0 then Exit;

    Dec(Count);
    if Count = 0 then
      SetLength(Items, 0)
    else
    if (ItemIndex < Count) then
      Move(Items[ItemIndex + 1], Items[ItemIndex], (Count - ItemIndex) * SizeOf(TPointerBucketItem));
  end;
  Dec(FCount);
end;

function TPointerMap.Remove(Item: PItem): PData;
var
  BucketIndex, ItemIndex: Integer;
begin
  if Exists(Item, BucketIndex, ItemIndex) then
    Result := Delete(BucketIndex, ItemIndex)
  else
    Result := nil;
end;

function TPointerMap.Contains(Item: PItem): Boolean;
var
  Dummy: Integer;
begin
  Result := Exists(Item, Dummy, Dummy);
end;

function TPointerMap.Find(Item: PItem; out Data: PPData): Boolean;
var
  BucketIndex, ItemIndex: Integer;
begin
  Result := Exists(Item, BucketIndex, ItemIndex);
  if Result then
    Data := @FBuckets[BucketIndex].Items[ItemIndex].Data;
end;

function TPointerMap.Exists(Item: PItem; out BucketIndex, ItemIndex: Integer): Boolean;
var
  I: Integer;
begin
  BucketIndex := Cardinal(Item) shr 8 and BUCKET_MASK; // KISS pointer hash(TM)
  // due to their randomness, pointers most commonly differ at byte 1, we use
  // this characteristic for our hash and just apply the mask to it.
  // Worst case scenario happens when most changes are at byte 0, which causes
  // one bucket to be saturated whereas the other buckets are almost empty...

  Result := False;
  with FBuckets[BucketIndex] do
  for I := 0 to Count - 1 do
    if Items[I].Item = Item then
    begin
      ItemIndex := I;
      Result := True;
      Exit;
    end;
end;

function TPointerMap.GetData(Item: PItem): PData;
var
  BucketIndex, ItemIndex: Integer;
begin
  if not Exists(Item, BucketIndex, ItemIndex) then
    raise EListError.CreateFmt(SItemNotFound, [Integer(Item)])
  else
    Result := FBuckets[BucketIndex].Items[ItemIndex].Data;
end;

procedure TPointerMap.SetData(Item: PItem; const Data: PData);
var
  BucketIndex, ItemIndex: Integer;
begin
  if not Exists(Item, BucketIndex, ItemIndex) then
    raise EListError.CreateFmt(SItemNotFound, [Integer(Item)])
  else
    FBuckets[BucketIndex].Items[ItemIndex].Data := Data;
end;

{ TPointerMapIterator }

constructor TPointerMapIterator.Create(SrcPointerMap: TPointerMap);
begin
  inherited Create;
  FSrcPointerMap := SrcPointerMap;

  FCurBucketIndex := -1;
  FCurItemIndex := -1;
end;

function TPointerMapIterator.Next: Boolean;
begin
  if FCurItemIndex > 0 then
    Dec(FCurItemIndex)
  else
  begin
    FCurItemIndex := -1;
    while (FCurBucketIndex < BUCKET_MASK) and (FCurItemIndex = -1) do
    begin
      Inc(FCurBucketIndex);
      FCurItemIndex := FSrcPointerMap.FBuckets[FCurBucketIndex].Count - 1;
    end;

    if FCurBucketIndex = BUCKET_MASK then
    begin
      Result := False;
      Exit;
    end
  end;

  Result := True;
  with FSrcPointerMap.FBuckets[FCurBucketIndex].Items[FCurItemIndex] do
  begin
    FItem := Item;
    FData := Data;
  end;
end;


{ TRectList }

destructor TRectList.Destroy;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TRectList.Add(const Rect: TRect): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Rect;
  Inc(FCount);
end;

procedure TRectList.Clear;
begin
  SetCount(0);
  SetCapacity(10);
end;

procedure TRectList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TRect));
end;

procedure TRectList.Exchange(Index1, Index2: Integer);
var
  Item: TRect;
begin
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TRectList.Get(Index: Integer): PRect;
begin
  if (Index < 0) or (Index >= FCount) then
    Result := nil
  else
    Result := @FList^[Index];
end;

procedure TRectList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 128 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 32
    else
      Delta := 8;
  SetCapacity(FCapacity + Delta);
end;

function TRectList.IndexOf(const Rect: TRect): Integer;
begin
  Result := 0;
  while (Result < FCount) and not EqualRect(FList^[Result], Rect) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TRectList.Insert(Index: Integer; const Rect: TRect);
begin
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TRect));
  FList^[Index] := Rect;
  Inc(FCount);
end;

procedure TRectList.Move(CurIndex, NewIndex: Integer);
var
  Item: TRect;
begin
  if CurIndex <> NewIndex then
  begin
    Item := Get(CurIndex)^;
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

function TRectList.Remove(const Rect: TRect): Integer;
begin
  Result := IndexOf(Rect);
  if Result >= 0 then
    Delete(Result);
end;

procedure TRectList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TRectList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TRect));
    FCapacity := NewCapacity;
  end;
end;

procedure TRectList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TRect), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{ TClassList }

function TClassList.Add(AClass: TClass): Integer;
begin
  Result := inherited Add(AClass);
end;

function TClassList.Extract(Item: TClass): TClass;
begin
  Result := TClass(inherited Extract(Item));
end;

function TClassList.Find(AClassName: string): TClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if TClass(List[I]).ClassName = AClassName then
    begin
      Result := TClass(List[I]);
      Break;
    end;
end;

function TClassList.First: TClass;
begin
  Result := TClass(inherited First);
end;

procedure TClassList.GetClassNames(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Strings.Add(TClass(List[I]).ClassName);
end;

function TClassList.GetItems(Index: Integer): TClass;
begin
  Result := TClass(inherited Items[Index]);
end;

function TClassList.IndexOf(AClass: TClass): Integer;
begin
  Result := inherited IndexOf(AClass);
end;

procedure TClassList.Insert(Index: Integer; AClass: TClass);
begin
  inherited Insert(Index, AClass);
end;

function TClassList.Last: TClass;
begin
  Result := TClass(inherited Last);
end;

function TClassList.Remove(AClass: TClass): Integer;
begin
  Result := inherited Remove(AClass);
end;

procedure TClassList.SetItems(Index: Integer; AClass: TClass);
begin
  inherited Items[Index] := AClass;
end;

{ TLinkedList }

function TLinkedList.Add: PLinkedNode;
begin
  New(Result);
  Result.Data := nil;
  Result.Next := nil;
  Result.Prev := nil;
  if Head = nil then
  begin
    Head := Result;
    Tail := Result;
  end
  else
    InsertAfter(FTail, Result);
end;

procedure TLinkedList.Clear;
var
  P, NextP: PLinkedNode;
begin
  P := Head;
  while Assigned(P) do
  begin
    NextP := P.Next;
    DoFreeData(P.Data);
    Dispose(P);
    P := NextP;
  end;
  Head := nil;
  Tail := nil;
  Count := 0;
end;

destructor TLinkedList.Destroy;
begin
  Clear;
end;

procedure TLinkedList.DoFreeData(Data: Pointer);
begin
  if Assigned(FOnFreeData) then FOnFreeData(Data);
end;

procedure TLinkedList.Exchange(Node1, Node2: PLinkedNode);
begin
  if Assigned(Node1) and Assigned(Node2) and (Node1 <> Node2) then
  begin
    if Assigned(Node1.Prev) then Node1.Prev.Next := Node2;
    if Assigned(Node1.Next) then Node1.Next.Prev := Node2;
    if Assigned(Node2.Prev) then Node2.Prev.Next := Node1;
    if Assigned(Node2.Next) then Node2.Next.Prev := Node1;
    if Head = Node1 then Head := Node2 else if Head = Node2 then Head := Node1;
    if Tail = Node1 then Tail := Node2 else if Tail = Node2 then Tail := Node1;
    Swap(Pointer(Node1.Next), Pointer(Node2.Next));
    Swap(Pointer(Node1.Prev), Pointer(Node2.Prev));
  end;
end;

function TLinkedList.GetNode(Index: Integer): PLinkedNode;
begin
  Result := Head;
  Advance(Result, Index);
end;

function TLinkedList.IndexOf(Node: PLinkedNode): Integer;
var
  I: Integer;
  P: PLinkedNode;
begin
  Result := -1;
  P := Head;
  for I := 0 to Count - 1 do
  begin
    if P = Node then
    begin
      Result := I;
      Exit;
    end;
    P := P.Next;
  end;
end;

procedure TLinkedList.InsertAfter(Node, NewNode: PLinkedNode);
begin
  if Assigned(Node) and Assigned(NewNode) then
  begin
    NewNode.Prev := Node;
    NewNode.Next := Node.Next;
    if Assigned(Node.Next) then Node.Next.Prev := NewNode;
    Node.Next := NewNode;
    if Node = Tail then Tail := NewNode;
    Inc(FCount);
  end;
end;

procedure TLinkedList.InsertBefore(Node, NewNode: PLinkedNode);
begin
  if Assigned(Node) and Assigned(NewNode) then
  begin
    NewNode.Next := Node;
    NewNode.Prev := Node.Prev;
    if Assigned(Node.Prev) then Node.Prev.Next := NewNode;
    Node.Prev := NewNode;
    if Node = Head then Head := NewNode;
    Inc(FCount);
  end;
end;

procedure TLinkedList.IterateList(CallBack: TIteratorProc);
var
  I: Integer;
  P: PLinkedNode;
begin
  P := Head;
  for I := 0 to Count - 1 do
  begin
    CallBack(P, I);
    P := P.Next;
  end;
end;

procedure TLinkedList.Remove(Node: PLinkedNode);
begin
  if Assigned(Node) then
  begin
    DoFreeData(Node.Data);
    if Assigned(Node.Prev) then Node.Prev.Next := Node.Next;
    if Assigned(Node.Next) then Node.Next.Prev := Node.Prev;
    if Node = Head then Head := Node.Next;
    if Node = Tail then Tail := Node.Prev;
    Dispose(Node);
    Dec(FCount);
  end;
end;

end.