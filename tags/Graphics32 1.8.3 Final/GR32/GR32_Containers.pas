unit GR32_Containers;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is Repaint Optimizer Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException OHG
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF CLX}
  Qt, Types, {$IFDEF LINUX}Libc, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  {$IFDEF COMPILER6}RTLConsts, {$ENDIF}
  GR32, SysUtils, GR32_LowLevel, Classes, TypInfo;

{$IFNDEF COMPILER6}
const
  SItemNotFound = 'Item not found ($0%x)';
{$ENDIF}

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

procedure SmartAssign(Src, Dst: TPersistent; TypeKinds: TTypeKinds = tkProperties);

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
    Count := GetPropList(Src.ClassInfo, TypeKinds, Props{$IFDEF COMPILER6}, False{$ENDIF});

    for I := 0 to Count - 1 do
    with Props^[I]^ do
    begin
      if PropType^.Kind = tkClass then
      begin
        SubDst := TPersistent(GetObjectProp(Dst, Name));
        if not Assigned(SubDst) then Continue;

        SubSrc := TPersistent(GetObjectProp(Src, Name));
        if Assigned(SubSrc) then SubDst.Assign(SubSrc);
      end
      else
        SetPropValue(Dst, Name, GetPropValue(Src, Name, False));
    end;
  finally
    FreeMem(Props, Count * SizeOf(PPropInfo));
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

    if Count = 1 then
      SetLength(Items, 0)
    else
      Move(Items[ItemIndex + 1], Items[ItemIndex], (Count - ItemIndex) * SizeOf(TPointerBucketItem));

    Dec(Count);
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
  BucketIndex := Integer(Item) shr 8 and BUCKET_MASK; // KISS pointer hash(TM)
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

end.

