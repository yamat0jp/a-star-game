unit Heap;

interface

uses
  System.SysUtils, System.Generics.Collections;

type

  THeap<T> = class
  type
    THikakuFunc = reference to function(Little, Large: T): Boolean;
  private
    function GetParentIndex(Index: Integer): Integer;
    function GetLeftChildIndex(Index: Integer): Integer;
    function GetRightChildIndex(Index: Integer): Integer;
    function GetCount: Integer;
  protected
    FItems: TList<T>;
    FFunc: THikakuFunc;
    procedure HeapifyUp(Index: integer);
    procedure HeapifyDown(Index: integer);
    function Hikaku(Little, Large: T): Boolean; virtual;
  public
    constructor Create(AFunc: THikakuFunc);
    destructor Destroy; override;
    procedure Add(Item: T);
    function Extract: T;
    function Peek: T;
    function IndexOf(const Value: T): Integer;
    property Count: Integer read GetCount;
  end;

implementation

{ THeap<T> }

constructor THeap<T>.Create(AFunc: THikakuFunc);
begin
  inherited Create;
  FItems := TList<T>.Create;
  FFunc := AFunc;
end;

destructor THeap<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function THeap<T>.GetParentIndex(Index: Integer): Integer;
begin
  result := (Index - 1) div 2;
end;

function THeap<T>.GetLeftChildIndex(Index: Integer): Integer;
begin
  result := (2 * Index) + 1;
end;

function THeap<T>.GetRightChildIndex(Index: Integer): Integer;
begin
  result := (2 * Index) + 2;
end;

procedure THeap<T>.HeapifyUp(Index: integer);
var
  ParentIndex: Integer;
  Temp: T;
begin
  while Index > 0 do
  begin
    ParentIndex := GetParentIndex(Index);
    if not Hikaku(FItems[Index], FItems[ParentIndex]) then
      Break;
    Temp := FItems[Index];
    FItems[Index] := FItems[ParentIndex];
    FItems[ParentIndex] := Temp;
    Index := ParentIndex;
  end;
end;

function THeap<T>.Hikaku(Little, Large: T): Boolean;
begin
  result := FFunc(Little, Large);
end;

function THeap<T>.IndexOf(const Value: T): Integer;
begin
  result := FItems.IndexOf(Value);
end;

procedure THeap<T>.HeapifyDown(Index: integer);
var
  LeftChild, RightChild, SmallestChild: Integer;
  Temp: T;
begin
  while true do
  begin
    LeftChild := GetLeftChildIndex(Index);
    RightChild := GetRightChildIndex(Index);
    SmallestChild := Index;

    if (LeftChild < FItems.Count) and Hikaku(FItems[LeftChild],
      FItems[SmallestChild]) then
      SmallestChild := LeftChild;

    if (RightChild < FItems.Count) and Hikaku(FItems[RightChild],
      FItems[SmallestChild]) then
      SmallestChild := RightChild;

    if SmallestChild = Index then
      Break;

    Temp := FItems[Index];
    FItems[Index] := FItems[SmallestChild];
    FItems[SmallestChild] := Temp;
    Index := SmallestChild;
  end;
end;

procedure THeap<T>.Add(Item: T);
begin
  FItems.Add(Item);
  HeapifyUp(FItems.Count-1);
end;

function THeap<T>.Extract: T;
begin
  if FItems.Count = 0 then
    raise Exception.Create('Heap is empty.');

  result := FItems[0];
  FItems[0] := FItems[FItems.Count - 1];
  FItems.Delete(FItems.Count - 1);
  HeapifyDown(0);
end;

function THeap<T>.Peek: T;
begin
  if FItems.Count = 0 then
    raise Exception.Create('Heap is empty.');

  result := FItems[0];
end;

function THeap<T>.GetCount: Integer;
begin
  result := FItems.Count;
end;

end.
