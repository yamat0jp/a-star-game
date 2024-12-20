unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids;

type
  TData = class
  private
    FCost: integer;
    FHcost: integer;
    FMap: integer;
    FClosed: Boolean;
    FX: integer;
    FY: integer;
    FP: TData;
    function GetScore: integer;
  public
    property closed: Boolean read FClosed write FClosed;
    property x: integer read FX write FX;
    property y: integer read FY write FY;
    property cost: integer read FCost write FCost;
    property hcost: integer read FHcost write FHcost;
    property p: TData read FP write FP;
    property map: integer read FMap write FMap;
    property score: integer read GetScore;
  end;

  TGrid = class
  private
    function GetState(x, y: integer): TData;
    procedure SetState(x, y: integer; const Value: TData);
  protected
    FGrid: array of TData;
    FCol, FRow: integer;
    property col: integer read FCol write FCol;
    property row: integer read FRow write FRow;
  public
    constructor Create(x, y: integer);
    destructor Destroy; override;
    procedure make; virtual;
    property state[x, y: integer]: TData read GetState write SetState; default;
  end;

  TForm1 = class(TForm)
    DrawGrid1: TDrawGrid;
    procedure DrawGrid1FixedCellClick(Sender: TObject; ACol, ARow: LongInt);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; state: TGridDrawState);
  private
    { Private �錾 }
    procedure answer;
  public
    { Public �錾 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.Types, System.Contnrs, System.Math, Heap;

type
  THeapData = class(THeap<TData>)
  protected
    // function Hikaku(Little, Large: TData): Boolean; override;
  public
    constructor Create;
    procedure Update(Item: TData);
  end;

const
  tilt = 10;

var
  st, gt: TPoint;
  grid: TGrid;
  list: THeapData;
  ans: array [0 .. 999] of TData;

function open_op(data: TData; cost: integer): Boolean;
begin
  if (data.closed = false) and (list.IndexOf(data) = -1) then
  begin
    data.cost := cost + data.map;
    data.hcost := Abs(gt.x - data.x) + Abs(gt.y - data.y);
    list.Add(data);
    result := true;
    Exit;
  end;
  if data.cost <= cost + data.map then
    result := false
  else
  begin
    data.cost := cost + data.map;
    data.closed := false;
    list.Update(data);
    result := true;
  end;
end;

procedure a_star;
var
  x, y: integer;
  data: TData;
  items: array [0 .. 3] of TData;
begin
  repeat
    if list.Count = 0 then
      Exit;
    data := list.Extract;
  until not data.closed;
  data.closed := true;
  x := data.x;
  y := data.y;
  if gt = Point(x, y) then
  begin
    Form1.answer;
    Exit;
  end;
  items[0] := grid[x + 1, y];
  items[1] := grid[x, y - 1];
  items[2] := grid[x - 1, y];
  items[3] := grid[x, y + 1];
  for var d in items do
    if Assigned(d) and (d.map < tilt) then
      if open_op(d, data.cost) then
        d.p := data;
  a_star;
end;

procedure TForm1.answer;
var
  i: integer;
  data: TData;
begin
  data := grid[gt.x, gt.y];
  i := 0;
  while data.p <> nil do
  begin
    data := data.p;
    ans[i] := data;
    inc(i);
  end;
end;

procedure TForm1.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: LongInt;
  Rect: TRect; state: TGridDrawState);
var
  data: TData;
begin
  DrawGrid1.Canvas.Brush.Color := clGray;
  if grid[ACol, ARow].map = tilt then
    DrawGrid1.Canvas.Rectangle(Rect);
  DrawGrid1.Canvas.Brush.Color := clAqua;
  for var p in ans do
    if p = grid[ACol, ARow] then
      DrawGrid1.Canvas.Rectangle(Rect);
  DrawGrid1.Canvas.Brush.Color := clGreen;
  if (st = Point(ACol, ARow)) or (gt = Point(ACol, ARow)) then
    DrawGrid1.Canvas.Rectangle(Rect);
  DrawGrid1.Canvas.Brush.Color := clWhite;
  data := grid[ACol, ARow];
  if data.cost > 0 then
    DrawGrid1.Canvas.TextOut(Rect.Left, Rect.Top,
      Format('%d(%d)', [data.cost, data.score]));
end;

procedure TForm1.DrawGrid1FixedCellClick(Sender: TObject; ACol, ARow: LongInt);
begin
  grid[ACol, ARow].map := tilt;
  DrawGrid1.Repaint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  grid := TGrid.Create(DrawGrid1.ColCount, DrawGrid1.RowCount);
  list := THeapData.Create;
  st := Point(3, 3);
  gt := Point(8, 6);
  list.Add(grid[st.x, st.y]);
  a_star;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  grid.Free;
  list.Free;
end;

{ TData }

function TData.GetScore: integer;
begin
  result := cost + hcost;
end;

{ TGrid }

constructor TGrid.Create(x, y: integer);
var
  data: TData;
begin
  inherited Create;
  FCol := x;
  FRow := y;
  SetLength(FGrid, x * y);
  for var i := 0 to High(FGrid) do
  begin
    data := TData.Create;
    data.closed := false;
    data.x := i mod x;
    data.y := i div x;
    data.cost := 0;
    data.hcost := 0;
    data.p := nil;
    data.map := 1;
    FGrid[i] := data;
  end;
  make;
end;

destructor TGrid.Destroy;
begin
  for var d in FGrid do
    d.Free;
  Finalize(FGrid);
  inherited;
end;

function TGrid.GetState(x, y: integer): TData;
begin
  if (x >= 0) and (x < col) and (y >= 0) and (y < row) then
    result := FGrid[y * col + x]
  else
    result := nil;
end;

procedure TGrid.make;
begin
  for var i := 2 to 6 do
    state[6, i].map := tilt;
  for var i := 3 to 5 do
    state[i, 6].map := tilt;
  {
    state[7, 5].map := tilt;
    state[6, 6].map := tilt; }
end;

procedure TGrid.SetState(x, y: integer; const Value: TData);
begin
  if (x >= 0) and (x < col) and (y >= 0) and (y < row) then
    FGrid[y * col + x] := Value;
end;

{ THeapData }

constructor THeapData.Create;
begin
  inherited Create(
    function(Little, Large: TData): Boolean
    begin
      if Little.score < Large.score then
        result := true
      else if Little.score > Large.score then
        result := false
      else
        result := Little.hcost < Large.hcost;
    end);
end;

procedure THeapData.Update(Item: TData);
var
  id: integer;
begin
  id := FItems.IndexOf(Item);
  HeapifyUp(id);
  HeapifyDown(id);
end;

end.
