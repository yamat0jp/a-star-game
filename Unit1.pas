unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
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
    { Private êÈåæ }
    procedure answer;
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.Types, System.Generics.Collections, System.Contnrs, System.Math;

const
  tilt = 10;

var
  st, gt: TPoint;
  grid: TGrid;
  list: TList<TData>;
  ans: array [0 .. 999] of TData;

function get_small: integer;
var
  min, min_cost: integer;
  data: TData;
begin
  min := 999;
  min_cost := 999;
  result := -1;
  for var i := 0 to list.Count - 1 do
    if not list[i].closed then
    begin
      data := list[i];
      if min > data.score then
        min := data.score
      else if (min = data.score) and (min_cost > data.cost) then
        min_cost := data.cost
      else
        continue;
      result := i
    end;
end;

function open_op(data: TData; cost: integer): Boolean;
begin
  if list.IndexOf(data) = -1 then
  begin
    data.cost := cost + data.map;
    data.hcost := Abs(gt.x - data.x) + Abs(gt.y - data.y);
    list.Add(data);
  end;
  if data.cost < cost + data.map then
    result := false
  else
  begin
    data.cost := cost + data.map;
    data.closed := false;
    result := true;
  end;
end;

procedure a_star;
var
  x, y, id: integer;
  data: TData;
  items: array [0 .. 3] of TData;
begin
  id := get_small;
  if id = -1 then
    Exit;
  data := list[id];
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
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  data: TData;
begin
  grid := TGrid.Create(DrawGrid1.ColCount, DrawGrid1.RowCount);
  list := TList<TData>.Create;
  st := Point(3, 3);
  gt := Point(8, 6);
  data := grid[st.x, st.y];
  list.Add(data);
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

end.
