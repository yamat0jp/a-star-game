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
    FState: integer;
    FX: integer;
    FY: integer;
    FP: TData;
    function GetScore: integer;
  public
    property state: integer read FState write FState;
    property x: integer read FX write FX;
    property y: integer read FY write FY;
    property cost: integer read FCost write FCost;
    property hcost: integer read FHcost write FHcost;
    property p: TData read FP write FP;
    property map: integer read FMap write FMap;
    property score: integer read GetScore;
  end;

  TGrid = class
  const
    max = 15;
  private
    function GetState(x, y: integer): TData;
    procedure SetState(x, y: integer; const Value: TData);
  protected
    FGrid: array [0 .. max - 1, 0 .. max - 1] of TData;
  public
    constructor Create;
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
  none = 0;
  open = 1;
  closed = 2;

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
  result := true;
  case data.state of
    open:
      if data.cost <= cost + data.map then
        result := false
      else
        data.cost := cost + data.map;
    none:
      begin
        data.cost := cost + data.map;
        data.state := open;
        data.hcost := Abs(gt.x - data.x) + Abs(gt.y - data.y);
        list.Add(data);
      end;
  end;
end;

procedure a_star;
var
  x, y, id: integer;
  data: TData;
  items: array [0 .. 3] of TData;
begin
  if list.Count > 0 then
  begin
    data := list[0];
    data.state := closed;
    list.Delete(0);
  end
  else
    Exit;
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
    if Assigned(d) and (d.map < tilt) and (d.state <> closed) then
      if open_op(d, data.cost) then
        d.p := data;
  id := get_small;
  if id > -1 then
    list.Move(id, 0);
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
  grid := TGrid.Create;
  list := TList<TData>.Create;
  st := Point(4 - 1, 5 - 1);
  gt := Point(7 - 1, 6 - 1);
  data := grid[st.x, st.y];
  data.state := open;
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

constructor TGrid.Create;
var
  data: TData;
begin
  inherited;
  for var i := 0 to max - 1 do
    for var j := 0 to max - 1 do
    begin
      data := TData.Create;
      data.state := none;
      data.x := i;
      data.y := j;
      data.cost := 0;
      data.hcost := 0;
      data.p := nil;
      data.map := 1;
      FGrid[i, j] := data;
    end;
  make;
end;

destructor TGrid.Destroy;
begin
  for var i := 0 to max - 1 do
    for var j := 0 to max - 1 do
      FGrid[i, j].Free;
  inherited;
end;

function TGrid.GetState(x, y: integer): TData;
begin
  if (x >= 0) and (x < max) and (y >= 0) and (y < max) then
    result := FGrid[x, y]
  else
    result := nil;
end;

procedure TGrid.make;
begin
  for var i := 4 to 6 do
    FGrid[6 - 1, i - 1].map := tilt;

  FGrid[5, 6].map := tilt;
  FGrid[6, 4].map := tilt;
  {
    FGrid[7, 5].map := tilt;
    FGrid[6, 6].map := tilt; }
end;

procedure TGrid.SetState(x, y: integer; const Value: TData);
begin
  if (x >= 0) and (x < max) and (y >= 0) and (y < max) then
    FGrid[x, y] := Value;
end;

end.
