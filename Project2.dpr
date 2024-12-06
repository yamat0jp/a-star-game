program Project2;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Heap in 'Heap.pas';

var
  Heap: THeap<integer>;

begin
  try
    { TODO -oUser -cConsole メイン : ここにコードを記述してください }
    Heap := THeap<integer>.Create(
      function(L, R: integer): Boolean
      begin
        result := L < R;
      end);
    for var i in [10, 20, 10, 5, 1, 2, 3, 9, 8, 7] do
      Heap.Add(i);
    while Heap.Count > 0 do
      Writeln(Heap.Extract);
    Readln;
    Heap.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
