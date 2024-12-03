object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object DrawGrid1: TDrawGrid
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Align = alClient
    ColCount = 10
    DefaultColWidth = 32
    DefaultRowHeight = 32
    FixedCols = 0
    RowCount = 10
    FixedRows = 0
    ScrollBars = ssNone
    TabOrder = 0
    OnDrawCell = DrawGrid1DrawCell
    OnFixedCellClick = DrawGrid1FixedCellClick
  end
end
