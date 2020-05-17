object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Test SQL Parser'
  ClientHeight = 597
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 633
    Height = 122
    Lines.Strings = (
      
        'SELECT Year(t1.date), * FROM t1 AS table1 INNER JOIN table2 ON t' +
        'able1.f1 = table2.f2 WHERE (table1.id = 12);')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 136
    Width = 633
    Height = 394
    TabOrder = 1
  end
  object Button1: TButton
    Left = 544
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Decode SQL'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 376
    Top = 536
    Width = 131
    Height = 25
    Caption = 'Decode SQL From File'
    TabOrder = 3
    OnClick = Button2Click
  end
end
