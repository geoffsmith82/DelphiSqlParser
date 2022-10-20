object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Test SQL Parser'
  ClientHeight = 1493
  ClientWidth = 1650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 240
  TextHeight = 34
  object Memo1: TMemo
    Left = 13
    Top = 439
    Width = 1583
    Height = 305
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Lines.Strings = (
      
        'SELECT Year(t1.date), * FROM t1 AS table1 INNER JOIN table2 ON t' +
        'able1.f1 = table2.f2 WHERE (table1.id = 12);')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 20
    Top = 760
    Width = 1583
    Height = 565
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 1360
    Top = 1340
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Decode SQL'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 940
    Top = 1340
    Width = 328
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Decode SQL From File'
    TabOrder = 3
    OnClick = Button2Click
  end
  object DBGrid1: TDBGrid
    Left = 60
    Top = 40
    Width = 1536
    Height = 361
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    DataSource = dsTestSQLStatements
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -28
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Statements'
        Visible = True
      end>
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=D:\Programming\DelphiSQLParser\Source\SQLParserDB.mdb'
      'DriverID=MSAcc')
    LoginPrompt = False
    Left = 352
    Top = 120
  end
  object tblTestSQLStatements: TFDTable
    AfterScroll = tblTestSQLStatementsAfterScroll
    IndexFieldNames = 'ID'
    Connection = FDConnection1
    TableName = 'TestSQLStatements'
    Left = 80
    Top = 160
  end
  object dsTestSQLStatements: TDataSource
    DataSet = tblTestSQLStatements
    Left = 124
    Top = 420
  end
end
