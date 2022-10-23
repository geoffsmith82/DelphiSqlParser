object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Test SQL Parser'
  ClientHeight = 1534
  ClientWidth = 2478
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
    Left = 14
    Top = 440
    Width = 1427
    Height = 304
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Lines.Strings = (
      
        'SELECT Year(t1.date), * FROM t1 AS table1 INNER JOIN table2 ON t' +
        'able1.f1 = table2.f2 WHERE (table1.id = '
      '12);')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 13
    Top = 760
    Width = 1428
    Height = 566
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
    Height = 64
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
    Height = 64
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Decode SQL From File'
    TabOrder = 3
    OnClick = Button2Click
  end
  object DBGrid1: TDBGrid
    Left = 13
    Top = 40
    Width = 1428
    Height = 362
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
        Width = 128
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Dialect'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Statements'
        Width = 128
        Visible = True
      end>
  end
  object DBGrid2: TDBGrid
    Left = 1457
    Top = 40
    Width = 904
    Height = 581
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    DataSource = dsSQLStatementTokens
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -28
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PositionNo'
        Width = 160
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenID'
        Width = 160
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenText'
        Width = 200
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenTypeName'
        Visible = True
      end>
  end
  object Button3: TButton
    Left = 400
    Top = 1340
    Width = 508
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Test SQL and Compare from DB'
    TabOrder = 6
    OnClick = Button3Click
  end
  object AccessConnection: TFDConnection
    Params.Strings = (
      'Database=D:\Programming\DelphiSQLParser\Source\SQLParserDB.mdb'
      'DriverID=MSAcc')
    LoginPrompt = False
    Left = 352
    Top = 120
  end
  object tblTestSQLStatements: TFDTable
    IndexFieldNames = 'ID'
    Connection = AccessConnection
    TableName = 'TestSQLStatements'
    Left = 120
    Top = 200
    object tblTestSQLStatementsID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object tblTestSQLStatementsDialect: TWideStringField
      FieldName = 'Dialect'
      Origin = 'Dialect'
      Size = 255
    end
    object tblTestSQLStatementsStatements: TWideMemoField
      FieldName = 'Statements'
      Origin = 'Statements'
      BlobType = ftWideMemo
    end
  end
  object dsTestSQLStatements: TDataSource
    DataSet = tblTestSQLStatements
    Left = 124
    Top = 420
  end
  object tblTestSQLStatementTokens: TFDTable
    OnCalcFields = tblTestSQLStatementTokensCalcFields
    IndexName = 'StatementID'
    MasterSource = dsTestSQLStatements
    MasterFields = 'ID'
    Connection = AccessConnection
    TableName = 'TestSQLStatementsTokens'
    Left = 600
    Top = 220
    object tblTestSQLStatementTokensTestSqlId: TFDAutoIncField
      FieldName = 'TestSqlId'
      Origin = 'TestSqlId'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object tblTestSQLStatementTokensStatementID: TIntegerField
      FieldName = 'StatementID'
      Origin = 'StatementID'
    end
    object tblTestSQLStatementTokensPositionNo: TIntegerField
      FieldName = 'PositionNo'
      Origin = 'PositionNo'
    end
    object tblTestSQLStatementTokensTokenText: TWideStringField
      FieldName = 'TokenText'
      Origin = 'TokenText'
      Size = 255
    end
    object tblTestSQLStatementTokensTokenID: TIntegerField
      FieldName = 'TokenID'
      Origin = 'TokenID'
    end
    object tblTestSQLStatementTokensTokenTypeName: TStringField
      FieldKind = fkCalculated
      FieldName = 'TokenTypeName'
      Calculated = True
    end
  end
  object dsSQLStatementTokens: TDataSource
    DataSet = tblTestSQLStatementTokens
    Left = 736
    Top = 80
  end
  object FirebirdConnection: TFDConnection
    Params.Strings = (
      'DriverID=FB'
      'User_Name=sysdba'
      'Password=masterkey')
    LoginPrompt = False
    Left = 1552
    Top = 600
  end
  object MySqlConnection: TFDConnection
    Params.Strings = (
      'DriverID=MySQL')
    LoginPrompt = False
    Left = 1552
    Top = 760
  end
  object MSSQLConnection: TFDConnection
    Params.Strings = (
      'DriverID=MSSQL')
    LoginPrompt = False
    Left = 1572
    Top = 920
  end
end
