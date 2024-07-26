object Form4: TForm4
  Left = 0
  Top = 991
  Margins.Left = 1
  Margins.Top = 1
  Margins.Right = 1
  Margins.Bottom = 1
  Caption = 'Test SQL Parser'
  ClientHeight = 589
  ClientWidth = 1001
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  DesignSize = (
    1001
    589)
  TextHeight = 13
  object Memo1: TMemo
    Left = 6
    Top = 176
    Width = 571
    Height = 122
    HideSelection = False
    Lines.Strings = (
      
        'SELECT Year(t1.date), * FROM t1 AS table1 INNER JOIN table2 ON t' +
        'able1.f1 = table2.f2 WHERE (table1.id = '
      '12);')
    TabOrder = 0
    OnChange = Memo1Change
  end
  object Memo2: TMemo
    Left = 6
    Top = 304
    Width = 571
    Height = 226
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button1: TButton
    Left = 544
    Top = 536
    Width = 75
    Height = 26
    Caption = 'Decode SQL'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 376
    Top = 536
    Width = 131
    Height = 26
    Caption = 'Decode SQL From File'
    TabOrder = 3
    OnClick = Button2Click
  end
  object DBGrid1: TDBGrid
    Left = 6
    Top = 16
    Width = 571
    Height = 145
    DataSource = dsTestSQLStatements
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 67
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Dialect'
        Width = 60
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Statements'
        Width = 67
        Visible = True
      end>
  end
  object DBGrid2: TDBGrid
    Left = 583
    Top = 16
    Width = 413
    Height = 514
    Anchors = [akLeft, akTop, akRight]
    DataSource = dsSQLStatementTokens
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PositionNo'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenTypeName'
        Width = 122
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenText'
        Width = 80
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenCurrentlyDecodedAs'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenMatch'
        Width = 131
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TokenCurrentString'
        Visible = True
      end>
  end
  object Button3: TButton
    Left = 160
    Top = 536
    Width = 203
    Height = 26
    Caption = 'Test SQL and Compare from DB'
    TabOrder = 6
    OnClick = Button3Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 570
    Width = 1001
    Height = 19
    Panels = <
      item
        Width = 20
      end>
  end
  object Button4: TButton
    Left = 656
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 8
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 760
    Top = 536
    Width = 75
    Height = 25
    Caption = 'ResetDB'
    TabOrder = 9
    OnClick = Button5Click
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
    object tblTestSQLStatementTokensTokenMatch: TStringField
      FieldKind = fkCalculated
      FieldName = 'TokenMatch'
      Calculated = True
    end
  end
  object dsSQLStatementTokens: TDataSource
    DataSet = tblResultsTokens
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
  object tblResultsTokens: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 440
    Top = 56
    object tblResultsTokensPositionNo: TIntegerField
      FieldName = 'PositionNo'
    end
    object tblResultsTokensTokenID: TStringField
      FieldName = 'TokenID'
    end
    object tblResultsTokensTokenTypeName: TStringField
      FieldName = 'TokenTypeName'
      Size = 30
    end
    object tblResultsTokensTokenText: TStringField
      FieldName = 'TokenText'
    end
    object tblResultsTokensTokenMatch: TStringField
      FieldName = 'TokenMatch'
    end
    object tblResultsTokensTokenCurrentlyDecodedAs: TStringField
      FieldName = 'TokenCurrentlyDecodedAs'
      Size = 30
    end
    object tblResultsTokensTokenCurrentString: TStringField
      FieldName = 'TokenCurrentString'
      Size = 40
    end
  end
end
