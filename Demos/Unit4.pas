unit Unit4;

interface

uses
    Winapi.Windows
  , Winapi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.Grids
  , Vcl.DBGrids
  , Data.DB
  , FireDAC.Stan.Intf
  , FireDAC.Stan.Option
  , FireDAC.Stan.Error
  , FireDAC.UI.Intf
  , FireDAC.Phys.Intf
  , FireDAC.Stan.Def
  , FireDAC.Stan.Pool
  , FireDAC.Stan.Async
  , FireDAC.Phys
  , FireDAC.Phys.MSAcc
  , FireDAC.Phys.MSAccDef
  , FireDAC.VCLUI.Wait
  , FireDAC.Comp.Client
  , FireDAC.Stan.Param
  , FireDAC.DatS
  , FireDAC.DApt.Intf
  , FireDAC.DApt
  , FireDAC.Comp.DataSet
  , FireDAC.Phys.FB
  , FireDAC.Phys.FBDef
  , FireDAC.Phys.MySQL
  , FireDAC.Phys.MySQLDef
  , FireDAC.Phys.MSSQL
  , FireDAC.Phys.MSSQLDef
  , Data.DB.Parser
  , Vcl.ComCtrls
  , RTTI
  , System.TypInfo
  , RegularExpressions
  ;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    AccessConnection: TFDConnection;
    FirebirdConnection: TFDConnection;
    MySqlConnection: TFDConnection;
    MSSQLConnection: TFDConnection;
    tblTestSQLStatements: TFDTable;
    dsTestSQLStatements: TDataSource;
    DBGrid1: TDBGrid;
    tblTestSQLStatementTokens: TFDTable;
    dsSQLStatementTokens: TDataSource;
    DBGrid2: TDBGrid;
    tblTestSQLStatementsID: TFDAutoIncField;
    tblTestSQLStatementsDialect: TWideStringField;
    tblTestSQLStatementsStatements: TWideMemoField;
    tblTestSQLStatementTokensTestSqlId: TFDAutoIncField;
    tblTestSQLStatementTokensStatementID: TIntegerField;
    tblTestSQLStatementTokensPositionNo: TIntegerField;
    tblTestSQLStatementTokensTokenText: TWideStringField;
    tblTestSQLStatementTokensTokenID: TIntegerField;
    tblTestSQLStatementTokensTokenTypeName: TStringField;
    Button3: TButton;
    tblTestSQLStatementTokensTokenMatch: TStringField;
    StatusBar1: TStatusBar;
    tblResultsTokens: TFDMemTable;
    tblResultsTokensPositionNo: TIntegerField;
    tblResultsTokensTokenID: TStringField;
    tblResultsTokensTokenTypeName: TStringField;
    tblResultsTokensTokenText: TStringField;
    tblResultsTokensTokenMatch: TStringField;
    Button4: TButton;
    tblResultsTokensTokenCurrentlyDecodedAs: TStringField;
    Button5: TButton;
    tblResultsTokensTokenCurrentString: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure tblTestSQLStatementsAfterScroll(DataSet: TDataSet);
    procedure tblTestSQLStatementTokensCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
    value : TSQLParser;
  public
    { Public declarations }
    function ProcessSQL(const SQL: string): Integer;
  end;

  TIntFieldHelper = class helper for TField
  private
    procedure SetTokenType(const Value: TTokenTypes);
    function AsTTokenType: TTokenTypes;
  published
    property AsTokenType: TTokenTypes read AsTTokenType write SetTokenType;
  end;

  TTokenHelper = record helper for TTokenTypes
    function ToString: string;
    function AsInteger: Integer;
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses
    System.IOUtils
  ;

procedure TForm4.FormCreate(Sender: TObject);
var
  filename : string;
begin
  value := TSQLParser.Create;
  filename := ExtractFilePath(ParamStr(0));
  filename := TPath.Combine(filename, '..\..\..\Source\SQLParserDB.mdb');
  AccessConnection.Params.Database := filename;
  AccessConnection.Connected := True;
  tblTestSQLStatements.Active := True;
  tblTestSQLStatementTokens.Active := True;
  tblTestSQLStatements.AfterScroll := tblTestSQLStatementsAfterScroll;

end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  ProcessSQL(Memo1.Text);
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  statements : TStringList;
  filename : string;
  I: Integer;
  GoodCount : Integer;
  j: Integer;
  undecodedCount : Integer;
begin
  GoodCount := 0;
  statements := TStringList.Create;
  try
    filename := ExtractFilePath(ParamStr(0));
    filename := TPath.Combine(filename, '..\..\..\Source\sql.txt');
    statements.LoadFromFile(filename);
    for I := 0 to statements.Count - 1 do
    begin
      undecodedCount := 0;
      ProcessSQL(statements[i]);
      for j := 0 to value.FTokens.Count - 1 do
      begin
        if value.FTokens[j].TokenSQL = tkUnknownToken then
          Inc(undecodedCount);
      end;
      Memo2.Lines.Add('Missed Decoding :' + undecodedCount.ToString);
      if undecodedCount = 0 then
        Inc(GoodCount);
    end;
    Memo2.Lines.Add('Total Good Statements :' + GoodCount.ToString);
    Memo2.Lines.Add('Total Statements :' + statements.Count.ToString);
  finally
    FreeAndNil(statements);
  end;
end;

procedure TForm4.Button3Click(Sender: TObject);
var
  i: Integer;
begin
   tblTestSQLStatementsAfterScroll(tblTestSQLStatements);
  repeat
    Assert(value.FTokens.Count  = tblTestSQLStatementTokens.RecordCount);
    for i := 0 to value.FTokens.Count - 1 do
    begin
      OutputDebugString(PChar('i = ' + i.ToString));
      if tblTestSQLStatementTokens.Locate('PositionNo', i, []) then
      begin
        Assert(value.FTokens[i].Token = tblTestSQLStatementTokensTokenText.AsString);
        Assert(value.FTokens[i].TokenSQL = tblTestSQLStatementTokensTokenID.AsTokenType);
      end;
    end;

    tblTestSQLStatements.Next;
  until tblTestSQLStatements.Eof;
end;

procedure TForm4.Memo1Change(Sender: TObject);
begin
//  OutputDebugString(PChar('SelStart:' + Memo1.SelStart.ToString));
end;


function TokenizeSQL(const SQL: string): TArray<string>;
var
  Tokens: TArray<string>;
  Matches: TMatchCollection;
  Match: TMatch;
  RegEx: TRegEx;
  i : Integer;
begin
  RegEx := TRegEx.Create('\b\w+\b|[^\w\s]+', [roMultiLine]);
  Matches := RegEx.Matches(SQL);
  SetLength(Tokens, Matches.Count);
  for i := 0 to Matches.Count - 1 do
  begin
    Match := Matches.Item[i];
    Tokens[i] := Match.Value;
  end;
  Result := Tokens;
end;


procedure TForm4.Button4Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 170 do
  begin
    try
      Memo1.Lines.Add(value.TokenIdToTokenString(TTokenTypes(i)) + ' = ' + i.ToString);
    except

    end;
  end;
end;

procedure TForm4.Button5Click(Sender: TObject);
begin
  tblTestSQLStatementTokens.First;
  repeat
    tblTestSQLStatementTokens.Delete;
  until tblTestSQLStatementTokens.Eof;
end;

function TForm4.ProcessSQL(const SQL: string): Integer;
var
  i : Integer;
  v : TArray<string>;
begin
  try
    Result := value.ProcessSQL(SQL);
  except

  end;
  Memo2.LockDrawing;
  try
  for i := 0 to value.FTokens.Count - 1 do
  begin
    if value.FTokens[i].TokenSQL = tkUnknownToken then
      Memo2.Lines.Add(i.ToString + '   ============= ' + value.FTokens[i].token + ' ' + value.FTokens[i].TokenSQL.ToString)
    else
    Memo2.Lines.Add(i.ToString +  '   ' + value.FTokens[i].token + ' ' + value.FTokens[i].TokenSQL.AsInteger.ToString);
  end;
    Memo2.Lines.Add('///////////////////////////');
  v := TokenizeSQL(SQL);
  for i := 0 to length(v) - 1 do
  begin
    Memo2.Lines.Add(v[i]);
  end;
  finally
    Memo2.UnlockDrawing;
  end;

end;

procedure TForm4.tblTestSQLStatementsAfterScroll(DataSet: TDataSet);
var
  undecodedCount : Integer;
  j : Integer;
  errPos : Integer;
begin
  if not Assigned(value) then
      value := TSQLParser.Create;
  Memo1.Clear;
  Memo2.Clear;
  Memo1.Lines.Text := DataSet.FieldByName('Statements').AsString;
  undecodedCount := 0;
  errPos := ProcessSQL(Memo1.Lines.Text);
  if errPos > 0 then
  begin
    Memo1.SelStart := errPos;
    Memo1.SelLength := 100;
  end;

  for j := 0 to value.FTokens.Count - 1 do
  begin
    if value.FTokens[j].TokenSQL = tkUnknownToken then
      Inc(undecodedCount);
  end;
  if tblTestSQLStatementTokens.RecordCount = 0 then
  begin
    for j := 0 to value.FTokens.Count - 1 do
    begin
      tblTestSQLStatementTokens.Append;
      tblTestSQLStatementTokens.FieldByName('StatementID').AsInteger := tblTestSQLStatementsID.AsInteger;
      tblTestSQLStatementTokens.FieldByName('PositionNo').AsInteger := j;
      if j < value.FTokens.Count then
      begin
        tblTestSQLStatementTokens.FieldByName('TokenText').AsString := value.FTokens[j].Token;
        tblTestSQLStatementTokens.FieldByName('TokenID').AsTokenType := value.FTokens[j].TokenSQL;
      end;
      tblTestSQLStatementTokens.Post;
    end;
  end;

  tblResultsTokens.Active := False;
  tblResultsTokens.Active := True;

  tblTestSQLStatementTokens.First;
  repeat
    tblResultsTokens.Append;
    tblResultsTokens.FieldByName('PositionNo').AsInteger := tblTestSQLStatementTokens.FieldByName('PositionNo').AsInteger;
    tblResultsTokens.FieldByName('TokenText').AsString := tblTestSQLStatementTokens.FieldByName('TokenText').AsString;
    tblResultsTokens.FieldByName('TokenID').AsInteger := tblTestSQLStatementTokens.FieldByName('TokenID').AsInteger;
    tblResultsTokens.FieldByName('TokenTypeName').AsString := value.TokenIdToTokenString(tblResultsTokens.FieldByName('TokenID').AsTTokenType);
    tblResultsTokens.FieldByName('TokenCurrentlyDecodedAs').AsString := value.TokenIdToTokenString(value.FTokens.LookupTokenByPosition(tblTestSQLStatementTokens.FieldByName('PositionNo').AsInteger).TokenSQL);
    tblResultsTokens.FieldByName('TokenCurrentString').AsString := value.FTokens.LookupTokenByPosition(tblTestSQLStatementTokens.FieldByName('PositionNo').AsInteger).Token;

    if tblResultsTokens.FieldByName('TokenID').AsInteger = Ord(value.FTokens.LookupTokenByPosition(tblTestSQLStatementTokens.FieldByName('PositionNo').AsInteger).TokenSQL) then
    begin
      tblResultsTokens.FieldByName('TokenMatch').AsString := 'Match';
    end
    else
    begin
      tblResultsTokens.FieldByName('TokenMatch').AsString := '.';
    end;

    tblResultsTokens.Post;
    tblTestSQLStatementTokens.Next;
  until tblTestSQLStatementTokens.Eof;



  if value.DoesStatementModifyDB then
  begin
    Memo2.Lines.Add('Statement Modifies Database');
  end;
  if value.DoesDoubleConstantExpressionExist then
  begin
    Memo2.Lines.Add('Double Constant Expression EXISTS!!!!!!!!!!!!!!');
  end;
  if value.StatementCount > 1 then
  begin
    Memo2.Lines.Add('MULTIPLE STATEMENTS EXISTS!!!!!!!!!!!!!!');
  end;

  if value.IsDDL then
  begin
    Memo2.Lines.Add('Statement is DDL');
  end;
  Memo2.Lines.Add('Missed Decoding :' + undecodedCount.ToString);
end;

procedure TForm4.tblTestSQLStatementTokensCalcFields(DataSet: TDataSet);
var
  TokenID : TTokenTypes;
  TokenCount : Integer;
  PositionNo : Integer;
begin
  DataSet.FieldByName('TokenTypeName').AsString := value.TokenIdToTokenString(DataSet.FieldByName('TokenID').AsTTokenType);
  PositionNo := 0;

  if DataSet.Active and (value.TokenCount > 0) and
    (value.TokenCount > tblTestSQLStatementTokens.FieldByName('PositionNo').AsInteger) then
  begin
    TokenCount := value.TokenCount;
    PositionNo := DataSet.FieldByName('PositionNo').AsInteger;
    if PositionNo > 300 then
      Exit;
    TokenID := value.FTokens.Items[PositionNo].TokenSQL;
//    value.FTokens.T
    if TokenID = DataSet.FieldByName('TokenID').AsTokenType then
      DataSet.FieldByName('TokenMatch').AsString := 'Matches'
    else
      DataSet.FieldByName('TokenMatch').AsString := '.';
  end;
end;

{ TIntFieldHelper }

function TIntFieldHelper.AsTTokenType: TTokenTypes;
begin
  Result := TTokenTypes(AsInteger);
end;

procedure TIntFieldHelper.SetTokenType(const Value: TTokenTypes);
begin
  AsInteger := Ord(Value);
end;

{ TTokenHelper }

function TTokenHelper.AsInteger: Integer;
begin
  Result := Ord(Self);
end;

function TTokenHelper.ToString: string;
begin
  if self = tkUnknownToken then
    Result := 'tkUnknownToken'
  else
    Result := GetEnumName(TypeInfo(TTokenHelper), Ord(Self));
end;

end.

