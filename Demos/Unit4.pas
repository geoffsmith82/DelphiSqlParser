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
  , Data.DB.Parser
  ;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    FDConnection1: TFDConnection;
    tblTestSQLStatements: TFDTable;
    dsTestSQLStatements: TDataSource;
    DBGrid1: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure tblTestSQLStatementsAfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
    value : TSQLParser;
  public
    { Public declarations }
    procedure ProcessSQL(SQL: string);
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
  FDConnection1.Params.Database := filename;
  FDConnection1.Connected := True;
  tblTestSQLStatements.Active := True;
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
        if value.FTokens[j].TokenSQL = -199 then
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

procedure TForm4.ProcessSQL(SQL: string);
var
  i : Integer;
begin
  value.ProcessSQL(SQL);

  for i := 0 to value.FTokens.Count - 1 do
  begin
    if value.FTokens[i].TokenSQL = -199 then
      Memo2.Lines.Add('============= ' + value.FTokens[i].token + ' ' + value.FTokens[i].TokenSQL.ToString)
    else
    Memo2.Lines.Add(value.FTokens[i].token + ' ' + value.FTokens[i].TokenSQL.ToString);
  end;
end;

procedure TForm4.tblTestSQLStatementsAfterScroll(DataSet: TDataSet);
var
  undecodedCount : Integer;
  j : Integer;
begin
  if not Assigned(value) then
      value := TSQLParser.Create;
  Memo1.Clear;
  Memo1.Lines.Text := DataSet.FieldByName('Statements').AsString;
  undecodedCount := 0;
  ProcessSQL(Memo1.Lines.Text);
  for j := 0 to value.FTokens.Count - 1 do
  begin
    if value.FTokens[j].TokenSQL = -199 then
      Inc(undecodedCount);
  end;
  if value.DoesStatementModifyDB then
  begin
    Memo2.Lines.Add('Statement Modifies Database');
  end;
  if value.IsDDL then
  begin
    Memo2.Lines.Add('Statement is DDL');
  end;
  Memo2.Lines.Add('Missed Decoding :' + undecodedCount.ToString);
end;

end.

