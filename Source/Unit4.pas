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
  , Data.DB.Parser
  , AnsiStrings
  ;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTokens: TTokenBucket;
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
begin
  FTokens := TTokenBucket.Create;
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
    filename := TPath.Combine(filename, '..\..\sql.txt');
    statements.LoadFromFile(filename);
    for I := 0 to statements.Count - 1 do
    begin
      undecodedCount := 0;
      ProcessSQL(statements[i]);
      for j := 0 to FTokens.Count - 1 do
      begin
        if FTokens[j].TokenSQL = -199 then
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
  parser: TParser;
  strStrm: TStringStream;
  token: string;
  info: TTokenInfo;
  i: Integer;
begin
  FTokens.Clear;
  strStrm := TStringStream.Create;
  strStrm.WriteString(SQL);
  strStrm.Position := 0;
  parser := TParser.Create(strStrm);
  repeat
    begin
      token := parser.TokenString;
{
    case parser.Token of
      toSymbol:
        Memo2.Lines.Add('Symbol');
      toInteger:
        Memo2.Lines.Add('Integer');
      toFloat:
        Memo2.Lines.Add('Float');
      System.Classes.toString:
        Memo2.Lines.Add('String');
      toWString:
        Memo2.Lines.Add('WString');
    else
        Memo2.Lines.Add('Something Else');
    end;
}
      info := TTokenInfo.Create;
      info.TokenType := parser.Token;
      info.Token := parser.TokenString;
      info.TokenSQL := TokenStringToTokenSQL(info);
//    Memo2.Lines.Add(token + ' ' + info.TokenSQL.ToString);
      FTokens.Add(info);
    end
  until (parser.NextToken = toEOF);

  i := 0;
  // Initial pass - join multi word commands together
  while i < FTokens.Count - 1 do
  begin
    if ((FTokens[i].TokenSQL = 4) and (FTokens[i + 1].TokenSQL = 7) and (FTokens[i + 2].TokenSQL = 8)) then
    begin
      FTokens.Join(i);
      FTokens.Join(i);  // RIGHT OUTER JOIN
      FTokens[i].TokenSQL := 79;
    end
    else if ((FTokens[i].TokenSQL = 5) and (FTokens[i + 1].TokenSQL = 8)) then
    begin
      FTokens.Join(i);  // INNER JOIN
      FTokens[i].TokenSQL := 80;
    end
    else if ((FTokens[i].TokenSQL = 3) and (FTokens[i + 1].TokenSQL = 8)) then
    begin
      FTokens.Join(i); // LEFT JOIN
      FTokens[i].TokenSQL := 81;
    end
    else if ((FTokens[i].TokenSQL = 4) and (FTokens[i + 1].TokenSQL = 8)) then
    begin
      FTokens.Join(i); // RIGHT JOIN
      FTokens[i].TokenSQL := 82;
    end
    else if ((FTokens[i].TokenSQL = 7) and (FTokens[i + 1].TokenSQL = 8)) then
    begin
      FTokens.Join(i); // OUTER JOIN
      FTokens[i].TokenSQL := 83;
    end
    else if ((FTokens[i].TokenSQL = 11) and (FTokens[i + 1].TokenSQL = 12)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 76;  // GROUP BY
    end
    else if ((FTokens[i].TokenSQL = 10) and (FTokens[i + 1].TokenSQL = 12)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 84;  // ORDER BY
    end
    else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 65)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 85;  // CREATE DATABASE
    end
    else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 65)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 112;  // DROP DATABASE
    end
    else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 60)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 109;  // CREATE VIEW
    end
    else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 60)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 113;  // DROP VIEW
    end
    else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 35)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 111;  // CREATE TABLE
    end
    else if ((FTokens[i].TokenSQL = 45) and (FTokens[i + 1].TokenSQL = 35)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 114;  // DROP TABLE
    end
    else if ((FTokens[i].TokenSQL = 46) and (FTokens[i + 1].TokenSQL = 35)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 116;  // TRUNCATE TABLE
    end
    else if ((FTokens[i].TokenSQL = 34) and (FTokens[i + 1].TokenSQL = 75)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 110;  // CREATE USER
    end
    else if ((FTokens[i].TokenSQL = 117) and (FTokens[i + 1].TokenSQL = 89)) then
    begin
      FTokens.Join(i);
      FTokens[i].TokenSQL := 110;  // IF EXISTS
    end;
    Inc(i);
  end;

  for i := 1 to FTokens.Count - 1 do
  begin
    if FTokens[i - 1].TokenSQL = 1  {'FROM'} then
      FTokens[i].TokenSQL := 22;

    if (i - 3 >= 0) and (FTokens[i - 3].TokenSQL = 1) and {'FROM3 schema2.1tablename0'}
       (FTokens[i - 1].TokenSQL = 19) then
    begin
      FTokens[i - 2].TokenSQL := 77;
      FTokens[i - 0].TokenSQL := 22;
    end;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].TokenSQL = 20) and (FTokens[i - 2].TokenSQL = 22) {'AS'} then
        FTokens[i].TokenSQL := 23;
    end;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].TokenSQL = 86) and (FTokens[i - 2].TokenSQL = 17) {'SUM('} then
        FTokens[i].TokenSQL := 86;
    end;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].TokenSQL = 87) and (FTokens[i - 2].TokenSQL = 17) {'COUNT('} then
        FTokens[i].TokenSQL := 87;
    end;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].TokenSQL = 88) and (FTokens[i - 2].TokenSQL = 17) {'AVG('} then
        FTokens[i].TokenSQL := 88;
    end;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].TokenSQL = 89) and (FTokens[i - 2].TokenSQL = 17) {'EXISTS('} then
        FTokens[i].TokenSQL := 89;
    end;


    if (i - 2 >= 0) and (i < FTokens.Count - 2) then // SELECT
    begin
      if (FTokens[i - 1].token = 'SELECT') and
         (FTokens[i + 1].token = '.')
          then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
      end;
    end;


    if i - 2 >= 0 then
    begin
      if (FTokens[i - 2].tokenSQL = 31) and (FTokens[i].tokenSQL = 32) then   // UPDATE ??? SET
        FTokens[i - 1].TokenSQL := 22;
    end;


    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].token = 'JOIN') or (FTokens[i - 1].token = 'INNER JOIN') or (FTokens[i - 1].token = 'LEFT JOIN') or (FTokens[i - 1].token = 'RIGHT JOIN') or (FTokens[i - 1].token = 'OUTER JOIN') or (FTokens[i - 1].token = 'FULL OUTER JOIN') then
        FTokens[i].TokenSQL := 22;
    end;

    if i - 2 >= 0 then  // ON
    begin
      if (FTokens[i - 1].token = 'ON') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
        (FTokens[i + 3].token = '=') and//          (FTokens[i + 4].Token = tablename) and
        (FTokens[i + 5].token = '.') then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
        FTokens[i + 4].TokenSQL := 22;
        FTokens[i + 6].TokenSQL := 24;
      end
    end;

    if i - 2 >= 0 then  // (
    begin
      if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
        (FTokens[i + 3].token = '=') and//          (FTokens[i + 4].Token = tablename) and
        (FTokens[i + 5].token = '.') then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
        FTokens[i + 4].TokenSQL := 22;
        FTokens[i + 6].TokenSQL := 24;
      end
    end;

    if i - 2 >= 0 then  // WHERE fieldname = ???
    begin
      if (FTokens[i - 1].token = 'WHERE') and
        (FTokens[i + 1].token = '=')
      then
      begin
        if FTokens[i].TokenType = toSymbol then
          FTokens[i].TokenSQL := 24
        else if FTokens[i].TokenType = toInteger then
          FTokens[i].TokenSQL := 71;

        if FTokens[i + 2].TokenType = toSymbol then
          FTokens[i + 2].TokenSQL := 24
        else if FTokens[i + 2].TokenType = toInteger then
          FTokens[i + 2].TokenSQL := 71
        else if FTokens[i + 2].TokenType = toFloat then
          FTokens[i + 2].TokenSQL := 71
        else if FTokens[i + 2].TokenType = System.Classes.toString then
          FTokens[i + 2].TokenSQL := 72;

      end
    end;

    if i - 2 >= 0 then  // (
    begin
      if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and//          (FTokens[i + 2].Token = fieldname)
        ((FTokens[i + 3].token = '=') or (FTokens[i + 3].token = '<') or (FTokens[i + 3].token = '>')) then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
      end
    end;

    if i - 2 >= 0 then  // =
    begin
      if (FTokens[i - 1].token = '=') and (FTokens[i + 1].token = '.') then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
      end
    end;

    if i - 1 >= 0 then  // USE dbname
    begin
      if (FTokens[i - 1].tokenSQL = 73) then
      begin
        FTokens[i].TokenSQL := 66;
      end
    end;

    if i - 2 >= 0 then  // =
    begin
      if ((FTokens[i - 1].token = '<') or (FTokens[i - 1].token = '=') or (FTokens[i - 1].token = '>')) and (FTokens[i + 1].token = '.') then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
      end
    end;

    if i - 2 >= 0 then  // =
    begin
      if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = '.') and (FTokens[i + 3].token = ')') then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
      end
    end;

    if i - 2 >= 0 then  // =
    begin
      if (FTokens[i - 1].token = ',') and (FTokens[i + 1].token = '.') then
      begin
        FTokens[i].TokenSQL := 22;
        FTokens[i + 2].TokenSQL := 24;
      end
    end;

    if i - 1 >= 0 then  // =
    begin
      if (FTokens[i - 1].token = '(') and (FTokens[i + 1].token = ')') then
      begin
        FTokens[i].TokenSQL := 24;
      end
    end;

    if i - 1 >= 0 then  // parameter
    begin
      if (FTokens[i - 1].token = ':') then
      begin
        FTokens[i].TokenSQL := 26;
      end
    end;

    if i - 5 >= 0 then  // GRANT5 operation4 ON3 object2 TO1 user0
    begin
      if (FTokens[i - 5].tokenSQL = 57) and
         (FTokens[i - 3].tokenSQL = 21) and
         (FTokens[i - 1].tokenSQL = 58) and
         (MatchText(FTokens[i - 4].token, ['SELECT', 'INSERT', 'DELETE']) = TRUE)
      then
      begin
        FTokens[i - 4].TokenSQL := 69;
        FTokens[i - 2].TokenSQL := 68;
        FTokens[i].TokenSQL := 70;
      end
    end;

    if i - 1 >= 0 then
    begin
      if (FTokens[i - 1].tokenSQL = 85) then
      begin
        FTokens[i].TokenSQL := 66; // CREATE DATABASE
      end
      else if (FTokens[i - 1].tokenSQL = 112) then
      begin
        FTokens[i].TokenSQL := 66; // DROP DATABASE
      end
      else if (FTokens[i - 1].tokenSQL = 109) then
      begin
        FTokens[i].TokenSQL := 67; // CREATE VIEW
      end
      else if (FTokens[i - 1].tokenSQL = 113) then
      begin
        FTokens[i].TokenSQL := 67; // DROP VIEW
      end
      else if (FTokens[i - 1].tokenSQL = 110) then
      begin
        FTokens[i].TokenSQL := 70; // CREATE USER
      end
      else if (FTokens[i - 1].tokenSQL = 110) then
      begin
        FTokens[i].TokenSQL := 115; // DROP USER
      end
      else if (FTokens[i - 1].tokenSQL = 111) then
      begin
        FTokens[i].TokenSQL := 22; // CREATE TABLE
      end
      else if (FTokens[i - 1].tokenSQL = 114) then
      begin
        FTokens[i].TokenSQL := 22; // DROP TABLE
      end
      else if (FTokens[i - 1].tokenSQL = 116) then
      begin
        FTokens[i].TokenSQL := 22; // TRUNCATE TABLE
      end
    end;


    if i - 2 >= 0 then
    begin
      if (FTokens[i - 2].tokenSQL = 45) and (FTokens[i - 1].tokenSQL = 50) then
      begin
        FTokens[i].TokenSQL := 74; // DROP CONSTRAINT
      end
    end;
  end;

  for i := 0 to FTokens.Count - 1 do
  begin
    if FTokens[i].TokenSQL = -199 then
      Memo2.Lines.Add('============= ' + FTokens[i].token + ' ' + FTokens[i].TokenSQL.ToString)
    else
    Memo2.Lines.Add(FTokens[i].token + ' ' + FTokens[i].TokenSQL.ToString);
  end;
end;

end.

