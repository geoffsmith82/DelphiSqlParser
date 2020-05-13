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
  ;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FTokens: TTokenBucket;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
  FTokens := TTokenBucket.Create;
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  parser: TParser;
  strStrm: TStringStream;
  token: string;
  info: TTokenInfo;
  i: Integer;
begin
  FTokens.Clear;
  strStrm := TStringStream.Create;
  strStrm.WriteString(Memo1.Text);
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
//   for i := 0 to FTokens.Count - 1 do
  while i < FTokens.Count - 1 do
  begin
    if ((FTokens[i].TokenSQL = 4) and (FTokens[i + 1].TokenSQL = 7) and (FTokens[i + 2].TokenSQL = 8)) then
    begin
      FTokens.Join(i);
      FTokens.Join(i);
    end
    else if ((FTokens[i].TokenSQL = 5) and (FTokens[i + 1].TokenSQL = 8)) or ((FTokens[i].TokenSQL = 3) and (FTokens[i + 1].TokenSQL = 8)) or ((FTokens[i].TokenSQL = 4) and (FTokens[i + 1].TokenSQL = 8)) or ((FTokens[i].TokenSQL = 7) and (FTokens[i + 1].TokenSQL = 8)) then
    begin
      FTokens.Join(i);
    end;
    Inc(i);
  end;

  for i := 1 to FTokens.Count - 1 do
  begin
    if FTokens[i - 1].TokenSQL = 1  {'FROM'} then
      FTokens[i].TokenSQL := 22;

    if i - 2 >= 0 then
    begin
      if (FTokens[i - 1].TokenSQL = 20) and (FTokens[i - 2].TokenSQL = 22) {'AS'} then
        FTokens[i].TokenSQL := 23;
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

    if i - 1 >= 0 then  // =
    begin
      if (FTokens[i - 1].token = ':') then
      begin
        FTokens[i].TokenSQL := 26;
      end
    end;
  end;

  for i := 0 to FTokens.Count - 1 do
  begin
    Memo2.Lines.Add(FTokens[i].token + ' ' + FTokens[i].TokenSQL.ToString);
  end;
end;


end.

