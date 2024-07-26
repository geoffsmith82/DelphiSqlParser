program TestSQLParser;

uses
  Vcl.Forms,
  Unit4 in 'Unit4.pas' {Form4},
  Data.DB.Lexer in '..\Source\Data.DB.Lexer.pas',
  Data.DB.Parser in '..\Source\Data.DB.Parser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
