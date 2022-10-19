# DelphiSqlParser
## Project Goals

### Create a SQL Parser in Delphi
  - To create a SQL parser in Delphi that can be used to create an AST of the SQL
  - Ability to create a Syntax highlighter for SQL
### Ability to prevent SQL injection attacks 
  - by forcing the use of parameters by preventing the use of constant values in SQL
  - detecting multiple statements that are going to be executed together
  - detecting always true / always false statements
	
## Project Status

  Currently the project is in a very early stage although, it can decode fair number of the simple SQL commands although for anything complicated it probably can't yet handle.
  - The project can currently decode 69 out of the 70 test SQL statements, although code to validate that statements have been correctly decoded still needs to be written, so the current success rate may be misleading.
  - At the moment no attempt to limit SQL dialect type has been implemented.