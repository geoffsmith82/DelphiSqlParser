# DelphiSqlParser
## Project Goals
I have listed some goals of this project below.  
### Create a SQL Parser in Delphi
  - To create a SQL parser in Delphi that can be used to create an AST of the SQL
  - Ability to create a Syntax highlighter for SQL
### Ability to prevent SQL injection attacks 
  - by forcing the use of parameters by preventing the use of constant values in SQL
  - detecting multiple statements that are going to be executed together
  - detecting always true / always false statements
### SQL Manipulation
  - Do equivilant of SQL refactoring (renaming various things like table names,field names etc)
  - Convert between different dialects of SQL
	
## Project Status

  Currently the project is in a very early stage although, it can decode fair number of the simple SQL commands although for anything complicated it probably can't yet handle.
  - The project can currently decode 69 out of the 70 test SQL statements.
  - The project can compare what the expected AST is to what the decoded AST is to ensure the parser is working properly
  - At the moment no attempt to limit SQL dialect type has been implemented.
  - Initial testing of 1 = 1 like conditions for detecting SQL Injection attacks
  - Add check for multiple statements in SQL text