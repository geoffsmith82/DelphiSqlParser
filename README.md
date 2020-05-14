# DelphiSqlParser
## Project Goal

  To create a SQL parser in Delphi that can be used to not only to create an AST of the SQL but able to prevent SQL injection attacks 
    - by forcing the use of parameters
    - detecting multiple statements that are going to be executed together
	- detecting always true / always false statements
	
## Project Status

  Currently the project is in a very early stage although, it can decode fair number of the simple SQL commands although for anything complicated it probably cant yet handle.
  The project can currently decode 13 out of 57 test SQL statements.