Language server for formatting T-SQL using SQL ScriptDOM: https://github.com/microsoft/sqlscriptdom

### Resources

* ScriptDOM documentation: https://learn.microsoft.com/en-us/dotnet/api/microsoft.sqlserver.transactsql.scriptdom?view=sql-transactsql-161
* Current T-SQL language server used in Azure Data Studio: https://github.com/microsoft/sqltoolsservice
  * T-SQL formatter: https://github.com/microsoft/sqltoolsservice/blob/main/src/Microsoft.SqlTools.ServiceLayer/Formatter/TSqlFormatterService.cs. Looks like a home rolled formatter.
* PoorMansTSqlFormatter: https://github.com/TaoK/PoorMansTSqlFormatter
  * Fork: https://github.com/bungeemonkee/PoorMansTSqlFormatterRedux
* LSP implementation from Ionide: https://github.com/ionide/LanguageServerProtocol
  * csharp-language-server in Ionide.LanguageServerProtocol: https://github.com/razzmatazz/csharp-language-server
* Roslyn LSP in C# (not available as a library): https://github.com/dotnet/roslyn/tree/main/src/Features/LanguageServer/Microsoft.CodeAnalysis.LanguageServer
* OmniSharp LSP in C#: https://github.com/OmniSharp/csharp-language-server-protocol
* Language Server Protocol types generator: https://github.com/microsoft/lsprotocol
* FsPretty: https://github.com/mjsottile/FsPretty/blob/master/src/FsPretty/Library.fs