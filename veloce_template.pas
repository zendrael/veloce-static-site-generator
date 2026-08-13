unit veloce_template;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, veloce_utils, veloce_config;

function RenderTemplate(const TemplatePath, Content: string; 
  const Config: TVeloceConfig; const Title, Description: string): string;

implementation

function ProcessIncludes(var Tpl: string; const BasePath: string): string;
var
  i, j, k: Integer;
  IncludePath, IncludeFile, IncludeContent: string;
begin
  Result := Tpl;
  i := Pos('{{>', Result);
  while i > 0 do
  begin
    j := Pos('}}', Result, i);
    if j = 0 then Break;
    IncludeFile := TrimString(Copy(Result, i + 3, j - i - 3));
    IncludePath := BasePath + IncludeFile;
    if FileExists(IncludePath) then
      IncludeContent := FileToString(IncludePath)
    else
      IncludeContent := '<!-- include not found: ' + IncludeFile + ' -->';

    Result := Copy(Result, 1, i - 1) + IncludeContent + Copy(Result, j + 2, Length(Result));
    i := Pos('{{>', Result);
  end;
end;

function RenderTemplate(const TemplatePath, Content: string; 
  const Config: TVeloceConfig; const Title, Description: string): string;
var
  Tpl: string;
  BasePath: string;
begin
  if FileExists(TemplatePath) then
    Tpl := FileToString(TemplatePath)
  else
    Tpl := '<!DOCTYPE html><html><head><title>{{title}}</title></head><body>{{content}}</body></html>';

  BasePath := IncludeTrailingPathDelimiter(ExtractFilePath(TemplatePath));

  // Processar includes
  Tpl := ProcessIncludes(Tpl, BasePath);

  // Substituir variáveis
  Tpl := ReplaceAll(Tpl, '{{title}}', Title);
  Tpl := ReplaceAll(Tpl, '{{description}}', Description);
  Tpl := ReplaceAll(Tpl, '{{content}}', Content);
  Tpl := ReplaceAll(Tpl, '{{site.title}}', Config.Title);
  Tpl := ReplaceAll(Tpl, '{{site.description}}', Config.Description);
  Tpl := ReplaceAll(Tpl, '{{site.url}}', Config.URL);
  Tpl := ReplaceAll(Tpl, '{{site.author}}', Config.Author);
  Tpl := ReplaceAll(Tpl, '{{site.language}}', Config.Language);

  Result := Tpl;
end;

end.
