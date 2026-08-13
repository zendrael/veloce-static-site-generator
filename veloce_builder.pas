unit veloce_builder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, veloce_utils, veloce_config, veloce_markdown, veloce_template;

procedure BuildSite(const SrcDir, OutDir: string; IsDev: Boolean);

implementation

procedure ProcessContentDir(const Dir, TemplatesDir, OutDir, BaseURL: string;
  const Config: TVeloceConfig; IsDev: Boolean; const RootContentDir: string); forward;

function ExtractFrontMatter(const Content: string; out Title, Description: string): string;
var
  SL: TStringList;
  i, j: Integer;
  Line, Key, Value: string;
  PosEq: Integer;
begin
  Title := '';
  Description := '';
  Result := Content;

  SL := TStringList.Create;
  try
    SL.Text := Content;
    if (SL.Count > 0) and (TrimString(SL[0]) = '---') then
    begin
      j := -1;
      for i := 1 to SL.Count - 1 do
      begin
        if TrimString(SL[i]) = '---' then
        begin
          j := i;
          Break;
        end;
        Line := TrimString(SL[i]);
        PosEq := Pos(':', Line);
        if PosEq > 0 then
        begin
          Key := LowerCase(TrimString(Copy(Line, 1, PosEq - 1)));
          Value := TrimString(Copy(Line, PosEq + 1, Length(Line)));
          if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
            Value := Copy(Value, 2, Length(Value) - 2);
          if Key = 'title' then Title := Value
          else if Key = 'description' then Description := Value;
        end;
      end;
      if j > 0 then
      begin
        Result := '';
        for i := j + 1 to SL.Count - 1 do
        begin
          if Result <> '' then Result := Result + LineEnding;
          Result := Result + SL[i];
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

function GetTemplateForFile(const FileName, TemplatesDir: string): string;
var
  BaseName, DirName: string;
  CustomTpl: string;
begin
  BaseName := ExtractFileNameNoExt(FileName);
  DirName := ExtractFileName(ExtractFileDir(FileName));

  CustomTpl := TemplatesDir + DirectorySeparator + BaseName + '.html';
  if FileExists(CustomTpl) then
  begin
    Result := CustomTpl;
    Exit;
  end;

  if DirName <> '' then
  begin
    CustomTpl := TemplatesDir + DirectorySeparator + DirName + '.html';
    if FileExists(CustomTpl) then
    begin
      Result := CustomTpl;
      Exit;
    end;
  end;

  Result := TemplatesDir + DirectorySeparator + 'base.html';
  if not FileExists(Result) then
    Result := '';
end;

procedure ProcessContentFile(const FileName, TemplatesDir, OutDir, BaseURL: string; 
  const Config: TVeloceConfig; IsDev: Boolean);
var
  Content, HTML, Rendered, FrontMatterTitle, FrontMatterDesc: string;
  OutFileName, RelPath, OutPath, TemplatePath: string;
  IsMarkdown: Boolean;
begin
  Content := FileToString(FileName);

  HTML := ExtractFrontMatter(Content, FrontMatterTitle, FrontMatterDesc);

  if FrontMatterTitle = '' then
    FrontMatterTitle := ExtractFileNameNoExt(FileName);

  IsMarkdown := SameText(ExtractFileExt(FileName), '.md');
  if IsMarkdown then
    HTML := MarkdownToHTML(HTML)
  else
    HTML := HTML;

  TemplatePath := GetTemplateForFile(FileName, TemplatesDir);

  Rendered := RenderTemplate(TemplatePath, HTML, Config, FrontMatterTitle, FrontMatterDesc);

  RelPath := ExtractFilePath(FileName);
  if StartsWithStr(RelPath, 'content' + DirectorySeparator) then
    RelPath := Copy(RelPath, Length('content') + 2, Length(RelPath));

  OutFileName := ChangeFileExt(ExtractFileName(FileName), '.html');
  OutPath := OutDir + DirectorySeparator + RelPath + OutFileName;

  ForceDirectories(ExtractFilePath(OutPath));
  StringToFile(OutPath, Rendered);

  WriteLn('  [BUILD] ', FileName, ' -> ', OutPath);
end;

procedure BuildSite(const SrcDir, OutDir: string; IsDev: Boolean);
var
  Config: TVeloceConfig;
  ContentDir, TemplatesDir, StaticDir: string;
  SR: TSearchRec;
  SearchPath: string;
  FileName: string;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('  VELOCE - Static Site Generator');
  if IsDev then
    WriteLn('  Modo: DESENVOLVIMENTO')
  else
    WriteLn('  Modo: PRODUÇÃO');
  WriteLn('========================================');
  WriteLn('');

  Config := LoadConfig(SrcDir + DirectorySeparator + 'veloce.toml');
  WriteLn('Site: ', Config.Title);
  WriteLn('');

  ContentDir := SrcDir + DirectorySeparator + 'content';
  TemplatesDir := SrcDir + DirectorySeparator + 'templates';
  StaticDir := SrcDir + DirectorySeparator + 'static';

  if DirectoryExists(OutDir) then
    DeleteDir(OutDir);
  ForceDirectories(OutDir);

  WriteLn('>> Processando conteúdo...');

  SearchPath := ContentDir + DirectorySeparator + '*.md';
  if FindFirst(SearchPath, faAnyFile, SR) = 0 then
  begin
    repeat
      FileName := ContentDir + DirectorySeparator + SR.Name;
      ProcessContentFile(FileName, TemplatesDir, OutDir, Config.URL, Config, IsDev);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  SearchPath := ContentDir + DirectorySeparator + '*.html';
  if FindFirst(SearchPath, faAnyFile, SR) = 0 then
  begin
    repeat
      FileName := ContentDir + DirectorySeparator + SR.Name;
      ProcessContentFile(FileName, TemplatesDir, OutDir, Config.URL, Config, IsDev);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  SearchPath := ContentDir + DirectorySeparator + '*';
  if FindFirst(SearchPath, faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then
      begin
        ProcessContentDir(ContentDir + DirectorySeparator + SR.Name, 
          TemplatesDir, OutDir, Config.URL, Config, IsDev, ContentDir);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  if DirectoryExists(StaticDir) then
  begin
    WriteLn('');
    WriteLn('>> Copiando arquivos estáticos...');
    CopyDir(StaticDir, OutDir);
  end;

  WriteLn('');
  WriteLn('========================================');
  WriteLn('  Build concluído com sucesso!');
  WriteLn('  Saída: ', OutDir);
  WriteLn('========================================');
  WriteLn('');
end;

procedure ProcessContentDir(const Dir, TemplatesDir, OutDir, BaseURL: string;
  const Config: TVeloceConfig; IsDev: Boolean; const RootContentDir: string);
var
  SR: TSearchRec;
  SearchPath, FileName: string;
begin
  SearchPath := Dir + DirectorySeparator + '*.md';
  if FindFirst(SearchPath, faAnyFile, SR) = 0 then
  begin
    repeat
      FileName := Dir + DirectorySeparator + SR.Name;
      ProcessContentFile(FileName, TemplatesDir, OutDir, BaseURL, Config, IsDev);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  SearchPath := Dir + DirectorySeparator + '*.html';
  if FindFirst(SearchPath, faAnyFile, SR) = 0 then
  begin
    repeat
      FileName := Dir + DirectorySeparator + SR.Name;
      ProcessContentFile(FileName, TemplatesDir, OutDir, BaseURL, Config, IsDev);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  SearchPath := Dir + DirectorySeparator + '*';
  if FindFirst(SearchPath, faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) <> 0 then
        ProcessContentDir(Dir + DirectorySeparator + SR.Name, 
          TemplatesDir, OutDir, BaseURL, Config, IsDev, RootContentDir);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

end.
