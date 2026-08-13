unit veloce_builder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, veloce_utils, veloce_config, veloce_markdown, veloce_template;

procedure BuildSite(const SrcDir, OutDir: string; IsDev: Boolean);

implementation

procedure ProcessContentDir(const Dir, TemplatesDir, OutDir, BaseURL: string;
  const Config: TVeloceConfig; IsDev: Boolean; const RootContentDir: string); forward;
function IsBlogPostFile(const RelativePath: string): Boolean; forward;

type
  TBlogPostItem = record
    Title: string;
    Description: string;
    Date: string;
    URL: string;
  end;

  TBlogPostArray = array of TBlogPostItem;

function GetFrontMatterValue(const Content, WantedKey: string): string;
var
  SL: TStringList;
  i: Integer;
  Line, Key, Value: string;
  PosEq: Integer;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Text := Content;
    if (SL.Count > 0) and (TrimString(SL[0]) = '---') then
    begin
      for i := 1 to SL.Count - 1 do
      begin
        Line := TrimString(SL[i]);
        if Line = '---' then Break;
        PosEq := Pos(':', Line);
        if PosEq > 0 then
        begin
          Key := LowerCase(TrimString(Copy(Line, 1, PosEq - 1)));
          Value := TrimString(Copy(Line, PosEq + 1, Length(Line)));
          if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
            Value := Copy(Value, 2, Length(Value) - 2);
          if Key = LowerCase(WantedKey) then
          begin
            Result := Value;
            Exit;
          end;
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

function RelativePathFromRoot(const FullFileName, RootContentDir: string): string;
var
  FullName, FullRoot: string;
begin
  FullName := ExpandFileName(FullFileName);
  FullRoot := IncludeTrailingPathDelimiter(ExpandFileName(RootContentDir));
  if StartsWithStr(FullName, FullRoot) then
    Result := Copy(FullName, Length(FullRoot) + 1, Length(FullName))
  else
    Result := ExtractFileName(FullFileName);
end;

function BuildPostURLFromRelativePath(const RelativePath: string): string;
var
  S: string;
begin
  S := ChangeFileExt(RelativePath, '');
  S := StringReplace(S, DirectorySeparator, '/', [rfReplaceAll]);
  Result := '/' + S + '/';
end;

procedure AddBlogPost(var Posts: TBlogPostArray; const ContentDir, FullPath: string);
var
  RelPath, Content, Title, Description, DateValue: string;
  N: Integer;
begin
  RelPath := RelativePathFromRoot(FullPath, ContentDir);
  if not IsBlogPostFile(RelPath) then Exit;
  if SameText(ExtractFileNameNoExt(FullPath), 'index') then Exit;
  if not (SameText(ExtractFileExt(FullPath), '.md') or SameText(ExtractFileExt(FullPath), '.html')) then Exit;

  Content := FileToString(FullPath);
  Title := GetFrontMatterValue(Content, 'title');
  Description := GetFrontMatterValue(Content, 'description');
  DateValue := GetFrontMatterValue(Content, 'date');

  if Title = '' then
    Title := ExtractFileNameNoExt(FullPath);

  N := Length(Posts);
  SetLength(Posts, N + 1);
  Posts[N].Title := Title;
  Posts[N].Description := Description;
  Posts[N].Date := DateValue;
  Posts[N].URL := BuildPostURLFromRelativePath(RelPath);
end;

procedure CollectBlogPostsInDir(const Dir, ContentDir: string; var Posts: TBlogPostArray);
var
  SR: TSearchRec;
  FullPath: string;
begin
  if not DirectoryExists(Dir) then Exit;
  if FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      FullPath := IncludeTrailingPathDelimiter(Dir) + SR.Name;
      if (SR.Attr and faDirectory) <> 0 then
        CollectBlogPostsInDir(FullPath, ContentDir, Posts)
      else
        AddBlogPost(Posts, ContentDir, FullPath);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure SortBlogPosts(var Posts: TBlogPostArray);
var
  i, j: Integer;
  Tmp: TBlogPostItem;
begin
  for i := 0 to High(Posts) - 1 do
    for j := i + 1 to High(Posts) do
      if (Posts[j].Date > Posts[i].Date) or
         ((Posts[j].Date = Posts[i].Date) and (Posts[j].Title < Posts[i].Title)) then
      begin
        Tmp := Posts[i];
        Posts[i] := Posts[j];
        Posts[j] := Tmp;
      end;
end;

function GetBlogPageURL(const PageNumber: Integer): string;
begin
  if PageNumber <= 1 then
    Result := '/blog/index.html'
  else
    Result := '/blog/page/' + IntToStr(PageNumber) + '/index.html';
end;

function BuildBlogIndexHTML(const Posts: TBlogPostArray; const PageNumber, PerPage, TotalPages: Integer): string;
var
  StartIdx, EndIdx, i: Integer;
  Pagination, DateHTML, DescHTML: string;
begin
  Result := '<h1>Blog</h1>' + LineEnding;

  if Length(Posts) = 0 then
  begin
    Result := Result + '<p>Nenhum post publicado ainda.</p>';
    Exit;
  end;

  StartIdx := (PageNumber - 1) * PerPage;
  EndIdx := StartIdx + PerPage - 1;
  if EndIdx > High(Posts) then
    EndIdx := High(Posts);

  Result := Result + '<ul class="blog-post-list">' + LineEnding;
  for i := StartIdx to EndIdx do
  begin
    if Posts[i].Date <> '' then
      DateHTML := '<small>' + Posts[i].Date + '</small>' + LineEnding
    else
      DateHTML := '';

    if Posts[i].Description <> '' then
      DescHTML := '<p>' + Posts[i].Description + '</p>' + LineEnding
    else
      DescHTML := '';

    Result := Result + '<li>' + LineEnding +
      '<h2><a href="' + Posts[i].URL + '">' + Posts[i].Title + '</a></h2>' + LineEnding +
      DateHTML + DescHTML +
      '</li>' + LineEnding;
  end;
  Result := Result + '</ul>' + LineEnding;

  if TotalPages > 1 then
  begin
    Pagination := '<nav class="pagination">';
    if PageNumber > 1 then
      Pagination := Pagination + '<a href="' + GetBlogPageURL(PageNumber - 1) + '">Anterior</a> ';

    Pagination := Pagination + '<span>Página ' + IntToStr(PageNumber) + ' de ' + IntToStr(TotalPages) + '</span>';

    if PageNumber < TotalPages then
      Pagination := Pagination + ' <a href="' + GetBlogPageURL(PageNumber + 1) + '">Próxima</a>';

    Pagination := Pagination + '</nav>';
    Result := Result + Pagination;
  end;
end;

procedure GenerateBlogIndexPages(const OutDir, TemplatesDir, ContentDir: string; const Config: TVeloceConfig);
var
  Posts: TBlogPostArray;
  BlogDir, PostsDir: string;
  PerPage, TotalPages, PageNumber: Integer;
  ContentHTML, Rendered, TemplatePath, OutPath, PageTitle: string;
begin
  SetLength(Posts, 0);
  BlogDir := ContentDir + DirectorySeparator + 'blog';
  PostsDir := ContentDir + DirectorySeparator + 'posts';

  CollectBlogPostsInDir(BlogDir, ContentDir, Posts);
  CollectBlogPostsInDir(PostsDir, ContentDir, Posts);
  if Length(Posts) > 1 then
    SortBlogPosts(Posts);

  PerPage := Config.BlogPostsPerPage;
  if PerPage < 1 then
    PerPage := 10;

  TotalPages := (Length(Posts) + PerPage - 1) div PerPage;
  if TotalPages < 1 then
    TotalPages := 1;

  TemplatePath := TemplatesDir + DirectorySeparator + 'blog.html';
  if not FileExists(TemplatePath) then
  begin
    TemplatePath := TemplatesDir + DirectorySeparator + 'page.html';
    if not FileExists(TemplatePath) then
      TemplatePath := TemplatesDir + DirectorySeparator + 'base.html';
  end;

  for PageNumber := 1 to TotalPages do
  begin
    ContentHTML := BuildBlogIndexHTML(Posts, PageNumber, PerPage, TotalPages);
    if PageNumber = 1 then
      PageTitle := 'Blog'
    else
      PageTitle := 'Blog - Página ' + IntToStr(PageNumber);

    Rendered := RenderTemplate(TemplatePath, ContentHTML, Config, PageTitle, 'Lista de posts do blog');

    if PageNumber = 1 then
      OutPath := OutDir + DirectorySeparator + 'blog' + DirectorySeparator + 'index.html'
    else
      OutPath := OutDir + DirectorySeparator + 'blog' + DirectorySeparator + 'page' + DirectorySeparator +
        IntToStr(PageNumber) + DirectorySeparator + 'index.html';

    ForceDirectories(ExtractFilePath(OutPath));
    StringToFile(OutPath, Rendered);
    WriteLn('  [BLOG] index página ', PageNumber, ' -> ', OutPath);
  end;
end;

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

function IsBlogPostFile(const RelativePath: string): Boolean;
begin
  Result := StartsWithStr(RelativePath, 'blog' + DirectorySeparator) or
            StartsWithStr(RelativePath, 'posts' + DirectorySeparator);
end;

procedure ProcessContentFile(const FileName, TemplatesDir, OutDir, BaseURL: string;
  const Config: TVeloceConfig; IsDev: Boolean; const RootContentDir: string);
var
  Content, HTML, Rendered, FrontMatterTitle, FrontMatterDesc: string;
  OutFileName, RelPath, OutPath, TemplatePath, PostTemplate, PageTemplate: string;
  FullFileName, FullRootDir: string;
  IsMarkdown: Boolean;
  IsBlogPost: Boolean;
  SlugPath: string;
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

  FullFileName := ExpandFileName(FileName);
  FullRootDir := IncludeTrailingPathDelimiter(ExpandFileName(RootContentDir));
  RelPath := ExtractFilePath(FullFileName);
  if StartsWithStr(RelPath, FullRootDir) then
    RelPath := Copy(RelPath, Length(FullRootDir) + 1, Length(RelPath))
  else
    RelPath := '';

  IsBlogPost := IsBlogPostFile(RelPath + ExtractFileName(FileName)) and
                (not SameText(ExtractFileNameNoExt(FileName), 'index'));

  if IsBlogPost then
  begin
    PostTemplate := TemplatesDir + DirectorySeparator + 'post.html';
    if FileExists(PostTemplate) then
      TemplatePath := PostTemplate;
  end
  else
  begin
    PageTemplate := TemplatesDir + DirectorySeparator + 'page.html';
    if FileExists(PageTemplate) then
      TemplatePath := PageTemplate;
  end;

  Rendered := RenderTemplate(TemplatePath, HTML, Config, FrontMatterTitle, FrontMatterDesc);

  OutFileName := ChangeFileExt(ExtractFileName(FileName), '.html');
  if IsBlogPost then
  begin
    SlugPath := ChangeFileExt(RelPath + ExtractFileName(FileName), '');
    OutPath := OutDir + DirectorySeparator + SlugPath + DirectorySeparator + 'index.html';
  end
  else
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
      ProcessContentFile(FileName, TemplatesDir, OutDir, Config.URL, Config, IsDev, ContentDir);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  SearchPath := ContentDir + DirectorySeparator + '*.html';
  if FindFirst(SearchPath, faAnyFile, SR) = 0 then
  begin
    repeat
      FileName := ContentDir + DirectorySeparator + SR.Name;
      ProcessContentFile(FileName, TemplatesDir, OutDir, Config.URL, Config, IsDev, ContentDir);
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

  WriteLn('');
  WriteLn('>> Gerando índice do blog...');
  GenerateBlogIndexPages(OutDir, TemplatesDir, ContentDir, Config);

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
      ProcessContentFile(FileName, TemplatesDir, OutDir, BaseURL, Config, IsDev, RootContentDir);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  SearchPath := Dir + DirectorySeparator + '*.html';
  if FindFirst(SearchPath, faAnyFile, SR) = 0 then
  begin
    repeat
      FileName := Dir + DirectorySeparator + SR.Name;
      ProcessContentFile(FileName, TemplatesDir, OutDir, BaseURL, Config, IsDev, RootContentDir);
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
