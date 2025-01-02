program sitegen;
{$mode objfpc}{$H+}
 
uses
  Classes, SysUtils, RegExpr, fpTemplate, httpdefs,
  MarkdownProcessor in 'external/MarkdownProcessor.pas',
  MarkdownDaringFireball in 'external/MarkdownDaringFireball.pas',
  MarkdownCommonMark in 'external/MarkdownCommonMark.pas',
  MarkdownUnicodeUtils in 'external/MarkdownUnicodeUtils.pas',
  MarkdownHTMLEntities in 'external/MarkdownHTMLEntities.pas';
 
const
  SiteMapHeader = '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
'<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">'  + LineEnding;
 
var
  DefsRe : TRegExpr;
 
function ExtractHeader(ARawContent: String; Defs : TStringList) : String;
var
  SList: TStringList;
begin
  SList := TStringList.Create;
  try
    SList.Text := ARawContent;
    while SList.Count > 0 do
    begin
      if not DefsRe.Exec(SList.Strings[0]) then
        Break;
      if Defs.IndexOfName(DefsRe.Match[1]) <> -1 then
          WriteLn('Warning : skipping duplicate header ' + DefsRe.Match[1]);
      Defs.AddPair(DefsRe.Match[1], DefsRe.Match[2]);
      SList.Delete(0);
    end;
    Result := SList.Text;
  finally
    FreeAndNil(SList);
  end;
end;
 
function ListFiles(APath: String): TStringArray;
var
  Info : TSearchRec;
begin
  Result := [];
  if FindFirst(APath, faAnyFile - faDirectory, Info) = 0 then
  repeat
    Result := Concat(Result, [String(Info.Name)]);
  until FindNext(info) <> 0;
  FindClose(Info);
end;
 
function LoadFromFile(AFileName: String) : String;
var
  FStream: TFileStream;
  SData: RawByteString = '';
begin
  FStream := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyWrite);
  SetLength(SData, FStream.Size);
  if FStream.Size > 0 then
    FStream.Read(SData[1], FStream.Size);
  Result := UTF8String(SData);
  FreeAndNil(FStream);
end;
 
procedure Main;
var
  T : TTemplateParser;
  Processor : TMarkdownProcessor;
  FStream: TFileStream;
  Global, Content, Defs : TStringList;
  S, Ext, Name, FileName, Tpl : String;
  I, J : Integer;
begin
  try
    DefsRe := TRegExpr.Create('^:(\S+): \s*(.*)\s*$');
    Processor := TMarkdownProcessor.createDialect(mdDaringFireball);
    Processor.Unsafe := True;
    Global := Nil;
    Content := TStringList.Create;
    Content.Sorted := True;
    Content.Duplicates := dupIgnore;
 
    for FileName in ListFiles('content' + DirectorySeparator + '*') do
    begin
        Ext := LowerCase(ExtractFileExt(FileName));
        case Ext of
          '.md', '.html' :
          begin
            S := LoadFromFile('content' + DirectorySeparator + FileName);
            if Length(S) = 0 then
            begin
              WriteLn('Warning : file empty ' + FileName);
              Continue;
            end;
            Name := ChangeFileExt(FileName, '.html');
            if Content.IndexOfName(Name) <> -1 then
            begin
              WriteLn('Warning : skipping duplicate ' + Name);
              Continue;
            end;
            Defs := TStringList.Create;
            Defs.Sorted := True;
            Defs.Duplicates := dupIgnore;
            S := ExtractHeader(S, Defs);
            if LowerCase(Name) = 'global.html' then
            begin
              if Assigned(Global) then
                WriteLn('Warning : global redefined');
              Global := Defs;
              Continue;
            end;
            if Ext = '.md' then
              S := Processor.Process(S);
            Content.AddPair(Name, S , Defs);
          end;
        else
          WriteLn('Warning : skipping unknown file ' + FileName);
        end;
    end;
 
    if Content.Count = 0 then
    begin
      WriteLn('Error : content not found');
      Halt(1);
    end;
 
    for FileName in ListFiles('public' + DirectorySeparator + '*') do
    begin
      Ext := LowerCase(ExtractFileExt(FileName));
      if (Ext = '.html') or (Ext = '.xml') or (Ext = '.txt') then
        if not DeleteFile('public' + DirectorySeparator + FileName) then
          WriteLn('Warning : could not delte file ' + FileName);
    end;
 
    if not DirectoryExists('public') then
      CreateDir('public');
 
    FStream := TFileStream.Create('public' + DirectorySeparator + 'sitemap.xml', fmCreate);
    S := SiteMapHeader;
    FStream.Write(S[1], Length(S));
 
    T := TTemplateParser.Create;
    T.StartDelimiter := '{%';
    T.EndDelimiter := '%}';
    T.AllowTagParams := False;
    T.Recursive := True;
    for I := 0 to Content.Count - 1 do
    begin
      T.Clear;
      T.Values['content'] := Content.ValueFromIndex[I];
      Defs := TStringList(Content.Objects[I]);
      if Assigned(Global) then
      begin
        for J := 0 to Global.Count - 1 do
          T.Values[Global.Names[J]] := Global.ValueFromIndex[J];
      end;
      for J := 0 to Defs.Count - 1 do
        T.Values[Defs.Names[J]] := Defs.ValueFromIndex[J];
 
      Tpl := Defs.Values['template'];
      if Length(Tpl) = 0 then
        Tpl := 'default.html';
 
      Name := Content.Names[I];
      T.ParseFiles('template' + DirectorySeparator + Tpl, 'public' + DirectorySeparator + Name);
      WriteLn('Processed ' + Name);
      FreeAndNil(Defs);
 
      S := HTTPEncode(T.ParseString('{%root%}' + Name));
      S := '<url><loc>' + S + '</loc></url>' + LineEnding;
      FStream.Write(S[1], Length(S));
    end;
    S := '</urlset>'  + LineEnding;
    FStream.Write(S[1], Length(S));
    WriteLn('Processed sitemap.xml');
 
    if FileExists('template' + DirectorySeparator + 'robots.txt') then
    begin
      T.Clear;
      if Assigned(Global) then
      begin
        for J := 0 to Global.Count - 1 do
          T.Values[Global.Names[J]] := Global.ValueFromIndex[J];
      end;
      T.ParseFiles('template' + DirectorySeparator + 'robots.txt', 'public' + DirectorySeparator + 'robots.txt');
      WriteLn('Processed robots.txt');
    end;
  finally
    FreeAndNil(FStream);
    FreeAndNil(Global);
    FreeAndNil(Content);
    FreeAndNil(DefsRe);
    FreeAndNil(T);
  end;
end;
 
begin
  Main;
end.
 