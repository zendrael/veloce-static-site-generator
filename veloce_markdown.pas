unit veloce_markdown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, veloce_utils;

function MarkdownToHTML(const MD: string): string;

implementation

function ProcessInline(const Line: string): string;
var
  S: string;
  i, j, k: Integer;
  Prefix, Suffix, Inner, URL, Text, Alt: string;
begin
  S := Line;

  // Código inline `texto`
  i := Pos('`', S);
  while i > 0 do
  begin
    j := PosEx('`', S, i + 1);
    if j = 0 then Break;
    Inner := Copy(S, i + 1, j - i - 1);
    S := Copy(S, 1, i - 1) + '<code>' + Inner + '</code>' + Copy(S, j + 1, Length(S));
    i := PosEx('`', S, i + 1);
  end;

  // Negrito **texto**
  i := Pos('**', S);
  while i > 0 do
  begin
    j := PosEx('**', S, i + 2);
    if j = 0 then Break;
    Inner := Copy(S, i + 2, j - i - 2);
    S := Copy(S, 1, i - 1) + '<strong>' + Inner + '</strong>' + Copy(S, j + 2, Length(S));
    i := PosEx('**', S, i + 1);
  end;

  // Itálico *texto* (evitar conflito com **)
  i := Pos('*', S);
  while i > 0 do
  begin
    if (i > 1) and (S[i-1] = '*') then begin i := PosEx('*', S, i + 1); Continue; end;
    j := PosEx('*', S, i + 1);
    if j = 0 then Break;
    if (j < Length(S)) and (S[j+1] = '*') then begin i := PosEx('*', S, i + 1); Continue; end;
    Inner := Copy(S, i + 1, j - i - 1);
    S := Copy(S, 1, i - 1) + '<em>' + Inner + '</em>' + Copy(S, j + 1, Length(S));
    i := PosEx('*', S, i + 1);
  end;

  // Imagens ![alt](url)
  i := Pos('![', S);
  while i > 0 do
  begin
    j := PosEx('](', S, i);
    if j = 0 then Break;
    k := PosEx(')', S, j);
    if k = 0 then Break;
    Alt := Copy(S, i + 2, j - i - 2);
    URL := Copy(S, j + 2, k - j - 2);
    S := Copy(S, 1, i - 1) + '<img src="' + URL + '" alt="' + Alt + '">' + Copy(S, k + 1, Length(S));
    i := PosEx('![', S, i + 1);
  end;

  // Links [texto](url)
  i := Pos('[', S);
  while i > 0 do
  begin
    if (i > 1) and (S[i-1] = '!') then begin i := PosEx('[', S, i + 1); Continue; end;
    j := PosEx('](', S, i);
    if j = 0 then Break;
    k := PosEx(')', S, j);
    if k = 0 then Break;
    Text := Copy(S, i + 1, j - i - 1);
    URL := Copy(S, j + 2, k - j - 2);
    S := Copy(S, 1, i - 1) + '<a href="' + URL + '">' + Text + '</a>' + Copy(S, k + 1, Length(S));
    i := PosEx('[', S, i + 1);
  end;

  Result := S;
end;

function MarkdownToHTML(const MD: string): string;
var
  SL, OutSL: TStringList;
  i: Integer;
  Line, Trimmed: string;
  InCodeBlock: Boolean;
  CodeBuffer: TStringList;
  InList: Boolean;
  ListType: Char; // 'u' = ul, 'o' = ol
  InParagraph: Boolean;
  HeaderLevel: Integer;
  j: Integer;
  ListContent: string;
  ParaBuffer: TStringList;
  FrontMatterDone: Boolean;
  InFrontMatter: Boolean;
begin
  SL := TStringList.Create;
  OutSL := TStringList.Create;
  CodeBuffer := TStringList.Create;
  ParaBuffer := TStringList.Create;
  try
    SL.Text := MD;
    InCodeBlock := False;
    InList := False;
    ListType := #0;
    InParagraph := False;
    FrontMatterDone := False;
    InFrontMatter := False;

    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      Trimmed := TrimString(Line);

      // Front matter (---)
      if not FrontMatterDone then
      begin
        if Trimmed = '---' then
        begin
          if not InFrontMatter then
          begin
            InFrontMatter := True;
            Continue;
          end
          else
          begin
            InFrontMatter := False;
            FrontMatterDone := True;
            Continue;
          end;
        end;
        if InFrontMatter then Continue;
        if Trimmed = '' then Continue;
        FrontMatterDone := True;
      end;

      // Bloco de código
      if StartsWithStr(Trimmed, '```') then
      begin
        if InCodeBlock then
        begin
          OutSL.Add('<pre><code>');
          for j := 0 to CodeBuffer.Count - 1 do
            OutSL.Add(CodeBuffer[j]);
          OutSL.Add('</code></pre>');
          CodeBuffer.Clear;
          InCodeBlock := False;
        end
        else
        begin
          InCodeBlock := True;
        end;
        Continue;
      end;

      if InCodeBlock then
      begin
        CodeBuffer.Add(Line);
        Continue;
      end;

      // Headers
      if StartsWithStr(Trimmed, '# ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<h1>' + ProcessInline(Copy(Trimmed, 3, Length(Trimmed))) + '</h1>');
        Continue;
      end;
      if StartsWithStr(Trimmed, '## ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<h2>' + ProcessInline(Copy(Trimmed, 4, Length(Trimmed))) + '</h2>');
        Continue;
      end;
      if StartsWithStr(Trimmed, '### ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<h3>' + ProcessInline(Copy(Trimmed, 5, Length(Trimmed))) + '</h3>');
        Continue;
      end;
      if StartsWithStr(Trimmed, '#### ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<h4>' + ProcessInline(Copy(Trimmed, 6, Length(Trimmed))) + '</h4>');
        Continue;
      end;
      if StartsWithStr(Trimmed, '##### ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<h5>' + ProcessInline(Copy(Trimmed, 7, Length(Trimmed))) + '</h5>');
        Continue;
      end;
      if StartsWithStr(Trimmed, '###### ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<h6>' + ProcessInline(Copy(Trimmed, 8, Length(Trimmed))) + '</h6>');
        Continue;
      end;

      // Linha horizontal
      if Trimmed = '---' then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if InList then begin OutSL.Add('</' + ListType + 'l>'); InList := False; end;
        OutSL.Add('<hr>');
        Continue;
      end;

      // Listas
      if StartsWithStr(Trimmed, '- ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if not InList then
        begin
          OutSL.Add('<ul>');
          InList := True;
          ListType := 'u';
        end;
        if ListType <> 'u' then
        begin
          OutSL.Add('</ol>');
          OutSL.Add('<ul>');
          ListType := 'u';
        end;
        OutSL.Add('<li>' + ProcessInline(Copy(Trimmed, 3, Length(Trimmed))) + '</li>');
        Continue;
      end;

      if StartsWithStr(Trimmed, '1. ') or StartsWithStr(Trimmed, '2. ') or 
         StartsWithStr(Trimmed, '3. ') or StartsWithStr(Trimmed, '4. ') or
         StartsWithStr(Trimmed, '5. ') or StartsWithStr(Trimmed, '6. ') or
         StartsWithStr(Trimmed, '7. ') or StartsWithStr(Trimmed, '8. ') or
         StartsWithStr(Trimmed, '9. ') or StartsWithStr(Trimmed, '0. ') then
      begin
        if InParagraph then begin OutSL.Add('</p>'); InParagraph := False; end;
        if not InList then
        begin
          OutSL.Add('<ol>');
          InList := True;
          ListType := 'o';
        end;
        if ListType <> 'o' then
        begin
          OutSL.Add('</ul>');
          OutSL.Add('<ol>');
          ListType := 'o';
        end;
        j := Pos('.', Trimmed);
        OutSL.Add('<li>' + ProcessInline(Copy(Trimmed, j + 2, Length(Trimmed))) + '</li>');
        Continue;
      end;

      // Parágrafo
      if Trimmed = '' then
      begin
        if InParagraph then
        begin
          OutSL.Add('</p>');
          InParagraph := False;
        end;
        if InList then
        begin
          OutSL.Add('</' + ListType + 'l>');
          InList := False;
        end;
        Continue;
      end;

      if not InParagraph then
      begin
        OutSL.Add('<p>');
        InParagraph := True;
      end;
      OutSL.Add(ProcessInline(Trimmed));
    end;

    // Fechar tags abertas
    if InParagraph then OutSL.Add('</p>');
    if InList then OutSL.Add('</' + ListType + 'l>');

    Result := OutSL.Text;
  finally
    SL.Free;
    OutSL.Free;
    CodeBuffer.Free;
    ParaBuffer.Free;
  end;
end;

end.
