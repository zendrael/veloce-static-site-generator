unit veloce_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

function FileToString(const FileName: string): string;
procedure StringToFile(const FileName, Content: string);
function ExtractFileNameNoExt(const FileName: string): string;
function StartsWithStr(const S, Prefix: string): Boolean;
function EndsWithStr(const S, Suffix: string): Boolean;
function TrimString(const S: string): string;
function ReplaceAll(const S, OldPattern, NewPattern: string): string;
procedure CopyDir(const SrcDir, DstDir: string);
procedure DeleteDir(const Dir: string);
function GetFilesInDir(const Dir, Extension: string): TStringList;
function EnsureDir(const Dir: string): string;

implementation

function FileToString(const FileName: string): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if FileExists(FileName) then
      SL.LoadFromFile(FileName);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure StringToFile(const FileName, Content: string);
var
  SL: TStringList;
begin
  ForceDirectories(ExtractFilePath(FileName));
  SL := TStringList.Create;
  try
    SL.Text := Content;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

function ExtractFileNameNoExt(const FileName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
end;

function StartsWithStr(const S, Prefix: string): Boolean;
begin
  Result := Copy(S, 1, Length(Prefix)) = Prefix;
end;

function EndsWithStr(const S, Suffix: string): Boolean;
begin
  if Length(S) >= Length(Suffix) then
    Result := Copy(S, Length(S) - Length(Suffix) + 1, Length(Suffix)) = Suffix
  else
    Result := False;
end;

function TrimString(const S: string): string;
begin
  Result := Trim(S);
end;

function ReplaceAll(const S, OldPattern, NewPattern: string): string;
begin
  Result := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll]);
end;

procedure CopyDir(const SrcDir, DstDir: string);
var
  SR: TSearchRec;
  SrcFile, DstFile: string;
begin
  if not DirectoryExists(SrcDir) then Exit;
  ForceDirectories(DstDir);
  if FindFirst(SrcDir + DirectorySeparator + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      SrcFile := SrcDir + DirectorySeparator + SR.Name;
      DstFile := DstDir + DirectorySeparator + SR.Name;
      if (SR.Attr and faDirectory) <> 0 then
        CopyDir(SrcFile, DstFile)
      else
      begin
        ForceDirectories(ExtractFilePath(DstFile));
        CopyFile(SrcFile, DstFile, [cffOverwriteFile]);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure DeleteDir(const Dir: string);
var
  SR: TSearchRec;
  FileName: string;
begin
  if not DirectoryExists(Dir) then Exit;
  if FindFirst(Dir + DirectorySeparator + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      FileName := Dir + DirectorySeparator + SR.Name;
      if (SR.Attr and faDirectory) <> 0 then
        DeleteDir(FileName)
      else
        DeleteFile(FileName);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  RemoveDir(Dir);
end;

function GetFilesInDir(const Dir, Extension: string): TStringList;
var
  SR: TSearchRec;
  FullDir: string;
begin
  Result := TStringList.Create;
  FullDir := IncludeTrailingPathDelimiter(Dir);
  if FindFirst(FullDir + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Name = '.') or (SR.Name = '..') then Continue;
      if (SR.Attr and faDirectory) = 0 then
      begin
        if (Extension = '') or (SameText(ExtractFileExt(SR.Name), Extension)) then
          Result.Add(FullDir + SR.Name);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

function EnsureDir(const Dir: string): string;
begin
  Result := IncludeTrailingPathDelimiter(Dir);
end;

end.
