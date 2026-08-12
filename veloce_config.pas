unit veloce_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, veloce_utils;

type
  TVeloceConfig = record
    Title: string;
    Description: string;
    URL: string;
    Author: string;
    Language: string;
    Theme: string;
  end;

function LoadConfig(const FileName: string): TVeloceConfig;

implementation

function LoadConfig(const FileName: string): TVeloceConfig;
var
  SL: TStringList;
  i: Integer;
  Line, Key, Value: string;
  PosEq: Integer;
begin
  Result.Title := 'Veloce Site';
  Result.Description := '';
  Result.URL := '';
  Result.Author := '';
  Result.Language := 'pt-BR';
  Result.Theme := 'default';

  if not FileExists(FileName) then Exit;

  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    for i := 0 to SL.Count - 1 do
    begin
      Line := TrimString(SL[i]);
      if (Line = '') or (Line[1] = '#') then Continue;

      PosEq := Pos('=', Line);
      if PosEq > 0 then
      begin
        Key := LowerCase(TrimString(Copy(Line, 1, PosEq - 1)));
        Value := TrimString(Copy(Line, PosEq + 1, Length(Line)));
        // Remove aspas
        if (Length(Value) >= 2) and (Value[1] = '"') and (Value[Length(Value)] = '"') then
          Value := Copy(Value, 2, Length(Value) - 2);

        if Key = 'title' then Result.Title := Value
        else if Key = 'description' then Result.Description := Value
        else if Key = 'url' then Result.URL := Value
        else if Key = 'author' then Result.Author := Value
        else if Key = 'language' then Result.Language := Value
        else if Key = 'theme' then Result.Theme := Value;
      end;
    end;
  finally
    SL.Free;
  end;
end;

end.
