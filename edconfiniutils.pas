unit EdConfIniUtils;

interface

uses SysUtils;

type
  TIniLineType = (iltComment, iltKeyValue, iltHeader, iltOther);

  TIniLine = record
    ltype : TIniLineType;
    value : string;
    key   : string;
  end;

  TAnsiCharSet = set of char;

  TIniSettings = record
    Comment: TAnsiCharSet;
  end;

function ParseIniFile(const iniline: string; const cfg: TIniSettings; out line: TIniLine): Boolean;
procedure IniSetting(out ini: TIniSettings; const AComment: TAnsiCharSet);

implementation

procedure IniSetting(out ini: TIniSettings; const AComment: TAnsiCharSet);
begin
  ini.Comment := AComment;
end;

function ParseIniFile(const iniline: string; const cfg: TIniSettings; out line: TIniLine): Boolean;
var
  s  : string;
  ln : integer;
  i  : integer;
begin
  line.key:='';
  line.value:='';
  line.ltype:=iltOther;
  s := Trim(iniline);
  Result:=true;
  if s='' then Exit;
  if (s[1] in cfg.Comment) then begin
    line.ltype:=iltComment;
  end else if (s[1] = '[') then begin
    ln:=length(s)-1;
    if (s[length(s)]=']') then dec(ln);
    line.value:=Copy(s, 2, ln);
    line.ltype:=iltHeader;
  end else begin
    i := Pos('=', s);
    if i<=0 then Exit;
    line.value:=Trim(Copy(s, i+1, length(s)));
    line.key:=Trim(Copy(s, 1, i-1));
    line.ltype:=iltKeyValue;
  end;
end;

end.
