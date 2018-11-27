program testglob;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, EditorConfigTypes, EditorConfigUtils;

var
  pat : string;
  str : string;
  res : Boolean;
begin
  if ParamCount<2 then begin
    writeln('please specify a glob pattern and a string to match it to');
    Exit;
  end;

  try
    pat := ParamStr(1);
    str := ParamStr(2);
    writeln('pattern: ', pat);
    writeln('string:  ', str);
    res := ECMatch(pat, str);
    writeln(res);
  except
    on e:exception do writeln('Error: ', e.message);
  end;
end.

