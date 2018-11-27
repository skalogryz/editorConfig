program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, EditorConfigUtils
  { you can add units after this };

var
  lk : TLookUpResult;
begin
  if ParamCount=0 then begin
    writeln('please provide the file name');
    exit;
  end;
  try
    if not LookupEditorConfig(ParamStr(1), lk) then
      writeln('no editorconfig found')
    else begin
      writeln('.editorconfig: ',lk.editorConfigFile);
      writeln('pattern:       ',lk.filename_pattern);
      writeln('filename:      ',ParamStr(1));
    end;

  except
    on e: exception do
      writeln('error: ',e.message);
  end;

end.

