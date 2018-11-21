program readEditorConfig;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, EditorConfigTypes;

procedure DumpEntry(en: TEditorEntry);
begin
  writeln('[',en.name,']');
  writeln('indent_style=', en.indent_style);
  writeln('indent_size=', en.indent_size);
  writeln('tab_width=', en.tab_width);
  writeln('end_of_line=', en.end_of_line);
  writeln('charset=', en.charset);
  writeln('trim_trailing_whitespace=', en.trim_trailing_whitespace);
  writeln('insert_final_newline=', en.insert_final_newline);
end;

procedure Run(const fn: string);
var
  ed : TEditorConfigFile;
  i  : integer;
begin
  ed := TEditorConfigFile.Create;
  try
    ReadFromFile(ed, fn);
    for i:=0 to ed.Count-1 do begin
      DumpEntry(ed.Entry[i]);
    end;
  finally
    ed.Free;
  end;
end;

begin
  if (ParamCount=0) then begin
    writeln('please specify .editorconfig file to read');
    Exit;
  end;
  Run(ParamStr(1));
end.

