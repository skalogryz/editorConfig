program editorconfig;
// this is a replica of editorconfig utility.
// The utility is used to run editorconfig-core

{$mode delphi}{$H+}

uses
  Windows,
  SysUtils, Classes, EditorConfigTypes, EditorConfigUtils, editorconfigcorepas;

procedure Run(file_paths: TStrings; const conf_filename, aversion: string);
var
  i : integer;
  j : integer;
  full_filename: string;
  eh: TEditorConfigHandle;
  err_num: Integer;
  name_value_count: integer;
  name, value: string;
begin
  // Go through all the files in the argument list
  for i := 0 to file_paths.Count-1 do begin

      full_filename := file_paths[i];

      // Print the file path first, with [], if more than one file is
      // specified
      if (file_paths.Count > 1) and (full_filename<> '-') then
          writeln('[',full_filename,']');

      (*
      if (!strcmp(full_filename, "-")) {
          int             len;

          /* Read a line from stdin. If EOF encountered, continue */
          if (!fgets(file_line_buffer, FILENAME_MAX + 1, stdin)) {
              if (!feof(stdin))
                  perror("Failed to read stdin");

              free(full_filename);
              continue;
          }

          -- i;

          /* trim the trailing space characters */
          len = strlen(file_line_buffer) - 1;
          while (len >= 0 && isspace(file_line_buffer[len]))
              -- len;
          if (len < 0) /* we meet a blank line */
              continue;
          file_line_buffer[len + 1] = '\0';

          full_filename = file_line_buffer;
          while (isspace(*full_filename))
              ++ full_filename;

          full_filename = strdup(full_filename);

          printf("[%s]\n", full_filename);
      }
      *)

      // Initialize the EditorConfig handle
      eh := TEditorConfigHandle.Create;

      // Set conf file name
      if (conf_filename<>'') then
        editorconfig_handle_set_conf_file_name(eh, conf_filename);

      // Set the version to be compatible with */
      if aversion<>'' then
        editorconfig_handle_set_version(eh, aversion);

      ///* parsing the editorconfig files */
      err_num := editorconfig_parse(full_filename, eh);

      if (err_num <> 0)then begin
          ///* print error message */
        writeln(stderr, editorconfig_get_error_msg(err_num));
          //if (err_num > 0)
              //fprintf(stderr, ":%d \"%s\"", err_num,
                      //editorconfig_handle_get_err_file(eh));
      ///    fprintf(stderr, "\n");
        Halt(1);
      end;

      // print the result
      name_value_count := editorconfig_handle_get_name_value_count(eh);
      for j := 0 to name_value_count-1 do begin
        editorconfig_handle_get_name_value(eh, j, name, value);
        writeln(name,'=',value);
      end;
      eh.Free;
      //if (editorconfig_handle_destroy(eh) != 0) {
          //fprintf(stderr, "Failed to destroy editorconfig_handle.\n");
          //exit(1);
      //}
  end;

end;

var
  gConfigFile : string = '';
  gReqVersion: string = '';
  gPrintHelp: Boolean = false;
  gPrintVersion: Boolean = false;
  gFiles : TStringList = nil;

function ParamStrSafe(i: integer): string;
begin
  if (i<0) or (i>ParamCount) then Result := ''
  else Result := ParamStr(i);
end;
procedure PrintVersion;
begin
  writeln('EditorConfig Pas Core Version '
    ,EditorConfig_Ver_Major
    ,'.'
    ,EditorConfig_Ver_Minor
    ,'.'
    ,EditorConfig_Ver_Release
    ,EditorConfig_Ver_Suffix);
end;

procedure PrintHelp;
begin
  writeln('Usage: editorconfig [OPTIONS] FILEPATH1 [FILEPATH2 FILEPATH3 ...]');
  writeln;
  writeln('FILEPATH can be a hyphen (-) if you want to path(s) to be read from stdin. Hyphen can also be specified with other file names. In this way, both file paths from stdin and the paths specified on the command line will be used. If more than one path specified on the command line, or the paths are reading from stdin (even only one path is read from stdin), the output format would be INI format, instead of the simple "key=value" lines.');
  writeln();
  writeln('-f	Specify conf filename other than ".editorconfig".');
  writeln('-b	Specify version (used by devs to test compatibility).');
  writeln('-h OR --help	Print this help message.');
  writeln('--version	Display version information.');
end;

procedure ParseParams;
var
  i : integer;
  s : string;
begin
  if not Assigned(gFiles) then gFiles := TStringList.Create;

  i:=1;
  while i<=ParamCount do begin
    s:=ParamStr(i);
    inc(i);
    if s='-f' then begin
      gConfigFile := ParamStrSafe(i);
      inc(i);
    end else if s = '-b' then begin
      gReqVersion := ParamStrSafe(i);
      inc(i);
    end else if (s = '-h') or (s = '--help') then begin
      gPrintHelp := true;
    end else if (s = '-v') or (s = '--version') then begin
      gPrintVersion := true;
    end else
      gFiles.Add(s);
  end;
end;

procedure RunBraceTest;
var
  i : integer;
  j : integer;
  f : text;
  ws : WideString;
  p, s : string;
  z : string;
begin
  if ParamCount>0 then p:=ParamStr(1) else p:='{a\,c,b}';
  if ParamCount>1 then s:=ParamStr(2) else s:='a,c';

  write('     ');
  for i:=1 to length(p) do begin
    z:=IntToStr(i);
    write(z[length(z)]);
  end;
  writeln;
  writeln('pat: ', p);

  write('     ');
  for i:=1 to length(s) do begin
    z:=IntToStr(i);
    write(z[length(z)]);
  end;
  writeln;
  writeln('str: ', s);

  i:=1;
  j:=1;
  writeln(BracePattern(p, i, s, j));
  exit;
end;

begin
  //RunBraceTest;
  //Exit;
{  AssignFile(f, 'C:\FPC_Laz\editorconfig\cmdline\editorconfig.txt');
  if not fileexists('C:\FPC_Laz\editorconfig\cmdline\editorconfig.txt')
    then Rewrite(f)
    else Append(f);
  writeln(f,GetCurrentDir);
  write(f,ParamStr(0));
  for i:=1 to ParamCount do
    write(f,' ',ParamStr(i));
  writeln(f);
  writeln(f, 'utf8:');
  ws := PWideChar(GetCommandLineW);
  writeln(f, UTF8Encode(ws));
  writeln(f);
  CloseFile(f);}

  try
    gFiles := TStringList.Create;
    try
      ParseParams;
      if gPrintVersion then begin
        PrintVersion;
        Exit;
      end else if (gFiles.Count = 0) or (gPrintHelp) then begin
        PrintHelp;
        Exit;
      end;
      Run(gFiles, gConfigFile, gReqVersion);
    finally
      gFiles.Free;
    end;
  except
    on e:exception do
      writeln(StdOut, e.Message);
  end;
end.

