unit EditorConfigUtils;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, EditorConfigTypes;

type
  TLookUpResult = record
    editorConfigFile: string;
    filename_pattern: string;

    indent_style : string;  //set to tab or space to use hard tabs or soft tabs respectively.
    indent_size  : integer; //a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). When set to tab, the value of tab_width (if specified) will be used.
    tab_width    : integer; //a whole number defining the number of columns used to represent a tab character. This defaults to the value of indent_size and doesn't usually need to be specified.
    end_of_line  : string;  //set to lf, cr, or crlf to control how line breaks are represented.
    charset      : string;  //set to latin1, utf-8, utf-8-bom, utf-16be or utf-16le to control the character set.
    trim_trailing_whitespace : TTriBool; //set to true to remove any whitespace characters preceding newline characters and false to ensure it doesn't.
    insert_final_newline     : TTriBool; //
  end;

function LookupEditorConfig(const FileName: RawByteString; out res: TLookUpResult; IgnoreCase: Boolean = true): Boolean; overload;
function LookupEditorConfig(const FileName: UnicodeString; out res: TLookUpResult; IgnoreCase: Boolean = true): Boolean; overload;
function LookupEditorConfig(const FileName: WideString; out res: TLookUpResult; IgnoreCase: Boolean = true): Boolean; overload;

function FindMatching(const SrchFileName: string; cfg: TEditorConfigFile; IgnoreCase: Boolean): TEditorConfigEntry;

procedure InitLookupResult(out lk: TLookupResult);

function GetTabWidth(tab_width, indent_size: integer; const defVal: integer = -1): integer; overload;
function GetTabWidth(ec: TEditorConfigEntry; const defVal: integer = -1): integer; overload;
function GetTabWidth(const ec: TLookUpResult; const defVal: integer = -1): integer; overload;
function isTabIndent(const indent_style: string): Boolean;
function isSpaceIndent(const indent_style: string): Boolean;

procedure ReadFromFile(dst: TEditorConfigFile; const fn: string; usePath: Boolean = true);
procedure ReadFromStream(dst: TEditorConfigFile; src: TStream);
procedure ReadFromStrings(dst: TEditorConfigFile; str: TStrings);


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

function ParseIniLine(const iniline: string; const cfg: TIniSettings; out line: TIniLine): Boolean;
procedure IniSetting(out ini: TIniSettings; const AComment: TAnsiCharSet);

implementation

procedure IniSetting(out ini: TIniSettings; const AComment: TAnsiCharSet);
begin
  ini.Comment := AComment;
end;

function ParseIniLine(const iniline: string; const cfg: TIniSettings; out line: TIniLine): Boolean;
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

function GetTabWidth(tab_width, indent_size: integer; const defVal: integer = -1): integer;
begin
  if tab_width>=0 then Result := tab_width
  else if indent_size>=0 then Result := indent_size
  else Result := defVal;
end;

function GetTabWidth(ec: TEditorConfigEntry; const defVal: integer = -1): integer;
begin
  if not Assigned(ec) then Result := defVal
  else Result := GetTabWidth(ec.tab_width, ec.indent_size, defVal);
end;

function GetTabWidth(const ec: TLookUpResult; const defVal: integer = -1): integer;
begin
  Result := GetTabWidth(ec.tab_width, ec.indent_size, defVal);
end;

function isTabIndent(const indent_style: string): Boolean;
var
  l : string;
begin
  if (indent_style = '') then Result := false
  else begin
    l := lowercase(indent_style);
    Result := (l = 'tab') or (l='tabs');
  end;
end;

function isSpaceIndent(const indent_style: string): Boolean;
var
  l : string;
begin
  if (indent_style = '') then Result := false
  else begin
    l := lowercase(indent_style);
    Result := (l = 'space') or (l='spaces');
  end;
end;

function LowKVToEditConfig(const lowkey, value: string; dst: TEditorConfigEntry): Boolean;
begin
  Result := true;
  if lowkey='indent_style' then
    // set to tab or space to use hard tabs or soft tabs respectively.
    dst.indent_style := value
  else if lowkey='indent_size' then
    // a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). When set to tab, the value of tab_width (if specified) will be used.
    Result := TryStrToInt( value, dst.indent_size )
  else if lowkey = 'tab_width' then
    // a whole number defining the number of columns used to represent a tab character. This defaults to the value of indent_size and doesn't usually need to be specified.
    Result := TryStrToInt( value, dst.tab_width )
  else if lowkey = 'end_of_line' then
    // set to lf, cr, or crlf to control how line breaks are represented.
    dst.end_of_line := value
  else if lowkey = 'end_of_line' then
    // set to latin1, utf-8, utf-8-bom, utf-16be or utf-16le to control the character set.
    dst.charset := dst.charset
  else if lowkey = 'trim_trailing_whitespace' then
    // set to true to remove any whitespace characters preceding newline characters and false to ensure it doesn't.
    Result := TryStrToTriBool(value,  dst.trim_trailing_whitespace )
  else if lowkey = 'insert_final_newline' then
    // set to true to ensure file ends with a newline when saving and false to ensure it doesn't.
    Result := TryStrToTriBool(value, dst.insert_final_newline)
  else
    Result := false;
end;

function KVToEditConfig(const key,value: string; dst: TEditorConfigEntry): Boolean;
begin
  Result := LowKVToEditconfig( LowerCase(key), value, dst);
end;

procedure ReadFromStrings(dst: TEditorConfigFile; str: TStrings);
var
  i   : integer;
  kv  : string;
  cfg : TIniSettings;
  ent : TEditorConfigEntry;
  ln  : TIniLine;
begin
  ent := nil;
  IniSetting(cfg, [';','#']);
  for i:=0 to str.Count-1 do begin
    kv := trim(str[i]);
    if not ParseIniLine(kv, cfg, ln) then Continue;
    case ln.ltype of
      iltKeyValue: begin
        ln.key:=LowerCase(ln.key);
        if not Assigned(ent) then begin
          if ln.key = 'root' then
            TryStrToBool(ln.value, dst.root);
        end else
          LowKVToEditConfig(ln.key, ln.value, ent);
      end;
      iltHeader: ent := dst.AddEntry(ln.value);
    end;
  end;
end;

procedure ReadFromFile(dst: TEditorConfigFile; const fn: string; usePath: Boolean);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    ReadFromStream(dst, fs);
    if usePath then dst.filepath := fn;
  finally
    fs.Free;
  end;
end;

procedure ReadFromStream(dst: TEditorConfigFile; src: TStream);
var
  lines : TStringList;
begin
  lines := TStringList.Create;
  try
    lines.LoadFromStream(src);
    ReadFromStrings(dst, lines);
  finally
    lines.Free;
  end;
end;

function FindMatching(const SrchFileName: string; cfg: TEditorConfigFile; IgnoreCase: Boolean): TEditorConfigEntry;
var
  cmp : string;
  i   : integer;
  match : Boolean;
begin
  Result := nil;
  if not Assigned(cfg) or (SrchFileName = '') then Exit;

  if IgnoreCase then cmp := LowerCase(SrchFileName) // todo: this is UTF8 file name, so the proper UTF8 lower case should be used
  else cmp := SrchFileName;

  for i:=0 to cfg.Count-1 do begin
    Result := cfg[i];
    if IgnoreCase then
      match := ECMatch( LowerCase(Result.name), cmp)
    else
      match := ECMatch(Result.name, cmp);
    if match then Exit;
  end;
  Result := nil;
end;

procedure CopyEntry(src: TEditorConfigEntry; var dst : TLookupResult);
begin
  dst.filename_pattern := src.name;

  dst.indent_style := src.indent_style;
  dst.indent_size  := src.indent_size;
  dst.tab_width    := src.tab_width;
  dst.end_of_line  := src.end_of_line;
  dst.charset      := src.charset;
  dst.trim_trailing_whitespace := src.trim_trailing_whitespace;
  dst.insert_final_newline     := src.insert_final_newline;
end;

procedure InitLookupResult(out lk: TLookupResult);
begin
  FillChar(lk, sizeof(lk), 0);
end;

function LookupEditorConfig(const FileName: RawByteString; out res: TLookUpResult; IgnoreCase: Boolean): Boolean;
var
  pp   : string;
  pth  : string;
  done : Boolean;
  cfg  : string;
  ec   : TEditorConfigFile;
  ent  : TEditorConfigEntry;
  srch : string;
  fulln: string;
begin
  fulln := ExpandFileName(FileName);
  Result := false;
  pth := ExtractFilePath(fulln);
  done := false;
  InitLookupResult(res);

  while not done do begin
    cfg := pth+'.editorconfig';
    if FileExists(cfg) then begin
      ec := TEditorConfigFile.Create;
      try
        ReadFromFile(ec, cfg, true);
        srch := fulln;
        Delete(srch, 1, length(pth));
        ent := FindMatching(srch, ec, IgnoreCase);

        Result := Assigned(ent);
        if Result then begin
          CopyEntry(ent, res);
          res.editorConfigFile := cfg;
          Done := true;
        end;

        Done := Done or ec.root; // it's root no need to search any further
      finally
        ec.Free;
      end;
    end;

    // checking the next folder
    if not Done then begin
      if (pth = '') or (pth ='.') or (pth = PathDelim) then
        Done := true
      else begin
        pp := pth;
        pth := ExtractFilePath(ExtractFileDir(pth));
        done := pp = pth;
      end;
    end;
  end;
end;

function LookupEditorConfig(const FileName: UnicodeString; out res: TLookUpResult; IgnoreCase: Boolean): Boolean; overload;
begin
  Result := LookupEditorConfig( UTF8Encode(FileName), res, IgnoreCase);
end;

function LookupEditorConfig(const FileName: WideString; out res: TLookUpResult; IgnoreCase: Boolean): Boolean; overload;
begin
  Result := LookupEditorConfig( UTF8Encode(FileName), res, IgnoreCase );
end;

end.
