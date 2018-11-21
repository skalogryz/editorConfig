unit EditorConfigTypes;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils;

type
  TTriBool = (
    tbUnset, // unspecified (an editor default sould be used)
    tbFalse, // false (specified)
    tbTrue   // true (specified)
  );

  { TEditorEntry }

  TEditorEntry = class(TObject)
  private
    fname        : string;
  public
    indent_style : string;  //set to tab or space to use hard tabs or soft tabs respectively.
    indent_size  : integer; //a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). When set to tab, the value of tab_width (if specified) will be used.
    tab_width    : integer; //a whole number defining the number of columns used to represent a tab character. This defaults to the value of indent_size and doesn't usually need to be specified.
    end_of_line  : string;  //set to lf, cr, or crlf to control how line breaks are represented.
    charset      : string;  //set to latin1, utf-8, utf-8-bom, utf-16be or utf-16le to control the character set.
    trim_trailing_whitespace : TTriBool; //set to true to remove any whitespace characters preceding newline characters and false to ensure it doesn't.
    insert_final_newline     : TTriBool; //set to true to ensure file ends with a newline when saving and false to ensure it doesn't.
    constructor Create(const aname: string);
    property name: string read fname;
  end;

  { TEditorConfigFile }

  TEditorConfigFile = class(TObject)
  private
    fitems : TList;
  protected
    function GetCount: Integer;
    function GetEntry(i: integer): TEditorEntry;
  public
    root : Boolean; // special property that should be specified at the top of the file outside of any sections. Set to true to stop .editorconfig files search on current file.
    filepath : String;
    constructor Create;
    destructor Destroy; override;
    function AddEntry(const aname: string): TEditorEntry;
    property Entry[i: integer]: TEditorEntry read GetEntry; default;
    property Count: Integer read GetCount;
  end;

procedure ReadFromFile(dst: TEditorConfigFile; const fn: string; usePath: Boolean = true);
procedure ReadFromStream(dst: TEditorConfigFile; src: TStream);
procedure ReadFromStrings(dst: TEditorConfigFile; str: TStrings);

implementation

uses
  EdConfIniUtils;

function TryStrToTriBool(const S: string; out Value: TTriBool): Boolean;
var
  l : string;
begin
  if S = '' then Value := tbUnset
  else begin
    l := AnsiLowerCase(s);
    if (l = '0') or (l = '0.0') or (l = 'false') then Value := tbFalse
    else Value := tbTrue;
  end;
  Result := true;
end;


function LowKVToEditConfig(const lowkey, value: string; dst: TEditorEntry): Boolean;
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

function KVToEditConfig(const key,value: string; dst: TEditorEntry): Boolean;
begin
  Result := LowKVToEditconfig( LowerCase(key), value, dst);
end;

procedure ReadFromStrings(dst: TEditorConfigFile; str: TStrings);
var
  i   : integer;
  kv  : string;
  cfg : TIniSettings;
  ent : TEditorEntry;
  ln  : TIniLine;
begin
  ent := nil;
  IniSetting(cfg, [';','#']);
  for i:=0 to str.Count-1 do begin
    kv := trim(str[i]);
    if not ParseIniFile(kv, cfg, ln) then Continue;
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

{ TEditorEntry }

constructor TEditorEntry.Create(const aname: string);
begin
  inherited Create;
  fname := aname;
  indent_size := -1; // negative = not set
  tab_width := -1; // negtive = not set
end;

{ TEditorConfigFile }

function TEditorConfigFile.GetCount: Integer;
begin
  Result := fitems.Count;
end;

function TEditorConfigFile.GetEntry(i: integer): TEditorEntry;
begin
  if (i<0) and (i>=fitems.Count) then Result:=nil
  else Result:=TEditorEntry(fitems[i]);
end;

constructor TEditorConfigFile.Create;
begin
  inherited Create;
  fitems := TList.Create;
end;

destructor TEditorConfigFile.Destroy;
var
  i : integer;
begin
  for i:=0 to fitems.Count-1 do TObject(fitems[i]).free;
  fitems.Free;
  inherited Destroy;
end;

function TEditorConfigFile.AddEntry(const aname: string): TEditorEntry;
begin
  Result := TEditorEntry.Create(aname);
  fitems.Add(Result);
end;

end.

