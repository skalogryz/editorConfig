unit EditorConfigTypes;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

const
  EditorConfig_Ver_Major   = 0;
  EditorConfig_Ver_Minor   = 11;
  EditorConfig_Ver_Release = 0;
  EditorConfig_Ver_Suffix  = '';
  EditorConfig_Name = '.editorconfig';

type
  TTriBool = (
    tbUnset, // unspecified (an editor default sould be used)
    tbFalse, // false (specified)
    tbTrue   // true (specified)
  );

  TEditorConfigKeyVal = record
    key   : string;
    value : string;
  end;

  { TEditorConfigEntry }

  TEditorConfigEntry = class(TObject)
  private
    fname        : string;
    function _IndexOf(const aname: string): integer;
  public
    indent_style : string;  //set to tab or space to use hard tabs or soft tabs respectively.
    indent_size  : integer; //a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). When set to tab, the value of tab_width (if specified) will be used.
    tab_width    : integer; //a whole number defining the number of columns used to represent a tab character. This defaults to the value of indent_size and doesn't usually need to be specified.
    end_of_line  : string;  //set to lf, cr, or crlf to control how line breaks are represented.
    charset      : string;  //set to latin1, utf-8, utf-8-bom, utf-16be or utf-16le to control the character set.
    trim_trailing_whitespace : TTriBool; //set to true to remove any whitespace characters preceding newline characters and false to ensure it doesn't.
    insert_final_newline     : TTriBool; //set to true to ensure file ends with a newline when saving and false to ensure it doesn't.

    keyval  : array of TEditorConfigKeyVal;
    keyvalCount : integer;
    constructor Create(const aname: string);
    function AddKeyVal(const aname, avalue: string; overwriteValue: Boolean = true): Boolean;
    function IndexOf(const aname: string): integer;
    property name: string read fname;
  end;

  { TEditorConfigFile }

  TEditorConfigFile = class(TObject)
  private
    fItems : array of TEditorConfigEntry;
    fItemsCount: integer;
  protected
    function GetCount: Integer;
    function GetEntry(i: integer): TEditorConfigEntry;
  public
    root : Boolean; // special property that should be specified at the top of the file outside of any sections. Set to true to stop .editorconfig files search on current file.
    filepath : String;
    destructor Destroy; override;
    function AddEntry(const aname: string): TEditorConfigEntry;
    property Entry[i: integer]: TEditorConfigEntry read GetEntry; default;
    property Count: Integer read GetCount;
  end;

function TryStrToTriBool(const S: string; out Value: TTriBool): Boolean;

// usual file mask as described: https://en.wikipedia.org/wiki/Glob_(programming)
function FileNameMatch(const pat, s: string): Boolean;

// editorConfig match, as desribed: https://editorconfig.org/#supported-properties
// todo: handle characters escape
function ECMatch(const pat, s: string): Boolean;

// converts an integer, to a number by specified width.
// if width is zero or negative, performs simple Str()
// otherwise it would also stuff the number with zeros.
// "-" always go in front of any number.
//  UnixIntToStr(-3, 3) = -03
//  UnixIntToStr( 3, 3) = 003
function UnixIntToStr(i: Int64; w: integer): string;

type
  TEditorConfigVersion = record
    major, minor, release: Integer;
  end;

const
  DefaultVersion : TEditorConfigVersion = (
    major   : EditorConfig_Ver_Major;
    minor   : EditorConfig_Ver_Minor;
    release : EditorConfig_Ver_Release
  );

function isVersionGreaterThan(const ver: TEditorConfigVersion; amajor, aminor, arelease: integer): Boolean;

function StrToVersion(const s: string; out ver: TEditorConfigVersion): Boolean;

// sets up default properties
// tab_width - to ident_size (if not set)
// (0.9.0+) indent_size - to tab_width, if tab_width is specified and indent_style=tab
procedure SetDefaultProps(entry: TEditorConfigEntry; const v: TEditorConfigVersion); overload;
procedure SetDefaultProps(entry: TEditorConfigEntry); overload;

const
  prop_indent_style             = 'indent_style';
  prop_indent_size              = 'indent_size';
  prop_tab_width                = 'tab_width';
  prop_end_of_line              = 'end_of_line';
  prop_charset                  = 'charset';
  prop_trim_trailing_whitespace = 'trim_trailing_whitespace';
  prop_insert_final_newline     = 'insert_final_newline';

function isLowCaseValue(const propname: string): Boolean;
function FileNameToCheckName(const nm: string): string;
// if the pattern starts with "/" it would be removed
function NormalizePattern(const pat: string): string;

type
  TBracePatResult = (
    bprNoMatch,
    bprErrorSyntax,
    bprSuccess
  );

function BracePattern(const pat: string; var pidx: integer;  MajPatLen: integer; const s: string; var sidx: integer): TBracePatResult;

type
  TAnsiChars = set of AnsiChar;

type
  TBraceInfo = record
    isRange    : Boolean;
    rngIsNum   : Boolean;
    rngWidth   : Integer;
    rngCh1     : AnsiChar;
    rngCh2     : AnsiChar;
    rngInt1    : int64;
    rngInt2    : Int64;

    commaCount : integer;
    nextIdx    : integer;
    ofs        : array of integer;
    ofsCount   : integer;
  end;

function BracePatternInfo(const pat: string; i: integer; out info: TBraceInfo): Boolean;
procedure BraceInfoAddOfs(var bi: TBraceInfo; ofs: integer);

implementation

function TryStrToInt(const S: string; out v: Integer): Boolean; overload;
var
  Err : integer;
begin
  Val(S, v, err);
  Result := err = 0;
end;

function TryStrToInt(const s: string; out v: Int64): Boolean; overload;
var
  err : integer;
begin
  Val(s, v, err);
  Result := err = 0;
end;

function TryStrToTriBool(const S: string; out Value: TTriBool): Boolean;
var
  l : string;
begin
  if S = '' then Value := tbUnset
  else begin
    l := LowerCase(s);
    if (l = '0') or (l = '0.0') or (l = 'false') then Value := tbFalse
    else Value := tbTrue;
  end;
  Result := true;
end;

{ TEditorConfigEntry }

function TEditorConfigEntry._IndexOf(const aname: string): integer;
var
  i : integer;
begin
  Result := -1;
  for i:=0 to keyvalCount-1 do
    if keyval[i].key = aname then begin
      Result:=i;
      Exit;
    end;
end;

constructor TEditorConfigEntry.Create(const aname: string);
begin
  inherited Create;
  fname := aname;
  indent_size := -1; // negative = not set
  tab_width := -1; // negtive = not set
end;

function TEditorConfigEntry.AddKeyVal(const aname, avalue: string; overwriteValue: Boolean): Boolean;
var
  //i : integer;
  ei : integer;
  n, v: string;
begin
  n:=lowercase(aname);
  if isLowCaseValue(n) then begin
    v:=lowercase(avalue);
  end else
    v:=avalue;

  ei:=IndexOf(n);

  // do not overwrite
  if not overwriteValue and (ei>=0) then begin
    Result := false;
    Exit;
  end;

  Result := true;

  if n = prop_indent_style then
    // set to tab or space to use hard tabs or soft tabs respectively.
    indent_style := v
  else if n = prop_indent_size then begin
    // this is a special case (for Core) of defaulting tab_width to ident_size value :(
    // a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). When set to tab, the value of tab_width (if specified) will be used.
    TryStrToInt( v, indent_size )
  end else if n = prop_tab_width then
    // a whole number defining the number of columns used to represent a tab character. This defaults to the value of indent_size and doesn't usually need to be specified.
    TryStrToInt( v, tab_width )
  else if n = prop_end_of_line then
    // set to lf, cr, or crlf to control how line breaks are represented.
    end_of_line := v
  else if n = prop_charset then
    // set to latin1, utf-8, utf-8-bom, utf-16be or utf-16le to control the character set.
    charset := v
  else if n = prop_trim_trailing_whitespace then
    // set to true to remove any whitespace characters preceding newline characters and false to ensure it doesn't.
    TryStrToTriBool(v,  trim_trailing_whitespace )
  else if n = prop_insert_final_newline then
    // set to true to ensure file ends with a newline when saving and false to ensure it doesn't.
    TryStrToTriBool(v, insert_final_newline);

  if ei < 0 then begin
    if keyvalCount = length(keyval) then begin
      if keyvalCount =0 then setLength(keyval, 8)
      else SetLength(keyval, keyvalCount*2);
    end;
    keyval[keyvalCount].key := n;
    keyval[keyvalCount].value := v;
    inc(keyvalCount);
  end else begin
    if keyval[ei].value = 'unset' then Exit;
    keyval[ei].value := v;
  end;
end;

function TEditorConfigEntry.IndexOf(const aname: string): integer;
begin
  Result := _IndexOf( lowercase(aname));
end;

{ TEditorConfigFile }

function TEditorConfigFile.GetCount: Integer;
begin
  Result := fitemsCount;
end;

function TEditorConfigFile.GetEntry(i: integer): TEditorConfigEntry;
begin
  if (i<0) and (i>=fitemsCount) then Result:=nil
  else Result:=TEditorConfigEntry(fitems[i]);
end;

destructor TEditorConfigFile.Destroy;
var
  i : integer;
begin
  for i:=0 to fitemsCount-1 do
    fitems[i].free;
  inherited Destroy;
end;

function TEditorConfigFile.AddEntry(const aname: string): TEditorConfigEntry;
begin
  Result := TEditorConfigEntry.Create(aname);
  if fItemsCount = length(fItems) then begin
    if fItemsCount = 0 then SetLength(fItems, 4)
    else SetLength(fItems, fItemsCount*2);
  end;
  fItems[fItemsCount]:=Result;
  inc(fitemsCount);
end;

function _ParseSet(const p: string; var pidx: integer; const s: string; var sidx: integer): Boolean;
var
  ch : char;
begin
  if (p[pidx]='[') then inc(pidx);
  ch := s[sidx];
  Result := false;
  while (pidx<=length(p)) and (p[pidx]<>']') do begin
    if not Result then begin
      if ((pidx<length(p)) and( p[pidx+1]='-')) then begin
        Result := (ch >= p[pidx]) and (ch <= p[pidx+2]);
        inc(pidx,3);
      end else begin
        Result := p[pidx]=ch;
        inc(pidx);
      end;
    end else
      inc(pidx)
  end;
  inc(pidx);
  if Result then inc(sidx);
end;

function isSetPattern(const p: string; i: integer): Boolean;
begin
  Result := (i<=length(p)) and (p[i]='[');
  if Result then begin
    inc(i);
    while (i<=length(p)) and (p[i]<>']') do begin
      if (p[i]='/') then break
      else if (p[i]='\') then inc(i); // skip over the next character
      inc(i);
    end;
    Result := (i<=length(p)) and (p[i]=']');
  end;
end;

function _ParseSetEscape(const p: string; var pidx: integer; const s: string; var sidx: integer): Boolean;
var
  ch : char;
  isneg : Boolean;
const
  PATH_SEPARATOR = '/';
begin
  if (p[pidx]='[') then inc(pidx);
  ch := s[sidx];
  Result := false;

  if (ch = PATH_SEPARATOR) then
    // [] are hanlded in editorConfig (similar to gitignore)
    // as fnmatch() function with FNM_PATHNAME set.
    // That means that '/' character cannot match to any wildcard (? or * or [])

    // todo: this should be options
    Exit;

  isneg := (pidx<=length(p)) and (p[pidx]='!');
  if isneg then inc(pidx);

  while (pidx<=length(p)) and (p[pidx]<>']') do begin
    if not Result then begin
      if ((pidx<length(p)) and( p[pidx+1]='-')) then begin
        Result := (ch >= p[pidx]) and (ch <= p[pidx+2]);
        inc(pidx,3);
      end else begin
        if p[pidx] = '\' then inc(pidx);
        Result := p[pidx]=ch;
        inc(pidx);
      end;
    end else
      inc(pidx)
  end;

  if isneg then Result := not Result;

  inc(pidx);
  if Result then inc(sidx);
end;

function _FileNameMatch(const p: string; pidx: integer; const s: string; sidx: integer): Boolean;
var
  pi, i: integer;
  j : integer;
begin
  pi := pidx;
  i := sidx;
  Result := true;
  while Result and (pi <= length(p)) and (i<=length(s)) do begin
    case p[pi] of
      '?': begin
        inc(pi);
        inc(i);
      end;
      '[': Result := _ParseSet(p, pi, s, i);
      '*': begin
        inc(pi);
        while (pi<=length(p)) and (p[pi]='*') do
          inc(pi);

        if (pi>length(p)) then begin
          Result := true;
          Exit;
        end else
          for j:=sidx to length(s) do
          begin
            Result := _FileNameMatch(p, pi, s, j);
            if Result then Exit;
          end;
      end;
    else
      if s[i] = p[pi] then begin
        inc(pi);
        inc(i);
      end else
        Result := false;
    end;
  end;
  Result := Result and (i > length(s)) and (pi>length(p));
end;

function FileNameMatch(const pat, s: string): Boolean;
begin
  Result := _FileNameMatch(pat, 1, s, 1);
end;

function _ECMatch(const p: string; var pidx: integer; const s: string; var sidx: integer; { const stopChars: TAnsiChars = []}patlen: integer = -1): Boolean;
var
  pi, i, j : integer;
  stoppath : Boolean;
  brres : TBracePatResult;
  ii, jj : integer;
  maxpat: integer;
begin
  pi := pidx;
  i := sidx;
  Result := true;

  if patlen < 0 then maxpat := length(p)
  else maxpat := pidx + patlen - 1;

  while Result and (pi <= maxpat) and (i<=length(s)) do begin
    case p[pi] of
      '\': begin
        inc(pi); // escaped character
        if (pi<=length(p)) and (s[i] = p[pi]) then begin
          inc(pi);
          inc(i);
        end else
          Result := false;
      end;
      '?': begin
        inc(pi);
        inc(i);
      end;
      '[': begin
          if isSetPattern(p, pi) then begin
            Result := _ParseSetEscape(p, pi, s, i);
          end else begin
            Result := s[i] = p[pi];
            if Result then begin
              inc(pi);
              inc(i);
            end;
          end;
        end;

      '{': begin
          ii:=pi;
          brres := BracePattern(p, pi, patlen, s, i);
          if brres = bprErrorSyntax then begin
            pi:=ii;
            Result := s[i] = p[pi];
            inc(pi);
            inc(i);
          end else begin
            Result := (brres <> bprNoMatch);
            Break;
          end;
        end;
      '*': begin
        inc(pi);
        stoppath := true;
        while (pi<=length(p)) and (p[pi]='*') do begin
          stoppath := false;
          inc(pi);
        end;

        if (pi>length(p)) then begin
          if not stoppath then
            Result := true
          else begin
            Result :=true;
            for j:=i to length(s) do
              if s[j]='/' then begin
                Result := false;
                break;
              end;
          end;
          i:=length(s)+1;
        end else begin
          ii := pi;
          for j:=i to length(s) do
            if stoppath and (s[j]='/') then begin
              Result := true; // continue the loop
              i:=j;
              break;
            end else begin
              jj := j;
              pi := ii;
              Result := _ECMatch(p, pi, s, jj, maxpat-pi+1);
              if Result then begin
                pidx := pi;
                sidx := jj;
                Exit;
              end else if not stoppath and (p[pi]='/') and (s[j-1]='/') then begin
                // handle cases for [d/**/z.c] matching /glob/d/z.c
                inc(pi);
                Result := _ECMatch(p, pi, s, jj, maxpat-pi+1);
                if Result then begin
                  pidx := pi;
                  sidx := jj;
                  Exit;
                end;
              end;
            end;
        end;
      end;
    else
      if s[i] = p[pi] then begin
        inc(pi);
        inc(i);
      end else begin
        Result := false;
      end;
    end;
  end;

  pidx := pi;
  sidx := i;
end;

function ECMatch(const pat, s: string): Boolean;
var
  pi: integer;
  i : integer;
begin
  if (pat = '*') then begin // special case :(
    result:=s<>'';
    exit;
  end;
  pi := 1;
  i := 1;
  Result := _ECMatch(pat, pi, s, i);
  Result := Result and (i > length(s)) and (pi>length(pat));
end;

function UnixIntToStr(i: Int64; w: integer): string;
var
  s: string;
begin
  Str(i, Result);
  if (w>0) and (length(Result)<w)then begin
    s := StringOfChar('0', w-length(Result));
    if i<0 then begin
      Result := '-'+s+Copy(Result, 2, length(result))
    end else
      Result := s+Result
  end;
end;

function isVersionGreaterThan(const ver: TEditorConfigVersion; amajor, aminor, arelease: integer): Boolean;
begin
  Result := (ver.major > amajor)

    or ((ver.major = amajor)
         and (ver.minor > aminor))

    or ((ver.major = amajor)
         and (ver.minor = aminor)
         and (ver.release > arelease));
end;

procedure SetDefaultProps(entry: TEditorConfigEntry; const v: TEditorConfigVersion); overload;
var
  i           : integer;
  idsz        : string;
  IdStyle     : string;
  tabw        : string;
  hasIdSize   : boolean;
  hasIdStyle  : boolean;
  hasTabWidth : boolean;
begin
  if not Assigned(entry) then Exit;

  idsz := '';
  IdStyle := '';
  tabw := '';
  hasTabWidth := false;
  hasIdSize := false;
  hasIdStyle := false;
  for i:=0 to entry.keyvalCount -1 do begin
    if entry.keyval[i].key = prop_indent_size then begin
      hasIdSize := true;
      idsz:=entry.keyval[i].value;
    end else if entry.keyval[i].key = prop_tab_width then begin
      hasTabWidth := true;
      tabw := entry.keyval[i].value;
    end else if entry.keyval[i].key = prop_indent_style then begin
      hasIdStyle := true;
      IdStyle := entry.keyval[i].value;
    end;
  end;
  if hasIdSize and not hastabWidth and (idsz<>'tab') then
    entry.AddKeyVal(prop_tab_width, idsz)
  else if isVersionGreaterThan(v, 0,8,0) and hasIdStyle and (IdStyle='tab') and not hasIdSize then
  begin
    if not hasTabWidth then tabw := 'tab';
    entry.AddKeyVal(prop_indent_size, tabw);
  end;
end;

procedure SetDefaultProps(entry: TEditorConfigEntry);
begin
  SetDefaultProps(entry, DefaultVersion);
end;

function StrToVersion(const s: string; out ver: TEditorConfigVersion): Boolean;
var
  i : integer;
  j : integer;
  n : integer;

  procedure RecordStr(const recs: string; vernum: integer);
  var
    t : integer;
    err : integer;
  begin
    Val(recs, t, err);
    if err = 0 then begin
      if vernum = 0 then ver.major := t
      else if vernum = 1 then ver.minor := t
      else if vernum = 2 then ver.release := t;
    end;
  end;

begin
  ver.major:=0;
  ver.minor:=0;
  ver.release:=0;
  i:=1;
  j:=1;
  n:=0;
  while i<=length(s) do begin
    if not (s[i] in ['0'..'9']) then begin
      RecordStr(Copy(s, j, i-j), n);
      inc(n);
      j:=i+1;
    end;
    inc(i);
  end;
  if j<i then
    RecordStr(Copy(s, j, i-j), n);
  Result := n=2;
end;

function isLowCaseValue(const propname: string): Boolean;
begin
  Result :=
    (propname = prop_indent_style)
    or (propname = prop_indent_size)
    or (propname = prop_tab_width)
    or (propname = prop_end_of_line)
    or (propname = prop_charset)
    or (propname = prop_trim_trailing_whitespace)
    or (propname = prop_insert_final_newline)
end;

function FileNameToCheckName(const nm: string): string;
var
  i : integer;
begin
  Result:=nm;
  for i:=1 to length(Result) do begin
    if Result[i] = '\' then Result[i] := '/';
  end;
end;

function NormalizePattern(const pat: string): string;
begin
  if (pat<>'') and (pat[1]='/') then
    Result := Copy(pat, 2, length(pat))
  else
    Result := pat;
end;

function BraceMinValue(const p: string; out isNumeric: Boolean;
  out Number: Int64; out str: string): Boolean;
var
  err : integer;
begin
  Result :=(length(p)>0);
  if Result then begin
    Val(p, Number, err);
    Result := (err=0) or (length(p)=1);
    isNumeric := err = 0;
    str := p;
  end;
end;

function BraceNumInRange(const CheckVal, Range1, Range2, Delta: Int64): Boolean;
begin
  if Range1<Range2 then
    Result := (CheckVal>=Range1) and (CheckVal<=Range2)
  else
    Result := (CheckVal>=Range2) and (CheckVal<=Range1);
end;

function NumWidth(const n: string): Integer;
var
  isneg : Boolean;
  i : integer;
begin
  Result := 0;
  if n ='' then Exit
  else if n = '0' then Exit
  else if n = '-0' then Exit;

  isneg := n[1] = '-';
  if isneg then i:=2 else i:=1;
  if (n[i]='0') then Result:=length(n);
end;


function isBraceRange(const pat: string; var pidx: integer; const afirst: string; var info: TBraceInfo): Boolean;
var
  isnum : Boolean;
  num1  : Int64;
  num2  : Int64;
  numw  : Integer;
  frst  : string;
  i     : integer;
  sc    : string;
begin
  info.isRange := false;
  if not BraceMinValue(afirst, isnum, num1, frst)  then begin
    Result := false;
    Exit;
  end;

  num2 := 0;
  if isnum then begin
    i:=pidx;
    if (pidx<=length(pat)) and (pat[pidx]='-') then inc(pidx);
    while (pat[pidx] in ['0'..'9']) do inc(pidx);
    if not BraceMinValue(Copy(pat, i, pidx-i), isnum, num2, sc) then begin
      Result := false;
      Exit;
    end;
    if not isnum then begin
      Result := false;
      Exit;
    end;
  end else begin
    if pat[pidx]='\' then begin
      sc:=Copy(pat, pidx, 2);
      inc(pidx, 2);
      sc:=sc[2];
    end else begin
      sc := pat[pidx];
      inc(pidx);
    end;
  end;

  //todo: check range

  Result := true;
  info.isRange := true;
  info.rngIsNum := isnum;
  if isnum then begin
    info.rngInt1:=num1;
    info.rngInt2:=num2;

    info.rngWidth:=NumWidth(frst);
    numw:= NumWidth(sc);
    if numw>info.rngWidth then info.rngWidth:=numw;

  end else begin
    info.rngCh1:=frst[1];
    info.rngCh2:=sc[1];
  end;
end;


function BraceRangeMarge(const s: string; var sidx: integer; const info: TBraceInfo): Boolean;
var
  i     : integer;
  vl    : string;
  err   : integer;
  delta : integer;
  vlnum : int64;
  vlpat : string;
  chval : AnsiChar;
  ch1   : AnsiChar;
  ch2   : AnsiChar;
begin
  delta:=0; //todo: this should come from info
  Result := false;
  if info.rngIsNum then begin
    i:=sidx;
    if (sidx<=length(s)) and (s[sidx]='-') then inc(sidx);
    while (s[sidx] in ['0'..'9']) do inc(sidx);

    vl := Copy(s, i, sidx-i);
    Val(vl, vlnum, err);
    if (err<>0) then Exit;

    if not BraceNumInRange(vlnum, info.rngInt1, info.rngInt2, delta) then
      Exit;
    vlpat := UnixIntToStr(vlnum, info.rngWidth);
    Result := vl=vlpat;
  end else begin

    chval := s[sidx];
    ch1 := info.rngCh1;
    ch2 := info.rngCh2;

    if ch1<ch2 then
      Result := (chval>=ch1) and (chval<=ch2)
    else
      Result := (chval>=ch2) and (chval<=ch1);
  end;
end;

function BracePatternInfo(const pat: string; i: integer; out info: TBraceInfo): Boolean;
var
  lvl : integer;
  j   : integer;
begin
  FillChar(info, sizeof(info),0);

  Result := pat[i] = '{';
  if not Result then Exit;
  inc(i);
  j:=i;

  lvl:=0;
  while (i<=length(pat)) do begin
    case pat[i] of
      '.': if (lvl=0) and (info.commaCount=0) and (i<length(pat)) and (pat[i+1]='.') then begin
             inc(i,2);
             if isBraceRange(pat, i, Copy(pat, j, i-j-2), info) then begin
               Result := (i<=length(pat)) and (pat[i]='}');
               if result then inc(i);
               info.nextIdx:=i;
               Exit;
             end;
           end;
      ',': if (lvl=0) then begin
             if info.commaCount=0 then BraceInfoAddOfs(info, j);
             BraceInfoAddOfs(info, i+1);
             inc(info.commaCount);
           end;
      '{': inc(lvl);
      '}': begin
        if lvl = 0 then begin
          info.nextIdx:=i+1;
          Result := info.commaCount>0;
          Exit;
        end else
          dec(lvl);
      end;
      '\': inc(i);
    end;
    inc(i);
  end;
  Result := false;
end;

procedure BraceInfoAddOfs(var bi: TBraceInfo; ofs: integer);
begin
  if bi.ofsCount=length(bi.ofs) then begin
    if bi.ofsCount = 0 then SetLength(bi.ofs, 4)
    else setLength(bi.ofs, bi.ofsCount * 2);
  end;
  bi.ofs[bi.ofsCount]:=ofs;
  inc(bi.ofsCount);
end;

function BracePattern(const pat: string; var pidx: integer; MajPatLen: integer; const s: string; var sidx: integer): TBracePatResult;
var
  i,j      : integer;
  si,wofs  : integer;
  bi       : TBraceInfo;
  rescheck : Boolean;
  patlen   : integer;
  initpidx : integer;
begin
  if not BracePatternInfo(pat, pidx, bi) then begin
    Result := bprErrorSyntax;
    Exit;
  end;
  initpidx:=pidx;
  Result := bprNoMatch;
  if bi.isRange then begin
    if BraceRangeMarge(s, sidx, bi) then begin
      pidx := bi.nextIdx;
      if (pidx <= length(pat)) or (sidx<=length(s)) then begin
        if _ECMatch(pat, pidx, s, sidx, MajPatLen - (pidx-initpidx+1)) then
          Result := bprSuccess
        else
          Result := bprNoMatch;
      end else
        Result := bprSuccess;
    end;
  end else begin
    inc(pidx);
    wofs := pidx;
    si := sidx;
    rescheck := false;
    for i:=0 to bi.ofsCount-1 do begin
      pidx := wofs;
      sidx := si;
      j := bi.ofs[i];
      if (pat[j]<>',') then begin
        pidx := j;
        if i<bi.ofsCount-1 then patlen := bi.ofs[i+1]-j-1
        else patlen := bi.nextIdx-1-j;
        rescheck := _ECMatch(pat, pidx, s, sidx, patlen);
        rescheck := rescheck and (pidx = j+patlen);
      end else
        rescheck := true;

      if rescheck then begin
        pidx := bi.nextIdx;
        if (pidx <= length(pat)) or (sidx<=length(s)) then begin
          rescheck := _ECMatch(pat, pidx, s, sidx, MajPatLen - (pidx-initpidx+1));
        end;
      end;

      if rescheck then break;
    end;
    if rescheck then Result := bprSuccess;
  end;
end;

end.

