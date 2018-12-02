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

function BracePattern(const pat: string; var pidx: integer; const s: string; var sidx: integer): TBracePatResult;

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

function _ECMatch(const p: string; pidx: integer; const s: string; sidx: integer): Boolean;
var
  pi, i, j : integer;
  stoppath : Boolean;
  brres : TBracePatResult;
  ii : integer;
begin
  pi := pidx;
  i := sidx;
  Result := true;
  while Result and (pi <= length(p)) and (i<=length(s)) do begin
    //writeln('idx=',pi,' ',p[pi]);
    case p[pi] of
      '\': inc(pi); // escaped character
      '?': begin
        inc(pi);
        inc(i);
      end;
      '[': begin
          if isSetPattern(p, pi) then
            Result := _ParseSetEscape(p, pi, s, i)
          else begin
            Result := s[i] = p[pi];
            if Result then begin
              inc(pi);
              inc(i);
            end;
          end;
        end;

      '{': begin
          ii:=pi;
          brres := BracePattern(p, pi, s, i);
          //writeln('brres=',brres,' ',pi,'/',length(p),'; ',i,'/',length(s),' ',s);
          if brres = bprErrorSyntax then begin
            pi:=ii;
            Result := s[i] = p[pi];
            inc(pi);
            inc(i);
          end else
            Result := (brres <> bprNoMatch);

          {br := ctx.GetSet(p, pi);
          if Assigned(br) then begin
            Result := br.Match(s,i, false);
            if not Result then
              Result := br.Match(s,i, true);
          end else begin
            Result := s[i] = p[pi];
            inc(pi);
            inc(i);
          end;}
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
        end else
          for j:=i to length(s) do
            if stoppath and (s[j]='/') then begin
              Result := true; // continue the loop
              i:=j;
              break;
            end else begin
              Result := _ECMatch(p, pi, s, j);
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

function ECMatch(const pat, s: string): Boolean;
begin
  if (pat = '*') then begin // special case :(
    result:=s<>'';
    exit;
  end;
  Result := _ECMatch(pat, 1, s, 1);
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

function EscapePatMatch(const str, pat: string; var strOfs: Integer; patOfs: Integer; patLen: integer): Boolean;
var
  i,j : integer;
begin
  i:=strOfs;
  j:=patOfs;
  Result := false;
  while (patLen>0) and (i<=length(str)) and (j <= length(pat)) do begin
    if (pat[j]='\') then begin
      dec(patLen);
      inc(j);
    end;
    if (pat[j]<>str[i]) then Exit;
    inc(i);
    inc(j);
    dec(patLen);
  end;
  Result := patLen = 0;
  if Result then strOfs := i;
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

function isDoubleDot(const s: string; ofs: integer): Boolean;
begin
  Result := (ofs>0) and (ofs<length(s)) and (s[ofs]='.') and (s[ofs+1]='.');
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

function BraceRange(const pat: string; var pidx: integer; const s: string; var sidx: integer;
  const afirst: string): TBracePatResult;
var
  isnum : Boolean;
  num1  : Int64;
  num2  : Int64;
  numw1 : Integer;
  numw2 : Integer;
  frst  : string;
  i     : integer;
  sc    : string;
  vl    : string;
  vlpat : string;
  vlnum : int64;
  err   : Integer;
  delta : Int64;
  ch1, ch2, chval : Char;

begin
  if not BraceMinValue(afirst, isnum, num1, frst)  then begin
    Result := bprErrorSyntax;
    Exit;
  end;

  num2 := 0;
  if isnum then begin
    i:=pidx;
    if (pidx<=length(pat)) and (pat[pidx]='-') then inc(pidx);
    while (pat[pidx] in ['0'..'9']) do inc(pidx);
    if not BraceMinValue(Copy(pat, i, pidx-i), isnum, num2, sc) then begin
      Result := bprErrorSyntax;
      Exit;
    end;
    if not isnum then begin
      Result := bprErrorSyntax;
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
  delta := 0;

  if pat[pidx]<>'}' then begin
    Result := bprErrorSyntax;
    Exit;
  end;

  if isnum then begin
    i:=sidx;
    if (sidx<=length(s)) and (s[sidx]='-') then inc(sidx);
    while (s[sidx] in ['0'..'9']) do inc(sidx);

    vl := Copy(s, i, sidx-i);
    Val(vl, vlnum, err);
    if (err<>0) then begin
      Result := bprErrorSyntax;
      Exit;
    end;

    if not BraceNumInRange(vlnum, num1, num2, delta) then begin
      Result := bprNoMatch;
      Exit;
    end;

    numw1 := NumWidth(frst);
    numw2 := NumWidth(sc);
    if numw2>numw1 then numw1:=num2;

    vlpat := UnixIntToStr(vlnum, numw1);
    if vl=vlpat then
      Result := bprSuccess
    else
      Result := bprNoMatch;
  end else begin

    chval := s[sidx];
    ch1 := frst[1];
    ch2 := sc[1];

    if ch1<ch2 then begin
      if (chval>=ch1) and (chval<=ch2) then
        Result := bprSuccess
      else
        Result := bprNoMatch;
    end else begin
      if (chval>=ch2) and (chval<=ch1) then
        Result := bprSuccess
      else
        Result := bprNoMatch;
    end;
  end;
end;

function BracePattern(const pat: string; var pidx: integer; const s: string; var sidx: integer): TBracePatResult;
var
  i,j,ii  : integer;
  wofs    : integer;
  wordcnt : integer; //
  resp    : TBracePatResult;
  res     : TBracePatResult;
  first   : string;
  isRange : Boolean;
  resj    : Integer;
begin
  Result := bprNoMatch;
  i:=pidx;
  j:=sidx;
  //writeln(' pidx=',pidx,'; j=',j);
  if (i<=0) or (i>length(pat)) or (pat[i]<>'{') then begin
    Result := bprErrorSyntax;
    Exit;
  end;
  inc(i);

  wofs:=i;
  resj := j;
  wordcnt := 0;
  isRange := false;
  while (i<=length(pat)) and (pat[i]<>'}') do begin
    //writeln('i = ',i);
    if (pat[i]='{') then begin
      if (wofs < i) then begin
        // there's a text prior to nested {} - must check
        if EscapePatMatch(s, pat, j, wofs, i - wofs) then
          resp := bprSuccess
        else
          resp := bprNoMatch;
      end else
        resp := bprSuccess;
      //writeln(' BracePattern: i=',i,'; j=',j);
      res := BracePattern(pat, i, s, j);
      //writeln(' recurs resp = ',resp,' res=',res,' i=',i);
      if res = bprErrorSyntax then begin
        //inc(i);
      end else if (res = bprSuccess) then begin
        Result := res;
        wofs:=i;
      end;
      //writeln(' after i=',i,'; j=',j);
      //writeln(' need recursive ', Result);
    end else if pat[i]='\' then begin
      inc(i,2);
    end else if pat[i]=',' then begin
      //writeln('   ofs=',j);
      if EscapePatMatch(s, pat, j, wofs, i - wofs) then begin
        Result := bprSuccess;
        resj := j;
      end else
        j := sidx;
      inc(wordcnt);
      wofs:=i+1;
      inc(i);
    end else if pat[i]='.' then begin

      if isDoubleDot(pat, i) then begin
        ii := i;
        first := Copy(pat, wofs, i-wofs);
        //writeln('dot! wofs=',wofs,' ',i-wofs,' f=',first);
        inc(i,2);
        res := BraceRange(pat, i, s, j, first);
        if res = bprErrorSyntax then
          //i := ii+1 // revert to the previous position
        else begin
          isRange := true; // oh yes, it's range! so we mark it as many words
          Result := res;
        end;
        //writeln('after BraceRange = ', res);
      end else
        inc(i);
    end else
      inc(i);
  end;

  //writeln('after loop = ',pat[i]);
  if (i<=length(pat)) and (pat[i]='}') then begin
    //writeln('  at result: ', Result,'; i=', i);

    if not isRange and (Result<>bprSuccess) then begin
      if (wordcnt>0) then begin
        if EscapePatMatch(s, pat, j, wofs, i - wofs) then
          Result := bprSuccess
        else begin
          Result := bprNoMatch;
          j := sidx;
        end;
      end else
        Result :=bprErrorSyntax;
    end else if not isRange and (Result=bprSuccess) then begin
      j:=resj;
    end;

    inc(i);
    if Result <> bprErrorSyntax then sidx := j;
    pidx := i;
  end else begin
    Result := bprErrorSyntax;
    pidx := i;
  end;
end;


end.

