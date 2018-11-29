unit EditorConfigTypes;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

const
  EditorConfig_Ver_Major   = 0;
  EditorConfig_Ver_Minor   = 11;
  EditorConfig_Ver_Release = 0;
  EditorConfig_Ver_Suffix  = '';

type
  TTriBool = (
    tbUnset, // unspecified (an editor default sould be used)
    tbFalse, // false (specified)
    tbTrue   // true (specified)
  );

  TEditorConfigKeyVal = record
    key, value: string;
  end;

  { TEditorConfigEntry }

  TEditorConfigEntry = class(TObject)
  private
    fname        : string;
    procedure Unset(aindex: integer);
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
    function AddKeyVal(const aname, avalue: string): Boolean;
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

procedure TEditorConfigEntry.Unset(aindex: integer);
var
  c : integer;
begin
  if (aindex<0) or (aindex>=keyvalCount) then Exit;
  c:=keyvalCount - aindex - 1; // 2 - 1 = 1
  if c>0 then
    Move(keyval[aindex], keyval[aindex-1], sizeof(TEditorConfigKeyVal) * c);
  dec(keyvalCount);
end;

constructor TEditorConfigEntry.Create(const aname: string);
begin
  inherited Create;
  fname := aname;
  indent_size := -1; // negative = not set
  tab_width := -1; // negtive = not set
end;

function TEditorConfigEntry.AddKeyVal(const aname, avalue: string): Boolean;
var
  i : integer;
  ei : integer;
  n, v: string;
  isunset: Boolean;
begin
  n:=lowercase(aname);
  isunset := false;
  if isLowCaseValue(n) then begin
    v:=lowercase(avalue);
    isunset := (v = 'unset');
  end else
    v:=avalue;

  ei:=-1;
  for i:=0 to keyvalCount-1 do
    if keyval[i].key = n then begin
      ei:=i;
      Break;
    end;

  if isunset then begin
    if ei>=0 then Unset(ei);
    Result := true;
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
  end else
    keyval[ei].value := v;
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

type
  TECStrings = record
    offset, len : integer;
  end;

  { TBraceSet }

  TBraceSet = class(TObject)
  public
    isValid : Boolean; // foag, if the pattern Parsed() succesfully
    Pattern : string;
    FullIdx : integer;
    FullLen : integer;

    isNum    : Boolean;
    isRange  : Boolean;
    min,max  : Int64;
    intwidth : integer; // if 0 then no strict zero
    Count    : integer;
    Strs     : array of TECStrings;
    function Parse(const p: string; var pidx: integer): Boolean;
    function Match(const s: string; var sidx: Integer; CheckEmpty: Boolean): Boolean;
    procedure AddWord(aidx, alen: integer);
  end;

  { TECContext }

  TECContext = class
  private
    st : array of TBraceSet;
    count: integer;
  public
    destructor Destroy; override;
    function GetSet(const p: string; var pidx: integer): TBraceSet;
  end;

function _ECMatch(ctx: TECContext; const p: string; pidx: integer; const s: string; sidx: integer): Boolean;
var
  pi, i, j : integer;
  stoppath : Boolean;
  br: TBraceSet;
begin
  pi := pidx;
  i := sidx;
  Result := true;
  while Result and (pi <= length(p)) and (i<=length(s)) do begin
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
          br := ctx.GetSet(p, pi);
          if Assigned(br) then begin
            Result := br.Match(s,i, false);
            if not Result then
              Result := br.Match(s,i, true);
          end else begin
            Result := s[i] = p[pi];
            inc(pi);
            inc(i);
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
        end else
          for j:=i to length(s) do
            if stoppath and (s[j]='/') then begin
              Result := true; // continue the loop
              i:=j;
              break;
            end else begin
              Result := _ECMatch(ctx, p, pi, s, j);
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
var
  ctx: TECContext;
begin
  if (pat = '*') then begin // special case :(
    result:=s<>'';
    exit;
  end;
  ctx:=TECContext.Create;
  try
    Result := _ECMatch(ctx, pat, 1, s, 1);
  finally
    ctx.Free;
  end;
end;

function TBraceSet.Parse(const p: string; var pidx: integer): Boolean;
var
  j: integer;
  k: integer;

  knownum: Boolean;
  n1,n2: Int64;
  //pstart: Integer;
begin
  Pattern := p;
  FullIdx:=pidx;
  if (p[pidx] = '{') then inc(pidx);
  j:=pidx;
  knownum:=false;
  //pstart:=pidx;

  while (pidx <= length(p)) and (p[pidx]<>'}') do begin

    if (p[pidx]=',') then begin
      AddWord(j, pidx-j);
      j:=pidx+1;
      knownum := true; // this is a list of words, and not numbers range
      inc(pidx);

    end else if (p[pidx]='.') and not knownum and (pidx<length(p)) and (p[pidx+1]='.') then begin
      isRange:=true;
      knownum:=true;
      if TryStrToInt( Copy(p, j, pidx-j), n1) then begin
        j:=pidx+2;
        k:=j;
        while (k<=length(p)) and (p[k]<>'}') do inc(k);
        isNum := TryStrToInt( Copy(p, j, k-j), n2);
        pidx:=k;
        if isNum then begin
          min:=n1;
          max:=n2;
          if p[pidx] = '}' then break;
        end;
      end;
      if not isNum then inc(pidx);

    end else if (p[pidx]='\') then
      inc(pidx);

    inc(pidx);
  end;

  if not isNum then AddWord(j, pidx-j);
  Result := (pidx <= length(p)) and (p[pidx]='}');

  inc(pidx);
  FullLen := pidx - fullidx;
  Result := Result and (isRange) or (Count>1); // {single} - is not considered to be a brace expansion
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

function TBraceSet.Match(const s: string; var sidx: Integer; CheckEmpty: Boolean): Boolean;
var
  i,j : integer;
  leftlen : integer;
  v: Int64;
  err: integer;
  un: string;
  numst: string;
begin
  if not isNum then begin
    leftlen := length(s) - sidx + 1;
    Result := false;
    if not CheckEmpty then begin
      for i:=0 to Count-1 do
        if (Strs[i].len<=leftlen) and (Strs[i].len>0) then begin
          if CompareChar( s[sidx], Pattern[strs[i].offset], Strs[i].len)=0 then begin
            Result := true;
            inc(sidx, Strs[i].len);
            Exit;
          end;
        end;
    end else begin
      for i:=0 to Count-1 do
        if (Strs[i].len<=leftlen) and (Strs[i].len=0) then begin
          Result := true;
          Exit;
        end;
      Result := false;
    end
  end else begin
    Result := sidx<=length(s);
    if not Result then Exit;
    i:=sidx;
    j:=i;

    if (s[i] in [{'+',}'-']) then
      inc(i);

    while (i<=length(s)) and (s[i] in ['0'..'9']) do
      inc(i);
    numst:=Copy(s, j, i-j);

    Val(Copy(s, j, i-j), v, err);
    Result := (err = 0) and (v>=min) and (v<=max);
    if Result then begin
      // the number is numerically withing the range.
      // BUT, we also should check if it's matching as a string.
      // https://www.gnu.org/software/bash/manual/html_node/Brace-Expansion.html
      // "Supplied integers may be prefixed with ‘0’ to force each term to have the same width"
      // "It is strictly textual."
      // Thus the number must match. I.e.
      // 60 is {3..120} but, 060 is not. (060 is in {003..120})
      // Note: {-03..3} => -03 -02 -01 000 001 002 003
      sidx := i;
      un:=UnixIntToStr(v, intwidth);
      Result := un = numst;
    end;
    // 060
    // 3..120
    // 60
  end;
end;

procedure TBraceSet.AddWord(aidx, alen: integer);
begin
  if Count=length(Strs) then begin
    if Count=0 then SetLength(Strs,4)
    else SetLength(Strs,Count*2);
  end;
  Strs[Count].offset:=aidx;
  Strs[Count].len:=alen;
  inc(Count);
end;

{ TECContext }

destructor TECContext.Destroy;
var
  i: integer;
begin
  for i:=0 to count-1 do st[i].Free;
  inherited Destroy;
end;

function TECContext.GetSet(const p: string; var pidx: integer): TBraceSet;
var
  i : integer;
  j : integer;
begin
  j := pidx;
  for i:=0 to count-1 do
    if st[i].FullIdx=pidx then begin
      if st[i].isValid then begin
        Result := st[i];
        pidx := pidx+st[i].FullLen;
      end else
        Result := nil; // it has been probed before, and the pattern is invalid
      Exit;
    end;

  if count=length(st) then begin
    if count=0 then SetLength(st,4)
    else SetLength(st, count*2);
  end;
  Result := TBraceSet.Create;
  Result.isValid := Result.Parse(p, pidx);
  st[count]:=Result;
  inc(count);

  if not Result.isValid then begin
    pidx:=j; // restore
    Result := nil;
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

end.

