unit EditorConfigTypes;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

const
  EditorConfig_Ver_Major   = 1;
  EditorConfig_Ver_Minor   = 0;
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
    function AddKeyVal(const aname, avalue: string; force: Boolean = false): Boolean;
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

constructor TEditorConfigEntry.Create(const aname: string);
begin
  inherited Create;
  fname := aname;
  indent_size := -1; // negative = not set
  tab_width := -1; // negtive = not set
end;

function TEditorConfigEntry.AddKeyVal(const aname, avalue: string; force: Boolean): Boolean;
var
  i : integer;
  ei : integer;
begin
  ei:=-1;
  for i:=0 to keyvalCount-1 do
    if keyval[i].key = aname then begin
      ei:=i;
      Break;
    end;

  if (ei>=0) and not force then begin
    Result := false;
    Exit;
  end;
  Result := true;

  if aname='indent_style' then
    // set to tab or space to use hard tabs or soft tabs respectively.
    indent_style := avalue
  else if aname='indent_size' then
    // a whole number defining the number of columns used for each indentation level and the width of soft tabs (when supported). When set to tab, the value of tab_width (if specified) will be used.
    TryStrToInt( avalue, indent_size )
  else if aname = 'tab_width' then
    // a whole number defining the number of columns used to represent a tab character. This defaults to the value of indent_size and doesn't usually need to be specified.
    TryStrToInt( avalue, tab_width )
  else if aname = 'end_of_line' then
    // set to lf, cr, or crlf to control how line breaks are represented.
    end_of_line := avalue
  else if aname = 'charset' then
    // set to latin1, utf-8, utf-8-bom, utf-16be or utf-16le to control the character set.
    charset := avalue
  else if aname = 'trim_trailing_whitespace' then
    // set to true to remove any whitespace characters preceding newline characters and false to ensure it doesn't.
    TryStrToTriBool(avalue,  trim_trailing_whitespace )
  else if aname = 'insert_final_newline' then
    // set to true to ensure file ends with a newline when saving and false to ensure it doesn't.
    TryStrToTriBool(avalue, insert_final_newline);

  if ei < 0 then begin
    if keyvalCount = length(keyval) then begin
      if keyvalCount =0 then setLength(keyval, 8)
      else SetLength(keyval, keyvalCount*2);
    end;
    keyval[keyvalCount].key := aname;
    keyval[keyvalCount].value := avalue;
    inc(keyvalCount);
  end else
    keyval[ei].value := avalue;
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

  { TECSet }

  TECSet = class(TObject)
  public
    Pattern : string;
    FullIdx : integer;
    FullLen : integer;

    isNum   : Boolean;
    min,max : Int64;
    Count   : integer;
    Strs    : array of TECStrings;
    procedure Parse(const p: string; var pidx: integer);
    function Match(const s: string; var sidx: Integer): Boolean;
    procedure AddWord(aidx, alen: integer);
  end;

  { TECContext }

  TECContext = class
  private
    st : array of TECSet;
    count: integer;
  public
    destructor Destroy; override;
    function GetSet(const p: string; var pidx: integer): TECSet;
  end;

function _ECMatch(ctx: TECContext; const p: string; pidx: integer; const s: string; sidx: integer): Boolean;
var
  pi, i, j : integer;
  stoppath : Boolean;
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

      '{': Result := ctx.GetSet(p, pi).Match(s,i);
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

procedure TECSet.Parse(const p: string; var pidx: integer);
var
  j: integer;
  k: integer;

  knownum: Boolean;
  n1,n2: Int64;
  pstart: Integer;
begin
  Pattern := p;
  FullIdx:=pidx;
  if (p[pidx] = '{') then inc(pidx);
  j:=pidx;
  knownum:=false;
  pstart:=pidx;

  while (pidx <= length(p)) and (p[pidx]<>'}') do begin

    if (p[pidx]=',') then begin
      AddWord(j, pidx-j);
      j:=pidx+1;
      knownum := true; // this is a list of words, and not numbers range
      inc(pidx);

    end else if (p[pidx]='.') and not knownum and (pidx<length(p)) and (p[pidx+1]='.') then begin
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
        end;
      end;
      if not isNum then inc(pidx);
    end else
      inc(pidx);
  end;

  if not isNum then AddWord(j, pidx-j);

  inc(pidx);
  FullLen := pidx - pstart;
end;

function TECSet.Match(const s: string; var sidx: Integer): Boolean;
var
  i,j : integer;
  leftlen : integer;
  v: Int64;
  err: integer;
begin
  if not isNum then begin
    leftlen := length(s) - sidx + 1;
    Result := false;
    for i:=0 to Count-1 do
      if Strs[i].len<=leftlen then begin
        if CompareChar( s[sidx], Pattern[strs[i].offset], Strs[i].len)=0 then begin
          Result := true;
          inc(sidx, Strs[i].len);
          Exit;
        end;
      end;
  end else begin
    Result := sidx<=length(s);
    if not Result then Exit;
    i:=sidx;
    j:=i;
    if (s[i] in ['+','-']) then inc(i);
    while (i<=length(s)) and (s[i] in ['0'..'9']) do inc(i);
    Val(Copy(s, j, i-j), v, err);
    Result := (err = 0) and (v>=min) and (v<=max);
    if Result then sidx := i;
  end;
end;

procedure TECSet.AddWord(aidx, alen: integer);
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

function TECContext.GetSet(const p: string; var pidx: integer): TECSet;
var
  i : integer;
begin
  for i:=0 to count-1 do
    if st[i].FullIdx=pidx then begin
      Result := st[i];
      pidx := pidx+st[i].FullLen;
      Exit;
    end;

  if count=length(st) then begin
    if count=0 then SetLength(st,4)
    else SetLength(st, count*2);
  end;
  Result := TECSet.Create;
  Result.Parse(p, pidx);
  st[count]:=Result;
  inc(count);
end;

end.

