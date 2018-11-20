unit edConfGlob;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

// usual file mask as described: https://en.wikipedia.org/wiki/Glob_(programming)
function FileNameMatch(const pat, s: string): Boolean;

// editorConfig match, as desribed: https://editorconfig.org/#supported-properties
// todo: handle characters escape
function ECMatch(const pat, s: string): Boolean;

implementation

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

function TryStrToInt(const s: string; out v: Int64): Boolean;
var
  err : integer;
begin
  Result := (s<>'')
    and (s[1] in ['-','+','0'..'9'])
    and ((length(s)>1) or (s[1] in ['0'..'9']));
  if not Result then Exit;
  Val(s, v, err);
  Result := err = 0;
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
      '[': Result := _ParseSet(p, pi, s, i);
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
