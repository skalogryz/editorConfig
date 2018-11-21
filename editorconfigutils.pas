unit editorConfigUtils;

interface

uses
  editorConfigTypes;

function GetTabWidth(ec: TEditorEntry; const defVal: integer = -1): integer;
function isTabIndent(const indent_style: string): Boolean;
function isSpaceIndent(const indent_style: string): Boolean;

implementation

function GetTabWidth(ec: TEditorEntry; const defVal: integer = -1): integer;
begin
  if not Assigned(ec) then Result := defVal
  else if ec.tab_width>=0 then Result := ec.tab_width
  else if ec.indent_size>=0 then Result := ec.indent_size
  else Result :=defVal;
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


end.
