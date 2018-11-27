unit editorConfigReg;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Forms, Classes, SysUtils, Controls, SynEdit
  //,IDEOptionsIntf
  //, IDEWindowIntf, ProjectIntf
  ,LazIDEIntf, SrcEditorIntf
  ,editorConfigTypes, editorConfigUtils
  ,LConvEncoding
  ,CodeCache // for TCodeBuffer
  ;

procedure Register;

implementation

type

  { TECWatch }

  TECWatch = class(TObject)
    procedure OnEditorActive(Sender: TObject);
    procedure ApplyEditorConfig(ed : TSourceEditorInterface);
    procedure ApplyToSynEdit(const ec: TLookupResult; ed: TSynEdit);
    procedure ApplyToCodeBuffer(const ec: TLookupResult; buf: TCodeBuffer);
  end;

var
  ecw : TECWatch = nil;

procedure Register;
begin
  ecw := TECWatch.Create;
  if Assigned(SourceEditorManagerIntf) then
    SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, ecw.OnEditorActive);
end;

{ TECWatch }

procedure TECWatch.ApplyEditorConfig(ed : TSourceEditorInterface);
var
  i  : integer;
  ctrl : TWinControl;
  cbuf : TObject;

  rs : TLookupResult;
begin
  ctrl := ed.EditorControl;
  cbuf := ed.CodeToolsBuffer;

  if not (ctrl is TSynEdit) then Exit;

  if LookupEditorConfig(ed.FileName, rs) then begin
    ApplyToSynEdit(rs, TSynEdit(ctrl));
    if cbuf is TCodeBuffer then
      ApplyToCodeBuffer(rs, TCodeBuffer(cbuf));
  end;
end;

procedure TECWatch.ApplyToSynEdit(const ec: TLookupResult; ed: TSynEdit);
var
  w: integer;
begin
  w:=GetTabWidth(ec);
  if w>0 then ed.TabWidth:=w;

  if isTabIndent(ec.indent_style) then
    ed.Options:=ed.Options - [eoTabsToSpaces]
  else if isSpaceIndent(ec.indent_style) then
    ed.Options:=ed.Options + [eoTabsToSpaces];

  if ec.trim_trailing_whitespace = tbTrue then
    ed.Options := ed.Options + [eoTrimTrailingSpaces]
  else if ec.trim_trailing_whitespace = tbFalse then
    ed.Options := ed.Options - [eoTrimTrailingSpaces];
end;

procedure TECWatch.ApplyToCodeBuffer(const ec: TLookupResult; buf: TCodeBuffer);
var
  l : string;
begin
  if ec.end_of_line<>'' then begin
    l := LowerCase(ec.end_of_line);
    if l = 'lf' then  buf.DiskLineEnding := #10
    else if l = 'cr' then buf.DiskLineEnding := #13
    else if l = 'crlf' then buf.DiskLineEnding := #13#10;
  end;

  if ec.charset<>'' then begin
    l := LowerCase(ec.charset);
    if l = 'latin1' then buf.DiskEncoding := EncodingCP1250
    else if l = 'utf-8' then buf.DiskEncoding := EncodingUTF8
    else if l = 'utf-8-bom' then buf.DiskEncoding := EncodingUTF8BOM
    else if l = 'utf-16be' then buf.DiskEncoding := EncodingUCS2BE
    else if l = 'utf-16le' then buf.DiskEncoding := EncodingUCS2LE
  end;

end;

procedure TECWatch.OnEditorActive(Sender: TObject);
var
  ed : TSourceEditorInterface;
begin
  try
    if not Assigned(SourceEditorManagerIntf) then Exit;
    ed := SourceEditorManagerIntf.ActiveEditor;
    if Assigned(ed) then
      ApplyEditorConfig(ed);
  except
    // paranoid error catching!!!!! SHOULD NOT PREVENT user from editting the page
  end;
end;

finalization
  ecw.Free;

end.
