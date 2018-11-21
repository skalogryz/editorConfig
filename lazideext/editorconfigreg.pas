unit editorConfigReg;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Forms, Classes, SysUtils, Controls, SynEdit
  //,IDEOptionsIntf
  //, IDEWindowIntf, ProjectIntf
  ,LazIDEIntf, SrcEditorIntf
  ,editorConfigTypes, edConfGlob, editorConfigUtils
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
    procedure ApplyToSynEdit(ec: TEditorEntry; ed: TSynEdit);
    procedure ApplyToCodeBuffer(ec: TEditorEntry; buf: TCodeBuffer);
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

  ecfn : string;
  ec: TEditorConfigFile;
  pth  : string;
  match : string;
begin
  ctrl := ed.EditorControl;
  cbuf := ed.CodeToolsBuffer;

  if not (ctrl is TSynEdit) then Exit;

  pth := ExtractFilePath(ed.FileName);
  ecfn := pth + '.editorconfig';

  // todo: traverse parent directories until "root" is found
  if not FileExists(ecfn) then Exit;

  ec:=TEditorConfigFile.Create;
  try
    ReadFromFile(ec, ecfn);
    match := Copy(ed.Filename, length(pth)+1, length(ed.FileName));
    for i:=0 to ec.Count-1 do begin
      if ECMatch( ec[i].name, match) then begin
        ApplyToSynEdit( ec[i], TSynEdit(ctrl));
        if cbuf is TCodeBuffer then
          ApplyToCodeBuffer(ec[i], TCodeBuffer(cbuf));
        Break;
      end;
    end;
  finally
    ec.Free;
  end;
end;

procedure TECWatch.ApplyToSynEdit(ec: TEditorEntry; ed: TSynEdit);
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

procedure TECWatch.ApplyToCodeBuffer(ec: TEditorEntry; buf: TCodeBuffer);
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
