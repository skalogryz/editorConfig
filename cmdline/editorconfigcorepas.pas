unit EditorConfigCorePas;

{$mode delphi}

interface

uses
  Classes, SysUtils, EditorConfigTypes, EditorConfigUtils;

type

  { TEditorConfigHandle }

  TEditorConfigHandle = class(TObject)
  private
  public
    ConfigFile : String;
    lastFound  : TEditorConfigEntry;
    targetVer  : TEditorConfigVersion;
    constructor Create;
    procedure SetVersion(const aversion: string);
    function Parse(const fullname: string): integer;
  end;

const
  // editorconfig_parse() return value: the full_filename parameter of
  // editorconfig_parse() is not a full path name
  EDITORCONFIG_PARSE_NOT_FULL_PATH = -2;

  // editorconfig_parse() return value: a memory error occurs.
  EDITORCONFIG_PARSE_MEMORY_ERROR  = -3;

  // editorconfig_parse() return value: the required version specified in @ref
  // editorconfig_handle is greater than the current version.
  EDITORCONFIG_PARSE_VERSION_TOO_NEW = -4;

{*!
 * @brief Parse editorconfig files corresponding to the file path given by
 * full_filename, and related information is input and output in h.
 *
 * An example is available at
 * <a href=https://github.com/editorconfig/editorconfig-core/blob/master/src/bin/main.c>src/bin/main.c</a>
 * in EditorConfig C Core source code.
 *
 * @param full_filename The full path of a file that is edited by the editor
 * for which the parsing result is.
 *
 * @param h The @ref editorconfig_handle to be used and returned from this
 * function (including the parsing result). The @ref editorconfig_handle should
 * be created by editorconfig_handle_init().
 *
 * @retval 0 Everything is OK.
 *
 * @retval "Positive Integer" A parsing error occurs. The return value would be
 * the line number of parsing error. err_file obtained from h by calling
 * editorconfig_handle_get_err_file() will also be filled with the file path
 * that caused the parsing error.
 *
 * @retval "Negative Integer" Some error occured. See below for the reason of
 * the error for each return value.
 *
 * @retval EDITORCONFIG_PARSE_NOT_FULL_PATH The full_filename is not a full
 * path name.
 *
 * @retval EDITORCONFIG_PARSE_MEMORY_ERROR A memory error occurs.
 *
 * @retval EDITORCONFIG_PARSE_VERSION_TOO_NEW The required version specified in
 * @ref editorconfig_handle is greater than the current version.
 *
 *}
function editorconfig_parse(const full_filename: string; h: TEditorConfigHandle): Integer;

{*!
 * @brief Get the conf_file_name field of an editorconfig_handle object.
 *
 * @param h The editorconfig_handle object whose conf_file_name field needs to
 * be obtained.
 *
 * @return The value of the conf_file_name field of the editorconfig_handle
 * object.
 *}
function editorconfig_handle_get_conf_file_name(h: TEditorConfigHandle): string;

{*!
 * @brief Get the count of name and value fields of an editorconfig_handle
 * object.
 *
 * @param h The editorconfig_handle object whose count of name and value fields
 * need to be obtained.
 *
 * @return the count of name and value fields of the editorconfig_handle
 * object.
 *}
function editorconfig_handle_get_name_value_count(h: TEditorConfigHandle): Integer;

{*!
 * @brief Get the nth name and value fields of an editorconfig_handle object.
 *
 * @param h The editorconfig_handle object whose name and value fields need to
 * be obtained.
 *
 * @param n The zero-based index of the name and value fields to be obtained.
 *
 * @param name If not null, *name will be set to point to the obtained name.
 *
 * @param value If not null, *value will be set to point to the obtained value.
 *
 * @return None.
 *}
procedure editorconfig_handle_get_name_value(h: TEditorConfigHandle;
 idx: integer; var name, value: string);

{*!
 * @brief Get the error message from the error number returned by
 * editorconfig_parse().
 *
 * An example is available at
 * <a href=https://github.com/editorconfig/editorconfig-core/blob/master/src/bin/main.c>src/bin/main.c</a>
 * in EditorConfig C Core source code.
 *
 * @param err_num The error number that is used to obtain the error message.
 *
 * @return The error message corresponding to err_num.
 *}
function editorconfig_get_error_msg(err_num: integer): string;

{*!
 * @brief Set the conf_file_name field of an editorconfig_handle object.
 *
 * @param h The editorconfig_handle object whose conf_file_name field needs to
 * be set.
 *
 * @param conf_file_name The new value of the conf_file_name field of the
 * editorconfig_handle object.
 *
 * @return None.
 *}
procedure editorconfig_handle_set_conf_file_name(h: TEditorConfigHandle;
  const config_file: string);

{*!
 * @brief Set the version fields of an editorconfig_handle object.
 *
 * @param h The editorconfig_handle object whose version fields need to be set.
 *
 * @param major If not less than 0, the major version field will be set to
 * major. If this parameter is less than 0, the major version field of the
 * editorconfig_handle object will remain unchanged.
 *
 * @param minor If not less than 0, the minor version field will be set to
 * minor. If this parameter is less than 0, the minor version field of the
 * editorconfig_handle object will remain unchanged.
 *
 * @param patch If not less than 0, the patch version field will be set to
 * patch. If this parameter is less than 0, the patch version field of the
 * editorconfig_handle object will remain unchanged.
 *
 * @return None.
 *}
procedure editorconfig_handle_set_version(h: TEditorConfigHandle;
  const aversion: string);

implementation

{ TEditorConfigHandle }

constructor TEditorConfigHandle.Create;
begin
  inherited Create;
  targetVer := DefaultVersion;
end;

procedure TEditorConfigHandle.SetVersion(const aversion: string);
begin
  StrToVersion(aversion, targetVer);
end;

function TEditorConfigHandle.Parse(const fullname: string): integer;
begin
  Result := 0;
  if Assigned(lastFound) then lastFound.Free;
  lastFound := nil;

  // making full-name relative

  if not LookupEditorConfig(fullname, lastFound, false, ConfigFile) then begin
    lastFound.Free;
    lastfound := nil;
  end;

  if Assigned(lastFound) then
    SetDefaultProps(lastFound, targetVer);
end;

function editorconfig_parse(const full_filename: string; h: TEditorConfigHandle): Integer;
begin
  Result := h.Parse(full_filename);
end;

function editorconfig_handle_get_conf_file_name(h: TEditorConfigHandle): string;
begin
  if not Assigned(h) then Result := '' else Result := h.ConfigFile;
end;

function editorconfig_handle_get_name_value_count(h: TEditorConfigHandle): Integer;
begin
  if not Assigned(h) or not Assigned(h.lastFound) then Result := 0
  else Result := h.lastFound.keyvalCount;
end;

procedure editorconfig_handle_get_name_value(h: TEditorConfigHandle;
  idx: integer; var name, value: string);
begin
  if not Assigned(h) or (not Assigned(h.lastFound)) or (idx<0) or (idx>=h.lastFound.keyvalCount) then begin
    name := '';
    value := '';
  end else begin
    name := h.lastFound.keyval[idx].key;
    value := h.lastFound.keyval[idx].value;
  end;
end;

function editorconfig_get_error_msg(err_num: integer): string;
begin
  if (err_num > 0) then begin
    Result := 'Failed to parse file.';
    Exit;
  end;

  case err_num of
    0: Result := 'No error occurred.';
    EDITORCONFIG_PARSE_NOT_FULL_PATH:
      Result := 'Input file must be a full path name.';
    EDITORCONFIG_PARSE_MEMORY_ERROR:
      Result := 'Memory error.';
    EDITORCONFIG_PARSE_VERSION_TOO_NEW:
      Result := 'Required version is greater than the current version.';
  else
    Result := 'Unknown error.';
  end;
end;

procedure editorconfig_handle_set_conf_file_name(h: TEditorConfigHandle;
  const config_file: string);
begin
  if Assigned(h) then h.ConfigFile := config_file;
end;

procedure editorconfig_handle_set_version(h: TEditorConfigHandle;
  const aversion: string);
begin
  if Assigned(h) then h.SetVersion(aversion);
end;

end.

