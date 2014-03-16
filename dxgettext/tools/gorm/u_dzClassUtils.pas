unit u_dzClassUtils;

interface

uses
  Windows;

///<summary>
/// Tries to read a string from the registry
/// @param Path is the full path, including the value name of the string
/// @param Value contains the string that was read, only valid if Result = true
/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
/// @returns true, if the value specified did exist, falso if it did not exist
/// @raises any exception that TRegistry raises if something goes wrong reading the value,
///         e.g. the value exists, but is not a string </summary>
function TRegistry_TryReadString(const _Path: string; out _Value: string;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean;

///<summary>
/// Writes a string to the registry
/// @param Path is the full path, including the value name of the string
/// @param Value contains the string to write
/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
/// @raises any exception that TRegistry raises if something goes wrong writing the value,
///         e.g. the cannot be opened or the value exists, but is not a string </summary>
procedure TRegistry_WriteString(const _Path: string; const _Value: string;
  _HKEY: HKEY = HKEY_CURRENT_USER);

implementation

uses
  SysUtils,
  Registry,
  gnugettext;

function TRegistry_TryReadString(const _Path: string; out _Value: string;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean;
var
  Key: string;
  Item: string;
  Reg: TRegistry;
begin
  Result := False;

  Key := ExtractFileDir(_Path);
  Item := ExtractFileName(_Path);

  Reg := TRegistry.Create;
  try
    Reg.RootKey := _HKEY;
    if Reg.OpenKeyReadOnly(Key) then begin
      try
        Result := Reg.ValueExists(Item);
        if not Result then
          Exit;
        _Value := Reg.ReadString(Item)
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

procedure TRegistry_WriteString(const _Path: string; const _Value: string;
  _HKEY: HKEY = HKEY_CURRENT_USER);
var
  Key: string;
  Item: string;
  Reg: TRegistry;
begin
  Key := ExtractFileDir(_Path);
  Item := ExtractFileName(_Path);

  Reg := TRegistry.Create;
  try
    Reg.RootKey := _HKEY;
    if not Reg.OpenKey(Key, True) then
      raise Exception.CreateFmt(_('Could not open Registry key for writing: %s'), [Key]);
    try
      Reg.WriteString(Item, _Value);
    finally
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

end.
