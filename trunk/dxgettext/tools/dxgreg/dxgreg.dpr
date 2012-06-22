program dxgreg;

uses
  gnugettext in '..\..\sample\gnugettext.pas',
  Windows,
  Registry,
  SysUtils,
  Classes,
  appconsts in '..\..\dxgettext\appconsts.pas';

{$R dxgreg.res}
{$R icons.res}

var
  apppath:string;

function RegOpenC (reg:TRegistry;subkey:string):boolean;
begin
  if reg.CurrentPath<>'' then
    reg.CloseKey;
  Result:=reg.OpenKey(subkey,false);
end;

procedure ResetUser;
var
  reg:TRegistry;
  s:string;
begin
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CURRENT_USER;
    s:='Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.po';
    if reg.KeyExists(s) then
      reg.DeleteKey(s);
    s:='Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.mo';
    if reg.KeyExists(s) then
      reg.DeleteKey(s);
  finally
    FreeAndNil (reg);
  end;
end;

procedure UnInstallFileType (typename:string);
var
  reg:TRegistry;
  keynames:TStringList;
  i:integer;
  s:string;
begin
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CLASSES_ROOT;
    if RegOpenC (reg,typename+'\Shell') then begin
      keynames:=TStringList.Create;
      try
        reg.GetKeyNames(keynames);
        for i:=0 to keynames.Count-1 do begin
          s:=typename+'\Shell\'+keynames.Strings[i]+'\Command';
          if RegOpenC(reg,s) then begin
            s:=reg.ReadString('');
            s:=uppercase(copy(s,2,length(apppath)));
            if s=uppercase(apppath) then begin
              reg.CloseKey;
              s:=typename+'\Shell\'+keynames.Strings[i];
              reg.DeleteKey(s);
            end;
          end;
        end;
      finally
        FreeAndNil (keynames);
      end;
    end;
    if reg.CurrentPath<>'' then
      reg.CloseKey;
  finally
    FreeAndNil (reg);
  end;
end;

procedure UnInstallExt (ext:string);
var
  reg:TRegistry;
  typename:string;
begin
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CLASSES_ROOT;
    if RegOpenC (reg,ext) then begin
      typename:=reg.ReadString('');
      if typename<>'' then
        UnInstallFileType (typename);
    end;
    if reg.CurrentPath<>'' then
      reg.CloseKey;
  finally
    FreeAndNil (reg);
  end;
end;

procedure UnInstall;
var
  reg:TRegistry;
begin
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CLASSES_ROOT;

    UnInstallExt ('.po');
    UnInstallExt ('.mo');
    UnInstallExt ('.exe');
    UnInstallExt ('.dll');
    UnInstallExt ('.ocx');
    UnInstallFileType ('Folder');
  finally
    FreeAndNil (reg);
  end;
end;

procedure RegOpen (reg:TRegistry;subkey:string);
begin
  if reg.CurrentPath<>'' then
    reg.CloseKey;
  if not reg.OpenKey(subkey,true) then
    raise Exception.Create(Format(_('Could not open or create subkey %s.'),['HKEY_CLASSES_ROOT\'+subkey]));
end;

procedure Install;
var
  typename:string;
  reg:TRegistry;
begin
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CLASSES_ROOT;

    // PO files
    RegOpen (reg,'.po');
    typename:=reg.ReadString('');
    if typename='' then begin
      typename:='PO file';
      reg.WriteString('',typename);
    end;
    RegOpen (reg,typename);
    reg.WriteString('','PO file');
    RegOpen (reg,typename+'\DefaultIcon');
    reg.WriteString('',paramstr(0)+',2');
    RegOpen (reg,typename+'\Shell\Merge template\Command');
    reg.WriteString('','"'+apppath+'ggmerge.exe" "%1"');
    RegOpen (reg,typename+'\Shell\Compile to mo file\Command');
    reg.WriteString('','"'+apppath+'ggfmt.exe" msgfmt.exe "%1" .mo');

    // Compiled translations
    RegOpen (reg,'.mo');
    typename:=reg.ReadString('');
    if typename='' then begin
      typename:='MO file';
      reg.WriteString('',typename);
    end;
    RegOpen (reg,typename);
    reg.WriteString('','MO file');
    RegOpen (reg,typename+'\DefaultIcon');
    reg.WriteString('',paramstr(0)+',1');
    RegOpen (reg,typename+'\Shell\Decompile to po file\Command');
    reg.WriteString('','"'+apppath+'ggfmt.exe" msgunfmt.exe "%1" .po');

    // Program files
    RegOpen (reg,'.exe');
    typename:=reg.ReadString('');
    if typename='' then
      raise Exception.Create (_('This Windows installation is corrupt - .exe extension is not associated with anything.'));
    RegOpen (reg,typename+'\Shell\Embed translations\Command');
    reg.WriteString('','"'+apppath+'ggassemble.exe" "%1"');
    RegOpen (reg,typename+'\Shell\Extract strings\Command');
    reg.WriteString('','"'+apppath+'ggdxgettext.exe" "%1"');

    // Dynamic Link Libraries
    RegOpen (reg,'.dll');
    typename:=reg.ReadString('');
    if typename='' then begin
      typename:='dllfile';
      reg.WriteString('',typename);
    end;
    RegOpen (reg,typename);
    reg.WriteString('',_('Dynamic Link Library'));
    RegOpen (reg,typename+'\Shell\Embed translations\Command');
    reg.WriteString('','"'+apppath+'ggassemble.exe" "%1"');
    RegOpen (reg,typename+'\Shell\Extract strings\Command');
    reg.WriteString('','"'+apppath+'ggdxgettext.exe" "%1"');

    // OCX files
    RegOpen (reg,'.ocx');
    typename:=reg.ReadString('');
    if typename='' then begin
      typename:='ocxfile';
      reg.WriteString('',typename);
    end;
    RegOpen (reg,typename);
    reg.WriteString('',_('OCX Dynamic Link Library'));
    RegOpen (reg,typename+'\Shell\Embed translations\Command');
    reg.WriteString('','"'+apppath+'ggassemble.exe" "%1"');
    RegOpen (reg,typename+'\Shell\Extract strings\Command');
    reg.WriteString('','"'+apppath+'ggdxgettext.exe" "%1"');

    // Borland Package Libraries
    RegOpen (reg,'.bpl');
    typename:=reg.ReadString('');
    if typename='' then begin
      typename:='bplfile';
      reg.WriteString('',typename);
    end;
    RegOpen (reg,typename);
    reg.WriteString('',_('Borland Package Library'));
    RegOpen (reg,typename+'\Shell\Embed translations\Command');
    reg.WriteString('','"'+apppath+'ggassemble.exe" "%1"');
    RegOpen (reg,typename+'\Shell\Extract strings\Command');
    reg.WriteString('','"'+apppath+'ggdxgettext.exe" "%1"');

    // File folder
    RegOpen (reg,'Folder\shell\Extract translations to template\Command');
    reg.WriteString('','"'+apppath+'ggdxgettext.exe" "%1"');
    reg.CloseKey;
  finally
    FreeAndNil (reg);
  end;
end;

begin
  TextDomain ('dxgettext');
  AddDomainForResourceString('delphi');
  apppath:=extractfilepath(paramstr(0));

  if (paramcount=1) and (paramstr(1)='--reset-user') then begin
    ResetUser;
  end else begin
    UnInstall;
    if (ParamCount<>1) or (paramstr(1)<>'--uninstall') then begin
      Install;
      MessageBoxW(0,PWideChar(_('The applications have now been integrated into your desktop.')),
                    PWideChar(Format(_('GNU gettext %s'),[version])),MB_OK);
    end;
  end;
end.
