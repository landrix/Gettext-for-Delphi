; To compile this script, get My Inno Setup Extensions at http://www.wintax.nl/isx/

#include "ModifyPath.iss"

[Setup]
AppName=GNU Gettext for Delphi and C++ Builder
AppVerName=GNU Gettext for Delphi and C++ Builder 1.3.0
AppPublisherURL=http://dybdahl.dk/dxgettext/
AppSupportURL=http://dybdahl.dk/dxgettext/
AppUpdatesURL=http://dybdahl.dk/dxgettext/
DefaultDirName={pf}\dxgettext
DefaultGroupName=GNU Gettext for Delphi and C++ Builder
AllowNoIcons=true
AppCopyright=This software is given under several licenses, please see license.txt
OutputBaseFilename=dxgettext
Compression=bzip

[Files]
Source: dxgettext\*; DestDir: {app}; Flags: ignoreversion
Source: dxgettext\delphi5\*; DestDir: {app}\delphi5; Flags: ignoreversion
Source: dxgettext\docs\*; DestDir: {app}\docs; Flags: ignoreversion
Source: dxgettext\docs\html\*; DestDir: {app}\docs\html; Flags: ignoreversion
Source: dxgettext\example\*; DestDir: {app}\example; Flags: ignoreversion
Source: dxgettext\example\cppbuilder\*; DestDir: {app}\example\cppbuilder; Flags: ignoreversion
Source: dxgettext\example\ngettext\*; DestDir: {app}\example\ngettext; Flags: ignoreversion
Source: dxgettext\example\ngettext\locale\de\LC_MESSAGES\*; DestDir: {app}\example\ngettext\locale\de\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\ngettext\locale\fr\LC_MESSAGES\*; DestDir: {app}\example\ngettext\locale\fr\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\ngettext\locale\pl\LC_MESSAGES\*; DestDir: {app}\example\ngettext\locale\pl\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\da_DK\LC_MESSAGES\*; DestDir: {app}\example\locale\da_DK\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\de\LC_MESSAGES\*; DestDir: {app}\example\locale\de\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\et\LC_MESSAGES\*; DestDir: {app}\example\locale\et\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\fi\LC_MESSAGES\*; DestDir: {app}\example\locale\fi\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\fr\LC_MESSAGES\*; DestDir: {app}\example\locale\fr\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\hr\LC_MESSAGES\*; DestDir: {app}\example\locale\hr\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\nl\LC_MESSAGES\*; DestDir: {app}\example\locale\nl\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\no\LC_MESSAGES\*; DestDir: {app}\example\locale\no\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\sv\LC_MESSAGES\*; DestDir: {app}\example\locale\sv\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\locale\ja\LC_MESSAGES\*; DestDir: {app}\example\locale\ja\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\example\TNTSample\*; DestDir: {app}\example\TNTSample; Flags: ignoreversion
Source: dxgettext\freepascal\*; DestDir: {app}\freepascal; Flags: ignoreversion
Source: dxgettext\locale\da\LC_MESSAGES\*; DestDir: {app}\locale\da\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\locale\de\LC_MESSAGES\*; DestDir: {app}\locale\de\LC_MESSAGES; Flags: ignoreversion
Source: dxgettext\media\earth.ico; DestDir: {app}\media; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[INI]
Filename: {app}\Homepage of GNU Gettext for Delphi and C++ Builder.url; Section: InternetShortcut; Key: URL; String: http://dybdahl.dk/dxgettext/
Filename: {app}\Documentation.url; Section: InternetShortcut; Key: URL; String: http://dybdahl.dk/dxgettext/docs/
Filename: {userdesktop}\GNU Gettext for Delphi and C++ Builder documentation.url; Section: InternetShortcut; Key: URL; String: http://dybdahl.dk/dxgettext/docs/; Tasks: DesktopIcons

[Icons]
Name: {group}\Documentation\C++ Builder notes; Filename: {app}\CPPBUILDER.txt
Name: {group}\Documentation\Changelog; Filename: {app}\CHANGELOG.txt
Name: {group}\Documentation\Homepage of GNU gettext for Delphi and C++ Builder; Filename: {app}\Homepage of GNU Gettext for Delphi and C++ Builder.url
Name: {group}\Documentation\License; Filename: {app}\LICENSE.txt
Name: {group}\Documentation\Manual (html); Filename: {app}\docs\html\index.html
Name: {group}\Documentation\Manual (pdf); Filename: {app}\docs\manual.pdf
Name: {group}\Documentation\Release notes; Filename: {app}\RELEASENOTES.txt
Name: {group}\GnuGettext.pas; Filename: {app}\gnugettext.pas
Name: {group}\Gorm; Filename: {app}\gorm.exe
Name: {group}\Ports\Delphi 5 files; Filename: {app}\delphi5\
Name: {group}\Ports\FreePascal files; Filename: {app}\freepascal\
Name: {group}\LanguageCodes\LanguageCodes.txt; Filename: {app}\LanguageCodes.txt
Name: {group}\LanguageCodes\LanguageCodes.pas; Filename: {app}\LanguageCodes.pas
Name: {group}\LanguageCodes\LanguageCodes.po; Filename: {app}\LanguageCodes.po
Name: {group}\Repair user interface integration system-wide; Filename: {app}\dxgreg.exe; IconFilename: {app}\media\earth.ico; IconIndex: 0
Name: {group}\Repair user interface integration for current user; Filename: {app}\dxgreg.exe; IconFilename: {app}\media\earth.ico; IconIndex: 0; Parameters: --reset-user
Name: {group}\Uninstall GNU Gettext for Delphi and C++ Builder; Filename: {uninstallexe}

[UninstallDelete]
Type: files; Name: {app}\ggassemble.url

[Run]
Filename: {app}\dxgreg.exe; WorkingDir: {app}; Description: Registering shell extensions; Tasks: DesktopIntegrate

[Tasks]
Name: DesktopIntegrate; Description: Integrate into desktop environment
Name: SetPath; Description: Modify path (required to make it work!); Flags: restart
Name: DesktopIcons; Description: Create icon on desktop

[Code]
procedure CurStepChanged(CurStep: TSetupStep);
var
  group:string;
begin
  case CurStep of
    ssPostInstall:
      begin
        group:=ExpandConstant('{group}\');
        DeleteFile (group+'Languagecodes.txt');
        DeleteFile (group+'Reset user shell extensions to system default.lnk');
        DeleteFile (group+'LanguageCodes.txt.lnk');
        DeleteFile (group+'LanguageCodes.po.lnk');
        DeleteFile (group+'LanguageCodes.pas.lnk');
        DeleteFile (group+'Homepage of GNU gettext for Delphi and C++ Builder.lnk');
        DeleteFile (group+'License.lnk');
        DeleteFile (group+'Delphi 5 files.lnk');
        DeleteFile (group+'Release notes.lnk');
        DeleteFile (group+'Documentation (PDF).lnk');
        DeleteFile (group+'Documentation.lnk');
        DeleteFile (group+'C++ Builder notes.lnk');
        DeleteFile (group+'Changelog.lnk');
        DeleteFile (group+'Register shell extensions.lnk');
        if IsTaskSelected('SetPath') then begin
          case ModifyPath('{app}', pmAddToBeginning + pmAddOnlyIfDirExists, psAllUsers) of
            mpMissingRights:      MsgBox('User has insufficent rights to modify path environment variable in registry.' + #13#10 + #13#10 +
                                  'Please log on as adminstrator and re-run this installation wizard', mbError, MB_OK);
            mpAutoexecNoWriteacc: MsgBox('Autoexec.bat can not be rewritten - possibly because it is write protected.', mbError, MB_OK);
          end;
        end;
      end;
  end;
end; // procedure CurStepChanged

[UninstallRun]
Filename: {app}\dxgreg.exe; Parameters: --uninstall; WorkingDir: {app}
