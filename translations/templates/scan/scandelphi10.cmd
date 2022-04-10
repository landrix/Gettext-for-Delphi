@rem use this for Delphi 10 Seattle
@echo off

echo getting path of Delphi installation
call doGetDelphiPath Xx10
echo DelphiPath: %DelphiPath%

set src=%DelphiPath%\source
echo source directory is: %src%

set dxgettext=..\..\..\buildtools\dxgettext.exe
set msgmergePOT=..\..\..\dxgettext\output\debug\tools\msgmergePOT.exe

if not exist %dxgettext% goto :gtnotfound
echo dxgettext.exe: %dxgettext%

if not exist %msgmergePOT% goto mmnotfound
echo msgmergePOT.exe: %msgmergePOT%

set dxgettextparams=--delphi --ignore-constreplace -q -o:msgid --no-wrap -o .

@rem These are the source directories of a Delphi Professional installation.
@rem But even these are probably not all of them.
@rem The more expensive SKUs have got some additional directories here
set srcdirslist=data data\ado data\cloud data\dbx data\dsnap data\firedac data\rest data\vclctrls
set srcdirslist=%srcdirslist% databinding\components,databinding\engine,databinding\graph
set srcdirslist=%srcdirslist% fmx
set srcdirslist=%srcdirslist% ibx "ibx\property editors"
set srcdirslist=%srcdirslist% indy10\core indy10\protocols indy10\system
set srcdirslist=%srcdirslist% internet,"property editors" "property editors\Indy10"
set srcdirslist=%srcdirslist% rtl\common rtl\net,rtl\osx,rtl\sys
set srcdirslist=%srcdirslist% soap soap\wsdimporter
set srcdirslist=%srcdirslist% tethering
set srcdirslist=%srcdirslist% toolsapi
set srcdirslist=%srcdirslist% vcl vcl\AppAnalytics
set srcdirslist=%srcdirslist% visualizers
set srcdirslist=%srcdirslist% xml

set endirs=
set dedirs=
set frdirs=
for %%X in (%srcdirslist%) do (
  @rem we must call a subroutine (or use EnableDelayedExpansion) for this to work
  call :addsubdir %%X
)
goto srcddone

:addsubdir
  set endirs=%endirs% -b %src%\%1
  set dedirs=%dedirs% -b %src%\%1\de
  set frdirs=%frdirs% -b %src%\%1\fr
goto :eof

:srcddone
echo endirs:
echo "%endirs%"
echo:

echo dedirs:
echo "%dedirs%"
echo:

echo frdirs:
echo "%frdirs%"
echo:

echo extracting strings from sources to default.po
%dxgettext% %dxgettextparams% %endirs%
echo moving default.po to english.po
move default.po english.po
pause

echo extracting strings from German translations to default.po
%dxgettext% --nonascii %dxgettextparams% %dedirs%
echo moving default.po to german.po
move default.po german.po
pause

echo extracting strings from French translations to default.po
%dxgettext% --nonascii %dxgettextparams% %frdirs%
echo moving default.po to french.po
move default.po french.po
pause

echo merging english.po with german.po to combined_de.po
%msgmergePOT% english.po german.po combined_de.po
pause

echo merging english.po with french.po to combined_fr.po
%msgmergePOT% english.po french.po combined_fr.po
pause
goto :eof

:gtnotfound
echo file not found: %dxgettext%
pause
goto :eof

:mmnotfound
echo file not found: %msgmergePOT%
pause
goto :eof
