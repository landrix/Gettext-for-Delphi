@echo off

Rem    Set user path (you must modify)
set SourceDir=D:\svn\dxGetText\dxgettext
set InnoDir=C:\Program Files (x86)\Inno Setup 5

Rem    additional Path, do not change
set TargetDir=%SourceDir%\..\dxgettext-packages\dxgettext\dxgettext

pushd "%SourceDir%"

Rem    Call Batch "rsvars" to set variables (Delphi)
call rsvars

Rem    Compile the "dxGetText"-project
call "msbuild.exe" %SourceDir%\_dxGetText.groupproj /target:Build /p:config=Release

popd

Rem    Copy the new Files into the release folder
copy "%SourceDir%\output\release\dxgettext.exe"              "%TargetDir%\"
copy "%SourceDir%\output\release\ggdxgettext.exe"            "%TargetDir%\"
copy "%SourceDir%\output\release\tools\assemble.exe"         "%TargetDir%\"
copy "%SourceDir%\output\release\tools\dxgreg.exe"           "%TargetDir%\"
copy "%SourceDir%\output\release\tools\ggassemble.exe"       "%TargetDir%\"
copy "%SourceDir%\output\release\tools\ggfmt.exe"            "%TargetDir%\"
copy "%SourceDir%\output\release\tools\ggmerge.exe"          "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgimport.exe"        "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgmergedx.exe"       "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgmergePOT.exe"      "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgmkignore.exe"      "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgremove.exe"        "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgshowheader.exe"    "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgsplitTStrings.exe" "%TargetDir%\"
copy "%SourceDir%\output\release\tools\msgstripCR.exe"       "%TargetDir%\"
copy "%SourceDir%\output\release\tools\ResStringsToPo.exe"   "%TargetDir%\"
copy "%SourceDir%\output\release\tools\stripifdef.exe"       "%TargetDir%\"
copy "%SourceDir%\output\release\tools\translation.exe"      "%TargetDir%\"
copy "%SourceDir%\output\release\gorm\Gorm.exe"              "%TargetDir%\"

pause

Rem    Copy additional files
copy %SourceDir%\sample\gnugettext.pas dxgettext\gnugettext.pas
copy dxgettext\gnugettext.pas dxgettext\example\gnugettext.pas

copy %SourceDir%\languages\languagecodes.pas dxgettext\
copy %SourceDir%\languages\languagecodes.po dxgettext\
copy %SourceDir%\languages\languagecodes.txt dxgettext\

"%InnoDir%\iscc.exe" setup.iss

pause