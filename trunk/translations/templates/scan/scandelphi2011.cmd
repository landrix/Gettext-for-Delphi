set dxgettext=dxgettext.exe --delphi --ignore-constreplace -q -o:msgid --no-wrap -o .
set msgmergePOT=msgmergePOT.exe

set src=%programfiles%\embarcadero\RAD Studio\8.0\source

%dxgettext% -b "%src%\database" -b "%src%\db" -b "%src%\dunit\src" -b "%src%\ibx" -b "%src%\ibx\property editors" -b "%src%\indy10\core" -b "%src%\indy10\protocols" -b "%src%\indy10\system" -b "%src%\internet" -b "%src%\property editors" -b "%src%\rtl\common" -b "%src%\rtl\sys" -b "%src%\samples\delphi" -b "%src%\soap" -b "%src%\soap\wsdimporter" -b "%src%\toolsapi" -b "%src%\vcl" -b "%src%\visualizers" -b "%src%\xml" -b "%src%\xtab"
move default.po english.po
pause

%dxgettext% --nonascii -b "%src%\database\de" -b "%src%\db\de" -b "%src%\dunit\src\de" -b "%src%\ibx\de" -b "%src%\ibx\property editors\de" -b "%src%\indy10\core\de" -b "%src%\indy10\protocols\de" -b "%src%\indy10\system\de" -b "%src%\internet\de" -b "%src%\property editors\de" -b "%src%\rtl\common\de" -b "%src%\rtl\sys\de" -b "%src%\samples\delphi\de" -b "%src%\soap\de" -b "%src%\soap\wsdimporter\de" -b "%src%\toolsapi\de" -b "%src%\vcl\de" -b "%src%\visualizers\de" -b "%src%\xml\de" -b "%src%\xtab\de"
move default.po german.po
pause

%msgmergePOT% english.po german.po combined.po
pause
