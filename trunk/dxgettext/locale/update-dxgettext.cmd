dxgettext -b ..\tools -b ..\dxgettext --delphi -o . -r
msgremove default.po -i ignore.po -o ..\..\translations\templates\dxgettext.po
del default.po
