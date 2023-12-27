set dxgettext=dxgettext.exe --delphi --ignore-constreplace -q -o:msgid --no-wrap -o .
set msgmergePOT=msgmergePOT.exe

set src=%ProgramFiles(x86)%\embarcadero\RAD Studio\8.0\source\

%dxgettext% -b "%src%" --delphi @DirList_org.lst 
move default.po english.po
pause

%dxgettext% --nonascii -b "%src%" --delphi @DirList_de.lst 
move default.po german.po
pause

%msgmergePOT% english.po german.po combined-en-de.po
%msgmergePOT% german.po english.po combined-de-en.po
pause
