rem Use this for Borland Delphi 2005 Architect

del ibx9.po
del indy9.po
del indy10.po
del delphi2005win32core.po
del delphi2005win32.po
del delphi2005.po

set src=c:\progra~1\borland\bds\3.0\source

dxgettext --ignore-constreplace -r -q *.pas *.inc *.rc *.dfm -b %src%\win32\ibx -o .
ren default.po ibx9.po
dxgettext --ignore-constreplace -r -q *.pas *.inc *.rc *.dfm -b %src%\win32\indy9 -o .
ren default.po indy9.po
dxgettext --ignore-constreplace -r -q *.pas *.inc *.rc *.dfm -b %src%\Indy10 -o .
ren default.po indy10.po

dxgettext --ignore-constreplace -r -q *.pas *.inc *.rc *.dfm -b %src%\win32\db -b %src%\win32\internet -b %src%\win32\rtl -b %src%\win32\soap -b %src%\win32\vcl -b %src%\win32\xml -b %src%\win32\samples -o .
ren default.po delphi2005win32core.po
dxgettext --ignore-constreplace -r -q *.pas *.inc *.rc *.dfm -b %src%\win32\db -b %src%\win32\Decisi~1 -b %src%\win32\internet -b %src%\win32\rtl -b %src%\win32\soap -b %src%\win32\vcl -b %src%\win32\websnap -b %src%\win32\xml -b %src%\win32\samples -b %src%\indy10 -b %src%\intraweb -o .
ren default.po delphi2005win32.po
dxgettext --ignore-constreplace -r -q *.pas *.inc *.rc *.dfm *.xfm -b %src% -o .
ren default.po delphi2005.po

