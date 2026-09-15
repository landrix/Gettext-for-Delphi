# Gettext-for-Delphi

GNU GetText translation tools for Borland Delphi and Borland C++ Builder

2026-09-16 Added support in dxgettext for Delphi12+ triple-quoted multi-line string format (in .pas files)

2026-09-15 Fixed bug in consoleoutput: no output text is generated when output is redirected or piped

2026-09-09 Sync (import) changes from trunk r196 https://sourceforge.net/p/dxgettext/code/196/log/?path=/trunk

2026-09-09 Added support for multi-line text (Lines.Strings) as one translation item, as opposed to Items.Strings, which are translated as separate items

	Note: 
	- Delphi 6 and up : Use gnugettext.pas version in \dxgettext\sample\    
	- Delphi 5 version of gnugettext.pas is not updated               
	- Freepascal version of gnugettext.pas is not updated            

2025-04-01 Sync with trunk r182 https://sourceforge.net/p/dxgettext/code/182/log/?path=/trunk

2023-04-20 Fork from https://sourceforge.net/projects/dxgettext/