{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Peter Thornqvist [peter3@peter3.com]
Portions created by XX are Copyright (C) 2003 Peter Thornqvist. All Rights Reserved.


You may retrieve the latest version of this file at dxgettext's home page,
located at http://dybdahl.dk/dxgettext/

-----------------------------------------------------------------------------}

program dfntopo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  dfntopoUtils in 'dfntopoUtils.pas';
var aExitCode:integer;
begin
  aExitCode := Run;
  if aExitCode <> DFN_ERR_SUCCESS then
    Halt(aExitCode);
end.

