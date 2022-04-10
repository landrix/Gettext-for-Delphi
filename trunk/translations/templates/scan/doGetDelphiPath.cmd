@rem set the DelphiPath variable for various Delphi versions
@rem the Delphi version must be passed as parameter in one of
@rem forms given as labels below.
@rem They follow the form:
@rem * [number] for Delphi 6 to 2010
@rem * XE[number] for Delphi XE1 to XE8
@rem * Xx[number] for Delphi 10
@rem * Xx[number+decimal] for Delphi 10.1 (Xx101) to 10.4 (Xx104)
@rem * Xx[number] for Delphi 11
@rem There are some alternative forms, but since these are even more inconsistent,
@rem their use is discouraged.

setlocal

set DelphiVersion=%1

rem Support for Windows 7/8, 64 Bit
set ProgFiles=%ProgramFiles(x86)%
set gx_windows_type=win64
if not "%ProgFiles%"=="" goto Win64Bit
set ProgFiles=%ProgramFiles%
set gx_windows_type=win32
:Win64Bit

set DelphiPath=

rem this is equivalent to a case/switch statement
rem call :Delphi%DelphiVersion% resolves into call :Delphi6 etc.
call :Delphi%DelphiVersion%
goto DelphiEndCase
:Delphi6
  call :ReadReg Borland\Delphi\6.0
  goto :DelphiEndCase
:Delphi7
  call :ReadReg Borland\Delphi\7.0
  goto :DelphiEndCase
:Delphi2005
  call :ReadReg Borland\BDS\3.0
  goto :DelphiEndCase
:Delphi2006
  call :ReadReg Borland\BDS\4.0
  goto :DelphiEndCase
:Delphi2007
  call :ReadReg Borland\BDS\5.0
  goto :DelphiEndCase
:Delphi2009
  call :ReadReg CodeGear\BDS\6.0
  goto :DelphiEndCase
:Delphi2010
  call :ReadReg CodeGear\BDS\7.0
  goto :DelphiEndCase
:DelphiXE
:DelphiXE1
  call :ReadReg Embarcadero\BDS\8.0
  goto :DelphiEndCase
:DelphiXE2
  call :ReadReg Embarcadero\BDS\9.0
  goto :DelphiEndCase
:DelphiXE3
  call :ReadReg Embarcadero\BDS\10.0
  goto :DelphiEndCase
:DelphiXE4
  call :ReadReg Embarcadero\BDS\11.0
  goto :DelphiEndCase
:DelphiXE5
  call :ReadReg Embarcadero\BDS\12.0
  goto :DelphiEndCase
:DelphiXE6
  call :ReadReg Embarcadero\BDS\14.0
  goto :DelphiEndCase
:DelphiXE7
  call :ReadReg Embarcadero\BDS\15.0
  goto :DelphiEndCase
:DelphiXE8
  call :ReadReg Embarcadero\BDS\16.0
  goto :DelphiEndCase
:Delphi10
:Delphi100
:DelphiXX10
:DelphiXX100
:Delphi10Seattle
:Delphi100Seattle
:DelphiXX10Seattle
:DelphiXX100Seattle
  call :ReadReg Embarcadero\BDS\17.0
  goto :DelphiEndCase
:Delphi101
:DelphiXX101
:Delphi101Berlin
:DelphiXX101Berlin
  call :ReadReg Embarcadero\BDS\18.0
  goto :DelphiEndCase
:Delphi102
:DelphiXX102
:Delphi102Tokyo
:DelphiXX102Tokyo
  call :ReadReg Embarcadero\BDS\19.0
  goto :DelphiEndCase
:Delphi103
:DelphiXX103
:Delphi103Rio
:DelphiXX103Rio
  call :ReadReg Embarcadero\BDS\20.0
  goto :DelphiEndCase
:Delphi104
:DelphiXX104
:Delphi104Sydney
:DelphiXX104Sydney
  call :ReadReg Embarcadero\BDS\21.0
  goto :DelphiEndCase
:Delphi11
:Delphi110
:Delphi11Alexandria
:Delphi110Alexandria
:DelphiXX11
:DelphiXX110
:DelphiXX11Alexandria
:DelphiXX110Alexandria
  call :ReadReg Embarcadero\BDS\22.0
  goto :DelphiEndCase
:DelphiEndCase

if exist "%DelphiPath%" goto allok
echo *** Error: Directory "%DelphiPath%" does not exist. Variable DelphiPath in %~df0 ***
pause
goto :eof

:allok
endlocal & set DelphiPath=%DelphiPath%
rem echo DelphiPath: "%DelphiPath%"
goto :eof

:ReadReg
rem read the registry entry
set DelphiPath=
FOR /F "usebackq skip=2 tokens=3,*" %%A IN (`REG QUERY "HKLM\SOFTWARE\%~1" /v RootDir 2^>nul`) DO (
  set DelphiPath=%%A %%B
  goto :HaveReg
)
:: we do not have a registry variable yet
if "%gx_windows_type%" == "win64" (
  FOR /F "usebackq skip=2 tokens=3,*" %%A IN (`REG QUERY "HKLM\SOFTWARE\Wow6432Node\%~1" /v RootDir 2^>nul`) DO (
    set DelphiPath=%%A %%B
    goto :HaveReg
  )
)
:HaveReg
rem remove one trailing space which might have been added because %%B was empty
rem remove any quotes
set DelphiPath=%DelphiPath:"=%
rem add quotes
set DelphiPath="%DelphiPath%"
rem remove space before the closing quote
set DelphiPath=%DelphiPath: "="%
rem remove any quotes
set DelphiPath=%DelphiPath:"=%
goto :eof
