@echo off

rem ***************************************************************************

rem Put this in your main project directory.
rem
rem It expects "huc" and "squirrel" directories to be in the parent
rem directory, i.e. one level higher.

rem ***************************************************************************

setlocal

cd /d "%~dp0"

pushd
cd ..
set PATH=%CD%\huc\bin;%PATH%
popd
set PCE_INCLUDE=%CD%

pceas -raw -m -l 2 -S teos.s
if errorlevel 1 goto :error

exit /b 0

rem ***************************************************************************

:error
rem pause
exit /b 1

rem ***************************************************************************
