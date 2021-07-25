@echo off

set toolchain=O:\Amiga\Toolchain\
set exename="O:\Amiga\DH0\%1.exe"

%toolchain%\bin\vasmm68k_mot -spaces -opt-brajmp -opt-allbra -align -Fhunk -maxerrors=0 -x  -I"%toolchain%targets\m68k-amigaos\ndk\include_i" -I"Source" -o "%1.o" main.asm
if errorlevel 1 (
   exit /b %errorlevel%
)

%toolchain%bin\vlink -bamigahunk -Bstatic -Cvbcc -mrel -nostdlib -L"%toolchain%targets\m68k-amigaos\lib" "%1.o" -S -s -x -o %exename%
if errorlevel 1 (
   exit /b %errorlevel%
)

set sizeb=0
set sizek=0
call :filesize %exename%
echo %1.exe: 	%sizeb% bytes (%sizek%K)

cranker.exe -q -f %exename% -o %exename%

set sizeb=0
set sizek=0
call :filesize %exename%
echo Crunched: %sizeb% bytes (%sizek%K)

goto :eof

:: set filesize of 1st argument in %size% variable, and return
:filesize
  set sizeb=%~z1
  set /a sizek=%sizeb%/1024
  exit /b 0
