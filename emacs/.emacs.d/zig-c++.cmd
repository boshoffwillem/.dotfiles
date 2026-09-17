@echo off
rem See zig-cc.cmd -- same .obj -> .o rename, for C++ scanner sources.
zig c++ %*
set "rc=%errorlevel%"
if not "%rc%"=="0" exit /b %rc%
for %%A in (%*) do (
  if /I "%%~xA"==".cc" if exist "%%~nA.obj" ren "%%~nA.obj" "%%~nA.o"
  if /I "%%~xA"==".cpp" if exist "%%~nA.obj" ren "%%~nA.obj" "%%~nA.o"
  if /I "%%~xA"==".cxx" if exist "%%~nA.obj" ren "%%~nA.obj" "%%~nA.o"
)
exit /b 0
