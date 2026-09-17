@echo off
rem zig cc always emits .obj object files on Windows, but treesit.el's
rem grammar-build link step only globs for *.o to pass to the final link --
rem so compiled objects have to be renamed .obj -> .o or the link step sees
rem zero inputs ("no input files").
zig cc %*
set "rc=%errorlevel%"
if not "%rc%"=="0" exit /b %rc%
for %%A in (%*) do (
  if /I "%%~xA"==".c" if exist "%%~nA.obj" ren "%%~nA.obj" "%%~nA.o"
)
exit /b 0
