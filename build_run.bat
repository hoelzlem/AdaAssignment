@call gprbuild -Ppfc_psu
@if %errorlevel% neq 0 exit /b %errorlevel%
@call obj\main.exe
