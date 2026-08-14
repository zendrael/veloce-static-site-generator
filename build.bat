@echo off
echo Compilando Veloce SSG...
fpc -O3 -XX -Xs veloce.pas
if %errorlevel% == 0 (
    strip -x veloce
    upx --best --ultra-brute veloce
    echo.
    echo Compilacao concluida com sucesso!
    echo Execute: veloce help
) else (
    echo.
    echo Erro na compilacao.
)
pause
