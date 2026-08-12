@echo off
echo Compilando Veloce SSG...
fpc -O3 -XX -Xs veloce.pas
if %errorlevel% == 0 (
    echo.
    echo Compilacao concluida com sucesso!
    echo Execute: veloce help
) else (
    echo.
    echo Erro na compilacao.
)
pause
