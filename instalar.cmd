@echo off
chcp 65001 >nul
title Quadrante - instalacao
setlocal
set "AQUI=%~dp0"
echo.
echo  QUADRANTE - instalacao
echo  ==========================
echo  Precisa de: R (4.3 ou mais novo), Python 3 e os pacotes do R (baixados agora, ~150 MB).
echo  Nada e enviado para fora do seu computador; as planilhas ficam na pasta "levantamentos".
echo.

rem ---- 1. R ----
echo [ 10%%] Procurando o R...
call "%AQUI%app\acha_r.cmd"
if defined RSCRIPT (
  echo [ok] R encontrado: %RSCRIPT%
) else (
  echo [falta] R nao encontrado.
  call :oferece_winget "RProject.R" "R" "https://cran.r-project.org/bin/windows/base/"
  call "%AQUI%app\acha_r.cmd"
  if not defined RSCRIPT ( echo Instale o R e rode este instalador de novo. & pause & exit /b 1 )
)

rem ---- 2. Python ----
echo [ 30%%] Procurando o Python...
call "%AQUI%app\acha_python.cmd"
if defined PY (
  echo [ok] Python encontrado: %PY%
) else (
  echo [falta] Python 3 nao encontrado.
  call :oferece_winget "Python.Python.3.13" "Python" "https://www.python.org/downloads/"
  call "%AQUI%app\acha_python.cmd"
  if not defined PY ( echo Instale o Python e rode este instalador de novo. & pause & exit /b 1 )
)

rem ---- 3. pacotes do R ----
echo.
echo [ 50%%] Instalando os pacotes do R na pasta r-pacotes (so na primeira vez; leva alguns minutos)...
"%RSCRIPT%" "%AQUI%R\instalar_pacotes.R"
if errorlevel 1 ( echo. & echo Os pacotes nao instalaram. Confira a internet e rode de novo. & pause & exit /b 1 )

echo.
echo [ 95%%] Instalando a fonte Inter dos graficos (so para este usuario)...
powershell -NoProfile -ExecutionPolicy Bypass -File "%AQUI%app\instalar_fontes.ps1" -Pasta "%AQUI%fontes"

echo.
echo [ 98%%] Criando o atalho Quadrante na area de trabalho...
powershell -NoProfile -ExecutionPolicy Bypass -Command "$d=[Environment]::GetFolderPath('Desktop'); $w=New-Object -ComObject WScript.Shell; $s=$w.CreateShortcut((Join-Path $d 'Quadrante.lnk')); $s.TargetPath='%AQUI%abrir_quadrante.cmd'; $s.WorkingDirectory='%AQUI%'; $s.IconLocation='%AQUI%identidade\quadrante.ico,0'; $s.WindowStyle=7; $s.Description='Quadrante'; $s.Save()" >nul 2>nul
echo [100%%] Tudo pronto. Para usar: dois cliques no atalho "Quadrante" da area de trabalho
echo        (ou em "abrir_quadrante.cmd"). Ele abre numa janela propria; ao fechar, encerra sozinho.
echo Para testar, solte a planilha da pasta "exemplo" na pagina que abrir.
pause
exit /b 0

:oferece_winget
where winget >nul 2>nul
if errorlevel 1 (
  echo   Baixe e instale o %~2 por este endereco: %~3
  start "" "%~3"
  pause
  exit /b 0
)
set "RESP="
set /p "RESP=  Instalar o %~2 agora pelo winget (gerenciador de programas do Windows)? [S/N] "
if /i "%RESP%"=="S" (
  winget install --id %~1 -e --accept-package-agreements --accept-source-agreements
) else (
  echo   Baixe e instale o %~2 por este endereco: %~3
  start "" "%~3"
  pause
)
exit /b 0
