@echo off
cd /d "%~dp0"

where node >nul 2>nul
if errorlevel 1 (
  echo Node.js no esta disponible en PATH.
  echo Instala Node.js o abre este repo desde una terminal donde node funcione.
  pause
  exit /b 1
)

node tools\post-launcher\server.mjs
pause
