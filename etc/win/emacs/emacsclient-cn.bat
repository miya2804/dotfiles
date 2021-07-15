@echo off
setlocal
for /f "usebackq delims=" %%A in (`wsl wslpath "%1"`) do set FILEPATH=%%A
if "%FILEPATH%"=="." ( set FILEPATH="" )
wsl $HOME/.dotfiles/etc/win/emacs/emacsclient-cn.sh %FILEPATH%
