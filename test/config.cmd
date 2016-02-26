@echo off

REM  setup file for running SLAyer tests, source after SLAyer/config.cmd

REM  The slam frontend uses the SDV cl.exe.
set PATH=%SLWIN%\SLAyer\tools\sdv\product\bin;%PATH%
set PATH=%SLWIN%\SLAyer\tools\sdv\product\bin\engine;%PATH%
set PATH=%SLWIN%\SLAyer\tools\sdv\product\bin\engine\slayer;%PATH%
set PATH=%SLWIN%\SLAyer\tools\sdv\product\bin\parser\x86_esp;%PATH%
set PATH=%SLWIN%\SLAyer\tools\sdv\dev\bin;%PATH%

REM  For cl.exe to pick up slayer.h, etc.
set INCLUDE=%SLWIN%\SLAyer\include;%INCLUDE%
set INCLUDE=%SLWIN%\SLAyer\test\sll;%INCLUDE%
set INCLUDE=%SLWIN%\SLAyer\test\csll;%INCLUDE%
set INCLUDE=%SLWIN%\SLAyer\test\kmdf;%INCLUDE%
set INCLUDE=%SLWIN%\SLAyer\test\kmdf\toaster\inc;%INCLUDE%
set INCLUDE=%SLWIN%\SLAyer\test\kmdf\toaster\func\shared;%INCLUDE%
