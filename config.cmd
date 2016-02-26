@echo off

REM SLAyer setup file.
REM Most of these settings are for running slayer in stand-alone mode.
REM slayer running under slam needs very little of this.

set SLWIN=%~dp0..

set PATH=%SLWIN%\SLAyer\bin;%PATH%
set PATH=%SLWIN%\SLAyer\tools\bin;%PATH%
set PATH=%SLWIN%\SLAyer\tools\Z3\build;%PATH%
set PATH=%SLWIN%\SLAyer\tools\flexdll;%PATH%

REM sd merge.
set PATH=%$PROGRAMFILES%\Emacs\emacs\bin;%PATH%
set SDMERGE=%SLWIN%\SLAyer\tools\site-lisp\msel\mymerge.bat

REM dot
set PATH=%PROGRAMFILES%\Graphviz2.27\bin;%PATH%

REM the directory the ocaml compiler executables are installed into
set OCAML=%SLWIN%\SLAyer\tools\ocaml\bin

REM add ocaml compilers to PATH
set PATH=%SLWIN%\SLAyer\tools\ocaml\bin;%PATH%

REM add ocaml library to INCLUDE, mainly for compiling C interface code
set INCLUDE=%SLWIN%\SLAyer\tools\ocaml\lib;%INCLUDE%

REM must be in windows format
set OCAMLLIB=%SLWIN%\SLAyer\tools\ocaml\lib
