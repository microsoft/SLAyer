#!/bin/sh

# echo "Setting environment for running SLAyer tests"

# setup file for running SLAyer tests, source after SLAyer/config.sh

# The slam frontend uses the SDV cl.exe.
# (SI: even when not using sdv, we still need sdv/product/bin/parser/ARCH/EspPersist.dll.)
export PATH="$SL_UNIX/SLAyer/tools/sdv/product/bin:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/sdv/product/bin/engine:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/sdv/product/bin/engine/slayer:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/sdv/product/bin/parser/x86_esp:$PATH"
export PATH="$SL_UNIX/SLAyer/tools/sdv/dev/bin:$PATH"

# For cl.exe to pick up slayer.h, etc.
export INCLUDE="$SL_WIN\SLAyer\include;$INCLUDE"
export INCLUDE="$SL_WIN\SLAyer\test\sll;$INCLUDE"
export INCLUDE="$SL_WIN\SLAyer\test\csll;$INCLUDE"
export INCLUDE="$SL_WIN\SLAyer\test\kmdf;$INCLUDE"

# Add these to your path (test/Makefile does) to compile drivers
export SL_INCLUDE_TOASTER="${SL_WIN}SLAyer\test\kmdf;${SL_WIN}SLAyer\test\kmdf\toaster\inc;${SL_WIN}SLAyer\test\kmdf\toaster\func\shared"
export SL_INCLUDE_PCIDRV="${SL_WIN}SLAyer\test\kmdf;${SL_WIN}SLAyer\test\kmdf\pci_drv;${SL_WIN}SLAyer\test\kmdf\pci_drv\HW"
