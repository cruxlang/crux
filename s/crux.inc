CRUX=dist/build/crux/crux

if [ "$MSYSTEM" = "MINGW64" ]; then
    CRUX_EXE=crux.exe
else
    CRUX_EXE=crux
fi

if [ -e .stack-work ] ; then
    echo STACK
    CRUX=$(find .stack-work/install -iname $CRUX_EXE -type f)
else
    echo NOSTACK
    CRUX=dist/build/crux/crux
fi
