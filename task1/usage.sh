#!/usr/bin/bash env

if [[ $# -ne 1 ]]; then
    echo "usage: $0 SOURCE"
    exit 1
fi

SOURCE="$1"

if [[ ! -f $SOURCE || ${SOURCE: -4} != ".ins" ]]; then
    echo "file '$SOURCE' has to exist and have .ins extension"
    exit 1
fi

COMPILER="./Compiler"

if [[ ! -f $COMPILER ]]; then
    echo "Couldn't find '$COMPILER', run 'make'."
    exit 1
fi

for DEPENDENCY in $DEPENDENCIES; do
    if [[ ! -f $DEPENDENCY ]]; then
        echo "Couldn't find '$DEPENDENCY'."
        exit 1
    fi
done

for VM_BINARY in $VM_BINARIES ; do
    if ! command -v "$VM_BINARY" &> /dev/null; then
        echo "Couldn't find '$VM_BINARY'."
        exit 1
    fi
done
