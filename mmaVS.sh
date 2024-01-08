#!/usr/bin/env bash

BINARYS=(math mcc mathematica wolfram Mathematica MathKernel WolframKernel)
MMA_PATH=/usr/local/Wolfram/Mathematica/

link_bin() {
    ver="$1"
    for bi in "${BINARYS[@]}"; do
        ln -sfv $MMA_PATH"$ver"/Executables/"$bi" /usr/local/bin/"$bi"
    done
    ln -sfv $MMA_PATH"$ver"/SystemFiles/Kernel/Binaries/Linux-x86-64/wolframscript /usr/bin/wolframscript
}

CUR_VERSION=$(realpath "$(which Mathematica)" | cut -d'/' -f6)
echo "Current Mathematica version ${CUR_VERSION}."

echo "Select another version:"

other_vers() {
    cur=$1
    find $MMA_PATH -maxdepth 1 -mindepth 1 -type d ! -name "*$cur" -printf "%f\n"
}
select ver in $(other_vers "$CUR_VERSION"); do
    echo "Switch to Mathematica version ${ver}."
    link_bin "$ver"
    break
done
