#!/usr/bin/env bash

# https://support.wolfram.com/12473?src=mathematica
# TODO seems there is another binary call `ELProver` in new versions of `mathematica`
BINARYS=(math mcc mathematica wolfram Mathematica MathKernel WolframKernel)
# NOTE keep those from =aur= helper untouchedj
# NOTE math == MathKernel = wolfram = WolframKernel
# NOTE wolframnb == WolframNB
# BINARYS=(ELProver MathKernel WolframKernel WolframNB math mcc wolfram wolframnb wolframscript)

# manually installed path from mma.iso
MMA_PATH=/usr/local/Wolfram/Mathematica/

LINK_PATH=~/.local/bin/

link_bin() {
    ver="$1"
    for bi in "${BINARYS[@]}"; do
        ln -sfv $MMA_PATH"$ver"/Executables/"$bi" $LINK_PATH"$bi"
    done
    ln -sfv $MMA_PATH"$ver"/SystemFiles/Kernel/Binaries/Linux-x86-64/wolframscript $LINK_PATH"wolframscript"
}

rm_links() {
    for bi in "${BINARYS[@]}"; do
        ls -l $LINK_PATH"${bi:?}" && rm -fv $LINK_PATH"${bi:?}" || exit
    done
    ls -l $LINK_PATH"wolframscript" && rm -fv $LINK_PATH"wolframscript" || exit
}

other_vers() {
    cur=$1
    find $MMA_PATH -maxdepth 1 -mindepth 1 -type d ! -name "$cur" -printf "%f\n"
}

switch_vers() {
    CUR_VERSION=$(realpath "$(which wolfram)" | cut -d'/' -f6)
    [[ -n "$CUR_VERSION" ]] && echo "Current wolfram version ${CUR_VERSION}." || echo "No available wolfram linked!"

    echo "Select another version:"
    select ver in $(other_vers "$CUR_VERSION"); do
        echo "Switch to wolfram version ${ver}."
        link_bin "$ver"
        break
    done
}

case "$1" in
--remove)
    rm_links
    ;;
--switch)
    switch_vers
    ;;
*)
    cat <<EOF
./mmaVS.sh [Options]
change version or remove links of mathematica in your \$PATH.

--remove     remove links (do this before a new fresh installation)
--switch     change version (by relink)
EOF
    ;;
esac
