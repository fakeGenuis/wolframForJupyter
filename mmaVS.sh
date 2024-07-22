#!/usr/bin/env bash

# https://support.wolfram.com/12473?src=mathematica
# TODO seems there is another binary call `ELProver` in new versions of `mathematica`
BINARYS=(math mcc mathematica wolfram Mathematica MathKernel WolframKernel)
MMA_PATH=/usr/local/Wolfram/Mathematica/

link_bin() {
    ver="$1"
    for bi in "${BINARYS[@]}"; do
        ln -sfv $MMA_PATH"$ver"/Executables/"$bi" /usr/local/bin/"$bi"
    done
    ln -sfv $MMA_PATH"$ver"/SystemFiles/Kernel/Binaries/Linux-x86-64/wolframscript /usr/bin/wolframscript
}

# do this if you want a `mathematica` installation with aur helper
rm_links() {
    for bi in "${BINARYS[@]}"; do
        ls -l "$(which "${bi:?}")" && rm -fv "$(which "${bi:?}")" || exit
    done
    ls -l "$(which wolframscript)" && rm -fv "$(which wolframscript)" || exit
}

other_vers() {
    cur=$1
    find $MMA_PATH -maxdepth 1 -mindepth 1 -type d ! -name "$cur" -printf "%f\n"
}

switch_vers() {
    CUR_VERSION=$(realpath "$(which Mathematica)" | cut -d'/' -f6)
    [[ -n "$CUR_VERSION" ]] && echo "Current Mathematica version ${CUR_VERSION}." || echo "Not available Mathematica linked!"

    echo "Select another version:"
    select ver in $(other_vers "$CUR_VERSION"); do
        echo "Switch to Mathematica version ${ver}."
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
sudo ./mmaVS.sh [Options]
change version or remove links of mathematica in your \$PATH.

--remove     remove links
--switch     change version (by relink)
EOF
esac
