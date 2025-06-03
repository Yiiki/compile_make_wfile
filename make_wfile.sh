#!/usr/bin/bash

module load intel

dir_psp=../pbe

flist=(
Si.SG15.PBE.UPF
Ge.SG15.PBE.UPF
H.SG15.PBE.UPF
)

fwf_list=(Si_W Ge_W H_W)

tatom_list=(14 32 1)

rho0_list=(0.03 0.6 0.0)

a0_list=(1.2 1.2 1.0)

echo "--------------------------------------------------------"

for i in {0..2}
do
# set -x # open debug mode
eval "fpsp="${flist[$i]}
eval "fwf="${fwf_list[$i]}
eval "tatom="${tatom_list[$i]}
eval "rho0="${rho0_list[$i]}
eval "a0="${a0_list[$i]}
# set +x # close debug mode
if [ -f $fpsp ]
then
    echo $fpsp" soft linked"
else
    ln -s $dir_psp/$fpsp .
    echo $fpsp" soft linked"
fi

~/compile_make_wfile/make_upf2w.x $fpsp $fwf $tatom $rho0 $a0

# output check file, that can be view old and new rho (e/Bohr3)
rad_box=4.5
cat >motif.input<<EOF
$fwf
$rad_box
EOF
~/compile_make_wfile/check_atomw.x

echo ""
echo ""
echo "--------------------------------------------------------"

done
