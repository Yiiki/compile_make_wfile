#!/usr/bin/bash
ifort -o make_upf2w.x make_upf2w.f90
cd dbug
../make_upf2w.x Au.SG15.PBE.UPF Au_W 79 0.0 1.0 | tee log.test
cd ..
vi ./dbug/Au_W
