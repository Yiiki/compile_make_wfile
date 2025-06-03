#!/usr/bin/bash
module load intel
ifort -o check_atomw.x check_atomw.f90
