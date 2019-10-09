#! /bin/bash

reset

gfortran -c *.f
gfortran *.o -o angcor_gfortran
./angcor_gfortran < input_PR251_J_2_1.2_120.com.txt > gfortran_out.txt

g77 -c *.f
g77 *.o -o angcor_g77
./angcor_g77 < input_PR251_J_2_1.2_120.com.txt > g77_out.txt

reset

diff -y --suppress-common-lines gfortran_out.txt g77_out.txt
#diff -y gfortran_out g77_out
