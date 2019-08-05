#Do CHUCK3 calculation
cd chuck
./chuck < input/mg24_dipol.com > output/mg24_dipol.lis
cp fort.2 ../angcor/input

#Do angcor input calculation
cd ../angcor/input
g++ make_input_PR244_J_1.c -o make_input_PR244_J_1
./make_input_PR244_J_1 .
./DoAngCorPR244J_1.sh
cd ../output
./make_final.sh
mv final.dat finalDipole.dat

cd ../../AngCorAveraging
g++ AverageAngCorResults.cpp -o AverageAngCorResults `root-config --cflags --libs` -O3
./AverageAngCorResults ../chuck/output/mg24_dipol.lis ../angcor/output/finalDipole.dat DipoleOutput.root