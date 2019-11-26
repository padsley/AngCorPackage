#Do CHUCK3 calculation
cd chuck
./chuck < input/mg24_dipol.com > output/mg24_dipol.lis
cp fort.2 ../angcor/input

#Do angcor input calculation
cd ../angcor/input
g++ make_input_PR251_J_1.c -o make_input_PR251_J_1
./make_input_PR251_J_1 .
./DoAngCorPR251J_1.sh
cd ../output
./make_final.sh
mv final.dat finalDipoleGamma.dat

cd ../../AngCorAveraging
g++ AverageAngCorResults.cpp -o AverageAngCorResults `root-config --cflags --libs` -O3
./AverageAngCorResults ../chuck/output/mg24_dipol.lis ../angcor/output/finalDipoleGamma.dat DipoleOutputGamma.root


#DO QUADRUPOLE
cd ../chuck
./chuck < input/mg24_quad.com > output/mg24_quad.lis
cp fort.2 ../angcor/input

cd ../angcor/input
g++ make_input_PR251_J_2.c -o make_input_PR251_J_2
./make_input_PR251_J_2 .
./DoAngCorPR251J_2.sh
cd ../output
./make_final.sh
mv final.dat finalQuadrupoleGamma.dat

cd ../../AngCorAveraging
g++ AverageAngCorResults.cpp -o AverageAngCorResults `root-config --cflags --libs` -O3
./AverageAngCorResults ../chuck/output/mg24_quad.lis ../angcor/output/finalQuadrupoleGamma.dat QuadOutputGamma.root