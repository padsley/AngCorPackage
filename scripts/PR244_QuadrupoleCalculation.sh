#Do CHUCK3 calculation
cd ../chuck
./chuck < input/mg24_quad.com > output/mg24_quad.lis
cp fort.2 ../angcor/input

#Do angcor input calculation
cd ../angcor/input
g++ make_input_PR244_J_2.c -o make_input_PR244_J_2
./make_input_PR244_J_2 .
./DoAngCorPR244J_2.sh
cd ../output
./make_final.sh
mv final.dat finalQuad.dat
cat finalQuad.dat

cd ../../AngCorAveraging
#This next step isn't really necessary as long as you aren't modifying the averaging code
g++ AverageAngCorResults.cpp -o AverageAngCorResults `root-config --cflags --libs` -O3
./AverageAngCorResults ../chuck/output/mg24_quad.lis ../angcor/output/finalQuad.dat QuadOutput.root
pwd
