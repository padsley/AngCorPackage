for dir in *.lis
do 
#echo $dir
awk -f sort.awk $dir | tail -n 181 ; done > final.dat
rm *.lis
