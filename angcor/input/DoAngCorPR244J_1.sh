for COUNT in {0..3}
do
    for COUNT3 in {0..9}
    do
	for COUNT2 in {0..180}
	do
	    #echo $COUNT
	    #echo $COUNT2
	    ANGLE=$COUNT"."$COUNT3
	    #echo $ANGLE
	    FILE="input_PR244_J_1_"$ANGLE"_"$COUNT2".com"
	    #echo $FILE
	    FILE2="../output/input_PR244_J_1_"$ANGLE"_"$COUNT2".lis"
	    #echo $FILE2
	    ../angcor < $FILE > $FILE2
	done
    done
done
