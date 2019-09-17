# AngCorPackage
This is the AngCor analysis package containing codes for form factors, the modified CHUCK3, AngCor itself and my averaging code

For instructions please see the pdf which should be within the repo.

If you use this code, please cite the following in the interests of enabling others to use it as well:

This repository.

The arXiv report giving guidance on how the package runs (arXiv:I've not uploaded it yet, give me time).

M. Harakeh (Private Communication) - He and his colleagues wrote all of these codes with the exception of the Averaging one which I (Phil) wrote.

If you have problems, comments, questions or concerns then please raise an issue on the Github. If you found some of the instructions hard to follow then please let me know and I'll try to improve them. Even better - you can suggest or make changes to the instructions yourself!

To compile angcor with g77 (this works for Ubuntu systems at least):

Install g77 on your system. There are plenty of instructions for how to do this online.

Also install g2c libraries on your system. There are instructions for how to do this online, a link will follow when I can find a good one.

Now compile:

g77 -c \*.f

Now to compile the angcor executable:

g77 \*.o -o angcor

If you get errors about missing crt1.o and crti.o files then you need to run the command:

locate crt1.o

and add the output path of that command into your .bashrc as

export LIBRARY_PATH=/that/path/name:$LIBRARY_PATH

where "/that/path/name" is replaced by the appropriate output of "locate crt1.o".

Then try again:

g77 \*.o -o angcor

You may get a "cannot find -lgcc_s" error, in which case follow the instructions at https://askubuntu.com/questions/815641/g77-running-problem

You should find the library file that you need using:

locate libgcc_s.so

There may be multiple options here btw.

You should find the locations where the compiler is looking for the library file with:

ld -lgcc_s --verbose

There will be a lot of possible options here.

Then you need to make a soft link for the library file (from the locate part) at the location at which the compiler is looking (from the output of the ld part). As there's plentiful options in both cases, I don't know what different they really make but for context, the command which I used is:

sudo ln -s /usr/lib/gcc/x86_64-linux-gnu/4.8/libgcc_s.so /usr/lib/x86_64-linux-gnu/

Yours may differ but you could try that if you needed.

This results in an AngCor executable which is giving reasonable results.

I hope to fix the problems with AngCor and to make a version which compiles with gfortran but that's a problem which can be fixed at my leisure now that AngCor itself is working properly.
