#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
float w=0.0;

	for(int i=0;i<41;i++)
	{
		for(int j=0;j<181;j++)
		{
			w=i/10.;
 			//printf("%.1f %d \n",w,j);

			FILE *f;
			char filename[200];
			sprintf(filename, "input_PR244_J_0_%.1f_%d.com", w,j);
			f = fopen(filename, "w");
		
			if(!f) {
				fprintf(stderr, "cannot open ...\n");
		
			}
			fprintf(f,"Test 0+ - state of 24Mg, Ex=9.305 MeV\n");
			fprintf(f,"3,0, 1,0,1,0,1,0,0,0,0,\n");
			fprintf(f,"150,0,0,0,0,0,1,\n");
			fprintf(f,"1,0,0,\n");
			fprintf(f,"0,0,0,\n");
			fprintf(f, "%.1f,%d.,0.,0.,0.,\n", w,j);
			fprintf(f,"0.,0.,0.,0.,\n");
			fprintf(f,"1,181,");

			fclose(f);

		}
	}


}
