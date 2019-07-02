#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
float w=0.0;

	for(int i=0;i<101;i++)
// 	for(int i=0;i<5;i++)
	{
		for(int j=0;j<180;j++)
// 		for(int j=0;j<5;j++)
		{			
			w=i/10.;
// 			printf("%.1f %d \n",w,j);

			FILE *f;
			char filename[200];
			sprintf(filename, "input_mo_%.1f_%d.com", w,j);
			f = fopen(filename, "w");
		
			if(!f) {
				fprintf(stderr, "cannot open ...\n");
		
			}
			fprintf(f,"Test 2+ - state of 48Mo\n"); //Card 0
			fprintf(f,"0,0,0,0,1,0,1,0,0,1,0,\n"); //Card 1
			fprintf(f,"150,0,0,0,4,0,1,\n"); //Card 2
			fprintf(f,"1,2,4,\n"); //Card 3
			fprintf(f,"2,0,0,\n");
 			fprintf(f,"0.98885,0.963146,0.92366,\n"); //Card 5
			fprintf(f, "%.1f,%d.,0.,0.,0.,\n", w,j); //Card 6 and 7
			fprintf(f,"0.,0.,0.,0.,\n1,181,");
		
			fclose(f);

		}
	}


}
