//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id$
// $HeadURL$
#include<assert.h>
#include<math.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

void writetofilef_(float *a, int *n1, int *n2, int *n3, int *it, char *jtname)
{
    FILE *fp;
    float *data;
    int i,j;
    size_t nmemb, nwrite;
    char filename[1024];

    fprintf(stderr,"Float n1=%d n2=%d n3=%d timestep=%d jtname=%s\n",*n1, *n2, *n3, *it, jtname);
    sprintf(filename,"Array%03d_%03d_%03d_%03d_%s.bin",*it, *n1, *n2, *n3, jtname);

    fprintf(stderr,"filename = %s\n", filename);

    nmemb = (*n1);
    data = (float *)calloc(nmemb,sizeof(float));
    fp = fopen(filename, "w+");
    for (i=0; i<(*n2)*(*n3); i++) {
        for (j=0; j<(*n1); j++) {
            data[j] = (float)a[i*(*n1)+j];
//            fprintf(stderr,"data[%d]=%e, %e\n", j, data[j], a[i*(*n1)+j]);
//            if ( fpclassify(data[j]) != FP_NORMAL) {
//              fprintf(stderr,"nan for time %d at point n=%d m=%d\n", *it, j, i);
//              data[j] = -1.0;
//            }
        }
        nwrite = fwrite(data, sizeof(float), nmemb, fp);
        assert(nwrite==nmemb);
    }
    fflush(fp);
    fclose(fp);

    free(data);
    return;
}

void writetofilei_(int *a, int *n1, int *n2, int *n3, int *it, char *jtname)
{
    FILE *fp;
    float *data;
    int i,j;
    size_t nmemb, nwrite;
    char filename[1024];

    fprintf(stderr,"Float n1=%d n2=%d n3=%d timestep=%d jtname=%s\n",*n1, *n2, *n3, *it, jtname);
    sprintf(filename,"ArrayI%03d_%03d_%03d_%03d_%s.bin",*it, *n1, *n2, *n3, jtname);

    fprintf(stderr,"filename = %s\n", filename);

    nmemb = (*n1);
    data = (float *)calloc(nmemb,sizeof(float));
    fp = fopen(filename, "w+");
    for (i=0; i<(*n2)*(*n3); i++) {
        for (j=0; j<(*n1); j++) {
            data[j] = (float)a[i*(*n1)+j];
//            fprintf(stderr,"data[%d]=%e, %e\n", j, data[j], a[i*(*n1)+j]);
//            if ( fpclassify(data[j]) != FP_NORMAL) {
//              fprintf(stderr,"nan for time %d at point n=%d m=%d\n", *it, j, i);
//              data[j] = -1.0;
//            }
        }
        nwrite = fwrite(data, sizeof(float), nmemb, fp);
        assert(nwrite==nmemb);
    }
    fflush(fp);
    fclose(fp);

    free(data);
    return;
}


void random_nmbr_(float *r)
{
    *r = (float)drand48();
}
