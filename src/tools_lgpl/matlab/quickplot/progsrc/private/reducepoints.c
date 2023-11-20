/*----- LGPL --------------------------------------------------------------------
 *
 *   Copyright (C) 2011-2023 Stichting Deltares.
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License as published by the Free Software Foundation version 2.1.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 *   contact: delft3d.support@deltares.nl
 *   Stichting Deltares
 *   P.O. Box 177
 *   2600 MH Delft, The Netherlands
 *
 *   All indications and logos of, and references to, "Delft3D" and "Deltares"
 *   are registered trademarks of Stichting Deltares, and remain the property of
 *   Stichting Deltares. All rights reserved.
 *
 *-------------------------------------------------------------------------------
 *   http://www.deltaressystems.com
 *   $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/tools/matlab/Delft3D-toolbox/progsrc/private/reducepoints.c $
 *   $Id: reducepoints.c 14474 2011-01-07 12:56:41Z jagers $
 */

#include <stdio.h>
#include <stdarg.h>
#include "mex.h"

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]) {
  int i,NElm,NInc, *J,ii;
  int ndims, ndims1, d;
  const int *dimarray, *dimarray1;
  int mrows,ncols;
  double *pthresh,thresh,*I,*x,*y,*z,dist,dx,dy,dz;
  bool include_i;

/* printf("%i input argument(s) and %i output argument(s).\n",nrhs,nlhs);
  for (i=0; i<nrhs; i++) {
    printf("arg%2.2i: ",i);
    ndims=mxGetNumberOfDimensions(prhs[i]);
    dimarray=mxGetDimensions(prhs[i]);
    printf("%i",dimarray[0]);
    for (d=1; d<ndims; d++) {
      printf("x%i",dimarray[d]);
    }
    printf(" %s\n",mxGetClassName(prhs[i]));
  } */

  /* Check for proper number of arguments. */
  if (nrhs<2)
    mexErrMsgTxt("At least two input arguments expected.");
  else if (nrhs>4)
    mexErrMsgTxt("Too many input arguments.");
  else if (nlhs>1)
    mexErrMsgTxt("Too many output arguments.");

  /* Get the threshold_distance. */
  mrows = mxGetM(prhs[0]);
  ncols = mxGetN(prhs[0]);
  if (!mxIsDouble(prhs[0]) || mxIsComplex(prhs[0]) || !(mrows==1 && ncols==1)) {
    mexErrMsgTxt("Threshold distance must be noncomplex scalar double.");
  }
  pthresh = mxGetPr(prhs[0]);
  thresh = pthresh[0];

  /* Check the input arrays for equal size. */
  if (!mxIsDouble(prhs[1]) || mxIsComplex(prhs[1])) {
    mexErrMsgTxt("Coordinate arrays must be noncomplex double.");
  }
  ndims1=mxGetNumberOfDimensions(prhs[1]);
  dimarray1=mxGetDimensions(prhs[1]);
  for (i=2; i<nrhs; i++) {
    if (!mxIsDouble(prhs[i]) || mxIsComplex(prhs[i])) {
      mexErrMsgTxt("Coordinate arrays must be noncomplex double.");
    }
    ndims=mxGetNumberOfDimensions(prhs[i]);
    if (ndims==ndims1) {
      dimarray=mxGetDimensions(prhs[i]);
      for (d=0; d<ndims; d++)
        if (dimarray[d] != dimarray1[d])
          mexErrMsgTxt("Coordinate arrays must be of equal size.");
    } else
      mexErrMsgTxt("Coordinate arrays must be of equal size.");
  }
  NElm = mxGetNumberOfElements(prhs[1]);

  /* Create return argument. */

  if (NElm>0) {
    J = mxCalloc(NElm,sizeof(int));
    J[0] = 0;
    NInc = 1;

    if (nrhs==2) {
      /* 1D */
      thresh = thresh*thresh; /* could use abs or not?*/

      /* Get coordinate arrays */
      x = mxGetPr(prhs[1]);

      /* Find 0-based indices to keep */
      for (i=1; i<NElm; i++) {
        include_i = true;
        for (d=NInc-1; d>=0; d--) {
          ii = J[d];
          dx = x[ii]-x[i];
          dist = dx*dx;
          if (dist<thresh) {
            include_i = false;
            break;
          }
        }
        if (include_i) {
          J[NInc] = i;
          NInc++;
        }
      }
    } else if (nrhs==3) {
      /* 2D */
      thresh = thresh*thresh;

      /* Get coordinate arrays */
      x = mxGetPr(prhs[1]);
      y = mxGetPr(prhs[2]);

      /* Find 0-based indices to keep */
      for (i=1; i<NElm; i++) {
        include_i = true;
        for (d=NInc-1; d>=0; d--) {
          ii = J[d];
          dx = x[ii]-x[i];
          dy = y[ii]-y[i];
          dist = dx*dx + dy*dy;
          if (dist<thresh) {
            include_i = false;
            break;
          }
        }
        if (include_i) {
          J[NInc] = i;
          NInc++;
        }
      }
    } else {
      /* 3D */
      thresh = thresh*thresh;

      /* Get coordinate arrays */
      x = mxGetPr(prhs[1]);
      y = mxGetPr(prhs[2]);
      z = mxGetPr(prhs[3]);

      /* Find 0-based indices to keep */
      for (i=1; i<NElm; i++) {
        include_i = true;
        for (d=NInc-1; d>=0; d--) {
          ii = J[d];
          dx = x[ii]-x[i];
          dy = y[ii]-y[i];
          dz = z[ii]-z[i];
          dist = dx*dx + dy*dy + dz*dz;
          if (dist<thresh) {
            include_i = false;
            break;
          }
        }
        if (include_i) {
          J[NInc] = i;
          NInc++;
        }
      }
    }

    /* Copy indices to output array and cleanup */
    plhs[0] = mxCreateDoubleMatrix(1,NInc,mxREAL);
    I = mxGetPr(plhs[0]);
    for (i=0; i<NInc; i++) {
      I[i] = (double)(J[i]+1);
    }
    mxFree(J);

  } else {

    /* empty array, empty index list */
    plhs[0] = mxCreateDoubleMatrix(1,0,mxREAL);

  }
}
