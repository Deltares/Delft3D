/*
    Copyright 2011 Davide Cesari <dcesari69 at gmail dot com>

    This file is part of FortranGIS.

    FortranGIS is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    FortranGIS is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with FortranGIS.  If not, see
    <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include "shapefil.h"

void SHPAPI_CALL SHPSetObjectFortran(void*, SHPObject*, 
 int*, int*, int*, int*, int*, int*,
 double*, double*, double*, double*,
 double*, double*, double*, double*,
 double*, double*, double*, double*);


void SHPSetObjectInt(void *ftnobject, SHPObject *psObject) {
  SHPSetObjectFortran(ftnobject, psObject,
		      &psObject->nSHPType, &psObject->nShapeId, &psObject->nParts,
		      psObject->panPartStart, psObject->panPartType,
		      &psObject->nVertices,
		      psObject->padfX, psObject->padfY,
		      psObject->padfZ, psObject->padfM,
		      &psObject->dfXMin, &psObject->dfYMin,
		      &psObject->dfZMin, &psObject->dfMMin,
		      &psObject->dfXMax, &psObject->dfYMax,
		      &psObject->dfZMax, &psObject->dfMMax);
}


int SHPAPI_CALL SHPReadObjectInt(SHPHandle hshp, int iShape, void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPReadObject(hshp, iShape);

  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}


int SHPAPI_CALL SHPCreateSimpleObjectInt(int nSHPType, int nVertices,
   double *padfX, double *padfY, double *padfZ, void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPCreateSimpleObject(nSHPType, nVertices, padfX, padfY, padfZ);
  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}
						 

int SHPAPI_CALL SHPCreateObjectInt(int nSHPType, int iShape,
   int nParts, int *panPartStart, int *panPartType,
   int nVertices, double *padfX, double *padfY, double *padfZ, double *padfM,
   void *ftnobject) {
  SHPObject *psObject;

  psObject = SHPCreateObject(nSHPType, iShape, nParts, panPartStart,
			     panPartType, nVertices, padfX, padfY, padfZ, padfM);
  if (psObject) {
    SHPSetObjectInt(ftnobject, psObject);
    return 0;
  } else {
    return 1;
  }
}
						 

void SHPAPI_CALL SHPComputeExtentsInt(SHPObject *psObject, void *ftnobject) {

  SHPComputeExtents(psObject);
  SHPSetObjectInt(ftnobject, psObject);

}


int SHPAPI_CALL SHPRewindObjectInt(SHPHandle *hSHP, SHPObject *psObject,
				   void *ftnobject) {
  int res;
  res = SHPRewindObject(*hSHP, psObject);
  SHPSetObjectFortran(ftnobject, psObject,
		      &psObject->nSHPType, &psObject->nShapeId, &psObject->nParts,
		      psObject->panPartStart, psObject->panPartType,
		      &psObject->nVertices,
		      psObject->padfX, psObject->padfY,
		      psObject->padfZ, psObject->padfM,
		      &psObject->dfXMin, &psObject->dfYMin,
		      &psObject->dfZMin, &psObject->dfMMin,
		      &psObject->dfXMax, &psObject->dfYMax,
		      &psObject->dfZMax, &psObject->dfMMax);
  return res;

}

