// Copyright (C) 2010 Deltares
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

/**
 * @file
 * @brief xxx
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */


#include "lookup2DTableConverter.h"
#include <iostream>
#include <stdexcept>
#include "utils.h"

using namespace std;
using namespace rtctools::utilities;

lookup2DTableConverter::lookup2DTableConverter(
	int nX, int nY, double *x, double *y, double **z, interpolationOption intOpt)
{
	this->nX = nX;
	this->nY = nY;

	this->x = new double[nX];
	for (int i=0; i<nX; i++) {
		this->x[i] = x[i];
	}
	this->y = new double[nY];
	for (int i=0; i<nY; i++) {
		this->y[i] = y[i];
	}
	this->z = utils::dmat(nX, nY);
	for (int i=0; i<nX; i++) {
		for (int j=0; j<nY; j++) {
			this->z[i][j] = z[i][j];
		}
	}
	this->intOpt = intOpt;
}

lookup2DTableConverter::~lookup2DTableConverter(void)
{
	delete []x;
	delete []y;
	utils::free_dmat(z);
}

double lookup2DTableConverter::convert(double xVal, double yVal)
{
	return interpolate(x, y, z, xVal, yVal);
}

double lookup2DTableConverter::interpolate(double *x, double *y, double **z, double xVal, double yVal)
{
	// check for NaN input
	if ((xVal!=xVal) || (yVal!=yVal)) return numeric_limits<double>::quiet_NaN();

	// get indizes to 4 surrounding data points
	int j = -1;
	int k = -1;
	for (int i=0; i<nX-1; i++) {
		if ((xVal>=x[i]) & (xVal<=x[i+1])) {
			j = i;
			break;
		}
	}
	for (int i=0; i<nY-1; i++) {
		if ((yVal>=y[i]) & (yVal<=y[i+1])) {
			k = i;
			break;
		}
	}
	if ((j==-1) || (k==-1)) 
	{
		throw runtime_error("double lookup2DTableConverter::interpolate(double *x, double *y, double **z, double xVal, double yVal) - input out of table range");
	}

	double z1 = z[j][k];
	double z2 = z[j+1][k];
	double z3 = z[j+1][k+1];
	double z4 = z[j][k+1];

	double t = (xVal-x[j])/(x[j+1]-x[j]);
	double u = (yVal-y[k])/(y[k+1]-y[k]);

	if (intOpt==BLOCK) {
		return z1;
    } else if (intOpt==BILINEAR) {
        return (1-t)*(1-u)*z1 + t*(1-u)*z2 + t*u*z3 + (1-t)*u*z4;
	}

	return numeric_limits<double>::quiet_NaN();
}
