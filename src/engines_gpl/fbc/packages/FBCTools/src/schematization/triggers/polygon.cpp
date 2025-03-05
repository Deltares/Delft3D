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

#include <schematization/triggers/polygon.h> 
#include <stdexcept>
#include <cmath>
#include <stdlib.h>

using namespace rtctools::schematization::triggers;
using namespace std;


polygon::polygon(int nEdges, vector<double>& x, vector<double>& y, double value)
	: nEdges(nEdges), x(x), y(y), value(value)
{
	// check whether the polygon is closed
	if ((x[nEdges-1]-x[0])>1e-10 || (y[nEdges-1]-y[0])>1e-10) {
		throw runtime_error("polygon::polygon(int nEdges, double *x, double *y, double value) - polygon not closed.");
	}

	// check whether the polygon is simple
	if (!this->isSimple()) {
		throw runtime_error("polygon::polygon(int nEdges, double *x, double *y, double value) - polygon is not simple.");
	}
}


bool polygon::contains(double tx, double ty) 
{
	int i,j;
	bool c = false;

    // ray tracing algorithm
    for (i=0, j=nEdges-1; i<nEdges; j=i++) {
		if ( ((y[i]>ty)!=(y[j]>ty)) &&
		     (tx < (x[j]-x[i]) * (ty-y[i]) / (y[j]-y[i]) + x[i]) ) {
                 c = !c;
        }
    }

    return c;
}


// Checks the polygon whether to be simple or not (intersecting edges)
bool polygon::isSimple()
{
	// Initialize sign change and winding number.
	int i;
	int ii;
	int j;
	int jj;
	int np;
	int schg = 0; // Sign change indicator
	int wind = 0;

	double px;
	double py;
	double dx;
	double dy;
	double ppx;
	double ppy;
	double ddx;
	double ddy;
	double t;
	double t1;
	double t2;
	double tp;
	double crs;
	double crsp = 0.0;

	np = this->nEdges;
	px = this->x[0] - this->x[np - 1];
	py = this->y[0] - this->y[np - 1];

	// Loop over edges.
	for (i = 0, ii = 1; i < np; i++, ii++)
	{
		if (ii == np) ii = 0;
		dx = this->x[ii] - this->x[i];
		dy = this->y[ii] - this->y[i];
		crs = px * dy - py * dx; // Cross product at this vertex.
		if (crs * crsp < 0) schg = 1; // Sign change (i.e. concavity) found.
		if (py <= 0.0) {
			if (dy > 0.0 && crs > 0.0) wind++;
		} else {
			if (dy <= 0.0 && crs < 0.0) wind--;
		}
		px = dx;
		py = dy;
		if (crs != 0.0) crsp = crs; // Save previous cross product only if it has a sign!
	}

	if (abs(wind) != 1) return false; //return 0;
	// Check whether polygon is simple and convex.
	if (schg == 0) return true; //return (wind > 0 ? 1 : -1); // If wind is bigger than null return 1, else -1.

	for (i = 0, ii = 0; i < np; i++, ii++)
	{
		if (ii == np) ii = 0;
		dx = this->x[ii];
		dy = this->y[ii];
		px = this->x[i];
		py = this->y[i];
		tp = 0.0;

		for (j = i + 1, jj = i + 2; j < np; j++, jj++)
		{
			if (jj == np) {
				if (i == 0) break; 
				jj = 0;
			}
			ddx = this->x[jj];
			ddy = this->y[jj];
			t = (ddx - dx) * (py - dy) - (ddy - dy) * (px - dx);
			if (t * tp <= 0.0 && j > i + 1) {
				ppx = this->x[j];
				ppy = this->y[j];
				t1 = (px - ddx) * (ppy - ddy) - (py - ddy) * (ppx - ddx);
				t2 = (dx - ddx) * (ppy - ddy) - (dy - ddy) * (ppx - ddx);
				if (t1 * t2 <= 0.0) return false; //return 0; // Found an intersection, so done.
			}
			tp = t;
		}
	}
	return true; //return (wind > 0 ? 2 : -2); // No intersections found, so simple concave.
}
