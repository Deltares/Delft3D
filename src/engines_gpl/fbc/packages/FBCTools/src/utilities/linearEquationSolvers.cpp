// Copyright (C) 2013 Deltares, University of Duisburg-Essen
// Institute of Hydraulic Engineering and Water Resources Management
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
 * @date 2013
 */ 

#include <utilities/linearEquationSolvers.h>
#include <utilities/utils.h>
#include <math.h>
#include <stdexcept>

using namespace rtctools::utilities;


void rtctools::utilities::gj(int n, double** A, double* b, double* x)
{
	// augment A with b 
	double** Ab = new double*[n];
	for (int i=0; i<n; i++) {
		Ab[i] = new double[n+1];
        for (int j=0; j<n; j++) {
			Ab[i][j] = A[i][j];
		}
		Ab[i][n] = b[i];
	}

	rtctools::utilities::gj(n, Ab);

	// assign result to x
    for (int i=0; i<n; i++) {
        x[i] = Ab[i][n];
    }

	for (int i=0; i<n; i++) delete Ab[i];
	delete Ab;
}

void rtctools::utilities::gj(int n, double** Ab)
{
	// compute scaling factors for pivoting
	double* scale = new double[n];
	for (int i=0; i<n; i++) {
		scale[i] = 0.0;
        for (int j=0; j<n; j++) {
			scale[i] = max(scale[i],fabs(Ab[i][j]));
		}
	}

	// partial implicit pivoting
	for (int i=0; i<n-1; i++) {
		int m = i; 
		for (int j=i+1; j<n; j++) { 
			if (fabs(Ab[j][i]/scale[j]) > fabs(Ab[m][i]/scale[m])){ 
				m = j; 
			} 
		}
		if (m!=i) {
			std::swap(Ab[m],Ab[i]);
			std::swap(scale[m],scale[i]);
		}
	}
	delete scale;

	// perform forward elimination 
    for (int i=0; i<n; i++) {
        // divide working row by value of working position to get 1 on the diagonal
		if (Ab[i][i] == 0.0) {
			throw runtime_error("void gaussJordan(int n, double** A, double* b) - diagonal element is zero");
		} else {
			double val = Ab[i][i];
			for (int j=0; j<n+1; j++) Ab[i][j] /= val;
		}

        // eliminate value below working position by subtracting a multiple of the current row
        for (int row=i+1; row<n; row++) {
            double val = Ab[row][i];
            for (int j=0; j<n+1; j++) {
                Ab[row][j] -= val*Ab[i][j];
            }
        }
    }

	// backward substitution steps
    for (int i=n-1; i>=0; i--) {
        // eliminate value above working position by subtracting a multiple of the current row
        for (int row=i-1; row>=0; row--) {
            double temp = Ab[row][i];
            for (int j=0; j<n+1; j++) {
                Ab[row][j] -= temp*Ab[i][j];
            }
        }
    }
}
