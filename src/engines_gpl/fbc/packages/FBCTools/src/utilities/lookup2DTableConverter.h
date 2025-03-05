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


#ifndef LOOKUP2DTABLECONVERTER_H
#define LOOKUP2DTABLECONVERTER_H

#include <limits>

using namespace std;

namespace rtctools
{
namespace utilities
{

class lookup2DTableConverter
{
public:
	enum interpolationOption{BLOCK, BILINEAR};

private:
	int nX;
	int nY;
	double *x;
	double *y;
	double **z;
	interpolationOption intOpt;

	double interpolate(double *x, double *y, double **z, double xVal, double yVal);

public:
	lookup2DTableConverter(int nX, int nY, double *x, double *y, double **z, interpolationOption intOpt);
	~lookup2DTableConverter(void);

	double convert(double xVal, double yVal);
};

} // end namespace utilities
} // end namespace rtctools

#endif //LOOKUP2DTABLECONVERTER_H
