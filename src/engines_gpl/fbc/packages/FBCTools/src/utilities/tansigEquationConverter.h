// Copyright (C) 2012 Deltares
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
 * @author Dirk Schwanenberg, Stefano Galelli
 * @version 1.0
 * @date 2012
 */


#ifndef TANSIG_EQUATION_CONVERTER_H
#define TANSIG_EQUATION_CONVERTER_H

#include "converter.h"

namespace rtctools
{
namespace utilities
{

class tansigEquationConverter : public converter
{
public:
	tansigEquationConverter();
	~tansigEquationConverter(void);

	void checkProperty(converter::propertyEnum prop) {};

	double convert(double xVal);
	double convertDer(double xVal);
	double reverseConvert(double yVal);
	double reverseConvertDer(double yVal);
	converter* getIntegrator();
};

} // end namespace utilities
} // end namespace rtctools

#endif //TANSIG_EQUATION_CONVERTER_H
