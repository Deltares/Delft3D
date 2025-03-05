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

#ifndef CONVERTER_H
#define CONVERTER_H

namespace rtctools
{
namespace utilities
{

class converter
{
public:
	enum interpolationOption{BLOCK, LINEAR};

	enum propertyEnum {
		POSITIVE_VALUE,
		NONNEGATIVE_VALUE,
		STRICTLY_MONOTONIC,
		MONOTONIC_2ND
	};

	converter() {};
	virtual ~converter() {};

	virtual void checkProperty(propertyEnum prop) = 0;

	virtual double convert(double xVal) = 0;
	virtual double convertDer(double xVal) = 0;
	virtual double reverseConvert(double yVal) = 0;
	virtual double reverseConvertDer(double yVal) = 0;
	virtual converter* getIntegrator() = 0;
};

} // end namespace utilities
} // end namespace rtctools

#endif /* CONVERTER_H */
