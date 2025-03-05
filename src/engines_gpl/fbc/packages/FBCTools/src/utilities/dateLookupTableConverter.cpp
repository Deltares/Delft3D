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
	@file dateLookupTableConverter.h
	@brief xxx
	@author Tobias Schruff
	@version 1.0
	@date 2010
 */


#include "dateLookupTableConverter.h"

#include <utilities/utils.h>
#include <stdexcept>

using namespace rtctools::utilities;

dateLookupTableConverter::dateLookupTableConverter(int nValue, int **dateArray, double *valueArray)
{
	this->nValue = nValue;
	// TODO copy arrays
	this->dateArray = dateArray;
	this->valueArray = valueArray;

	if (nValue>1 && !isAscending(dateArray)) {
		throw runtime_error("dateLookupTableConverter::dateLookupTableConverter(int nValue, int **dateArray, double *valueArray) - date array is not configured in ascending order!");
	}
}

dateLookupTableConverter::~dateLookupTableConverter()
{
	// TODO free arrays, see above
}

double dateLookupTableConverter::convert(long long searchTime)
{
	// only one value available
	if (nValue==1) return valueArray[0];

	if (searchTime < setDateFields(searchTime, dateArray[0])) {
		// search time lower than first date
		return valueArray[0];
	} else if (searchTime > setDateFields(searchTime, dateArray[nValue-1])) {
		// search time larger than last date
		return valueArray[nValue-1];
	} else {
		// search time within date array range
		for (int i=1; i<nValue; i++) {
			long long t1 = setDateFields(searchTime, dateArray[i-1]);
			long long t2 = setDateFields(searchTime, dateArray[i]);
			if ((searchTime >= t1) && (searchTime <= t2)) {
				return valueArray[i-1] + (valueArray[i]-valueArray[i-1])*(searchTime-t1)/(t2-t1);
			}
		}
	}

	throw runtime_error("double dateLookupTableConverter::convert(long long searchTime) - error");
}

int dateLookupTableConverter::size() const
{
	return nValue;
}

double dateLookupTableConverter::value(int index)
{
	if (index < 0 || index >= size())
		throw runtime_error("dateLookupTableConverter::value(int index) - index out of range");

	return valueArray[index];
}

bool dateLookupTableConverter::isAscending(int **dateArray)
{
	int myDate[7] = {1972, 11, 9, 17, 0, 0, 0};
	long long t = utils::date2time(myDate);

    // check if the following dates are increasing
    for (int i=1; i<nValue; i++) {
		if (setDateFields(t, dateArray[i-1]) >= setDateFields(t, dateArray[i])) {
			return false;
		}
    }

    return true;
}

long long dateLookupTableConverter::setDateFields(long long time, int *dateArray)
{
	int myDate[7];
	utils::time2date(time, myDate);

	for (int i=0; i<7; i++) {
		if (dateArray[i]>-1) {
			myDate[i] = dateArray[i];
		}
	}

	return utils::date2time(myDate);
}
