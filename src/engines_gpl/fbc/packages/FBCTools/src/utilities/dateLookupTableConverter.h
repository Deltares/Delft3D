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

#ifndef DATELOOKUPTABLECONVERTER_H
#define DATELOOKUPTABLECONVERTER_H

namespace rtctools
{
namespace utilities
{

/**
	Convert from date / time values to double value.
	<p>
	Table: input[int[]] output[double]

	<p>
	Date fields consist of six int values:
	<br>[year, month_of_year, day_of_month, hour_of_day, minute_of_hour, second_of_minute]
	<p>
	If a date field is to be ignored enter -1.
	<p>
	example:
	<br>days of month [-1, (1-12), (1-31), -1, -1, -1][double]
	<br>time of day [-1, -1, -1, (1-24), (1-60), (1-60)][double]
	<br>date time [year, (1-12), (1-31), (1-24), (1-60), (1-60)][double]
 */
class dateLookupTableConverter
{

public:

    /**
	 *	Default constructor.
	 *
	 *	\param dateValues	Values of input column.
	 *	\param values		Values of output column.
     */
	dateLookupTableConverter(int nValue, int **dateArray, double *valueArray);

	/**
		Deletes the dateLookupTableConverter.
	*/
	~dateLookupTableConverter();

	/**
	 *	Perform lookup table conversion from x-column to y-column.
     *
	 *	\param searchTime	Time in millis for which to return a value.
	 *	\return				Corresponding value for \searchTime.
     */
	double convert(long long searchTime);

	/**
		Returns the size of the date table which is the length of the x-column.
	*/
	int size() const;

	/**
		Returns the corresponding value at \a index.

		\see convert()
	*/
	double value(int index);

private:
	bool isAscending(int **dateValues);
	long long setDateFields(long long time, int dateArray[]);

	int nValue;
	int **dateArray;
	double *valueArray;
};

} // end namespace utilities
} // end namespace rtctools

#endif // DATELOOKUPTABLECONVERTER_H
