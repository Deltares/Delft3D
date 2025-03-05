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

#ifndef OPENMIINTERFACE_H
#define OPENMIINTERFACE_H

#include <string>
#include <vector>

#include "openMIExchangeItem.h"
#include "timeSeriesMatrixInterface.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{

/**
  * @brief The class contains the OpenMI interface.
  */
class openMIInterface
{
	private:
		/**
		 * @brief Array with information about the input exchange items.
		 */
		vector<openMIExchangeItem> input;

		/**
		 * @brief Array with information about the output exchange items.
		 */
		vector<openMIExchangeItem> output;

		timeSeriesMatrixInterface *tsMatrix;
		int iStep;
		long long dTime;

	public:
		/**
		 * @brief Constructor.
		 */
		openMIInterface();
		openMIInterface(timeSeriesMatrixInterface *tsMatrix, vector<openMIExchangeItem> input, vector<openMIExchangeItem> output);

		/**
		 * @brief Destructor.
		 */
		~openMIInterface() {};

		int getInputExchangeItemCount();
		openMIExchangeItem* getInputExchangeItem(int index);
		int getOutputExchangeItemCount();
		openMIExchangeItem* getOutputExchangeItem(int index);

		void prepare();
		long long getCurrentTime();
		int getCurrentTimeStep();
		double getValue(int sIndex);
		void setValue(int sIndex, double value, int timeStepCount=1);
		int performTimeStep();
};

} // end namespace timeseries
} // end namespace rtctools

#endif //OPENMIINTERFACE_H
