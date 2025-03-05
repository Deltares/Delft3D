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

#ifndef TIMESERIESMODEL_H
#define TIMESERIESMODEL_H

#include <string>

#include "boost/filesystem.hpp"

#include "utilities/utils.h"
#include "timeseries/csvInterface.h"
#include "timeseries/piTimeSeriesSAX2Handler.h"
#include "timeseries/openMIInterface.h"
#include "timeseries/stateInterface.h"
#include "timeseries/openMIExchangeItem.h"
#include "timeseries/timeSeriesTensorInterface.h"
#include "RTCToolsDLLDefs.h"

using namespace std;
using namespace rtctools::utilities;

namespace rtctools
{
namespace timeseries
{

/**
  * @brief The class contains the time series and supplies interfaces to them.
  */
class timeSeriesModel
{
	private:
		/**
		 * @brief Value tensor with 3D tensor of
		 * input and output time series, dimensions are representing
		 * ensembles, time steps, and series
		 */
		timeSeriesTensorInterface *valueTensor;

		/**
		 * @brief Csv interface for exporting data in Excel format (tab-separated)
		 */
		csvInterface *csvInt;

		/**
		 * @brief OpenMI interface for online coupling of models
		 */
		openMIInterface *openMIInt;

		/**
		 * @brief SAX2 parsing interface for reading / writing PI XML
		 */
		piTimeSeriesSAX2Handler *piSAX2Int;

		/**
		 * @brief State interface for reading / writing of states in openda / treeVector format (xml).
		 */
		stateInterface *stateInt;

		int nImport;
		int nExport;

		void readTimeSeriesTensor(boost::filesystem::path schemaDir, boost::filesystem::path workDir, string filename, long long t1, long long t2, long long dt, 
			int nEnsemble, vector<int>& ensembleMap, bool adjointOutput, bool limitedMemory);

	public:

		/**
		 * @brief Constructor.
		 *
		 * @param runtimeFilename Filename of file with runtime config
		 * @param dataFilename Filename of file with data config
		 */
		timeSeriesModel(boost::filesystem::path schemaDir, boost::filesystem::path workDir, string filename, long long t1, long long t2, long long dt, 
			int nEnsemble, vector<int>& ensembleMap, bool adjointOutput, bool limitedMemory);

		/**
		 * @brief Destructor.
		 */
		~timeSeriesModel(void);

		/**
		 * @brief Reads time series data from all available interfaces.
		 */
		void read(void);

        /**
         * @brief Opens file(s) for writing
         */
        void openFiles(bool adjointOutput);

		void write(bool adjointOutput);

		/**
		 * @brief Writes time series data into all available interfaces.
		 */
        void write(bool parInt, int timeStep, bool isLastTimeStep, std::vector<string>& additionalTimeseriesNames, std::vector<int>& additionalTimeseries);

        /**
         * @brief Closes file(s) for writing
         */
        void closeFiles(bool adjointOutput);


		/**
		 * @brief Gets the time series tensor.
		 */
		timeSeriesTensorInterface* getTimeSeriesTensor();

		/**
		 * @brief Gets the OpenMI interface.
		 */
		openMIInterface* getOpenMIInterface();

		/**
		 * @brief Gets the CSV interface.
		 */
		csvInterface* getCsvInterface() { return csvInt; };

		/**
		 * @brief Gets the PI-XML SAX interface.
		 */
		piTimeSeriesSAX2Handler* getPISAXInterface() { return piSAX2Int; };

		double getValue(int ensembleId, int tIndex, int sIndex);
		void setValue(int ensembleId, int tIndex, int sIndex, double value);

      void writeState(string filename, int tIndex);

		int getNImport() { return nImport; };
		int getNExport() { return nExport; };

      void writeStateExport(int tIndex, bool isSnapshot = false);
};

char* convert2CStr(string str) ;

} // end namespace timeseries
} // end namespace rtctools

#endif /* TIMESERIESMODEL_H */
