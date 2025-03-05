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

#ifndef CSVINTERFACE_H
#define CSVINTERFACE_H

#include <string>
#include <vector>

#include "boost/filesystem.hpp"

#include "timeSeriesTensorInterface.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{

class csvInterface 
{
private:
	timeSeriesTensorInterface *tsTensor;
	boost::filesystem::path workDir;
	char decimalSeparator;
	char delimiter;
	bool adjointOutput;
    ofstream* files;
	void write(char filename[], int n, string* series, double** data);
	void write(ofstream& file, int timeStep, double *data);
    void write(ofstream& file, int timeStep, double *data, std::vector<string>& additionalTimeSerieNames, std::vector<int>& additionalTimeserie);

public:
	csvInterface(timeSeriesTensorInterface* valueTensor, boost::filesystem::path workDir, 
		char decimalSeparator = '.', char delimiter = ',', bool adjointOutput = false);
	~csvInterface();

    void openFiles();
	void writeFiles(string filename, vector<string> series, double*** tensor);
	void writeFiles(string filename, int nSeries, string* series, double*** tensor);
    void writeFiles(int timeStep);
    void writeFiles(int timeStep, std::vector<string>& additionalTimeSerieNames, std::vector<int>& additionalTimeseries);

    void closeFiles();
	void write(int timeStep);
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* CSVINTERFACE_H */
