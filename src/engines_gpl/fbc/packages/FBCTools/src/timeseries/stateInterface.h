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


#ifndef STATEINTERFACE_H
#define STATEINTERFACE_H

#include <string>

#include "boost/filesystem.hpp"

#include "timeSeriesTensorInterface.h"

using namespace std;

namespace rtctools
{
namespace timeseries
{

class stateInterface 
{
private:
	boost::filesystem::path schemaDir;
	boost::filesystem::path workDir;
	timeSeriesTensorInterface *tsTensor;

public:
	stateInterface(boost::filesystem::path schemaDir, boost::filesystem::path workDir, timeSeriesTensorInterface *tsTensor);
	~stateInterface();

	void read();
	void write(int tIndex=-1);
   void writeData(string pathAndFilename, int tIndex = -1);
   
   /**
   * @brief Writes state at defined time index
   */
   void writeStateExport(int tIndex, bool isSnapshot = false);
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* STATEINTERFACE_H */
