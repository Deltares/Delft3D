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

#ifndef PIDIAGINTERFACE_H
#define PIDIAGINTERFACE_H

#include <string>
#include "boost/filesystem.hpp"
#include "RTCToolsDLLDefs.h"

using namespace std;

namespace rtctools
{

/**
  * @brief Diagnostics interface with static functions for handling diagnistigs messages.
  */
class RTCTOOLS_DLL_API piDiagInterface
{
	public:
		/**
		 * @brief Initialize the interface.
		 */
		static void initializeRtcDiagWriter(boost::filesystem::path workDir = boost::filesystem::path("."), bool eventCodeOutput = false);

		/**
		 * @brief Get logging level.
		 */
		static int getLogLevel();

		/**
		 * @brief Get logging level.
		 */
		static bool getIncludeError();

		/**
		 * @brief Set logging level.
		 */
		static void setLogLevel(int logLevel);

		/**
		 * @brief Set logging level.
		 */
		static void setEventCode(bool eventCode);

		static void setFlush(bool flag);

		/**
		 * @brief Add line to the diag model.
		 */
		static void addLine(int level, string description, string eventCode = "");

		/**
		 * @brief Writes diag model to file.
		 */
		static void write(void);

		/**
		 * @brief free memory used by diag model
		 */
		static void freeRtcDiagWriter(void);
};

}
#endif //PIDIAGINTERFACE_H
