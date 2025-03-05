// Copyright (C) 2015 Deltares
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
 * @date 2015
 */

#ifndef STRINGCONTAINER_H
#define STRINGCONTAINER_H

#include <string>
#include <map>


using namespace std;

namespace rtctools
{
namespace timeseries
{

/**
  * @brief The class contains the time series and supplies interfaces to them.
  */
class stringContainer
{
	private:
		map<string,string> container;

	public:
		stringContainer();
		~stringContainer() {};

		void addString(int a, int b, int c, string s);
		void setString(int a, int b, int c, string s);
		string getString(int a, int b, int c) const;
};

} // end namespace timeseries
} // end namespace rtctools

#endif /* STRINGCONTAINER_H */
