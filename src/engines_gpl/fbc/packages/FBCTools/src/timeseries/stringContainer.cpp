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


#include "stringContainer.h"
#include <sstream>

using namespace std;
using namespace rtctools::timeseries;


stringContainer::stringContainer()
{
	container = map<string,string>();
}

void stringContainer::setString(int a, int b, int c, string s) 
{
	stringstream ss;
	ss << a << "," << b << "," << c;

	if (s.size()>0) {
		container[ss.str()] = s;
	}
}

void stringContainer::addString(int a, int b, int c, string s) 
{
	stringstream ss;
	ss << a << "," << b << "," << c;

	if (s.size()>0) {
		setString(a, b, c, container[ss.str()]+s);
	}
}

string stringContainer::getString(int a, int b, int c) const
{
	stringstream ss;
	ss << a << "," << b << "," << c;
    
    map<string,string>::const_iterator itr = container.find(ss.str());
	if (itr!=container.end()) {
		return itr->second;
	} else {
		return string("");
	}
}

	
