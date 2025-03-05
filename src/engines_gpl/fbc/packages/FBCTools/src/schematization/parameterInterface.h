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
 * @date 2010, 2011
 */

#ifndef PARAMETER_INTERFACE_H
#define PARAMETER_INTERFACE_H

#include "rtcToolsEnums.h"
#include <vector>
#include <map>
#include <vector>
#include <string>

#pragma warning(disable : 4503)

using namespace std;

namespace rtctools
{
namespace schematization
{

class parameterInterface
{
public:
	parameterInterface();
	~parameterInterface() {};

	void addFileContent(string schemaLocation, string filename, fileTypeEnum type, prefixEnum prefix);
	bool getBoolParameter(string id);
	int getIntParameter(string id);
	string getStringParameter(string id);
	int getNDblParameter();
	double getDblParameter(string id);
	void setDblParameter(string id, double value);
	void setDblParameters(int nPar, string* id, double* value);
	map<string,vector<double> > getTableRowMap(string id);
	map<string,vector<string> > getTableColStrMap(string id);
	map<string,vector<double> > getTableColDblMap(string id);
	void write(string filename);

private:
	map<string,bool> boolParMap;
	map<string,int> intParMap;
	map<string,double> dblParMap;
	map<string,string> stringParMap;

	map<string,map<string,vector<double> > > tableRowMap;
	map<string,map<string,vector<string> > > tableColStrMap;
	map<string,map<string,vector<double> > > tableColDblMap;
};

} // end namespace schematization
} // end namespace rtctools

#endif /* PARAMETER_INTERFACE_H */
