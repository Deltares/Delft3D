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

#ifndef UTILS_H
#define UTILS_H

#include <fstream>
#include <sstream>
#include <string>

#include "boost/filesystem.hpp"

using namespace std;

enum timeSteppingScheme{FORWARDEULER, THETA};
enum relationalOperator{GREATER, GREATEREQUAL, EQUAL, UNEQUAL, LESSEQUAL, LESS};
enum logicalOperator{AND, OR, XOR};

namespace rtctools
{
namespace utilities
{
	template <class T> string to_string(const T& t)
	{
		std::stringstream ss;
		ss << t;
		return ss.str();
	};

class utils
{
public:

	static long long date2time(const char *dateCharArray, const char *timeCharArray);
	static long long date2time(int *date);
	static void time2date(long long time, int *date);
	static int time2month(long long time);
	static char *time2datestring(long long time, char *buffer);
	static char *time2timestring(long long time, char *buffer);
	static char *time2datetimestring(long long time, char *buffer);
	static int getDayOfYear(long long time);

	static int getIndex(string myId, int nSeries, string *idVec);
	static int **imat(long nRow, long nCol);
	static void free_imat(int **m);
	static double **dmat(long nRow, long nCol);
	static void free_dmat(double **m);
	static int ***iten(long nRow, long nCol, long nDep);
	static void free_iten(double ***t);
	static double ***dten(long nRow, long nCol, long nDep);
	static void free_dten(double ***t);

	static bool fileAvailable(string filename);
	static string getAbsoluteFilename(string filename);
	static string getAbsoluteFilename(boost::filesystem::path path, string filename);
	static void convert2CStr(string str, char *cstr, int len);
	static string xsd_filename(string filename);
	static double string2double(string text);
   static bool fileExists(string filename);
   static void deleteFile(string filename);
};

struct metadata {
	string id;
	string label;
	int flag;
	string message;
	int ensemble;
	int step;
};

} // end namespace utilities
} // end namespace rtctools

#endif // UTILS_H
