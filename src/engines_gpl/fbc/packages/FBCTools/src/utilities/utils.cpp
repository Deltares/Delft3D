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

#include "utilities/utils.h"

#include <stdlib.h>
#include <sstream>
#include <string.h>
#include <stdexcept>
#ifdef _MSC_VER
#include <io.h>
#else
#include <sys/io.h>
#endif
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/filesystem.hpp>

#ifdef _MSC_VER
#define access _access
#define strncpy strncpy_s
#endif

#if _MSC_VER && _MSC_VER < 1900 
#define snprintf sprintf_s
#endif

using namespace boost::gregorian;
using namespace boost::posix_time;
using namespace std;
using namespace rtctools::utilities;


// keep the C time reference
const ptime t1(date(1970,1,1));

static ptime time2ptime(long long time)
{
	//ptime t2 = t1 + milliseconds(time);   // Note that this is not working for larger years !!!
	long h = (long)(time/1000/3600);        // This formulation has been verified in years 1900 - 9999
	long long ms = time - 1000*3600*(long long)h;
	ptime t2 = t1 + hours(h) + milliseconds(ms);

	return t2;
}

long long utils::date2time(const char *dateCharArray, const char *timeCharArray)
{
	int myDate[7];

	string dateString = string(dateCharArray);
	string timeString = string(timeCharArray);

	myDate[0] = atoi(dateString.substr(0, 4).c_str());
	myDate[1] = atoi(dateString.substr(5, 7).c_str());
	myDate[2] = atoi(dateString.substr(8, 10).c_str());

	myDate[3] = atoi(timeString.substr(0, 2).c_str());
	myDate[4] = atoi(timeString.substr(3, 5).c_str());
	myDate[5] = atoi(timeString.substr(6, 8).c_str());
	myDate[6] = 0;

	return date2time(myDate);
}

long long utils::date2time(int *dateArray)
{
	date myDate(
		dateArray[0],  // year
		dateArray[1],  // month
		dateArray[2]); // day

	time_duration myDuration(
		dateArray[3],  // hours
		dateArray[4],  // minutes
		dateArray[5],  // seconds
		dateArray[6] * time_duration::ticks_per_second() / 1000);  // milliseconds

	ptime t2(myDate, myDuration);

	return (long long)(t2-t1).total_milliseconds();
}

void utils::time2date(long long time, int *dateArray)
{
	ptime t2 = time2ptime(time);
	
	date myDate = t2.date();
	dateArray[0] = myDate.year();
	dateArray[1] = myDate.month();
	dateArray[2] = myDate.day();

	time_duration myDuration = t2 - ptime(myDate);
	dateArray[3] = myDuration.hours();
	dateArray[4] = myDuration.minutes();
	dateArray[5] = myDuration.seconds();
	dateArray[6] = (int)(1000 * myDuration.fractional_seconds() / time_duration::ticks_per_second());
}

int utils::time2month(long long time)
{
	ptime t2 = time2ptime(time);
	date myDate = t2.date();
	
	return (int)myDate.month();
}

char *utils::time2datestring(long long time, char *buffer)
{
	int dateArray[7];

	time2date(time, dateArray);
	snprintf(buffer, 20, "%04d-%02d-%02d", dateArray[0], dateArray[1], dateArray[2]);

	return buffer;
}

char *utils::time2timestring(long long time, char *buffer)
{
	int dateArray[7];

	time2date(time, dateArray);
	snprintf(buffer, 20, "%02d:%02d:%02d", dateArray[3], dateArray[4], dateArray[5]);

	return buffer;
}

char *utils::time2datetimestring(long long time, char *buffer)
{
	int dateArray[7];

    try {
    	time2date(time, dateArray);
	    snprintf(buffer, 20, "%04d-%02d-%02d %02d:%02d:%02d", dateArray[0], dateArray[1], dateArray[2], dateArray[3], dateArray[4], dateArray[5]);
    } catch (exception e) {
        string res = e.what();
    }

	return buffer;
}

int utils::getDayOfYear(long long time)
{
	ptime t2 = time2ptime(time);

	// check for leap year
	time_duration dt = ptime(date(t2.date().year(),3,1)) - ptime(date(t2.date().year(),2,28));
	bool leap = false;
	if (dt.total_seconds() == 2*24*3600) leap = true;

	// leap year correction, 29 Feb is treated like 28 Feb
	int doy = t2.date().day_of_year();
	if (leap & (doy>=60)) doy--;

	return doy;
}

int utils::getIndex(string myID, int nSeries, string *idVec)
{
	for (int i=0; i<nSeries; i++) {
		if (myID.compare(idVec[i])==0) {
			return i;
		}
	}

	string s = string("getIndex(): index '" + myID + "' is not existing in time series model");
	throw runtime_error(s.c_str());
}

double **utils::dmat(long nRow, long nCol)
{
	// allocate pointer array to rows
	double** m = new double*[nRow];

	// allocate double array and initialize pointer array
	m[0] = new double[nRow*nCol];
	for (int i=1; i<nRow; i++) m[i] = m[i-1]+nCol;

	return m;
}

void utils::free_dmat(double **m)
{
	delete [] m[0];
	delete [] m;
}

int **utils::imat(long nRow, long nCol)
{
	// allocate pointer array to rows
	int** m = new int*[nRow];

	// allocate double array and initialize pointer array
	m[0] = new int[nRow * nCol];
	for (int i=1; i<nRow; i++) m[i] = m[i-1] + nCol;

	return m;
}

void utils::free_imat(int **m)
{
	delete [] m[0];
	delete [] m;
}

double ***utils::dten(long nRow, long nCol, long nDep)
{
	// allocate pointer array to pointers to rows 
	double*** t = new double**[nRow];

	// allocate pointer array to rows and data array and set related pointers
	t[0] = new double*[nRow * nCol];
	t[0][0] = new double[nRow * nCol * nDep];
	for (int j=1; j<nCol; j++) t[0][j] = t[0][j-1] + nDep;
	for (int i=1; i<nRow; i++) {
		t[i] = t[i-1] + nCol;
		t[i][0] = t[i-1][0] + nCol * nDep;
		for (int j=1; j<nCol; j++) t[i][j] = t[i][j-1] + nDep;
	}

	return t;
}

void utils::free_dten(double ***t)
{
	delete [] t[0][0];
	delete [] t[0];
	delete [] t;
}

int ***utils::iten(long nRow, long nCol, long nDep)
{
	// allocate pointer array to pointers to rows 
	int*** t = new int**[nRow];

	// allocate pointer array to rows and data array and set related pointers
	t[0] = new int*[nRow * nCol];
	t[0][0] = new int[nRow * nCol * nDep];
	for (int j=1; j<nCol; j++) t[0][j] = t[0][j-1] + nDep;
	for (int i=1; i<nRow; i++) {
		t[i] = t[i-1] + nCol;
		t[i][0] = t[i-1][0] + nCol * nDep;
		for (int j=1; j<nCol; j++) t[i][j] = t[i][j-1] + nDep;
	}

	return t;
}

void utils::free_iten(double ***t)
{
	delete [] t[0][0];
	delete [] t[0];
	delete [] t;
}

bool utils::fileAvailable(string filename)
{

	return access(filename.c_str(), 0)==0;
}

string utils::getAbsoluteFilename(string filename)
{
	boost::filesystem::path abs_path = boost::filesystem::absolute(filename);
	return abs_path.string();
}

string utils::getAbsoluteFilename(boost::filesystem::path path, string filename)
{
	return (boost::filesystem::absolute(path) /= filename).string();
}

void utils::convert2CStr(string str, char* returnChar, int len)
{
	if (len >= str.size()+1){
#ifdef _MSC_VER
	strncpy(returnChar, str.size()+1, str.c_str(), str.size()+1);
#else
	strncpy(returnChar, str.c_str(), str.size()+1);
#endif
	} else {
	// truncate if size of string is larger than returnbuffer. NO warning given.
#ifdef _MSC_VER
	strncpy(returnChar, len, str.c_str(), len);
#else
	strncpy(returnChar, str.c_str(), len);
#endif

	}
	return;
}

string utils::xsd_filename(string filename)
{
	int pos = (int)filename.find(" ");
	while (pos!=-1) {
		filename.replace(pos, 1, "%20");
		pos = (int)filename.find(" ");
	}
	return filename;
}

double utils::string2double(string text)
{
	double value;
	std::stringstream ss;

    ss << text;
    ss >> value;

	return value;
}

bool utils::fileExists(string filename)
{
   boost::filesystem::path path = boost::filesystem::absolute(filename);
   return boost::filesystem::exists(path);
}

void utils::deleteFile(string filename)
{
   boost::filesystem::path path = boost::filesystem::absolute(filename);
   boost::filesystem::remove(path);
}
