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


#if _MSC_VER && _MSC_VER < 1900 
#define snprintf sprintf_s
#endif
 
#include "timeseries/csvInterface.h"
#include "utilities/utils.h"
#include "piDiagInterface.h"

using namespace std;
using namespace timeseries;
using namespace utilities;


template<typename CharT>
class DecimalSeparator : public std::numpunct<CharT>
{
public:
    DecimalSeparator(CharT Separator) : m_Separator(Separator) {}

protected:
    CharT do_decimal_point()const { return m_Separator; }

private:
    CharT m_Separator;
};

csvInterface::csvInterface(timeSeriesTensorInterface *tsTensor, boost::filesystem::path workDir, char decimalSeparator, char delimiter, bool adjointOutput)
	: tsTensor(tsTensor), workDir(workDir), decimalSeparator(decimalSeparator), delimiter(delimiter), 
	adjointOutput(adjointOutput)
{
	this->tsTensor = tsTensor;
    files = new ofstream[2 * tsTensor->getNEnsemble()]; // always reserve enough space to open files for adjoint mode
}

csvInterface::~csvInterface()
{
    delete [] files;
}

void csvInterface::openFiles() {

	char buffer[2048];

	// csv value output, one file each for each ensemble member
	for (int i=0; i<tsTensor->getNEnsemble(); i++) {
		snprintf(buffer, sizeof(buffer), "timeseries_%04d.csv", i);
		files[i].open((utils::getAbsoluteFilename(workDir, string(buffer))).c_str(), ios::out);
		files[i].imbue(locale(files[i].getloc(), new DecimalSeparator<char>(decimalSeparator)));

		// csv obj output, one file each for each ensemble member
        if (adjointOutput) {
			snprintf(buffer, sizeof(buffer), "timeseries_adjoint_%04d.csv", i);
            files[tsTensor->getNEnsemble() + i].open((utils::getAbsoluteFilename(workDir, string(buffer))).c_str(), ios::out);
        }
	}
}

void csvInterface::closeFiles() {

    char buffer[500];

	for (int i=0; i<tsTensor->getNEnsemble(); i++) {
		snprintf(buffer, sizeof(buffer), "csv interface: CSV output file 'timeseries_%04d.csv' written", i);
		piDiagInterface::addLine(4, string(buffer));
        files[i].close();

        if (adjointOutput) {
			snprintf(buffer, sizeof(buffer), "csv interface: CSV output file 'timeseries_%04d_obj.csv' written", i);
			piDiagInterface::addLine(4, string(buffer));
            files[i + tsTensor->getNEnsemble()].close();
        }
	}
}

void csvInterface::writeFiles(string filename, vector<string> series, double*** tensor)
{
	int nSeries = (int)series.size();
	string* series_str = new string[nSeries];
	for (int i=0; i<nSeries; i++) {
		series_str[i] = series[i];
	}

	writeFiles(filename, nSeries, series_str, tensor);

	delete[] series_str;
}

void csvInterface::writeFiles(string filename, int nSeries, string* series, double*** tensor)
{
	char buffer[500];

	for (int i=0; i<(int)tsTensor->getNEnsemble(); i++) 
    {
		snprintf(buffer, sizeof(buffer), "%s_%04d.csv", filename.c_str(), i);
		write(buffer, nSeries, series, tensor[i]);
		snprintf(buffer, sizeof(buffer), "csv interface: CSV output file '%s_%04d.csv' written", filename.c_str(), i);
		piDiagInterface::addLine(4, string(buffer));
	}
}

void csvInterface::writeFiles(int timeStep) 
{
	try {
		for (int i=0; i<tsTensor->getNEnsemble(); i++) {
		    write(files[i], timeStep, tsTensor->getState(i, timeStep));

			if (adjointOutput) {
				write(files[tsTensor->getNEnsemble() + i], timeStep, tsTensor->getObjTensor()[i][timeStep]);
			}
		}
	}
	catch (exception &e)
	{
		piDiagInterface::addLine(1, "csvInterface::writeFiles(int timeStep) - writing of csv file skipped - " + string(e.what()));
	}

}

void csvInterface::writeFiles(int timeStep, std::vector<string>& additionalTimeSerieNames, std::vector<int>& additionalTimeseries)
{
    try {
        for (int i = 0; i<tsTensor->getNEnsemble(); i++) {
            write(files[i], timeStep, tsTensor->getState(i, timeStep), additionalTimeSerieNames, additionalTimeseries);

            if (adjointOutput) {
                write(files[tsTensor->getNEnsemble() + i], timeStep, tsTensor->getObjTensor()[i][timeStep]);
            }
        }
    }
    catch (exception &e)
    {
        piDiagInterface::addLine(1, "csvInterface::writeFiles(int timeStep) - writing of csv file skipped - " + string(e.what()));
    }
}

void csvInterface::write(char filename[], int n, string* series, double** data) {

	char timeOrDate[30];

	ofstream csvFile;
	csvFile.open((utils::getAbsoluteFilename(workDir, string(filename))).c_str(), ios::out);
	csvFile.imbue(locale(csvFile.getloc(), new DecimalSeparator<char>(decimalSeparator)));

	// header
	csvFile << "date time";
	for (int j=0; j<n; j++) {
		csvFile << delimiter;
		csvFile << series[j];
	}
	csvFile << endl;

	// data
	for (int j=0; j<tsTensor->getNTimeStep(); j++) {

		utils::time2datestring(tsTensor->getTimes()[j], timeOrDate);
		csvFile << timeOrDate << " ";
		utils::time2timestring(tsTensor->getTimes()[j], timeOrDate);
		csvFile << timeOrDate;

		for (int k=0; k<n; k++) 
        {
			csvFile << delimiter;
			// check for NaN
			double value = data[j][k];
			if (value == value) 
            {
				csvFile.precision(12);
				csvFile << value;
			}
		}
		csvFile << endl;
	}

	csvFile.close();
}

void csvInterface::write(ofstream& file, int timeStep, double *data) 
{

    char timeOrDate[30];

    if (timeStep == 0)
    {
        // header
        file << "date time";
        for (int j = 0; j < tsTensor->getNSeries(); j++)
        {
            file << delimiter;
            file << tsTensor->getSeriesIDs()[j];
        }
        file << endl;
    }

    // data
    utils::time2datestring(tsTensor->getTimes()[timeStep], timeOrDate);
    file << timeOrDate << " ";
    utils::time2timestring(tsTensor->getTimes()[timeStep], timeOrDate);
    file << timeOrDate;
    file.precision(12);

    for (int k = 0; k < tsTensor->getNSeries(); k++)
    {
        file << delimiter;
        // check for NaN
        if (data[k]== data[k])
        {
            file << data[k];
        }
    }
    file << endl;

    file.flush(); // output needs to be available right away
}

void csvInterface::write(ofstream& file, int timeStep, double *data, std::vector<string>& additionalTimeSerieNames, std::vector<int>& additionalTimeserie)
{

    char timeOrDate[30];

    if (timeStep == 0)
    {
        // header
        file << "date time";
        for (int j = 0; j < tsTensor->getNSeries(); j++)
        {
            file << delimiter;
            file << tsTensor->getSeriesIDs()[j];
        }
        // additional header infos
        for (int j = 0; j < additionalTimeSerieNames.size(); j++)
        {
            file << delimiter;
            file << additionalTimeSerieNames[j];
        }

        file << endl;
    }

    // data
    utils::time2datestring(tsTensor->getTimes()[timeStep], timeOrDate);
    file << timeOrDate << " ";
    utils::time2timestring(tsTensor->getTimes()[timeStep], timeOrDate);
    file << timeOrDate;
    file.precision(12);

    for (int k = 0; k < tsTensor->getNSeries(); k++)
    {
        file << delimiter;
        // check for NaN
        if (data[k] == data[k])
        {
            file << data[k];
        }
    }

    // additional header infos
    for (int j = 0; j < additionalTimeserie.size(); j++)
    {
        file << delimiter;
        file << additionalTimeserie[j];
    }
    file << endl;
    
    file.flush(); // output needs to be available right away

}

