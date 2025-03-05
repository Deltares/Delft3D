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

#ifdef _WIN32
#include <codecvt>
#endif

#include "stateInterface.h"
#include "piDiagInterface.h"
#include "dataBinding/treeVector.hxx"
#include "timeSeriesSparseTensor.h"
#include <stdexcept>

#define isnan(a) ((a)!=(a))

#ifdef _MSC_VER
#define sscanf sscanf_s
#endif

const string STATEINTERFACE_CODE = "RTCTools.timeseries.stateInterface";

using namespace openda;
using namespace timeseries;
using namespace utilities;
using namespace std;


stateInterface::stateInterface(boost::filesystem::path schemaDir, boost::filesystem::path workDir, timeSeriesTensorInterface *tsTensor)
{
    this->schemaDir = schemaDir;
    this->workDir = workDir;
    this->tsTensor = tsTensor;
}

stateInterface::~stateInterface() {}

void stateInterface::read() 
{
    piDiagInterface::addLine(4, "stateInterface: start reading state file 'state_import.xml'", STATEINTERFACE_CODE);

    ::xml_schema::Properties properties;
    properties.schema_location("http://www.openda.org", utils::xsd_filename(utils::getAbsoluteFilename(schemaDir, "treeVector.xsd")));
    auto_ptr<TreeVectorFileXML> root;
    try 
    {
        root = parseTreeVectorFile(utils::getAbsoluteFilename(workDir, "state_import.xml"), 0, properties);
    } 
    catch (const xml_schema::Exception &e) 
    {
        cout << e << endl;
        throw runtime_error("error parsing state_import.xml file");
    }

    // check number of states in file
    int nSeriesFile = (int)root->getTreeVector().getTreeVectorLeaf().size();
    if (nSeriesFile != tsTensor->getNSeries()) 
    {
        piDiagInterface::addLine(2, "stateInterface: number of states in state file is not consistent with schematization", STATEINTERFACE_CODE);
    }

    for (int i=0; i<nSeriesFile; i++) 
    {
        // get state ID
        string stateId = root->getTreeVector().getTreeVectorLeaf()[i].getId().get();
        int indx;
        try 
        {
            indx = tsTensor->getScalarIndex(stateId);
        }
        catch (...) 
        {
            piDiagInterface::addLine(2, "stateInterface: id = '" + stateId + "' not found in time series model, skipping state value", STATEINTERFACE_CODE);
            continue;
        }

      // get state value
      auto stateValueString = root->getTreeVector().getTreeVectorLeaf()[i].getVector();
      double stateValue_m1 = numeric_limits<double>::quiet_NaN();
      double stateValue_m2 = numeric_limits<double>::quiet_NaN();
      double stateValue_m3 = numeric_limits<double>::quiet_NaN();


      if (stateValueString.compare("1.#QNAN") == 0) 
      {
         piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' is 1.#QNAN", STATEINTERFACE_CODE);
      }
      else if (stateValueString.compare("NaN") == 0) 
      {
         piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' is NaN", STATEINTERFACE_CODE);
      }
      else 
      {
         sscanf(stateValueString.c_str(), "%lf %lf %lf", &stateValue_m1, &stateValue_m2, &stateValue_m3);

         if (!isnan(stateValue_m3)) 
         {
            this->tsTensor->setValue(0, -2, indx, stateValue_m3);
            piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' found in time series model, state value = '" + stateValueString + "' replaced.", STATEINTERFACE_CODE);
         }
         else 
         {
            piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' found in time series model, state value = '" + stateValueString + "' not recognized as number.", STATEINTERFACE_CODE);
         }

         if (!isnan(stateValue_m2)) {
            this->tsTensor->setValue(0, -1, indx, stateValue_m2);
            piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' found in time series model, state value = '" + stateValueString + "' replaced.", STATEINTERFACE_CODE);
         }
         else 
         {
            piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' found in time series model, state value = '" + stateValueString + "' not recognized as number.", STATEINTERFACE_CODE);
         }

         if (!isnan(stateValue_m1)) {
            this->tsTensor->setValue(0, 0, indx, stateValue_m1);
            piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' found in time series model, state value = '" + stateValueString + "' replaced.", STATEINTERFACE_CODE);
         }
         else 
         {
            piDiagInterface::addLine(4, "stateInterface: id = '" + stateId + "' found in time series model, state value = '" + stateValueString + "' not recognized as number.", STATEINTERFACE_CODE);
         }
      }
   }

    // check for time series for which no state values have been found
    double *state = tsTensor->getState(0,0);
    int noState = 0;
    for (int i=0; i<tsTensor->getNSeries(); i++) 
    {
        if (state[i]!=state[i]) noState++;
    }
    stringstream ss;
    if (noState>0) 
    {
        stringstream ss;
        ss << "stateInterface: state file includes " << noState << " NaN values.";
        piDiagInterface::addLine(4, ss.str(), STATEINTERFACE_CODE);
    }

    // copy states to all other ensembles, if available
    for (int i=1; i<tsTensor->getNEnsemble(); i++) 
    {
        for (int j=0; j<tsTensor->getNSeries(); j++) 
        {
            tsTensor->setValue(i, 0, j, state[j]);
        }
    }

    piDiagInterface::addLine(4, "stateInterface: state file 'state_import.xml' read", STATEINTERFACE_CODE);
}

void stateInterface::writeData(string pathAndFilename, int tIndex) {

   if (tIndex < 0) tIndex = tsTensor->getNTimeStep() - 1;

   // state export of first ensemble member only
   wofstream xmlFile;
#ifdef _WIN32
   xmlFile.imbue(std::locale(std::locale::empty(), new std::codecvt_utf8<wchar_t, 0x10ffff, std::generate_header>));
#endif
   xmlFile.open(pathAndFilename.c_str(), ios::out);

   xmlFile << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
   xmlFile << "<treeVectorFile xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.openda.org\" xsi:schemaLocation=\"http://www.openda.org ../../../xsd/treeVector.xsd\">" << endl;
   xmlFile << "  <treeVector>" << endl;

   int numTimeStepsToSave = 2;
   double *vals = tsTensor->getState(0, tIndex - numTimeStepsToSave);
   int numTimeSeries = tsTensor->getNSeries();
   for (int i = 0; i < numTimeSeries ; i++)
   {
      xmlFile << "    <treeVectorLeaf id=\"" << tsTensor->getSeriesIDs()[i].c_str() << "\">" << endl;

      for (int j = numTimeStepsToSave; j >= 0; --j)
      {
         double val = vals[i + j * numTimeSeries];
         if (val == val)
         {
            xmlFile.precision(12);
            if (j == numTimeStepsToSave)
               xmlFile << "      <vector>" << val << " ";
            else if (j == 0)
               xmlFile << val << " </vector>" << endl;
            else
               xmlFile << " " << val << " ";
         }
         else
         {
            if (j == numTimeStepsToSave)
               xmlFile << "      <vector>NaN ";
            else if (j == 0)
               xmlFile << "NaN" << " </vector>" << endl;
            else
               xmlFile << " NaN ";
         }
      }
      xmlFile << "    </treeVectorLeaf>" << endl;
   }

   xmlFile << "  </treeVector>" << endl;
   xmlFile << "</treeVectorFile>" << endl;
   xmlFile.close();
}

void stateInterface::write(int tIndex) {

    char buffer[50];

    // --- state data ------------------------------------------------------

    piDiagInterface::addLine(4, "stateInterface: start writing state file 'state_export.xml'", STATEINTERFACE_CODE);
    writeStateExport(tIndex);
    piDiagInterface::addLine(4, "stateInterface: state file 'state_export.xml' written", STATEINTERFACE_CODE);

    // --- meta data for DelftFEWS -----------------------------------------

    ofstream xmlFile;

    piDiagInterface::addLine(4, "stateInterface: start writing meta data file for states 'statePI.xml'", STATEINTERFACE_CODE);

    xmlFile.open(utils::getAbsoluteFilename(workDir, "statePI.xml").c_str(), ios::out);

    xmlFile << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
    xmlFile << "<State xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.wldelft.nl/fews/PI\" xsi:schemaLocation=\"http://www.wldelft.nl/fews/PI ../../../../xsd/pi_state.xsd\" version=\"1.2\">" << endl;
    xmlFile << "  <stateId>warm</stateId>" << endl;
    xmlFile << "  <timeZone>0.0</timeZone>" << endl;

    utils::time2datestring(tsTensor->getEndTime(), buffer);
    xmlFile << "  <dateTime date=\"" << buffer;
    utils::time2timestring(tsTensor->getEndTime(), buffer);
    xmlFile << "\" time=\"" << buffer << "\"/>" << endl;

    xmlFile << "  <stateLoc type=\"file\">" << endl;
    xmlFile << "    <readLocation>state_import.xml</readLocation>" << endl;
    xmlFile << "    <writeLocation>state_export.xml</writeLocation>" << endl;
    xmlFile << "  </stateLoc>" << endl;

    xmlFile << "</State>" << endl;

    xmlFile.close();
}

void stateInterface::writeStateExport(int tIndex, bool isSnapshot) 
{
   //writing a snapshot during the simulation
   if (isSnapshot) 
   {
      if (tIndex > 0)
      {
         // format rtc_20150101_HHMMSS
         char buffer[30];
         std::string absoluteFilePath = utils::getAbsoluteFilename(workDir, "rtc");
         utils::time2datestring(tsTensor->getTimes()[tIndex], buffer);
         string str(buffer);
         str.erase(std::remove_if(str.begin(), str.end(), [](char ch) { return ch == '-'; }), str.end());
         absoluteFilePath = absoluteFilePath + "_" + str ;
         utils::time2timestring(tsTensor->getTimes()[tIndex], buffer);
         str = buffer;
         str.erase(std::remove_if(str.begin(), str.end(), [](char ch) { return ch == ':'; }), str.end());
         absoluteFilePath = absoluteFilePath + "_" + str + ".xml";
         writeData(absoluteFilePath, tIndex);
         // always overwrite the state export
         auto stateExportName = utils::getAbsoluteFilename(workDir, "state_export.xml");
         if (utils::fileExists(stateExportName))
         {
            utils::deleteFile(stateExportName);
         }
         writeData(stateExportName, tIndex);
      }
   }
   //writing at the end of the simulation
   else
   {
      writeData(utils::getAbsoluteFilename(workDir, "state_export.xml"), tIndex);
   }
}