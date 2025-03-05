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

#include "parameterInterface.h"
#include "piDiagInterface.h"
#include "dataBinding/pi_modelparameters.hxx"
#include "dataBinding/treeVector.hxx"
#include "utilities/utils.h"
#include <iostream>
#include <fstream>
#include <stdexcept>
#ifdef _WIN32
#include <codecvt>
#endif

using namespace openda;
using namespace rtctools::schematization;
using namespace rtctools::utilities;
using namespace std;
using namespace fews;
using namespace PI;


parameterInterface::parameterInterface() 
{
	// allocate vectors
	boolParMap = map<string,bool>();
	intParMap = map<string,int>();
	dblParMap = map<string,double>();
	stringParMap = map<string,string>();
}

void parameterInterface::addFileContent(string schemaDir, string filename, fileTypeEnum type, prefixEnum prefix) 
{
	piDiagInterface::addLine(4, "parameter interface: start reading parameter file '" + filename + "'");

	if (type==TREEVECTOR) {

		// OpenMI treeVector
		::xml_schema::Properties properties;
		properties.schema_location("http://www.openda.org", utils::xsd_filename(utils::getAbsoluteFilename(schemaDir, "treeVector.xsd")));
		auto_ptr<TreeVectorFileXML> root;
		try {
			root = parseTreeVectorFile(filename, 0, properties);
		} catch (const xml_schema::Exception &e) {
			cout << e << endl;
			throw runtime_error("error parsing rtcParameterConfig.xml (treeVector.xsd) file");
		}

		// read parameters from file
		for (int i=0; i<(int)root->getTreeVector().getTreeVectorLeaf().size(); i++) {

			string id = root->getTreeVector().getTreeVectorLeaf()[i].getId().get();
			string s = root->getTreeVector().getTreeVectorLeaf()[i].getVector().data();
			double value = numeric_limits<double>::quiet_NaN();
			if ((s.compare("1.#QNAN")!=0) && (s.compare("NaN")!=0)) value = atof(s.c_str());
			dblParMap.insert(pair<string,double>(id, value));
		}

	} else if (type==PIMODELPARAMETERS) {

		::xml_schema::Properties properties;
		properties.schema_location("http://www.wldelft.nl/fews/PI", utils::xsd_filename(utils::getAbsoluteFilename(schemaDir, "pi_modelparameters.xsd")));
		auto_ptr<ModelParametersComplexType> root;
		try {
			root = parseParameters(filename, 0, properties);
		} catch (const xml_schema::Exception &e) {
			cout << e << endl;
			throw runtime_error("error parsing rtcParameterConfig.xml (pi_modelparameters.xsd) file");
		}

		for (int i=0; i<(int)root->getGroup().size(); i++) {

			// optional prefix from locationId to make the parameterId unique
			string pre = "";
			if (prefix==PREFIX_LOCATIONID && root->getGroup()[i].getLocationId().present()) {
				pre = root->getGroup()[i].getLocationId().get() + "_";
			}

			for (int j=0; j<(int)root->getGroup()[i].getParameter().size(); j++) {

				if (root->getGroup()[i].getParameter()[j].getBoolValue().present()) {

					string id = pre + root->getGroup()[i].getParameter()[j].getId();
					bool value = root->getGroup()[i].getParameter()[j].getBoolValue().get().compare("true")==0;
					boolParMap.insert(pair<string, bool>(id, value));

				} else if (root->getGroup()[i].getParameter()[j].getIntValue().present()) {

					string id = pre + root->getGroup()[i].getParameter()[j].getId();
					string s = root->getGroup()[i].getParameter()[j].getIntValue().get();
					int value = (int)atof(root->getGroup()[i].getParameter()[j].getIntValue().get().c_str());
					intParMap.insert(pair<string, int>(id, value));

				} else if (root->getGroup()[i].getParameter()[j].getDblValue().present()) {

					string id = pre + root->getGroup()[i].getParameter()[j].getId();
					string s = root->getGroup()[i].getParameter()[j].getDblValue().get();
					double value = atof(root->getGroup()[i].getParameter()[j].getDblValue().get().c_str());
					dblParMap.insert(pair<string, double>(id, value));

				} else if (root->getGroup()[i].getParameter()[j].getStringValue().present()) {

					string id = pre + root->getGroup()[i].getParameter()[j].getId();
					string value = root->getGroup()[i].getParameter()[j].getStringValue().get();
					stringParMap.insert(pair<string, string>(id, value));

				} else if (root->getGroup()[i].getParameter()[j].getTable().present()) {
					
					string id = pre + root->getGroup()[i].getParameter()[j].getId();
					ModelParameterComplexType::TableType t = root->getGroup()[i].getParameter()[j].getTable().get();
					map<string,vector<double> > rowMap = map<string,vector<double> >();
					map<string,vector<double> > colDblMap = map<string,vector<double> >();
					map<string,vector<string> > colStrMap = map<string,vector<string> >();

					int nRow = (int)t.getRow().size();
					vector<double> dblData(nRow);
					vector<string> strData(nRow);

					// A
					for (int i=0; i<nRow; i++) {
						strData[i] = string(t.getRow()[i].getA());
						dblData[i] = utils::string2double(strData[i]);
					}
					if (t.getColumnTypes().present() && t.getColumnTypes().get().getA().compare("string")==0) {
						colStrMap[string(t.getColumnIds().get().getA())] = strData;
					} else {
						colDblMap[string(t.getColumnIds().get().getA())] = dblData;
					}

					// B
					if (t.getColumnIds().get().getB().present()) {
						for (int i=0; i<nRow; i++) {
							strData[i] = string(t.getRow()[i].getB().get());
							dblData[i] = utils::string2double(strData[i]);
						}
						if (t.getColumnTypes().present() && t.getColumnTypes().get().getB().get().compare("string")==0) {
							colStrMap[t.getColumnIds().get().getB().get()] = strData;
						} else {
							colDblMap[t.getColumnIds().get().getB().get()] = dblData;
						}
					}

					// C
					if (t.getColumnIds().get().getC().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getC().get());
						colDblMap[t.getColumnIds().get().getC().get()] = dblData;
					}

					// D
					if (t.getColumnIds().get().getD().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getD().get());
						colDblMap[t.getColumnIds().get().getD().get()] = dblData;
					}

					// E
					if (t.getColumnIds().get().getE().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getE().get());
						colDblMap[t.getColumnIds().get().getE().get()] = dblData;
					}

					// F
					if (t.getColumnIds().get().getF().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getF().get());
						colDblMap[t.getColumnIds().get().getF().get()] = dblData;
					}

					// G
					if (t.getColumnIds().get().getG().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getG().get());
						colDblMap[t.getColumnIds().get().getG().get()] = dblData;
					}

					// H
					if (t.getColumnIds().get().getH().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getH().get());
						colDblMap[t.getColumnIds().get().getH().get()] = dblData;
					}

					// I
					if (t.getColumnIds().get().getI().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getI().get());
						colDblMap[t.getColumnIds().get().getI().get()] = dblData;
					}

					// J
					if (t.getColumnIds().get().getJ().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getJ().get());
						colDblMap[t.getColumnIds().get().getJ().get()] = dblData;
					}

					// K
					if (t.getColumnIds().get().getK().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getK().get());
						colDblMap[t.getColumnIds().get().getK().get()] = dblData;
					}

					// L
					if (t.getColumnIds().get().getL().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getL().get());
						colDblMap[t.getColumnIds().get().getL().get()] = dblData;
					}

					// M
					if (t.getColumnIds().get().getM().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getM().get());
						colDblMap[t.getColumnIds().get().getM().get()] = dblData;
					}

					// N
					if (t.getColumnIds().get().getN().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getN().get());
						colDblMap[t.getColumnIds().get().getN().get()] = dblData;
					}

					// O
					if (t.getColumnIds().get().getO().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getO().get());
						colDblMap[t.getColumnIds().get().getO().get()] = dblData;
					}

					// P
					if (t.getColumnIds().get().getP().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getP().get());
						colDblMap[t.getColumnIds().get().getP().get()] = dblData;
					}

					// Q
					if (t.getColumnIds().get().getQ().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getQ().get());
						colDblMap[t.getColumnIds().get().getQ().get()] = dblData;
					}

					// R
					if (t.getColumnIds().get().getR().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getR().get());
						colDblMap[t.getColumnIds().get().getR().get()] = dblData;
					}

					// S
					if (t.getColumnIds().get().getS().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getS().get());
						colDblMap[t.getColumnIds().get().getS().get()] = dblData;
					}

					// T
					if (t.getColumnIds().get().getT().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getT().get());
						colDblMap[t.getColumnIds().get().getT().get()] = dblData;
					}

					// U
					if (t.getColumnIds().get().getU().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getU().get());
						colDblMap[t.getColumnIds().get().getU().get()] = dblData;
					}

					// V
					if (t.getColumnIds().get().getV().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getV().get());
						colDblMap[t.getColumnIds().get().getV().get()] = dblData;
					}

					// W
					if (t.getColumnIds().get().getW().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getW().get());
						colDblMap[t.getColumnIds().get().getW().get()] = dblData;
					}

					// X
					if (t.getColumnIds().get().getX().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getX().get());
						colDblMap[t.getColumnIds().get().getX().get()] = dblData;
					}

					// Y
					if (t.getColumnIds().get().getY().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getY().get());
						colDblMap[t.getColumnIds().get().getY().get()] = dblData;
					}

					// Z
					if (t.getColumnIds().get().getZ().present()) {
						for (int i=0; i<nRow; i++) dblData[i] = utils::string2double(t.getRow()[i].getZ().get());
						colDblMap[t.getColumnIds().get().getZ().get()] = dblData;
					}

					tableColStrMap[id] = colStrMap;
					tableColDblMap[id] = colDblMap;
					int nCol = (int)colStrMap.size() + (int)colDblMap.size();

					// column meta data
					for (int i=0; i<(int)t.getColumnMetaData().size(); i++) {
						string id = t.getColumnMetaData()[i].getId().get();
						rowMap[id] = vector<double>(nCol);
						rowMap[id][0] = utils::string2double(t.getColumnMetaData()[i].getA());
						rowMap[id][1] = utils::string2double(t.getColumnMetaData()[i].getB().get());
						if (nCol>2) rowMap[id][2] = utils::string2double(t.getColumnMetaData()[i].getC().get());
						if (nCol>3) rowMap[id][3] = utils::string2double(t.getColumnMetaData()[i].getD().get());
						if (nCol>4) rowMap[id][4] = utils::string2double(t.getColumnMetaData()[i].getE().get());
						if (nCol>5) rowMap[id][5] = utils::string2double(t.getColumnMetaData()[i].getF().get());
						if (nCol>6) rowMap[id][6] = utils::string2double(t.getColumnMetaData()[i].getG().get());
						if (nCol>7) rowMap[id][7] = utils::string2double(t.getColumnMetaData()[i].getH().get());
						if (nCol>8) rowMap[id][8] = utils::string2double(t.getColumnMetaData()[i].getI().get());
						if (nCol>9) rowMap[id][9] = utils::string2double(t.getColumnMetaData()[i].getJ().get());
						if (nCol>10) rowMap[id][10] = utils::string2double(t.getColumnMetaData()[i].getK().get());
						if (nCol>11) rowMap[id][11] = utils::string2double(t.getColumnMetaData()[i].getL().get());
						if (nCol>12) rowMap[id][12] = utils::string2double(t.getColumnMetaData()[i].getM().get());
						if (nCol>13) rowMap[id][13] = utils::string2double(t.getColumnMetaData()[i].getN().get());
						if (nCol>14) rowMap[id][14] = utils::string2double(t.getColumnMetaData()[i].getO().get());
						if (nCol>15) rowMap[id][15] = utils::string2double(t.getColumnMetaData()[i].getP().get());
						if (nCol>16) rowMap[id][16] = utils::string2double(t.getColumnMetaData()[i].getQ().get());
						if (nCol>17) rowMap[id][17] = utils::string2double(t.getColumnMetaData()[i].getR().get());
						if (nCol>18) rowMap[id][18] = utils::string2double(t.getColumnMetaData()[i].getS().get());
						if (nCol>19) rowMap[id][19] = utils::string2double(t.getColumnMetaData()[i].getT().get());
						if (nCol>20) rowMap[id][20] = utils::string2double(t.getColumnMetaData()[i].getU().get());
						if (nCol>21) rowMap[id][21] = utils::string2double(t.getColumnMetaData()[i].getV().get());
						if (nCol>22) rowMap[id][22] = utils::string2double(t.getColumnMetaData()[i].getW().get());
						if (nCol>23) rowMap[id][23] = utils::string2double(t.getColumnMetaData()[i].getX().get());
						if (nCol>24) rowMap[id][24] = utils::string2double(t.getColumnMetaData()[i].getY().get());
						if (nCol>25) rowMap[id][25] = utils::string2double(t.getColumnMetaData()[i].getZ().get());
					}
					tableRowMap[id] = rowMap;
				}
			}
		}
	}

	piDiagInterface::addLine(4, "parameter interface: file '" + filename + "' read");
}

void parameterInterface::setDblParameter(string id, double value)
{ 
	dblParMap[id] = value;
}

void parameterInterface::setDblParameters(int nPar, string* id, double* value)
{
	for (int i=0; i<nPar; i++) {
		dblParMap[id[i]] = value[i];
	}
}

bool parameterInterface::getBoolParameter(string id)
{
	bool v;

	if (id[0]=='$') {
		if ((id.size()>2) && (id[id.size()-1]=='$')) {
			if (boolParMap.find(id.substr(1, id.size()-2)) == boolParMap.end()) throw runtime_error(string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
			v = boolParMap[id.substr(1, id.size()-2)];
		} 
	} else {
		v = id.compare("true")==0;
	}

	return v;
}

int parameterInterface::getIntParameter(string id)
{
	int v;

	if (id[0]=='$') {
		if ((id.size()>2) && (id[id.size()-1]=='$')) {
			if (intParMap.find(id.substr(1, id.size()-2)) == intParMap.end()) throw runtime_error(string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
			v = intParMap[id.substr(1, id.size()-2)];
		} 
	} else {
		v = (int)atof(id.c_str());
	}

	return v;
}

string parameterInterface::getStringParameter(string id)
{
	string v;

	if (id[0]=='$') {
		if ((id.size()>2) && (id[id.size()-1]=='$')) {
			if (stringParMap.find(id.substr(1, id.size()-2)) == stringParMap.end()) throw runtime_error(string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
			v = stringParMap[id.substr(1, id.size()-2)];
		} 
	} else {
		v = id;
	}

	return v;
}

int parameterInterface::getNDblParameter()
{
	return (int)dblParMap.size();
}

double parameterInterface::getDblParameter(string id)
{
	double v;

	if (id[0]=='$') {
		if ((id.size()>2) && (id[id.size()-1]=='$')) {
			if (dblParMap.find(id.substr(1, id.size()-2)) == dblParMap.end()) throw runtime_error(string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
			v = dblParMap[id.substr(1, id.size()-2)];
		} 
	} else {
		v = atof(id.c_str());
	}

	return v;
}

map<string,vector<double> > parameterInterface::getTableRowMap(string id)
{
	if (tableRowMap.find(id)==tableRowMap.end()) throw runtime_error(
		string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
	return tableRowMap[id];
}

map<string,vector<string> > parameterInterface::getTableColStrMap(string id)
{
	if (tableColStrMap.find(id)==tableColStrMap.end()) throw runtime_error(
		string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
	return tableColStrMap[id];
}

map<string,vector<double> > parameterInterface::getTableColDblMap(string id)
{
	if (tableColDblMap.find(id)==tableColDblMap.end()) throw runtime_error(
		string("parameter interface: external parameter Id '" + id + "'not available in file").c_str());
	return tableColDblMap[id];
}

void parameterInterface::write(string filename)
{
	// xml output
	wofstream xmlFile;
#ifdef _WIN32
	xmlFile.imbue(std::locale(std::locale::empty(), new std::codecvt_utf8<wchar_t, 0x10ffff, std::generate_header>));
#endif
	xmlFile.open(filename.c_str(), ios::out | ios::trunc);

	xmlFile << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
	xmlFile << "<treeVectorFile xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.openda.org\" xsi:schemaLocation=\"http://www.openda.org ../../../xsd/treeVector.xsd\">" << endl;
	xmlFile << "  <treeVector>" << endl;

	for (map<string,double>::iterator iter=dblParMap.begin(); iter!=dblParMap.end(); iter++) {
		xmlFile << "    <treeVectorLeaf id=\"" << iter->first.c_str() << "\">" << endl;
		xmlFile << "      <vector>" << iter->second << "</vector>" << endl;
		xmlFile << "    </treeVectorLeaf>" << endl;
	}

	xmlFile << "  </treeVector>" << endl;
	xmlFile << "</treeVectorFile>" << endl;

	// close files
	xmlFile.close();
}
