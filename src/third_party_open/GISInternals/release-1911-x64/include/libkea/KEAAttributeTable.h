/*
 *  KEAAttributeTable.h
 *  LibKEA
 *
 *  Created by Pete Bunting on 20/07/2012.
 *  Copyright 2012 LibKEA. All rights reserved.
 *
 *  This file is part of LibKEA.
 *
 *  Permission is hereby granted, free of charge, to any person 
 *  obtaining a copy of this software and associated documentation 
 *  files (the "Software"), to deal in the Software without restriction, 
 *  including without limitation the rights to use, copy, modify, 
 *  merge, publish, distribute, sublicense, and/or sell copies of the 
 *  Software, and to permit persons to whom the Software is furnished 
 *  to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be 
 *  included in all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
 *  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
 *  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR 
 *  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
 *  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION 
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#ifndef KEAAttributeTable_H
#define KEAAttributeTable_H

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <math.h>

#include "H5Cpp.h"

#include "libkea/KEACommon.h"
#include "libkea/KEAException.h"

namespace kealib{
    
    enum KEAATTType
    {
        kea_att_undefined = 0,
        kea_att_mem = 1,
        kea_att_file = 2
    };
    
    struct KEAATTFeature
    {
        size_t fid;
        std::vector<bool> *boolFields;
        std::vector<int64_t> *intFields;
        std::vector<double> *floatFields;
        std::vector<std::string> *strFields;
        std::vector<size_t> *neighbours;
    };
    
    enum KEAFieldDataType
    {
        kea_att_na = 0,
        kea_att_bool = 1,
        kea_att_int = 2,
        kea_att_float = 3,
        kea_att_string = 4
    };
    
    struct KEAATTField
    {
        std::string name;
        KEAFieldDataType dataType;
        size_t idx;
        std::string usage;
        size_t colNum;
    };
    
    struct KEAAttributeIdx
    {
        char *name;
        unsigned int idx;
        char *usage;
        unsigned int colNum;
    };
    
    /*struct KEAAttString
    {
        char *str;
    };*/
    
    typedef struct 
    {
        size_t length;
        void   *p;
    } VarLenFieldHDF;
    
    class DllExport KEAAttributeTable
    {
    public:
        KEAAttributeTable(KEAATTType keaAttType);
        virtual KEAATTType getKEAATTType() const;
        
        virtual bool getBoolField(size_t fid, const std::string &name) const throw(KEAATTException)=0;
        virtual int64_t getIntField(size_t fid, const std::string &name) const throw(KEAATTException)=0;
        virtual double getFloatField(size_t fid, const std::string &name) const throw(KEAATTException)=0;
        virtual std::string getStringField(size_t fid, const std::string &name) const throw(KEAATTException)=0;
        
        virtual void setBoolField(size_t fid, const std::string &name, bool value) throw(KEAATTException)=0;
        virtual void setIntField(size_t fid, const std::string &name, int64_t value) throw(KEAATTException)=0;
        virtual void setFloatField(size_t fid, const std::string &name, double value) throw(KEAATTException)=0;
        virtual void setStringField(size_t fid, const std::string &name, const std::string &value) throw(KEAATTException)=0;
        
        virtual void setBoolValue(const std::string &name, bool value) throw(KEAATTException);
        virtual void setIntValue(const std::string &name, int64_t value) throw(KEAATTException);
        virtual void setFloatValue(const std::string &name, double value) throw(KEAATTException);
        virtual void setStringValue(const std::string &name, const std::string &value) throw(KEAATTException);
        
        virtual bool getBoolField(size_t fid, size_t colIdx) const throw(KEAATTException)=0;
        virtual int64_t getIntField(size_t fid, size_t colIdx) const throw(KEAATTException)=0;
        virtual double getFloatField(size_t fid, size_t colIdx) const throw(KEAATTException)=0;
        virtual std::string getStringField(size_t fid, size_t colIdx) const throw(KEAATTException)=0;

        // RFC40 methods
        virtual void getBoolFields(size_t startfid, size_t len, size_t colIdx, bool *pbBuffer) const throw(KEAATTException)=0;
        virtual void getIntFields(size_t startfid, size_t len, size_t colIdx, int64_t *pnBuffer) const throw(KEAATTException)=0;
        virtual void getFloatFields(size_t startfid, size_t len, size_t colIdx, double *pfBuffer) const throw(KEAATTException)=0;
        virtual void getStringFields(size_t startfid, size_t len, size_t colIdx, std::vector<std::string> *psBuffer) const throw(KEAATTException)=0;
        virtual void getNeighbours(size_t startfid, size_t len, std::vector<std::vector<size_t>* > *neighbours) const throw(KEAATTException)=0;
        
        virtual void setBoolField(size_t fid, size_t colIdx, bool value) throw(KEAATTException)=0;
        virtual void setIntField(size_t fid, size_t colIdx, int64_t value) throw(KEAATTException)=0;
        virtual void setFloatField(size_t fid, size_t colIdx, double value) throw(KEAATTException)=0;
        virtual void setStringField(size_t fid, size_t colIdx, std::string value) throw(KEAATTException)=0;

        // RFC40 methods
        virtual void setBoolFields(size_t startfid, size_t len, size_t colIdx, bool *pbBuffer) throw(KEAATTException)=0;
        virtual void setIntFields(size_t startfid, size_t len, size_t colIdx, int64_t *pnBuffer) throw(KEAATTException)=0;
        virtual void setFloatFields(size_t startfid, size_t len, size_t colIdx, double *pfBuffer) throw(KEAATTException)=0;
        virtual void setStringFields(size_t startfid, size_t len, size_t colIdx, std::vector<std::string> *papszStrList) throw(KEAATTException)=0;
        virtual void setNeighbours(size_t startfid, size_t len, std::vector<std::vector<size_t>* > *neighbours) throw(KEAATTException)=0;
        
        virtual void setBoolValue(size_t colIdx, bool value) throw(KEAATTException);
        virtual void setIntValue(size_t colIdx, int64_t value) throw(KEAATTException);
        virtual void setFloatValue(size_t colIdx, double value) throw(KEAATTException);
        virtual void setStringValue(size_t colIdx, const std::string &value) throw(KEAATTException);
        
        virtual KEAATTFeature* getFeature(size_t fid) const throw(KEAATTException)=0;
        
        virtual void addAttBoolField(const std::string &name, bool val, std::string usage="") throw(KEAATTException);
        virtual void addAttIntField(const std::string &name, int64_t val, std::string usage="") throw(KEAATTException);
        virtual void addAttFloatField(const std::string &name, double val, std::string usage="") throw(KEAATTException);
        virtual void addAttStringField(const std::string &name, const std::string &val, std::string usage="") throw(KEAATTException);
        virtual void addFields(std::vector<KEAATTField*> *inFields) throw(KEAATTException);
        virtual void addFields(std::vector<KEAATTField> inFields) throw(KEAATTException);
        
        virtual KEAFieldDataType getDataFieldType(const std::string &name) const throw(KEAATTException);
        virtual size_t getFieldIndex(const std::string &name) const throw(KEAATTException);
        virtual KEAATTField getField(const std::string &name) const throw(KEAATTException);
        virtual KEAATTField getField(size_t globalColIdx) const throw(KEAATTException);
        virtual std::vector<std::string> getFieldNames() const;
        virtual bool hasField(const std::string &name) const;
        virtual size_t getNumBoolFields() const;
        virtual size_t getNumIntFields() const;
        virtual size_t getNumFloatFields() const;
        virtual size_t getNumStringFields() const;
        
        virtual size_t getSize() const =0;
        virtual size_t getTotalNumOfCols() const;
        virtual size_t getMaxGlobalColIdx() const;
        virtual void addRows(size_t numRows)=0;
        
        virtual void exportToKeaFile(H5::H5File *keaImg, unsigned int band, unsigned int chunkSize=KEA_ATT_CHUNK_SIZE, unsigned int deflate=KEA_DEFLATE)throw(KEAATTException, KEAIOException)=0;
        virtual void exportToASCII(const std::string &outputFile)throw(KEAATTException, KEAIOException);
        
        virtual void printAttributeTableHeaderInfo();
        
        virtual ~KEAAttributeTable();

        // for cross heap use in Windows
        static void destroyAttributeTable(KEAAttributeTable *pTable);
    protected:
        static H5::CompType* createAttibuteIdxCompTypeDisk() throw(KEAATTException);
        static H5::CompType* createAttibuteIdxCompTypeMem() throw(KEAATTException);
        static H5::CompType* createKeaStringCompTypeDisk() throw(KEAATTException);
        static H5::CompType* createKeaStringCompTypeMem() throw(KEAATTException);
        virtual void addAttBoolField(KEAATTField field, bool val) throw(KEAATTException)=0;
        virtual void addAttIntField(KEAATTField field, int64_t val) throw(KEAATTException)=0;
        virtual void addAttFloatField(KEAATTField field, float val) throw(KEAATTException)=0;
        virtual void addAttStringField(KEAATTField field, const std::string &val) throw(KEAATTException)=0;
        virtual KEAATTFeature* createKeaFeature() throw(KEAATTException);
        virtual void deleteKeaFeature(KEAATTFeature *feat) throw(KEAATTException);
        
        std::map<std::string, KEAATTField> *fields;
        KEAATTType attType;
        size_t numBoolFields;
        size_t numIntFields;
        size_t numFloatFields;
        size_t numStringFields;
        size_t numOfCols;
    };
}


#endif




