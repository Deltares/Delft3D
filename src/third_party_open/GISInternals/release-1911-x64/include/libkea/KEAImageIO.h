/*
 *  KEAImageIO.h
 *  LibKEA
 *
 *  Created by Pete Bunting on 02/07/2012.
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

#ifndef KEAImageIO_H
#define KEAImageIO_H

#include <iostream>
#include <string>
#include <vector>

#include "H5Cpp.h"

#include "libkea/KEACommon.h"
#include "libkea/KEAException.h"
#include "libkea/KEAAttributeTable.h"
#include "libkea/KEAAttributeTableInMem.h"
#include "libkea/KEAAttributeTableFile.h"

namespace kealib{
        
    class DllExport KEAImageIO
    {
    public:
        KEAImageIO();
                
        void openKEAImageHeader(H5::H5File *keaImgH5File)throw(KEAIOException);
        
        void writeImageBlock2Band(uint32_t band, void *data, uint64_t xPxlOff, uint64_t yPxlOff, uint64_t xSizeOut, uint64_t ySizeOut, uint64_t xSizeBuf, uint64_t ySizeBuf, KEADataType inDataType)throw(KEAIOException);
        void readImageBlock2Band(uint32_t band, void *data, uint64_t xPxlOff, uint64_t yPxlOff, uint64_t xSizeIn, uint64_t ySizeIn, uint64_t xSizeBuf, uint64_t ySizeBuf, KEADataType inDataType)throw(KEAIOException);
        
        void createMask(uint32_t band, uint32_t deflate=KEA_DEFLATE)throw(KEAIOException);
        void writeImageBlock2BandMask(uint32_t band, void *data, uint64_t xPxlOff, uint64_t yPxlOff, uint64_t xSizeOut, uint64_t ySizeOut, uint64_t xSizeBuf, uint64_t ySizeBuf, KEADataType inDataType)throw(KEAIOException);
        void readImageBlock2BandMask(uint32_t band, void *data, uint64_t xPxlOff, uint64_t yPxlOff, uint64_t xSizeIn, uint64_t ySizeIn, uint64_t xSizeBuf, uint64_t ySizeBuf, KEADataType inDataType)throw(KEAIOException);
        bool maskCreated(uint32_t band)throw(KEAIOException);
        
        void setImageMetaData(std::string name, std::string value)throw(KEAIOException);
        std::string getImageMetaData(std::string name)throw(KEAIOException);
        std::vector<std::string> getImageMetaDataNames()throw(KEAIOException);
        std::vector< std::pair<std::string, std::string> > getImageMetaData()throw(KEAIOException);
        void setImageMetaData(std::vector< std::pair<std::string, std::string> > data)throw(KEAIOException);
        
        void setImageBandMetaData(uint32_t band, std::string name, std::string value)throw(KEAIOException);
        std::string getImageBandMetaData(uint32_t band, std::string name)throw(KEAIOException);
        std::vector<std::string> getImageBandMetaDataNames(uint32_t band)throw(KEAIOException);
        std::vector< std::pair<std::string, std::string> > getImageBandMetaData(uint32_t band)throw(KEAIOException);
        void setImageBandMetaData(uint32_t band, std::vector< std::pair<std::string, std::string> > data)throw(KEAIOException);
        
        void setImageBandDescription(uint32_t band, std::string description)throw(KEAIOException);
        std::string getImageBandDescription(uint32_t band)throw(KEAIOException);
        
        void setNoDataValue(uint32_t band, const void *data, KEADataType inDataType)throw(KEAIOException);
        void getNoDataValue(uint32_t band, void *data, KEADataType inDataType)throw(KEAIOException);
        void undefineNoDataValue(uint32_t band)throw(KEAIOException);
        
        
        std::vector<KEAImageGCP*>* getGCPs()throw(KEAIOException);
        void setGCPs(std::vector<KEAImageGCP*> *gcps, std::string projWKT)throw(KEAIOException);
        uint32_t getGCPCount()throw(KEAIOException);
        std::string getGCPProjection()throw(KEAIOException);
        void setGCPProjection(std::string projWKT)throw(KEAIOException);
        
        void setSpatialInfo(KEAImageSpatialInfo *spatialInfo)throw(KEAIOException);
        KEAImageSpatialInfo* getSpatialInfo() throw(KEAIOException);
                
        uint32_t getNumOfImageBands() throw(KEAIOException);
        
        uint32_t getImageBlockSize(uint32_t band) throw(KEAIOException);
        
        KEADataType getImageBandDataType(uint32_t band) throw(KEAIOException);
        
        std::string getKEAImageVersion() throw(KEAIOException);
        
        void setImageBandLayerType(uint32_t band, KEALayerType imgLayerType) throw(KEAIOException);
        KEALayerType getImageBandLayerType(uint32_t band) throw(KEAIOException);
        
        void setImageBandClrInterp(uint32_t band, KEABandClrInterp imgLayerClrInterp) throw(KEAIOException);
        KEABandClrInterp getImageBandClrInterp(uint32_t band) throw(KEAIOException);
        
        void createOverview(uint32_t band, uint32_t overview, uint64_t xSize, uint64_t ySize) throw(KEAIOException);
        void removeOverview(uint32_t band, uint32_t overview) throw(KEAIOException);
        uint32_t getOverviewBlockSize(uint32_t band, uint32_t overview) throw(KEAIOException);
        void writeToOverview(uint32_t band, uint32_t overview, void *data, uint64_t xPxlOff, uint64_t yPxlOff, uint64_t xSizeOut, uint64_t ySizeOut, uint64_t xSizeBuf, uint64_t ySizeBuf, KEADataType inDataType) throw(KEAIOException);
        void readFromOverview(uint32_t band, uint32_t overview, void *data, uint64_t xPxlOff, uint64_t yPxlOff, uint64_t xSizeIn, uint64_t ySizeIn, uint64_t xSizeBuf, uint64_t ySizeBuf, KEADataType inDataType) throw(KEAIOException);
        uint32_t getNumOfOverviews(uint32_t band) throw(KEAIOException);
        void getOverviewSize(uint32_t band, uint32_t overview, uint64_t *xSize, uint64_t *ySize) throw(KEAIOException);
                
        KEAAttributeTable* getAttributeTable(KEAATTType type, uint32_t band) throw(KEAATTException, KEAIOException);
        void setAttributeTable(KEAAttributeTable* att, uint32_t band, uint32_t chunkSize=KEA_ATT_CHUNK_SIZE, uint32_t deflate=KEA_DEFLATE) throw(KEAATTException, KEAIOException);
        bool attributeTablePresent(uint32_t band);
        uint32_t getAttributeTableChunkSize(uint32_t band) throw(KEAIOException);
        
        void close()throw(KEAIOException);

        /**
         * Adds a new image band to the file.
         */
        virtual void addImageBand(const KEADataType dataType, const std::string bandDescrip, const uint32_t imageBlockSize = KEA_IMAGE_CHUNK_SIZE, const uint32_t attBlockSize = KEA_ATT_CHUNK_SIZE, const uint32_t deflate = KEA_DEFLATE) throw(KEAIOException);

        static H5::H5File* createKEAImage(std::string fileName, KEADataType dataType, uint32_t xSize, uint32_t ySize, uint32_t numImgBands, std::vector<std::string> *bandDescrips=NULL, KEAImageSpatialInfo *spatialInfo=NULL, uint32_t imageBlockSize=KEA_IMAGE_CHUNK_SIZE, uint32_t attBlockSize=KEA_ATT_CHUNK_SIZE, int mdcElmts=KEA_MDC_NELMTS, hsize_t rdccNElmts=KEA_RDCC_NELMTS, hsize_t rdccNBytes=KEA_RDCC_NBYTES, double rdccW0=KEA_RDCC_W0, hsize_t sieveBuf=KEA_SIEVE_BUF, hsize_t metaBlockSize=KEA_META_BLOCKSIZE, uint32_t deflate=KEA_DEFLATE)throw(KEAIOException);
        static bool isKEAImage(std::string fileName)throw(KEAIOException);
        static H5::H5File* openKeaH5RW(std::string fileName, int mdcElmts=KEA_MDC_NELMTS, hsize_t rdccNElmts=KEA_RDCC_NELMTS, hsize_t rdccNBytes=KEA_RDCC_NBYTES, double rdccW0=KEA_RDCC_W0, hsize_t sieveBuf=KEA_SIEVE_BUF, hsize_t metaBlockSize=KEA_META_BLOCKSIZE)throw(KEAIOException);
        static H5::H5File* openKeaH5RDOnly(std::string fileName, int mdcElmts=KEA_MDC_NELMTS, hsize_t rdccNElmts=KEA_RDCC_NELMTS, hsize_t rdccNBytes=KEA_RDCC_NBYTES, double rdccW0=KEA_RDCC_W0, hsize_t sieveBuf=KEA_SIEVE_BUF, hsize_t metaBlockSize=KEA_META_BLOCKSIZE)throw(KEAIOException);
        virtual ~KEAImageIO();

    protected:
        /********** STATIC PROTECTED **********/
        /**
         * Converts KEA datatypes to the respective standard HDF5 datatypes.
         */
        static H5::DataType convertDatatypeKeaToH5STD(const KEADataType dataType) throw(KEAIOException);

        /**
         * Converts KEA datatypes to the respective native HDF5 datatypes.
         */
        static H5::DataType convertDatatypeKeaToH5Native( const KEADataType dataType) throw(KEAIOException);

        /**
         * Adds an image band to the specified file. Does NOT flush the file
         * buffer.
         *
         * NOTE: attBlockSize doesn't have any effect at the moment
         */
        static void addImageBandToFile(H5::H5File *keaImgH5File, const KEADataType dataType, const uint32_t xSize, const uint32_t ySize, const uint32_t bandIndex, std::string bandDescrip, const uint32_t imageBlockSize, const uint32_t attBlockSize, const uint32_t deflate) throw(KEAIOException);

        /**
         * Updates the number of image bands in the file metadata. Does NOT
         * flush the file buffer.
         */
        static void setNumImgBandsInFileMetadata(H5::H5File *keaImgH5File, const uint32_t numImgBands) throw(KEAIOException);

        static H5::CompType* createGCPCompTypeDisk() throw(KEAIOException);
        static H5::CompType* createGCPCompTypeMem() throw(KEAIOException);
        
        static std::string readString(H5::DataSet& dataset, H5::DataType strDataType) throw(KEAIOException);
        
        /********** PROTECTED MEMBERS **********/
        bool fileOpen;
        H5::H5File *keaImgFile;
        KEAImageSpatialInfo *spatialInfoFile;
        uint32_t numImgBands;
        std::string keaVersion;
    };
    
}

// returns the current KEA version as a double
extern "C" double get_kealibversion();

#endif




