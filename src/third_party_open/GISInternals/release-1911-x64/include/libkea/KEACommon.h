/*
 *  KEACommon.cpp
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

#ifndef KEACommon_H
#define KEACommon_H

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "H5Cpp.h"

// mark all exported classes/functions with DllExport to have
// them exported by Visual Studio
#ifndef DllExport
    #ifdef _MSC_VER
        #define DllExport   __declspec( dllexport )
    #else
        #define DllExport
    #endif
#endif

// MSVC 2008 uses different names....
#ifdef _MSC_VER
    #if _MSC_VER >= 1600
        #include <stdint.h>
    #else        
        typedef __int8              int8_t;
        typedef __int16             int16_t;
        typedef __int32             int32_t;
        typedef __int64             int64_t;
        typedef unsigned __int8     uint8_t;
        typedef unsigned __int16    uint16_t;
        typedef unsigned __int32    uint32_t;
        typedef unsigned __int64    uint64_t;
    #endif
#else
    #include <stdint.h>
#endif

namespace kealib{
        
    static const std::string KEA_DATASETNAME_HEADER( "/HEADER" );
    static const std::string KEA_DATASETNAME_HEADER_NUMBANDS( "/HEADER/NUMBANDS" );
    static const std::string KEA_DATASETNAME_HEADER_BLOCKSIZE( "/HEADER/BLOCKSIZE" );
    static const std::string KEA_DATASETNAME_HEADER_RES( "/HEADER/RES" );
    static const std::string KEA_DATASETNAME_HEADER_TL( "/HEADER/TL" );
    static const std::string KEA_DATASETNAME_HEADER_ROT( "/HEADER/ROT" );
    static const std::string KEA_DATASETNAME_HEADER_SIZE( "/HEADER/SIZE" );
    static const std::string KEA_DATASETNAME_HEADER_WKT( "/HEADER/WKT" );
    
    static const std::string KEA_DATASETNAME_HEADER_FILETYPE( "/HEADER/FILETYPE" );
    static const std::string KEA_DATASETNAME_HEADER_GENERATOR( "/HEADER/GENERATOR" );
    static const std::string KEA_DATASETNAME_HEADER_VERSION( "/HEADER/VERSION" );
    
	static const std::string KEA_DATASETNAME_METADATA( "/METADATA" );
    static const std::string KEA_DATASETNAME_BAND( "/BAND" );
    
    static const std::string KEA_BANDNAME_DATA( "/DATA" );
    static const std::string KEA_BANDNAME_MASK( "/MASK" );
    static const std::string KEA_BANDNAME_DESCRIP( "/DESCRIPTION" );
    static const std::string KEA_BANDNAME_DT( "/DATATYPE" );
    static const std::string KEA_BANDNAME_TYPE( "/LAYER_TYPE" );
    static const std::string KEA_BANDNAME_USAGE( "/LAYER_USAGE" );
    static const std::string KEA_BANDNAME_NO_DATA_VAL( "/NO_DATA_VAL" );
    static const std::string KEA_BANDNAME_METADATA( "/METADATA" );
    static const std::string KEA_BANDNAME_METADATA_MIN( "/METADATA/STATISTICS_MINIMUM" );
    static const std::string KEA_BANDNAME_METADATA_MAX( "/METADATA/STATISTICS_MAXIMUM" );
    static const std::string KEA_BANDNAME_METADATA_MEAN( "/METADATA/STATISTICS_MEAN" );
    static const std::string KEA_BANDNAME_METADATA_STDDEV( "/METADATA/STATISTICS_STDDEV" );
    static const std::string KEA_BANDNAME_METADATA_MODE( "/METADATA/STATISTICS_MODE" );
    static const std::string KEA_BANDNAME_METADATA_HISTOMIN( "/METADATA/STATISTICS_HISTOMIN" );
    static const std::string KEA_BANDNAME_METADATA_HISTOMAX( "/METADATA/STATISTICS_HISTOMAX" );
    static const std::string KEA_BANDNAME_METADATA_HISTONUMBINS( "/METADATA/STATISTICS_HISTONUMBINS" );
    static const std::string KEA_BANDNAME_METADATA_HISTOBINVALUES( "/METADATA/STATISTICS_HISTOBINVALUES" );
    static const std::string KEA_BANDNAME_METADATA_HISTOBINFUNCTION( "/METADATA/STATISTICS_HISTOBINFUNCTION" );
    static const std::string KEA_BANDNAME_METADATA_WAVELENGTH( "/METADATA/WAVELENGTH" );
    static const std::string KEA_BANDNAME_METADATA_FWHM( "/METADATA/FWHM" );
    
    static const std::string KEA_BANDNAME_ATT( "/ATT" );   
    static const std::string KEA_ATT_GROUPNAME_HEADER( "/ATT/HEADER" );
	static const std::string KEA_ATT_GROUPNAME_DATA( "/ATT/DATA" );
    static const std::string KEA_ATT_GROUPNAME_NEIGHBOURS( "/ATT/NEIGHBOURS" );
    static const std::string KEA_ATT_BOOL_DATA( "/ATT/DATA/BOOL" );
    static const std::string KEA_ATT_INT_DATA( "/ATT/DATA/INT" );
    static const std::string KEA_ATT_FLOAT_DATA( "/ATT/DATA/FLOAT" );
    static const std::string KEA_ATT_STRING_DATA( "/ATT/DATA/STRING" );
    static const std::string KEA_ATT_NEIGHBOURS_DATA( "/ATT/NEIGHBOURS/NEIGHBOURS" );
    static const std::string KEA_ATT_BOOL_FIELDS_HEADER( "/ATT/HEADER/BOOL_FIELDS" );
    static const std::string KEA_ATT_INT_FIELDS_HEADER( "/ATT/HEADER/INT_FIELDS" );
    static const std::string KEA_ATT_FLOAT_FIELDS_HEADER( "/ATT/HEADER/FLOAT_FIELDS" );
    static const std::string KEA_ATT_STRING_FIELDS_HEADER( "/ATT/HEADER/STRING_FIELDS" );
    static const std::string KEA_ATT_SIZE_HEADER( "/ATT/HEADER/SIZE" );
    static const std::string KEA_ATT_CHUNKSIZE_HEADER( "/ATT/HEADER/CHUNKSIZE" );
    
    static const std::string KEA_ATT_NAME_FIELD( "NAME" );
    static const std::string KEA_ATT_INDEX_FIELD( "INDEX" );
    static const std::string KEA_ATT_USAGE_FIELD( "USAGE" );
    static const std::string KEA_ATT_COLNUM_FIELD( "COLNUM" );
    
    static const std::string KEA_ATT_STRING_FIELD( "STRING" );
    
    static const std::string KEA_BANDNAME_OVERVIEWS( "/OVERVIEWS" );
    static const std::string KEA_OVERVIEWSNAME_OVERVIEW( "/OVERVIEWS/OVERVIEW" );
    
    static const std::string KEA_GCPS( "/GCPS" );
    static const std::string KEA_GCPS_DATA( "/GCPS/GCPS" );
    static const std::string KEA_GCPS_NUM( "/GCPS/NUM_GCPS" );
    static const std::string KEA_GCPS_PROJ( "/GCPS/PROJ" );
    
    static const std::string KEA_GCPS_PSZID( "PSZ_ID" );
    static const std::string KEA_GCPS_PSZINFO( "PSZ_INFO" );
    static const std::string KEA_GCPS_DFPIXEL( "DF_PIXEL" );
    static const std::string KEA_GCPS_DFLINE( "DF_LINE" );
    static const std::string KEA_GCPS_DFX( "DF_X" );
    static const std::string KEA_GCPS_DFY( "DF_Y" );
    static const std::string KEA_GCPS_DFZ( "DF_Z" );
    
    static const std::string KEA_ATTRIBUTENAME_CLASS( "CLASS" );
	static const std::string KEA_ATTRIBUTENAME_IMAGE_VERSION( "IMAGE_VERSION" );
    static const std::string KEA_ATTRIBUTENAME_BLOCK_SIZE( "BLOCK_SIZE" );
    
    static const std::string KEA_NODATA_DEFINED( "NO_DATA_DEFINED" );
    
    static const int KEA_MDC_NELMTS( 0 ); // 0
    static const hsize_t  KEA_RDCC_NELMTS( 512 ); // 512
    static const hsize_t  KEA_RDCC_NBYTES( 1048576 ); // 1048576
    static const double KEA_RDCC_W0( 0.75 ); // 0.75
    static const hsize_t  KEA_SIEVE_BUF( 65536 ); // 65536
    static const hsize_t  KEA_META_BLOCKSIZE( 2048 ); // 2048
    static const unsigned int KEA_DEFLATE( 1 ); // 1
    static const hsize_t KEA_IMAGE_CHUNK_SIZE( 256 ); // 256
    static const hsize_t KEA_ATT_CHUNK_SIZE( 1000 ); // 1000
    
    enum KEADataType
    {
        kea_undefined = 0,
        kea_8int = 1,
        kea_16int = 2,
        kea_32int = 3,
        kea_64int = 4,        
        kea_8uint = 5,
        kea_16uint = 6,
        kea_32uint = 7,
        kea_64uint = 8,
        kea_32float = 9,
        kea_64float = 10
    };
    
    enum KEALayerType
    {
        kea_continuous = 0,
        kea_thematic = 1
    };
    
    enum KEABandClrInterp
    {
        kea_generic = 0,
        kea_greyindex = 1,
        kea_paletteindex = 2,
        kea_redband = 3,
        kea_greenband = 4,
        kea_blueband = 5,
        kea_alphaband = 6,
        kea_hueband = 7,
        kea_saturationband = 8,
        kea_lightnessband = 9,
        kea_cyanband = 10, 
        kea_magentaband = 11,
        kea_yellowband = 12,
        kea_blackband = 13,
        kea_ycbcr_yband = 14,
        kea_ycbcr_cbband = 15,
        kea_ycbcr_crband = 16
    };
    
    struct KEAImageSpatialInfo
    {
        std::string wktString;
        double tlX;
        double tlY;
        double xRes;
        double yRes;
        double xRot;
        double yRot;
        uint64_t xSize;
        uint64_t ySize;
    };
    
    struct KEAImageGCP
    {
        std::string pszId;
        std::string pszInfo;
        double dfGCPPixel;
        double dfGCPLine;
        double dfGCPX;
        double dfGCPY;
        double dfGCPZ;
    };
    
    struct KEAImageGCP_HDF5
    {
        char *pszId;
        char *pszInfo;
        double dfGCPPixel;
        double dfGCPLine;
        double dfGCPX;
        double dfGCPY;
        double dfGCPZ;
    };
    
    struct KEAString
    {
        char *str;
    };
    
    inline std::string int2Str(int32_t num)
    {
        std::ostringstream convert;
        convert << num;
        return convert.str();
    }
    
    inline std::string uint2Str(uint32_t num)
    {
        std::ostringstream convert;
        convert << num;
        return convert.str();
    }
    
    inline std::string long2Str(int64_t num)
    {
        std::ostringstream convert;
        convert << num;
        return convert.str();
    }
    
    inline std::string ulong2Str(uint64_t num)
    {
        std::ostringstream convert;
        convert << num;
        return convert.str();
    }
    
    inline std::string sizet2Str(size_t num)
    {
        std::ostringstream convert;
        convert << num;
        return convert.str();
    }
    
    inline std::string getDataTypeAsStr(KEADataType dataType)
    {
        std::string strDT = "Unknown";
        
        if(dataType == kea_8int)
        {
            strDT = "Integer 8 bit";
        }
        else if(dataType == kea_16int)
        {
            strDT = "Integer 16 bit";
        }
        else if(dataType == kea_32int)
        {
            strDT = "Integer 32 bit";
        }
        else if(dataType == kea_64int)
        {
            strDT = "Integer 64 bit";
        }
        else if(dataType == kea_8uint)
        {
            strDT = "Unsigned Integer 8 bit";
        }
        else if(dataType == kea_16uint)
        {
            strDT = "Unsigned Integer 16 bit";
        }
        else if(dataType == kea_32uint)
        {
            strDT = "Unsigned Integer 32 bit";
        }
        else if(dataType == kea_64uint)
        {
            strDT = "Unsigned Integer 64 bit";
        }
        else if(dataType == kea_32float)
        {
            strDT = "Float 32 bit";
        }
        else if(dataType == kea_64float)
        {
            strDT = "Float 64 bit";
        }
        else
        {
            strDT = "Unknown";
        }
           
        return strDT;
    }
    
    
}


#endif

