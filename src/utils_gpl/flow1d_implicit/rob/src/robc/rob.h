/*
  Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.

  DESCRIPTION
    RIZA OpenMI Buffer implementation

  AUTHOR
    Johan Ansink, HKV lijn in water

  $Header: $
  $NoKeywords: $

  Modifications;

  R. Terveer, RIZA,  june 2005 : introduction Quantityset & QuantiyProperties

*/
#ifndef _ROB_H_
#define _ROB_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <windows.h>

#define ROB_VERYEAR             "2004-2005"
#define ROB_VERSION             "1.2 DLL 24/06/2005"


typedef enum ELEMENTSETTYPE                   /* RIZA OpenMI Buffer Elementsettype            */
{
  IDBASED = 0,                                /* ID based                                     */
  XYPOINT,
  XYLINE,
  XYPOLYLINE,
  XYPOLYGON,
  XYZPOINT,
  XYZLINE,
  XYZPOLYLINE,
  XYZPOLYGON,
  POLYHEDRON
} ELEMENTSETTYPE;

typedef struct MSGTYPE
{
	char Description[1000];
	int  Code, Type;
} MsgType;

typedef enum ROLE                              /* RIZA OpenMI Buffer Roles                    */
{
  PROVIDING = 1,                               /* Providing role                              */
  ACCEPTING                                    /* Accepting role                              */
} ROLE;

///
///  API entries
///

__declspec(dllexport) double APIENTRY  RobMissingValueGetValue(void);
__declspec(dllexport) int    APIENTRY  RobInitialize(const char * logFileName);

__declspec(dllexport) int    APIENTRY  RobUnitAdd (const char *unitID, const char *unitDescription, double conversionFactor, double conversionOffset);
__declspec(dllexport) int    APIENTRY  RobUnitGetInfo(const char *unitID, char *unitDescription, double *conversionFactor, double *conversionOffset);

__declspec(dllexport) int    APIENTRY  RobQuantityAdd (const char *quantityID, const char *quantityDescription, const char *unitID);

__declspec(dllexport) int    APIENTRY  RobQuantityAddProperty(const char*, const char*,const char*,const char*);
__declspec(dllexport) char*  APIENTRY  RobQuantityPropertyGetValue(const char*, const char*,const char*);
__declspec(dllexport) int    APIENTRY  RobQuantityPropertySetValue(const char *quantitysetID, const char *quantityID,const char *propertyID, const char *propertyValue);
__declspec(dllexport) int    APIENTRY  RobQuantityProperty(const char *quantitysetID, const char *quantityID,const char *propertyID, char* propertyValue);

__declspec(dllexport) int    APIENTRY  RobQuantityAddQuantity (const char *quantitysetID, const char *quantityID, const char *quantityDescription, const char *unitID);
__declspec(dllexport) int    APIENTRY  RobQuantitysetRemoveQuantity (const char *quantitysetID, const char *quantityID);
__declspec(dllexport) int    APIENTRY  RobQuantitysetRemoveAllQuantities (const char *quantitysetID);
__declspec(dllexport) int    APIENTRY  RobQuantityGetInfo(const char *quantityID, char *quantityDescription, char *unitID, int *NumberOfQuantities);

__declspec(dllexport) short  APIENTRY  RobIsQuantityset(const char*);
__declspec(dllexport) char*  APIENTRY  RobQuantitysetGetQuantityID (const char* quantitysetID, const int quantityOrderNr);
__declspec(dllexport) int    APIENTRY  RobQuantitysetQuantityID (const char* quantitysetID, const int quantityOrderNr, char* quantityID);

__declspec(dllexport) int    APIENTRY  RobModelAdd (const char *modelID, const char *modelDescription);
__declspec(dllexport) int    APIENTRY  RobModelCount(void);
__declspec(dllexport) int    APIENTRY  RobModelGetInfo(int index, char *modelID, char *modelDescription);

__declspec(dllexport) int    APIENTRY  RobElementsetAdd (const char *modelID, const char *elementsetID, const char *elementsetDescription,ELEMENTSETTYPE elementsetType);
__declspec(dllexport) int    APIENTRY  RobElementsetAddElementIDBased (const char *modelID, const char *elementsetID, const char *elementID);
__declspec(dllexport) int    APIENTRY  RobElementsetAddIDBasedMultiple (const char *modelID, const char *elementsetID, const char *elementsetDescription, int elementCount, const char * * elementIDs);
__declspec(dllexport) int    APIENTRY  RobElementsetAddIDBasedSingle (const char *modelID, const char *elementsetID, const char *elementsetDescription);

__declspec(dllexport) int    APIENTRY  RobElementsetElementCount (const char *modelID, const char *elementsetID);
__declspec(dllexport) int    APIENTRY  RobElementsetGetInfo(const char *modelID, const char *elementsetID, char *elementsetDescription, ELEMENTSETTYPE *elementsetType, int *elementCount);
__declspec(dllexport) int    APIENTRY  RobElementsetGetElement(const char *modelID, const char *elementsetID, int index, char *elementID);

__declspec(dllexport) int    APIENTRY  RobExchangeitemAdd (const char *modelID, const char *quantityID, const char *elementsetID, ROLE role);
__declspec(dllexport) int    APIENTRY  RobExchangeitemCount(const char *modelID);
__declspec(dllexport) int    APIENTRY  RobExchangeitemGetInfo(const char *modelID, int index, char *quantityID, char *elementsetID, ROLE *role);
__declspec(dllexport) void   APIENTRY  RobExchangeitemsTrim(void);
__declspec(dllexport) void   APIENTRY  RobExchangeitemsResize(void);

__declspec(dllexport) int    APIENTRY  RobPutInt(const char *modelId, const char *quantitysetID, const char *quantityID, const char *elementSetID, ROLE role, int nElements, int *iValues);
__declspec(dllexport) int    APIENTRY  RobPutReal(const char *modelId, const char *quantitysetID, const char *quantityID, const char *elementSetID, ROLE role, int nElements, float *fValues);
__declspec(dllexport) int    APIENTRY  RobPutDouble(const char *modelId, const char *quantitysetID, const char *quantityID, const char *elementSetID, ROLE role, int nElements, double *dValues);
__declspec(dllexport) int    APIENTRY  RobGetDouble(const char *modelId, const char *quantitysetID, const char *quantityID, const char *elementSetID, ROLE role, int nElements, double *values);
__declspec(dllexport) void   APIENTRY  RobVersion(char *version);
__declspec(dllexport) void   APIENTRY  RobDump(const char *dumpFile);
__declspec(dllexport) void   APIENTRY  RobFinalize(void);
__declspec(dllexport) void	 APIENTRY  RobGetLastError(int Code, char *Description, int *Type);

#ifdef __cplusplus
}
#endif

#endif // _ROB_H_
