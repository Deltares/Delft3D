/*
  Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.

  DESCRIPTION
    RIZA OpenMI Buffer implementation

  AUTHOR
    Johan Ansink, HKV lijn in water

  $Header: $
  $NoKeywords: $

*/
#ifndef _GENERAL_H_
#define _GENERAL_H_

#ifdef __cplusplus

extern "C" {
#endif

#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <stdio.h>
#include <malloc.h>
#include <limits.h>
#include <float.h>

#include "rob.h"

#define MISSING_VALUE DBL_MAX                   /* Default element data Value  */


typedef struct RobUnit RobUnit;
typedef RobUnit * RobUnitPntr;
struct RobUnit                                 /* Unit                                         */
{
  char *            unitID;                    /* Unit identification string                   */
  char *            unitDescription;           /* Unit identification string                   */
  double            conversionFactor;          /* Conversion factor                            */
  double            conversionOffset;          /* Conversion offset                            */
  RobUnitPntr       pNext;                     /* Pointer naar de volgende                     */
};

typedef struct RobProperty RobProperty;
typedef RobProperty * RobPropertyPntr;
struct RobProperty                             /* Property                                     */
{
  char *            PropertyID;                /* Property identification string               */
  char *            PropertyValue;             /* Property value                               */
  RobPropertyPntr   pNext;                     /* Pointer naar de volgende                     */
};

typedef struct RobQuantity RobQuantity;
typedef RobQuantity * RobQuantityPntr;
struct RobQuantity                             /* Quantity                                     */
{
  char *            quantityID;                /* Quantity identification string               */
  char *            quantityDescription;       /* Quantity description string                  */
  RobUnitPntr       pUnit;                     /* Unit identification string                   */
  RobPropertyPntr   pProperties;               /* First Property                               */
  RobQuantityPntr   pQuantities;               /* Quantityset, first element                   */
  RobQuantityPntr   pNext;                     /* Pointer naar de volgende                     */
};

typedef struct RobModel RobModel;
typedef RobModel * RobModelPntr;
struct RobModel                                /* Model                                        */
{
  char *              modelID;                 /* Model identification string                  */
  char *              modelDescription;        /* Model description string                     */
  RobModelPntr        pNext;                   /* Pointer naar de volgende                     */
};

typedef struct RobElementset RobElementset;
typedef RobElementset * RobElementsetPntr;
struct RobElementset                           /* Elementset                                   */
{
  char *              elementsetID;            /* Elementset identification string             */
  char *              elementsetDescription;   /* Elementset description string                */
  ELEMENTSETTYPE      elementsetType;          /* Elementset type                              */
  RobModelPntr        pModel;                  /* Model                                        */
  int                 elementCount;            /* #elements in set                             */
  char * *            elementIDs;              /* Element identification strings               */
  double *            xCoords;                 /* x-coordinates                                */
  double *            yCoords;                 /* y-coordinates                                */
  RobElementsetPntr   pNext;                   /* Pointer naar de volgende                     */
};

typedef struct RobExchangeitem RobExchangeitem;
typedef RobExchangeitem * RobExchangeitemPntr;
struct RobExchangeitem                         /* Exchange item                                */
{
  RobQuantityPntr     pQuantity;               /* Quantity                                     */
  RobElementsetPntr   pElementset;             /* Elementset                                   */
  int                 nElements;               /* Number of elements                           */
  double              *values;                 /* Element value(s)                             */
  ROLE                role;                    /* Role                                         */
  RobExchangeitemPntr pNext ;                  /* Pointer naar de volgende                     */
};

///
/// Data
///
static RobElementsetPntr      RobElementsets;
static RobExchangeitemPntr    RobExchangeitems;
static RobModelPntr           RobModels;
static RobQuantityPntr        RobQuantities;
static RobUnitPntr            RobUnits;


void *                RobMalloc ( long );
void *                RobRealloc ( void *, long );

#if 0
void                  RobFree ( void * );
#else
#define RobFree(ptr)  if(ptr!=NULL){free(ptr);ptr=NULL;}
#endif

short                 RobStringCompare ( const char *, const char * );
char *                RobCopyText(const char * );
RobUnitPntr           RobUnitFind ( const char *);
RobQuantityPntr       RobQuantityFind (const char *);
RobElementsetPntr     RobElementsetFind (const char *, const char *);
RobExchangeitemPntr   RobExchangeitemFind (const char *, const char *, const char *, ROLE);
RobModelPntr          RobModelFind (const char *);
void                  RobFreeUnits(void);
void                  RobFreeElementsets(void);
void                  RobFreeQuantities(void);
void                  RobFreeExchangeitems(void);
void                  RobFreeModels(void);
void                  RobDumpUnits(FILE *);
void                  RobDumpElementsets(FILE *);
void                  RobDumpQuantities(FILE *);
void                  RobDumpExchangeitems(FILE *);
void                  RobDumpModels(FILE *);
int                   RobQuantityIndexInSet(const char*, const char*);

#ifdef __cplusplus
}
#endif

#endif // _GENERAL_H_
