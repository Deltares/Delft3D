/*
  Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.

  DESCRIPTION
    RIZA OpenMI Buffer implementation

  AUTHOR
    Johan Ansink, HKV lijn in water

  $Header: $
  $NoKeywords: $

  HISTORY

  June 2005 : R. Terveer, some changes due to introduction "QuantitySet"
*/
#include "general.h"

extern FILE * logFileHandle;
extern MsgType Message;

/// <summary>
/// Remove a exchangeitem from memory
/// </summary>
/// <param name="p">pointer to the exchangeitem</param> 
/// <param name="modelID">model identification string</param> 
/// <param name="quantityID">quantity identification string</param> 
/// <param name="elementsetID">elementset identification string</param> 
/// <returns>pointer to a exchangeitem</returns>
static RobExchangeitemPntr freeExchangeitem (RobExchangeitemPntr p, const char *modelID, const char *quantityID, const char *elementsetID )
{
  RobExchangeitemPntr retval;

  if (p != NULL)
  {
    if (( RobStringCompare(modelID, p->pElementset->pModel->modelID) == 0) &&
        ( RobStringCompare(quantityID, p->pQuantity->quantityID) == 0) &&
        ( RobStringCompare(elementsetID, p->pElementset->elementsetID) == 0) )
    {
      RobFree(p->values);
    }
    
    retval = p->pNext;

    RobFree(p);

    return retval;
  }
  else
  {
    p->pNext = freeExchangeitem(p->pNext, modelID, quantityID, elementsetID);
    return p;
  }
}

/// <summary>
/// Remove all exchangeitems from memory
/// </summary>
void RobFreeExchangeitems(void)
{
  while (RobExchangeitems != NULL)
  {
    RobExchangeitems = freeExchangeitem(RobExchangeitems,
            RobExchangeitems->pElementset->pModel->modelID,
            RobExchangeitems->pQuantity->quantityID,
            RobExchangeitems->pElementset->elementsetID);
  }
}

/// <summary>
/// dump all exchangeitems to file
/// </summary>
void RobDumpExchangeitems(FILE *stream)
{
  RobExchangeitemPntr p;
  int                 i, qIndex, dumpRatio;

  RobQuantityPntr     pQ;
  char                subQuantID[256+1];
  int                 quantityCount;
  char **             quantIDs=NULL;

  p = RobExchangeitems;
  if (p != NULL)
  {
    while (p)
    {
      fprintf (stream , "\tRole               : %d\n", p->role);
      fprintf (stream , "\tModel              : %s\n", p->pElementset->pModel->modelID);
      fprintf (stream , "\tQuantity           : %s\n", p->pQuantity->quantityID);
      fprintf (stream , "\tElementset         : %s\n", p->pElementset->elementsetID);
      fflush  (stream);

      quantityCount = 0;
      pQ=p->pQuantity->pQuantities;
      while(pQ!=NULL) { pQ = pQ->pNext; quantityCount++;}

      if ( quantityCount > 0 )
      {
        quantIDs = RobMalloc(sizeof(char*)*quantityCount);
        for (i=0, pQ=p->pQuantity->pQuantities; i < quantityCount && pQ != NULL ; i++, pQ=pQ->pNext)
        {
          quantIDs[i] = pQ->quantityID;
        }
      }

      fprintf (stream ,   "\tNumber of elements : %d\n", p->nElements);
      for (i=0, qIndex=0; i < p->nElements ; i++)
      {
        subQuantID[0] = '\0';
        if ( quantityCount > 0 )
        {
          dumpRatio = p->nElements/quantityCount;
          if ( dumpRatio == 0 ) dumpRatio = 1;
          if ( i % ( dumpRatio ) == 0 )
            sprintf(subQuantID, "\t(%s)", quantIDs[qIndex++]);
        }
        if(p->values[i] == MISSING_VALUE)
          fprintf (stream , "\t\tElement[%2d]= %s (=%g)%s\n", i, "MISSING_VALUE", MISSING_VALUE, subQuantID);
        else
          fprintf (stream , "\t\tElement[%2d]= %12.6f%s\n", i, (float)p->values[i], subQuantID);
        fflush  (stream);
      }

      if ( quantityCount > 0 )
        RobFree(quantIDs);

      fprintf (stream , "\n\n");
      fflush(stream);
      p = p->pNext;
    }
  }
}

/// <summary>
/// add a exchangeitem in memory
/// </summary>
/// <param name="p1">pointer to the exchangeitems</param> 
/// <param name="p2">pointer to the current exchangeitem</param> 
/// <returns>pointer to the exchangeitems</returns>
static RobExchangeitemPntr addExchangeitem (RobExchangeitemPntr p1, RobExchangeitemPntr p2 )
{
  if (p1 == NULL)
  {
     p1 = p2;
     p1->pNext = NULL;
  }
  else
  {
    p1->pNext = addExchangeitem(p1->pNext , p2);
  }
  return p1;
}

/// <summary>
/// Find a Exchangeitem
/// </summary>
/// <param name="p1">pointer to the elementsets</param> 
/// <param name="modelID">model identification string</param> 
/// <param name="quantityID">quantity identification string</param> 
/// <param name="elementsetID">elementset identification string</param> 
/// <returns>pointer to the elementset</returns>
static RobExchangeitemPntr findExchangeitem (RobExchangeitemPntr p, const char *modelID, const char *quantityID, const char *elementsetID, ROLE role)
{
  if (p != NULL)
  {
#if 0
    fprintf(logFileHandle, "\tChecking >%s<, >%s<, %1d, >%s<\n", 
		                  p->pQuantity->quantityID,
		                  p->pElementset->elementsetID,
		                  p->role,
		                  p->pElementset->pModel->modelID);
    fflush(logFileHandle);
#endif

    if (( RobStringCompare(modelID, p->pElementset->pModel->modelID) == 0) &&
        ( RobStringCompare(quantityID, p->pQuantity->quantityID) == 0) &&
        ( RobStringCompare(elementsetID, p->pElementset->elementsetID) == 0) &&
          role == p->role )
    {
#if 0
      fprintf(logFileHandle, "\t\tFOUND!\n");
      fflush(logFileHandle);
#endif
      return p;
    }
    else
    {
      return (findExchangeitem (p->pNext , modelID, quantityID, elementsetID, role));
    }
  }
  return(NULL);
}

/// <summary>
/// Check if this exchangeitem exists
/// </summary>
/// <param name="modelID">model identification string</param> 
/// <param name="quantityID">quantity identification string</param> 
/// <param name="elementsetID">elementset identification string</param> 
/// <returns>pointer to the existing exchangeitem</returns>
RobExchangeitemPntr RobExchangeitemFind (const char *modelID, const char *quantityID, const char *elementsetID, ROLE role)
{
#if 0
    fprintf(logFileHandle, "Searching >%s<, >%s<, %1d, >%s<\n", 
		                        quantityID,
		                        elementsetID,
		                        role,
		                        modelID);
    fflush(logFileHandle);
#endif
  return (findExchangeitem(RobExchangeitems, modelID, quantityID, elementsetID, role));
}


/// <summary>
/// Add a Exchangeitem
/// </summary>
/// <param name="modelID">model identification string</param> 
/// <param name="quantityID">quantity identification string</param> 
/// <param name="elementID">element identification string</param> 
/// <param name="role">( providing | accepting )</param> 
int APIENTRY RobExchangeitemAdd (const char *modelID, const char *quantityID, const char *elementsetID, ROLE role)
{
  RobExchangeitemPntr p;
  RobQuantityPntr     pQ;
  RobElementsetPntr   pE;
  RobModelPntr        pM;
  int                 NumberOfQuantities=1;
  int                 j;

  p = RobExchangeitemFind(modelID, quantityID, elementsetID, role);
  if (p == NULL)
  {
    pM= RobModelFind(modelID);
    if (pM != NULL)
    {
      pQ = RobQuantityFind(quantityID);
      if (pQ != NULL)
      {
        pE = RobElementsetFind(modelID, elementsetID);
        if (pE != NULL)
        {
          p = (RobExchangeitemPntr) RobMalloc(sizeof(RobExchangeitem));

          p->role       = role;

          p->pQuantity  = pQ;
          p->pElementset= pE;

          // --------------------------------------------------------------------
          // June 2005 : R. Terveer
          // if Quantity is a QuantitySet, use modified array length
          if(RobIsQuantityset(quantityID))
          {
            RobQuantityGetInfo(quantityID,NULL,NULL,&NumberOfQuantities);
          }
          // --------------------------------------------------------------------

          p->nElements =  p->pElementset->elementCount * NumberOfQuantities;

          p->values = RobMalloc(p->nElements*sizeof(double));
          // Set default
          for(j=0;j<p->nElements;j++) { p->values[j] = MISSING_VALUE; }

          RobExchangeitems  = addExchangeitem(RobExchangeitems, p);

          return (0);
        }
        else
        {
		  sprintf(Message.Description, "elementSet %s not found", elementsetID);
		  Message.Code = -3;
          return (-3);
        }
      }
      else
      {
		sprintf(Message.Description, "quantity %s not found", quantityID);
		Message.Code = -2;
        return (-2);
      }
    }
    else
    {
	  sprintf(Message.Description, "model %s not found", modelID);
	  Message.Code = -1;
      return (-1);
    }
  }  
  return (1);
}


/// <summary>
/// Number of exchangeitems
/// </summary>
/// <param name="modelID">model identification string</param> 
/// <returns>the number of exchangeitems</returns>
int APIENTRY RobExchangeitemCount(const char *modelID)
{
  RobExchangeitemPntr  p;
  int                  count = 0;

  p = RobExchangeitems;
  if (p != NULL)
  {
    while (p)
    {
      if ( RobStringCompare(modelID, p->pElementset->pModel->modelID) == 0) 
      {
        count++;
      }
      p = p->pNext;
    }
  }
  return count;
}


/// <summary>
/// Get information of a exchangeitem
/// </summary>
/// <param name="modelID">model identification string</param> 
/// <param name="index">index in the elementset</param> 
/// <param name="quantityID">quantity identification string</param> 
/// <param name="elementsetID">elementset identification string</param> 
/// <param name="role">exchangeitem role, accepting or providing</param> 
/// <returns>return value</returns>
int APIENTRY RobExchangeitemGetInfo(const char *modelID, int index, char *quantityID, char *elementsetID, ROLE *role)
{
  RobExchangeitemPntr  p;
  int                  count = 0;
  
  p = RobExchangeitems;
  if (p != NULL)
  {
    while ( (p) &&  (index != count))
    {
      if ( RobStringCompare(modelID, p->pElementset->pModel->modelID) == 0) 
      {
        count++;
      }
      if (index == count)
      {
        int   l;
        
        l = strlen( quantityID);
        strncpy (quantityID, p->pQuantity->quantityID, l);

        l = strlen( elementsetID);
        strncpy(elementsetID, p->pElementset->elementsetID, l);

        *role         = p->role;
      }
      p = p->pNext;
    }
    return 0;
  }
  sprintf(Message.Description, "exchangeitem of quantity %s and elementset %s not found", 
				quantityID, elementsetID);
  Message.Code = -1;
  return -1;
}

/*
 Set all buffer values to "MissingValue"
 */
void RobClear()
{
  RobExchangeitemPntr  p = RobExchangeitems;
  int                  NumberOfQuantities=1,n,j;

  while(p != NULL)
  {
      n = RobQuantityGetInfo(p->pQuantity->quantityID,NULL,NULL,&NumberOfQuantities);
      if(NumberOfQuantities == 0) NumberOfQuantities = 1;
      for (j=0;j < p->pElementset->elementCount*NumberOfQuantities ;j++ )   // TODO: check
      {
          p->values[j] = MISSING_VALUE;
      }
      p=p->pNext;
  }
}


/*
 Adjust buffer size and clear values for changed QuantitySet.
*/
void robExchangeitemTrim(RobExchangeitemPntr  p)
{
  int                  NumberOfQuantities,n, nSize;

  n = RobQuantityGetInfo(p->pQuantity->quantityID,NULL,NULL,&NumberOfQuantities);
  if(NumberOfQuantities == 0) NumberOfQuantities = 1;
  nSize = p->pElementset->elementCount * NumberOfQuantities;

  if(nSize != p->nElements)
  {
      double *V = RobMalloc(nSize*sizeof(double));
      for(n=0;n < nSize; n++){ V[n] = MISSING_VALUE;}
      RobFree(p->values);
      p->values=V;
      p->nElements = nSize;
  }
}

/*
 Adjust buffer size and clear values for changed QuantitySets.
*/
void APIENTRY RobExchangeitemsTrim(void)
{
  RobExchangeitemPntr  p = RobExchangeitems;

  while(p != NULL)
  {
    robExchangeitemTrim(p);
    p=p->pNext;
  }
}


/*
 Resize buffer size for changed QuantitySet.
*/
void robExchangeitemResize(RobExchangeitemPntr  p)
{
  int NumberOfQuantities,n, nSize;

  n = RobQuantityGetInfo(p->pQuantity->quantityID,NULL,NULL,&NumberOfQuantities);
  if(NumberOfQuantities > 0)
  {
    nSize = p->pElementset->elementCount * NumberOfQuantities;

    if(nSize != p->nElements)
    {
        p->values = RobRealloc(p->values, nSize*sizeof(double));
        for(n=p->nElements ;n < nSize; n++){ p->values[n] = MISSING_VALUE;}
        p->nElements = nSize;
    }
  }
}

/*
 Resize buffer size for changed QuantitySets.
*/
void APIENTRY RobExchangeitemsResize(void)
{
  RobExchangeitemPntr  p = RobExchangeitems;

  while(p != NULL)
  {
    robExchangeitemResize(p);
    p=p->pNext;
  }
}


