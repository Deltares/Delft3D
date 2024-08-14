/*
  Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.

  DESCRIPTION
    RIZA OpenMI Buffer implementation

  AUTHOR
    Johan Ansink, HKV lijn in water

  $Header: $
  $NoKeywords: $

*/
#include "general.h"

extern MsgType Message;

static RobElementsetPntr newElementSet()
{
    RobElementsetPntr p= (RobElementsetPntr) RobMalloc(sizeof(RobElementset));

    if(p !=NULL)
    {
		p->elementsetID         = NULL;
		p->elementsetDescription= NULL;
		p->elementCount         = 0;
        p->elementIDs           = NULL;
		p->pModel               = NULL;
		p->xCoords              = NULL;
		p->yCoords              = NULL;
        p->pNext                = NULL;
    }
    return p;
}


/// <summary>
/// Remove a elementset from memory
/// </summary>
/// <param name="p">pointer to the elementset</param>
/// <param name="elementsetID">elementset identification string</param>
/// <returns>pointer to a elementset</returns>
static RobElementsetPntr freeElementset (RobElementsetPntr p, const char *elementsetID )
{
  RobElementsetPntr retval;
  int e;

  if (p != NULL)
  {
    if ( RobStringCompare(elementsetID, p->elementsetID) == 0)
    {
      RobFree(p->elementsetID);
      RobFree(p->elementsetDescription);

      p->pModel= NULL;

	  if ( p->elementIDs != NULL )
	  {
		  for ( e = 0 ; e < p->elementCount ; e++ )
		  {
			  RobFree(p->elementIDs[e]);
		  }
		  RobFree(p->elementIDs);
	  }

      p->elementCount= 0;

      RobFree(p->xCoords);
      RobFree(p->yCoords);
    }

    retval = p->pNext;

    RobFree(p);
    p=NULL;

    return retval;
  }
  else
  {
    p->pNext = freeElementset(p->pNext, elementsetID);
    return p;
  }
}

/// <summary>
/// Remove all elementsets from memory
/// </summary>
void RobFreeElementsets(void)
{
  while (RobElementsets != NULL)
  {
    RobElementsets = freeElementset(RobElementsets, RobElementsets->elementsetID );
  }
}

/// <summary>
/// dump all units to file
/// </summary>
void RobDumpElementsets(FILE *stream)
{
  RobElementsetPntr p;
  int               i;
  char *            elementID;

  p = RobElementsets;
  if (p != NULL)
  {
    while (p)
    {
      fprintf (stream , "\tElementset         : %s\n", p->elementsetID);
      fprintf (stream , "\tDescription        : %s\n", p->elementsetDescription);
      fprintf (stream , "\tType               : %d\n", p->elementsetType );
      fprintf (stream , "\t#Elements          : %d\n", p->elementCount );
      fflush  (stream);

      for (i=0; i < p->elementCount ; i++)
      {
        const int maxDumpStringLen = 256+1;
        elementID = (char *) RobMalloc(maxDumpStringLen+1);
        RobElementsetGetElement(p->pModel->modelID, p->elementsetID, i, elementID);
        fprintf (stream , "\t\tElement[%2d]: %s\n", i, elementID);
        fflush  (stream);
        RobFree(elementID);
      }
      fprintf (stream , "\n");
      fflush  (stream);

      p = p->pNext;
    }
  }
}

/// <summary>
/// add a elementset in memory
/// </summary>
/// <param name="p1">pointer to the elementsets</param>
/// <param name="p2">pointer to the current elementset</param>
/// <returns>pointer to the elementsets</returns>
static RobElementsetPntr addElementset (RobElementsetPntr p1, RobElementsetPntr p2 )
{
  if (p1 == NULL)
  {
    p1 = p2;
    p1->pNext = NULL;
  }
  else
  {
    p1->pNext = addElementset(p1->pNext , p2);
  }
  return p1;
}

/// <summary>
/// Find a elementset
/// </summary>
/// <param name="p1">pointer to the elementsets</param>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="elementID">element identification string</param>
/// <returns>pointer to the elementset</returns>
static RobElementsetPntr findElementset (RobElementsetPntr p, const char *elementsetID, const char *elementID )
{
  if (p != NULL)
  {
    if ( RobStringCompare(elementsetID, p->elementsetID) == 0 )
    {
      return p;
    }
    else
    {
      return (findElementset (p->pNext, elementsetID, elementID));
    }
  }
  return(NULL);
}


static RobElementsetPntr findElementsetId (RobElementsetPntr p, const char *modelID, const char *elementsetID )
{
  if (p != NULL)
  {
    if ( ( RobStringCompare(modelID     , p->pModel->modelID) == 0 ) &&
         ( RobStringCompare(elementsetID, p->elementsetID)   == 0 )   )
    {
      return p;
    }
    else
    {
      return (findElementsetId (p->pNext, modelID, elementsetID));
    }
  }
  return(NULL);
}

RobElementsetPntr RobElementsetFind (const char *modelID, const char *elementsetID)
{
  return (findElementsetId(RobElementsets, modelID, elementsetID));
}

/// <summary>
/// Add a element to the Elementset
/// </summary>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="elementsetDescription">elementset description string</param>
/// <param name="elementsetType">type elementset</param>
int APIENTRY RobElementsetAdd (const char *modelID, const char *elementsetID, const char *elementsetDescription, ELEMENTSETTYPE elementsetType)
{
  RobElementsetPntr p;
  RobModelPntr      m;

  p = RobElementsetFind(modelID, elementsetID);
  if (p == NULL)
  {
    m = RobModelFind(modelID);

    if ( m != NULL )
    {
      if (elementsetType == IDBASED)
      {
        p = (RobElementsetPntr) newElementSet();

        p->elementsetID          = RobCopyText(elementsetID);
        p->elementsetDescription = RobCopyText(elementsetDescription );

        p->pModel                = m;

        p->elementsetType        = elementsetType;

        RobElementsets = addElementset(RobElementsets, p);

        return (0);
      }
      else
      {
        // elementype not yet implemented
		sprintf(Message.Description, "elementtype %d not yet implemented", elementsetType);
		Message.Code = -2;
        return (-2);
      }
    }
    else
    {
      // model not found
	  sprintf(Message.Description, "model %s not found", modelID);
	  Message.Code = -1;
      return (-1);
    }
  }
  sprintf(Message.Description, "elementset %s already exists", elementsetID);
  Message.Code = 1;
  return (1);
}

/// <summary>
/// Add a element to the Elementset
/// </summary>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="elementsetDescription">elementset description string</param>
/// <param name="elementsetType">type elementset</param>
int APIENTRY RobElementsetAddElementIDBased(const char *modelID, const char *elementsetID, const char *elementID)
{
  RobElementsetPntr p;

  p = RobElementsetFind(modelID, elementsetID);
  if (p != NULL)
  {
    if ( p->elementsetType != IDBASED )
    {
    }
    else
    {
      if ( p->elementCount == 0 )
      {
        // First element. Malloc
        p->elementIDs = (char * *) RobMalloc(sizeof(char * *));
      }
      else
      {
        // Add element. Re-alloc.
        p->elementIDs = (char * *) RobRealloc(p->elementIDs, ( p->elementCount+1)*sizeof(char * *) );
      }

      p->elementIDs[ p->elementCount ] = RobCopyText(elementID);
      p->elementCount++;
 
      return (0);
    }

    // elementset not ID-Based
    sprintf(Message.Description, "elementset %s is not ID-Based", elementsetID);
    Message.Code = -2;
    return (-2);
  }
  // elementset not found
  sprintf(Message.Description, "elementset %s already exists", elementsetID);
  Message.Code = -1;
  return (-1);
}

/// <summary>
/// Add a element to the Elementset
/// </summary>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="elementsetDescription">elementset description string</param>
/// <param name="elementCount">number of elements in set (== in elementIDs)</param>
/// <param name="elementIDs">element identification strings</param>
int APIENTRY RobElementsetAddIDBasedMultiple (const char *modelID, const char *elementsetID, const char *elementsetDescription, int elementCount, const char * * elementIDs)
{
  RobElementsetPntr p;
  RobModelPntr      m;
  int e;

  p = RobElementsetFind(modelID, elementsetID);
  if (p == NULL)
  {
    m = RobModelFind(modelID);

    if ( m != NULL )
    {
      p = (RobElementsetPntr) newElementSet();

      p->elementsetID          = RobCopyText(elementsetID);
      p->elementsetDescription = RobCopyText(elementsetDescription );

      p->pModel                = m;

      p->elementsetType        = IDBASED;
      p->elementCount          = elementCount;

      p->elementIDs            = (char * *) RobMalloc(sizeof(char *) * elementCount);

      for ( e = 0 ; e < elementCount ; e++ )
      {
        p->elementIDs[e] = RobCopyText(elementIDs[e]);
      }

      RobElementsets = addElementset(RobElementsets, p);

      return (0);
    }
    else
    {
      // model not found
      sprintf(Message.Description, "model %s not found", modelID);
      Message.Code = -1;
      return (-1);
    }
  }
  sprintf(Message.Description, "elementset %s already exists", elementsetID);
  Message.Code = 1;
  return (1);
}

/// <summary>
/// Add an ID_based element to an Elementset
/// </summary>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="elementsetDescription">elementset description string</param>
/// <param name="elementIDs">element identification strings</param>
int APIENTRY RobElementsetAddIDBasedSingle (const char *modelID, const char *elementsetID, const char *elementsetDescription)
{
  char * * elementIDs;
  int      elementCount;
  int      rv;

  elementCount = 1;
  elementIDs = RobMalloc(elementCount * sizeof(char *));
  elementIDs[0] = (char *) elementsetID;

  rv = RobElementsetAddIDBasedMultiple (modelID, elementsetID, elementsetDescription, elementCount, elementIDs);

  RobFree(elementIDs);

  return rv;
}

int APIENTRY RobElementsetElementCount (const char *modelID, const char *elementsetID)  // TODO model ID
{
  RobElementsetPntr  p;
  int                count = -1;

  p = RobElementsetFind(modelID, elementsetID);
  if (p != NULL)
  {
    count = p->elementCount;
  }
  return count;
}

/// <summary>
/// Get all information of a elementset
/// </summary>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="elementsetDescription">elementset description string</param>
/// <param name="elementsetType">type elementset</param>
/// <param name="elementCount">aantal elementen</param>
/// <returns>return value</returns>
int APIENTRY RobElementsetGetInfo(const char *modelID, const char *elementsetID, char *elementsetDescription, ELEMENTSETTYPE *elementsetType, int *elementCount)
{
  RobElementsetPntr p;

  p = RobElementsetFind(modelID, elementsetID);
  if (p != NULL)
  {
    int   l;
    l = strlen(elementsetDescription);
    strncpy(elementsetDescription, p->elementsetDescription, l);

    *elementsetType = p->elementsetType ;

    *elementCount = p->elementCount;

    return 0;
  }
  sprintf(Message.Description, "elementset %s not found", elementsetID);
  Message.Code = -1;
  return -1;
}

/// <summary>
/// Get information of a elementset
/// </summary>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="index">index in the elementset</param>
/// <param name="elementID">element identification string</param>
/// <returns>return value</returns>
int APIENTRY RobElementsetGetElement(const char *modelID, const char *elementsetID, int index, char *elementID)
{
  RobElementsetPntr p;

  int retVal = -1;

  p = RobElementsetFind(modelID, elementsetID);
  if (p != NULL)
  {
    if ( (index >= 0) && (index < p->elementCount) )
    {
      int len = strlen(p->elementIDs[index]);
      strncpy(elementID, p->elementIDs[index], len+1);
      retVal = 0;
    }
  }
  sprintf(Message.Description, "element with index %d in elementset %s not found", index, elementsetID);
  Message.Code = -1;
  return retVal;
}
