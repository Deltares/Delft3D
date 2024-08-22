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
/// <summary>
/// Remove a unit from memory
/// </summary>
/// <param name="p">pointer to the unit</param>
/// <param name="unitID">unit identification string</param>
/// <returns>pointer to a unit</returns>
static RobUnitPntr freeUnit (RobUnitPntr p, const char *unitID )
{
  RobUnitPntr retval;

  if (p != NULL)
  {
    if ( RobStringCompare(unitID, p->unitID) == 0)
    {
      RobFree(p->unitDescription);
      RobFree(p->unitID);
    }

    retval = p->pNext;

    RobFree(p);

    return retval;
  }
  else
  {
    p->pNext = freeUnit(p->pNext, unitID);
    return p;
  }
}

/// <summary>
/// Remove all units from memory
/// </summary>
void RobFreeUnits(void)
{
  while (RobUnits != NULL)
  {
    RobUnits = freeUnit(RobUnits, RobUnits->unitID );
  }
}

/// <summary>
/// dump all units to file
/// </summary>
void RobDumpUnits(FILE *stream)
{
  RobUnitPntr p;

  p = RobUnits;
  if (p != NULL)
  {
    while (p)
    {
      fprintf (stream , "\tID                : %s\n", p->unitID);
      fprintf (stream , "\tDescription       : %s\n", p->unitDescription);
      fprintf (stream , "\tConversion factor : %f\n", p->conversionFactor);
      fprintf (stream , "\tConversion offset : %f\n\n", p->conversionOffset);
      p = p->pNext;
    }
  }
}

/// <summary>
/// add a unit in memory
/// </summary>
/// <param name="p1">pointer to the units</param>
/// <param name="p2">pointer to the current unit</param>
/// <returns>pointer to the units</returns>
static RobUnitPntr addUnit (RobUnitPntr p1, RobUnitPntr p2 )
{
  if (p1 == NULL)
  {
    p1 = p2;
    p1->pNext = NULL;
  }
  else
  {
    p1->pNext = addUnit(p1->pNext , p2);
  }
  return p1;
}


static RobUnitPntr findUnit (RobUnitPntr p, const char *unitID )
{
  if (p != NULL)
  {
    if ( RobStringCompare(unitID, p->unitID) == 0)
    {
      return p;
    }
    else
    {
      return (findUnit (p->pNext , unitID));
    }
  }
  return(NULL);
}

//
// Check if this Unit exists
//
RobUnitPntr RobUnitFind ( const char *unitID)
{
  return (findUnit(RobUnits, unitID));
}

/// <summary>
/// Add a Unit
/// </summary>
/// <param name="unitID">unit identification string</param>
/// <param name="unitDescription">unit description string</param>
/// <param name="conversionFactor">conversion factor</param>
/// <param name="conversionOffset">conversion offset</param>
int APIENTRY RobUnitAdd (const char *unitID, const char *unitDescription,
                         double conversionFactor, double conversionOffset)
{
  RobUnitPntr p;

  p = RobUnitFind(unitID);
  if (p == NULL)
  {
    p = (RobUnitPntr) RobMalloc(sizeof(RobUnit));

    p->unitID = RobCopyText( unitID );
    p->unitDescription = RobCopyText( unitDescription );
    p->conversionFactor = conversionFactor;
    p->conversionOffset = conversionOffset;

    RobUnits = addUnit(RobUnits, p);

    return(0);
  }
  sprintf(Message.Description, "Unit %s already exists", unitID);
  Message.Code = 1;
  return (1);
}

/// <summary>
/// Get information of a unit
/// </summary>
/// <param name="unitID">unit identification string</param>
/// <param name="unitDescription">unitscription string</param>
/// <param name="conversionFactor">conversion factorm>
/// <param name="conversionOffset">conversion offset</param>
/// <returns>return value</returns>
int APIENTRY RobUnitGetInfo(const char *unitID, char *unitDescription, double *conversionFactor, double *conversionOffset)
{
  RobUnitPntr p;


  p = RobUnits;
  if (p != NULL)
  {
    while (p)
    {
      if ( RobStringCompare(unitID, p->unitID) == 0)
      {
        int   l;

        l = strlen(unitDescription);
        strncpy(unitDescription, p->unitDescription, l);

        *conversionFactor = p->conversionFactor;
        *conversionOffset = p->conversionOffset;
      }
      p = p->pNext;
    }
    return 0;
  }
  sprintf(Message.Description, "Unit %s not found", unitID);
  Message.Code = -1;
  return -1;
}
