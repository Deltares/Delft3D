/*
  Copyright © 2004, Rijkswaterstaat/RIZA & ®HKV Consultants, All Rights Reserved.

  DESCRIPTION
    RIZA OpenMI Buffer implementation

  AUTHOR
    Johan Ansink, HKV lijn in water

  $Header: $
  $NoKeywords: $

  HISTORY

  Juni 2005 : R. Terveer
  - Added Properties
  - A Quantity becomes a Quantityset after adding another Quantity to it.
    (or a Quantity is an "one element" Quantityset)

*/
#include "general.h"

extern FILE * logFileHandle;
extern MsgType Message;

static void RobFreeQuantitiesP(RobQuantityPntr p);

static RobQuantityPntr newQuantity()
{
    RobQuantityPntr p= (RobQuantityPntr) RobMalloc(sizeof(RobQuantity));

    if(p !=NULL)
    {
        p->quantityID           = NULL;
        p->quantityDescription  = NULL;
        p->pUnit                = NULL;
        p->pQuantities          = NULL;
        p->pProperties          = NULL;
        p->pNext                = NULL;
    }
    return p;
}


static void RobFreeQuantity(RobQuantityPntr p)
{
    RobPropertyPntr  pC;

    if(p !=NULL)
    {
        RobFree(p->quantityID);
        RobFree(p->quantityDescription);
        p->pUnit = NULL;

        RobFreeQuantitiesP(p->pQuantities);
        while(p->pProperties != NULL)
        {
            pC=p->pProperties;
            p->pProperties=p->pProperties->pNext;
            RobFree(pC->PropertyID); RobFree(pC->PropertyValue);
	    RobFree(pC);
        }
    }
}


/// <summary>
/// Remove a quantity from memory
/// </summary>
/// <param name="p">pointer to the quantity</param>
/// <param name="quantityID">quantity identification string</param>
/// <returns>pointer to a quantity</returns>
static RobQuantityPntr freeQuantity (RobQuantityPntr p, const char *quantityID )
{
  RobQuantityPntr retval;

  if (p != NULL)
  {
    retval = NULL;
    if ( RobStringCompare(quantityID, p->quantityID) == 0)
    {
        retval = p->pNext;
        RobFreeQuantity(p);
    }
    return retval;
  }
  else
  {
    p->pNext = freeQuantity(p->pNext, quantityID);
    return p;
  }
}

// Remove all Quantities from given point
static void RobFreeQuantitiesP(RobQuantityPntr p)
{
  while (0 && p != NULL) // TODO
  {
    p = freeQuantity(p, p->quantityID );
  }
}


/// <summary>
/// Remove all quantities from memory
/// </summary>
void RobFreeQuantities(void)
{
  while (RobQuantities != NULL)
  {
    RobQuantities = freeQuantity(RobQuantities, RobQuantities->quantityID );
  }
}


/// <summary>
/// dump all quantities to file
/// </summary>
void RobDumpQuantities(FILE *stream)
{
  RobQuantityPntr p,t;
  RobPropertyPntr r;

  p = RobQuantities;
  if (p != NULL)
  {
    while (p)
    {
      fprintf (stream , "%s\n", (p->pQuantities == NULL)?"\n\tQuantity":"\n\tQuantityset");
      fprintf (stream , "\tID                : %s\n", p->quantityID);
      fprintf (stream , "\tDescription       : %s\n", p->quantityDescription);
      fprintf (stream , "\tUnit ID           : %s\n", p->pUnit->unitID);

      if((r=p->pProperties) != NULL) fprintf (stream, "\tProperties :\n");
      while(r != NULL)
      {
        fprintf(stream, "\tPropertyID : %s  ,PropertyValue : %s\n", r->PropertyID, r->PropertyValue);
        r=r->pNext;
      }

      t=p->pQuantities;
      while(t !=NULL)
      {
        fprintf (stream , "\t\tID                : %s\n", t->quantityID);
        fprintf (stream , "\t\tDescription       : %s\n", t->quantityDescription);
        fprintf (stream , "\t\tUnit ID           : %s\n", t->pUnit->unitID);

        if((r=t->pProperties) != NULL) fprintf (stream, "\t\t\tProperties :\n");
        while(r != NULL)
        {
          fprintf(stream, "\t\t\tPropertyID : %10s  ,PropertyValue : %s\n", r->PropertyID, r->PropertyValue);
          r=r->pNext;
        }
        t=t->pNext;
      }

      p = p->pNext;
    }
	fflush(stream);
  }
}

/// <summary>
/// add a quantity in memory
/// </summary>
/// <param name="p1">pointer to the quantities</param>
/// <param name="p2">pointer to the current quantity</param>
/// <returns>pointer to the quantities</returns>
static RobQuantityPntr addQuantity (RobQuantityPntr p1, RobQuantityPntr p2 )
{
  if (p1 == NULL)
  {
    p1 = p2;
    p1->pNext = NULL;
  }
  else
  {
    p1->pNext = addQuantity(p1->pNext , p2);
  }
  return p1;
}

static RobQuantityPntr findQuantity (RobQuantityPntr p, const char *quantityID )
{
  if (p != NULL)
  {
    if (RobStringCompare(quantityID, p->quantityID) == 0)
    {
      return p;
    }
    else
    {
      return (findQuantity (p->pNext , quantityID));
    }
  }
  return(NULL);
}


//
// Check if this Quantity exists
//
RobQuantityPntr RobQuantityFind (const char *quantityID)
{
  return (findQuantity(RobQuantities, quantityID));
}


//
// Find or Add a Quantity
//
int APIENTRY RobQuantityAdd (const char *quantityID, const char *quantityDescription, const char *unitID)
{
  RobQuantityPntr p;
  RobUnitPntr     pU;

  p = RobQuantityFind(quantityID);
  if (p == NULL)
  {
    pU = RobUnitFind(unitID);
    if (pU != NULL)
    {
      p = newQuantity();
      if(p !=NULL)
      {
        p->quantityID           = RobCopyText( quantityID );
        p->quantityDescription  = RobCopyText( quantityDescription );
        p->pUnit                = pU;
      }
      else
      {
        /* memory allocation faillure */
		sprintf(Message.Description, "Memory allocation faillure");
		Message.Code = -2;
        return -2;
      }

      RobQuantities = addQuantity(RobQuantities, p);
      /* Quantity added */
      return (0);
    }
    else
    {
      /* Unit does not exist */
	  sprintf(Message.Description, "Unit %s does not exist", unitID);
	  Message.Code = -1;
      return (-1);
    }
  }
  /* Quantity already exists*/
  sprintf(Message.Description, "Quantity %s already exists", quantityID);
  Message.Code = 1;
  return (1);
}

/// <summary>
/// Add a quantity to another (create/extend a quantityset)
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="quantityDescription">quantity description string</param>
/// <param name="unitID">unit identification string</param>
/// <returns>return value</returns>
int APIENTRY RobQuantityAddQuantity (const char *quantitysetID, const char *quantityID, const char *quantityDescription, const char *unitID)
{
    RobQuantityPntr p = RobQuantityFind(quantitysetID);

    if (p != NULL)
    {
        RobUnitPntr pU = RobUnitFind(unitID);
        if ( pU == NULL && RobStringCompare(unitID, "-") == 0 )
        {
            int addUnitResult = RobUnitAdd ("-", "Undefined Unit", 0.0L, 1.0L);
            if ( addUnitResult != 0 && addUnitResult != 1 )
            {
                /* could not add dummy unit, strang internal error */
                return -99;
            }
            pU = RobUnitFind(unitID);
            if (pU == NULL)
            {
                /* could not find dummy unit */
                return -99;
            }
        }

        if (pU != NULL)
        {
            if(findQuantity(p->pQuantities,quantityID) ==NULL)
            {
                RobQuantityPntr pNew = newQuantity();
                if(pNew != NULL)
                {
                    pNew->quantityID           = RobCopyText( quantityID );
                    pNew->quantityDescription  = RobCopyText( quantityDescription );
                    pNew->pUnit                = pU;

                    p->pQuantities = addQuantity(p->pQuantities, pNew);
                    /* Quantity added */
                    return (0);
                }
                else
                {
                    /* Memory allocation faillure */
                    sprintf(Message.Description, "Memory allocation failure");
                    Message.Code = -3;
                    return -3;
                }
            }
            else
            {
                /* quantity already exists */
                sprintf(Message.Description, "quantity %s already exists", quantityID);
                Message.Code = 1;
                return 1;
            }
        }
        else
        {
            /* Unit does not exist */
            sprintf(Message.Description, "unit %s does not exist", unitID);
            Message.Code = -2;
            return (-2);
        }
    }
    /* Quantity(Set) does not exists*/
    sprintf(Message.Description, "quantityset %s does not exist", quantitysetID);
    Message.Code = -1;
    return (-1);
}

/// <summary>
/// Remove a specific quantity from a Quantityset
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <returns>return value</returns>
int APIENTRY RobQuantitysetRemoveQuantity (const char *quantitysetID, const char *quantityID)
{
    RobQuantityPntr p = RobQuantityFind(quantitysetID);
    RobQuantityPntr pPrev, pQ;
    int             rv=-1; /* Quantity(Set) does not exists*/

    if (p != NULL)
    {
        pQ=p->pQuantities; pPrev=p;
        while(pQ !=NULL && RobStringCompare(quantityID, pQ->quantityID) != 0)
        {
            pPrev= pQ; pQ=pQ->pNext;
        }

        if(pQ != NULL)
        {
            if(pPrev==p)
                p->pQuantities = pQ->pNext;
            else
                pPrev->pNext = pQ->pNext;

            RobFreeQuantity(pQ);
            rv = 0;
        }
        else
        {
            /* Quantity not in Quantityset */
            sprintf(Message.Description, "quantity %s does not exist in %s", quantityID, quantitysetID);
            Message.Code = -2;
            rv = -2;
        }
    }

    if (rv == -1)
    {
        sprintf(Message.Description, "quantityset %s does not exist", quantitysetID);
        Message.Code = -1;
    }
    return rv;
}


/// <summary>
/// Remove all the quantities in a Quantityset
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <returns>return value</returns>
int APIENTRY RobQuantitysetRemoveAllQuantities (const char *quantitysetID)
{
    RobQuantityPntr p = RobQuantityFind(quantitysetID);
    int             rv=-1; /* Quantity(Set) does not exists*/

    if (p != NULL){RobFreeQuantitiesP(p->pQuantities); p->pQuantities=NULL; rv = 0;}
    else
    {
        sprintf(Message.Description, "quantity %s does not exist", quantitysetID);
        Message.Code = -1;
    }
    return rv;
}

/// <summary>
/// Get ID of a quantity in a Quantityset
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <param name="quantityOrderNr"> ordernumber quantity in set</param>
/// <returns>return value</returns>
char* APIENTRY RobQuantitysetGetQuantityID (const char* quantitysetID, const int quantityOrderNr)
{
    RobQuantityPntr p;
    char*           rv=NULL;

    if( (p = RobQuantityFind(quantitysetID)) !=NULL && quantityOrderNr > 0)
    {
        int j=1;
        p=p->pQuantities;
        while (p != NULL && quantityOrderNr != j)
        {
            p=p->pNext; j+=1;
        }
        if(p!=NULL) rv=RobCopyText(p->quantityID);
    }
    return rv;
}

// For Fortran compatibility
int APIENTRY RobQuantitysetQuantityID (const char* quantitysetID, const int quantityOrderNr, char* quantityID)
{
    char *p = RobQuantitysetGetQuantityID(quantitysetID,quantityOrderNr);
    if(p != NULL) strcpy(quantityID,p);
    return(p == NULL)? 0 : 1;
}


/// <summary>
/// Get information of a quantity
/// </summary>
/// <param name="quantityID">quantity identification string</param>
/// <param name="quantityDescription">quantity description string</param>
/// <param name="unitID">unit identification string</param>
/// <param name="quantityCount">If > 0: number of quanties in set.</param>
/// <returns>return value</returns>
int APIENTRY RobQuantityGetInfo(const char *quantityID, char *quantityDescription, char *unitID, int *quantityCount)
{
    RobQuantityPntr p = RobQuantities;

    *quantityCount = 0;

    if (p != NULL)
    {
        while (p)
        {
            if (RobStringCompare(quantityID, p->quantityID) == 0)
            {
		int   l;
		RobQuantityPntr t=p->pQuantities;

		if(quantityDescription != NULL)
		{
		    l = strlen(quantityDescription);
		    strncpy(quantityDescription, p->quantityDescription, l);
		}

		if(unitID !=NULL)
		{
		    l = strlen(unitID);
		    strncpy(unitID, p->pUnit->unitID, l);
		}

		while(t != NULL){ *quantityCount+=1; t=t->pNext; }

		return 0;
            }

            p = p->pNext;
        }
    }
    sprintf(Message.Description, "quantity %s does not exist", quantityID);
    Message.Code = -1;
    return -1;
}


static RobPropertyPntr newProperty()
{
    RobPropertyPntr p = (RobPropertyPntr) RobMalloc(sizeof(RobProperty));
    if(p != NULL)
    {
        p->PropertyID     = NULL;
        p->PropertyValue  = NULL;
        p->pNext          = NULL;
    }
    return(p);
}

static RobPropertyPntr RobFindProperty(RobPropertyPntr p, const char* propertyID)
{
    RobPropertyPntr rv = p;

    if( !(p==NULL || propertyID == NULL) )
    {
       while(rv !=NULL && RobStringCompare(rv->PropertyID, propertyID) != 0) { rv=rv->pNext;}
       return rv;
    }
	return rv;
}


/// <summary>
/// Add a property to a quantity (or a quantity in a quantityset)
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="PropertyID">property description string</param>
/// <param name="PropertyValue">property's value string</param>
/// <returns>return value</returns>
int APIENTRY RobQuantityAddProperty(const char *quantitysetID,const char *quantityID,const char *propertyID,const char *propertyValue)
{
    short b = quantitysetID==NULL || strlen(quantitysetID) < 1;
    RobQuantityPntr p = (b) ? RobQuantityFind(quantityID) : RobQuantityFind(quantitysetID);
    RobPropertyPntr pP, pNewP;

    if(p!=NULL && !b) p=findQuantity(p->pQuantities,quantityID); /* search for quantity in set */
    {
        if(p != NULL && (pP=RobFindProperty(p->pProperties,propertyID)) == NULL)
        {
            pNewP                = newProperty();
            pNewP->PropertyID    = RobCopyText(propertyID);
            pNewP->PropertyValue = RobCopyText(propertyValue);

            if(p->pProperties==NULL)
            {
                p->pProperties=pNewP;
            }
            else
            {
                pP=p->pProperties;
                while(pP->pNext !=NULL) pP=pP->pNext;
                pP->pNext=pNewP;
            }
            return 0;
        }
        sprintf(Message.Description, "quantity %s does not exist", quantityID);
        Message.Code = 1;
        return 1;
    }
    /* quantity or quantityset/quantitiy does not exist */
    sprintf(Message.Description, "quantity %s does not exist", quantityID);
    Message.Code = -1;
    return -1;
}

/// <summary>
/// Change the value of a quantity's property
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="PropertyID">property description string</param>
/// <param name="PropertyValue">property's value string</param>
/// <returns>return value</returns>
int APIENTRY RobQuantityPropertySetValue(const char *quantitysetID, const char *quantityID,const char *propertyID, const char *propertyValue)
{
    short           b = quantitysetID == NULL || strlen(quantitysetID) == 0;
    RobQuantityPntr p = (b) ? RobQuantityFind(quantityID) : RobQuantityFind(quantitysetID);
    RobPropertyPntr pP;
    int rv=0;

    if(p!=NULL && !b) p=findQuantity(p->pQuantities,quantityID); /* search for quantity in set */

    if(p!=NULL)
    {
        if((pP=RobFindProperty(p->pProperties, propertyID)) != NULL)
        {
            rv = 1;
            RobFree(pP->PropertyValue);
            pP->PropertyValue = RobCopyText(propertyValue);
        }
    }
    return rv;
}

/// <summary>
/// Get the value string for a quantity's property)
/// </summary>
/// <param name="quantitysetID">quantity identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="PropertyID">property description string</param>
/// <returns>return value</returns>
char* APIENTRY RobQuantityPropertyGetValue(const char *quantitysetID, const char *quantityID,const char *propertyID)
{
    short           b = quantitysetID == NULL || strlen(quantitysetID) == 0;
    RobQuantityPntr p = (b) ? RobQuantityFind(quantityID) : RobQuantityFind(quantitysetID);
    RobPropertyPntr pP;
    char *rv=NULL;

    if(p!=NULL && !b) p=findQuantity(p->pQuantities,quantityID); /* search for quantity in set */

    if(p!=NULL)
    {
        pP=RobFindProperty(p->pProperties, propertyID);
        if(pP != NULL) rv = RobCopyText(pP->PropertyValue);
    }
    return rv;
}

// For Fortran compatibility
int APIENTRY RobQuantityProperty(const char *quantitysetID, const char *quantityID,const char *propertyID, char *propertyValue)
{
    char* p=NULL;
    if ( (p=RobQuantityPropertyGetValue(quantitysetID,quantityID,propertyID)) != NULL) strcpy(propertyValue,p);
    return(propertyValue == NULL)? 0 : 1;
}


short APIENTRY RobIsQuantityset(const char *quantityID)
{
    RobQuantityPntr p = RobQuantityFind(quantityID);
    short rv=0;

    if(p != NULL) rv = (p->pQuantities !=NULL)? 1 : 0;
    return(rv);
}

int RobQuantityIndexInSet(const char *quantitysetID, const char *quantityID)
{
    RobQuantityPntr p = RobQuantityFind(quantitysetID);
    RobQuantityPntr p0=NULL, p1=NULL;
    int Offset =0,j=0;
    if(p !=NULL)
    {
        p0=findQuantity(p->pQuantities,quantityID);  p1=p->pQuantities;
	if ( p0 == NULL )
	{
            return -1;
	}
	else
	{
            while(p0 != p1 && p1!=NULL)
            {
                Offset+=1; p1=p1->pNext;
            }
            return Offset;
        }
    }
    else
    {
        return 0;
    }
}

