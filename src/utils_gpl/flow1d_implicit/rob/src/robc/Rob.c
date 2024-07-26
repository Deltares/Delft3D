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

FILE * logFileHandle=NULL;

MsgType Message;

double APIENTRY RobMissingValueGetValue(void)
{
    return MISSING_VALUE;
}
/// <summary>
/// Initialize RIZA OpenMI buffer
/// </summary>
int APIENTRY RobInitialize(const char * logFileName)
{
  if ( logFileName != NULL )
  {
    if ( strlen(logFileName) > 0 )
    {
      if ( logFileName[0] != ' ' )
      {
        logFileHandle = fopen(logFileName, "w");
      }
    }
  }
  RobModels = NULL;
  RobExchangeitems = NULL;
  RobQuantities = NULL;
  RobElementsets = NULL;
  RobUnits = NULL;
  Message.Description[0]= '\0';
  Message.Code			= 0;
  Message.Type			= 0;
  return (0);
}

/// <summary>
/// Get version info of the RIZA OpenMI buffer
/// </summary>
void APIENTRY RobVersion(char *version)
{
  sprintf(version, "Rob copyright © %s  Rijkswaterstaat/RIZA  version %s\n", ROB_VERYEAR, ROB_VERSION);
}

/// <summary>
/// Dump all of the RIZA OpenMI buffer
/// </summary>
void APIENTRY RobDump(const char *dumpFile)
{
  FILE *stream;
  char version[255];

  if( (stream = fopen( dumpFile, "w+" )) != NULL )
  {
    RobVersion(version);
    fprintf(stream, version);

    fprintf(stream, "\nUnits\n");
    RobDumpUnits(stream);

    fprintf(stream, "\nQuantities\n");
    RobDumpQuantities(stream);

    fprintf(stream, "\nModels\n");
    RobDumpModels(stream);

    fprintf(stream, "\nElementsets\n");
    RobDumpElementsets(stream);

    fprintf(stream, "\nExchange items\n");
    RobDumpExchangeitems(stream);

    fclose(stream);

  }
}

/// <summary>
/// Finalize RIZA OpenMI buffer
/// </summary>
void APIENTRY RobFinalize(void)
{
  RobFreeExchangeitems();
  RobFreeElementsets();
  RobFreeModels();
  RobFreeQuantities();
  RobFreeUnits();

  if ( logFileHandle != NULL )
  {
     fclose(logFileHandle);
  }
}

/// <summary>
/// Put integer values of a exchangeitem
/// </summary>
/// <param name="modelID">model identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="nElements">number of elements</param>
/// <param name="dValues">elements vector</param>
/// <returns>return value</returns>
int APIENTRY RobPutInt(const char *modelId, const char *quantitySetID,  const char *quantityID, const char *elementSetID, ROLE role, int nElements, int *iValues)
{
  RobExchangeitemPntr pEi;
  int                 i;
  int                 nQuantity=1;
  int                 result;
  int                 Offset=0;
  short               IsSet = !(quantitySetID == NULL || strlen(quantitySetID) == 0);

  if(IsSet)
  {
    if( (pEi = RobExchangeitemFind(modelId, quantitySetID, elementSetID, role)) != NULL)
    {
        i = RobQuantityGetInfo(quantitySetID,NULL,NULL,&nQuantity);
        Offset = RobQuantityIndexInSet(quantitySetID,quantityID) * pEi->nElements/(int)nQuantity;
    }
  }
  else
  {
    pEi = RobExchangeitemFind(modelId, quantityID, elementSetID, role);
  }

  if ( pEi != NULL && Offset >= 0 )
  {
    if (pEi->nElements > 0)
    {
      for (i=0; i < pEi->nElements/nQuantity ; i++)
      {
        pEi->values[Offset+i] = (nElements==1)? iValues[0] : iValues[i];
      }
      result = 0;
    }
    else
    {
	  sprintf(Message.Description, "number of elements is zero");
	  Message.Code = -2;
      result = -2;
    }
  }
  else
  {
    // Exchangeitem not found
	sprintf(Message.Description, "Exchange item: quantity %s, Elementset %s not found",
			quantityID, elementSetID);
	Message.Code = -1;
    result = -1;
  }

  return (result);
}


/// <summary>
/// Put real values of a exchangeitem
/// </summary>
/// <param name="modelID">model identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="nElements">number of elements</param>
/// <param name="dValues">elements vector</param>
/// <returns>return value</returns>
int APIENTRY RobPutReal(const char *modelId, const char *quantitySetID,  const char *quantityID, const char *elementSetID, ROLE role, int nElements, float *fValues)
{
  RobExchangeitemPntr pEi;
  int                 i;
  int                 nQuantity=1;
  int                 result;
  int                 Offset=0;
  short               IsSet = !(quantitySetID == NULL || strlen(quantitySetID) == 0);

  if(IsSet)
  {
    if( (pEi = RobExchangeitemFind(modelId, quantitySetID, elementSetID, role)) != NULL)
    {
        i = RobQuantityGetInfo(quantitySetID,NULL,NULL,&nQuantity);
        Offset = RobQuantityIndexInSet(quantitySetID,quantityID) * pEi->nElements/nQuantity;
    }
  }
  else
  {
    pEi = RobExchangeitemFind(modelId, quantityID, elementSetID, role);
  }

  if ( pEi != NULL && Offset >= 0 )
  {
    if (pEi->nElements > 0)
    {
      for (i=0; i < pEi->nElements/nQuantity ; i++)
      {
        pEi->values[Offset+i] = (nElements==1)? fValues[0] : fValues[i];
      }
      result = 0;
    }
    else
    {
	  sprintf(Message.Description, "Number of elements is 0 for exchange item %s / %s",
			  quantityID, elementSetID);
	  Message.Code = -2;
      result = -2;
    }
  }
  else
  {
    // Exchangeitem not found
	  sprintf(Message.Description, "Exchange item %s / %s not found",
			  quantityID, elementSetID);
	  Message.Code = -1;
    result = -1;
  }

  return (result);
}


/// <summary>
/// Put double values of a exchangeitem
/// </summary>
/// <param name="modelID">model identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="nElements">number of elements</param>
/// <param name="dValues">elements vector</param>
/// <returns>return value</returns>
int APIENTRY RobPutDouble(const char *modelId, const char *quantitySetID, const char *quantityID, const char *elementSetID, ROLE role, int nElements, double *dValues)
{
	RobExchangeitemPntr pEi;
	int                 i;
	int                 nQuantity=1;
	int                 result;
	int                 Offset=0;
	short               IsSet = !(quantitySetID == NULL || strlen(quantitySetID) == 0);

#if 1
	if ( logFileHandle != NULL )
	{
		fprintf(logFileHandle, "%s calls RobPutDouble for %s %s\t(es:%s)\n",
								( role == PROVIDING ) ? "Component" : "Wrapper",
								(IsSet) ? quantitySetID : "",
								quantityID,
								elementSetID);
		fflush(logFileHandle);
	}
#endif
	if(IsSet)
	{

		if((pEi = RobExchangeitemFind(modelId, quantitySetID, elementSetID, role)) != NULL)
		{
			i = RobQuantityGetInfo(quantitySetID,NULL,NULL,&nQuantity);
			if ( i == 0 )
			{
				if ( nQuantity > 0 )
				{
					Offset = RobQuantityIndexInSet(quantitySetID,quantityID) * pEi->nElements/nQuantity;
				}
				else
				{
					sprintf(Message.Description, "number of quantities in ElementSet %s / %s is zero",
						  quantitySetID, elementSetID);
					Message.Code = -3;
					return Message.Code;
				}
			}
		}
	}
	else
	{
		pEi = RobExchangeitemFind(modelId, quantityID, elementSetID, role);
	}

  if ( pEi != NULL && Offset >= 0 )
  {
    if (pEi->nElements > 0)
    {
      if ( logFileHandle != NULL )
      {
        fprintf(logFileHandle, "\t%s put value(s) for %s %s\t(es:%s)\n",
								( role == PROVIDING ) ? "Component" : "Wrapper",
                                (IsSet) ? quantitySetID : "",
		                            quantityID,
		                            elementSetID);
        fflush(logFileHandle);
      }
      for (i=0; i < pEi->nElements/nQuantity ; i++)
      {
        pEi->values[Offset+i] = (nElements==1)? dValues[0] : dValues[i];
        if ( logFileHandle != NULL )
        {
          fprintf(logFileHandle, "\t\tO=%3d\ti=%3d\t%g\n", Offset, i, pEi->values[Offset+i]);
          fflush(logFileHandle);
        }
      }
      result = 0;
    }
    else
    {
	  sprintf(Message.Description, "number of elements in Exchange item %s / %s is zero",
			  quantityID, elementSetID);
	  Message.Code = -2;
      result = -2;
    }
  }
  else
  {
    // Exchangeitem not found
	sprintf(Message.Description, "Exchange item %s / %s not found",
			  quantityID, elementSetID);
	Message.Code = -1;
    result = -1;
  }

  return (result);
}


/// <summary>
/// Get values of a exchangeitem
/// </summary>
/// <param name="modelID">model identification string</param>
/// <param name="quantityID">quantity identification string</param>
/// <param name="elementsetID">elementset identification string</param>
/// <param name="nElements">number of elements</param>
/// <param name="dValues">elements vector</param>
/// <returns>return value</returns>
int APIENTRY RobGetDouble(const char *modelId, const char *quantitySetID, const char *quantityID, const char *elementSetID, ROLE role, int nElements, double *values)
{
	RobExchangeitemPntr pEi;
	int                 i;
	int                 nQuantity=1;
	int                 result;
	int                 Offset = 0;
	short               IsSet = !(quantitySetID == NULL || strlen(quantitySetID) == 0);

#if 1
	if ( logFileHandle != NULL )
	{
		fprintf(logFileHandle, "%s calls RobGetDouble for %s %s\t(es:%s)\n",
								( role == ACCEPTING ) ? "Component" : "Wrapper",
								(IsSet) ? quantitySetID : "",
								quantityID,
								elementSetID);
		fflush(logFileHandle);
	}
#endif

	if(IsSet)
	{
		if((pEi = RobExchangeitemFind(modelId, quantitySetID, elementSetID, role)) != NULL)
		{
			i = RobQuantityGetInfo(quantitySetID,NULL,NULL,&nQuantity);
			if ( i == 0 )
			{
				if ( nQuantity > 0 )
				{
					Offset = RobQuantityIndexInSet(quantitySetID,quantityID) * pEi->nElements/nQuantity;
				}
				else
				{
					sprintf(Message.Description, "number of quantities in ElementSet %s / %s is zero",
						  quantitySetID, elementSetID);
					Message.Code = -3;
					return Message.Code;
				}
			}
		}
	}
	else
	{
		pEi = RobExchangeitemFind(modelId, quantityID, elementSetID, role);
	}

	if ( pEi != NULL && Offset >= 0 )
	{
		if (pEi->nElements > 0)
		{
			if ( (nElements >= pEi->nElements/nQuantity) )
			{
				int valuesAvailable = 0;
				if ( logFileHandle != NULL )
				{
					for (i=0; i < pEi->nElements/nQuantity; i++)
					{
						if ( pEi->values[Offset+i] != MISSING_VALUE )
						{
							valuesAvailable = 1;
							break;
						}
					}
					if ( valuesAvailable )
					{
						fprintf(logFileHandle, "\t%s got value(s) for %s %s\t(es:%s)\n",
												( role == ACCEPTING ) ? "Component" : "Wrapper",
												(IsSet) ? quantitySetID : "",
												quantityID,
												elementSetID);
												fflush(logFileHandle);
					}

				}

				for (i=0; i < pEi->nElements/nQuantity; i++)
				{
					if ( pEi->values[Offset+i] != MISSING_VALUE )
					{
						if ( logFileHandle != NULL )
						{
							fprintf(logFileHandle, "\t\tO=%3d\ti=%3d\t%g\n", Offset, i, pEi->values[Offset+i]);
							fflush(logFileHandle);
						}
					}
					values[i] = pEi->values[Offset+i];
				}

				result= 0;
			}
		}
		else
		{
			sprintf(Message.Description, "number of elements in Exchange item %s / %s is zero",
			quantityID, elementSetID);
			Message.Code = -2;
			result = -2;
		}
	}
	else
	{
		sprintf(Message.Description, "Exchange item %s / %s not found",
		quantityID, elementSetID);
		Message.Code = -1;
		result = -1;
	}
	return (result);
}

void APIENTRY RobGetLastError(int Code, char *Description, int *Type)
{
	if (Code == Message.Code)
	{
		if (Message.Description[0] != '\0')
		{
			strcpy(Description, Message.Description);
			*Type = Message.Type;
		}
		else
		{
			sprintf(Description, "Unknown error");
			*Type = 999;
		}
	}
	else
	{
		sprintf(Description, "Internal error inconsistent codes, last reported error : %s",
				Message.Description);
	}
}

