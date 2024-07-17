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
/// Remove a model from memory
/// </summary>
/// <param name="p">pointer to the model</param>
/// <param name="modelID">model identification string</param>
/// <returns>pointer to a model</returns>
static RobModelPntr freeModel (RobModelPntr p, const char *modelID )
{
  RobModelPntr retval;

  if (p != NULL)
  {
    if ( RobStringCompare(modelID, p->modelID) == 0)
    {
      RobFree(p->modelDescription);
      RobFree(p->modelID);
    }

    retval = p->pNext;

    RobFree(p);

    return retval;
  }
  else
  {
    p->pNext = freeModel(p->pNext, modelID);
    return p;
  }
}

/// <summary>
/// Remove all models from memory
/// </summary>
void RobFreeModels(void)
{
  while (RobModels != NULL)
  {
    RobModels = freeModel(RobModels, RobModels->modelID );
  }
}

/// <summary>
/// dump all units to file
/// </summary>
void RobDumpModels(FILE *stream)
{
  RobModelPntr p;

  p = RobModels;
  if (p != NULL)
  {
    while (p)
    {
      fprintf (stream , "\tID                : %s\n", p->modelID);
      fprintf (stream , "\tDescription       : %s\n\n", p->modelDescription);
      p = p->pNext;
    }
  }
}

/// <summary>
/// add a model in memory
/// </summary>
/// <param name="p1">pointer to the models</param>
/// <param name="p2">pointer to the current model</param>
/// <returns>pointer to the models</returns>
static RobModelPntr addModel (RobModelPntr p1, RobModelPntr p2 )
{
  if (p1 == NULL)
  {
    p1 = p2;
    p1->pNext = NULL;
  }
  else
  {
    p1->pNext = addModel(p1->pNext , p2);
  }
  return p1;
}


static RobModelPntr findModel (RobModelPntr p, const char *modelID )
{
  if (p != NULL)
  {
    if  ( RobStringCompare(modelID, p->modelID) == 0)
    {
      return p;
    }
    else
    {
      return (findModel (p->pNext , modelID));
    }
  }
  return(NULL);
}

//
// Check if this Model exists
//
RobModelPntr RobModelFind (const char *modelID)
{
  return (findModel(RobModels, modelID));
}

/// <summary>
/// Add a Model
/// </summary>
/// <param name="modelID">model identification</param>
/// <param name="modelDescription">model description</param>
/// <>
int APIENTRY RobModelAdd (const char *modelID, const char *modelDescription)
{
  RobModelPntr        pM;

  pM = RobModelFind(modelID);
  if (pM == NULL)
  {
    pM = (RobModelPntr) RobMalloc(sizeof(RobModel));

    pM->modelID = RobCopyText( modelID );
    pM->modelDescription = RobCopyText( modelDescription );
    pM->pNext = NULL;

    RobModels = addModel(RobModels, pM);

    return (0);
  }

  sprintf(Message.Description, "Model %s already exists", modelID);
  Message.Code = 1;
  return (1);
}

int APIENTRY RobModelCount(void)
{
  RobModelPntr  p;
  int           count = 0;

  p = RobModels;
  if (p != NULL)
  {
    while (p)
    {
      count++;
      p = p->pNext;
    }
  }
  return count;
}


/// <summary>
/// Get information of a model
/// </summary>
/// <param name="index">index in the models</param>
/// <param name="modelID">model identification string</param>
/// <param name="modelDescription">model description string</param>
/// <returns>return value</returns>
int APIENTRY RobModelGetInfo(int index, char *modelID, char *modelDescription)
{
  RobModelPntr  p;
  int           count = 0;

  p = RobModels;
  if (p != NULL)
  {
    while ( (p) &&  (index != count))
    {
      count++;
      if (index == count)
      {
        int   l;

        l = strlen(modelID);
        strncpy(modelID, p->modelID, l);

        l = strlen(modelDescription);
        strncpy(modelDescription, p->modelDescription, l);
      }
      p = p->pNext;
    }
    return 0;
  }

  sprintf(Message.Description, "Model %s does not exist", modelID);
  Message.Code = -1;
  return -1;

}
