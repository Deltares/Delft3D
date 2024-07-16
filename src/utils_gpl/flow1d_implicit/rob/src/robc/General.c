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


void * RobMalloc ( long size )
{
  return calloc ( 1, size );
}

void * RobRealloc ( void * p, long size )
{
  return realloc ( p, size );
}

char * RobCopyText(const char * text)
{
  char * return_value = NULL;

  if ( text != (char *) NULL )
  {
    return_value = RobMalloc (( strlen(text)+1) * sizeof(char));
    if ( return_value != NULL )
    {
      (void)strcpy ( return_value, text );
    }
  }
  return return_value;
}

short RobStringCompare (const char * a, const char * b)
{
  size_t i;
  size_t j;

  for ( i = strlen(a) ;  a[--i] == ' ' ; ) ;
  for ( j = strlen(b) ;  b[--j] == ' ' ; ) ;
  if ( j == i )
  {
    return (short)strncmp ( a, b, ++j );
  }
  return (short)1;
}

