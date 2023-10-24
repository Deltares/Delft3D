// Expose METIS options enumeration, for Fortran calls for example
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_METIS

#ifdef WIN32 
#   define strcasecmp _stricmp 
#endif


#include "metis.h"
#include <string.h>

/// Fortan example: ierror = metisopts(opts,"CONTIG"//char(0),1)
/// set value 1 at location METIS_OPTION_CONTIG in opts array

int metisoptions(int *options, const char *optionname, const int *optionval)
{
	int i = -1;

	if ( strcasecmp(optionname,"NITER")==0 )
	{
		i = (int)METIS_OPTION_NITER;
	}
	else if ( strcasecmp(optionname,"CONTIG")==0 )
	{
		i = (int)METIS_OPTION_CONTIG;
	}
	else if ( strcasecmp(optionname,"DBGLVL")==0 )
	{
		i = (int)METIS_OPTION_DBGLVL;
	}
	else if ( strcasecmp(optionname,"UFACTOR")==0 )
	{
		i = (int)METIS_OPTION_UFACTOR;
	}
	else if ( strcasecmp(optionname,"NCUTS")==0 )
	{
		i = (int)METIS_OPTION_NCUTS;
	}
	else if ( strcasecmp(optionname,"SEED")==0 )
	{
		i = (int)METIS_OPTION_SEED;
	}
	
	if (i>=0)
	{
		options[i] = *optionval;
		return 0;	// no error
	}
	else
	{
		return 1;	// error
	}
}

int METISOPTIONS(int *options, const char *optionname, const int *optionval)
{
	return metisoptions(options, optionname, optionval);
}

int metisoptions_(int *options, const char *optionname, const int *optionval)
{
	return metisoptions(options, optionname, optionval);
}

int metisoptions__(int *options, const char *optionname, const int *optionval)
{
	return metisoptions(options, optionname, optionval);
}


#endif