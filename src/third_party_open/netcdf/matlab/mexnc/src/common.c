/**********************************************************************
 *
 * common.c
 *
 * This file contains routines that are used or are intended for use
 * by the other source files of mexnc.
 *
 *********************************************************************/

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"


/*
 * This variable is used in almost all routines, so I declare it 
 * globally for this file.
 * */
static char	error_message[1000];


/*
 * mexncCreateDoubleScalar:
 *
 * The netcdf-3 mexnc was originally written on an R13 platform.  
 * Backporting to R11 created a conflict with the mxCreateDoubleScalar
 * routine, which was not available to the R11.  Since the equivalent 
 * code would be cumbersome, it was decided to just stub out all calls
 * to mxCreateDoubleScalar and handle all the differences in just one
 * place.
 * */
mxArray *mexncCreateDoubleScalar ( double value ) {

	mxArray *mx;

#ifdef MEXNC_R11
	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int             mx_count_coord[1]; 

	/*
	 * Pointer to data part of the matrix.
	 * */
	double          *pr;

	mx_count_coord[0] = 1;

	mx = mxCreateNumericArray ( 1, mx_count_coord, mxDOUBLE_CLASS, mxREAL );
	pr = mxGetPr ( mx );
	pr[0] = value;

#else

	mx = mxCreateDoubleScalar ( value );

#endif

	return ( mx );

}










#ifdef MEXNCR12
mxArray *mxCreateDoubleScalar ( double value ) {

	mxArray *mx;

	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int             mx_count_coord[1]; 

	/*
	 * Pointer to data part of the matrix.
	 * */
	double          *pr;

	mx_count_coord[0] = 1;

	mx = mxCreateNumericArray ( 1, mx_count_coord, mxDOUBLE_CLASS, mxREAL );
	pr = mxGetPr ( mx );
	pr[0] = value;

	return ( mx );

}
#endif











void Usage ( ) {
	mexPrintf ( "You must have at least one input argument.  Try\n\n" );
	mexPrintf ( "    >> help mexcdf53\n\n" );
}






/*
 * Mat2Size_t:  
 *
 * Turns values in an mxArray into a size_t array.
 * */
size_t * Mat2Size_t ( const mxArray *mat )
{
	double	*	pr;
	size_t		*	pint;
	size_t		*	p;
	int			len;
	int			i;

	len = mxGetM(mat) * mxGetN(mat);
	
	pint = (size_t *) mxCalloc(len, sizeof(size_t));
	p = pint;
	pr = mxGetPr(mat);
	
	for (i = 0; i < len; i++)	{
		*p++ = (size_t) *pr++;
	}
	
	return (pint);
}









/*
 * Mat2Ptrdiff_t:  
 *
 * Turns values in an mxArray into a ptrdiff_t array.
 * */
ptrdiff_t * Mat2Ptrdiff_t ( const mxArray *mat )
{
	double	*	pr;
	ptrdiff_t		*	pint;
	ptrdiff_t		*	p;
	int			len;
	int			i;

	len = mxGetM(mat) * mxGetN(mat);
	
	pint = (ptrdiff_t *) mxCalloc(len, sizeof(ptrdiff_t));
	p = pint;
	pr = mxGetPr(mat);
	
	for (i = 0; i < len; i++)	{
		*p++ = (size_t) *pr++;
	}
	
	return (pint);
}









/**********************************************************************
 *
 * CHECK_CHAR_ARGUMENT_TYPE
 *
 * Verifies that a parameter passed in from matlab is character.
 *
 * PARAMETERS:
 * mx:
 *     matlab array whom we wish to verify
 * opname:
 *     name of the mexnc operation whose argument is being verified.
 * idx:
 *     Index of the calling parameter in question.
 * 
 *********************************************************************/
void check_char_argument_type 
( 
	const mxArray *mx[], 
	char          *opname, 
	int            idx
) 
{


	char *error_fmt 
		= { "argument in position %d must be character, operation \"%s\"\n" };

	if ( mxIsChar(mx[idx]) == false ) {
		sprintf ( error_message, error_fmt, idx+1, opname );
		mexErrMsgTxt ( error_message );
	}


}












/**********************************************************************
 *
 * CHECK_CHAR_OR_ARGUMENT_TYPE
 *
 * Check as to whether an argument is character or numeric datatype.
 * PARAMETERS:
 * mx:
 *     matlab array whom we wish to verify
 * opname:
 *     name of the mexnc operation whose argument is being verified.
 * idx:
 *     Index of the calling parameter in question.
 * 
 *********************************************************************/
void check_char_or_numeric_argument_type 
( 
	const mxArray *mx[], 
	char          *opname, 
	int            idx
) 
{


	char *error_fmt 
		= { "argument in position %d must be either character or numeric, operation \"%s\"\n" };

	if ( !( mxIsChar(mx[idx]) || mxIsNumeric(mx[idx]) ) ) {
		sprintf ( error_message, error_fmt, idx+1, opname );
		mexErrMsgTxt ( error_message );
	}


}





/**********************************************************************
 *
 * CHECK_MODE_ARGUMENT_TYPE
 *
 * Verifies that the input matlab parameter is the proper datatype for
 * a "mode" argument.
 * 
 * PARAMETERS:
 * mx:
 *     matlab array whom we wish to verify
 * opname:
 *     name of the mexnc operation whose argument is being verified.
 * idx:
 *     Index of the calling parameter in question.
 *
 *********************************************************************/
void check_mode_argument_type 
( 
    const mxArray *mode_mx[], 
    char *opname, 
    int idx 
) 
{


	if ( !((mxIsChar(mode_mx[idx]) == true) || (mxIsDouble(mode_mx[idx]) == true )) ) {
		sprintf ( error_message, "the mode argument in position %d of operation %s must be either matlab native double precision or a string corresponding to the mode", idx+1, opname );
		mexErrMsgTxt ( error_message );
	}

}

















/**********************************************************************
 *
 * CHECK_NUMERIC_ARGUMENT_TYPE
 *
 * Check the argument as to whether it is an acceptable numeric
 * PARAMETERS:
 * mx:
 *     matlab array whom we wish to verify
 * opname:
 *     name of the mexnc operation whose argument is being verified.
 * idx:
 *     Index of the calling parameter in question.
 * 
 *********************************************************************/
void check_numeric_argument_type 
( 
	const mxArray *mx[], 
	char          *opname, 
	int            idx
) 
{

	char *error_fmt 
		= { "argument in position %d must be numeric, operation \"%s\"\n" };

	if ( mxIsChar(mx[idx]) ) {
		sprintf ( error_message, error_fmt, idx+1, opname );
		mexErrMsgIdAndTxt ( "MEXNC:checkNumericArgumentType:wasChar", error_message );
	}

}










/***********************************************************************
 *
 * INTERPRET_CHAR_PARAMETER
 *
 * For backwards compatibility, some matlab inputs are done with 
 * character strings, such as using 'NC_CHAR' for a variable 
 * declaration in DEF_VAR instead of nc_char.  We have to be able to
 * handle that.  This code is a clean implementation of the netcdf2 
 * routine "Parameter", which accomplished the same thing.
 *
 * Parameters:
 * mx:
 *     matlab array that contains a character string
 *
 * Return Value:
 *     If the parameter is identified, it is returned as an integer
 *     value.  Otherwise an exception is thrown.
 *
 **********************************************************************/
int interpret_char_parameter ( const mxArray *mx )
{

	/*
	 * Define a datastructure to hold the enumerated constant and
	 * the string equivalent.
	 * */
	typedef struct nc_constant_struct	{
		int      code;
		char    *name;
	} nc_const;

	nc_const ncconst[] =	{
	
		/*
		 * Datatypes.
		 * */
		{ NC_BYTE,                    "BYTE"                    }, 
		{ NC_CHAR,                    "CHAR"                    }, 
		{ NC_DOUBLE,                  "DOUBLE"                  }, 
		{ NC_FLOAT,                   "FLOAT"                   }, 
		{ NC_INT,                     "INT"                     }, 
		{ NC_LONG,                    "LONG"                    }, 
		{ NC_NAT,                     "NAT"                     }, 
		{ NC_SHORT,                   "SHORT"                   }, 

		/*
		 * Open and create modes.
		 * */
		{ NC_CLOBBER,                 "CLOBBER"                 }, 
		{ NC_LOCK,                    "LOCK"                    }, 
		{ NC_NOCLOBBER,               "NOCLOBBER"               }, 
		{ NC_NOWRITE,                 "NOWRITE"                 }, 
		{ NC_SHARE,                   "SHARE"                   }, 
		{ NC_WRITE,                   "WRITE"                   }, 
		{ NC_64BIT_OFFSET,            "64BIT_OFFSET"            }, 
		{ NC_NETCDF4,                 "NETCDF4"                 }, 
		{ NC_CLASSIC_MODEL,           "CLASSIC_MODEL"           }, 

		/*
		 * Format modes.
		 * */
		{ NC_FORMAT_CLASSIC,          "FORMAT_CLASSIC"          }, 
		{ NC_FORMAT_64BIT,            "FORMAT_64BIT"            }, 


		/*
		 * Error codes.
		 * */
		{ NC_NOERR,                   "NOERR"                   }, 
		{ NC_EBADID,                  "EBADID"                  }, 
		{ NC_ENFILE,                  "ENFILE"                  }, 
		{ NC_EEXIST,                  "EEXIST"                  }, 
		{ NC_EINVAL,                  "EINVAL"                  }, 
		{ NC_EPERM,                   "EPERM"                   }, 
		{ NC_ENOTINDEFINE,            "ENOTINDEFINE"            }, 
		{ NC_EINDEFINE,               "EINDEFINE"               }, 

		/*
		 * -40
		 *  */
		{ NC_EINVALCOORDS,            "EINVALCOORDS"            }, 
		{ NC_EMAXDIMS,                "EMAXDIMS"                }, 
		{ NC_ENAMEINUSE,              "ENAMEINUSE"              }, 
		{ NC_ENOTATT,                 "ENOTATT"                 }, 
		{ NC_EMAXATTS,                "EMAXATTS"                }, 
		{ NC_EBADTYPE,                "EBADTYPE"                }, 
		{ NC_EBADDIM,                 "EBADDIM"                 }, 
		{ NC_EUNLIMPOS,               "EUNLIMPOS"               }, 
		{ NC_EMAXVARS,                "EMAXVARS"                }, 
		{ NC_ENOTVAR,                 "ENOTVAR"                 }, 

		/*
		 * -50
		 *  */
		{ NC_EGLOBAL,                 "EGLOBAL"                 }, 
		{ NC_ENOTNC,                  "ENOTNC"                  }, 
		{ NC_ESTS,                    "ESTS"                    }, 
		{ NC_EMAXNAME,                "EMAXNAME"                }, 
		{ NC_EUNLIMIT,                "EUNLIMIT"                }, 
		{ NC_ENORECVARS,              "ENORECVARS"              }, 
		{ NC_ECHAR,                   "ECHAR"                   }, 
		{ NC_EEDGE,                   "EEDGE"                   }, 
		{ NC_ESTRIDE,                 "ESTRIDE"                 }, 
		{ NC_EBADNAME,                "EBADNAME"                }, 

		/*
		 * -60
		 *  */
		{ NC_ERANGE,                  "ERANGE"                  }, 
		{ NC_ENOMEM,                  "ENOMEM"                  }, 
		{ NC_EVARSIZE,                "EVARSIZE"                }, 
		{ NC_EDIMSIZE,                "EDIMSIZE"                }, 



		{ NC_FATAL,                   "FATAL"                   }, 
		{ NC_FILL,                    "FILL"                    }, 
		{ NC_GLOBAL,                  "GLOBAL"                  }, 
		{ NC_NOFILL,                  "NOFILL"                  }, 
		{ NC_UNLIMITED,               "UNLIMITED"               }, 
		{ NC_SIZEHINT_DEFAULT,        "SIZEHINT_DEFAULT"        }, 
		{ NC_VERBOSE,                 "VERBOSE"                 }, 

		/*
		 * Chunking.
		 * */
		{ NC_CONTIGUOUS,              "CONTIGUOUS"              }, 
		{ NC_CHUNKED,                 "CHUNKED"                 }, 


		{ 0,                          "NONE"                    }
	};





	int	parameter;

	/*
	 * Character pointers for manipulating the parameter.
	 * */
	char	*p, *q;
	
	/*
	 * Loop index 
	 * */
	int	i;


	/*
	 * flag as to whether or not we found the parameter in the
	 * list of candidates.
	 * */
	int did_not_find_it;
	
	parameter = -1;
	
	p = unpackString ( mx );
	
	/*
	 * Make it upper case.
	 * */
	q = p;
	for (i = 0; i < strlen(p); i++)	{
		*q = (char) toupper((int) *q);
		q++;
	}

	/*
	 * Trim away any leading "NC_".
	 * */
	if (strncmp(p, "NC_", 3) == 0)	{	
		q = p + 3;
	}
	else	{
		q = p;
	}
	
	/*
	 * Loop thru the known parameter list.  See if we can't identify
	 * the parameter.
	 * */
	i = 0;
	did_not_find_it = 1;
	while (strcmp(ncconst[i].name, "NONE") != 0)	{
		if (strcmp( q, ncconst[i].name ) == 0)	{
			parameter = ncconst[i].code;
			did_not_find_it = 0;
			break;
		}
		else	{
			i++;
		}
	}

	if ( did_not_find_it ) {
	        sprintf ( error_message, "unable to identify parameter \"%s\"\n", p );
	        mexErrMsgTxt ( error_message );
	}
	return (parameter);
	
}









/***********************************************************************
 *
 * SET_OUTPUT_MATRIX_SIZE
 *
 * We need to set the dimensions of the matrix as the reverse of how
 * it is defined in the netcdf file.  This makes the size of the matrix
 * seem transposed (upon return to matlab), but otherwise the data gets
 * layed out incorrectly due to the difference between row-major order
 * (C) and column-major order (matlab).
 *
 * PARAMETERS:
 * ncid:
 *     NetCDF file ID
 * num_nc_dims:
 *     Number of dimensions for the current variable.  The variable
 *     itself is not required here.
 * dimids:
 *     Array of dimension IDs
 * nc_count_coords:
 *     NetCDF enumerated type of a compound in question.   
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 * mx_rank:
 *     The size of the output matrix is set in this array.
 *
 *
 **********************************************************************/
void set_output_matrix_rank 
( 
    int     ncid, 
    int     num_nc_dims,
    int    *dimids, 
    size_t *nc_count_coords, 
    op     *nc_op, 
    int    *mx_rank 
    ) 
{

    /*
     * Size of a dimension.
     * */
    size_t dimlen;

    /*
     * Loop index.
     * */
    int j;


    /*
     * Return status from netcdf operation.
     * */
    int      status;

    /*
     * Determine the size of the output matrix.
     *
     * In the case of a singleton or 1D netcdf array, we then assume
     * that the output matrix is a 2D.
     * */
    mx_rank[0] = 1;
    mx_rank[1] = 1;

    switch ( nc_op->opcode ) {

        /*
         * The output size is determined by the size of the netcdf
         * variable.
         * */
        case GET_VAR_DOUBLE:
        case GET_VAR_FLOAT:
        case GET_VAR_INT:
        case GET_VAR_SHORT:
        case GET_VAR_SCHAR:
        case GET_VAR_UCHAR:
        case GET_VAR_TEXT:
            for ( j = 0; j < num_nc_dims; ++j ) {
                status = nc_inq_dimlen ( ncid, dimids[j], &dimlen );
                if ( status != NC_NOERR ) {
                    sprintf ( error_message, "nc_inq_dimlen failed, line %d, file %s\n", __LINE__, __FILE__ );
                    mexErrMsgTxt ( error_message );
                }
        
                mx_rank[num_nc_dims - j - 1] = dimlen;
            }
            break;


        /*
         * We're already done in the case of a single-element grab.
         * */
        case GET_VAR1_DOUBLE:
        case GET_VAR1_FLOAT:
        case GET_VAR1_INT:
        case GET_VAR1_SHORT:
        case GET_VAR1_SCHAR:
        case GET_VAR1_UCHAR:
        case GET_VAR1_TEXT:
            break;


        /* 
         * The size is determined by the user!
         * */
        case GET_VARA_DOUBLE:
        case GET_VARA_FLOAT:
        case GET_VARA_INT:
        case GET_VARA_SHORT:
        case GET_VARA_SCHAR:
        case GET_VARA_UCHAR:
        case GET_VARA_TEXT:
        case GET_VARS_DOUBLE:
        case GET_VARS_FLOAT:
        case GET_VARS_INT:
        case GET_VARS_SHORT:
        case GET_VARS_SCHAR:
        case GET_VARS_UCHAR:
        case GET_VARS_TEXT:
            for ( j = 0; j < num_nc_dims; ++j ) {
                mx_rank[num_nc_dims - j - 1] = nc_count_coords[j];
            }
            break;


        default:
			sprintf ( error_message, 
                        "unhandled opcode %d, %s, line %d, file %s\n", 
                        nc_op->opcode, nc_op->opname, 
                        __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );

    }



    return;

}













/***********************************************************************
 *
 * unpackDataType
 *
 * This routine unpacks an nc_type from a matlab matrix.  This function
 could be called, say, from an invocation of
 *
 * status = mexnc ( 'PUT_ATT', ncid, varid, attname, xtype, attlength, attdata );
 *
 * where "xtype" could be the numeric representation of the datatype, 
 * such as NC_INT (this is the preferred way, please follow this 
 * convention), or alternatively possibly "int" or "nc_int" instead.  
 * This opens a can of worms due to the fact that NetCDF-4 allows for 
 * user-defined datatypes that we cannot possibly know about 
 * beforehand.  So, only atomic types will be checked for string 
 * versions.  All other types MUST be the numeric representation.
 *
 * Input:
 *     mx:  a matlab array 
 * Output:
 *     The numeric representation of the intended datatype.
 *
 **********************************************************************/
nc_type unpackDataType ( const mxArray *mx ) {

	/*
	 * Shortcut to matlab matrix data.
	 * */
	double *pr;

	/*
	 * Original version of extracted string value.
	 * */
	char *typeString;


	/*
	 * typeString as converted to upper case and possibly stripped
	 * of the leading three characters.
	 * */
	char *p;

	/*
	 * Length of the string.
	 * */
	int num_chars;


	/*
	 * Loop index.
	 * */
	int	j;

	if ( mxIsNumeric ( mx )  ) {

		/*
		 * This part is easy.
		 * */
		pr = mxGetPr ( mx );
		return ( (nc_type)pr[0] );

	}



	/*
	 * Ok, the g****** user must have given us a character string.
	 * First, extract it and convert it to upper case.
	 * */
	typeString = mxArrayToString ( mx );
	p = typeString;
	num_chars = strlen ( typeString );
	for ( j = 0; j < num_chars; ++j ) {
		p[j] = toupper ( p[j] );
	}


	/*
	 * if the leading three characters are "NC_", then shave
	 * it off.  See how problematic this is getting???
	 * */
	if ( strncmp ( typeString, "NC_", 3 ) == 0 ) {
		p = & p[2];
	}

	/*
	 * Now check it against the atomic types that we know about.
	 * */
	if ( strcmp ( p, "NAT" ) == 0 ) {
		return ( NC_NAT );
	} else if ( strcmp ( p, "BYTE" ) == 0 ) {
		return ( NC_BYTE );
	} else if ( strcmp ( p, "CHAR" ) == 0 ) {
		return ( NC_CHAR );
	} else if ( strcmp ( p, "SHORT" ) == 0 ) {
		return ( NC_SHORT );
	} else if ( strcmp ( p, "INT" ) == 0 ) {
		return ( NC_INT );
	} else if ( strcmp ( p, "LONG" ) == 0 ) {
		return ( NC_LONG );
	} else if ( strcmp ( p, "FLOAT" ) == 0 ) {
		return ( NC_FLOAT );
	} else if ( strcmp ( p, "DOUBLE" ) == 0 ) {
		return ( NC_DOUBLE );
	} else {
	        sprintf ( error_message, "unable to identify parameter \"%s\"\n", p );
	        mexErrMsgTxt ( error_message );
	}

	/*
	 * We should never encounter this...
	 * */
        sprintf ( error_message, "unable to identify parameter \"%s\"\n", p );
        mexErrMsgTxt ( error_message );
	return ( NC_NAT );

}











/***********************************************************************
 *
 * unpackPtrdiff_t
 *
 * This routine unpacks ptrdiff_t data from a matlab matrix.
 *
 * Input:
 *     mx:  a matlab array 
 * Output:
 *     The function's return value is a pointer to the unpacked string.
 *
 **********************************************************************/
ptrdiff_t * unpackPtrdiff_t ( const mxArray *mx ) {

	/*
	 * Shortcut to matlab matrix data.
	 * */
	double *pr;


	/*
	 * C array that will hold the unpacked ptrdiff_t data.
	 * */
	ptrdiff_t *ptrdiff_tp;

	/*
	 * Number of elements in the size_t array.
	 * */
	int     buflen;

	/*
	 * Loop index.
	 * */
	int	i;


	buflen = mxGetM(mx) * mxGetN(mx);
	
	ptrdiff_tp = (ptrdiff_t *) mxCalloc(buflen, sizeof(ptrdiff_t));

	pr = mxGetPr(mx);
	
	for (i = 0; i < buflen; i++)	{
		ptrdiff_tp[i] = (ptrdiff_t) (pr[i]);
	}
	
	return (ptrdiff_tp);
}












/***********************************************************************
 *
 * unpackSize_t
 *
 * This routine unpacks size_t data from a matlab matrix.
 *
 * Input:
 *     mx:  a matlab array 
 * Output:
 *     The function's return value is a pointer to the unpacked string.
 *
 **********************************************************************/
size_t * unpackSize_t ( const mxArray *mx ) {

	/*
	 * Shortcut to matlab matrix data.
	 * */
	double *pr;


	/*
	 * C array that will hold the unpacked size_t data.
	 * */
	size_t *size_tp;

	/*
	 * Number of elements in the size_t array.
	 * */
	int     buflen;

	/*
	 * Loop index.
	 * */
	int	i;


	buflen = mxGetM(mx) * mxGetN(mx);
	
	size_tp = (size_t *) mxCalloc(buflen, sizeof(size_t));

	pr = mxGetPr(mx);
	
	for (i = 0; i < buflen; i++)	{
		size_tp[i] = (size_t) (pr[i]);
	}
	
	return (size_tp);
}











/***********************************************************************
 *
 * unpackString
 *
 * This routine unpacks a (char *) string from a matlab matrix and
 * checks the return status appropriately.
 *
 * Input:
 *     mx:  a matlab array 
 * Output:
 *     The function's return value is a pointer to the unpacked string.
 *
 **********************************************************************/
char * unpackString ( const mxArray *mx ) {

	/*
	 * Defines the size and value of the extracted string.
	 * */
	char *theString;
	int   buflen;

	/*
	 * success or failure of mxGetString function call
	 * */
	int   status;           

	buflen = mxGetM(mx)*mxGetN(mx) + 1;
	theString = mxCalloc ( buflen, sizeof(char) );
	status = mxGetString( mx, theString, buflen);
	if ( status == 1 ) {
		sprintf(error_message, "mxGetString failed, file %s, line %d\n",  __FILE__, __LINE__); 
		mexErrMsgTxt ( error_message ); 
	}
	
	return (theString);
}





/***********************************************************************
 *
 * UNPACK_CHAR_FILE_MODE:
 *
 * For historical (and bad) reasons, the users have been getting away
 * with using character strings for the mode being passed into nc_open.
 * For backwards compatibility purposes, we need to be able to handle
 * this.
 *
 * PARAMETERS:
 * string_mode:
 *     Can only be a string for one of the earliest codes, such as
 *     "nc_clobber", "nc_noclobber", "nc_write", or "nc_nowrite".  
 *     Can also be capitalized, or missing the leading "nc_".
 *
 * RETURN VALUE:
 *     Integer mode corresponding to the string mode.
 *
 **********************************************************************/
int unpack_char_file_mode ( char *input_string_mode ) {

    /*
     * This is the integer mnemonic code that we will be passing into
     * nc_open.  If the user hadn't been such a complete asshole, this
     * is what they would have given us in the first place.
     * */
    int cmode;

    /*
     * Loop index.
     * */
    int j;

    /*
     * Same as the input mode, but possibly trimmed of any leading "NC_"
     * */
    char *trimmed_mode;


    /*
     * Convert it to lower case.
     * */
    for ( j = 0; j < strlen(input_string_mode); ++j ) {
        input_string_mode[j] = toupper ( input_string_mode[j] );
    }


    /*
     * Trim off any leading "NC_"
     * */
    if ( strncmp ( input_string_mode, "NC_", 3 ) == 0 ) {
        trimmed_mode = &input_string_mode[3];
    } else {
        trimmed_mode = input_string_mode;
    }

    /*
     * Compare to any of the earlier NC2 modes.  For anything more advanced,
     * the user has to use numerical codes.
     * */
    if ( strcmp(trimmed_mode,"CLOBBER") == 0 ) {
        cmode = NC_CLOBBER;
    } else if ( strcmp(trimmed_mode,"NOCLOBBER") == 0 ) {
        cmode = NC_NOCLOBBER;
    } else if ( strcmp(trimmed_mode,"WRITE") == 0 ) {
        cmode = NC_WRITE;
    } else if ( strcmp(trimmed_mode,"NOWRITE") == 0 ) {
        cmode = NC_NOWRITE;
    } else {
        sprintf ( error_message, "unknown parameter \"%s\", line %d file \"%s\"\n", input_string_mode, __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
    }

    return ( cmode );


}









/***********************************************************************
 *
 * VARM_COORD_SANITY_CHECK
 *
 * Check that the lengths of the input "start", "count", "stride", and 
 * "imap" matrices match the rank of the netcdf variable.
 *
 * Inputs:
 *     prhs:
 *         Array of matlab array structures.
 *     ndims:
 *         This is the rank of the netcdf variable being worked with.
 *
 * Outputs:
 *     None.  If the routine does not succeed, a matlab exception is 
 *     thrown.
 *
 **********************************************************************/
void varm_coord_sanity_check ( const mxArray *prhs[], int ndims ) {

	/*
	 * Loop index.
	 * */
	int j;

	/*
	 * number of rows, columns of "start", "count", "stride", and "imap" coord arrays.
	 * */
	int m, n;

	/*
	 * length of "start", "count", "stride", and "imap" coord arrays
	 * */
	int vector_length;



	for ( j = 3; j < 7; ++j ) {

		/*
		 * Check that the input coordinates are 1-D arrays.
		 */
		m = mxGetM ( prhs[j] );
		n = mxGetN ( prhs[j] );
		if ( ( m == 1 ) || ( n == 1 ) ) {
			;	
		} else {
			sprintf ( error_message, "input array %d must be a 1D vector, not %dx%d\n", j, m, n );
			mexErrMsgTxt ( error_message );
		}



		/*
		 * Check that the vector length is ok.  Must be the same as netcdf variable rank.
		 */
		vector_length = m*n;

		if ( vector_length != ndims ) {
			sprintf ( error_message, "input array %d  must have a length equal to the netcdf variable rank, %d\n", j, ndims );
			mexErrMsgTxt ( error_message );
		}

	}



}










