/**********************************************************************
 *
 * mexgateway.c
 *
 * This file functions as the mex-file entry point.  The intended mexnc
 * operation is gleaned from the first argument, and then we transfer
 * control to the source file that handles either the NetCDF-2 or
 * NetCDF-3 API.
 *
 *********************************************************************/

/*
 * $Id: mexgateway.c 2159 2007-03-06 16:50:52Z johnevans007 $
 * */

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"
# include "netcdf2.h"
# include "netcdf3.h"

static char *mexnc_date_id="Sat Apr 19 12:48:47 EDT 2008";
static char *mexnc_release_id="2.0.31";



op    *opname2opcode ( const char *, int, int );
void   get_mexnc_info ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], OPCODE );    


/******************************************************************************
 *
 * MEXFUNCTION
 *
 * Gateway routine for the mex-file.
 *
 * PARAMETERS:
 * nlhs:
 *     Number of output arguments defined in matlab.
 * plhs:
 *     Array of output matlab arrays.  
 * nrhs:
 *     number of input arguments defined in matlab.
 * prhs:
 *     Array of input matlab arrays.  
 *
 ******************************************************************************/
void mexFunction ( int nlhs, mxArray *plhs[], 
                   int nrhs, const mxArray *prhs[] ) {


    /*
     * Metadata about the requested operation.
     */
    op   *nc_op;

    char error_message[1000];



    /*
     * Disable the NC_FATAL option from ncopts.      This is a netcdf-2 thing.
     * Is this needed anymore?
     * */
    if (ncopts & NC_FATAL)    {
        ncopts -= NC_FATAL;
    }
    


    /*    
     * We need at least one input argument.
     * */
    if (nrhs == 0)    {
        Usage();
        return;
    }



    /*
     * Make sure the first argument is not the empty set.
     * */
    if ( mxIsEmpty ( prhs[0] ) ) {
        mexErrMsgTxt ( "First parameter to mexnc cannot be the empty set.\n" );
    }

    
    nc_op = opname2opcode ( mxArrayToString(prhs[0]) , nlhs, nrhs );
    
    

    /*
     * Now make sure that none of the other arguments are the
     * empty set.  We need to know the name of the netcdf operation
     * before we can do this, since a few of the netcdf-2 functions
     * do actually allow for the empty set.  If there are any illegal 
     * empty set arguments, then an exception is thrown.
     *
     * */
    check_other_args_for_empty_set ( nc_op, prhs, nrhs );


    
    /*    
     * Here's the switchyard for all the mexnc routines.
     */
    switch ( nc_op->opcode)    {

        case GET_MEXNC_INFO:
            plhs[0] = mxCreateString ( mexnc_release_id );
            plhs[1] = mxCreateString ( mexnc_date_id );
            break;

        
        /*
         * NetCDF-3 stuff.
         */
        case ABORT:
            handle_nc_abort ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case CLOSE:
            handle_nc_close ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case COPY_ATT:  
            handle_nc_copy_att ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case _CREATE: 
            handle_nc__create ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case CREATE: 
            handle_nc_create ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case DEF_DIM:
            handle_nc_def_dim ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case DEF_VAR:
            handle_nc_def_var ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case DEF_VAR_CHUNKING:
            handle_nc_def_var_chunking ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case DEF_VAR_DEFLATE:
            handle_nc_def_var_deflate ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case DEL_ATT:
            handle_nc_del_att ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case _ENDDEF:
            handle_nc__enddef ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        /*
         * This should have just been for ENDDEF.  I mistakenly misspelled it, and
         * then had to support it.
         */
        case END_DEF:
        case ENDDEF:
            handle_nc_enddef ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
        case GET_ATT_DOUBLE:
        case GET_ATT_FLOAT:
        case GET_ATT_INT:
        case GET_ATT_SHORT:
        case GET_ATT_SCHAR:
        case GET_ATT_UCHAR:
        case GET_ATT_TEXT:
            handle_nc_get_att ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case GET_VAR_DOUBLE:
        case GET_VAR_FLOAT:
        case GET_VAR_INT:
        case GET_VAR_SHORT:
        case GET_VAR_SCHAR:
        case GET_VAR_UCHAR:
        case GET_VAR_TEXT:
        case GET_VAR1_DOUBLE:
        case GET_VAR1_FLOAT:
        case GET_VAR1_INT:
        case GET_VAR1_SHORT:
        case GET_VAR1_SCHAR:
        case GET_VAR1_UCHAR:
        case GET_VAR1_TEXT:
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
            handle_nc_get_var_x ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case GET_VARM_DOUBLE:
        case GET_VARM_FLOAT:
        case GET_VARM_INT:
        case GET_VARM_SHORT:
        case GET_VARM_SCHAR:
        case GET_VARM_UCHAR:
        case GET_VARM_TEXT:
            handle_nc_get_varm_x ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ: 
            handle_nc_inq ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_NDIMS: 
            handle_nc_inq_ndims ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_NVARS: 
            handle_nc_inq_nvars ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_NATTS: 
            handle_nc_inq_natts ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_ATT:
            handle_nc_inq_att ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_ATTID: 
            handle_nc_inq_attid ( nlhs, plhs, nrhs, prhs, nc_op ); 
            break;

        case INQ_ATTLEN: 
            handle_nc_inq_attlen ( nlhs, plhs, nrhs, prhs, nc_op ); 
            break;

        case INQ_ATTNAME: 
            handle_nc_inq_attname ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_ATTTYPE: 
            handle_nc_inq_atttype ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_DIM:
            handle_nc_inq_dim ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_DIMID:
            handle_nc_inq_dimid ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
        
        case INQ_DIMLEN:
            handle_nc_inq_dimlen ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_DIMNAME:
            handle_nc_inq_dimname ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
        case INQ_LIBVERS:
            plhs[0] = mxCreateString ( nc_inq_libvers() );
            break;

        case INQ_VAR:
            handle_nc_inq_var ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VAR_CHUNKING:
            handle_nc_inq_var_chunking ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VAR_DEFLATE:
            handle_nc_inq_var_deflate ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VARNAME:
            handle_nc_inq_varname ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VARTYPE:
            handle_nc_inq_vartype ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VARNDIMS:
            handle_nc_inq_varndims ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
        
        case INQ_VARDIMID:
            handle_nc_inq_vardimid ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VARNATTS:
            handle_nc_inq_varnatts ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
        
        case INQ_UNLIMDIM: 
            handle_nc_inq_unlimdim ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case INQ_VARID:
            handle_nc_inq_varid ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case _OPEN: 
            handle_nc__open ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
        case OPEN: 
            handle_nc_open ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
        case PUT_ATT_DOUBLE:
        case PUT_ATT_FLOAT:
        case PUT_ATT_INT:
        case PUT_ATT_SHORT:
        case PUT_ATT_SCHAR:
        case PUT_ATT_UCHAR:
        case PUT_ATT_TEXT:
            handle_nc_put_att ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case PUT_VAR_DOUBLE:
        case PUT_VAR_FLOAT:
        case PUT_VAR_INT:
        case PUT_VAR_SHORT:
        case PUT_VAR_SCHAR:
        case PUT_VAR_UCHAR:
        case PUT_VAR_TEXT:
        case PUT_VAR1_DOUBLE:
        case PUT_VAR1_FLOAT:
        case PUT_VAR1_INT:
        case PUT_VAR1_SHORT:
        case PUT_VAR1_SCHAR:
        case PUT_VAR1_UCHAR:
        case PUT_VAR1_TEXT:
        case PUT_VARA_DOUBLE:
        case PUT_VARA_FLOAT:
        case PUT_VARA_INT:
        case PUT_VARA_SHORT:
        case PUT_VARA_SCHAR:
        case PUT_VARA_UCHAR:
        case PUT_VARA_TEXT:
        case PUT_VARS_DOUBLE:
        case PUT_VARS_FLOAT:
        case PUT_VARS_INT:
        case PUT_VARS_SHORT:
        case PUT_VARS_SCHAR:
        case PUT_VARS_UCHAR:
        case PUT_VARS_TEXT:
            handle_nc_put_var_x ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case PUT_VARM_DOUBLE:
        case PUT_VARM_FLOAT:
        case PUT_VARM_INT:
        case PUT_VARM_SHORT:
        case PUT_VARM_SCHAR:
        case PUT_VARM_UCHAR:
        case PUT_VARM_TEXT:
            handle_nc_put_varm_x ( nlhs, plhs, nrhs, prhs, nc_op );
            break;


        case REDEF:
            handle_nc_redef ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
        case RENAME_ATT: 
            handle_nc_rename_att ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case RENAME_DIM:
            handle_nc_rename_dim ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case RENAME_VAR:
            handle_nc_rename_var ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case SET_FILL: 
            handle_nc_set_fill ( nlhs, plhs, nrhs, prhs, nc_op );
            break;

        case STRERROR:
            handle_nc_strerror ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
        case SYNC:
            handle_nc_sync ( nlhs, plhs, nrhs, prhs, nc_op );
            break;
            
            
            

            
        /*
         * Ok these are all the NetCDF 2.4 API calls.  Keep'em locked 
         * away in the attic.
         * */
        case ATTCOPY:
        case ATTDEL:
        case ATTGET:
        case ATTINQ:
        case ATTNAME:
        case ATTRENAME:
        case ATTPUT:
        case DIMDEF:
        case DIMID:
        case DIMINQ:
        case DIMRENAME:
        case ENDEF:
        case ERR:
        case INQUIRE:
        case PARAMETER:
        case RECPUT:
        case RECGET:
        case RECINQ:
        case SETFILL:
        case SETOPTS:
        case TYPELEN:
        case VARCOPY:
        case VARID:
        case VARDEF:
        case VARGET:
        case VARGET1:
        case VARGETG:
        case VARINQ:
        case VARPUT:
        case VARPUT1:
        case VARPUTG:
        case VARRENAME:

            handle_netcdf2_api ( nlhs, plhs, nrhs, prhs, nc_op );    
            break;




        
        default:
        
            sprintf ( error_message, "MEXNC ERROR:\n" );
            sprintf ( error_message+strlen(error_message), 
                "\tUnhandled opcode %d, %s, line %d, file %s\n", 
                nc_op->opcode, nc_op->opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            break;
    }
    
    return;
}




/*******************************************************************************
 *
 * OPNAME2OPCODE
 *
 * This function transforms the implied name of a netcdf function (such as 
 * "nc_open") into an enumerated type (such as OPEN) that is more readily 
 * handled by switch statements.  We also check to make sure that the right 
 * number of inputs and outputs are specified.
 *
 * PARAMETERS:
 * opname:  
 *     the name of the netcdf operation, such as GET_VAR_DOUBLE
 * nlhs, nrhs:  
 *     number of left-hand and right-hand arguments that were passed in from 
 *     matlab.
 *
 * RETURN VALUE:
 *     Pointer to a structure containing metadata about the specified operation.
 *
 ******************************************************************************/
op *opname2opcode ( const char *opname, int nlhs, int nrhs ) {

	static op ops[] =	{
		{ ABORT,            "abort",            2, 1 },
		{ CLOSE,            "close",            2, 1 },
		{ COPY_ATT,         "copy_att",         6, 1 }, 
		{ _CREATE,          "_create",          4, 3 }, 
		{ CREATE,           "create",           2, 2 }, 
		{ DEF_DIM,          "def_dim",          4, 2 }, 
		{ DEF_VAR,          "def_var",          6, 2 }, 
		{ DEF_VAR_CHUNKING, "def_var_chunking", 5, 1 }, 
		{ DEF_VAR_DEFLATE,  "def_var_deflate",  6, 1 }, 
		{ DEL_ATT,          "del_att",          4, 1 }, 
		{ _ENDDEF,          "_enddef",          5, 1 }, 
		{ END_DEF,          "end_def",          1, 1 }, 
		{ ENDDEF,           "enddef",           1, 1 }, 
		{ GET_ATT_DOUBLE,   "get_att_double",   4, 2 }, 
		{ GET_ATT_FLOAT,    "get_att_float",    4, 2 }, 
		{ GET_ATT_INT,      "get_att_int",      4, 2 }, 
		{ GET_ATT_SHORT,    "get_att_short",    4, 2 }, 
		{ GET_ATT_SCHAR,    "get_att_schar",    4, 2 }, 
		{ GET_ATT_UCHAR,    "get_att_uchar",    4, 2 }, 
		{ GET_ATT_TEXT,     "get_att_text",     4, 2 }, 
		{ GET_MEXNC_INFO,   "get_mexnc_info",   1, 2 }, 
		{ GET_VAR_DOUBLE,   "get_var_double",   3, 2 }, 
		{ GET_VAR_FLOAT,    "get_var_float",    3, 2 }, 
		{ GET_VAR_INT,      "get_var_int",      3, 2 }, 
		{ GET_VAR_SHORT,    "get_var_short",    3, 2 }, 
		{ GET_VAR_SCHAR,    "get_var_schar",    3, 2 }, 
		{ GET_VAR_UCHAR,    "get_var_uchar",    3, 2 }, 
		{ GET_VAR_TEXT,     "get_var_text",     3, 2 }, 
		{ GET_VAR1_DOUBLE,  "get_var1_double",  4, 2 }, 
		{ GET_VAR1_FLOAT,   "get_var1_float",   4, 2 }, 
		{ GET_VAR1_INT,     "get_var1_int",     4, 2 }, 
		{ GET_VAR1_SHORT,   "get_var1_short",   4, 2 }, 
		{ GET_VAR1_SCHAR,   "get_var1_schar",   4, 2 }, 
		{ GET_VAR1_UCHAR,   "get_var1_uchar",   4, 2 }, 
		{ GET_VAR1_TEXT,    "get_var1_text",    4, 2 }, 
		{ GET_VARA_DOUBLE,  "get_vara_double",  5, 2 }, 
		{ GET_VARA_FLOAT,   "get_vara_float",   5, 2 }, 
		{ GET_VARA_INT,     "get_vara_int",     5, 2 }, 
		{ GET_VARA_SHORT,   "get_vara_short",   5, 2 }, 
		{ GET_VARA_SCHAR,   "get_vara_schar",   5, 2 }, 
		{ GET_VARA_UCHAR,   "get_vara_uchar",   5, 2 }, 
		{ GET_VARA_TEXT,    "get_vara_text",    5, 2 }, 
		{ GET_VARS_DOUBLE,  "get_vars_double",  6, 2 }, 
		{ GET_VARS_FLOAT,   "get_vars_float",   6, 2 }, 
		{ GET_VARS_INT,     "get_vars_int",     6, 2 }, 
		{ GET_VARS_SHORT,   "get_vars_short",   6, 2 }, 
		{ GET_VARS_SCHAR,   "get_vars_schar",   6, 2 }, 
		{ GET_VARS_UCHAR,   "get_vars_uchar",   6, 2 }, 
		{ GET_VARS_TEXT,    "get_vars_text",    6, 2 }, 
		{ GET_VARM_DOUBLE,  "get_varm_double",  7, 2 }, 
		{ GET_VARM_FLOAT,   "get_varm_float",   7, 2 }, 
		{ GET_VARM_INT,     "get_varm_int",     7, 2 }, 
		{ GET_VARM_SHORT,   "get_varm_short",   7, 2 }, 
		{ GET_VARM_SCHAR,   "get_varm_schar",   7, 2 }, 
		{ GET_VARM_UCHAR,   "get_varm_uchar",   7, 2 }, 
		{ GET_VARM_TEXT,    "get_varm_text",    7, 2 }, 
		{ INQ,              "inq",              2, 5 }, 
		{ INQ_ATT,          "inq_att",          4, 3 }, 
		{ INQ_ATTID,        "inq_attid",        4, 2 }, 
		{ INQ_ATTLEN,       "inq_attlen",       4, 2 }, 
		{ INQ_ATTNAME,      "inq_attname",      4, 2 }, 
		{ INQ_ATTTYPE,      "inq_atttype",      4, 2 }, 
		{ INQ_DIM,          "inq_dim",          3, 3 }, 
		{ INQ_DIMID,        "inq_dimid",        3, 2 }, 
		{ INQ_DIMLEN,       "inq_dimlen",       3, 2 }, 
		{ INQ_DIMNAME,      "inq_dimname",      3, 2 }, 
		{ INQ_LIBVERS,      "inq_libvers",      1, 1 }, 
		{ INQ_NDIMS,        "inq_ndims",        2, 2 }, 
		{ INQ_NVARS,        "inq_nvars",        2, 2 }, 
		{ INQ_NATTS,        "inq_natts",        2, 2 }, 
		{ INQ_UNLIMDIM,     "inq_unlimdim",     1, 2 }, 
		{ INQ_VARID,        "inq_varid",        3, 2 }, 
		{ INQ_VAR,          "inq_var",          3, 6 }, 
		{ INQ_VAR_CHUNKING, "inq_var_chunking", 3, 3 }, 
		{ INQ_VAR_DEFLATE,  "inq_var_deflate",  3, 4 }, 
		{ INQ_VARNAME,      "inq_varname",      3, 2 }, 
		{ INQ_VARTYPE,      "inq_vartype",      3, 2 }, 
		{ INQ_VARNDIMS,     "inq_varndims",     3, 2 }, 
		{ INQ_VARDIMID,     "inq_vardimid",     3, 2 }, 
		{ INQ_VARNATTS,     "inq_varnatts",     3, 2 }, 
		{ _OPEN,            "_open",            4, 3 }, 
		{ OPEN,             "open",             2, 2 }, 
		{ PUT_ATT_DOUBLE,   "put_att_double",   7, 1 }, 
		{ PUT_ATT_FLOAT,    "put_att_float",    7, 1 }, 
		{ PUT_ATT_INT,      "put_att_int",      7, 1 }, 
		{ PUT_ATT_SHORT,    "put_att_short",    7, 1 }, 
		{ PUT_ATT_SCHAR,    "put_att_schar",    7, 1 }, 
		{ PUT_ATT_UCHAR,    "put_att_uchar",    7, 1 }, 
		{ PUT_ATT_TEXT,     "put_att_text",     7, 1 }, 
		{ PUT_VAR_DOUBLE,   "put_var_double",   4, 1 }, 
		{ PUT_VAR_FLOAT,    "put_var_float",    4, 1 }, 
		{ PUT_VAR_INT,      "put_var_int",      4, 1 }, 
		{ PUT_VAR_SHORT,    "put_var_short",    4, 1 }, 
		{ PUT_VAR_SCHAR,    "put_var_schar",    4, 1 }, 
		{ PUT_VAR_UCHAR,    "put_var_uchar",    4, 1 }, 
		{ PUT_VAR_TEXT,     "put_var_text",     4, 1 }, 
		{ PUT_VARA_DOUBLE,  "put_vara_double",  6, 1 }, 
		{ PUT_VARA_FLOAT,   "put_vara_float",   6, 1 }, 
		{ PUT_VARA_INT,     "put_vara_int",     6, 1 }, 
		{ PUT_VARA_SHORT,   "put_vara_short",   6, 1 }, 
		{ PUT_VARA_SCHAR,   "put_vara_schar",   6, 1 }, 
		{ PUT_VARA_UCHAR,   "put_vara_uchar",   6, 1 }, 
		{ PUT_VARA_TEXT,    "put_vara_text",    6, 1 }, 
		{ PUT_VARS_DOUBLE,  "put_vars_double",  7, 1 }, 
		{ PUT_VARS_FLOAT,   "put_vars_float",   7, 1 }, 
		{ PUT_VARS_INT,     "put_vars_int",     7, 1 }, 
		{ PUT_VARS_SHORT,   "put_vars_short",   7, 1 }, 
		{ PUT_VARS_SCHAR,   "put_vars_schar",   7, 1 }, 
		{ PUT_VARS_UCHAR,   "put_vars_uchar",   7, 1 }, 
		{ PUT_VARS_TEXT,    "put_vars_text",    7, 1 }, 
		{ PUT_VARM_DOUBLE,  "put_varm_double",  8, 1 }, 
		{ PUT_VARM_FLOAT,   "put_varm_float",   8, 1 }, 
		{ PUT_VARM_INT,     "put_varm_int",     8, 1 }, 
		{ PUT_VARM_SHORT,   "put_varm_short",   8, 1 }, 
		{ PUT_VARM_SCHAR,   "put_varm_schar",   8, 1 }, 
		{ PUT_VARM_UCHAR,   "put_varm_uchar",   8, 1 }, 
		{ PUT_VARM_TEXT,    "put_varm_text",    8, 1 }, 
		{ PUT_VAR1_DOUBLE,  "put_var1_double",  5, 1 }, 
		{ PUT_VAR1_FLOAT,   "put_var1_float",   5, 1 }, 
		{ PUT_VAR1_INT,     "put_var1_int",     5, 1 }, 
		{ PUT_VAR1_SHORT,   "put_var1_short",   5, 1 }, 
		{ PUT_VAR1_SCHAR,   "put_var1_schar",   5, 1 }, 
		{ PUT_VAR1_UCHAR,   "put_var1_uchar",   5, 1 }, 
		{ PUT_VAR1_TEXT,    "put_var1_text",    5, 1 }, 
		{ REDEF,            "redef",            1, 1 }, 
		{ RENAME_ATT,       "rename_att",       5, 1 }, 
		{ RENAME_DIM,       "rename_dim",       4, 1 }, 
		{ RENAME_VAR,       "rename_var",       4, 1 }, 
		{ SET_FILL,         "set_fill",         3, 2 }, 
		{ STRERROR,         "strerror",         1, 1 }, 
		{ SYNC,             "sync",             2, 1 }, 
	
		/*
		 * Deprecated Netcdf 2.4
		 * */
		{ ATTDEL,           "attdel",           4, 1 }, 
		{ DIMDEF,           "dimdef",           4, 2 }, 
		{ DIMID,            "dimid",            3, 2 }, 
		{ DIMINQ,           "diminq",           3, 3 }, 
		{ DIMRENAME,        "dimrename",        4, 1 }, 
		{ ENDEF,            "endef",            1, 1 }, 
		{ VARINQ,           "varinq",           3, 6 }, 
		{ INQUIRE,          "inquire",          2, 5 }, 
		{ VARDEF,           "vardef",           6, 2 }, 
		{ VARID,            "varid",            3, 2 }, 
	
		{ VARPUT1,          "varput1",          5, 1 }, 
		{ VARGET1,          "varget1",          4, 2 }, 
		{ VARPUT,           "varput",           6, 1 }, 
		{ VARGET,           "varget",           5, 2 }, 
		{ VARPUTG,          "varputg",          7, 1 }, 
		{ VARGETG,          "vargetg",          6, 2 }, 
		{ VARRENAME,        "varrename",        4, 1 }, 
		{ VARCOPY,          "varcopy",          3, 2 }, 
		{ ATTCOPY,          "attcopy",          6, 1 }, 
		{ ATTPUT,           "attput",           7, 1 }, 
		{ ATTINQ,           "attinq",           4, 3 }, 
		{ ATTGET,           "attget",           4, 2 }, 
		{ ATTNAME,          "attname",          4, 2 }, 
		{ ATTRENAME,        "attrename",        5, 1 }, 
		{ RECPUT,           "recput",           4, 1 }, 
		{ RECGET,           "recget",           3, 2 }, 
		{ RECINQ,           "recinq",           2, 3 }, 
		{ TYPELEN,          "typelen",          2, 2 }, 
		{ SETFILL,          "setfill",          3, 2 }, 
		{ SETOPTS,          "setopts",          2, 1 }, 
		{ ERR,              "err",              1, 1 }, 
		{ PARAMETER,        "parameter",        1, 1 }, 
		{ NONE,             "none",             0, 0 }
	};



    int      j;

    /*
     * Used to access the opcode portions.
     * */
    char    *p;

    char     error_message[1000];


    /*
     * The operation name must be converted to lower case.
     * Initialize it to '\0'.
     * */
    char    lcopname[MAX_NC_NAME] = {0};

    sprintf ( lcopname, "%s", opname );


    /*    
     * Convert the operation name to its opcode.    
     */
    for (j = 0; j < strlen(lcopname); j++)    {
        lcopname[j] = (char) tolower((int) lcopname[j]);
    }
    p = lcopname;


    /*
     * Trim off leading "nc" or "nc_" if it is there
     */
    if (strncmp(p, "nc", 2) == 0)  {    
        p += 2;
    }
    if (strncmp(p, "nc_", 3) == 0) {  
        p += 3;
    }
    

    /*
     * Go thru the list of mexnc routines, try to find a match.
     * */
    j = 0;
    while (ops[j].opcode != NONE)    {

        if ( strcmp(p, ops[j].opname) != 0 ) {

            /*
             * This wasn't it.  Go on to the next.
             */
            j++;
        } else {
            break;
        } 

    } 


    /*
     * So did we find a match for the requested operation?
     */
    if (ops[j].opcode == NONE)    {
        sprintf ( error_message, "MEXNC ERROR:\n" );
        sprintf ( error_message+strlen(error_message), 
            "\tNo such operation as \"%s\"\n", opname );
        mexErrMsgTxt(error_message);
    }


    
    /*
     * Check that proper number of inputs and outputs has been respected.
     * */
    if (ops[j].nrhs > nrhs)    {
        sprintf ( error_message, "MEXNC ERROR:\n" );
        sprintf ( error_message+strlen(error_message), 
                    "\t\"%s\" requires at least %d input arguments, you provided %d.\n",
                    opname, ops[j].nrhs, nrhs );
        mexErrMsgTxt(error_message);
    }

    if (ops[j].nlhs < nlhs)    {
        sprintf ( error_message, "MEXNC ERROR:\n" );
        sprintf ( error_message+strlen(error_message), 
                    "\t\"%s\" provides at most %d output arguments, you requested %d.\n",
                    opname, ops[j].nlhs, nlhs );
        mexErrMsgTxt(error_message);
    }
    

    return ( & ops[j] );


}



















/*******************************************************************************
 *
 * CHECK_OTHER_ARGS_FOR_EMPTY_SET
 *
 * After the first argument has been checked to see that it is not the empty 
 * set, the op name can be extracted.  If the op name is known, then we can 
 * intelligently check the other input arguments to make sure that none of them 
 * are the empty set except where specifically allowed.  The reason for the 
 * allowed cases aren't really explained anywhere.  And unfortunately there's 
 * already code out there that makes use of this.  Fantastic.
 * 
 * The empty string is considered different than [], I suppose, so
 * we skip that.
 *
 * PARAMETERS:
 * nc_op:  
 *     Structure of metadata pertaining to the requested mexnc operation.
 * prhs:
 *     Array of right-hand side arguments passed in from matlab.
 * nrhs:
 *     Number of right-hand side arguments.
 *
 * RETURN VALUE:
 *     None.
 *
 ******************************************************************************/
void check_other_args_for_empty_set 
( 
    op            *nc_op, 
    const mxArray *prhs[], 
    int            nrhs 
) 
{

    /*
     * If we encounter the empty set where it is illegal, say so here.
     * */
    char error_msg[500];




    /*
     * Loop index for matlab array inputs.
     * */
    int i;


    for ( i = 1; i < nrhs; ++i ) {


        int not_ok = 1;

        /*
         * If an argument is empty, check to see if it isn't one of the 
         * allowed cases.
         */
        if ( mxIsEmpty(prhs[i]) ) {

            /*
             * These cases are allowed.
             */

            /*
             * Causes the creation of a zero-length character attribute.
             */
            if ( ( nc_op->opcode == ATTPUT ) && ( i == 6 ) ) {
                not_ok = 0;
            }

            /*
             * DEF_VAR and VARDEF allow for the creation of a singleton
             * variable.  The empty set in this case is the array of
             * dimension IDs.
             */
            if ( ( nc_op->opcode == DEF_VAR ) && ( i == 5 ) ) {
                not_ok = 0;
            }

            if ( ( nc_op->opcode == VARDEF ) && ( i == 5 ) ) {
                not_ok = 0;
            }

            if ( ( nc_op->opcode == DEF_VAR_CHUNKING ) && ( i == 4 ) ) {
                not_ok = 0;
            }

            /*
             * The C API for ncvargetg and ncvarputg allow for an "imap"
             * parameter that specifies an "in-memory" mapping.  
             * This is not used in the C API.  The documentation was 
             * unfortunately written to suggest usage of the empty set,
             * however, so we have to allow for that here.
             */
            if ( ( nc_op->opcode == VARGETG ) && ( i == 6 ) ) {
                not_ok = 0;
            }

            if ( ( nc_op->opcode == VARPUTG ) && ( i == 6 ) ) {
                not_ok = 0;
            }

            if ( ( nc_op->opcode == VARPUTG ) && ( i == 7 ) ) {
                not_ok = 0;
            }

            if ( ( nc_op->opcode == VARPUT1 ) && ( i == 4 ) ) {
                not_ok = 0;
            }


            if ( ( nc_op->opcode == VARPUT ) && ( i == 5 ) ) {
                not_ok = 0;
            }

            /*
             * An empty string is sometimes allowed.
             * */
            if ( mxIsChar ( prhs[i] ) ) {
                not_ok = 0;
            }

        } else {
            not_ok = 0;
        }

        if ( not_ok ) {
            sprintf ( error_msg, "%s:  cannot have empty set in input position %d.\n", nc_op->opname, i+1 );
            mexErrMsgTxt ( error_msg );
        }

    }




}


