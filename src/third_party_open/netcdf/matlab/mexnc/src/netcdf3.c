/**********************************************************************
 *
 * netcdf3.c
 *
 * This file contains code to handle the mexfile interface to NetCDF-3
 * API calls.
 *
 *********************************************************************/

/*
 * $Id: netcdf3.c 2828 2010-01-15 14:48:55Z johnevans007 $
 * */

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"
# include "netcdf3.h"


/*
 * Let each function use this over and over again.
 * */
static char    error_message[1000];



/*
 * DETERMINE_VARM_OUTPUT_SIZE
 *
 * The get_varm and put_varm routines are unusual in that the size of 
 * the resulting output array do not necessarily match what that of
 * the nc_count_coord array.
 *
 * The size of each edge depends upon computing the ratio of the two
 * smallest edges of the imap vector.  It has to be that way, if you think
 * about the definition of the imap vector.  For example, suppose that the
 * netcdf variable is 8x6x4 in size and that the imap vector is [24 4 1].
 * This trivially maps the output into an 8x6x4 output array, but it's a
 * good example, nevertheless.
 *
 * There are 192 elements to be retrieved, but the first imap element of
 * 24 means that there is a distance of 
 * So the output size is [8 6 4].
 *
 * As a second example, suppose we have the same 8x6x4 array, but the
 * imap vector is [1 8 48].  
 *
 * Ex. #2 NetCDF size = [5 4 3 2].
 *        Want the trivial matlab size of [5 4 3 2].
 *        Think in terms of the transpose, [2 3 4 5].
 *        Imap vector is [1 5 20 60]
 *
 * Not sure why this works, exactly.  It makes sense for the trivial
 * mapping, but hard to figure otherwise.
 *
 * */
void     determine_varm_output_size ( 
        int        ndims, 
        int        num_requested_elements, 
        size_t    *nc_count_coord, 
        ptrdiff_t *nc_stride_coord, 
        ptrdiff_t *nc_imap_coord, 
        int       *result_size )
{


    /*
     * Loop index
     * */
    int j, k;


    char    error_message[1000];


    /*
     * If an element in this array is flagged, it means that we 
     * have already figured its contribution.
     * */
    int still_unused[MAX_NC_DIMS];


    /*
     * Keeps track of the largest remaining imap coordinate for
     * each dimension.
     * */
    ptrdiff_t max_imap_element_size;
    ptrdiff_t max_imap_element_index;


    /*
     * Initialize the flag array.
     * */
    for ( j = 0; j < MAX_NC_DIMS; ++j ) {
        still_unused[j] = 1;
    }


    /*
     * For each dimension, figure the contribution.
     * */
    for ( j = 0; j < ndims; ++j ) {

        /*
         * Find the largest remaining imap element.
         * */
        max_imap_element_size = -1; 
        for ( k = 0; k < ndims; ++k ) { 
            if ( (nc_imap_coord[k] > max_imap_element_size) && ( still_unused[k] ) ) {
                max_imap_element_index = k;
                max_imap_element_size = nc_imap_coord[k];
            }
        }


        /*
         * Figure the contribution for this dimension.
         * Remember to reverse the order, otherwise the row-major order
         * and column-major order issue will get us.:W
         * */
        result_size[j] = num_requested_elements / nc_imap_coord [ max_imap_element_index ];

        /*
         * Reduce the dimensionality.
         * */
        num_requested_elements = nc_imap_coord [ max_imap_element_index ];


        /*
         * We are done with this imap coordinate.  Mark it as used.
         * */
        still_unused[max_imap_element_index] = 0;

    }


    /*
     * Check that no elements in result_size are non-positive.
     * That can crash matlab hard.
     * */
    for ( j = 0; j < ndims; ++j ) {
        if  ( result_size[j] < 1 ) {

            sprintf ( error_message, "Requested data extent is invalid.\n\n" );
            mexPrintf ( error_message );

            mexPrintf ( "count " );
            for ( j = 0; j < ndims; ++j ) {
                printf ( "[%d]", nc_count_coord[j] );
            }
            mexPrintf ( "\n\n" );
    
    
            mexPrintf ( "stride " );
            for ( j = 0; j < ndims; ++j ) {
                printf ( "[%d]", nc_stride_coord[j] );
            }
            mexPrintf ( "\n\n" );
    
    
            mexPrintf ( "imap " );
            for ( j = 0; j < ndims; ++j ) {
                printf ( "[%d]", nc_imap_coord[j] );
            }
            mexPrintf ( "\n\n" );
    
            mexPrintf ( "result_size " );
            for ( j = 0; j < ndims; ++j ) {
                printf ( "[%d]", result_size[j] );
            }
            mexPrintf ( "\n\n" );
    
            mexErrMsgTxt ( "\n\n" );

        }
    }


}







/***********************************************************************
 *
 * HANDLE_NC_ABORT
 *
 * code for handling the nc_abort routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_abort function call 
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'ABORT'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_abort 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file and variable IDs used in nc_copy_att
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_abort(ncid);
    plhs[0] = mexncCreateDoubleScalar(status);
    


    return;

}










/***********************************************************************
 *
 * HANDLE_NC_CLOSE
 *
 * code for handling the nc_close routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the return status of the nc_close function call 
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_CLOSE'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_close 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file and variable IDs used in nc_copy_att
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    /*
     * Unpack the parameters.
     * */
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);

    status = nc_close(ncid);
    plhs[0] = mexncCreateDoubleScalar ( status );




    return;

}
































/***********************************************************************
 *
 * HANDLE_COPY_ATT:
 *
 * code for handling the nc_copy_att routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  The return status of the
 *     nc_copy_att function call is placed into plhs[0].
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'COPY_ATT'
 *       prhs[1] = ID of the source netcdf file
 *       prhs[2] = ID of the source parent variable of the referenced 
 *                 source attribute           
 *       prhs[3] = name of the referenced source attribute
 *       prhs[4] = ID of the destination netcdf file
 *       prhs[5] = ID of the destination parent variable of the 
 *                 referenced destination attribute           
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_copy_att 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file and variable IDs used in nc_copy_att
     * */
    int ncid_in;
    int varid_in;
    int ncid_out;
    int varid_out;

    /* 
     * name of netcdf attribute name
     * */
    char    *attname;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type    ( prhs, nc_op->opname, 3 );
    check_numeric_argument_type ( prhs, nc_op->opname, 4 );
    check_numeric_argument_type ( prhs, nc_op->opname, 5 );
    
    



    pr = mxGetData ( prhs[1] );
    ncid_in = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid_in = (int)(pr[0]);
    attname = unpackString(prhs[3]);
    pr = mxGetData ( prhs[4] );
    ncid_out = (int)(pr[0]);
    pr = mxGetData ( prhs[5] );
    varid_out = (int)(pr[0]);

    status = nc_copy_att ( ncid_in, varid_in, attname, ncid_out, varid_out );
    plhs[0] = mexncCreateDoubleScalar ( status );
    return;

}







            
/***********************************************************************
 *
 * HANDLE_NC__CREATE:
 *
 * code for handling the nc__create routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return value of the chunksize parameter.  Please 
 *                 see the C language NetCDF User's Guide for further
 *                 details.
 *       plhs[1] = file id (ncid) of the just-created netcdf file
 *       plhs[2] = return status of the nc__create function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = '_CREATE' or '_create'
 *       prhs[1] = path to the netcdf file
 *       prhs[2] = open mode for the netcdf file
 *       prhs[3] = initial size of the netcdf file.  Please see the C 
 *                 language NetCDF User's Guide for further
 *                 details.
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc__create 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file and variable IDs used in nc_copy_att
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;

    /*
     * NetCDF file creation mode.
     * */
    int cmode;

    /*
     * sets the initial size of the file at creation time.
     * */
    size_t initialsize;


    /*
     * See the man page for a description of chunksize.
     * */
    size_t chunksize;


    /*
     * path of NetCDF file
     * */
    char    *path;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_char_argument_type  ( prhs, nc_op->opname, 1 );
    check_mode_argument_type  ( prhs, nc_op->opname, 2 );
    check_numeric_argument_type  ( prhs, nc_op->opname, 3 );
    

    
    path = unpackString(prhs[1]);
    pr = mxGetData ( prhs[2] );
    cmode = (int)(pr[0]);
    pr = mxGetData ( prhs[3] );
    initialsize = (int)(pr[0]);
            
            
    status = nc__create ( path, cmode, initialsize, &chunksize, &ncid );
    plhs[0] = mexncCreateDoubleScalar ( chunksize );
    plhs[1] = mexncCreateDoubleScalar ( ncid );
    plhs[2] = mexncCreateDoubleScalar ( status );
            
    return;

}







/***********************************************************************
 *
 * HANDLE_NC_CREATE:
 *
 * code for handling the nc_create routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = file ID of the newly-created netcdf file
 *       plhs[1] = the return status of the nc_close function call 
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  The netcdf-2 code originally 
 *     allowed the user to skip the create mode, using a default
 *     of NC_NO_CLOBBER.  This is really bad policy.
 *       prhs[0] = 'CREATE'
 *       prhs[1] = path to proposed netcdf file
 *       prhs[2] = create mode for the proposed netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_create 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file and variable IDs used in nc_copy_att
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;

    /*
     * NetCDF file creation mode.
     * */
    int cmode;

    /*
     * path of NetCDF file
     * */
    char    *path;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_char_argument_type ( prhs, nc_op->opname, 1 );

    
    /*
     * The mode argument has historically been something like 'clobber', which is
     * a character data type.  This is now frowned upon.  The user should use a mnemonic
     * constant instead.
     * */
    if ( nrhs == 3 ) {
        if ( mxIsChar(prhs[2]) == true) {

            int num_chars = mxGetM ( prhs[2] ) * mxGetN ( prhs[2] );
            char smode[NC_MAX_NAME];

            status = mxGetString( prhs[2], smode, num_chars+1 );
            if (status != 0) {
                sprintf ( error_message, "mxGetString failed, line %d file \"%s\"\n", __LINE__, __FILE__ );
                mexErrMsgTxt ( error_message );
                return;
            }

            cmode = unpack_char_file_mode ( smode );

        
        }

        else if ( mxIsDouble(prhs[2]) == true ) {
            pr = mxGetPr ( prhs[2] );
            cmode = (int) pr[0];
        } 
        
        
        else {
            sprintf ( error_message, 
                "operation \"%s\":  mode argument must be either character or numeric, line %d file \"%s\"\n", 
                nc_op->opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
        }

        
    } else {
        cmode = NC_NOCLOBBER;
    }

	/*
	 * Do not allow enhanced mode.
	 * */
	if ( cmode & NC_NETCDF4 ) {
		if ( cmode == NC_NETCDF4 ) {
	            sprintf ( error_message, 
	                "operation \"%s\":  if creating a netcdf-4 file, the file creation mode create mode must be a bitwise-or with NC_CLASSIC_MODE, line %d file \"%s\"\n", 
	                nc_op->opname, __LINE__, __FILE__ );
	            mexErrMsgTxt ( error_message );
		}
	}



    
    path = unpackString(prhs[1]);
            
            
    status = nc_create ( path, cmode, &ncid );
    plhs[0] = mexncCreateDoubleScalar ( ncid );
    plhs[1] = mexncCreateDoubleScalar ( status );
            
    return;

}

















/***********************************************************************
 *
 * HANDLE_NC_DEF_DIM:
 *
 * code for handling the nc_def_dim routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the dimension ID of the proposed new dimension
 *       plhs[1] = the return status of the nc_def_dim function call 
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'DEF_DIMS'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = name of the proposed new dimension
 *       prhs[3] = length of the proposed new dimension
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_def_dim 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file handle
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /* 
     * name of netcdf variable 
     * */
    char    *dimension_name;


    /*
     * Length of the dimension being defined.
     * */
    size_t  dim_length;


    /*
     * NetCDF identifier for newly defined dimension.
     * */
    int     dimid;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_char_argument_type ( prhs, nc_op->opname, 2 );
	if ( mxIsChar(prhs[3])) {
		dim_length = interpret_char_parameter(prhs[3]);
	} else {
		check_numeric_argument_type ( prhs, nc_op->opname, 3 );
		pr = mxGetData ( prhs[3] );
	    dim_length = (size_t)(pr[0]);
	}
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    dimension_name = unpackString(prhs[2]);

    status = nc_def_dim ( ncid, dimension_name, dim_length, &dimid );
    plhs[0] = mexncCreateDoubleScalar ( dimid );
    plhs[1] = mexncCreateDoubleScalar ( status );




    return;

}











/***********************************************************************
 *
 * HANDLE_NC_DEF_VAR:
 *
 * code for handling the nc_def_var routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the ID of the proposed new variable
 *       plhs[1] = the return status of the nc_var_dim function call 
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'DEF_VAR'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = name of the proposed new variable
 *       prhs[3] = datatype of the proposed new variable
 *       prhs[4] = number of dimensions of the proposed new variable
 *       prhs[5] = array of dimension IDs for the proposed new variable
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 * In order to create a singleton variable, ndims (prhs[4]) must be
 * zero AND dimids (prhs[5]) must have a length of zero.
 *
 **********************************************************************/
void handle_nc_def_var 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file handle
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /* 
     * name of netcdf variable 
     * */
    char    *name;



    /* 
     * UNIDATA defined type of a variable.
     * */
    nc_type  datatype;

    /* 
     * number of dimensions in a NetCDF file and their IDs.
     * */
    int             ndims;
    int             dimids[NC_MAX_DIMS]; 

    /* 
     * NetCDF variable ID
     * */
    int      varid;


    /*
     * Loop index
     * */
    int j;

    /*
     * Sizes of input matrices.
     * */
    int m;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_char_argument_type ( prhs, nc_op->opname, 2 );

    if ( !((mxIsChar(prhs[3]) == false) || (mxIsDouble(prhs[3]) == false )) ) {
        sprintf ( error_message, 
                "datatype argument must be matlab native double precision (<== that one, please) or character, operation \"%s\", line %d file \"%s\"\n", 
                nc_op->opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }

    check_numeric_argument_type ( prhs, nc_op->opname, 4 );
    check_numeric_argument_type ( prhs, nc_op->opname, 5 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    name = unpackString(prhs[2]);

    if ( mxIsChar ( prhs[3] ) ) {

        /*
         * This one is for backwards compatibility.  I really 
         * wish people would not do this...
         * */
        datatype = interpret_char_parameter  ( prhs[3] );

    } else {
        pr = mxGetData ( prhs[3] );
        datatype = (nc_type) (pr[0]);
    }


    pr = mxGetData ( prhs[4] );
    ndims = (int) (pr[0]);

    /*
     * Make sure the user didn't do something really stupid like give too many dimensions.
     * */
    if ( ndims > NC_MAX_VAR_DIMS ) {
            sprintf ( error_message, 
                "given number of dimensions (%d) exceeds preset maximum of %d, operation \"%s\", line %d file \"%s\"\n", 
                ndims, NC_MAX_VAR_DIMS, nc_op->opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }



    /*
     * And check that the given number of dimensions matches what the dimension array really has.
     * */
    pr = mxGetData ( prhs[5] );
    m = mxGetNumberOfElements ( prhs[5] );
    if ( ndims != m ) {
            sprintf ( error_message, "MEXNC ERROR:  %s\n", nc_op->opname );
            sprintf ( error_message+strlen(error_message), 
                    "\tGiven number of dimensions (%d) does not equal the length of the given dimid array (%d)\n", 
                    ndims, m );
            sprintf ( error_message+strlen(error_message), 
                    "\tLine %d, file %s\n.", __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }


    /*
     * Copy the dimension ids on over.
     * */
    for ( j = 0; j < ndims; ++j ) {
        dimids[j] = (int) (pr[j]);
    }


    status = nc_def_var ( ncid, name, datatype, ndims, dimids, &varid );
    plhs[0] = mexncCreateDoubleScalar ( varid );
    plhs[1] = mexncCreateDoubleScalar ( status );


    return;

}











/***********************************************************************
 *
 * HANDLE_NC_DEL_ATT:
 *
 * code for handling the nc_del_att routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the return status of the nc_del_att function call 
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'DEL_ATT'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the parent variable of the referenced 
 *                 attribute
 *       prhs[3] = name of the referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_del_att 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file handle
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /* 
     * name of netcdf variable 
     * */
    char    *name;



    /* 
     * NetCDF variable ID
     * */
    int      varid;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    name = unpackString(prhs[3]);
    status = nc_del_att ( ncid, varid, name );
    plhs[0] = mexncCreateDoubleScalar ( status );

    return;
            
}










/***********************************************************************
 *
 * HANDLE_NC__ENDDEF:
 *
 * code for handling the nc__enddef routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc__enddef function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = '_ENDDEF' or '_enddef'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = h_minfree argument
 *       prhs[3] = v_align argument
 *       prhs[4] = v_minfree argument
 *       prhs[5] = r_align argument.  Please, please, please see the C 
 *                 language NetCDF User's Guide for further
 *                 details.
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc__enddef 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file handle
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * performance tuning parameters.
     * See the netcdf man page for details.
     * */
    size_t h_minfree, v_align, v_minfree, r_align;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type  ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type  ( prhs, nc_op->opname, 2 );
    check_numeric_argument_type  ( prhs, nc_op->opname, 3 );
    check_numeric_argument_type  ( prhs, nc_op->opname, 4 );
    check_numeric_argument_type  ( prhs, nc_op->opname, 5 );
    
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    h_minfree = (size_t)(pr[0]);
    pr = mxGetData ( prhs[3] );
    v_align = (size_t)(pr[0]);
    pr = mxGetData ( prhs[4] );
    v_minfree = (size_t)(pr[0]);
    pr = mxGetData ( prhs[5] );
    r_align = (size_t)(pr[0]);
    status = nc__enddef(ncid, h_minfree, v_align, v_minfree, r_align);
    plhs[0] = mexncCreateDoubleScalar ( status );
    return;
            
}







/***********************************************************************
 *
 * HANDLE_NC_END_DEF:
 *
 * code for handling the nc_enddef routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the return status of the nc_enddef function call 
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'ENDDEF'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_enddef 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file handle
     * */
    int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_enddef(ncid);
    plhs[0] = mexncCreateDoubleScalar ( status );
    return;
            
}









/***********************************************************************
 *
 * HANDLE_NC_GET_ATT
 *
 * Implements the nc_get_att function
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = attribute value
 *       plhs[1] = return status of the nc_get_att function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'GET_ATT'
 *       prhs[1] = ID of the root/parent group of the parent variable
 *                 of the referenced attribute
 *       prhs[2] = ID of the parent variable
 *       prhs[3] = name of the referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_get_att   
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *the_op 
)
{
    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * Generic space for character data.
     * */
    char   *buffer;

    /*
     * NCTYPE of the attribute
     * */
    nc_type xtype;


    /*
     * Return status from netcdf operation.
     * */
    int      status;

    /*
     * name of attribute
     * */
    char     *attname;


    /*
     * Length of the attribute.
     * */
    size_t attlen;


    /*
     * Defines the attribute size in matlab space.
     * */
    int mxsize[2];


    char   *opname = the_op->opname;  


    /*
     * Generic ptr for accessing mxChar data.
     * */
    mxChar *mxchar_ptr;

    /*
     * Generic ptr for accessing char data.
     * */
    char *char_ptr;


    /*
     * Loop index.
     * */
    int j;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, opname, 1 );
    check_numeric_argument_type ( prhs, opname, 2 );
    check_char_argument_type    ( prhs, opname, 3 );
    
     


    /*
     * Extract what we can from the inputs.
     * */
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    attname = unpackString(prhs[3]);


    /*
     * Need to know some information about the attribute
     * */
    status = nc_inq_att ( ncid, varid, attname, &xtype, &attlen );
    if ( status != NC_NOERR ) {
        sprintf ( error_message, 
              "nc_inq_att failed, \"%s\", file %s, line %d : \n",
              nc_strerror(status), __FILE__, __LINE__ );
        mexErrMsgTxt ( error_message );
    }





    mxsize[0] = 1;
    mxsize[1] = attlen;
    switch ( the_op->opcode ) {

        /*
         * Some of these are netcdf-3 operations, for which we already have
         * pre-existing code.
         * */
        case GET_ATT_DOUBLE:
            plhs[0] = mxCreateNumericArray ( 2, mxsize, mxDOUBLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_att_double ( ncid, varid, attname, (void *)pr );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;


        case GET_ATT_FLOAT:
            plhs[0] = mxCreateNumericArray ( 2, mxsize, mxSINGLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_att_float ( ncid, varid, attname, (void *)pr );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;


        case GET_ATT_INT:
            plhs[0] = mxCreateNumericArray ( 2, mxsize, mxINT32_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_att_int ( ncid, varid, attname, (void *)pr );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;


        case GET_ATT_SHORT:
            plhs[0] = mxCreateNumericArray ( 2, mxsize, mxINT16_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_att_short ( ncid, varid, attname, (void *)pr );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;


        case GET_ATT_SCHAR:
            plhs[0] = mxCreateNumericArray ( 2, mxsize, mxINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_att_schar ( ncid, varid, attname, (void *)pr );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;


        case GET_ATT_UCHAR:
            plhs[0] = mxCreateNumericArray ( 2, mxsize, mxUINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_att_uchar ( ncid, varid, attname, (void *)pr );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;


        case GET_ATT_TEXT:
            plhs[0] = mxCreateCharArray ( 2, mxsize );
            pr = mxGetData ( plhs[0] );
            buffer = mxCalloc ( mxsize[1], sizeof(mxChar) );
            status = nc_get_att_text ( ncid, varid, attname, buffer );
            plhs[1] = mexncCreateDoubleScalar ( status );

            /*
             * Copy them into the mxChar array one char at a time.
             * We apparently need to do it this way because there is
             * a mismatch between the datatypes char and mxChar.
             * */
            char_ptr = buffer;
            mxchar_ptr = (mxChar *)pr;
            for ( j = 0; j < mxsize[1]; ++j ) {
                mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
            }
            mxFree ( buffer );

            break;



        default:
            sprintf ( error_message, "unhandled opcode %d, file %s, line %d.\n",  the_op->opcode, __FILE__, __LINE__ );
            mexErrMsgTxt ( error_message );
    }


    return;
}
















/***********************************************************************
 *
 * HANDLE_NC_GET_VAR_X
 *
 * Implements the nc_get_var x_y family of function calls
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = netcdf data
 *       plhs[1] = return status of the nc_get_var function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'GET_VARX_Y' where X is '', '1', 'A', or 'S', and 
 *                  Y is SHORT, DOUBLE, etc.   For example, this 
 *                  function could be called with GET_VAR_DOUBLE, 
 *                  GET_VAR1_SHORT, etc.
 *       prhs[1] = ID of the root/parent group 
 *       prhs[2] = ID of the referenced variable 
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_get_var_x ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

    /*
     * This array will define the size of the matlab matrix.
     * */
    int     mx_size[MAX_NC_DIMS];
    
    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * NetCDF type of the variable.
     * */
    nc_type xtype;


    /*
     * Generic space for character data.
     * */
    char   *char_buffer;

    /*
     * Return status from netcdf operation.
     * */
    int      status;

    /* 
     * Number of matlab dimensions.  This can be different from the 
     * number of netcdf dimensions, because a matlab matrix ALWAYS has
     * at least rank 2.
     * */
    int num_mat_dims;

    /*
     * number of dimensions in a NetCDF file and their IDs.
     * */
    int      num_nc_dims;
    int      dimids[NC_MAX_DIMS]; 


    /*
     * loop index
     * */
    int              j;

    /*
     * number of elements in a variable hyperslab
     * */
    int             num_items;

    /*
     * Generic ptr for accessing mxChar data.
     * */
    mxChar *mxchar_ptr;

    /*
     * Generic ptr for accessing char data.
     * */
    char *char_ptr;


    char    error_message[1000];

    OPCODE  opcode = nc_op->opcode;
    char   *opname = nc_op->opname;  


    /*
     * Constitutes the position index where the data read is to begin.
     * 
     * These are not used in all cases.
     * */
    size_t          *nc_start_coord;
    size_t          *nc_count_coord;
    ptrdiff_t      *nc_stride_coord;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);

    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);


    /*
     * Unpack the optional inputs.
     * */
    switch ( nc_op->opcode ) {

        case GET_VAR_DOUBLE:
        case GET_VAR_FLOAT:
        case GET_VAR_INT:
        case GET_VAR_SHORT:
        case GET_VAR_SCHAR:
        case GET_VAR_UCHAR:
        case GET_VAR_TEXT:
            break;

        case GET_VAR1_DOUBLE:
        case GET_VAR1_FLOAT:
        case GET_VAR1_INT:
        case GET_VAR1_SHORT:
        case GET_VAR1_SCHAR:
        case GET_VAR1_UCHAR:
        case GET_VAR1_TEXT:
            check_numeric_argument_type ( prhs, nc_op->opname, 3 );
            nc_start_coord = unpackSize_t ( prhs[3] );
            break;

        case GET_VARA_DOUBLE:
        case GET_VARA_FLOAT:
        case GET_VARA_INT:
        case GET_VARA_SHORT:
        case GET_VARA_SCHAR:
        case GET_VARA_UCHAR:
        case GET_VARA_TEXT:
            check_numeric_argument_type ( prhs, nc_op->opname, 3 );
            check_numeric_argument_type ( prhs, nc_op->opname, 4 );
            nc_start_coord = unpackSize_t ( prhs[3] );
            nc_count_coord = unpackSize_t ( prhs[4] );
            break;

        case GET_VARS_DOUBLE:
        case GET_VARS_FLOAT:
        case GET_VARS_INT:
        case GET_VARS_SHORT:
        case GET_VARS_SCHAR:
        case GET_VARS_UCHAR:
        case GET_VARS_TEXT:
            check_numeric_argument_type ( prhs, nc_op->opname, 3 );
            check_numeric_argument_type ( prhs, nc_op->opname, 4 );
            check_numeric_argument_type ( prhs, nc_op->opname, 5 );
            nc_start_coord = unpackSize_t ( prhs[3] );
            nc_count_coord = unpackSize_t ( prhs[4] );
            nc_stride_coord = unpackPtrdiff_t ( prhs[5] );
            break;

        default:
            sprintf ( error_message, 
                        "unhandled opcode %d, %s, line %d, file %s\n", 
                        nc_op->opcode, nc_op->opname, 
                        __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
    }
    



    

    /*
     * Get the dimensions that define the variable.
     * */
    status = nc_inq_var ( ncid, varid, NULL, &xtype, &num_nc_dims, dimids, NULL );
    if ( status != NC_NOERR ) {
        sprintf ( error_message, 
                "nc_inq_var failed, \"%s\", line %d, file %s\n",
                nc_strerror(status), __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
    }





    /*
     * Set the rank of the output matlab array appropriately.  A matlab array
     * needs at least rank 2.
     * */
    if ( num_nc_dims < 2 ) {
        num_mat_dims = 2;
    } else { 
        num_mat_dims = num_nc_dims;
    }



    set_output_matrix_rank ( ncid, num_nc_dims, dimids, nc_count_coord, nc_op, mx_size );



    /*
     * count the number of items
     * */
    num_items = 1;
    for ( j = 0; j < num_mat_dims; ++j ) {
        num_items *= mx_size[j];
    }









    /*
     * And finally retrieve the data.
     * */
    switch ( opcode ) {

        case GET_VAR_DOUBLE:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxDOUBLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var_double ( ncid, varid, pr );
            break;

        case GET_VAR_FLOAT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxSINGLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var_float ( ncid, varid, (float *)pr );
            break;

        case GET_VAR_INT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT32_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var_int ( ncid, varid, (int *)pr );
            break;

        case GET_VAR_SHORT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT16_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var_short ( ncid, varid, (short int *)pr );
            break;

        case GET_VAR_SCHAR:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var_schar ( ncid, varid, (signed char *)pr );
            break;

        case GET_VAR_UCHAR:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxUINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var_uchar ( ncid, varid, (unsigned char *)pr );
            break;

        case GET_VAR_TEXT:
            plhs[0] = mxCreateCharArray ( num_mat_dims, mx_size );
            pr = mxGetData ( plhs[0] );

            char_buffer = mxCalloc ( num_items + 1, sizeof(char) );
            status = nc_get_var_text ( ncid, varid, char_buffer );

            /*
             * Copy them into the mxChar array one char at a time.
             * We apparently need to do it this way because there is
             * a mismatch between the datatypes char and mxChar.
             * */
            char_ptr = char_buffer;
            mxchar_ptr = (mxChar *)pr;
            for ( j = 0; j < num_items; ++j ) {
                mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
            }


            mxFree(char_buffer);
            break;


        /*
         * GET_VAR1 group
         * */
        case GET_VAR1_DOUBLE:
            plhs[0] = mxCreateNumericArray ( 1, mx_size, mxDOUBLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var1_double ( ncid, varid, nc_start_coord, pr );
            break;

        case GET_VAR1_FLOAT:
            plhs[0] = mxCreateNumericArray ( 1, mx_size, mxSINGLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var1_float ( ncid, varid, nc_start_coord, (float *)pr );
            break;

        case GET_VAR1_INT:
            plhs[0] = mxCreateNumericArray ( 1, mx_size, mxINT32_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var1_int ( ncid, varid, nc_start_coord, (int *)pr );
            break;

        case GET_VAR1_SHORT:
            plhs[0] = mxCreateNumericArray ( 1, mx_size, mxINT16_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var1_short ( ncid, varid, nc_start_coord, (short int *)pr );
            break;

        case GET_VAR1_SCHAR:
            plhs[0] = mxCreateNumericArray ( 1, mx_size, mxINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var1_schar ( ncid, varid, nc_start_coord, (signed char *)pr );
            break;

        case GET_VAR1_UCHAR:
            plhs[0] = mxCreateNumericArray ( 1, mx_size, mxUINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_var1_uchar ( ncid, varid, nc_start_coord, (unsigned char *)pr );
            break;

        case GET_VAR1_TEXT:
            char_buffer = mxCalloc ( 2, sizeof(char) );
            status = nc_get_var1_text ( ncid, varid, nc_start_coord, char_buffer );
            char_buffer[1] = '\0';
            plhs[0] = mxCreateString ( char_buffer );
            mxFree(char_buffer);
            break;



        /*
         * GET_VARA group
         * */
        case GET_VARA_DOUBLE:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxDOUBLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vara_double ( ncid, varid, nc_start_coord, nc_count_coord, (double *)pr );
            break;

        case GET_VARA_FLOAT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxSINGLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vara_float ( ncid, varid, nc_start_coord, nc_count_coord, (float *)pr );
            break;

        case GET_VARA_INT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT32_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vara_int ( ncid, varid, nc_start_coord, nc_count_coord, (int *)pr );
            break;

        case GET_VARA_SHORT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT16_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vara_short ( ncid, varid, nc_start_coord, nc_count_coord, (short int *)pr );
            break;

        case GET_VARA_SCHAR:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vara_schar ( ncid, varid, nc_start_coord, nc_count_coord, (signed char *)pr );
            break;

        case GET_VARA_UCHAR:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxUINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vara_uchar ( ncid, varid, nc_start_coord, nc_count_coord, (unsigned char *)pr );
            break;

        case GET_VARA_TEXT:
            plhs[0] = mxCreateCharArray ( num_mat_dims, mx_size );
            pr = mxGetData ( plhs[0] );

            char_buffer = mxCalloc ( num_items + 1, sizeof(mxChar) );
            status = nc_get_vara_text ( ncid, varid, nc_start_coord, nc_count_coord, char_buffer );

            /*
             * Copy them into the mxChar array one char at a time.
             * We apparently need to do it this way because there is
             * a mismatch between the datatypes char and mxChar.
             * */
            char_ptr = char_buffer;
            mxchar_ptr = (mxChar *)pr;
            for ( j = 0; j < num_items; ++j ) {
                mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
            }

            mxFree(char_buffer);
            break;


        /*
         * GET_VARS group
         * */
        case GET_VARS_DOUBLE:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxDOUBLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vars_double ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (double *)pr );
            break;

        case GET_VARS_FLOAT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxSINGLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vars_float ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (float *)pr );
            break;

        case GET_VARS_INT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT32_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vars_int ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (int *)pr );
            break;

        case GET_VARS_SHORT:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT16_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vars_short ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (short int *)pr );
            break;

        case GET_VARS_SCHAR:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vars_schar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (signed char *)pr );
            break;

        case GET_VARS_UCHAR:
            plhs[0] = mxCreateNumericArray ( num_mat_dims, mx_size, mxUINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_vars_uchar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (unsigned char *)pr );
            break;

        case GET_VARS_TEXT:
            plhs[0] = mxCreateCharArray ( num_mat_dims, mx_size );
            pr = mxGetData ( plhs[0] );

            char_buffer = mxCalloc ( num_items + 1, sizeof(mxChar) );
            status = nc_get_vars_text ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, char_buffer );

            /*
             * Copy them into the mxChar array one char at a time.
             * We apparently need to do it this way because there is
             * a mismatch between the datatypes char and mxChar.
             * */
            char_ptr = char_buffer;
            mxchar_ptr = (mxChar *)pr;
            for ( j = 0; j < num_items; ++j ) {
                mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
            }

            mxFree(char_buffer);
            break;




        default:
            sprintf ( error_message, 
                    "unhandled opcode %d, %s, line %d file %s\n", 
                    opcode, opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            

    }
    plhs[1] = mexncCreateDoubleScalar ( status );

    return;

}


















/***********************************************************************
 *
 * HANDLE_NC_GET_VARM_X
 *
 * handles the nc_get_varm family of function calls
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       prhs[0] = netcdf variable data
 *       plhs[1] = return status of the nc_get_vara function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = a variant on 'GET_VARM_X' where X is something like
 *                 'DOUBLE' or 'SHORT'
 *       prhs[1] = ID of the root/parent group of the referenced
 *                 file
 *       prhs[2] = ID of the referenced variable
 *       prhs[3] = array of indices for the starting index of the
 *                 data section desired
 *       prhs[4] = array of extents along each dimension that specify
 *                 how much contiguous data is to be read
 *       prhs[5] = array of strides along each dimension 
 *       prhs[6] = imap array that specifies how the data is to be 
 *                 transposed
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_get_varm_x 
( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *the_op 
) 
{

    /*
     * Constitutes the position index where the data write is to begin.
     * */
    size_t          *nc_start_coord;
    size_t          *nc_count_coord;
    ptrdiff_t       *nc_stride_coord;
    ptrdiff_t       *nc_imap_coord;
    
    /*
     * We have to determine ourselves how big the resulting matrix will be.
     * */
    int    result_size[MAX_NC_DIMS];

    /*
     * This is actually used by matlab to create the matrix.  It's different
     * than result_size because of the row-major-order-column-major-order issue.
     * */
    int    mx_result_size[MAX_NC_DIMS];

    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * Generic space for character data.
     * */
    char   *char_buffer;
    char   *char_ptr;

    /*
     * Generic ptr for accessing mxChar data.
     * */
    mxChar *mxchar_ptr;


    /*
     * Return status from netcdf operation.
     * */
    int      status;

    char    error_message[1000];

    /*
     * number of dimensions in a NetCDF file and their IDs.
     * */
    int             ndims;
    int             dimids[NC_MAX_DIMS]; 
    size_t          netcdf_dimension_sizes[NC_MAX_DIMS]; 

    /*
     * number of elements in a variable hyperslab
     * */
    int             num_requested_elements;
    int             total_num_elements;

    /*
     * loop index
     * */
    int j;

    OPCODE  opcode = the_op->opcode;
    char   *opname = the_op->opname;  

    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, opname, 1 );
    check_numeric_argument_type ( prhs, opname, 2 );
    check_numeric_argument_type ( prhs, opname, 3 );
    check_numeric_argument_type ( prhs, opname, 4 );
    check_numeric_argument_type ( prhs, opname, 5 );
    check_numeric_argument_type ( prhs, opname, 6 );



    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    nc_start_coord = unpackSize_t ( prhs[3] );
    nc_count_coord = unpackSize_t ( prhs[4] );
    nc_stride_coord = unpackPtrdiff_t ( prhs[5] );
    nc_imap_coord = unpackPtrdiff_t ( prhs[6] );



    /*
     * Get the dimensions that define the variable.
     * */
    status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
    if ( status != NC_NOERR ) {
        sprintf ( error_message, "nc_inq_var failed, line %d, file %s\n", __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
    }



    /*
     * Check that the lengths of the input "start", "count", "stride", and "imap" 
     * matrices match the rank of the netcdf variable.
     */
    varm_coord_sanity_check ( prhs, ndims );






    /*
     * Get the size of each dimension.  
     * */
    for ( j = 0; j < ndims; ++j ) {

        status = nc_inq_dimlen ( ncid, dimids[j], &netcdf_dimension_sizes[j] );
        if ( status != NC_NOERR ) {
            sprintf ( error_message, "nc_inq_dimlen failed on dimid %d \n", dimids[j] );
            mexErrMsgTxt ( error_message );
        }


    }


    /*
     * Figure out the total number of elements the user thinks they
     * are asking for.  Also compute the total number of elements.
     * */
    num_requested_elements = 1;
    total_num_elements = 1;
    for ( j = 0; j < ndims; ++j ) {
        num_requested_elements *= nc_count_coord[j];
        total_num_elements *= netcdf_dimension_sizes[j];
    }


    determine_varm_output_size ( ndims, num_requested_elements, 
            nc_count_coord, nc_stride_coord, nc_imap_coord, 
            result_size );



    /*
     * We need to set the dimensions of the matrix as the reverse
     * of how it is defined by the user.  This makes the
     * size of the matrix seem transposed (upon return to matlab), 
     * but otherwise the data gets layed out incorrectly due to 
     * the difference between row-major order (C) and column-major 
     * order (matlab).
     * */
    for ( j = 0; j < ndims; ++j ) {
        mx_result_size[ndims - j - 1] = result_size[j];
    }

    switch ( opcode ) {

        case GET_VARM_DOUBLE:
            plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxDOUBLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_varm_double ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (double *)pr );
            break;

        case GET_VARM_FLOAT:
            plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxSINGLE_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_varm_float ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (float *)pr );
            break;

        case GET_VARM_INT:
            plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxINT32_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_varm_int ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (int *)pr );
            break;

        case GET_VARM_SHORT:
            plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxINT16_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_varm_short ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (short int *)pr );
            break;

        case GET_VARM_SCHAR:
            plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_varm_schar ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (signed char *)pr );
            break;

        case GET_VARM_UCHAR:
            plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxUINT8_CLASS, mxREAL );
            pr = mxGetData ( plhs[0] );
            status = nc_get_varm_uchar ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (unsigned char *)pr );
            break;

        case GET_VARM_TEXT:
            plhs[0] = mxCreateCharArray ( ndims, mx_result_size );
            pr = mxGetData ( plhs[0] );
        
            char_buffer = mxCalloc ( num_requested_elements + 1, sizeof(mxChar) );
            status = nc_get_varm ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    (void *)char_buffer );
        
            /*
             * Copy them into the mxChar array one char at a time.
             * We apparently need to do it this way because there is
             * a mismatch between the datatypes char and mxChar.
             * */
            char_ptr = char_buffer;
            mxchar_ptr = (mxChar *)pr;
            for ( j = 0; j < num_requested_elements; ++j ) {
                mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
            }
            plhs[1] = mexncCreateDoubleScalar ( status );

            mxFree(char_buffer);
            break;



        default:
            sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );

    }
    plhs[1] = mexncCreateDoubleScalar ( status );


    return;

}









/***********************************************************************
 *
 * HANDLE_NC_PUT_ATT
 *
 * Implements the nc_put_att function
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_put_att_x function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'PUT_ATT'
 *       prhs[1] = ID of the root/parent group of the parent variable
 *                 of the proposed/referenced attribute
 *       prhs[2] = ID of the parent variable
 *       prhs[3] = name of the proposed/referenced attribute
 *       prhs[4] = netcdf datatype of the proposed/referenced attribute
 *       prhs[5] = length of the attribute
 *       prhs[6] = proposed attribute data
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_put_att   
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *the_op 
) 
{

    /* 
     * NetCDF File ID 
     * */
    int      ncid;
    
    /* 
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * This points to actual attribute data.
     * */
    double  *attribute_value;

    /*
     * Generic space for character data.
     * */
    char   *char_buffer;

    /*
     * NCTYPE of the attribute
     * */
    nc_type datatype;


    /*
     * length of the attribute
     * */
    size_t att_len;

    /*
     * Return status from netcdf operation.
     * */
    int      status;

    /*
     * name of attribute
     * */
    char     *name;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, the_op->opname, 1 );
    check_numeric_argument_type ( prhs, the_op->opname, 2 );
    check_char_argument_type    ( prhs, the_op->opname, 3 );
    check_char_or_numeric_argument_type ( prhs, the_op->opname, 4 );


    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    name = unpackString(prhs[3]);
    datatype = unpackDataType ( prhs[4] );
    pr = mxGetData ( prhs[5] );
    att_len = (size_t)(pr[0]);
    attribute_value = mxGetData ( prhs[6] );


    switch ( the_op->opcode ) {

        case PUT_ATT_DOUBLE:
            pr = mxGetData ( prhs[6] );
            status = nc_put_att_double ( ncid, varid, name, datatype, att_len, (double *)pr );
            break;

        case PUT_ATT_FLOAT:
            pr = mxGetData ( prhs[6] );
            status = nc_put_att_float ( ncid, varid, name, datatype, att_len, (float *)pr );
            break;

        case PUT_ATT_INT:
            pr = mxGetData ( prhs[6] );
            status = nc_put_att_int ( ncid, varid, name, datatype, att_len, (int *)pr );
            break;

        case PUT_ATT_SHORT:
            pr = mxGetData ( prhs[6] );
            status = nc_put_att_short ( ncid, varid, name, datatype, att_len, (short int *)pr );
            break;

        case PUT_ATT_SCHAR:
            pr = mxGetData ( prhs[6] );
            status = nc_put_att_schar ( ncid, varid, name, datatype, att_len, (signed char *)pr );
            break;

        case PUT_ATT_UCHAR:
            pr = mxGetData ( prhs[6] );
            status = nc_put_att_uchar ( ncid, varid, name, datatype, att_len, (unsigned char *)pr );
            break;

        case PUT_ATT_TEXT:
            char_buffer = mxArrayToString ( prhs[6] );
            status = nc_put_att_text ( ncid, varid, name, att_len, char_buffer );
            mxFree ( char_buffer );
            break;

        default:
            sprintf ( error_message, "unhandled operation %s", the_op->opname );
            mexErrMsgTxt ( error_message ); 


    }

    plhs[0] = mexncCreateDoubleScalar ( status );


    return;
}












/***********************************************************************
 *
 * HANDLE_NC_PUT_VAR_X
 *
 * Implements the nc_put_var family of function calls
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of nc_put_varm function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'PUT_VAR_X' where X is something like SHORT, DOUBLE,
 *                 etc.
 *       prhs[1] = ID of the root/parent group 
 *       prhs[2] = ID of the referenced variable 
 *       prhs[3] = data to be written out to file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_put_var_x ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
    op            *nc_op ) 
{

    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * This points to actual matrix data.
     * */
    void  *data_buffer;


    /*
     * Return status from netcdf operation.
     * */
    int      status;


    char    error_message[1000];

    OPCODE  opcode = nc_op->opcode;
    char   *opname = nc_op->opname;  


    /*
     * Generic space for character data.
     * */
    char   *char_buffer;


    
    /*
     * Constitutes the position index where the data write is to begin.
     * 
     * These are not used in all cases.
     * */
    size_t          *nc_start_coord;
    size_t          *nc_count_coord;
    ptrdiff_t       *nc_stride_coord;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, opname, 1 );
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);

    check_numeric_argument_type ( prhs, opname, 2 );
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);




    /*
     * Unpack the optional arguments.
     * */
    switch ( nc_op->opcode ) {
        case PUT_VAR_DOUBLE:
        case PUT_VAR_FLOAT:
        case PUT_VAR_INT:
        case PUT_VAR_SHORT:
        case PUT_VAR_SCHAR:
        case PUT_VAR_UCHAR:
        case PUT_VAR_TEXT:
            data_buffer = mxGetData ( prhs[3] );
            break;

        case PUT_VAR1_DOUBLE:
        case PUT_VAR1_FLOAT:
        case PUT_VAR1_INT:
        case PUT_VAR1_SHORT:
        case PUT_VAR1_SCHAR:
        case PUT_VAR1_UCHAR:
        case PUT_VAR1_TEXT:
            check_numeric_argument_type ( prhs, opname, 3 );
            nc_start_coord = unpackSize_t ( prhs[3] );
            data_buffer = mxGetData ( prhs[4] );
            break;

        case PUT_VARA_DOUBLE:
        case PUT_VARA_FLOAT:
        case PUT_VARA_INT:
        case PUT_VARA_SHORT:
        case PUT_VARA_SCHAR:
        case PUT_VARA_UCHAR:
        case PUT_VARA_TEXT:
            check_numeric_argument_type ( prhs, opname, 3 );
            check_numeric_argument_type ( prhs, opname, 4 );
            nc_start_coord = unpackSize_t ( prhs[3] );
            nc_count_coord = unpackSize_t ( prhs[4] );
            data_buffer = mxGetData ( prhs[5] );
            break;

        case PUT_VARS_DOUBLE:
        case PUT_VARS_FLOAT:
        case PUT_VARS_INT:
        case PUT_VARS_SHORT:
        case PUT_VARS_SCHAR:
        case PUT_VARS_UCHAR:
        case PUT_VARS_TEXT:
            check_numeric_argument_type ( prhs, opname, 3 );
            check_numeric_argument_type ( prhs, opname, 4 );
            check_numeric_argument_type ( prhs, opname, 5 );
            nc_start_coord = unpackSize_t ( prhs[3] );
            nc_count_coord = unpackSize_t ( prhs[4] );
            nc_stride_coord = unpackPtrdiff_t ( prhs[5] );
            data_buffer = mxGetData ( prhs[6] );
            break;

        default:
            sprintf ( error_message, 
                    "unhandled opcode %s, file %s, line %d\n", 
                     nc_op->opname, __FILE__, __LINE__ );
            mexErrMsgTxt ( error_message ); 

    }





    switch ( opcode ) {

        case PUT_VAR_DOUBLE:
            status = nc_put_var_double ( ncid, varid, (double *)data_buffer );
            break;

        case PUT_VAR_FLOAT:
            status = nc_put_var_float ( ncid, varid, (float *)data_buffer );
            break;

        case PUT_VAR_INT:
            status = nc_put_var_int ( ncid, varid, (int *)data_buffer );
            break;

        case PUT_VAR_SHORT:
            status = nc_put_var_short ( ncid, varid, (short int *)data_buffer );
            break;

        case PUT_VAR_SCHAR:
            status = nc_put_var_schar ( ncid, varid, (signed char *)data_buffer );
            break;

        case PUT_VAR_UCHAR:
            status = nc_put_var_uchar ( ncid, varid, (unsigned char *)data_buffer );
            break;

        case PUT_VAR_TEXT:
            char_buffer = mxArrayToString ( prhs[3] );
            status = nc_put_var_text ( ncid, varid, char_buffer );
            mxFree ( char_buffer );
            break;


        /*
         * VARA section.
         * */
        case PUT_VAR1_DOUBLE:
            status = nc_put_var1_double ( ncid, varid, nc_start_coord, (double *)data_buffer );
            break;

        case PUT_VAR1_FLOAT:
            status = nc_put_var1_float ( ncid, varid, nc_start_coord, (float *)data_buffer );
            break;

        case PUT_VAR1_INT:
            status = nc_put_var1_int ( ncid, varid, nc_start_coord, (int *)data_buffer );
            break;

        case PUT_VAR1_SHORT:
            status = nc_put_var1_short ( ncid, varid, nc_start_coord, (short int *)data_buffer );
            break;

        case PUT_VAR1_SCHAR:
            status = nc_put_var1_schar ( ncid, varid, nc_start_coord, (signed char*)data_buffer );
            break;

        case PUT_VAR1_UCHAR:
            status = nc_put_var1_uchar ( ncid, varid, nc_start_coord, (unsigned char*)data_buffer );
            break;

        case PUT_VAR1_TEXT:
            char_buffer = mxArrayToString ( prhs[4] );
            status = nc_put_var1_text ( ncid, varid, nc_start_coord, char_buffer );
            mxFree ( char_buffer );
            break;


        /*
         * VARA section.
         * */
        case PUT_VARA_DOUBLE:
            status = nc_put_vara_double ( ncid, varid, 
                    nc_start_coord, nc_count_coord, (double *)data_buffer );
            break;

        case PUT_VARA_FLOAT:
            status = nc_put_vara_float ( ncid, varid, 
                    nc_start_coord, nc_count_coord, 
                    (float *)data_buffer );
            break;

        case PUT_VARA_INT:
            status = nc_put_vara_int ( ncid, varid, 
                    nc_start_coord, nc_count_coord, 
                    (int *)data_buffer );
            break;

        case PUT_VARA_SHORT:
            status = nc_put_vara_short ( ncid, varid, 
                    nc_start_coord, nc_count_coord, 
                    (short int *)data_buffer );
            break;

        case PUT_VARA_SCHAR:
            status = nc_put_vara_schar ( ncid, varid, 
                    nc_start_coord, nc_count_coord, 
                    (signed char *)data_buffer );
            break;

        case PUT_VARA_UCHAR:
            status = nc_put_vara_uchar ( ncid, varid, 
                    nc_start_coord, nc_count_coord, 
                    (unsigned char *)data_buffer );
            break;

        case PUT_VARA_TEXT:
            char_buffer = mxArrayToString ( prhs[5] );
            status = nc_put_vara_text ( ncid, varid, nc_start_coord, nc_count_coord, char_buffer );
            mxFree ( char_buffer );
            break;


        /*
         * VARS section
         * */
        case PUT_VARS_DOUBLE:
            status = nc_put_vars_double ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            (double *)data_buffer );
            break;

        case PUT_VARS_FLOAT:
            status = nc_put_vars_float ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            (float *)data_buffer );
            break;

        case PUT_VARS_INT:
            status = nc_put_vars_int ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            (int *)data_buffer );
            break;

        case PUT_VARS_SHORT:
            status = nc_put_vars_short ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            (short int *)data_buffer );
            break;

        case PUT_VARS_SCHAR:
            status = nc_put_vars_schar ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            (signed char *)data_buffer );
            break;

        case PUT_VARS_UCHAR:
            status = nc_put_vars_uchar ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            (unsigned char *)data_buffer );
            break;

        case PUT_VARS_TEXT:
            char_buffer = mxArrayToString ( prhs[6] );
            status = nc_put_vars_text ( ncid, 
                            varid, 
                            nc_start_coord, 
                            nc_count_coord, 
                            nc_stride_coord, 
                            char_buffer );
            mxFree ( char_buffer );
            break;


        default:
            sprintf ( error_message, 
                    "unhandled opcode %d, %s, line %d file %s\n", 
                    opcode, opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
            
    }
    plhs[0] = mexncCreateDoubleScalar ( status );

    return;
}














/***********************************************************************
 *
 * HANDLE_NC_PUT_VARM_X
 *
 * Implements the nc_put_varm family of function calls
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_put_vara function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'PUT_VARM_X' where X is something like SHORT, DOUBLE,
 *                 etc.
 *       prhs[1] = ID of the root/parent group 
 *       prhs[2] = ID of the referenced variable 
 *       prhs[3] = array of indices specifying the index in the variable
 *                 where the first of the data values will be written. 
 *       prhs[4] = array of indices specifying the edge lengths along 
 *                 each dimension of the block of data values to be 
 *                 written. 
 *       prhs[5] = array specifying the sampling interval along each 
 *                 dimension of the netCDF variable. 
 *       prhs[6] = imap array that specifies how the data is to be 
 *                 permuted
 *       prhs[7] = data to be written out to file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_put_varm_x ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

    /*
     * Constitutes the position index where the data write is to begin.
     * */
    size_t          *nc_start_coord;
    size_t          *nc_count_coord;
    ptrdiff_t       *nc_stride_coord;
    ptrdiff_t       *nc_imap_coord;
    
    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * This points to actual matrix data.
     * */
    double  *data_buffer;

    /*
     * Generic space for character data.
     * */
    char   *char_buffer;

    /*
     * Return status from netcdf operation.
     * */
    int      status;


    char    error_message[1000];

    OPCODE  opcode = nc_op->opcode;
    char   *opname = nc_op->opname;  




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_numeric_argument_type ( prhs, nc_op->opname, 3 );
    check_numeric_argument_type ( prhs, nc_op->opname, 4 );
    check_numeric_argument_type ( prhs, nc_op->opname, 5 );
    check_numeric_argument_type ( prhs, nc_op->opname, 6 );
    
    




    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    nc_start_coord = unpackSize_t ( prhs[3] );
    nc_count_coord = unpackSize_t ( prhs[4] );
    nc_stride_coord = unpackPtrdiff_t ( prhs[5] );
    nc_imap_coord = unpackPtrdiff_t ( prhs[6] );
    data_buffer = mxGetData ( prhs[7] );


    switch ( opcode ) {

        case PUT_VARM_DOUBLE:
            status = nc_put_varm_double ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    data_buffer );
            break;

        case PUT_VARM_FLOAT:
            status = nc_put_varm_float ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    (float *)data_buffer );
            break;

        case PUT_VARM_INT:
            status = nc_put_varm_int ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    (int *)data_buffer );
            break;

        case PUT_VARM_SHORT:
            status = nc_put_varm_short ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    (short int *)data_buffer );
            break;

        case PUT_VARM_SCHAR:
            status = nc_put_varm_schar ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    (signed char *)data_buffer );
            break;

        case PUT_VARM_UCHAR:
            status = nc_put_varm_uchar ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    (unsigned char *)data_buffer );
            break;

        case PUT_VARM_TEXT:
            char_buffer = mxArrayToString ( prhs[7] );
            status = nc_put_varm_text ( ncid, varid, 
                    nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
                    char_buffer );
            mxFree ( char_buffer );
            break;

        default:
            sprintf ( error_message, 
                    "unhandled opcode %d, %s, line %d file %s\n", 
                    opcode, opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            
    }

    plhs[0] = mexncCreateDoubleScalar ( status );
    return;

}
















/***********************************************************************
 *
 * HANDLE_NC_INQ_ATTID:
 *
 * code for handling the nc_inq_attid routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  The requested attribute ID is
 *     stored in plhs[0], and the return status of the nc_inq_attid 
 *     function call is placed into plhs[1].
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_ATTID'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the parent variable of the referenced 
 *                 attribute
 *       prhs[3] = name of the referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_attid 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

    /* 
     * name of netcdf variable 
     * */
    char    *attname;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
    * numeric id of an attribute
    * */
    int     attribute_id;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
    
    
    


    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    attname = unpackString(prhs[3]);
            
    status = nc_inq_attid ( ncid, varid, attname, &attribute_id );
    plhs[0] = mexncCreateDoubleScalar (attribute_id);
    plhs[1] = mexncCreateDoubleScalar (status);
        
            


    return;

}


            
/***********************************************************************
 *
 * HANDLE_NC_INQ_ATTNAME:
 *
 * code for handling the nc_inq_attname routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the requested attribute name 
 *       plhs[1] = return status of the nc_inq_attname function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_ATTNAME'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of parent variable of the referenced attribute
 *       prhs[3] = ID of the referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_attname 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

    /* 
     * name of netcdf variable 
     * */
    char    *attname;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;

    /*
     * numeric id of an attribute
     * */
    int     attribute_id;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_numeric_argument_type ( prhs, nc_op->opname, 3 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    pr = mxGetData ( prhs[3] );
    attribute_id = (int)(pr[0]);

    attname = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));

    status = nc_inq_attname ( ncid, varid, attribute_id, attname );
    if ( status == NC_NOERR ) {
        plhs[0] = mxCreateString (attname);
        plhs[1] = mexncCreateDoubleScalar (status);
    } else {
        plhs[0] = mxCreateString ("");
        plhs[1] = mexncCreateDoubleScalar (status);
    }


    mxFree(attname);
    return;

}
























/***********************************************************************
 *
 * HANDLE_NC_INQ:
 *
 * code for handling the nc_inq routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = number of dimensions in the netcdf file/group
 *       plhs[1] = number of variables in the netcdf file/group
 *       plhs[2] = number of global attributes in the netcdf file/group
 *       plhs[3] = id of the record dimension, if any
 *       plhs[4] = return status of the nc_inq function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /* 
     * Return arguments.
     * */
    int      ndims;     /* number of dimensions in the netcdf file */
    int      nvars;     /* number of variables in the netcdf file */
    int      natts;     /* number of global attributes in the netcdf file */
    int      recdim;    /* ID of the record dimension in the netcdf file */


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_inq (ncid, &ndims, &nvars, &natts, &recdim);
    plhs[0] = mexncCreateDoubleScalar(ndims);
    plhs[1] = mexncCreateDoubleScalar(nvars);
    plhs[2] = mexncCreateDoubleScalar(natts);
    plhs[3] = mexncCreateDoubleScalar(recdim);
    plhs[4] = mexncCreateDoubleScalar(status);
    return;
            

}










/***********************************************************************
 *
 * HANDLE_NC_INQ_NDIMS:
 *
 * code for handling the nc_inq_ndims routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = number of dimensions in the netcdf file
 *       plhs[1] = return status of the nc_inq_ndims function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_NDIMS'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_ndims 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /* 
     * Return arguments.
     * */
    int      ndims;     /* number of dimensions in the netcdf file */


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_inq_ndims (ncid, &ndims);
    plhs[0] = mexncCreateDoubleScalar(ndims);
    plhs[1] = mexncCreateDoubleScalar(status);
    return;
            


}









/***********************************************************************
 *
 * HANDLE_NC_INQ_NVARS:
 *
 * code for handling the nc_inq_nvars routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = number of variables in the netcdf file
 *       plhs[1] = return status of the nc_inq_nvars function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_NVARS'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_nvars 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * Number of variables in a netcdf file
     * */
    int      nvars;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_inq_nvars (ncid, &nvars);
    plhs[0] = mexncCreateDoubleScalar(nvars);
    plhs[1] = mexncCreateDoubleScalar(status);
    return;
            




}









/***********************************************************************
 *
 * HANDLE_NC_INQ_NATTS:
 *
 * code for handling the nc_inq_natts routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = number of global attributes in the netcdf file
 *       plhs[1] = return status of the nc_inq_natts function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_NATTS'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_natts
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * Number of global attributes in a netcdf file
     * */
    int      natts;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_inq_natts (ncid, &natts);
    plhs[0] = mexncCreateDoubleScalar(natts);
    plhs[1] = mexncCreateDoubleScalar(status);
    return;
            

}









/***********************************************************************
 *
 * HANDLE_NC_INQ_ATT:
 *
 * code for handling the nc_inq_att routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = datatype of the referenced attribute
 *       plhs[1] = length of the referenced attribute
 *       plhs[2] = return status of the nc_inq_ndims function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_ATT'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the parent variable of the referenced 
 *                 attribute
 *       prhs[3] = name of referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_att 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

    /*
     * name of attribute
     * */
    char    *name;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * NetCDF attribute datatype and attribute length
     * */
    nc_type      datatype;
    size_t      attribute_length;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
            

        
    /*
     * Unpack the parameters.
     * */
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    name = unpackString(prhs[3]);
            
    status = nc_inq_att ( ncid, varid, name, &datatype, &attribute_length );
    if ( status < 0 ) {
        plhs[0] = mexncCreateDoubleScalar ( mxGetNaN() );
        plhs[1] = mexncCreateDoubleScalar ( mxGetNaN() );
    } else {
        plhs[0] = mexncCreateDoubleScalar (datatype);
        plhs[1] = mexncCreateDoubleScalar (attribute_length);
    }
    plhs[2] = mexncCreateDoubleScalar (status);
        
    return;
            




}










/***********************************************************************
 *
 * HANDLE_NC_INQ_ATTLEN:
 *
 * code for handling the nc_inq_attlen routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = length of the referenced attribute
 *       plhs[1] = return status of the nc_inq_attlen function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_ATTTLEN'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the parent variable of the referenced 
 *                 attribute
 *       prhs[3] = name of referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_attlen 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

    /*
     * name of attribute
     * */
    char    *name;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * NetCDF attribute datatype and attribute length
     * */
    size_t      attribute_length;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
    
            
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    name = unpackString(prhs[3]);
    
    status = nc_inq_attlen ( ncid, varid, name, &attribute_length );
    plhs[0] = mexncCreateDoubleScalar ( attribute_length );
    plhs[1] = mexncCreateDoubleScalar (status);
        
            
    return;

}











/***********************************************************************
 *
 * HANDLE_NC_INQ_ATTTYPE:
 *
 * code for handling the nc_inq_atttype routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = datatype of the referenced attribute
 *       plhs[1] = return status of the nc_inq_ndims function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_ATTTYPE'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the parent variable of the referenced 
 *                 attribute
 *       prhs[3] = name of referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_atttype 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs 
         * */
        int ncid;
        int varid;

    /*
     * name of attribute
     * */
    char    *name;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;



    /*
     * Enumerated NetCDF datatype.
     * */
    nc_type datatype;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    name = unpackString(prhs[3]);

    status = nc_inq_atttype ( ncid, varid, name, &datatype );
    plhs[0] = mexncCreateDoubleScalar (datatype);
    plhs[1] = mexncCreateDoubleScalar (status);
        

    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_DIM:
 *
 * code for handling the nc_inq_dim routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = name of the referenced dimension
 *       plhs[1] = length of the referenced dimension
 *       plhs[2] = return status of the nc_inq_dim function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_DIMID'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced dimension
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_dim 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;

    /*
     * name of attribute
     * */
    char    *name;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * length of a dimension
     * */
    size_t  dim_length;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    dimid = (int)(pr[0]);

    name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));

    status = nc_inq_dim ( ncid, dimid, name, & dim_length);
    plhs[0] = mxCreateString ( name );
    plhs[1] = mexncCreateDoubleScalar ( (double) dim_length );
    plhs[2] = mexncCreateDoubleScalar (status);

    mxFree(name);
    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_DIMID:
 *
 * code for handling the nc_inq_dimid routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = identifier of the referenced dimension
 *       plhs[1] = return status of the nc_inq_ndims function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_DIMID'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = name of the referenced dimension
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_dimid 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;

    /*
     * name of attribute
     * */
    char    *name;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_char_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    name = unpackString(prhs[2]);
    status = nc_inq_dimid( ncid, name, &dimid);
    plhs[0] = mexncCreateDoubleScalar(dimid);
    plhs[1] = mexncCreateDoubleScalar(status);

    return;


}













/***********************************************************************
 *
 * HANDLE_NC_INQ_DIMLEN:
 *
 * code for handling the nc_inq_dimlen routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = length of the referenced dimension
 *       plhs[1] = return status of the nc_inq_dimlen function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_DIMID'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced dimension
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_dimlen 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;


    /*
     * length of a dimension
     * */
    size_t  dim_length;



    /* 
     * Return status from netcdf operation.  
     * */
    int      status;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    dimid = (int)(pr[0]);
    status = nc_inq_dimlen ( ncid, dimid, & dim_length);
    plhs[0] = mexncCreateDoubleScalar ( (double) dim_length );
    plhs[1] = mexncCreateDoubleScalar (status);


    return;


}













/***********************************************************************
 *
 * HANDLE_NC_INQ_DIMNAME:
 *
 * code for handling the nc_inq_dimname routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = name of the referenced dimension
 *       plhs[1] = return status of the nc_inq_ndims function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_DIMNAME'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced dimension
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_dimname 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;




    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * name of dimension
     * */
    char    *name;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    dimid = (int)(pr[0]);
    name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
    status = nc_inq_dimname ( ncid, dimid, name );
    plhs[0] = mxCreateString ( name );
    plhs[1] = mexncCreateDoubleScalar (status);


    mxFree(name);
    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_VAR:
 *
 * code for handling the nc_inq_var routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = name of the referenced variable
 *       plhs[1] = datatype of the referenced variable
 *       plhs[2] = number of dimensions for the referenced variable
 *       plhs[3] = identifiers for each of the dimensions
 *       plhs[4] = number of attributes for the referenced variable
 *       plhs[5] = return status of the nc_inq_var function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_VAR'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced variable
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_var 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /*
     * UNIDATA defined type of a variable.
     * */
    nc_type  datatype;


    /*
     * Loop index 
     * */
    int j;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * name of dimension
     * */
    char    *name;


    /*
     * number of dimensions in a NetCDF file and their IDs.
     * */
    int     ndims;
    int     dimids[NC_MAX_DIMS]; 


    /*
     * Number of attributes for a netcdf variable.
     * */
    int      natts;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
    
    status = nc_inq_var ( ncid, varid, name, &datatype, &ndims, dimids, &natts );
    if ( status < 0 ) {
        plhs[0] = mxCreateString ("");
        plhs[1] = mexncCreateDoubleScalar (-1);
        plhs[2] = mexncCreateDoubleScalar (-1);
        plhs[3] = mexncCreateDoubleScalar (-1);
        plhs[4] = mexncCreateDoubleScalar (-1);
        plhs[5] = mexncCreateDoubleScalar (status);
    } else {
        plhs[0] = mxCreateString (name);
        plhs[1] = mexncCreateDoubleScalar (datatype);
        plhs[2] = mexncCreateDoubleScalar (ndims);

        /*
         * Copy the dimension ids into the matrix
         * */
        plhs[3] = mxCreateDoubleMatrix ( 1, ndims, mxREAL );
        pr = mxGetData ( plhs[3] );
        for ( j = 0; j < ndims; ++j ) {
            pr[j] = dimids[j];
        }


        plhs[4] = mexncCreateDoubleScalar (natts);
        plhs[5] = mexncCreateDoubleScalar (status);
    }
        


    mxFree(name);
    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_VARNAME:
 *
 * code for handling the nc_inq_varname routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = name of the referenced variable
 *       plhs[1] = return status of the nc_inq_varname function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_VARTYPE'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced variable
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_varname 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;




    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * name of dimension
     * */
    char    *name;



    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
            
    status = nc_inq_varname ( ncid, varid, name );
    if ( status < 0 ) {
        plhs[0] = mxCreateString ("");
        plhs[1] = mexncCreateDoubleScalar (status);
    } else {
        plhs[0] = mxCreateString (name);
        plhs[1] = mexncCreateDoubleScalar (status);
    }
        

    mxFree(name);
    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_VARTYPE:
 *
 * code for handling the nc_inq_vartype routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = datatype of the referenced variable
 *       plhs[1] = return status of the nc_inq_vartype function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_VARTYPE'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced variable
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_vartype 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * UNIDATA defined type of a variable.
     * */
    nc_type  datatype;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    status = nc_inq_vartype ( ncid, varid, &datatype );
    plhs[0] = mexncCreateDoubleScalar (datatype);
    plhs[1] = mexncCreateDoubleScalar (status);



    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_VARNDIMS:
 *
 * code for handling the nc_inq_varndims routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = number of dimensions of the referenced variable
 *       plhs[1] = return status of the nc_inq_varndims function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_VARNDIMS'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced variable
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_varndims 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * number of dimensions in a NetCDF file and their IDs.
     * */
    int     ndims;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    status = nc_inq_varndims ( ncid, varid, &ndims );
    plhs[0] = mexncCreateDoubleScalar (ndims);
    plhs[1] = mexncCreateDoubleScalar (status);

    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_VARDIMID:
 *
 * code for handling the nc_inq_vardimid routine.
 *
 * This is a strange case.  Rather than call nc_inq_vardimid
 * we just call nc_inq_var with NULL in place of certain
 * arguments.  We need to have ndims in order to properly
 * size the dimids matrix.  Otherwise we'd make two calls
 * instead of one.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = array of dimension ids for the referenced variable
 *       plhs[1] = return status of the nc_inq_vardimid function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_VARDIMID'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced variable
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_vardimid 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

    /*
     * Loop index.
     * */
    int j;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * number of dimensions in a NetCDF file and their IDs.
     * */
    int     ndims;
    int     dimids[NC_MAX_DIMS]; 






    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
    if ( status != NC_NOERR ) {

        plhs[0] = mexncCreateDoubleScalar ( mxGetNaN() );

    } else {

        /*
         * Copy the dimension ids into the matrix
         * */
        plhs[0] = mxCreateDoubleMatrix ( 1, ndims, mxREAL );
        pr = mxGetData ( plhs[0] );
        for ( j = 0; j < ndims; ++j ) {
            pr[j] = dimids[j];
        }

    }
        
    plhs[1] = mexncCreateDoubleScalar (status);

    return;

}













/***********************************************************************
 *
 * HANDLE_NC_INQ_VARNATTS:
 *
 * code for handling the nc_inq_varnatts routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = number of attributes of the referenced variable
 *       plhs[1] = return status of the nc_inq_varnatts function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = 'INQ_VARNATTS'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = ID of the referenced variable
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_varnatts 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * number of variable attributes
     * */
    int     natts;






    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    
            
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    status = nc_inq_varnatts ( ncid, varid, &natts );
    plhs[0] = mexncCreateDoubleScalar (natts);
    plhs[1] = mexncCreateDoubleScalar (status);

    return;

}










/***********************************************************************
 *
 * HANDLE_NC_INQ_UNLIMDIM:
 *
 * code for handling the nc_inq_unlimdim routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = ID of the unlimited dimension (if any) of the 
 *                 referenced variable
 *       plhs[1] = return status of the nc_inq_varname function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_UNLIMDIM' or 'inq_unlimdim'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_unlimdim 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file id 
         * */
        int ncid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * dimension id for unlimited dimension
     * */
    int      recdim;





    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );

    
            
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_inq_unlimdim ( ncid, &recdim );
    plhs[0] = mexncCreateDoubleScalar(recdim);
    plhs[1] = mexncCreateDoubleScalar(status);

    return;

}










/***********************************************************************
 *
 * HANDLE_NC_INQ_VARID:
 *
 * code for handling the nc_inq_varid routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = ID of the referenced variable
 *       plhs[1] = return status of the nc_inq_varname function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'INQ_VARID' or 'inq_varid'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = name of referenced variable
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_inq_varid 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * name of variable
     * */
    char    *name;








    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_char_argument_type ( prhs, nc_op->opname, 2 );
    
            
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    name = unpackString(prhs[2]);
    status = nc_inq_varid ( ncid, name, &varid );
    plhs[0] = mexncCreateDoubleScalar(varid);
    plhs[1] = mexncCreateDoubleScalar(status);
    return;

}











/***********************************************************************
 *
 * HANDLE_NC_OPEN:
 *
 * code for handling the nc_open routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = File ID (ncid) of the just-opened netcdf file.
 *       plhs[1] = return status of the nc_open function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  If the user is a complete moron,
 *     they will use two input arguments instead of three, relying on
 *     the deprecated old behavior that the open mode would be 
 *     NC_NO_WRITE in that case.  Bad, bad, bad.
 *       prhs[0] = 'OPEN' or 'open'
 *       prhs[1] = path to the netcdf file
 *       prhs[2] = proposed open mode for the netcdf file.
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_open 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /*
     * file handle
     * */
    int ncid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * path of netcdf file
     * */
    char    *netcdf_filename;



    /*
     * NetCDF file opening mode
     * */
    int     mode;






    /*
     * Make sure that the inputs are the right type.
     * */
    check_char_argument_type ( prhs, nc_op->opname, 1 );

    


    /*
     * The mode argument has historically been something like 'write', which is
     * a character data type.  This is now frowned upon.  The user should use a mnemonic
     * constant instead.
     * */
    if ( nrhs == 3 ) {
        if ( mxIsChar(prhs[2]) == true) {

            int num_chars = mxGetM ( prhs[2] ) * mxGetN ( prhs[2] );
            char smode[NC_MAX_NAME];

            status = mxGetString( prhs[2], smode, num_chars+1 );
            if (status != 0) {
                sprintf ( error_message, "mxGetString failed, line %d file \"%s\"\n", __LINE__, __FILE__ );
                mexErrMsgTxt ( error_message );
                return;
            }

            mode = unpack_char_file_mode ( smode );

        
        }

        else if ( mxIsDouble(prhs[2]) == true ) {
            pr = mxGetPr ( prhs[2] );
            mode = (int) pr[0];
        } 
        
        
        else {
                sprintf ( error_message, 
                    "operation \"%s\":  mode argument must be either character or numeric, line %d file \"%s\"\n", 
                    nc_op->opname, __LINE__, __FILE__ );
                mexErrMsgTxt ( error_message );
        }

        
    /*
     * If this argument has not been supplied, then the default is to 
     * open the file in NC_NOWRITE mode.  Another unfortunate historical 
     * holdover.
     * */
    } else {
        mode = NC_NOWRITE;
    }






            
    netcdf_filename = unpackString(prhs[1]);
    
    status = nc_open(netcdf_filename, mode, &ncid);
    plhs[0] = mexncCreateDoubleScalar ( ncid );
    plhs[1] = mexncCreateDoubleScalar ( status );

            
    return;

}









/***********************************************************************
 *
 * HANDLE_NC_REDEF:
 *
 * code for handling the nc_redef routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_redef function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'REDEF' or 'redef'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_redef 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

        /*
         * file and variable id
         * */
        int ncid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_redef ( ncid );
    plhs[0] = mexncCreateDoubleScalar ( status );


    return;

}










/***********************************************************************
 *
 * HANDLE_NC_RENAME_ATT:
 *
 * code for handling the nc_rename_att routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_rename_att function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'RENAME_ATT' or 'rename_att'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = parent variable ID of the referenced attribute
 *       prhs[3] = current name of referenced attribute
 *       prhs[4] = proposed new name of the referenced attribute
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_rename_att 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * used to rename attributes
     * */
    char    *old_att_name;
    char    *new_att_name;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
    check_char_argument_type ( prhs, nc_op->opname, 4 );
    



    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    old_att_name = unpackString(prhs[3]);
    new_att_name = unpackString(prhs[4]);
    status = nc_rename_att ( ncid, varid, old_att_name, new_att_name );
    plhs[0] = mexncCreateDoubleScalar ( status );


    return;

}













/***********************************************************************
 *
 * HANDLE_NC_RENAME_DIM:
 *
 * code for handling the nc_rename_dim routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_rename_dim function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'RENAME_DIM' or 'rename_dim'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = dimension ID of the referenced dimension
 *       prhs[3] = proposed new name
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_rename_dim 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    char    *new_dimension_name;





    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type ( prhs, nc_op->opname, 3 );
    

    

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    dimid = (int)(pr[0]);
    new_dimension_name = unpackString(prhs[3]);
    status = nc_rename_dim ( ncid, dimid, new_dimension_name );
    plhs[0] = mexncCreateDoubleScalar ( status );
    return;

}










/***********************************************************************
 *
 * HANDLE_NC_RENAME_VAR:
 *
 * code for handling the nc_rename_var routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_rename_var function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'RENAME_VAR' or 'rename_var'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = variable ID of the referenced variable
 *       prhs[3] = proposed new name
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_rename_var 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

        /*
         * file and variable IDs 
         * */
        int ncid;
        int varid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    char    *new_variable_name;





    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_char_argument_type    ( prhs, nc_op->opname, 3 );

    

    

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    new_variable_name = unpackString(prhs[3]);
    status = nc_rename_var(ncid, varid, new_variable_name);
    plhs[0] = mexncCreateDoubleScalar(status);


    return;







}






/***********************************************************************
 *
 * HANDLE_NC_SET_FILL:
 *
 * code for handling the nc_set_fill routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = the value of the previous fill mode
 *       plhs[1] = return status of the nc_set_fill function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'SET_FILL' or 'set_fill'
 *       prhs[1] = file ID (ncid) of the netcdf file
 *       prhs[2] = The new fill mode for the netcdf file.
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_set_fill 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

        /*
         * file and variable IDs 
         * */
        int ncid;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;



    int new_fill_mode;
    int old_fill_mode;




    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    

    

    pr = mxGetData(prhs[1]);
    ncid = (int)(pr[0]);
    pr = mxGetData(prhs[2]);
    new_fill_mode = (int) pr[0];
    status = nc_set_fill ( ncid, new_fill_mode, &old_fill_mode );
    plhs[0] = mexncCreateDoubleScalar(old_fill_mode);
    plhs[1] = mexncCreateDoubleScalar(status);
            
    return;







}







/***********************************************************************
 *
 * HANDLE_NC_STRERROR:
 *
 * code for handling the nc_strerror routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = character string that is the error message returned
 *                 by nc_strerror
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'STRERROR' or 'strerror'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_strerror 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;





    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );

    


    pr = mxGetData(prhs[1]);
    status = (int) pr[0];
    plhs[0] = mxCreateString ( nc_strerror( status ) );
    return;







}








/***********************************************************************
 *
 * HANDLE_NC_SYNC:
 *
 * code for handling the nc_sync routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return status of the nc_sync function call
 * nrhs:
 *     number of input arguments defined in matlab
 * prhs:
 *     Array of input matlab arrays.  
 *       prhs[0] = 'SYNC' or 'sync'
 *       prhs[1] = file ID (ncid) of the netcdf file
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc_sync 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /* 
     * netcdf file id
     * */
    int      ncid;

    /*
     * Return value 
     * */
    int status;





    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );


    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    status = nc_sync(ncid);
    plhs[0] = mexncCreateDoubleScalar(status);
            
            
    return;







}






















/***********************************************************************
 *
 * HANDLE_NC__OPEN:
 *
 * code for handling the nc__open routine.
 *
 * PARAMETERS:
 * nlhs:
 *     number of output arguments defined in matlab
 * plhs:
 *     Array of output matlab arrays.  
 *       plhs[0] = return value of the chunksize parameter.  Please 
 *                 see the C language NetCDF User's Guide for further
 *                 details.
 *       plhs[1] = file id (ncid) of the just-opened netcdf file
 *       plhs[2] = return status of the nc__create function call
 * nrhs:
 *     number of input arguments defined in matlab
 *       prhs[0] = '_OPEN' or '_open'
 *       prhs[1] = path to the netcdf file
 *       prhs[2] = open mode for the netcdf file
 *       prhs[3] = input value of the chunksize parameter.  Please 
 *                 see the C language NetCDF User's Guide for further
 *                 details.
 * prhs:
 *     Array of input matlab arrays.  
 * nc_op:
 *     Pointer to the current mexnc operation structure. 
 *
 **********************************************************************/
void handle_nc__open 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{


    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;

    /*
     * NetCDF file creation mode.
     * */
    int cmode;


    /*
     * See the man page for a description of chunksize.
     * */
    size_t chunksizehint;


    /*
     * path of NetCDF file
     * */
    char    *path;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_char_argument_type  ( prhs, nc_op->opname, 1 );
    check_mode_argument_type  ( prhs, nc_op->opname, 2 );
    check_numeric_argument_type  ( prhs, nc_op->opname, 3 );
    

    
    path = unpackString(prhs[1]);
    pr = mxGetData ( prhs[2] );
    cmode = (int)(pr[0]);
    pr = mxGetData ( prhs[3] );
    chunksizehint = (size_t)(pr[0]);
            
            
    status = nc__open ( path, cmode, &chunksizehint, &ncid );
    plhs[0] = mexncCreateDoubleScalar ( chunksizehint );
    plhs[1] = mexncCreateDoubleScalar ( ncid );
    plhs[2] = mexncCreateDoubleScalar ( status );
            
    return;

}








