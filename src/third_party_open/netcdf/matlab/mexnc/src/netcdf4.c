/**********************************************************************
 *
 * netcdf4.c
 *
 * This file contains code to handle the mexfile interface to NetCDF-4
 * API calls.
 *
 *********************************************************************/

/*
 * $Id: netcdf3.c 2469 2008-03-25 13:24:05Z johnevans007 $
 * */

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"
# include "netcdf4.h"


/*
 * Let each function use this over and over again.
 * */
static char    error_message[1000];



/***********************************************************************
 *
 * HANDLE_NC_DEF_VAR_CHUNKING:
 *
 * code for handling the nc_def_var_chunking routine.
 *
 * status = mexnc('def_var_chunking',ncid,varid,storage,chunksize);
 *
 **********************************************************************/
void handle_nc_def_var_chunking 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{

    int ncid;
    int varid;
    int storage;
	size_t chunksize[NC_MAX_DIMS];
	

    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;


    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * Loop index
     * */
    int j;

	/*
	 * Number of dimensions (inferred from length of chunk argument)
	 * */
	int ndims;


	/*
	 * number of elements in chunking parameter.
	 * */
	mwSize nelts;


    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );

    if ( !((mxIsChar(prhs[3]) == false) || (mxIsDouble(prhs[3]) == false )) ) {
        sprintf ( error_message, 
                "datatype argument must be matlab native double precision (<== that one, please) or character, operation \"%s\", line %d file \"%s\"\n", 
                nc_op->opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }

    check_numeric_argument_type ( prhs, nc_op->opname, 4 );
    
    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

    if ( mxIsChar ( prhs[3] ) ) {

        /*
         * This one is for backwards compatibility.  I really 
         * wish people would not do this...
         * */
        storage = interpret_char_parameter  ( prhs[3] );

    } else {
        pr = mxGetData ( prhs[3] );
        storage = (int) (pr[0]);
    }


	status = nc_inq_varndims(ncid,varid,&ndims);
	if ( status != NC_NOERR ) {
        sprintf ( error_message, 
                 "Internal call to nc_inq_varndims failed, operation \"%s\", line %d file \"%s\"\n", 
                  nc_op->opname, __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
        return;
	}

	nelts = mxGetNumberOfElements(prhs[4]);


    /*
     * Make sure the user didn't do something really stupid like give too many dimensions.
     * */
    if ( nelts > NC_MAX_VAR_DIMS ) {
            sprintf ( error_message, 
                "given number of chunk elements (%d) exceeds preset maximum of %d, operation \"%s\", line %d file \"%s\"\n", 
                nelts, NC_MAX_VAR_DIMS, nc_op->opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }


	/*
	 * Tell the user not to provide a chunksize argument for contiguous storage.
	 * */
	
	if ( ( storage == NC_CONTIGUOUS ) && (nelts > 0) ) {
        sprintf ( error_message, 
                "If the storage type is NC_CONTIGUOUS, then the chunksize parameter must be [], line %d file \"%s\"",
                __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
        return;
	}


    pr = mxGetData ( prhs[4] );
	for ( j = 0; j < nelts; ++j ) {
		chunksize[j] = pr[j];
	}

    status = nc_def_var_chunking ( ncid, varid, storage, chunksize );
    plhs[0] = mexncCreateDoubleScalar ( status );


    return;

}







/***********************************************************************
 *
 * HANDLE_NC_DEF_VAR_DEFLATE:
 *
 * code for handling the nc_def_var_chunking routine.
 *
 * status = mexnc('def_var_deflate',ncid,varid,shuffle,deflate,deflate_level);
 *
 **********************************************************************/
void handle_nc_def_var_deflate 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{

    int ncid;
    int varid;
    int shuffle = 0;
    int deflate = 0;
    int deflate_level;
	

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
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );
    check_numeric_argument_type ( prhs, nc_op->opname, 3 );
    check_numeric_argument_type ( prhs, nc_op->opname, 4 );
    check_numeric_argument_type ( prhs, nc_op->opname, 5 );

    
    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);
    pr = mxGetData ( prhs[3] );
    shuffle = (int)(pr[0]);
    pr = mxGetData ( prhs[4] );
    deflate = (int)(pr[0]);
    pr = mxGetData ( prhs[5] );
    deflate_level = (int)(pr[0]);

    status = nc_def_var_deflate(ncid,varid,shuffle,deflate,deflate_level);
    plhs[0] = mexncCreateDoubleScalar ( status );


    return;

}







/***********************************************************************
 *
 * HANDLE_NC_INQ_VAR_CHUNKING:
 *
 * code for handling the nc_inq_var_chunking routine.
 *
 * [storage,chunksize,status] = mexnc('inq_var_chunking',ncid,varid);
 *
 **********************************************************************/
void handle_nc_inq_var_chunking 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{

    int ncid;
    int varid;
    int storage;
	size_t chunksize[NC_MAX_DIMS];
	

    /*
     * Pointer shortcut to matrix data.
     * */
    double *pr;

	/*
	 * Size of chunking matrix.
	 * */
	mwSize mxsize[2];

    /* 
     * Return status from netcdf operation.  
     * */
    int      status;


    /*
     * Loop index
     * */
    int j;

	/*
	 * Number of dimensions (inferred from length of chunk argument)
	 * */
	int ndims;

	/*
	 * File format.  
	 * */
	int format;

    /*
     * Make sure that the inputs are the right type.
     * */
    check_numeric_argument_type ( prhs, nc_op->opname, 1 );
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

	status = nc_inq_format(ncid,&format);
	if ( status != NC_NOERR ) {
        sprintf ( error_message, 
                 "Internal call to nc_inq_format failed, operation \"%s\", line %d file \"%s\"\n", 
                  nc_op->opname, __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
        return;
	}

	switch(format){
		case NC_FORMAT_CLASSIC:
		case NC_FORMAT_64BIT:
			plhs[0] = mxCreateString ( "contiguous" );
			plhs[1] = mxCreateNumericArray(0,0, mxDOUBLE_CLASS, mxREAL );
			plhs[2] = mxCreateNumericArray(0,0, mxDOUBLE_CLASS, mxREAL );
			return;
	}



	status = nc_inq_varndims(ncid,varid,&ndims);
	if ( status != NC_NOERR ) {
        sprintf ( error_message, 
                 "Internal call to nc_inq_varndims failed, operation \"%s\", line %d file \"%s\"\n", 
                  nc_op->opname, __LINE__, __FILE__ );
        mexErrMsgTxt ( error_message );
        return;
	}

    status = nc_inq_var_chunking ( ncid, varid, &storage, chunksize );
    plhs[2] = mexncCreateDoubleScalar ( status );

	if ( storage == NC_CONTIGUOUS ) {
		plhs[0] = mxCreateString ( "contiguous" );

		/*
		 * Return [] for the chunksize
		 * */
		mxsize[0] = 0;
		mxsize[1] = 0;
		plhs[1] = mxCreateNumericArray ( 2, mxsize, mxDOUBLE_CLASS, mxREAL );

		return;
	}

	plhs[0] = mxCreateString ( "chunked" );

	mxsize[0] = 1;
	mxsize[1] = ndims;
	plhs[1] = mxCreateNumericArray ( 2, mxsize, mxDOUBLE_CLASS, mxREAL );
	pr = mxGetData ( plhs[1] );
	for ( j = 0; j < ndims; ++j ) {
		pr[j] = chunksize[j];
	}



    return;

}

/***********************************************************************
 *
 * HANDLE_NC_INQ_VAR_DEFLATE:
 *
 * code for handling the nc_inq_var_deflate routine.
 *
 * [shuffle,deflate,deflate_level,status] = mexnc('inq_var_deflate',ncid,varid);
 *
 **********************************************************************/
void handle_nc_inq_var_deflate 
( 
    int            nlhs, 
    mxArray       *plhs[], 
    int            nrhs, 
    const mxArray *prhs[], 
    op            *nc_op 
) 
{

    int ncid;
    int varid;
    int shuffle;
    int deflate;
    int deflate_level;
    int format;
	

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
    check_numeric_argument_type ( prhs, nc_op->opname, 2 );

    pr = mxGetData ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetData ( prhs[2] );
    varid = (int)(pr[0]);

	switch(format){
		case NC_FORMAT_CLASSIC:
		case NC_FORMAT_64BIT:
    		plhs[0] = mxCreateDoubleScalar(0);
		    plhs[1] = mxCreateDoubleScalar(0);
		    plhs[2] = mxCreateDoubleScalar(0);
    		plhs[3] = mxCreateDoubleScalar(0);
			return;
	}


    status = nc_inq_var_deflate(ncid,varid,&shuffle,&deflate,&deflate_level);
    plhs[0] = mxCreateDoubleScalar(shuffle);
    plhs[1] = mxCreateDoubleScalar(deflate);
    plhs[2] = mxCreateDoubleScalar(deflate_level);
    plhs[3] = mxCreateDoubleScalar(status);



    return;

}


