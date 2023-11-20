/**********************************************************************
 *
 * netcdf2.c
 *
 * This file contains much of the code that went into the original 
 * mexcdf source file (mexcdf53.c).  
 *
 *********************************************************************/


/*
 * $Id: netcdf2.c 2471 2008-04-13 00:28:56Z johnevans007 $
 * */

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <math.h>
# include <limits.h>
# include <assert.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"
# include "netcdf2.h"


static int     Convert             (OPCODE, nc_type, int, VOIDP, DOUBLE, DOUBLE, DOUBLE *);
static int     Count               (const Matrix *mat);
static VOID    Free (VOIDPP);
mxArray       *Int2Mat             (int *, int, int);
static Matrix *Int2Scalar          (int i);
static Matrix *Long2Mat            (long *, int, int);
int           *Mat2Int             (const mxArray *);
static long   *Mat2Long	           (const Matrix *);
char	*Mat2Str ( const mxArray	*mat );
static int    Scalar2Int         (const Matrix *);
static long    Scalar2Long         (const Matrix *);
static Matrix *SetNum              (const Matrix *);
static Matrix *SetStr              (const Matrix *);
static Matrix *Str2Mat (char *);

static	Matrix  *Long2Scalar       (long);
static	DOUBLE   Scale_Factor      (int, int);
static	DOUBLE   Add_Offset        (int, int);

long m53_round(double x);


static parm parms[] =	{


	{ MAX_NC_NAME, "MAX_NC_NAME", 8 },
	{ MAX_NC_DIMS, "MAX_NC_DIMS", 8 },
	{ MAX_NC_VARS, "MAX_NC_VARS", 8 },
	{ MAX_NC_ATTRS, "MAX_NC_ATTRS", 8 },
	{ MAX_VAR_DIMS, "MAX_VAR_DIMS", 9 },
	{ NC_BYTE, "BYTE", 1 },
	{ NC_CHAR, "CHAR", 2 },
	{ NC_CLOBBER, "CLOBBER", 2 },
	{ NC_DOUBLE, "DOUBLE", 1 },
	{ NC_FATAL, "FATAL", 2 },
	{ NC_FILL, "FILL", 2 },
	{ NC_FLOAT, "FLOAT", 2 },
	{ NC_GLOBAL, "GLOBAL", 1 },
	{ NC_INT, "INT", 1 },
	{ NC_LONG, "LONG", 3 },
	{ NC_LOCK, "NC_LOCK", 3 },
	{ NC_NOCLOBBER, "NOCLOBBER", 3 },
	{ NC_NOFILL, "NOFILL", 3 },
	{ NC_NOWRITE, "NOWRITE", 3 },
	{ NC_SHARE, "SHARE", 3 },
	{ NC_SHORT, "SHORT", 3 },
	{ NC_UNLIMITED, "UNLIMITED", 1 },
	{ NC_VERBOSE, "VERBOSE", 1 },
	{ NC_WRITE, "WRITE", 1 },
	{ 0, "NONE", 0 }

};




/*	MexFunction(): Mex-file entry point.	*/

void
handle_netcdf2_api	(
	int			nlhs,
	Matrix	*	plhs[],
	int			nrhs,
	const Matrix	*	prhs[],
	op              *nc_op
	/*
	OPCODE          opcode
	*/
	)

{
	
	Matrix		*	mat;
	
	int				status;
	char		*	path;
	int				cmode;
	int				mode;
	int				cdfid;
	int				ndims;
	int				nvars;
	int				natts;
	int				recdim;
	char		*	name;
	long			length;
	int				dimid;
	nc_type			datatype;
	int			*	dim;
	int				varid;
	long		*	coords;
	VOIDP			value;
	long		*	start;
	long		*	count;
	int			*	intcount;
	long		*	stride;
	long		*	imap;
	long			recnum;
	int				nrecvars;
	int			*	recvarids;
	long		*	recsizes;
	VOIDPP			datap;		/*	pointers for record access.	*/
	int				len;
	int				incdf;
	int				invar;
	int				outcdf;
	int				outvar;
	int				attnum;
	char		*	attname;
	char		*	newname;
	int				fillmode;
	
	int				i;

	/*
	 * m and n are the number of rows and columns of a matrix.
	 * */
	int				m, n;


	char		*	p;
	char			buffer[MAX_BUFFER];
	char			error_message[MAX_BUFFER];
	
	DOUBLE		*	pr;
	DOUBLE			addoffset;
	DOUBLE			scalefactor;
	int		 autoscale;		/* do auto-scaling if this flag is non-zero. */

	char	error_buffer[1000];

	
	int		 nclen;			/* result of call to nctypelen */
						/* It's the number of bytes that the datatype takes up. */


	OPCODE		 opcode = nc_op->opcode;

	/*
	 * Type of input data
	 * */
	mxClassID    class_id;


	/*
	 * These are error message templates.
	 * */
	char		*ncid_error_fmt = "ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n"; 
	char		*dimid_error_fmt = "dimid argument must be matlab native double precision (<== that one, please) or character, operation \"%s\", line %d file \"%s\"\n"; 
	char		*varid_error_fmt = "varid argument must be matlab native double precision (<== that one, please) or character, operation \"%s\", line %d file \"%s\"\n"; 
	char		*attname_error_fmt = "attribute argument must be matlab native double precision or character, operation \"%s\", line %d file \"%s\"\n"; 




	/*	Extract the cdfid by number.	*/
	
	switch (opcode)	{
	
	case CREATE:
	case OPEN:
	case TYPELEN:
	case SETOPTS:
	case ERR:
	case PARAMETER:
	
		break;
	
	default:

		if ( mxIsDouble(prhs[1]) == false ) {
		        sprintf ( error_message, ncid_error_fmt, nc_op->opname, __LINE__, __FILE__ );
		        mexErrMsgTxt ( error_message );
		        return;
		}

	

		cdfid = Scalar2Int(prhs[1]);
	
		break;
	}


	
	/*	Extract the dimid by number or name.	*/
	
	switch (opcode)	{

	case DIMINQ:
	case DIMRENAME:
	
		if ( !((mxIsChar(prhs[2]) == true) || (mxIsDouble(prhs[2]) == true )) ) {
		        sprintf ( error_message, dimid_error_fmt, nc_op->opname, __LINE__, __FILE__ );
		        mexErrMsgTxt ( error_message );
		}
	
		if (mxIsDouble(prhs[2]))	{
			dimid = Scalar2Int(prhs[2]);
		}
		else	{
			name = Mat2Str(prhs[2]);
			dimid = ncdimid(cdfid, name);
			Free((VOIDPP) & name);
		}
		break;






	
	default:
	
		break;
	}
	
	/*	Extract the varid by number or name.	*/
	
	switch (opcode)	{

	case VARINQ:
	case VARPUT1:
	case VARGET1:
	case VARPUT:
	case VARGET:
	case VARPUTG:
	case VARGETG:
	case VARRENAME:
	case VARCOPY:
	case ATTPUT:
	case ATTINQ:
	case ATTGET:
	case ATTCOPY:
	case ATTNAME:
	case ATTRENAME:
	case ATTDEL:
	
		if ( !((mxIsChar(prhs[2]) == true) || (mxIsDouble(prhs[2]) == true )) ) {
		        sprintf ( error_message, varid_error_fmt, nc_op->opname, __LINE__, __FILE__ );
		        mexErrMsgTxt ( error_message );
		}
	
		name = Mat2Str(prhs[2]);
		varid = ncvarid(cdfid, name);
		Free((VOIDPP) & name);
		if (varid == -1)	{
			varid = Parameter(prhs[2]);
		}
	
	default:
	
		break;
	}
	




	/*	Extract the attname by name or number.	*/
	
	switch (opcode)	{
	
	case ATTPUT:
	case ATTINQ:
	case ATTGET:
	case ATTCOPY:
	case ATTRENAME:
	case ATTDEL:
	
		if ( !((mxIsChar(prhs[3]) == true) || (mxIsDouble(prhs[3]) == true )) ) {
		        sprintf ( error_message, attname_error_fmt, nc_op->opname, __LINE__, __FILE__ );
		        mexErrMsgTxt ( error_message );
		}
	
		if (mxIsNumeric(prhs[3]))	{
			attnum = Scalar2Int(prhs[3]);
			attname = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
			status = ncattname(cdfid, varid, attnum, attname);
		}
		else	{
			attname = Mat2Str(prhs[3]);
		}
		break;
	
	default:
	
		break;
	}





	
	/*	Extract the "add_offset" and "scale_factor" attributes.	*/
	
	switch (opcode)	{
	
	case VARPUT1:
	case VARGET1:
	case VARPUT:
	case VARGET:
	case VARPUTG:
	case VARGETG:

		addoffset = Add_Offset(cdfid, varid);
		scalefactor = Scale_Factor(cdfid, varid);
		if (scalefactor == 0.0)	{
			scalefactor = 1.0;
		}
		
		break;
	
	default:
	
		break;
	}
	
	/*	Perform the NetCDF operation.	*/
	
	switch (opcode)	{
		
	
	case CREATE:
		
		path = Mat2Str(prhs[1]);
		
		if (nrhs > 2)	{
			cmode = Parameter(prhs[2]);
		}
		else	{
			cmode = NC_NOCLOBBER;	/*	Default.	*/
		}
		
		cdfid = nccreate(path, cmode);
		
		plhs[0] = Int2Scalar(cdfid);
		plhs[1] = Int2Scalar((cdfid >= 0) ? 0 : -1);
		
		Free((VOIDPP) & path);
		
		break;
		
	case OPEN:
		
		path = Mat2Str(prhs[1]);
		
		if (nrhs > 2)	{
			mode = Parameter(prhs[2]);
		}
		else	{
			mode = NC_NOWRITE;	/*	Default.	*/
		}
		
		cdfid = ncopen(path, mode);
		
		plhs[0] = Int2Scalar(cdfid);
		plhs[1] = Int2Scalar((cdfid >= 0) ? 0 : -1);
		
		Free((VOIDPP) & path);
		
		break;
		
	case REDEF:
		
		status = ncredef(cdfid);
		
		plhs[0] = Int2Scalar(status);
		
		break;
		
	case ENDEF:
		
		status = ncendef(cdfid);
		
		plhs[0] = Int2Scalar(status);
		
		break;
		
	case CLOSE:
		
		status = ncclose(cdfid);
		
		plhs[0] = Int2Scalar(status);
		
		break;
		
	case INQUIRE:
	
		status = ncinquire(cdfid, & ndims, & nvars, & natts, & recdim);
		
		if ( status != -1 ) {
			status = 0;
		} 

		if (nlhs > 1)	{
			plhs[0] = Int2Scalar(ndims);
			plhs[1] = Int2Scalar(nvars);
			plhs[2] = Int2Scalar(natts);
			plhs[3] = Int2Scalar(recdim);
			plhs[4] = Int2Scalar(status);
		}
		else	{	/*	Default to 1 x 5 row vector.	*/
			plhs[0] = mxCreateDoubleMatrix(1, 5, REAL);
			pr = mxGetPr(plhs[0]);
			if (status == 0)	{
				pr[0] = (DOUBLE) ndims;
				pr[1] = (DOUBLE) nvars;
				pr[2] = (DOUBLE) natts;
				pr[3] = (DOUBLE) recdim;
				pr[4] = (DOUBLE) status;
			}
		}
		
		break;
		
	case SYNC:
	
		status = ncsync(cdfid);
		
		plhs[0] = Int2Scalar(status);
		
		break;
		
	case ABORT:
	
		status = ncabort(cdfid);
		
		plhs[0] = Int2Scalar(status);
		
		break;
		
	case DIMDEF:
	
		name = Mat2Str(prhs[2]);

		length = Parameter(prhs[3]);
		
		dimid = ncdimdef(cdfid, name, length);
		
		plhs[0] = Int2Scalar(dimid);
		plhs[1] = Int2Scalar((dimid >= 0) ? 0 : dimid);
		
		Free((VOIDPP) & name);
		
		break;
		
	case DIMID:
	
		name = Mat2Str(prhs[2]);
		
		dimid = ncdimid(cdfid, name);
		
		plhs[0] = Int2Scalar(dimid);
		plhs[1] = Int2Scalar((dimid >= 0) ? 0 : dimid);
		
		Free((VOIDPP) & name);
		
		break;
		
	case DIMINQ:
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		
		status = ncdiminq(cdfid, dimid, name, & length);
		
		plhs[0] = Str2Mat(name);
		plhs[1] = Long2Scalar(length);
		plhs[2] = Int2Scalar(status);
		
		Free((VOIDPP) & name);
		
		break;
		
	case DIMRENAME:
		
		name = Mat2Str(prhs[3]);
		
		status = ncdimrename(cdfid, dimid, name);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & name);
		
		break;
		
	case VARDEF:
	
		name = Mat2Str(prhs[2]);
		datatype = (nc_type) Parameter(prhs[3]);
		ndims = Scalar2Int(prhs[4]);
		if (ndims == -1)	{
			ndims = Count(prhs[5]);
		}


		/*
		 * Check against the case where [] was passed in as the list of 
		 * dimensions.  This is kind of bad form, I think, but it's been
		 * done too much in the past to expect people to stop doing it
		 * now.
		 * */
		m = mxGetM ( prhs[5] );
		n = mxGetN ( prhs[5] );
		if ( ndims == 0 ) {
			dim = NULL;
		} else if ( (m*n) == 0 ) {
			dim = NULL;
		} else {
			dim = Mat2Int(prhs[5]);
		}
		
		varid = ncvardef(cdfid, name, datatype, ndims, dim);
		
		Free((VOIDPP) & name);
		
		plhs[0] = Int2Scalar(varid);
		plhs[1] = Int2Scalar((varid >= 0) ? 0 : varid);
		
		break;
		
	case VARID:
	
		name = Mat2Str(prhs[2]);
		
		varid = ncvarid(cdfid, name);
		
		Free((VOIDPP) & name);
		
		plhs[0] = Int2Scalar(varid);
		plhs[1] = Int2Scalar((varid >= 0) ? 0 : varid);
		
		break;
		
	case VARINQ:
	
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_VAR_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status == -1 ) {
			plhs[0] = Str2Mat("");
			plhs[1] = Int2Scalar(-1);
			plhs[2] = Int2Scalar(-1);
			plhs[3] = Int2Scalar(-1);
			plhs[4] = Int2Scalar(-1);
			plhs[5] = Int2Scalar(status);
		} else {
		
			plhs[0] = Str2Mat(name);
			plhs[1] = Int2Scalar(datatype);
			plhs[2] = Int2Scalar(ndims);
			plhs[3] = Int2Mat(dim, 1, ndims);
			plhs[4] = Int2Scalar(natts);
			plhs[5] = Int2Scalar(status);
		
		}
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		break;
		
	case VARPUT1:
		
		class_id = mxGetClassID ( prhs[4] );
		switch ( class_id ) {
			case mxDOUBLE_CLASS:
			case mxCHAR_CLASS:
				break;
			default:
				mexErrMsgTxt ( "VARPUT1 required either double or char data" );
					
		}

		coords = Mat2Long(prhs[3]);
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_NC_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status < 0 ) {
			plhs[0] = Int2Scalar(status);
			return;
		}
		
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		if (datatype == NC_CHAR)	{
			mat = SetNum(prhs[4]);
		}
		else	{
			mat = (mxArray *) prhs[4];
		}
		if (mat == NULL)	{
			mat = (mxArray *) prhs[4];
		}
		
		pr = mxGetPr(mat);
		
		autoscale = (nrhs > 5 && Scalar2Int(prhs[5]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		status = Convert(opcode, datatype, 1, buffer, scalefactor, addoffset, pr);
		status = ncvarput1(cdfid, varid, coords, buffer);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & coords);
		
		break;
		
	case VARGET1:
		
		coords = Mat2Long(prhs[3]);
		
		autoscale = (nrhs > 4 && Scalar2Int(prhs[4]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_NC_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status < 0 ) {
			plhs[0] = Int2Scalar(-1);
			plhs[1] = Int2Scalar(status);
			return;
		}
		
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		mat = Int2Scalar(0);
		
		pr = mxGetPr(mat);
		
		status = ncvarget1(cdfid, varid, coords, buffer);
		status = Convert(opcode, datatype, 1, buffer, scalefactor, addoffset, pr);
		
		if (datatype == NC_CHAR)	{
			plhs[0] = SetStr(mat);
		}
		else	{
			plhs[0] = mat;
		}
		if (plhs[0] == NULL)	{
/*			prhs[0] = mat;		*/
			plhs[0] = mat;		/*	ZYDECO 24Jan2000	*/
		}
		
		plhs[1] = Int2Scalar(status);
		
		Free((VOIDPP) & coords);
		
		break;
		
	case VARPUT:
		
		class_id = mxGetClassID ( prhs[5] );
		switch ( class_id ) {
			case mxDOUBLE_CLASS:
			case mxCHAR_CLASS:
				break;
			default:
				mexErrMsgTxt ( "VARPUT required either double or char data" );
					
		}

		start = Mat2Long(prhs[3]);
		count = Mat2Long(prhs[4]);
		
		autoscale = (nrhs > 6 && Scalar2Int(prhs[6]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_NC_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status < 0 ) {
			plhs[0] = Int2Scalar(status);
			return;
		}
		
		
		if (datatype == NC_CHAR)	{
			mat = SetNum(prhs[5]);
		}
		else	{
			mat = (mxArray *) prhs[5];
		}
		if (mat == NULL)	{
			mat = (mxArray *) prhs[5];
		}
		
		pr = mxGetPr(mat);
		
		for (i = 0; i < ndims; i++)	{
			if (count[i] == -1)	{
				status = ncdiminq(cdfid, dim[i], name, & count[i]);
				count[i] -= start[i];
			}
		}
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		len = 0;
		if (ndims > 0)	{
			len = 1;
			for (i = 0; i < ndims; i++)	{
				len *= count[i];
			}
		}
		

		/*
		 * The return value of EVERY function call should be checked.
		 */
		nclen = nctypelen ( datatype );
		if ( nclen == -1 ) {
			plhs[0] = Int2Scalar(-1);
			break;
		}

		
		value = (VOIDP) mxCalloc(len, nclen);
		status = Convert(opcode, datatype, len, value, scalefactor, addoffset, pr);
		status = ncvarput(cdfid, varid, start, count, value);
		Free((VOIDPP) & value);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & start);
		Free((VOIDPP) & count);
		
		break;
		
	case VARGET:
		
		start = Mat2Long(prhs[3]);
		count = Mat2Long(prhs[4]);
		intcount = Mat2Int(prhs[4]);
		
		autoscale = (nrhs > 5 && Scalar2Int(prhs[5]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_NC_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status < 0 ) {
			plhs[0] = Int2Scalar(-1);
			plhs[1] = Int2Scalar(status);
			return;
		}
		
		
		for (i = 0; i < ndims; i++)	{
			if (count[i] == -1)	{
				status = ncdiminq(cdfid, dim[i], name, & count[i]);
				count[i] -= start[i];
			}
		}
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		m = 0;
		n = 0;
		if (ndims > 0)	{
			m = count[0];
			n = count[0];
			for (i = 1; i < ndims; i++)	{
				n *= count[i];
				if (count[i] > 1)	{
					m = count[i];
				}
			}
			n /= m;
		}
		len = m * n;
		if (ndims < 2)	{
			m = 1;
			n = len;
		}
		
		for (i = 0; i < ndims; i++)	{
			intcount[i] = count[ndims-i-1];   /*	Reverse order.	*/
		}
		
		if (MEXCDF_4 || ndims < 2)	{
			mat = mxCreateDoubleMatrix(m, n, mxREAL);	/*	mxCreateDoubleMatrix	*/
		}
# if MEXCDF_5
		else	{
			mat = mxCreateNumericArray(ndims, intcount, mxDOUBLE_CLASS, mxREAL);
		}
# endif
		
		pr = mxGetPr(mat);



		/*
		 * The return value of EVERY function call should be checked.
		 */
		nclen = nctypelen ( datatype );
		if ( nclen == -1 ) {
			plhs[0] = mexncCreateDoubleScalar ( -1 );
			break;
		}

		
		value = (VOIDP) mxCalloc(len, nclen);
		status = ncvarget(cdfid, varid, start, count, value);
		if ( status == -1 ) {
			sprintf ( error_buffer, "call to ncvarget failed.\n" );
			plhs[0] = mexncCreateDoubleScalar ( -1 );
			plhs[1] = mexncCreateDoubleScalar ( -1 );

			/*
			 * Not having these seem to make the solaris version core dump.
			 */
			Free((VOIDPP) & value);
			Free((VOIDPP) & intcount);
			Free((VOIDPP) & count);
			Free((VOIDPP) & start);
		
			mexErrMsgTxt ( error_buffer );
			break;
		}

		status = Convert(opcode, datatype, len, value, scalefactor, addoffset, pr);
		Free((VOIDPP) & value);
		
		if (datatype == NC_CHAR)	{
			plhs[0] = SetStr(mat);
		}
		else	{
			plhs[0] = mat;
		}
		if (plhs[0] == NULL)	{
			plhs[0] = mat;
		}
		
		plhs[1] = Int2Scalar(status);
		
		Free((VOIDPP) & intcount);
		Free((VOIDPP) & count);
		Free((VOIDPP) & start);
		
		break;
		
	case VARPUTG:
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_NC_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status < 0 ) {
			plhs[0] = Int2Scalar(status);
			return;
		}
			
		
		
		if (nrhs > 7)	{
			if (datatype == NC_CHAR)	{
				mat = SetStr(prhs[7]);
			}
			else	{
				mat = (mxArray *) prhs[7];
			}
			if (mat == NULL)	{
				mat = (mxArray *) prhs[7];
			}
		}
		else	{
			if (datatype == NC_CHAR)	{
				mat = SetStr(prhs[6]);
			}
			else	{
				mat = (mxArray *) prhs[6];
			}
			if (mat == NULL)	{
				mat = (mxArray *) prhs[6];
			}
		}

		class_id = mxGetClassID ( mat );
		switch ( class_id ) {
			case mxDOUBLE_CLASS:
			case mxCHAR_CLASS:
				break;
			default:
				mexErrMsgTxt ( "VARPUTG required either double or char data" );
					
		}

		pr = mxGetPr(mat);
		
		start = Mat2Long(prhs[3]);
		count = Mat2Long(prhs[4]);
		stride = Mat2Long(prhs[5]);
		imap = NULL;
		
		for (i = 0; i < ndims; i++)	{
			if (count[i] == -1)	{
				status = ncdiminq(cdfid, dim[i], name, & count[i]);
				count[i] -= start[i];
			}
		}
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		len = 0;
		if (ndims > 0)	{
			len = 1;
			for (i = 0; i < ndims; i++)	{
				len *= count[i];
			}
		}
		
		autoscale = (nrhs > 8 && Scalar2Int(prhs[8]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		

		/*
		 * The return value of EVERY function call should be checked.
		 */
		nclen = nctypelen ( datatype );
		if ( nclen == -1 ) {
			plhs[0] = mexncCreateDoubleScalar ( -1 );
			break;
		}

		value = (VOIDP) mxCalloc(len, nclen);
		status = Convert(opcode, datatype, len, value, scalefactor, addoffset, pr);
		status = ncvarputg(cdfid, varid, start, count, stride, imap, value);
		Free((VOIDPP) & value);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & stride);
		Free((VOIDPP) & count);
		Free((VOIDPP) & start);
		
		break;
		
	case VARGETG:
		
		start = Mat2Long(prhs[3]);
		count = Mat2Long(prhs[4]);
        intcount = Mat2Int(prhs[4]);
		stride = Mat2Long(prhs[5]);
		imap = NULL;
		
		autoscale = (nrhs > 7 && Scalar2Int(prhs[7]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		dim = (int *) mxCalloc(MAX_NC_DIMS, sizeof(int));
		
		status = ncvarinq(cdfid, varid, name, & datatype, & ndims, dim, & natts);
		if ( status < 0 ) {
			plhs[0] = Int2Scalar(-1);
			plhs[1] = Int2Scalar(status);
			return;
		}
		
		
		for (i = 0; i < ndims; i++)	{
			if (count[i] == -1)	{
				status = ncdiminq(cdfid, dim[i], name, & count[i]);
				count[i] -= start[i];
			}
		}
		
		Free((VOIDPP) & name);
		Free((VOIDPP) & dim);
		
		m = 0;
		n = 0;
		if (ndims > 0)	{
			m = count[0];
			n = count[0];
			for (i = 1; i < ndims; i++)	{
				n *= count[i];
				if (count[i] > 1)	{
					m = count[i];
				}
			}
			n /= m;
		}
		len = m * n;
		if (ndims < 2)	{
			m = 1;
			n = len;
		}
		
		for (i = 0; i < ndims; i++)	{
			intcount[i] = count[ndims-i-1];   /*	Reverse order.	*/
		}
		
		if (MEXCDF_4 || ndims < 2)	{
			mat = mxCreateDoubleMatrix(m, n, mxREAL);	/*	mxCreateDoubleMatrix	*/
		}
# if MEXCDF_5
		else	{
			mat = mxCreateNumericArray(ndims, intcount, mxDOUBLE_CLASS, mxREAL);
		}
# endif
		
		pr = mxGetPr(mat);
		
		/*
		 * The return value of EVERY function call should be checked.
		 */
		nclen = nctypelen ( datatype );
		if ( nclen == -1 ) {
			plhs[0] = mexncCreateDoubleScalar ( -1 );
			plhs[1] = mexncCreateDoubleScalar ( -1 );
			break;
		}

		value = (VOIDP) mxCalloc(len, nclen);
		status = ncvargetg(cdfid, varid, start, count, stride, imap, value);
		status = Convert(opcode, datatype, len, value, scalefactor, addoffset, pr);
		Free((VOIDPP) & value);
		
		if (datatype == NC_CHAR)	{
			plhs[0] = SetStr(mat);
		}
		else	{
			plhs[0] = mat;
		}
		if (plhs[0] == NULL)	{
/*			prhs[0] = mat;		*/
			plhs[0] = mat;		/*	ZYDECO 24Jan2000	*/
		}
		
		plhs[1] = Int2Scalar(status);
		
		Free((VOIDPP) & stride);
		Free((VOIDPP) & intcount);
		Free((VOIDPP) & count);
		Free((VOIDPP) & start);
		
		break;

	case VARRENAME:
		
		name = Mat2Str(prhs[3]);
		
		status = ncvarrename(cdfid, varid, name);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & name);
		
		break;
		
	case VARCOPY:
	
		incdf = cdfid;
		
		invar = varid;
		
		outcdf = Scalar2Int(prhs[3]);
	
		outvar = -1;
/*		outvar = ncvarcopy(incdf, invar, outcdf);	*/
		
		plhs[0] = Int2Scalar(outvar);
		plhs[1] = Int2Scalar((outvar >= 0) ? 0 : outvar);
		
		break;
		
	case ATTPUT:
		
		datatype = (nc_type) Parameter(prhs[4]);
		

		/*
		 * The return value of EVERY function call should be checked.
		 */
		nclen = nctypelen ( datatype );
		if ( nclen == -1 ) {
			plhs[0] = Int2Scalar(-1);
			break;
		}

		
        if ( mxIsEmpty ( prhs[6] ) ) {
            fprintf ( stdout, "last arg is empty\n" );
        }
		if (datatype == NC_CHAR)	{
			mat = SetNum(prhs[6]);
		}
		else	{
			mat = (mxArray *) prhs[6];
		}
		if (mat == NULL)	{
			mat = (mxArray *) prhs[6];
		}
		
		len = Scalar2Int(prhs[5]);
		if (len <= -1)	{
			len = Count(mat);
		}
		
		pr = mxGetPr(mat);
		value = (VOIDP) mxCalloc(len, nclen);
		status = Convert(opcode, datatype, len, value, (DOUBLE) 1.0, (DOUBLE) 0.0, pr);
		
		status = ncattput(cdfid, varid, attname, datatype, len, value);
		
		if (value != NULL)	{
			Free((VOIDPP) & value);
		}
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		
		break;
		
	case ATTINQ:
		
		status = ncattinq(cdfid, varid, attname, & datatype, & len);
		
		
		plhs[0] = Int2Scalar((int) datatype);
		plhs[1] = Int2Scalar(len);
		plhs[2] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		
		break;
		
	case ATTGET:
		
		status = ncattinq(cdfid, varid, attname, & datatype, & len);
		if ( status == -1 ) {
			plhs[0] = Int2Scalar(status);
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		
		/*
		 * The return value of EVERY function call should be checked.
		 */
		nclen = nctypelen ( datatype );
		if ( nclen == -1 ) {
			plhs[0] = Int2Scalar(-1);
			plhs[1] = Int2Scalar(-1);
			break;
		}

		
		value = (VOIDP) mxCalloc(len, nclen);
		status = ncattget(cdfid, varid, attname, value);
		if ( status == -1 ) {
			plhs[0] = Int2Scalar(status);
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		mat = mxCreateDoubleMatrix(1, len, mxREAL);
		
		pr = mxGetPr(mat);
		
		status = Convert(opcode, datatype, len, value, (DOUBLE) 1.0, (DOUBLE) 0.0, pr);
		
		if (value != NULL)	{
			Free((VOIDPP) & value);
		}
		
		if (datatype == NC_CHAR)	{
			plhs[0] = SetStr(mat);
		}
		else	{
			plhs[0] = mat;
		}
		if (plhs[0] == NULL)	{
/*			prhs[4] = mat;		*/
			plhs[0] = mat;		/*	ZYDECO 24Jan2000	*/
		}
		
		plhs[1] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		
		break;
		
	case ATTCOPY:
	
		incdf = cdfid;
		
		invar = varid;
		
		outcdf = Scalar2Int(prhs[4]);
	
		if (mxIsNumeric(prhs[5]))	{
			outvar = Scalar2Int(prhs[5]);
		}
		else	{
			name = Mat2Str(prhs[5]);
			outvar = ncvarid(cdfid, name);
			Free((VOIDPP) & name);
		}
	
		status = ncattcopy(incdf, invar, attname, outcdf, outvar);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		
		break;
		
	case ATTNAME:
		
		attnum = Scalar2Int(prhs[3]);
		attname = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
		
		status = ncattname(cdfid, varid, attnum, attname);
		
		plhs[0] = Str2Mat(attname);
		plhs[1] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		
		break;
		
	case ATTRENAME:
	
		newname = Mat2Str(prhs[4]);
		
		status = ncattrename(cdfid, varid, attname, newname);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		Free((VOIDPP) & newname);
		
		break;
		
	case ATTDEL:
		
		status = ncattdel(cdfid, varid, attname);
		
		plhs[0] = Int2Scalar(status);
		
		Free((VOIDPP) & attname);
		
		break;
		
	case RECPUT:
		
		recnum = Scalar2Long(prhs[2]);
		pr = mxGetPr(prhs[3]);
		
		autoscale = (nrhs > 4 && Scalar2Int(prhs[4]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		recvarids = (int *) mxCalloc(MAX_VAR_DIMS, sizeof(int));
		recsizes = (long *) mxCalloc(MAX_VAR_DIMS, sizeof(long));
		datap = (VOIDPP) mxCalloc(MAX_VAR_DIMS, sizeof(VOIDP));
		
		status = ncrecinq(cdfid, & nrecvars, recvarids, recsizes);
		
		if (status == -1)	{
			plhs[0] = Int2Scalar(status);
			break;
		}
		
		length = 0;
		n = 0;
		for (i = 0; i < nrecvars; i++)	{
			ncvarinq(cdfid, recvarids[i], NULL, & datatype, NULL, NULL, NULL);
		

			/*
			 * The return value of EVERY function call should be checked.
			 */
			nclen = nctypelen ( datatype );
			if ( nclen == -1 ) {
				plhs[0] = Int2Scalar(-1);
				break;
			}

		
			
			length += recsizes[i];
			n += (recsizes[i] / nclen);
		}
		
		if (Count(prhs[3]) < n)	{
			status = -1;
			plhs[0] = Int2Scalar(status);
			break;
		}
		
		if ((value = (VOIDP) mxCalloc((int) length, sizeof(char))) == NULL)	{
			status = -1;
			plhs[0] = Int2Scalar(status);
			break;
		}
		
		length = 0;
		p = value;
		for (i = 0; i < nrecvars; i++)	{
			datap[i] = p;
			p += recsizes[i];
		}
		
		p = (char *) value;
		pr = mxGetPr(prhs[3]);
		
		for (i = 0; i < nrecvars; i++)	{
			ncvarinq(cdfid, recvarids[i], NULL, & datatype, NULL, NULL, NULL);
		
		
			/*
			 * The return value of EVERY function call should be checked.
			 */
			nclen = nctypelen ( datatype );
			if ( nclen == -1 ) {
				plhs[0] = Int2Scalar(-1);
				break;
			}

		
			length = recsizes[i] / nclen;
			if (autoscale)	{
				addoffset = Add_Offset(cdfid, recvarids[i]);
				scalefactor = Scale_Factor(cdfid, recvarids[i]);
				if (scalefactor == 0.0)	{
					scalefactor = 1.0;
				}
			}
			Convert(opcode, datatype, length, (VOIDP) p,  scalefactor, addoffset, pr);
			pr += length;
			p += recsizes[i];
		}
		
		status = ncrecput(cdfid, recnum, datap);
		
		plhs[0] = Int2Scalar(status);
		
		Free ((VOIDPP) & value);
		Free ((VOIDPP) & datap);
		Free ((VOIDPP) & recsizes);
		Free ((VOIDPP) & recvarids);
		
		break;
		
	case RECGET:
		
		recnum = Scalar2Long(prhs[2]);
		
		autoscale = (nrhs > 3 && Scalar2Int(prhs[3]) != 0);
		
		if (!autoscale)	{
			scalefactor = 1.0;
			addoffset = 0.0;
		}
		
		recvarids = (int *) mxCalloc(MAX_VAR_DIMS, sizeof(int));
		recsizes = (long *) mxCalloc(MAX_VAR_DIMS, sizeof(long));
		datap = (VOIDPP) mxCalloc(MAX_VAR_DIMS, sizeof(VOIDP));
		
		status = ncrecinq(cdfid, & nrecvars, recvarids, recsizes);
		
		if (status == -1)	{
			Free ((VOIDPP) & recsizes);
			Free ((VOIDPP) & recvarids);
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		if (nrecvars == 0)	{
			Free ((VOIDPP) & recsizes);
			Free ((VOIDPP) & recvarids);
			plhs[0] = mxCreateDoubleMatrix(0, 0, REAL);
			break;
		}
		
		length = 0;
		n = 0;
		for (i = 0; i < nrecvars; i++)	{
			ncvarinq(cdfid, recvarids[i], NULL, & datatype, NULL, NULL, NULL);
		



			/*
			 * The return value of EVERY function call should be checked.
			 */
			nclen = nctypelen ( datatype );
			if ( nclen == -1 ) {
				plhs[0] = Int2Scalar(-1);
				plhs[1] = Int2Scalar(-1);
				break;
			}

		
			
			length += recsizes[i];
			n += (recsizes[i] / nclen);
		}
		
		if ((value = (VOIDP) mxCalloc((int) length, sizeof(char))) == NULL)	{
			status = -1;
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		if (value == NULL)	{
			status = -1;
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		length = 0;
		p = value;
		for (i = 0; i < nrecvars; i++)	{
			datap[i] = p;
			p += recsizes[i];
		}
		
		if ((status = ncrecget(cdfid, recnum, datap)) == -1)	{
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		m = 1;
		
		plhs[0] = mxCreateDoubleMatrix(m, n, REAL);
		
		if (plhs[0] == NULL)	{
			status = -1;
			plhs[1] = Int2Scalar(status);
			break;
		}
		
		pr = mxGetPr(plhs[0]);
		p = (char *) value;
		
		for (i = 0; i < nrecvars; i++)	{
			status = ncvarinq(cdfid, recvarids[i], NULL, & datatype, NULL, NULL, NULL);
		
			
			/*
			 * The return value of EVERY function call should be checked.
			 */
			nclen = nctypelen ( datatype );
			if ( nclen == -1 ) {
				plhs[0] = Int2Scalar(-1);
				plhs[1] = Int2Scalar(-1);
				break;
			}

		
			if (status == -1)	{
				plhs[1] = Int2Scalar(status);
				break;
			}
			length = recsizes[i] / nclen;
			if (autoscale)	{
				addoffset = Add_Offset(cdfid, recvarids[i]);
				scalefactor = Scale_Factor(cdfid, recvarids[i]);
				if (scalefactor == 0.0)	{
					scalefactor = 1.0;
				}
			}
			Convert(opcode, datatype, length, (VOIDP) p,  scalefactor, addoffset, pr);
			pr += length;
			p += recsizes[i];
		}
		
		plhs[1] = Int2Scalar(status);
		
		Free ((VOIDPP) & value);
		Free ((VOIDPP) & datap);
		Free ((VOIDPP) & recsizes);
		Free ((VOIDPP) & recvarids);
		
		break;

	case RECINQ:
		
		recvarids = (int *) mxCalloc(MAX_VAR_DIMS, sizeof(int));
		recsizes = (long *) mxCalloc(MAX_VAR_DIMS, sizeof(long));
		
		status = ncrecinq(cdfid, & nrecvars, recvarids, recsizes);
		
		if (status != -1)	{
			for (i = 0; i < nrecvars; i++)	{
				ncvarinq(cdfid, recvarids[i], NULL, & datatype, NULL, NULL, NULL);
		
			
				/*
				 * The return value of EVERY function call should be checked.
				 */
				nclen = nctypelen ( datatype );
				if ( nclen == -1 ) {
					plhs[0] = Int2Scalar(-1);
					plhs[1] = Int2Scalar(-1);
					plhs[2] = Int2Scalar(-1);
					break;
				}

		
				recsizes[i] /= nclen;
			}
			m = 1;
			n = nrecvars;
			plhs[0] = Int2Mat(recvarids, m, n);
			plhs[1] = Long2Mat(recsizes, m, n);
		}
		
		plhs[2] = Int2Scalar(status);
		
		Free ((VOIDPP) & recsizes);
		Free ((VOIDPP) & recvarids);
		
		break;
		
	case TYPELEN:
	
		datatype = (nc_type) Parameter(prhs[1]);
		
		len = nctypelen(datatype);
		
		plhs[0] = Int2Scalar(len);
		plhs[1] = Int2Scalar((len >= 0) ? 0 : 1);
		
		break;
		
	case SETFILL:
	
		fillmode = Scalar2Int(prhs[1]);
		
		status = ncsetfill(cdfid, fillmode);
		
		plhs[0] = Int2Scalar(status);
		plhs[1] = Int2Scalar(0);
		
		break;

	case SETOPTS:
		
		plhs[0] = Int2Scalar(ncopts);
		plhs[1] = Int2Scalar(0);
		ncopts = Scalar2Int(prhs[1]);
		
		break;
		
	case ERR:
	
		plhs[0] = Int2Scalar(ncerr);
		ncerr = 0;
		plhs[1] = Int2Scalar(0);
		
		break;
		
	case PARAMETER:
	
		if (nrhs > 1)	{
			plhs[0] = Int2Scalar(Parameter(prhs[1]));
			plhs[1] = Int2Scalar(0);
		}
		else	{
			i = 0;
			while (strcmp(parms[i].name, "NONE") != 0)	{
				mexPrintf("%12d %s\n", parms[i].code, parms[i].name);
				i++;
			}
			plhs[0] = Int2Scalar(0);
			plhs[1] = Int2Scalar(-1);
		}
		
		break;
		
	default:
	
		break;
	}
	
	return;
}


/*	Convert(): Convert between DOUBLE and NetCDF numeric types.	*/

static int
Convert	(
	OPCODE		opcode,
	nc_type		datatype,
	int			len,
	VOIDP		value,
	DOUBLE		scalefactor,
	DOUBLE		addoffset,
	DOUBLE	*	pr
	)

{
	signed char	*	pbyte;
	char		*	pchar;
	short		*	pshort;
	nclong		*	plong;	/*	Note use of nclong.	*/
	float		*	pfloat;
	double		*	pdouble;
	
	int				i;
	int				status;

	status = 0;
	
	switch (opcode)	{
	
	case VARPUT:
	case VARPUT1:
	
		switch	(datatype)	{
		case NC_BYTE:
			pbyte = (signed char *) value;
			for (i = 0; i < len; i++)	{
				*pbyte++ = (signed char) m53_round ( (*pr++ - addoffset) / scalefactor );
			}
			break;
		case NC_CHAR:
			pchar = (char *) value;
			for (i = 0; i < len; i++)	{
				*pchar++ = (char) m53_round ( (*pr++ - addoffset) / scalefactor );
			}
			break;
		case NC_SHORT:
			pshort = (short *) value;
			for (i = 0; i < len; i++)	{
				*pshort++ = (short) m53_round ( (*pr++ - addoffset) / scalefactor );
			}
			break;
		case NC_LONG:
			plong = (nclong *) value;
			for (i = 0; i < len; i++)	{
				*plong++ = (nclong) m53_round ( (*pr++ - addoffset) / scalefactor );
			}
			break;
		case NC_FLOAT:
			pfloat = (float *) value;
			for (i = 0; i < len; i++)	{
				*pfloat++ = (float) ((*pr++ - addoffset) / scalefactor);
			}
			break;
		case NC_DOUBLE:
			pdouble = (double *) value;
			for (i = 0; i < len; i++)	{
				*pdouble++ = (double) ((*pr++ - addoffset) / scalefactor);
			}
			break;
		default:
			status = -1;
			break;
		}
		break;




	/*
	 * But wait!!! ATTPUT is different because NaN could possibly be given as a value.
	 * Feeding NaN thru m53_round results in a segmentation fault.
	 * */
	case ATTPUT:
		switch	(datatype)	{
		case NC_BYTE:
			pbyte = (signed char *) value;
			for (i = 0; i < len; i++)	{
				*pbyte++ = (signed char) *pr++;
			}
			break;
		case NC_CHAR:
			pchar = (char *) value;
			for (i = 0; i < len; i++)	{
				*pchar++ = (char) *pr++;
			}
			break;
		case NC_SHORT:
			pshort = (short *) value;
			for (i = 0; i < len; i++)	{
				*pshort++ = (short) *pr++;
			}
			break;
		case NC_LONG:
			plong = (nclong *) value;
			for (i = 0; i < len; i++)	{
				*plong++ = (nclong) *pr++;
			}
			break;
		case NC_FLOAT:
			pfloat = (float *) value;
			for (i = 0; i < len; i++)	{
				*pfloat++ = (float) *pr++;
			}
			break;
		case NC_DOUBLE:
			pdouble = (double *) value;
			for (i = 0; i < len; i++)	{
				*pdouble++ = (double) *pr++;
			}
			break;
		default:
			status = -1;
			break;
		}
		break;



		
	case VARGET:
	case VARGET1:
	case ATTGET:
	
		switch	(datatype)	{
		case NC_BYTE:
			pbyte = (signed char *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pbyte++;
			}
			break;
		case NC_CHAR:
			pchar = (char *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pchar++;
			}
			break;
		case NC_SHORT:
			pshort = (short *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pshort++;
			}
			break;
		case NC_LONG:
			plong = (nclong *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *plong++;
			}
			break;
		case NC_FLOAT:
			pfloat = (float *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pfloat++;
			}
			break;
		case NC_DOUBLE:
			pdouble = (double *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pdouble++;
			}
			break;
		default:
			status = -1;
			break;
		}
		break;
	
	case VARPUTG:
	case RECPUT:
	
		switch	(datatype)	{
		case NC_BYTE:
			pbyte = (signed char *) value;
			for (i = 0; i < len; i++)	{
				*pbyte++ = (signed char) ((*pr++ - addoffset) / scalefactor + 0.5);
			}
			break;
		case NC_CHAR:
			pchar = (char *) value;
			for (i = 0; i < len; i++)	{
				*pchar++ = (char) ((*pr++ - addoffset) / scalefactor + 0.5);
			}
			break;
		case NC_SHORT:
			pshort = (short *) value;
			for (i = 0; i < len; i++)	{
				*pshort++ = (short) ((*pr++ - addoffset) / scalefactor + 0.5);
			}
			break;
		case NC_LONG:
			plong = (nclong *) value;
			for (i = 0; i < len; i++)	{
				*plong++ = (nclong) ((*pr++ - addoffset) / scalefactor + 0.5);
			}
			break;
		case NC_FLOAT:
			pfloat = (float *) value;
			for (i = 0; i < len; i++)	{
				*pfloat++ = (float) ((*pr++ - addoffset) / scalefactor);
			}
			break;
		case NC_DOUBLE:
			pdouble = (double *) value;
			for (i = 0; i < len; i++)	{
				*pdouble++ = (double) ((*pr++ - addoffset) / scalefactor);
			}
			break;
		default:
			status = -1;
			break;
		}
		break;
		
	case VARGETG:
	case RECGET:
	
		switch	(datatype)	{
		case NC_BYTE:
			pbyte = (signed char *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pbyte++;
			}
			break;
		case NC_CHAR:
			pchar = (char *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pchar++;
			}
			break;
		case NC_SHORT:
			pshort = (short *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pshort++;
			}
			break;
		case NC_LONG:
			plong = (nclong *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *plong++;
			}
			break;
		case NC_FLOAT:
			pfloat = (float *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pfloat++;
			}
			break;
		case NC_DOUBLE:
			pdouble = (double *) value;
			for (i = 0; i < len; i++)	{
				*pr++ = addoffset + scalefactor * (DOUBLE) *pdouble++;
			}
			break;
		default:
			status = -1;
			break;
		}
		break;
	
	default:
		status = -1;
		break;
	}
	
	return (status);
}




/*	Scale_Factor: Return "scale_factor" attribute as DOUBLE.	*/

static DOUBLE
Scale_Factor	(
	int	cdfid,
	int	varid
	)

{
	int			status;
	nc_type		datatype;
	int			len;
	char		value[32];
	DOUBLE		d;
	
	d = 1.0;
	
	if ((status = ncattinq(cdfid, varid, "scale_factor", &datatype, &len)) == -1)	{
	}
	else if ((status = ncattget(cdfid, varid, "scale_factor", value)) == -1)	{
	}
	else	{
		switch (datatype)	{
			case NC_BYTE:
				d = (DOUBLE) *((signed char *) value);
				break;
			case NC_CHAR:
				d = (DOUBLE) *((char *) value);
				break;
			case NC_SHORT:
				d = (DOUBLE) *((short *) value);
				break;
			case NC_LONG:
				d = (DOUBLE) *((nclong *) value);
				break;
			case NC_FLOAT:
				d = (DOUBLE) *((float *) value);
				break;
			case NC_DOUBLE:
				d = (DOUBLE) *((double *) value);
				break;
			default:
				break;
		}
	}
	
	return (d);
}


/*	Add_Offset: Return "add_offset" attribute as DOUBLE.	*/

static DOUBLE
Add_Offset	(
	int	cdfid,
	int	varid
	)

{
	int			status;
	nc_type		datatype;
	int			len;
	char		value[32];
	DOUBLE		d;
	
	d = 0.0;
	
	if ((status = ncattinq(cdfid, varid, "add_offset", &datatype, &len)) == -1)	{
	}
	else if ((status = ncattget(cdfid, varid, "add_offset", value)) == -1)	{
	}
	else	{
		switch (datatype)	{
			case NC_BYTE:
				d = (DOUBLE) *((signed char *) value);
				break;
			case NC_CHAR:
				d = (DOUBLE) *((char *) value);
				break;
			case NC_SHORT:
				d = (DOUBLE) *((short *) value);
				break;
			case NC_LONG:
				d = (DOUBLE) *((nclong *) value);
				break;
			case NC_FLOAT:
				d = (DOUBLE) *((float *) value);
				break;
			case NC_DOUBLE:
				d = (DOUBLE) *((double *) value);
				break;
			default:
				break;
		}
	}
	
	return (d);
}


/*	SetNum(): Convert matrix to numeric matrix.	*/

static Matrix *
SetNum	(
	const Matrix	*	mat
	)

{
	mxArray *m_array[1];
	Matrix	*	result = NULL;
	int			status;

	m_array[0] = (mxArray *)(mat);
	
	if (mxIsChar(mat))	{
		mexSetTrapFlag(1);
		status = mexCallMATLAB(1, & result, 1, m_array, "abs");
		if (status == 1)	{
			result = NULL;
		}
		mexSetTrapFlag(0);
	}
	
	return (result);
}


/*	SetStr(): Convert matrix to string matrix.	*/

static Matrix *
SetStr	(
	const Matrix	*	mat
	)

{
	Matrix	*	result = NULL;
	int			status;
	mxArray *m_array[1];
	
	m_array[0] = (mxArray *)(mat);
	if (mxIsNumeric(mat))	{
		mexSetTrapFlag(1);
		status = mexCallMATLAB(1, & result, 1, m_array, "setstr");
		if (status == 1)	{
			result = NULL;
		}
		mexSetTrapFlag(0);
	}
	
	return (result);
}



/*	Mat2Long(): Return matrix values as a long integer array.	*/

static long *
Mat2Long	(
	const Matrix	*	mat
	)

{
	DOUBLE	*	pr;
	long	*	plong;
	long	*	p;
	int			len;
	int			i;

	len = mxGetM(mat) * mxGetN(mat);
	
	plong = (long *) mxCalloc(len, sizeof(long));
	p = plong;
	pr = mxGetPr(mat);
	
	for (i = 0; i < len; i++)	{
		*p++ = (long) *pr++;
	}
	
	return (plong);
}


/*	Long2Mat(): Convert long integer array to a matrix.	*/

static Matrix *
Long2Mat	(
	long	*	plong,
	int			m,
	int			n
	)

{
	Matrix	*	mat;
	DOUBLE	*	pr;
	long	*	p;
	int			len;
	int			i;

	mat = mxCreateDoubleMatrix(m, n, REAL);
	
	pr = mxGetPr(mat);
	p = plong;
	
	len = m * n;
	for (i = 0; i < len; i++)	{
		*pr++ = (long) *p++;
	}
	
	return (mat);
}




/*	Int2Scalar(): Convert integer value to a scalar matrix.	*/

static Matrix *
Int2Scalar	(
	int		i
	)

{
	Matrix	*	scalar;
	
	scalar = mxCreateDoubleMatrix(1, 1, REAL);
	
	*(mxGetPr(scalar)) = (DOUBLE) i;
	
	return (scalar);
}


/*	Scalar2Int(): Return integer value of a scalar matrix.*/

static int Scalar2Int	(
	const Matrix	*	scalar
	)

{
	return ((int) *(mxGetPr(scalar)));
}


/*	Long2Scalar(): Convert long integer value to a scalar matrix.	*/

static Matrix *
Long2Scalar	(
	long		along
	)

{
	Matrix	*	scalar;
	
	scalar = mxCreateDoubleMatrix(1, 1, REAL);
	
	*(mxGetPr(scalar)) = (DOUBLE) along;
	
	return (scalar);
}


/*	Scalar2Long(): Return long integer value of a scalar matrix.	*/

static long
Scalar2Long	(
	const Matrix	*	scalar
	)

{
	return ((long) *(mxGetPr(scalar)));
}


/*	Count(): Element count of a matrix.	*/

static int
Count	(
	const Matrix	*	mat
	)

{
	return ((int) (mxGetM(mat) * mxGetN(mat)));
}


/*	Free(): De-allocate memory by address of pointer.	*/

static VOID
Free	(
	VOIDPP		p
	)

{
	if (*p)	{
		if (1)	{
			mxFree(*p);
			*p = (VOIDP) 0;
		}
	}
	else if (VERBOSE)	{
		mexPrintf(" ## MexCDF53/Free(): Attempt to free null-pointer.\n");
	}
}





/*	Str2Mat():	Convert string into a string-matrix.	*/

static Matrix * Str2Mat	( char	*	str)

{
	mxArray	*	mat;

	mat = mxCreateString(str);
	
	return (mat);
}






/*
 * Use this function to correctly round scaled integer data.  What a 
 * freakin' mess.  Scaling should never have been written into mexcdf 
 * in the first place.
 * */
long m53_round(double x) {
	assert(x >= LONG_MIN-0.5);
	assert(x <= LONG_MAX+0.5);
	if (x >= 0)
		return (long) (x+0.5);
	return (long) (x-0.5);
}









/*	Parameter(): Get NetCDF parameter by name.	*/

int Parameter	( const mxArray	*	mat)

{
	int			parameter;
	char	*	p;
	char	*	q;
	int			i;
	
	parameter = -1;
	
	if (mxIsNumeric(mat))	{
		parameter = Scalar2Int(mat);
	}
	else	{
		p = Mat2Str(mat);
		q = p;
		for (i = 0; i < strlen(p); i++)	{
			*q = (char) toupper((int) *q);
			q++;
		}
		if (strncmp(p, "NC_", 3) == 0)	{	/*	Trim away "NC_".	*/
			q = p + 3;
		}
		else	{
			q = p;
		}
		
		i = 0;
		while (strcmp(parms[i].name, "NONE") != 0)	{
			if (strncmp(q, parms[i].name, parms[i].len) == 0)	{
				parameter = parms[i].code;
				break;
			}
			else	{
				i++;
			}
		}
		
		mxFree ( p);
		/*
		Free ((VOIDPP) & p);
		*/
	}
	
	return (parameter);
}






/*	Mat2Int(): Return matrix values as an integer array.	*/

int * Mat2Int	( const mxArray	*	mat)

{
	double	*	pr;
	int		*	pint;
	int		*	p;
	int			len;
	int			i;

	len = mxGetM(mat) * mxGetN(mat);
	
	pint = (int *) mxCalloc(len, sizeof(int));
	p = pint;
	pr = mxGetPr(mat);
	
	for (i = 0; i < len; i++)	{
		*p++ = (int) *pr++;
	}
	
	return (pint);
}






/*	Mat2Str(): Return string from a string-matrix.	*/

char	*Mat2Str ( const mxArray	*mat )
{
	char	*	str;
	int			len;

	len = mxGetM(mat) * mxGetN(mat);
	
	str = (char *) mxCalloc(len + 1, sizeof(char));
	
	mxGetString(mat, str, len + 1);
	
	return (str);
}





/*	Int2Mat(): Convert integer array to a matrix.	*/

mxArray *Int2Mat ( int *pint, int m, int n) {

	mxArray	*	mat;
	double	*	pr;
	int	*	p;
	int			len;
	int			i;

	mat = mxCreateDoubleMatrix(m, n, mxREAL);
	
	pr = mxGetPr(mat);
	p = pint;
	
	len = m * n;
	for (i = 0; i < len; i++)	{
		*pr++ = (int) *p++;
	}
	
	return (mat);
}








