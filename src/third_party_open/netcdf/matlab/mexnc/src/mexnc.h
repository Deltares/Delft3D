/*
 * $Id: mexnc.h 2828 2010-01-15 14:48:55Z johnevans007 $
 * */

# if !defined	MEXNC_H
# define		MEXNC_H



/*	
 *	NetCDF Operations.	
 *
 *	*/

typedef enum s_opcode	{

	ABORT = 0,
	CLOSE,
	COPY_ATT,
	_CREATE,
	CREATE,
	DEF_DIM,
	DEF_VAR,
	DEF_VAR_CHUNKING,
	DEF_VAR_DEFLATE,
	DEL_ATT,
	_ENDDEF,
	END_DEF,
	ENDDEF,
	GET_ATT_DOUBLE,
	GET_ATT_FLOAT,
	GET_ATT_INT,
	GET_ATT_SHORT,
	GET_ATT_SCHAR,
	GET_ATT_UCHAR,
	GET_ATT_TEXT,
	GET_VAR_DOUBLE,
	GET_VAR_FLOAT,
	GET_VAR_INT,
	GET_VAR_SHORT,
	GET_VAR_SCHAR,
	GET_VAR_UCHAR,
	GET_VAR_TEXT,
	GET_VAR1_DOUBLE,
	GET_VAR1_FLOAT,
	GET_VAR1_INT,
	GET_VAR1_SHORT,
	GET_VAR1_SCHAR,
	GET_VAR1_UCHAR,
	GET_VAR1_TEXT,
	GET_VARA_DOUBLE,
	GET_VARA_FLOAT,
	GET_VARA_INT,
	GET_VARA_SHORT,
	GET_VARA_SCHAR,
	GET_VARA_UCHAR,
	GET_VARA_TEXT,
	GET_VARS_DOUBLE,
	GET_VARS_FLOAT,
	GET_VARS_INT,
	GET_VARS_SHORT,
	GET_VARS_SCHAR,
	GET_VARS_UCHAR,
	GET_VARS_TEXT,
	GET_VARM_DOUBLE,
	GET_VARM_FLOAT,
	GET_VARM_INT,
	GET_VARM_SHORT,
	GET_VARM_SCHAR,
	GET_VARM_UCHAR,
	GET_VARM_TEXT,
	INQ,
	INQ_ATT,
	INQ_ATTID,
	INQ_ATTLEN,
	INQ_ATTNAME,
	INQ_ATTTYPE,
	INQ_DIM,
	INQ_DIMID,
	INQ_DIMLEN,
	INQ_DIMNAME,
	INQ_LIBVERS,
	INQ_NDIMS,
	INQ_NVARS,
	INQ_NATTS,
	INQ_UNLIMDIM,
	INQ_VARID,
	INQ_VAR,
	INQ_VAR_CHUNKING,
	INQ_VAR_DEFLATE,
	INQ_VARNAME,
	INQ_VARTYPE,
	INQ_VARNDIMS,
	INQ_VARDIMID,
	INQ_VARNATTS,
	_OPEN,
	OPEN,
	PUT_ATT_DOUBLE,
	PUT_ATT_FLOAT,
	PUT_ATT_INT,
	PUT_ATT_SHORT,
	PUT_ATT_SCHAR,
	PUT_ATT_UCHAR,
	PUT_ATT_TEXT,
	PUT_VAR_DOUBLE,
	PUT_VAR_FLOAT,
	PUT_VAR_INT,
	PUT_VAR_SHORT,
	PUT_VAR_SCHAR,
	PUT_VAR_UCHAR,
	PUT_VAR_TEXT,
	PUT_VARA_DOUBLE,
	PUT_VARA_FLOAT,
	PUT_VARA_INT,
	PUT_VARA_SHORT,
	PUT_VARA_SCHAR,
	PUT_VARA_UCHAR,
	PUT_VARA_TEXT,
	PUT_VARS_DOUBLE,
	PUT_VARS_FLOAT,
	PUT_VARS_INT,
	PUT_VARS_SHORT,
	PUT_VARS_SCHAR,
	PUT_VARS_UCHAR,
	PUT_VARS_TEXT,
	PUT_VARM_DOUBLE,
	PUT_VARM_FLOAT,
	PUT_VARM_INT,
	PUT_VARM_SHORT,
	PUT_VARM_SCHAR,
	PUT_VARM_UCHAR,
	PUT_VARM_TEXT,
	PUT_VAR1_DOUBLE,
	PUT_VAR1_FLOAT,
	PUT_VAR1_INT,
	PUT_VAR1_SHORT,
	PUT_VAR1_SCHAR,
	PUT_VAR1_UCHAR,
	PUT_VAR1_TEXT,
	REDEF,
	RENAME_ATT,
	RENAME_DIM,
	RENAME_VAR,
	SET_FILL,
	STRERROR,
	SYNC,

	/*
	 * Deprecated NetCDF 2.4
	 * */
	ATTDEL,
	DIMDEF,
	DIMID,
	DIMINQ,
	DIMRENAME,
	ENDEF,
	INQUIRE,
	VARDEF,
	VARID,
	VARINQ,
	VARPUT1,
	VARGET1,
	VARPUT,
	VARGET,
	VARPUTG,
	VARGETG,
	VARRENAME,
	VARCOPY,
	ATTCOPY,
	ATTPUT,
	ATTINQ,
	ATTGET,
	ATTNAME,
	ATTRENAME,
	RECPUT,
	RECGET,
	RECINQ,
	TYPELEN,
	SETFILL,
	SETOPTS,
	ERR,
	PARAMETER,
	NONE,

	/*
	 * Non-mexnc calls
	 * */
	GET_MEXNC_INFO

	}	OPCODE;

typedef struct s_op	{
	OPCODE   opcode;
	char    *opname;
	int      nrhs;		/*	Required nrhs.	*/
	int      nlhs;		/*	Maximum nlhs.	*/
} op;

/*	NetCDF Parameters.	*/

typedef struct s_parm	{
	int      code;
	char    *name;
	int      len;		/*	Minimal unique length.	*/
} parm;
	




void          handle_netcdf2_api ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op );	
void          handle_netcdf3_api ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op );	
size_t       *Mat2Size_t ( const mxArray *mat );
ptrdiff_t    *Mat2Ptrdiff_t ( const mxArray *mat );
char         *Mat2Str (const mxArray *);

void          check_char_argument_type    ( const mxArray *mx[], char *opname, int idx ); 
void          check_mode_argument_type    ( const mxArray *mx[], char *opname, int idx ); 
void          check_mode_argument_type    ( const mxArray *mx[], char *opname, int idx ); 
void          check_numeric_argument_type ( const mxArray *mx[], char *opname, int idx );
void          check_char_or_numeric_argument_type ( const mxArray *mx[], char *opname, int idx );

void          check_other_args_for_empty_set ( op *nc_op, const mxArray *prhs[], int nrhs );

int           interpret_char_parameter ( const mxArray *mx );
mxArray      *mexncCreateDoubleScalar ( double value );
int           Parameter (const mxArray *);
void          Usage (void);

void set_output_matrix_rank 
( 
    int     ncid, 
    int     num_nc_dims,
    int    *dimids, 
    size_t *nc_count_coords, 
    op     *nc_op, 
    int    *mx_rank 
); 

nc_type       unpackDataType ( const mxArray *mx );
ptrdiff_t    *unpackPtrdiff_t ( const mxArray *mx );
size_t       *unpackSize_t ( const mxArray *mx );
char         *unpackString ( const mxArray *mx );
int           unpack_char_file_mode ( char *input_string_mode );

void          varm_coord_sanity_check ( const mxArray *prhs[], int ndims );

/*
 * Need this to get a successful compile on win32
#ifdef DLL_NETCDF
#define snprintf sprintf_s
#endif
 * */

/*
 * Need to redefine mxCreateDoubleScalar to get this to compile
 * on R12
 * */
#ifdef MEXNCR12
mxArray *mxCreateDoubleScalar ( double value );
#endif


# endif
