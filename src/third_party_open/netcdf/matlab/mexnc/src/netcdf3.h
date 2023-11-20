/*
 * These prototypes are specific to handling NetCDF 3 stuff.
 * */
void   handle_nc_abort        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_close        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc__create      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_create       ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_def_dim      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_def_var      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_del_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc__enddef      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_enddef       ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_copy_att     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_var_x    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_varm_x   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq          ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_ndims    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_nvars    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_natts    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_attid    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_attlen   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_attname  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_atttype  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dim      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dimid    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dimlen   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dimname  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_unlimdim ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_var      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varid    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varname  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_vartype  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varndims ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_vardimid ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varnatts ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc__open        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_open         ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_redef        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_rename_att   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_rename_dim   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_rename_var   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_var_x    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_varm_x   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_set_fill     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_strerror     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_sync         ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );


void 	determine_varm_output_size ( 
		int        ndims, 
		int        num_requested_elements, 
		size_t    *nc_count_coord, 
		ptrdiff_t *nc_stride_coord, 
		ptrdiff_t *nc_imap_coord, 
		int       *result_size ) ;

