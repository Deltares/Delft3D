/*
 * These prototypes are specific to handling NetCDF 4 stuff.
 * */
void   handle_nc_def_var_chunking ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_var_chunking ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_def_var_deflate ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_var_deflate ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );


