function backend = snc_write_backend(ncfile)
% SNC_WRITE_BACKEND:  figure out which backend to use, either mexnc or the
% native matlab netcdf/hdf4 package

v = version('-release');
fmt = snc_format(ncfile);
switch(fmt)
    case 'NetCDF'
        switch(v)
            case {'14','2006a','2006b','2007a','2007b','2008a'}
				try
                    mexnc('inq_libvers');
                catch
                    error('No write capability on MATLAB version R%s when mexnc mex-file is not present.', version('-release'));
                end
                backend = 'mexnc';

            otherwise
                backend = 'tmw';
        end

    case 'NetCDF-4'
        switch(v)
            case {'14','2006a','2006b','2007a','2007b','2008a','2008b','2009a','2009b','2010a'}
                lver = mexnc('inq_libvers');
                if lver(1) == '4'
                   backend = 'mexnc';
                else 
                   error('No write capability with a version earlier than R2010b.');
                end 

            otherwise
               backend = 'tmw';
        end

    case 'HDF4'
        switch(v)
            case {'14','2006a','2006b','2007a','2007b','2008a','2008b', ...
                '2009a','2009b','2010a','2010b','2011a'}
		        backend = 'tmw_hdf4';
            otherwise
                backend = 'tmw_hdf4_2011b';
        end
        return
        
    otherwise
        error('snctools:writeBackend:unknown', '%s is not a NetCDF file.', ncfile);

end

return




