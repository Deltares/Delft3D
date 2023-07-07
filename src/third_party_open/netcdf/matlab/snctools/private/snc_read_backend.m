function [retrieval_method,fmt] = snc_read_backend(ncfile)
%SNC_READ_BACKEND   determine which netCDF library to use
%
% which backend do we employ?  Many, many possibilities to consider here.
%
%   [retrieval_method,fmt] = snc_read_backend(ncfile)
%
% returns selection for specified file, http or url.
%
%   [retrieval_method,fmt] = snc_read_backend()
%
% returns all available retrieval_method and fmt options
%
%See also: snctools, snc_format

import ucar.nc2.dods.*    
import ucar.nc2.*

if exist('NetcdfFile','class')
    have_java = true;
    snc_turnoff_log4j;
else
    have_java = false;
end
    
retrieval_methods.java         = 'java';
retrieval_methods.tmw_hdf4     = 'tmw_hdf4';
retrieval_methods.tmw_hdf4_2011a = 'tmw_hdf4_2011a';
retrieval_methods.mexnc        = 'mexnc';
retrieval_methods.tmw          = 'tmw';

fmts = snc_format();

if nargin==0
   retrieval_method = retrieval_methods;
else   
   retrieval_method = '';
end

% Check for this early.
if isa(ncfile,'ucar.nc2.NetcdfFile') 
    retrieval_method = retrieval_methods.java;
    fmt = fmts.netcdf_java;
    return
end

mv = version('-release');

fmt = snc_format(ncfile);

% For HDF4, there is no alternative.
if strcmp(fmt,fmts.HDF4) 
    switch(mv)
        case {'14','2006a','2006b','2007a','2007b','2008a','2008b', ...
                '2009a','2009b','2010a','2010b'}
            retrieval_method = retrieval_methods.tmw_hdf4;
        otherwise
            retrieval_method = retrieval_methods.tmw_hdf4_2011a;
    end

    fmt = fmts.HDF4;
    return
end

DEBUG = 0;
if DEBUG
    % Set DEBUG to 1 to force use of netcdf-java at all times.
    % You shouldn't have to do this unless you are trying to
    % find a bug somewhere.
    retrieval_method = retrieval_methods.java; 
    fmt = fmts.netcdf_java;
    return
end

if strcmp(fmt,fmts.URL) 

    % URLs must be handled by netcdf-java until R2012a.
    switch(mv)
        case {'14','2006a','2006b','2007a','2007b','2008a','2008b', ...
                '2009a','2009b','2010a','2010b', '2011a', '2011b'} 
            if ~have_java
                error('snctools:noNetcdfJava', ...
                    'netcdf-java must be available in order to read OPeNDAP URLs.');
            end
            retrieval_method = retrieval_methods.java; 
            fmt = fmts.netcdf_java;

        otherwise
            % 12a and above has native matlab support for opendap.
            if strcmpi(ncfile(1:5),'https')
                % Still, use netcdf-java for SSL.
                retrieval_method = retrieval_methods.java; 
                fmt = fmts.netcdf_java;
            elseif getpref('SNCTOOLS','USE_NETCDF_JAVA',false)
                % Force the use of netcdf-java if the user
                % really want it.
                retrieval_method = retrieval_methods.java; 
                fmt = fmts.netcdf_java;
            else
                retrieval_method = retrieval_methods.tmw;
                fmt = fmts.NetCDF;
            end
    end

elseif ( strcmp(fmt,fmts.GRIB) || strcmp(fmt,fmts.GRIB2) )

    % Always use netcdf-java for grib files when java is enabled).
    if ~have_java
        error('snctools:noNetcdfJava', ...
            'netcdf-java must be available in order to read this.');
    end
    retrieval_method = retrieval_methods.java;
    fmt = fmts.netcdf_java;
    return
end

switch ( mv )
    case { '11', '12', '13' };
        error('Not supported on releases below R14.');

    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
        % No native matlab support here.  Use mexnc if available, 
        % otherwise try java.
        switch(fmt)
            case fmts.NetCDF
                try
                    mexnc('inq_libvers');
                    retrieval_method = retrieval_methods.mexnc;
                catch %#ok<CTCH>
                    if ~exist('NetcdfFile','class')
                        error('snctools:noMexncNoNetcdfJava', ...
                              'Either netcdf-java or the mexnc mex-file must be available in order to read this.');
                    end
                    retrieval_method = retrieval_methods.java;
                end
                fmt = fmts.NetCDF;
    
            case fmts.NetCDF4
                if ~have_java
                    error('snctools:noNetcdfJava', ...
                        'netcdf-java must be available in order to read this.');
                end
                retrieval_method = retrieval_methods.java;
                % Last chance is if it is some format that netcdf-java can handle.
                % Hope for the best.
                fmt = fmts.NetCDF4;
        end
        
    case { '2008b', '2009a', '2009b', '2010a' }
        % 2008b introduced native netcdf-3 support.
        % netcdf-4 still requires either mexnc or java, and we will favor
        % java again.
        switch(fmt)
            case fmts.NetCDF
                % Use TMW for all local netcdf-3 files.
                retrieval_method = retrieval_methods.tmw;
                fmt = fmts.NetCDF;
    
            case fmts.NetCDF4
                if ~exist('NetcdfFile','class') 
                    error('snctools:noNetcdfJava', ...
                        'netcdf-java must be available in order to read this.');
                end
                retrieval_method = retrieval_methods.java;
                fmt = fmts.NetCDF4;

            otherwise
                if ~have_java
                    error('snctools:noNetcdfJava', ...
                        'File format is unknown and netcdf-java is not available.  Not sure how to read this file.');
                end
                % not netcdf-3 or netcdf-4
                % Last chance is if it is some format that netcdf-java can handle.
                retrieval_method = retrieval_methods.java;
                fmt = fmts.netcdf_java;
        end

    case '2010b'
        switch(fmt)
            case {fmts.NetCDF, fmts.NetCDF4}
                % Introduced netcdf-4 support in tmw.
                retrieval_method = retrieval_methods.tmw;
                fmt = fmts.NetCDF;

            otherwise
                if ~have_java
                    error('snctools:noNetcdfJava', ...
                        'File format is unknown and netcdf-java is not available.  Not sure how to read this file.');
                end
                % Last chance is if it is some format that netcdf-java can
                % handle.
                fmt = fmts.netcdf_java;
        end

    otherwise
        % Better HDF5 support in tmw starting in 2011a.
        switch(fmt)

            case {fmts.NetCDF, fmts.NetCDF4}
                retrieval_method = retrieval_methods.tmw;
                fmt = fmts.NetCDF;

            otherwise
                % Last chance is if it is some format that netcdf-java can handle.
                if ~have_java
                    error('snctools:noNetcdfJava', ...
                        'File format is unknown and netcdf-java is not available.  Not sure how to read this file.');
                end                
                fmt = fmts.netcdf_java;
        end

end

if isempty(retrieval_method)
    error('snctools:unknownWriteBackendSituation', ...  
          'Could not determine which backend to use with %s.', ...
       ncfile );
end
return
