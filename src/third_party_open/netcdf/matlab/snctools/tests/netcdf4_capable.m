function tf = netcdf4_capable()
% Is the current mexnc installation capable of netcdf-4 operations?
v = version('-release');
switch(v)
    case { '14', '2006a', '2006b', '2007a', '2007b', '2008a', '2008b', ...
            '2009a', '2009b', '2010a' }
        import ucar.nc2.dods.*    
        import ucar.nc2.*
        if exist('NetcdfFile','class')
            tf = true;
        else
            tf = false;
        end

        
    otherwise
		% 2010b is definitely netcdf4-capable.
        tf = true;

end

return;



