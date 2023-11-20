function nc_addhist(ncfile,attval)
%NC_ADDHIST  Add text to global history attribute.
%
%   NC_ADDHIST(NCFILE,TEXT) adds the TEXT string to the standard convention
%   "history" global attribute of the netCDF file NCFILE.  The string is 
%   prepended, rather than appended.  The datestring is automatically
%   inserted as the history entry.
%
%   Example:  
%       nc_create_empty('myfile.nc');
%       nc_addhist('myfile.nc','Created file.');
%       nc_adddim('myfile.nc','lat',180);
%       nc_addhist('myfile.nc','added lat dimension.');
%       nc_dump('myfile.nc');
%
%   See also NC_ATTPUT.

if ~ischar(attval)
	error ('snctools:addHist:badDatatype', ...
	       'The history attribute value must be character.' );
end


try
	old_hist = nc_attget(ncfile,nc_global,'history');
catch %#ok<CTCH>
	% The history attribute must not have existed.  That's ok.
	old_hist = '';
end


if isempty(old_hist)
	new_history = sprintf('%s:  %s',datestr(now),attval);
else
	new_history = sprintf('%s:  %s\n%s',datestr(now),attval,old_hist);
end
nc_attput(ncfile,nc_global,'history',new_history);



