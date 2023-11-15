function return_status = nc_diff ( nc1, nc2, varargin )
%NC_DIFF  Determine if two NetCDF files contain the same data.  
%   This function is not recommended and is no longer supported.
%
%   STATUS = NC_DIFF(NCFILE1,NCFILE2) compares the data in two
%   netCDF files.  STATUS will be zero if the files are the same.
%
%   STATUS = NC_DIFF(NCFILE1,NCFILE2,'-attributes') compares 
%   data and attributes in two netCDF files.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Id$
% $LastChangedDate$
% $LastChangedRevision$
% $LastChangedBy$
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


warning ( 'SNCTOOLS:nc_diff:deprecated', ...
          'NC_DIFF is deprecated and may be removed in a future version of SNCTOOLS');

return_status = -1;
do_attributes = 0;

%
% parse any extra arguments
for j = 3:nargin
	switch varargin{j-2}
	case { '-attributes', '-attribute' }
		do_attributes = 1;
	otherwise
		fprintf ( 1, '%s:  unrecognized option ''%s''\n', mfilename, varargin{j-2} );
	end
end


%
% Get all the metadata about each file
nc1_info = nc_info ( nc1 );
nc2_info = nc_info ( nc2 );




num_datasets_to_check = length(nc1_info.Dataset);
num_nc2_datasets = length(nc2_info.Dataset);

no_difference_found = 1;

for j = 1:num_datasets_to_check
	
	nc1_varname = nc1_info.Dataset(j).Name;

	%
	% Find the this dataset in the 2nd netcdf file
	foundit = 0;
	for k = 1:num_nc2_datasets
		if strcmp ( nc1_varname, nc2_info.Dataset(k).Name )
			foundit=1;
		end
	end

	if foundit
		fprintf ( 1, 'Checking variable %s...\n', nc1_varname );
		var1_info = nc_getvarinfo ( nc1, nc1_varname );
		var2_info = nc_getvarinfo ( nc2, nc1_varname );
		var1 = nc_varget ( nc1, nc1_varname );
		var2 = nc_varget ( nc2, nc1_varname );

		%
		% Check the sizes
		if any(var1_info.Size-var2_info.Size)
			no_difference_found = 0;
			var1_size = sprintf ( ' %d ', var1_info.Size );
			var2_size = sprintf ( ' %d ', var2_info.Size );
			fprintf ( 1, 'Variable %s differs.  It has a size of [ %s ] in %s and [ %s ] in %s.\n', nc1_varname, var1_size, nc1, var2_size, nc2 );
			continue;
		end


		d = var1 - var2;
		ind = find(abs(d)>0);
		if ~isempty(ind)
			no_difference_found = 0;
			percent_different = length(ind) / prod ( nc1_info.Dataset(j).Size ) * 100;
			fprintf ( 1, 'Variable %s differs.  About %.1f%% of the values are different.\n', nc1_varname, percent_different );
		end
	else
		fprintf ( 1, 'Variable %s is not present in the 2nd NetCDF file %s.\n', nc1_varname, nc2 );
	end

	if do_attributes
		status = compare_attribute_list ( var1_info, var2_info );
		if status == 0
			no_difference_found = 0;
		end
	end

end

%
% fake it out for the global attributes
if do_attributes
	g1.Name = 'NC_GLOBAL';
	g1.Attribute = nc1_info.Attribute;
	g2.Name = 'NC_GLOBAL';
	g2.Attribute = nc2_info.Attribute;
	status = compare_attribute_list ( g1, g2 );
	if status == 0
		no_difference_found = 0;
	end
end

if no_difference_found 
	return_status = 0;
	fprintf ( 1, 'No difference was found.\n' );
end

return


function no_difference_found = compare_attribute_list (var1, var2)

	no_difference_found = 1;

	n1 = length(var1.Attribute);
	n2 = length(var2.Attribute);

	%
	% If neither has attributes, then all is well.
	if ( n1 == n2 ) && ( n1 == 0 )
		return;
	end

	%
	% If one or the other has no attributes.
	if ( n1 == 0 ) 
		fprintf ( 1, '%s:  first ncfile variable %s has no attributes while 2nd ncfile variable has %d.\n', mfilename, var1.Name, n2 );
		no_difference_found = 0;
		return;
	end

	%
	% If one or the other has no attributes.
	if ( n2 == 0 ) 
		fprintf ( 1, '%s:  first ncfile variable %s has %d attributes while 2nd ncfile variable has none.\n', mfilename, var1.Name, n1 );
		no_difference_found = 0;
		return;
	end

	%
	% If they don't have the same number.
	if ( n1 ~= n2 ) 
		fprintf ( 1, '%s:  first ncfile variable %s has %d attributes while 2nd ncfile variable has %d.\n', mfilename, var1.Name, n1, n2 );
		no_difference_found = 0;
	end

	%
	% Go thru all the var1 attributes.
	for j = 1:n1
		found_it = 0;
		for k = 1:n2
			if strcmp ( var1.Attribute(j).Name, var2.Attribute(k).Name )
				found_it = 1;
				break;
			end
		end

		%
		% Was the attribute present in the 2nd file?
		if found_it == 0
			no_difference_found = 0;
			format = '%s:  var ''%s'' attribute ''%s'' was not present in the 2nd file.\n';
			fprintf ( 1, format, mfilename, var1.Name, var1.Attribute(j).Name  );
			continue
		end



		%
		% Are the datatypes the same?
		if (var1.Attribute(j).Nctype ~= var2.Attribute(k).Nctype)
			no_difference_found = 0;
			format = '%s:  var ''%s'' attribute ''%s'' has type %s in the first file, type %s in the 2nd.\n';
			fprintf ( 1, format, mfilename, var1.Name, var1.Attribute(j).Name, nc_datatype_string(var1.Attribute(j).Nctype), nc_datatype_string(var2.Attribute(k).Nctype) );
			continue
		end

		%
		% Check lengths.
		l1 = length(var1.Attribute(j).Value);
		l2= length(var2.Attribute(k).Value);
		if (l1 ~= l2)
			no_difference_found = 0;
			format = '%s:  var ''%s'' attribute ''%s'' has length %d in the first file, length %d in the 2nd.\n';
			fprintf ( 1, format, mfilename, var1.Name, var1.Attribute(j).Name, l1, l2 );
			continue
		end


		%
		% Check values
		if ( any(var1.Attribute(j).Value - var2.Attribute(k).Value) )
			no_difference_found = 0;
			format = '%s:  var ''%s'' attribute ''%s'' differs from that of the 2nd file.\n';
			fprintf ( 1, format, mfilename, var1.Name, var1.Attribute(j).Name );
			continue
		end

	end

return
