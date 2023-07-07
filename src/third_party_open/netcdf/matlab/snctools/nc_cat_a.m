function nc_cat_a ( input_ncfiles, output_ncfile, abscissa_var )
%NC_CAT_A  Concatentates a set of netcdf files into ascending order.
%   This function is not recommended.  Use nc_cat instead.
%
%   The concatenation is done only along unlimited variable, which by
%   definition have an unlimited dimension.  Variables which do NOT have
%   an unlimited dimension are copied over from the first of the input
%   netcdf input files.
%  
%   This m-file is not meant as a replacement for ncrcat or any of Charles
%   Zender's terrific NCO tools.  If you need NCO functionality, you should
%   get NCO tools from http://nco.sourceforge.net.  I would also characterize
%   this function as being clinically insane, and it should probably not be
%   used by anyone outside of Rutgers University.  Don't ask.
%   
%   USAGE:  nc_cat_a ( input_ncfiles, output_ncfile, abscissa_var )
%   
%   PARAMETERS:
%     Input:
%         input_ncfiles:
%             This can be either a cell array of netcdf files, or a text
%             file with one netcdf file per line
%         output_ncfile:
%             This file will be generated from scratch.
%         abscissa_var:
%             Name of an unlimited variable.  Supposing we are dealing
%             with time series, then a good candidate for this would
%             be a variable called, oh, I don't know, maybe "time".  
%     Output:
%         None.  An exception is thrown in case of an error.
%  
%   The best way to explain this is with simple examples.  Suppose that
%   the abscissa_var is "time" and that the other netcdf variable is "tsq".
%   Suppose that the first netcdf file has files for "time" and "tsq" of
%  
%        time: 0 2  4
%        tsq:  0 4 16
%  
%   Suppose the 2nd netcdf file has values of
%  
%        time:  4  6  8
%        tsq:  18 36 64
%  
%   Note that the 2nd time series has a different value of "tsq" for the 
%   abscissa value of 4.
%  
%   Running nc_cat_a will produce a single time series of
%   
%        time:  0   2   4   6   8
%        tsq:   0   4  18  36  64
%  
%   In other words, the 2nd netcdf file's abscissa/ordinate values take
%   precedence.  So the order of your netcdf files matter, and the output
%   netcdf file will have unique abscissa values.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Id$
% $LastChangedDate$
% $LastChangedRevision$
% $LastChangedBy$
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



error(nargchk(3,3,nargin,'struct'));
error(nargoutchk(0,0,nargout,'struct'));


%
% If the first input is of type char and is a file, then read it in.
% At the end of this process, the list of netcdf files to be 
% concatenated is in a cell array.
if ischar(input_ncfiles) && exist(input_ncfiles,'file')

    afid = fopen ( input_ncfiles, 'r' );
    x = textscan ( afid, '%s' );
    input_ncfiles = x{1};

elseif iscell ( input_ncfiles )

    %
    % Do nothing

else
    error ( 'first input must be either a text file or a cell array\n' );
end

num_input_files = length(input_ncfiles);


%
% This is how close the abscissa variable values have to be before they
% are considered to be the same value.
tol = 10*eps;


%
% Now construct the empty output netcdf file.
ncm = nc_info ( input_ncfiles{1} );
mode = nc_clobber_mode;

nc_create_empty(output_ncfile,mode);


%
% Add the dimensions.
for d = 1:length(ncm.Dimension)
    if ncm.Dimension(d).Unlimited
        nc_add_dimension ( output_ncfile, ncm.Dimension(d).Name, 0 );
    else
        nc_add_dimension ( output_ncfile, ncm.Dimension(d).Name, ncm.Dimension(d).Length );
    end
end

%
% Add the variables
for v = 1:length(ncm.Dataset)
    nc_addvar ( output_ncfile, ncm.Dataset(v) );

    %
    % If the variable is NOT unlimited, then we can copy over
    % its data now
    if ~ncm.Dataset(v).Unlimited
        vardata = nc_varget ( input_ncfiles{1}, ncm.Dataset(v).Name );
        nc_varput ( output_ncfile, ncm.Dataset(v).Name, vardata );
    end
end


%
% Go thru and figure out how much data we are looking at,
% then pre-allocate for speed.
total_length = 0;
for j = 1:num_input_files
    sz = nc_varsize ( input_ncfiles{j}, abscissa_var );
    total_length = total_length + sz;
end

abscissa_vardata = NaN*ones(total_length,1);
file_index = NaN*ones(total_length,1);
infile_abscissa_varindex = NaN*ones(total_length,1);



%
% Now read in the abscissa variable for each file.
start_index = 1;
for j = 1:num_input_files
    v = nc_varget ( input_ncfiles{j}, abscissa_var );
    nv = length(v);

    end_index = start_index + nv - 1;
    inds = start_index:end_index;

    abscissa_vardata(inds) = v;
    file_index(inds) = j*ones(nv,1);
    infile_abscissa_varindex(inds) = (0:nv-1)';

    start_index = start_index + nv;
end


%
% Sort the ascissa_vardata into ascending order.  
[abscissa_vardata,I] = sort ( abscissa_vardata );
file_index = file_index(I);
infile_abscissa_varindex = infile_abscissa_varindex(I);

%
% Are there any duplicates?
ind = find ( diff(abscissa_vardata) < tol );
if ~isempty(ind)
    abscissa_vardata(ind) = [];
    file_index(ind) = [];
    infile_abscissa_varindex(ind) = [];
end


%
% So now go thru each record and append it to the output file and we 
% are done.
for j = 1:length(abscissa_vardata)
    ncfile = input_ncfiles{file_index(j)};
    start = infile_abscissa_varindex(j);
    input_record = nc_getbuffer ( ncfile, start, 1 );
    nc_addnewrecs ( output_ncfile, input_record, abscissa_var );
end

