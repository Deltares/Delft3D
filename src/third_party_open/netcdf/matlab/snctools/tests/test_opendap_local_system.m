function test_opendap_local_system()

fprintf('\t\tRunning system level test of OPeNDAP/netcdf/write  ...  ' );
run_opendap_save_compare()
fprintf('OK\n');


%--------------------------------------------------------------------------
function run_opendap_save_compare()
%NETCDF_TEST   load opendap vars, save as local netcdf3, load it again & compare
%
% The original author of this code is Gerben de Boer

%% define
ncurl  = 'http://opendap.deltares.nl/thredds/dodsC/opendap/rijkswaterstaat/kustlidar/09bn2.nc';
ncfile = 'nctools_test.nc';

%% load url (always java)

D.x = nc_varget(ncurl,'x');
D.y = nc_varget(ncurl,'y');
			   
%% save nc (mexnc or mathworks native)
nc_create_empty(ncfile);
				     
nc_adddim(ncfile,'x',length(D.x));
nc_adddim(ncfile,'y',length(D.y));

tmp = nc_info(ncurl);
I.x = clean_strings(tmp.Dataset(1));
I.y = clean_strings(tmp.Dataset(2));
									   
nc_addvar(ncfile,I.x);
nc_addvar(ncfile,I.y);
											    
nc_varput(ncfile,'x',D.x);
nc_varput(ncfile,'y',D.y);

%% load nc (java, mexnc or mathworks native)

E.x = nc_varget(ncfile,'x');
E.y = nc_varget(ncfile,'y');

tmp = nc_info(ncfile);
J.x = tmp.Dataset(1);
J.y = tmp.Dataset(2);
																	    
%% test

OK(1) = isequal(E,D); % test data
OK(2) = isequal(I,J); % test meta-data  

OK = all(OK);
if ~OK
	error('failed');
end


return




%--------------------------------------------------------------------------
function y = clean_strings(x)
% Turn any cellstr of length 1 into just a char array.
% Remove chunking, shuffle, and deflate

y = x;

for j = 1:numel(y.Attribute)
    
    % If any char arrays claim to be NC_STRING, force them to look like
    % NC_CHAR.
    if ischar(y.Attribute(j).Value) && ...
            ((y.Attribute(j).Nctype == 12) || strcmp(y.Attribute(j).Datatype,'string'))
        y.Attribute(j).Nctype = 2;
        y.Attribute(j).Datatype = 'char';       
    end
    

end

