function test_nc_varget_neg_opendap()

if getpref('SNCTOOLS','TEST_REMOTE',false)

	% Regression
	%test_regressionErrorMsgBadUrl;

end

return

%==========================================================================
function test_regressionErrorMsgBadUrl ()
% Regression test.  If the URL is wrong, then the error message must give 
% name of the wrong url.   01-04-2007
% 

    url = 'http://doesntexits:8080/thredds/dodsC/nexrad/composite/1km/agg';
    try
        nc_varget ( url, 'y', 0, 1 );
    catch me
        if ~strcmp(me.identifier,'SNCTOOLS:nc_varget_java:fileOpenFailure')
            error ( 'Error id ''%s'' was not expected.', id );
        end
        if ~strfind(me.message, url)
            error ( 'Error message did not contain the incorrect url.');
        end
        fprintf('\n\n\tThe above error message is expected, don''t freak out...\n\n');
    end
return






