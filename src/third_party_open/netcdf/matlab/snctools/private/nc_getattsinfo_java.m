function Attribute = nc_getattsinfo_java(j_att_list)
% NC_GETATTSINFO_JAVA:  returns metadata about netcdf attributes
%
% USAGE:  Attribute = nc_getattsinfo_java(j_att_list);
%
% PARAMETERS:
% Input:
%     j_att_list:
%         Of type "java.util.ArrayList".  Each list member is of type
%         "ucar.nc2.Attribute"
% Output:
%     Attribute:
%         Structure array of attribute metadata.  The fields are 
%         
%         Name
%         Nctype (backwards compatibility)
%         Datatype
%         Value

j_att_iterator = j_att_list.listIterator();
j = 0;

Attribute = struct('Name','','Nctype',0,'Datatype','','Value',[]);
while 1
    
    % This throws an exception when we've reached the end of the list.
    try
        jatt = j_att_iterator.next();
    catch %#ok<CTCH>
        break;
    end
    
    j = j + 1;
    Attribute(j) = nc_getattinfo_java(jatt);

end

if j == 0
    Attribute = [];
end

return

