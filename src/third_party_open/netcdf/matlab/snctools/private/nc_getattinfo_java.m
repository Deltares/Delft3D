function Attribute = nc_getattinfo_java(jatt)
%NC_GETATTINFO_JAVA:  return metadata about netcdf attribute

Attribute = struct('Name','','Nctype',0,'Datatype','','Value',[]);
    
Attribute.Name = char(jatt.getName());
    
datatype = char(jatt.getDataType().toString());
switch ( datatype )
    case 'double'
        Attribute.Nctype = 6; %#ok<*AGROW>
        Attribute.Datatype = 'double';
        
        j_array = jatt.getValues();
        values = j_array.copyTo1DJavaArray();
        Attribute.Value = double(values)';
        
    case 'float'
        Attribute.Nctype = 5;
        Attribute.Datatype = 'single';
        
        j_array = jatt.getValues();
        values = j_array.copyTo1DJavaArray();
        Attribute.Value = double(values)';
        
    case 'String'
        Attribute.Nctype = 12;
        Attribute.Datatype = 'string';
        shape = double(jatt.getLength);
        Attribute.Value = snc_pp_strings( jatt, jatt.getValues(), shape) ;
        
    case 'char'
        Attribute.Nctype = 2;
        Attribute.Datatype = 'char';
        Attribute.Value = char ( jatt.getStringValue());
        
    case 'byte'
        Attribute.Nctype = 1;
        Attribute.Datatype = 'int8';
        
        j_array = jatt.getValues();
        values = j_array.copyTo1DJavaArray();
        Attribute.Value = double(values)';
        
    case 'short'
        Attribute.Nctype = 3;
        Attribute.Datatype = 'int16';
        
        j_array = jatt.getValues();
        values = j_array.copyTo1DJavaArray();
        Attribute.Value = double(values)';
        
    case 'int'
        Attribute.Nctype = 4;
        Attribute.Datatype = 'int32';
        
        j_array = jatt.getValues();
        values = j_array.copyTo1DJavaArray();
        Attribute.Value = double(values)';
        
    case 'long'
        Attribute.Nctype = 4;
        Attribute.Datatype = 'int64';
        
        j_array = jatt.getValues();
        values = j_array.copyTo1DJavaArray();
        Attribute.Value = int64(values)';
        
    otherwise
        error('snctools:nc_attinfo_java:unhandledDatatype', ...
            'Unhandled attribute datatype ''%s''\n', datatype );
end
    

return


