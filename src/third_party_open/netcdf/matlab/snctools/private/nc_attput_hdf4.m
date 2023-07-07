function nc_attput_hdf4(hfile,varname,attname,attval)
% HDF4 handler for NC_ATTPUT.

sd_id = hdfsd('start',hfile,'write');
if sd_id < 0
    error('snctools:attput:hdf4:startFailed', ...
        'START failed on %s.', hfile);
end

if varname == -1
    obj_id = sd_id;
else
    idx = hdfsd('nametoindex',sd_id,varname);
    if idx < 0
        hdfsd('end',sd_id);
        error('snctools:nc_info:hdf4:nametoindexFailed', ...
            'Unable to index %s.', varname);
    end
    obj_id = hdfsd('select',sd_id,idx);
    if  obj_id < 0
        hdfsd('end',sd_id);
        error('snctools:nc_info:hdf4:selectFailed', ...
            'Unable to select %s.', varname);
    end
end

% Is it a predefined attribute?
switch(attname)
    case 'long_name'
         [label,unit,format,coordsys,status] = hdfsd('getdatastrs',obj_id,1000); %#ok<ASGLU>
        if ( status < 0 )
            unit = '';
            format = '';
            coordsys = '';
        end
        label = attval;
        status = hdfsd('setdatastrs',obj_id,label,unit,format,coordsys);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getdatstrsFailed', ...
                'Unable to set datastrings.' );
        end
        
    case 'units'
         [label,unit,format,coordsys,status] = hdfsd('getdatastrs',obj_id,1000); %#ok<ASGLU>
        if ( status < 0 )
            label = '';
            format = '';
            coordsys = '';
        end
        unit = attval;
        status = hdfsd('setdatastrs',obj_id,label,unit,format,coordsys);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getdatstrsFailed', ...
                'Unable to set datastrings.' );
        end
     
    case 'format'
        [label,unit,format,coordsys,status] = hdfsd('getdatastrs',obj_id,1000); %#ok<ASGLU>
        if ( status < 0 )
            unit = '';
            label = '';
            coordsys = '';
        end
        format = attval;
        status = hdfsd('setdatastrs',obj_id,label,unit,format,coordsys);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getdatstrsFailed', ...
                'Unable to set datastrings.' );
        end
   
    case 'coordsys'
        [label,unit,format,coordsys,status] = hdfsd('getdatastrs',obj_id,100); %#ok<ASGLU>
        if ( status < 0 )
            unit = '';
            format = '';
            label = '';
        end
        coordsys = attval;
        status = hdfsd('setdatastrs',obj_id,label,unit,format,coordsys);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getdatstrsFailed', ...
                'Unable to set datastrings.' );
        end      
        
    case 'scale_factor'
        [cal,cal_err,offset,offset_err,data_type,status] = hdfsd('getcal',obj_id); %#ok<ASGLU>
        if ( status < 0 )
            cal_err = 0;
            offset = 0;
            offset_err = 0;
            data_type = 'double';
        end
        status = hdfsd('setcal',obj_id,attval,cal_err,offset,offset_err,data_type);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getcalFailed', ...
                'Unable to set calibration.' );
        end
        
    case 'add_offset'
        [cal,cal_err,offset,offset_err,data_type,status] = hdfsd('getcal',obj_id); %#ok<ASGLU>
        if ( status < 0 )
            cal = 1;
            cal_err = 0;
            offset_err = 0;
            data_type = 'double';
        end
        status = hdfsd('setcal',obj_id,cal,cal_err,attval,offset_err,data_type);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getcalFailed', ...
                'Unable to set calibration.' );
        end
  
    case 'valid_range'
        [rmax,rmin,status] = hdfsd('getrange',obj_id); 
        if ( status < 0 )
            rmax = attval(2);
            rmin = attval(1);
        end
        status = hdfsd('setrange',obj_id,rmax,rmin);
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getrangeFailed', ...
                'Unable to set calibration.' );
        end
        
    case '_FillValue'
        [name,rank,dimsizes,data_type,nattrs,status] = hdfsd('getinfo',obj_id); %#ok<ASGLU>
        if ( status < 0 )
            if varname == -1
                hdfsd('endaccess',obj_id);
            end
            hdfsd('end',sd_id);
            error('snctools:hdf4:getInfoFailed', ...
                'Unable to get information about dataset.' );
        end
        switch(data_type)
            case 'double'
                attval = double(attval);
            case 'single'
                attval = single(attval);
            case 'int32'
                attval = int32(attval);
            case 'uint32'
                attval = uint32(attval);
            case 'int16'
                attval = int16(attval);
            case 'uint16'
                attval = uint16(attval);
            case 'int8'
                attval = int8(attval);
            case 'uint8'
                attval = uint8(attval);
            case 'char'
                attval = char(attval);
        end
        status = hdfsd('setfillvalue',obj_id,attval);
        
    otherwise
        status = hdfsd('setattr',obj_id,attname,attval);
end

if status < 0
    if varname == -1
        hdfsd('endaccess',obj_id);
    end
    hdfsd('end',sd_id);
    error('snctools:attput:hdf4:setattrFailed', ...
        'SETATTR failed on %s.', hfile);
end

if varname ~= -1
    status = hdfsd('endaccess',obj_id);
    if status < 0
        hdfsd('end',sd_id);
        error('snctools:attput:hdf4:endaccessFailed', ...
            'ENDACCESS failed on %s.', hfile);
    end
end
status = hdfsd('end',sd_id);
if status < 0
    error('snctools:attput:hdf4:endFailed', ...
        'END failed on %s".', hfile);
end
return


