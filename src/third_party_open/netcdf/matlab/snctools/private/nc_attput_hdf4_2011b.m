function nc_attput_hdf4_2011b(hfile,varname,attname,attval)
% HDF4 SD package handler for NC_ATTPUT.

import matlab.io.hdf4.*

sd_id = sd.start(hfile,'write');

try
    if varname == -1
        obj_id = sd_id;
    else
        idx = sd.nameToIndex(sd_id,varname);
        obj_id = sd.select(sd_id,idx);
    end
    
    % Is it a predefined attribute?
    switch(attname)
        case 'long_name'
            try 
                [label,unit,format,coordsys] = sd.getDataStrs(obj_id); 
            catch me
                unit = ''; 
                format = '';
                coordsys = '';
            end
            label = attval;
            sd.setDataStrs(obj_id,label,unit,format,coordsys);
            
        case 'units'
            try
                [label,unit,format,coordsys] = sd.getDataStrs(obj_id); 
            catch me
                label = '';
                format = '';
                coordsys = '';
            end
            unit = attval;
            sd.setDataStrs(obj_id,label,unit,format,coordsys);
         
        case 'format'
            try 
                [label,unit,format,coordsys] = sd.getDataStrs(obj_id); 
            catch me
                label = '';
                unit = '';
                coordsys = '';
            end
            format = attval;
            sd.setDataStrs(obj_id,label,unit,format,coordsys);
       
        case 'coordsys'
            try 
                [label,unit,format,coordsys] = sd.getDataStrs(obj_id); 
            catch me
                label = '';
                unit = '';
                format = '';
            end
            coordsys = attval;
            sd.setDataStrs(obj_id,label,unit,format,coordsys);
            
        case 'scale_factor'
            try
                [cal,cal_err,offset,offset_err,data_type] = sd.getCal(obj_id); 
            catch me
                cal_err = 0; 
                offset = 0;
                offset_err = 0;
                data_type = 'double';
            end
            sd.setCal(obj_id,attval,cal_err,offset,offset_err,data_type);
            
        case 'add_offset'
            try
                [cal,cal_err,offset,offset_err,data_type] = sd.getCal(obj_id); 
            catch me
                cal = 1.0;
                cal_err = 0; 
                offset_err = 0;
                data_type = '';
            end
            sd.setCal(obj_id,cal,cal_err,attval,offset_err,data_type);
      
        case 'valid_range'
            [rmax,rmin] = sd.getRange(obj_id); 
            sd.setRange(obj_id,rmax,rmin);
            
        case '_FillValue'
            [name,dimsizes,data_type,nattrs] = sd.getInfo(obj_id); 
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
            sd.setFillValue(obj_id,attval);
            
        otherwise
            sd.setAttr(obj_id,attname,attval);
    end
    
    if varname ~= -1
        sd.endAccess(obj_id);
    end
catch me
    sd.close(sd_id);
    rethrow(me);
end

sd.close(sd_id);

return
