function nc_varrename_hdf4(hfile,old_varname,new_varname )
% HDF4 backend to NC_VARRENAME.

fid = hdfh('open',hfile,'readwrite',0);
if fid < 0
    error('Could not open %s.', hfile);
end

status = hdfv('start',fid);
if status < 0
    hdfh('close',fid);
    error('Could not initialize Vgroup interface.');
end


% Look for the vgroup that represents the old variable.
vg_ref = hdfv('find',fid, old_varname);
if vg_ref < 0
    hdfv('end',fid);
    hdfh('close',fid);
    error('Could not find SDS %s.', old_varname);
end

% Get access to the vgroup 
vg_id = hdfv('attach',fid, vg_ref, 'w');
if vg_id < 0
    hdfv('end',fid);
    hdfh('close',fid);
    error('Could not attach to Vgroup for %s.', old_varname);
end

% Change from "SDS A" to "SDS B" 
status = hdfv('setname', vg_id, new_varname);
if status < 0
    hdfv('detach',vg_id);
    hdfv('end',fid);
    hdfh('close',fid);
    error('Could not rename variable from %s to %s.', old_varname, new_varname);
end

% Terminate access to the vgroup. 
status = hdfv('detach',vg_id);
if status < 0
    hdfv('end',fid);
    hdfh('close',fid);
    error('Could not detach from vgroup.');
end


status = hdfv('end',fid);
if status < 0
    hdfh('close',fid);
    error('Could not close down Vgroup interface.');
end

hdfh('close',fid);

