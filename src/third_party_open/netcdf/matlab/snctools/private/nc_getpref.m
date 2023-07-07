function value = nc_getpref(name)
%   PREF = NC_GETPREF(NAME) returns the value of an SNCTOOLS preference.
%   
%   This routine should not be called directly.

persistent PRESERVE_FVD

if isempty(PRESERVE_FVD)
    PRESERVE_FVD = getpref('SNCTOOLS','PRESERVE_FVD',false);
end

if strcmp(name,'PRESERVE_FVD')
    value = PRESERVE_FVD;
else
    error('unrecognized input to NC_GETPREF');
end
