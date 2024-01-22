function [varargout] = mexcdf(varargin)

% mexcdf -- Driver for Matlab-5/NetCDF C-Language interface.
%  mexcdf('action', ...) performs the specified NetCDF action.
%   When writing or reading a NetCDF variable, the dimensional
%   sequence of the targetted Matlab array must be reversed,
%   either BEFORE using 'varput' or 'varputg', or AFTER using
%   'varget' or 'vargetg'.  This preserves the left-to-right
%   dimensional arrangement that is defined by 'vardef' and
%   retrieved by 'varinq'.  Data output from a variable are
%   returned as a Matlab matrix (two-dimensional), in keeping
%   with the behavior of mexcdf under Matlab-4.  The base-index
%   for slabs is zero (0), and -1 can be used to specify the
%   remaining count along any variable direction from the
%   starting point.  NOTE: a vector (an array with no more
%   than one non-unity dimension) is always returned as
%   a column, by mexcdf convention.
%
%   To process multi-dimensional arrays under Matlab-5,
%   use "ncmex", rather than "mexcdf".
%
% mexcdf('USAGE')
% [cdfid, rcode] = mexcdf('CREATE', 'path', cmode)
% cdfid = mexcdf('OPEN', 'path', mode)
% status = mexcdf('REDEF', cdfid)
% status = mexcdf('ENDEF', cdfid)
% [ndims, nvars, natts, recdim, status] = mexcdf('INQUIRE', cdfid)
% status = mexcdf('SYNC', cdfid)
% status = mexcdf('ABORT', cdfid)
% status = mexcdf('CLOSE', cdfid)
%
% status = mexcdf('DIMDEF', cdfid, 'name', length)
% [dimid, rcode] = mexcdf('DIMID', cdfid, 'name')
% ['name', length, status] = mexcdf('DIMINQ', cdfid, dimid)
% status = mexcdf('DIMRENAME', cdfid, 'name')
%
% status = mexcdf('VARDEF', cdfid, 'name', datatype, ndims, [dim])
% [varid, rcode] = mexcdf('VARID', cdfid, 'name')
% ['name', datatype, ndims, [dim], natts, status] = mexcdf('VARINQ', cdfid, varid)
% status = mexcdf('VARPUT1', cdfid, varid, coords, value, autoscale)
% [value, status] = mexcdf('VARGET1', cdfid, varid, coords, autoscale)
% status = mexcdf('VARPUT', cdfid, varid, start, count, value, autoscale)
% [value, status] = mexcdf('VARGET', cdfid, varid, start, count, autoscale)
% status = mexcdf('VARPUTG', cdfid, varid, start, count, stride, [], value, autoscale)
% [value, status] = mexcdf('VARGETG', cdfid, varid, start, count, stride, [], autoscale)
% status = mexcdf('VARRENAME', cdfid, varid, 'name')
%
% status = mexcdf('ATTPUT', cdfid, varid, 'name', datatype, len, value) 
% [datatype, len, status] = mexcdf('ATTINQ', cdfid, varid, 'name')
% [value, status] = mexcdf('ATTGET', cdfid, varid, 'name')
% status = mexcdf('ATTCOPY', incdf, invar, 'name', outcdf, outvar)
% ['name', status] = mexcdf('ATTNAME', cdfid, varid, attnum)
% status = mexcdf('ATTRENAME', cdfid, varid, 'name', 'newname')
% status = mexcdf('ATTDEL', cdfid, varid, 'name')
%
% status = mexcdf('RECPUT', cdfid, recnum, [data], autoscale, recdim)
% [[data], status] = mexcdf('RECGET', cdfid, recnum, autoscale, recdim)
% [[recvarids], [recsizes], status] = mexcdf('RECINQ', cdfid, recdim)
%
% len = mexcdf('TYPELEN', datatype)
% old_fillmode = mexcdf('SETFILL', cdfid, fillmode)
%
% old_ncopts = mexcdf('SETOPTS', ncopts)
% ncerr = mexcdf('ERR')
% code = mexcdf('PARAMETER', 'NC_...')
%
% Notes:
%  1. The rcode is always zero.
%  2. The dimid can be number or name.
%  3. The varid can be number or name.
%  4. The attname can be name or number.
%  5. The operation and parameter names are not case-sensitive.
%  6. The cmode defaults to 'NC_NOCLOBBER'.
%  7. The mode defaults to 'NC_NOWRITE'.
%  8. The value -1 determines length automatically.
%  9. The operation names can prepend 'nc'.
% 10. The parameter names can drop 'NC_' prefix.
% 11. Dimensions: Matlab (i, j, ...) <==> [..., j, i] NetCDF.
% 12. Indices and identifiers are zero-based.
% 13. One-dimensional slabs are returned as column-vectors.
% 14. Attributes are returned as row-vectors.
% 15. Scaling can be automated via 'scale_factor' and 'add_offset'.
 
% Copyright (C) 1992-1997 Dr. Charles R. Denham, ZYDECO.
% All Rights Reserved.

% Version of 16-May-96 at 10:17:47.75.
% Version of 06-Jan-97 at 14:04:00.
% Version of 20-Mar-97 at 14:46:00.
% Version of 15-Jul-1997 07:52:23.
% Updated    07-Dec-2000 14:50:39.

if nargin < 1, help(mfilename), return, end

% Mex-file gateway.

v = version;
if eval(v(1)) > 4
	fcn = 'mexcdf53';   % Matlab-5 or 6.
elseif eval(v(1)) == 4
	fcn = 'mexcdf4';    % Matlab-4 only.
else
	error(' ## Unrecognized Matlab version.')
end

% The "record" routines are emulated.

op = lower(varargin{1});
if any(findstr(op, 'rec'))
	fcn = op;
	if ~strcmp(fcn(1:2), 'nc')
		fcn = ['nc' fcn];
	end
	varargin{1} = [];
end

% Matlab-5 comma-list syntax.

if nargout > 0
	varargout = cell(1, nargout);
	[varargout{:}] = feval(fcn, varargin{:});
else
	feval(fcn, varargin{:});
end

% Collapse to two-dimensions.

if nargout > 0
	switch op
	case {'varget', 'vargetg', 'ncvarget', 'ncvargetg'}
		theSize = size(varargout{1});
		f = find(theSize > 1);
		if ~any(f), f = 1; end
		if length(theSize) > 1   % Always true in Matlab.
			m = theSize(f(1));
			n = prod(theSize) ./ m;
			if m == 1, m = n; n = 1; end
			varargout{1} = reshape(varargout{1}, [m n]);
		end
	otherwise
	end
end
