function [varargout] = mexcdf60 ( varargin )
% MEXCDF60:  wrapper routine to call mexnc.
%
% Provided for backwards compatibility.  "mexcdf60" is no longer a 
% name for the underlying mexfile.

if nargout > 0
	varargout = cell(1, nargout);
	[varargout{:}] = feval('mexnc', varargin{:});
else
	feval('mexnc', varargin{:});
end
