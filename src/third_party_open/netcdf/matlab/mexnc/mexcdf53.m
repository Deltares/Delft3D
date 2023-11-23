function [varargout] = mexcdf53 ( varargin )
% MEXCDF53:  wrapper routine to call mexnc.
%
% Provided for backwards compatibility.  "mexcdf53" is no longer the 
% name of the underlying mexfile.

if nargout > 0
	varargout = cell(1, nargout);
	[varargout{:}] = feval('mexnc', varargin{:});
else
	feval('mexnc', varargin{:});
end

