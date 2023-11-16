function data = nc_getall ( ncfile ,varargin)
%NC_GETALL Read entire contents of netCDF file.
%   This function is intended only for test purposes, not for analysing 
%   data.
%   
%   NCDATA = NC_GETALL(NCFILE) reads the entire contents of 
%   the netCDF file NCFILE into the structure NCDATA. NC_GETALL 
%   makes valid matlab field names from all variable and 
%   attribute names, thereby discarding any information on 
%   dimension names, and hence destroying an essential feature
%   in netCDF's self-describing data rationale. We suggest to
%   limit the use of this function for test purposes, and to use
%   NC_DUMP/NC_INFO and NC_VARGET/NC_ATTGET instead.
%
%   NCDATA = NC_GETALL(NCFILE,<maxSize>) loads only variables 
%   whose data size (NUMEL) does not exceed maxSize (default 
%   2e6, set to Inf the load the entire file, and to 0 to load
%   only meta-data). 
% 
%   Note: NC_INFO has similar functionality, but 
%   - does not turn attribute names into field names and 
%     hence keeps meta-data essential for the netCDF rationale
%   - does not load any data
%   - loads additional meta-data as Size, Dimension names, 
%     Datatype and Unlimited
% 
%See also: NC_INFO

   warning ( 'SNCTOOLS:nc_getall:dangerous', ...
             'NC_GETALL discards information on dimensions and hence poses a risk when interpreting data, use only for testing.');

   % Show usage if too few arguments.
   maxSize = 2e6; % not too big a default, 
   % but big enough to load looong timeseries such as 
   % http://opendap.deltares.nl/thredds/dodsC/opendap/rijkswaterstaat/waterbase/sea_surface_height/id1-DELFZL.nc.html (mar 2010)
   if nargin < 1
      error ( 'must have one input argument.\n' );
   elseif nargin > 1
       maxSize = varargin{1};
   end
   
   % let all netCDF backend and format issues be handled behidn the screend my nc_info and nc_varget
   
   fileinfo = nc_info(ncfile);
   Att = atts2struct(fileinfo.Attribute);
   if ~isempty(Att)      
       data.global_atts = Att;
   end

   for idat =1:length(fileinfo.Dataset)
      fld             = fileinfo.Dataset(idat).Name;
      Att             = atts2struct(fileinfo.Dataset(idat).Attribute);
      if ~isempty(Att)
      data.(fld)      = Att;
      end
      if prod(fileinfo.Dataset(idat).Size) <= maxSize
      data.(fld).data = nc_varget(ncfile,fld);
      else
      data.(fld).data = ['>> Not loaded as it has more than maxSize=',num2str(maxSize),' elements'];
      data.(fld).Size = fileinfo.Dataset(idat).Size;
      end
   end
   
   if ~exist('data','var')
      data = struct([]);
   end
        
%-----------------------------------------------------------------------

function S = atts2struct(A)

   S = [];

   for iatt =1:length(A)
      sanitized_attname = genvarname(A(iatt).Name);
      S.(sanitized_attname) = A(iatt).Value;
   end

%-----------------------------------------------------------------------
