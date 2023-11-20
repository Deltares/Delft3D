function Fcn = getdata_wrapper(Fcn)
%GETDATA_WRAPPER

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2023 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

Fcn.open       = @classicopen;
Fcn.dimensions = @classicdimensions;
Fcn.locations  = @classiclocations;
Fcn.quantities = @classicquantities;
Fcn.data       = @classicdata;
Fcn.grid       = @classicgrid;
Fcn.options    = @classicoptions;
Fcn.plot       = @classicplot;

function [Data,Key] = classicopen(varargin)
if nargin==0 || ischar(varargin{1})
   Data = qpfopen(varargin{:});
   Key.Name = Data.FileName;
   Key.Type = Data.FileType;
   if isequal(Key.Type,'NEFIS')
      Key.Type = sprintf('%s %s',Key.Type,Data.SubType);
      Key.Name = [Key.Name Data.DatExt];
   end
else
   Key = varargin{1};
   Data = qpfopen(Key.Name);
end

function [Dimensions,Success] = classicdimensions(FI)
%   [Success,Dimensions]            = QP_GETDATA(FI,'dimensions')
[Success,Dimensions] = qp_getdata(FI,'dimensions');

function [Locations,Success] = classiclocations(FI)
%   [Success,Locations ]            = QP_GETDATA(FI,'locations')
[Success, Locations] = qp_getdata(FI,'locations');

function [Quantities, Success] = classicquantities(FI)
%   [Success,Locations ]            = QP_GETDATA(FI,'quantities')
[Success, Quantities] = qp_getdata(FI,'quantities');


function [Data, NewFI, Success] = classicdata(FI,Quantity,DimSelection)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,'data',Quantity,DimSelection)
[Success, Data, NewFI] = qp_getdata(FI,'data',Quantity,DimSelection);
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%cell = struct2cell(Selection);
%[Success, Data, NewFI] = qp_getdata(FI,Domain,DataFld,'data',cell{:});

function [Data, NewFI, Success] = classicgrid(FI,Domain,DataFld,Selection)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
cell = struct2cell(Selection);
[Success, Data, NewFI] = qp_getdata(FI,Domain,DataFld,'gridcell',cell{:});

function varargout = classicoptions(FI,OptionsFigure,Cmd,varargin)
%   [Success]                       = QP_GETDATA(FI,'options',OptionsFigure,'initialize')
%   [Success,NewFI     ,cmdargs]    = QP_GETDATA(FI,'options',OptionsFigure,OptionsCommand, ...)
[varargout{1:max(1,nargout)}] = qp_getdata(FI,'options',OptionsFigure,Cmd,varargin{:});

function [hNew, NewFI, Success] = classicplot(FI,Domain,DataFld,varargin)
%   [Success,hNew      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
[Success, hNew, NewFI] = qp_getdata(FI,Domain,DataFld,'plot',varargin{:});

%   [Success,StNames   ]            = QP_GETDATA(FI,Domain,DataFld,'stations',S)
%   [Success,SubFields ]            = QP_GETDATA(FI,Domain,DataFld,'subfields',F)
