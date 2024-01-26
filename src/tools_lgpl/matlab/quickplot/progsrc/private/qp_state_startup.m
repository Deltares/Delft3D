function State=qp_state_startup
%QP_STATE_STARTUP Initialize QuickPlot plot state.

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

State.version=1;

State.D='';
State.T=[];
State.S=[];
State.M=[];
State.N=[];
State.K=[];
State.Slider=0;
State.vectorscalingmode='automatic';
State.vectorscale=1;
State.verticalscalingmode='unrestricted'; %''
State.verticalscalefactor=1;
State.presentationtype=''; %'dummy'
State.numformat='%.2f';
State.fontsize=6;
State.horizontalalignment='centre';
State.verticalalignment='middle';
State.vectorcomponent='';
State.vectorcolour='';
State.colourdams=0;
State.classifycolours=0;
State.thresholds=[]; %'none'
State.thresholddistribution='linear';
State.colour=[0 0 1];
State.facecolour=[.8 .9 .6];
State.textboxfacecolour=[1 1 128/255];
%State.MNK

L=set(0,'defaultlinelinestyle');
Li=strmatch('-',L,'exact');
if isempty(Li)
    Li=1;
end
State.linestyle=L{Li};

State.linewidth=0.5;

L=set(0,'defaultlinemarker');
Li=strmatch('none',L,'exact');
if isempty(Li)
    Li=1;
end
State.marker=L{Li};

State.markercolour=[0 0 1];
State.markerfillcolour=[0 0 1];
State.colourlimits=[];
State.symmetriccolourlimits=0;

cmaps=qp_colormap;
imap=ustrcmpi('jet',cmaps);
if imap<0
    imap=1;
end
State.colourmap=cmaps{imap};

State.colourbar='vertical';

State.thinningmode='none';
State.thinningfactors=[1 1 1];
State.thinningdistance=50;
State.clippingvalues='';
