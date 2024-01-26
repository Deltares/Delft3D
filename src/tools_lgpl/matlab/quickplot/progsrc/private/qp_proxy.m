function varargout = qp_proxy(cmd,varargin)
%QP_PROXY Proxy for file data in figures.

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
persistent save

switch cmd
    case {'open','openldb','opennew','openurl','grid_opennew','grid_open'}
        type = cmd(1:4);
        if strcmpi(type,'grid')
            [FI,FileName,Tp,Otherargs]=grid_fopen(cmd(6:end),varargin{:});
        else
            type='main';
            [FI,FileName,Tp,Otherargs]=qp_fmem(cmd,varargin{:});
        end
        if isempty(FI)
            Proxy = [];
        else
            Proxy.QPF              = 2;
            Proxy.Name             = FileName;
            Proxy.Data.Key         = [type '_proxy_key_' num2hex(now)];
            if isfield(FI,'QP_Options')
                Proxy.Data.Options = FI.QP_Options;
                FI = rmfield(FI,'QP_Options');
            else
                Proxy.Data.Options = [];
            end
            save.(Proxy.Data.Key)  = FI;
            Proxy.FileType         = Tp;
            if isfield(FI,'Options')
                Proxy.Options      = FI.Options;
            else
                Proxy.Options      = 0;
            end
            Proxy.Otherargs        = Otherargs;
        end
        [varargout{1:4}] = deal(Proxy,FileName,Tp,Otherargs);

    case 'reload'
        Proxy = varargin{1};
        Key   = Proxy.Data.Key;
        switch Key(1:4)
            case 'main'
                [FI,FileName,Tp,Otherargs]=qp_fmem('open',Proxy.Name,Proxy.Otherargs{:});
            case 'grid'
                [FI,FileName,Tp,Otherargs]=grid_fopen('open',Proxy.Name,Proxy.Otherargs{:});
        end
        if ~isempty(FI)
            save.(Key) = FI;
            %
            % We may need to update the Proxy.Options field here. Usually
            % this doesn't change, so therefore stop step is currently
            % skipped.
            %
            if isfield(Proxy.Data.Options,'AttribFiles')
                for i = 1:length(Proxy.Data.Options.AttribFiles)
                    qp_proxy('reload',Proxy.Data.Options.AttribFiles(1))
                end
            end
        else
            error('Unable to reload file.')
        end
        
    case 'clear'
        Proxy = varargin{1};
        Key   = Proxy.Data.Key;
        if isfield(save,Key)
            save = rmfield(save,Key);
            %
            if isfield(Proxy.Data.Options,'AttribFiles')
                for i = 1:length(Proxy.Data.Options.AttribFiles)
                    qp_proxy('clear',Proxy.Data.Options.AttribFiles(1))
                end
            end
        end
        
    case 'load'
        Proxy = varargin{1};
        Key   = Proxy.Data.Key;
        if ~isfield(save,Key)
            qp_proxy('reload',Proxy)
        end
        if isfield(save,Key)
            Info = save.(Key);
            Info.QP_Options = Proxy.Data.Options;
        else
            Info = [];
        end
        varargout{1} = Info;

    case 'store'
        Proxy = varargin{1};
        Key   = Proxy.Data.Key;
        FI    = varargin{2};
        if isfield(FI,'QP_Options')
            Proxy.Data.Options = FI.QP_Options;
            FI = rmfield(FI,'QP_Options');
        end
        save.(Key) = FI;
        varargout{1} = Proxy;

    otherwise
        error('Invalid qp_proxy command: %s',cmd)
end
