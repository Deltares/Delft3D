function putter
%PUTTER  Example of sending data via DelftIO.
%   Run this function in one MATLAB session while GETTER runs in another on
%   the same machine.
%
%   See also DIO_DEFINE, DIO_WRITE, DIO_DELETE.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                 
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

% initialize constants known to both putter and getter
NUM_DATA = 20;
NUM_TIME = 2;
MAT_SIZE = 100;

% determine appropriate buffer size (note we will not use the header block)
size_of_single = 4;
size_of_double = 8;
MAX_NUM_BYTES = max(size_of_double*NUM_DATA, size_of_single*MAT_SIZE*MAT_SIZE);

% open DelftIO connection
fprintf('\n--- Opening DelftIO stream of %i bytes. ---\n', MAX_NUM_BYTES);
dsh = dio_define('MatlabPutter2Getter', MAX_NUM_BYTES);
fprintf('Address of communication channel: %u\n', dsh);

% initialize data to be sent
data = ((1:NUM_DATA)-1)*2+0.000001;
fprintf('setdata: %d to %f\n', [1:NUM_DATA;data]);

% for various data types send data vector
for type = {'double', 'single', 'int32', 'uint8'}
    fprintf('\n--- Sending 1x%i data vector as %s. ---\n', NUM_DATA, type{:});
    for t = 0:NUM_TIME-1
        % modify data
        data = data+1;
        
        % send data
        fprintf('PUTTER writes data for time step %d\n', t);
        if ~dio_startwrite(dsh), break; end
        switch type{:}
            case 'double'
                dio_write(dsh, data);
            case 'single'
                dio_write(dsh, single(data));
            case 'int32'
                dio_write(dsh, int32(data));
            case 'uint8'
                dio_write(dsh, uint8(data));
        end
        dio_endwrite(dsh);
        fprintf('PUTTER has written data for time step %d\n', t);
    end
end

% send a single precision matrix
fprintf('\n--- Sending single %ix%i matrix ---\n', MAT_SIZE, MAT_SIZE);
matrix = rand(MAT_SIZE);
fprintf('matrix = \n\n');
fprintf('   %7.4f   %7.4f   %7.4f   %7.4f    ...\n', matrix(1:4, 1:4)');
fprintf('    ...       ...       ...       ...       ...\n\n');
fprintf('PUTTER writes matrix.\n');
if dio_startwrite(dsh)
    dio_write(dsh, single(matrix));
    dio_endwrite(dsh);
    fprintf('PUTTER has written matrix.\n\n');
end

% clean up connection
fprintf('PUTTER is closing DelftIO connection.\n');
dio_delete(dsh);
fprintf('PUTTER is finished.\n');
