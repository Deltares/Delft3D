%--------------------------------------------------------------------
%Examples
%--------------------------------------------------------------------
A = qp_data_resource('F:\test_cases\Delft3D-FLOW - TRIM - s33\data\trim-s33.dat');
disp(A)
A.vert.unit('ft^2/s').quantity(1).Data('k',3:6)

A = qp_data_resource('F:\test_cases\TRITON - 2D - autodetect\data\co01.dat');
disp(A)
A.velocity.quantity(1)

A = qp_data_resource('F:\test_cases\Delft3D-FLOW - TRIM - fl2 - Slib3D\data\trim-fl2.dat');
disp(A)
A.Location('conc').quantitynames

A = qp_data_resource('D:\2_WL_Proj\ZWS - Q\Q4082 - DVR\Ribb\2dom_03_graded_unif\trim-flume_d1.dat');
disp(A)
A.sed

%--------------------------------------------------------------------
%Draft design of the proposed object interface
%--------------------------------------------------------------------
%
%   $Id$
%
File = qpopen(file);
%
%      [Success,Domains   ]            = QP_GETDATA(FI,'domains')
File.LocationNames                                                         %#ok
D = File.Location(1);                                                      %#ok
D = File.Location(1:5);                                                    %#ok
D = File.Location('XX1');                                                  %#ok
D = File.Location('XX1','XX2');                                            %#ok
%
%      [Success,DataProps ]            = QP_GETDATA(FI,Domain)
%      [Success,DataProps ]            = QP_GETDATA(FI,Domain,DimMask)
%      [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain)
%      [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain,DimMask)
D.QuantityNames                                                            %#ok
File.Location(1).QuantityNames                                             %#ok
File.Location(1:5).QuantityNames                                           %#ok
File.Location('XX1').QuantityNames                                         %#ok
File.Location('XX1','XX2').QuantityNames                                   %#ok
File.QuantityNames                                                         %#ok
Q = D.Quantity(1);                                                         %#ok
Q = D.Quantity('turbulent energy');                                        %#ok
%
Q.NDimensions                                                              %#ok
Q.Dimensions                                                               %#ok
%      [Success,Size      ]            = QP_GETDATA(FI,Domain,DataFld,'size')
Q.Size                                                                     %#ok
Q.Size(1)                                                                  %#ok
Q.Size('Time','N')                                                         %#ok
%
%      [Success,SubFields ]            = QP_GETDATA(FI,Domain,DataFld,'subfields',F)
Q.NSubfields                                                               %#ok
Q.SubfieldsNames                                                           %#ok
Q.Subfields                                                                %#ok
Q.SubfieldValues(1)                                                        %#ok
Q.SubfieldValues('sfDim1')                                                 %#ok
%
%      [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times')
%      [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times',T)
Q.TimeDependent                                                            %#ok
Q.NTimes = Q.Size('Time');                                                 %#ok
Q.Times                                                                    %#ok
%
%      [Success,StNames   ]            = QP_GETDATA(FI,Domain,DataFld,'stations',S)
Q.NStations = Q.Size('Station');                                           %#ok
Q.Stationnames                                                             %#ok
Q.Stations                                                                 %#ok
ST = Q.Station(StName);
%
File.Location('XX1').Quantity('turbulent energy').Subfield(SF1,SF2).Data(T,ST,M,N,K)%#ok
File.Location('XX1').Quantity('turbulent energy').sfDim1(SF1).sfDim2(SF2).Data(T,ST,M,N,K)%#ok
Q.Subfield(SF1,SF2).Data(T,ST,M,N,K)                                       %#ok
Q.sfDim1(SF1).sfDim2(SF2).Data(T,ST,M,N,K)                                 %#ok
Q2 = Q.Subfield(SF1,SF2);                                                  %#ok
Q2 = Q.sfDim1(SF1).sfDim2(SF2);                                            %#ok
Q2.Data(T,ST,M,N,K)                                                        %#ok
%
%      [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%      [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%      [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%      [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
Q.Data(T,ST,M,N,K,SF1,SF2)                                                 %#ok
Q.Data('Time',T,'Station',ST,'sfDim1',SF1,'sfDim2',SF2)                    %#ok
Q.Data('Time',T,'Station',StName,'sfDim1',SF1,'sfDim2',SF2)
%
Q.Data('Time',T,'M',M,'N',N,'K',K,'sfDim1',SF1,'sfDim2',SF2)               %#ok
Q.Data('Time',T,'M',M,'N',N,'zValue',Z,'sfDim1',SF1,'sfDim2',SF2)
Q.Data('Time',T,'M',M,'N',N,'zRange',[Z1 Z2],'sfDim1',SF1,'sfDim2',SF2)
Q.Data('Time',T,'M',M,'N',N,'dzFromSurface',dZ,'sfDim1',SF1,'sfDim2',SF2)
Q.Data('Time',T,'M',M,'N',N,'dzRangeFromSurface',[dZ1 dZ2],'sfDim1',SF1,'sfDim2',SF2)
Q.Data('Time',T,'M',M,'N',N,'dzFromBottom',dZ,'sfDim1',SF1,'sfDim2',SF2)
Q.Data('Time',T,'M',M,'N',N,'dzRangeFromSurface',[dZ1 dZ2],'sfDim1',SF1,'sfDim2',SF2)
File.Location('XX1').Quantity('turbulent energy').Data('Time',T,'M',M,'N',N,'dzRangeFromSurface',[dZ1 dZ2],'sfDim1',SF1,'sfDim2',SF2)
File.Location('XX1').Quantity('turbulent energy').Subfield(SF1,SF2).Data('Time',T,'hIndex',{M,N},'dzRangeFromSurface',[dZ1 dZ2])
File('Location','XX1','Quantity','turbulent energy','sfDim1',SF1,'sfDim2',SF2,'Time',T,'hIndex',{M,N},'dzRangeFromSurface',[dZ1 dZ2])
File.Location('XX1').Quantity('turbulent energy').Subfield(SF1,SF2).Time(T).HIndex(M,N).dzRangeFromSurface([dZ1 dZ2])
%
Q.Data('IndexTime',T,'IndexM',M,'IndexN',N,'IndexK',K,'IndexSfDim1',SF1,'IndexSfDim2',SF2)
Q.Data('Time',T,'M',M,'N',N,'K',K,'sfDim1',SF1,'sfDim2',SF2)               %#ok
Q.Data('Time',T,'hIndexTrack',MN,'K',K,'sfDim1',SF1,'sfDim2',SF2)
Q.Data('Time',T,'xyLine',XY,'K',K,'sfDim1',SF1,'sfDim2',SF2)
%