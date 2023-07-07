netfile = 'index4_net.nc';
mapfile = 'index4_1d2d_map.nc';
hisfile = 'index4_1d2d_his.nc';
%%
netdata = unstruc.readNet(netfile);
mapdata = unstruc.readMap(mapfile);
hisdata = unstruc.readHis(hisfile);
%%
figure(1);clf;
unstruc.plotNet(netdata);

figure(2);clf;
unstruc.plotNet(mapdata, 'PlotType','flow');

figure(3);clf;
unstruc.plotHisStations(hisdata);
%unstruc.plotHisStations(hisdata, 'testldbfile.ldb')
