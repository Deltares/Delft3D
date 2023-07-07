function windsetup;

close all;

x       = [-12:0.125000000:(25*0.125000000-12)];
y       = [ 48:0.083333333:(60*0.083333333+48)];

xb      = [ -9.5; -11.5];
yb      = [ 48.5;  50.0];

fidu    = fopen('windtest.amu');
for i=1:18;
    tline    = fgetl(fidu);
end
for i=1:61;
    tline    = fgetl(fidu);
    u1(i,:)  = str2num(tline);
end
fclose all;

fidv    = fopen('windtest.amv');
for i=1:18;
    tline    = fgetl(fidv);
end
for i=1:61;
    tline    = fgetl(fidv);
    v1(i,:)  = str2num(tline);
end
fclose all;

lw      = 2;

u1 = flipud(u1);
v1 = flipud(v1);

quiver(x,y,u1,v1,3); 
line([xb(1) xb(2)],[yb(1) yb(1)],'color','k','linewidth',lw);
line([xb(1) xb(2)],[yb(2) yb(2)],'color','k','linewidth',lw);
line([xb(1) xb(1)],[yb(1) yb(2)],'color','k','linewidth',lw);
line([xb(2) xb(2)],[yb(1) yb(2)],'color','k','linewidth',lw);
daspect([1 1 1]);