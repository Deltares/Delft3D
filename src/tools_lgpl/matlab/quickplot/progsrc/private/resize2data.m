function [val,s,z] = resize2data(val,s,z,Ops)
if isfield(Ops,'extend2edge') && Ops.extend2edge
    [s,z,val] = face2surf(s,z,val);
    return
end
nH = size(val,1);
nV = size(val,2);
if size(s,1)==nH+1
    s = (s(1:end-1,:)+s(2:end,:))/2;
end
if size(s,2)==1
    s = repmat(s,[1,nV]);
elseif size(s,2)==nV+1
    s = (s(:,1:end-1)+s(:,2:end))/2;
end
if size(z,1)==nH+1
    z = (z(1:end-1,:)+z(2:end,:))/2;
elseif size(z,1)==nH-1
    z = (z([1 1:end],:)+z([1:end end],:))/2;
end
if size(z,2)==nV+1
    z = (z(:,1:end-1)+z(:,2:end))/2;
end