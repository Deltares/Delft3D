switch computer
    otherwise
        if isunix
            appopt={'-m'}; % add '-C' to keep ctf in separate file
        else
            appopt={'-e'};
        end
        if matlabversionnumber>=7.04
           cleanup({'*_r13_6p5.*'})
        end
        files = {'d3d_qp.m'};
        if matlabversionnumber<=7.10
           files{end+1} = 'wl_identification.c';
        end
        %
        % Including wl_identification.c doesn't seem to work in R2011a (maybe also not in R2010b)
        % The help suggests that c code should be first compiled as mex file before it can be included
        % but the mex file will be included in the zipped ctf archive and thus not accessible for the
        % version reader tool. Need to find a different solution...
        % The documentation suggests to compile matlab part as library and to use it in c project, but
        % I don't want to write a generic wrapper for d3d_qp just to include version number string.
        %
        mcc(appopt{:},'-a','./units.ini','-a','./qp_icons.mat','-a','./grib','-v',files{:})
end