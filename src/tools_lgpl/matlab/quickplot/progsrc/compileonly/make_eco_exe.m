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
        files = {'ecoplot.m'};
        if matlabversionnumber<=7.10
           files{end+1} = 'wl_identification.c';
        end
        mcc(appopt{:},'-a','./units.ini','-a','./qp_icons.mat','-a','./grib','-v',files{:})
end