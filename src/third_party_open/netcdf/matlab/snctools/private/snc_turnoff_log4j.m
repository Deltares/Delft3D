function snc_turnoff_log4j()
% These next few steps are needed to stop annoying warning messages
% from log4j.

% Problems have been reported with multiple calls to this function, and we
% should only need to do it once.  So if this function has been called for
% the first time in a matlab session, go ahead and make the java calls to
% turn off logging.  If we detect that it's the 2nd, 3rd, or nth time being
% called, we shouldn't have to do anything.  Just return.
persistent first_time
if isempty(first_time)
    first_time = 0;
else
    return
end

if exist('org.apache.log4j.BasicConfigurator','class')
    org.apache.log4j.BasicConfigurator.configure();
    level = org.apache.log4j.Level.OFF;
    logger = org.apache.log4j.Logger.getRootLogger();
    logger.setLevel(level);
end
