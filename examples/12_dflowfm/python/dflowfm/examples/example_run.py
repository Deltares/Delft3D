from dflowfm import model

fm = model(lib_name=r'../../../bin/Win32/Debug/unstruc.dll', run_dir=r'../../../test_data/flow_salt_2d')

fm.initialize(config_file='2d.mdu')

fm.update(1)

n_variables = fm.get_n_variables()

print 'Variables: '
for i in range(n_variables):
    print '  ' + fm.get_variable_name(i)

fm.finalize()

