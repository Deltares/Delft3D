This is a python wrapper for DFLOW-FM model (FORTRAN).




    # unfinished    
    def _test_grid(self):
        grid = dflowfm.Grid.frombmi(self.fm)
        grid = gridlib.Grid.frombmi(self.fm, vars={'x':'doemaarx','y':'doemaareeny'})
        
        grid = gridlib.Grid()
        grid.x = self.fm.variables['x']
        grid.y = self.fm.variables['y']

        m = fm.bmi

        m.get_vars
        m.attributes

        s0 = bmi.varaibles['s0']

        # asdfasdf
        grid = fm.grid

        for bc in fm.boundary_conditions:
            print bc.type # rieman, neuman .... 
            print bc.data # ...
            print bc.geometry

        time_series = fm.bc[0].data

        for b in fm.boundaries:
            print b.geometry

        for s in fm.structures:
            print s.name    
            print s.geometry    

        plot(fm.grid)
        plot(fm.boundaries)
    def test_lalala():
        1/0
