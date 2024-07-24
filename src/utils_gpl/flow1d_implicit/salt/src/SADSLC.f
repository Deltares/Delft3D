      subroutine sadslc (nbran  ,ngrid ,maxtab ,ntabm  ,time   ,dispf ,
     &                   branch ,ntab  ,table  ,distmp ,x      ,csa1  ,
     &                   grid   ,disgr  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADSLC (SAlt DiSpersion Linear Coefficient)
c
c Module description: Calculate a dispersion coefficient for each point
c                     in the network (Option 2).
c                     [ Doc: S-FO-001.5KV / Eq. 19-7 ]
c
c                     The dispersion formulation uses two functions
c                     which can be a time function or several constants
c                     in place.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 branch            P  -
c 12 csa1              P  -
c 14 disgr(ngrid)      IO Dispersion coefficient in every grid point at
c                         time t(n+1).
c  6 dispf(2,3)        I  Depending on the chosen dispersion formulation
c                         (dsopt) one, two or three functions are used.
c                         A functions can be a function of time for the
c                         whole network or a function of place
c                         (constants for each grid point). Each (first)
c                         index indicates a function number. For each
c                         function number the type of function is sto-
c                         red.
c                         (1,1) = Type of function for f1:
c                                 cdsftm (1) : function of time
c                                 cdsfpl (2) : function of place
c                         (2,1) = Table pointer to table structure.
c                         (1,2) = Type of function for f2.
c                         (2,2) = Table pointer to table structure.
c                         (1,3) = Type of function for f3.
c                         (2,3) = Table pointer to table structure.
c 10 distmp(ngrid)     IO Scratch array for dispersion calculation.
c 13 grid              P  -
c  3 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  8 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c  4 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  9 table(ntabm)      I  Contains all table values:
c                         An example for table 2 is given below:
c                         ntab (1,2)    <---->
c                         ntab (3,2)-------------+
c                         ntab (2,2)----+        |
c                                       |        |
c                                       v        v
c                         table | <1> | x2..x2 | y2..y2 | ... | <n> |
c                         -------------------------------------------
c                         - Fourier or tidal components:
c                         In this case the x-table with length ntab(1,k)
c                         contains the following information:
c                         1     : Avarage A0.
c                         2     : Number of harmonics  or tide frequen-
c                                 cies n.
c                         - For harmonics only:
c                         3     : Base frequency W0.
c                         - For every harmonic component:
c                         2+n*2 : Amplitude Ai.
c                         3+n*2 : Phase GAMMAi.
c                         - For every tidel components:
c                         n*3   : Frequency Wi.
c                         1+n*3 : Amplitude Ai.
c                         2+n*3 : Phase GAMMAi.
c  5 time              P  -
c 11 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c sadcdx  SAlt DC/DX calculation
c sadsst  SAlt DiSpersion Spatial or Time dependent
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sadslc.pf,v $
c Revision 1.5  1995/10/18  09:00:18  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/30  12:37:15  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:06  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:58  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:40  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:07  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:33  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer   nbran ,ngrid  ,maxtab  ,ntabm
      integer   dispf (2,3)   ,branch(4,nbran) ,ntab (4,maxtab)
      integer   grid  (ngrid)
      real      table (ntabm) ,distmp(ngrid)   ,x    (ngrid)   ,
     &          csa1  (ngrid) ,disgr (ngrid)
      double    precision      time
c
c     Declaration of local variables
c
      integer   igr  ,igrt  ,itab ,inc
      real      f2
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Calculate f1(x,t)
c
      call sadsst (ngrid ,maxtab ,ntabm  ,time ,dispf ,ntab  ,
     &             table ,disgr )
c
c     Calculate dc/dx
c                                                              dc/dx
      call sadcdx (nbran ,ngrid  ,branch ,grid   ,csa1 ,x     ,distmp)
c
c     Multiply abs(dc/dx) with f2(x,t)
c
      itab = dispf(2,2)
      if (dispf(1,2) .eq. cdsftm) then
c
c        Time series.
c
         call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                table(ntab(2,itab)),
     &                table(ntab(3,itab)),
     &                time               ,f2          )
         do 10 igr = 1,ngrid
c                                  dc/dx
            distmp(igr) = f2 * abs(distmp(igr))
   10    continue
      else
c
c        Place function.
c        (Constant if table length = 1)
c
         inc  = min(ntab(1,itab)-1,1)
         igrt = ntab(3,itab)
         do 20 igr = 1,ngrid
c                                           dc/dx
            distmp(igr) = table(igrt) * abs(distmp(igr))
            igrt        = igrt + inc
   20    continue
      endif
c
c     Add to f1(x,t)
c
      do 30 igr = 1,ngrid
c                     f1(x,t)      f2(x,t)*abs(dc/dx)
         disgr(igr) = disgr(igr) + distmp(igr)
   30 continue
c
      end
