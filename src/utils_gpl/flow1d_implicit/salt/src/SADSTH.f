      subroutine sadsth (nbran  ,ngrid  ,maxtab ,ntabm  ,g      ,time  ,
     &                   dispf  ,branch ,ntab   ,table  ,thcsum ,wf    ,
     &                   q2     ,c      ,cdcdx1 ,cdcdx2 ,distmp ,disgr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADSTH (SAlt DiSpersion Thatcher Harl. or zwendl)
c
c Module description: Calculate a dispersion coefficient for each point
c                     in the network (Option 3 and 4).
c                     [ Doc: S-FO-001.5KV / Eq. 19-8/9 ]
c
c                     The dispersion formulation uses three functions
c                     which can be a time function or several constants
c                     in place.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 14 c(ngrid)          I  Actual Chezy coefficient for total channel in
c                         every grid point.
c 15 cdcdx1(ngrid)     I  Contains for every grid point the averaged
c                         value of term <c/c0*dc/dx> over the last tide.
c 16 cdcdx2(ngrid)     I  Contains for every grid point the averaged
c                         value of term <c/c0*dc/dx> over the tide befo-
c                         re the last tide.
c 18 disgr(ngrid)      IO Dispersion coefficient in every grid point at
c                         time t(n+1).
c  7 dispf(2,3)        I  Depending on the chosen dispersion formulation
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
c 17 distmp(ngrid)     IO Scratch array for dispersion calculation.
c  5 g                 I  Acceleration of gravity.
c  3 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  9 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c 13 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 10 table(ntabm)      I  Contains all table values:
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
c 11 thcsum(2,nbran)   I  Contains Thatcher-Harleman sum per branch:
c                         (1,i) = Constant part of Thatcher-Harleman
c                                 sum.
c                         (2,i) = Part of Thatcher-Harleman sum that is
c                                 constant in one tidal period.
c  6 time              P  -
c 12 wf(ngrid)         I  Actual flow width at every grid point.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c sadsst  SAlt DiSpersion Spatial or Time dependent
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sadsth.pf,v $
c Revision 1.4  1995/10/18  09:00:20  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:08  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:01  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:43  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:11  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:40  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer   nbran ,ngrid  ,maxtab  ,ntabm
      integer   dispf (2,3)   ,branch(4,nbran) ,ntab (4,maxtab)
      real      g
      real      table (ntabm) ,thcsum(2,nbran) ,wf    (ngrid)  ,
     &          c     (ngrid)   ,cdcdx1(ngrid)  ,
     &          cdcdx2(ngrid) ,distmp(ngrid)   ,disgr (ngrid)
      double    precision      time            ,q2    (ngrid)
c
c     Declaration of local variables
c
      integer   igr  ,igrt  ,itab ,inc  ,ibr ,i1 ,i2
      real      f3   ,f4    ,sum  ,sqrtg
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Calculate f1(x,t)
c
      call sadsst (ngrid ,maxtab ,ntabm ,time ,dispf ,ntab ,
     &             table ,disgr )
c
c     Calculate f3(x,t)
c
      itab = dispf(2,2)
      if (dispf(1,2) .eq. cdsftm) then
c
c        Time series.
c
         call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                table(ntab(2,itab)),
     &                table(ntab(3,itab)),
     &                time               ,f3          )
         do 10 igr = 1,ngrid
            distmp(igr) = f3
   10    continue
      else
c
c        Place function.
c        (Constant if table length = 1)
c
         igrt = ntab(3,itab)
         inc  = min(ntab(1,itab)-1,1)
         do 20 igr = 1,ngrid
            distmp(igr) = table(igrt)
            igrt        = igrt + inc
   20    continue
      endif
c
c     Multiply f3(x,t) with d(x,t)*srt(g)/c(x,t)*abs(u(x,t))
c     and add to f1(x,t)
c
      sqrtg = sqrt(g)
      do 30 igr = 1,ngrid
c                     f1(x,t)      f3(x,t)
         disgr(igr) = disgr(igr) + distmp(igr) * abs(q2(igr)) *  sqrtg /
     &                (wf(igr) * c(igr))
   30 continue
c
      itab = dispf(2,3)
      if (dispf(1,3) .eq. cdsftm) then
c
c        Time series.
c
         call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                table(ntab(2,itab)),
     &                table(ntab(3,itab)),
     &                time               ,f4          )
         do 40 igr = 1,ngrid
            distmp(igr) = f4
   40    continue
      else
c
c        Place function.
c        (Constant if table length = 1)
c
         igrt = ntab(3,itab)
         inc  = min(ntab(1,itab)-1,1)
         do 50 igr = 1,ngrid
            distmp(igr) = table(igrt)
            igrt        = igrt + inc
   50    continue
      endif
c
c     Add with Thather Harleman sum
c
      do 70 ibr = 1,nbran
         sum = .5 * thcsum(2,ibr)
         i1  = branch(3,ibr)
         i2  = branch(4,ibr)
c
c        Calculation per grid point.
         do 60 igr = i1,i2
c                                           f4(x,t)
            disgr(igr) = disgr(igr) + sum * distmp(igr) *
     &                   (cdcdx1(igr) + cdcdx2(igr))
   60    continue
c
   70 continue
c
      end
