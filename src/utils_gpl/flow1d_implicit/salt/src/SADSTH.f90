subroutine sadsth (nbran  ,ngrid  ,maxtab ,ntabm  ,g      ,time  ,&
&dispf  ,branch ,ntab   ,table  ,thcsum ,wf    ,&
&q2     ,c      ,cdcdx1 ,cdcdx2 ,distmp ,disgr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADSTH (SAlt DiSpersion Thatcher Harl. or zwendl)
!
! Module description: Calculate a dispersion coefficient for each point
!                     in the network (Option 3 and 4).
!                     [ Doc: S-FO-001.5KV / Eq. 19-8/9 ]
!
!                     The dispersion formulation uses three functions
!                     which can be a time function or several constants
!                     in place.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 14 c(ngrid)          I  Actual Chezy coefficient for total channel in
!                         every grid point.
! 15 cdcdx1(ngrid)     I  Contains for every grid point the averaged
!                         value of term <c/c0*dc/dx> over the last tide.
! 16 cdcdx2(ngrid)     I  Contains for every grid point the averaged
!                         value of term <c/c0*dc/dx> over the tide befo-
!                         re the last tide.
! 18 disgr(ngrid)      IO Dispersion coefficient in every grid point at
!                         time t(n+1).
!  7 dispf(2,3)        I  Depending on the chosen dispersion formulation
!                         (dsopt) one, two or three functions are used.
!                         A functions can be a function of time for the
!                         whole network or a function of place
!                         (constants for each grid point). Each (first)
!                         index indicates a function number. For each
!                         function number the type of function is sto-
!                         red.
!                         (1,1) = Type of function for f1:
!                                 cdsftm (1) : function of time
!                                 cdsfpl (2) : function of place
!                         (2,1) = Table pointer to table structure.
!                         (1,2) = Type of function for f2.
!                         (2,2) = Table pointer to table structure.
!                         (1,3) = Type of function for f3.
!                         (2,3) = Table pointer to table structure.
! 17 distmp(ngrid)     IO Scratch array for dispersion calculation.
!  5 g                 I  Acceleration of gravity.
!  3 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  9 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
!  4 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 13 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 10 table(ntabm)      I  Contains all table values:
!                         An example for table 2 is given below:
!                         ntab (1,2)    <---->
!                         ntab (3,2)-------------+
!                         ntab (2,2)----+        |
!                                       |        |
!                                       v        v
!                         table | <1> | x2..x2 | y2..y2 | ... | <n> |
!                         -------------------------------------------
!                         - Fourier or tidal components:
!                         In this case the x-table with length ntab(1,k)
!                         contains the following information:
!                         1     : Avarage A0.
!                         2     : Number of harmonics  or tide frequen-
!                                 cies n.
!                         - For harmonics only:
!                         3     : Base frequency W0.
!                         - For every harmonic component:
!                         2+n*2 : Amplitude Ai.
!                         3+n*2 : Phase GAMMAi.
!                         - For every tidel components:
!                         n*3   : Frequency Wi.
!                         1+n*3 : Amplitude Ai.
!                         2+n*3 : Phase GAMMAi.
! 11 thcsum(2,nbran)   I  Contains Thatcher-Harleman sum per branch:
!                         (1,i) = Constant part of Thatcher-Harleman
!                                 sum.
!                         (2,i) = Part of Thatcher-Harleman sum that is
!                                 constant in one tidal period.
!  6 time              P  -
! 12 wf(ngrid)         I  Actual flow width at every grid point.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
! sadsst  SAlt DiSpersion Spatial or Time dependent
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sadsth.pf,v $
! Revision 1.4  1995/10/18  09:00:20  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:08  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:01  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:43  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:11  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:40  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer   nbran ,ngrid  ,maxtab  ,ntabm
   integer   dispf (2,3)   ,branch(4,nbran) ,ntab (4,maxtab)
   real      g
   real      table (ntabm) ,thcsum(2,nbran) ,wf    (ngrid)  ,&
   &c     (ngrid)   ,cdcdx1(ngrid)  ,&
   &cdcdx2(ngrid) ,distmp(ngrid)   ,disgr (ngrid)
   double    precision      time            ,q2    (ngrid)
!
!     Declaration of local variables
!
   integer   igr  ,igrt  ,itab ,inc  ,ibr ,i1 ,i2
   real      f3   ,f4    ,sum  ,sqrtg
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Calculate f1(x,t)
!
   call sadsst (ngrid ,maxtab ,ntabm ,time ,dispf ,ntab ,&
   &table ,disgr )
!
!     Calculate f3(x,t)
!
   itab = dispf(2,2)
   if (dispf(1,2) .eq. cdsftm) then
!
!        Time series.
!
      call inttab (ntab (1,itab)      ,ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &time               ,f3          )
      do 10 igr = 1,ngrid
         distmp(igr) = f3
10    continue
   else
!
!        Place function.
!        (Constant if table length = 1)
!
      igrt = ntab(3,itab)
      inc  = min(ntab(1,itab)-1,1)
      do 20 igr = 1,ngrid
         distmp(igr) = table(igrt)
         igrt        = igrt + inc
20    continue
   endif
!
!     Multiply f3(x,t) with d(x,t)*srt(g)/c(x,t)*abs(u(x,t))
!     and add to f1(x,t)
!
   sqrtg = sqrt(g)
   do 30 igr = 1,ngrid
!                     f1(x,t)      f3(x,t)
      disgr(igr) = disgr(igr) + distmp(igr) * abs(q2(igr)) *  sqrtg /&
      &(wf(igr) * c(igr))
30 continue
!
   itab = dispf(2,3)
   if (dispf(1,3) .eq. cdsftm) then
!
!        Time series.
!
      call inttab (ntab (1,itab)      ,ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &time               ,f4          )
      do 40 igr = 1,ngrid
         distmp(igr) = f4
40    continue
   else
!
!        Place function.
!        (Constant if table length = 1)
!
      igrt = ntab(3,itab)
      inc  = min(ntab(1,itab)-1,1)
      do 50 igr = 1,ngrid
         distmp(igr) = table(igrt)
         igrt        = igrt + inc
50    continue
   endif
!
!     Add with Thather Harleman sum
!
   do 70 ibr = 1,nbran
      sum = .5 * thcsum(2,ibr)
      i1  = branch(3,ibr)
      i2  = branch(4,ibr)
!
!        Calculation per grid point.
      do 60 igr = i1,i2
!                                           f4(x,t)
         disgr(igr) = disgr(igr) + sum * distmp(igr) *&
         &(cdcdx1(igr) + cdcdx2(igr))
60    continue
!
70 continue
!
end
