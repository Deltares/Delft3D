subroutine sadslc (nbran  ,ngrid ,maxtab ,ntabm  ,time   ,dispf ,&
&branch ,ntab  ,table  ,distmp ,x      ,csa1  ,&
&grid   ,disgr  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADSLC (SAlt DiSpersion Linear Coefficient)
!
! Module description: Calculate a dispersion coefficient for each point
!                     in the network (Option 2).
!                     [ Doc: S-FO-001.5KV / Eq. 19-7 ]
!
!                     The dispersion formulation uses two functions
!                     which can be a time function or several constants
!                     in place.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 branch            P  -
! 12 csa1              P  -
! 14 disgr(ngrid)      IO Dispersion coefficient in every grid point at
!                         time t(n+1).
!  6 dispf(2,3)        I  Depending on the chosen dispersion formulation
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
! 10 distmp(ngrid)     IO Scratch array for dispersion calculation.
! 13 grid              P  -
!  3 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  8 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  9 table(ntabm)      I  Contains all table values:
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
!  5 time              P  -
! 11 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
! sadcdx  SAlt DC/DX calculation
! sadsst  SAlt DiSpersion Spatial or Time dependent
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sadslc.pf,v $
! Revision 1.5  1995/10/18  09:00:18  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/30  12:37:15  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:06  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:58  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:40  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:07  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:33  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer   nbran ,ngrid  ,maxtab  ,ntabm
   integer   dispf (2,3)   ,branch(4,nbran) ,ntab (4,maxtab)
   integer   grid  (ngrid)
   real      table (ntabm) ,distmp(ngrid)   ,x    (ngrid)   ,&
   &csa1  (ngrid) ,disgr (ngrid)
   double    precision      time
!
!     Declaration of local variables
!
   integer   igr  ,igrt  ,itab ,inc
   real      f2
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Calculate f1(x,t)
!
   call sadsst (ngrid ,maxtab ,ntabm  ,time ,dispf ,ntab  ,&
   &table ,disgr )
!
!     Calculate dc/dx
!                                                              dc/dx
   call sadcdx (nbran ,ngrid  ,branch ,grid   ,csa1 ,x     ,distmp)
!
!     Multiply abs(dc/dx) with f2(x,t)
!
   itab = dispf(2,2)
   if (dispf(1,2) .eq. cdsftm) then
!
!        Time series.
!
      call inttab (ntab (1,itab)      ,ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &time               ,f2          )
      do 10 igr = 1,ngrid
!                                  dc/dx
         distmp(igr) = f2 * abs(distmp(igr))
10    continue
   else
!
!        Place function.
!        (Constant if table length = 1)
!
      inc  = min(ntab(1,itab)-1,1)
      igrt = ntab(3,itab)
      do 20 igr = 1,ngrid
!                                           dc/dx
         distmp(igr) = table(igrt) * abs(distmp(igr))
         igrt        = igrt + inc
20    continue
   endif
!
!     Add to f1(x,t)
!
   do 30 igr = 1,ngrid
!                     f1(x,t)      f2(x,t)*abs(dc/dx)
      disgr(igr) = disgr(igr) + distmp(igr)
30 continue
!
end
