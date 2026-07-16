subroutine sadsst (ngrid ,maxtab ,ntabm ,time ,dispf ,ntab ,&
&table ,disgr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADSST (SAlt DiSpersion Spatial or Time dependent)
!
! Module description: Calculate a dispersion coefficient for each point
!                     in the network (Option 1).
!                     [ Doc: S-FO-001.5KV / Eq. 19-6 ]
!
!                     The dispersion can be one time function or several
!                     constants in place.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 disgr(ngrid)      O  Dispersion coefficient in every grid point at
!                         time t(n+1).
!  5 dispf(2,3)        I  Depending on the chosen dispersion formulation
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
!  2 maxtab            I  Maximum number of defined tables.
!  1 ngrid             I  Number of grid points in network.
!  6 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  3 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  7 table(ntabm)      I  Contains all table values:
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
!  4 time              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
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
! $Log: sadsst.pf,v $
! Revision 1.3  1995/05/30  09:56:07  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:00  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:42  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:09  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:37  kuipe_j
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
   integer   ngrid ,maxtab  ,ntabm
   integer   dispf (2,3)    ,ntab (4,maxtab)
   real      table (ntabm)  ,disgr(ngrid)
   double    precision       time
!
!     Declaration of local variables
!
   integer   igr  ,igrt ,itab  ,inc
   real      f1
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   itab = dispf(2,1)
   if (dispf(1,1) .eq. cdsftm) then
!
!        Time series.
!
      call inttab (ntab (1,itab)      ,ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &time               ,f1          )
      do 10 igr = 1,ngrid
         disgr(igr) = f1
10    continue
   else
!
!        Place function.
!        (Constant if table length = 1)
!
      inc  = min(ntab(1,itab)-1,1)
      igrt = ntab(3,itab)
      do 20 igr = 1,ngrid
         disgr(igr) = table(igrt)
         igrt       = igrt + inc
20    continue
   endif
!
end
