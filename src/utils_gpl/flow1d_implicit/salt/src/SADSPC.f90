subroutine sadspc (dsopt  ,nbran  ,ngrid  ,maxtab ,ntabm ,g      ,&
&time   ,dispf  ,branch ,ntab   ,table ,thcsum ,&
&grid   ,x      ,wf     ,q2     ,c     ,csa1   ,&
&cdcdx1 ,cdcdx2 ,distmp ,disgr  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADSPC (SAlt DiSPersion Coefficient)
!
! Module description: Calculate a dispersion coefficient for each point
!                     in the network.
!
!                     The dispersion coefficient can be calculated in
!                     four ways. This routine calls the processing rou-
!                     tine for the selected dispersion formulation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 branch            P  -
! 17 c                 P  -
! 19 cdcdx1            P  -
! 20 cdcdx2            P  -
! 18 csa1              P  -
! 22 disgr             P  -
!  8 dispf             P  -
! 21 distmp            P  -
!  1 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  6 g                 P  -
! 13 grid              P  -
!  4 maxtab            I  Maximum number of defined tables.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 10 ntab              P  -
!  5 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 16 q2                P  -
! 11 table             P  -
! 12 thcsum            P  -
!  7 time              P  -
! 15 wf                P  -
! 14 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! sadslc  SAlt DiSpersion Linear Coefficient
! sadsst  SAlt DiSpersion Spatial or Time dependent
! sadsth  SAlt DiSpersion Thatcher Harl. or zwendl
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sadspc.pf,v $
! Revision 1.5  1995/10/18  09:00:19  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/30  12:37:16  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:06  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:59  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:41  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:08  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:35  kuipe_j
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
   integer   dsopt ,nbran ,ngrid ,maxtab   ,ntabm
   integer   dispf (2,3)  ,branch(4,nbran) ,ntab (4,maxtab) ,&
   &grid  (ngrid)
   real      g
   real      table (ntabm)  ,thcsum(2,nbran),x     (ngrid)  ,&
   &wf    (ngrid)  ,c     (ngrid)  ,&
   &csa1  (ngrid)  ,cdcdx1(ngrid)  ,cdcdx2(ngrid)  ,&
   &distmp(ngrid)  ,disgr (ngrid)
   double    precision       time           ,q2    (ngrid)
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   if      (dsopt .eq. cds1fu) then
!
      call sadsst (ngrid ,maxtab ,ntabm  ,time   ,dispf  ,ntab   ,&
      &table ,disgr )
!
   else if (dsopt .eq. cds2fu) then
!
      call sadslc (nbran  ,ngrid ,maxtab ,ntabm  ,time   ,dispf  ,&
      &branch ,ntab  ,table  ,distmp ,x      ,csa1   ,&
      &grid   ,disgr )
!
   else if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
!
      call sadsth (nbran  ,ngrid ,maxtab ,ntabm  ,g      ,time   ,&
      &dispf  ,branch,ntab   ,table  ,thcsum ,wf     ,&
      &q2     ,c     ,cdcdx1 ,cdcdx2 ,distmp ,disgr  )
!
   endif
!
end
