subroutine wqfigp ( ngrid  ,qaggr  ,igp    ,isecfr ,&
&isecto ,idir   ,qex    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFIGP (Water Quality Flow In GridPoint)
!
! Module description: This routine calculates the exchange flow in a
!                     particular gridpoint and section.
!
!                     The routine calculates two flows and determines a
!                     ratio distribution. After this the sign of the
!                     exchange flow will be determined in the following
!                     way:
!
!                         |---------|       |---------|
!                         |         |       |         |
!                     ----|  Seg 1  |---X---|  Seg 2  |----
!                         |         |       |         |
!                         |---------|       |---------|
!                                   -------->>
!                     positive branch direction
!
!                     If the pointer table contains an exchange FROM
!                     segment S1 TO segment S2 the exchange flow will
!                     have the sign of the flow calculated in the grid
!                     point X.
!
!                     If the pointer table contains an exchange FROM
!                     segment S2 TO segment S1 the exchange flow will
!                     have the opposite sign of the flow calculated in
!                     the grid point X.
!
!                     The resulting sign of Qex is shown in the follo-
!                     wing table:
!
!                      sign (Q)  pointer direction   sign (Qex)
!                      --------  -----------------   ----------
!                        1                 1                 1
!                       -1                 1                -1
!                        1                -1                -1
!                       -1                -1                 1
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 idir              I  Direction of from --> to definition:
!                         +1 = from --> to in positive branch direction
!                         -1 = from --> to in negative branch direction
!  3 igp               P  -
!  4 isecfr            P  -
!  5 isecto            P  -
!  1 ngrid             I  Number of grid points in network.
!  2 qaggr             P  -
!  7 qex               IO Calculated exchange flow.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqgpfl  Water Quality GridPoint FLow
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
! $Log: wqfigp.pf,v $
! Revision 1.3  1999/03/12  12:35:00  kuipe_j
! parallel segments added
!
! Revision 1.2  1995/05/30  07:08:29  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:50  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer idir,&
   &igp,&
   &isecfr,&
   &isecto,&
   &ngrid

   real    qex

   real    qaggr (ngrid,3)

!
!     Variables
!
   real    qfrom, qto
!
!     Calculate flow from section
!
   call wqgpfl ( ngrid, qaggr, igp, isecfr, qfrom )

!
!     Calculate flow to section
!
   call wqgpfl ( ngrid, qaggr, igp, isecto,  qto   )
!
!     Calculate Qex
!     Two possibilities exist:
!     (1) connection between two separated or
!         two non-separated segments:
!         qfrom = qto = qex by definition
!     (2) connection between a separated and
!         a non-separated segment:
!         qfrom /= qto and qex = the smallest of the two
!
   qex = min( abs (qfrom) , abs(qto) )
!
!     Determine sign of exchange flow
!
   qex = sign (1,idir) * sign (1.0,qfrom+qto) * qex

   return
end
