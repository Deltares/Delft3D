      subroutine wqfigp ( ngrid  ,qaggr  ,igp    ,isecfr ,
     +                    isecto ,idir   ,qex    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFIGP (Water Quality Flow In GridPoint)
c
c Module description: This routine calculates the exchange flow in a
c                     particular gridpoint and section.
c
c                     The routine calculates two flows and determines a
c                     ratio distribution. After this the sign of the
c                     exchange flow will be determined in the following
c                     way:
c
c                         |---------|       |---------|
c                         |         |       |         |
c                     ----|  Seg 1  |---X---|  Seg 2  |----
c                         |         |       |         |
c                         |---------|       |---------|
c                                   -------->>
c                     positive branch direction
c
c                     If the pointer table contains an exchange FROM
c                     segment S1 TO segment S2 the exchange flow will
c                     have the sign of the flow calculated in the grid
c                     point X.
c
c                     If the pointer table contains an exchange FROM
c                     segment S2 TO segment S1 the exchange flow will
c                     have the opposite sign of the flow calculated in
c                     the grid point X.
c
c                     The resulting sign of Qex is shown in the follo-
c                     wing table:
c
c                      sign (Q)  pointer direction   sign (Qex)
c                      --------  -----------------   ----------
c                        1                 1                 1
c                       -1                 1                -1
c                        1                -1                -1
c                       -1                -1                 1
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 idir              I  Direction of from --> to definition:
c                         +1 = from --> to in positive branch direction
c                         -1 = from --> to in negative branch direction
c  3 igp               P  -
c  4 isecfr            P  -
c  5 isecto            P  -
c  1 ngrid             I  Number of grid points in network.
c  2 qaggr             P  -
c  7 qex               IO Calculated exchange flow.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqgpfl  Water Quality GridPoint FLow
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
c $Log: wqfigp.pf,v $
c Revision 1.3  1999/03/12  12:35:00  kuipe_j
c parallel segments added
c
c Revision 1.2  1995/05/30  07:08:29  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:50  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer idir,
     +        igp,
     +        isecfr,
     +        isecto,
     +        ngrid

      real    qex

      real    qaggr (ngrid,3)

c
c     Variables
c
      real    qfrom, qto
c
c     Calculate flow from section
c
      call wqgpfl ( ngrid, qaggr, igp, isecfr, qfrom )

c
c     Calculate flow to section
c
      call wqgpfl ( ngrid, qaggr, igp, isecto,  qto   )
c
c     Calculate Qex
c     Two possibilities exist:
c     (1) connection between two separated or
c         two non-separated segments:
c         qfrom = qto = qex by definition
c     (2) connection between a separated and
c         a non-separated segment:
c         qfrom /= qto and qex = the smallest of the two
c
      qex = min( abs (qfrom) , abs(qto) )
c
c     Determine sign of exchange flow
c
      qex = sign (1,idir) * sign (1.0,qfrom+qto) * qex

      return
      end
