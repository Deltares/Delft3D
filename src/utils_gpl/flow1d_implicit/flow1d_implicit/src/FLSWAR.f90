subroutine FLSWAR (istru  ,strpar ,sign   ,zs     ,wstr   ,&
&cw     ,slim   ,itab   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLSWAR (FLow get Simple Weir ARguments)
!
! Module description: Parameters for a simple weir are extracted from
!                     the packed array strpar.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 cw                O  Correction coefficient for weir flow.
!  1 istru             I  Number of structure.
!  8 itab              O  Table number.
!  3 sign              I  Flow direction (+/-).
!  7 slim              O  Submergence limit.
!  2 strpar(21,nstru)  I  Each structure is characterized by a number of
!                         specific parameters. strpar (i,j) = parameter
!                         i of structure j:
!                         - Simple weir:
!                         (1,j) = Crest height Zs.
!                         (2,j) = Crest width Ws.
!                              Positive flow:
!                         (3,j) = Correction coefficient cw.
!                         (4,j) = Submergence limit Slim.
!                         (5,j) = Table pointer for drowned reduction
!                                 curve f(h2/h1).
!                              Negative flow:
!                         (6,j) = Correction coefficient cw.
!                         (7,j) = Submergence limit Slim.
!                         (8,j) = Table pointer for drowned reduction
!                                 curve f(h2/h1).
!                         - Advanced weir:
!                         (1,j) = Crest height Zs.
!                         (2,j) = Total net width Wn.
!                         (3,j) = Number of piers N.
!                              Positive flow:
!                         (4,j) = Heigth of upstream face P.
!                         (5,j) = Design head H0 of the weir.
!                         (6,j) = Pier contraction coefficient Kp.
!                         (7,j) = Abutment contraction coefficient Ka.
!                              Negative flow:
!                         (8,j) = Heigth of upstream face P.
!                         (9,j) = Design head H0 of the weir.
!                         (10,j)= Pier contraction coefficient Kp.
!                         (11,j)= Abutment contraction coefficient Ka.
!                         - Pump:
!                         (1,j) = Control direction:
!                                 cpmpup (-1) : upward control
!                                 cpmpdw (+1) : downward control
!                         (2,j) = Table pointer for pump capacitity re-
!                                 duction factor.
!                         (3,j) = Capacity.
!                         (4,j) = Water level which starts pump.
!                         (5,j) = Water level which stops pump.
!                         - General structure:
!                         (1,j) = Width left side of structure W1.
!                         (2,j) = Bed level left side of structure Zb1.
!                         (3,j) = Width structure left side Wsdl.
!                         (4,j) = Bed left side of structure Zbsl.
!                         (5,j) = Width structure centre Ws.
!                         (6,j) = Bed level centre Zs.
!                         (7,j) = Width structure right side Wsdr.
!                         (8,j) = Bed right side of structure Zbsr.
!                         (9,j) = Width right side of structure W2.
!                         (10,j)= Bed level right side of structure Zb2.
!                         (11,j)= Gate opening heigth dg.
!                              Positive flow:
!                         (12,j)= Correction coefficient for free gate
!                                 flow cgf.
!                         (13,j)= Correction coefficient for drowned
!                                 gate flow cgd.
!                         (14,j)= Correction coefficient for free weir
!                                 flow cwf.
!                         (15,j)= Correction coefficient for drowned
!                                 weir flow cwd.
!                         (16,j)= Contraction coefficient for free gate
!                                 flow MU-gf.
!                              Negative flow:
!                         (17,j)= Correction coefficient for free gate
!                                 flow cgf.
!                         (18,j)= Correction coefficient for drowned
!                                 gate flow cgd.
!                         (19,j)= Correction coefficient for free weir
!                                 flow cwf.
!                         (20,j)= Correction coefficient for drowned
!                                 weir flow cwd.
!                         (21,j)= Contraction coefficient for free gate
!                                 flow MU-gf.
!  5 wstr              O  Width at centre of structure.
!  4 zs                O  Bed level at centre of structure.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flswar.pf,v $
! Revision 1.5  1999/03/15  15:50:52  kuipe_j
! tabs removed
!
! Revision 1.4  1995/09/22  10:02:19  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:55:31  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:32  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:11  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:39  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer istru, itab
   real    strpar(dmstrpar,*), sign, zs, wstr, cw, slim
!
!     Determine crest height and crest width
!
   zs   = strpar(1,istru)
   wstr = strpar(2,istru)
!
!     Determine Cw, Slim and table pointer
!     (flow direction dependent)
!
   if ( sign .gt. 0.0 ) then
      cw   = strpar(3,istru)
      slim = strpar(4,istru)
      itab = int(strpar(5,istru))
   else
      cw   = strpar(6,istru)
      slim = strpar(7,istru)
      itab = int(strpar(8,istru))
   endif
!
end
