subroutine KAAWAR(istru  ,strpar ,sign   ,h0     ,ka     ,kp     ,&
&n      ,p      ,wn     ,zs     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAAWAR (KAlman get Advanced Weir Arguments)
!
! Module description: Parameters for a advanced weir are extracted from
!                     array strpar.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 h0                O  -
!  1 istru             I  Number of structure.
!  5 ka                O  -
!  6 kp                O  -
!  7 n                 O  -
!  8 p                 O  -
!  3 sign              I  Flow direction (+/-).
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
!  9 wn                O  -
! 10 zs                O  -
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaawar.pf,v $
! Revision 1.3  1999/03/15  15:51:32  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:36  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:12  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer istru
   real    strpar(dmstrpar,*)
   real    sign, h0, ka, kp, n, p, wn, zs
!
!     Determine crest height, total net width and number of piers
!     -----------------------------------------------------------
!
   zs = strpar(1,istru)
   wn = strpar(2,istru)
   n  = strpar(3,istru)
!
!     Determine P, H0, Kp and Ka
!     (flow direction dependent)
!
   if ( sign .gt. 0.0 ) then
      p  = strpar( 4,istru)
      h0 = strpar( 5,istru)
      kp = strpar( 6,istru)
      ka = strpar( 7,istru)
   else
      p  = strpar( 8,istru)
      h0 = strpar( 9,istru)
      kp = strpar(10,istru)
      ka = strpar(11,istru)
   endif
!
end
