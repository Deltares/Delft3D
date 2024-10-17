subroutine FLPPAR(istru  ,strpar ,ngrid  ,h1     ,il     ,ir     ,&
&itab   ,cap    ,capold ,hstart ,hstop  ,&
&iup    ,idown  ,sign   ,hact   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLPPAR (FLow get PumP ARguments)
!
! Module description: Parameters for the pump are extracted
!                     from the packed array strpar.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 cap               O  -
! 10 capold            O  -
!  4 h1(ngrid)         I  Water level in every grid point at time t(n).
! 16 hact              O  Actual water level at gridpoint in branch.
! 11 hstart            IO -
! 12 hstop             IO -
! 14 idown             O  Index of gridpoint downstream the structure.
!  5 il                I  Grid point on left side of structure (lower
!                         index).
!  6 ir                I  Grid point on right side of structure (upper
!                         index).
!  1 istru             I  Number of structure.
!  8 itab              O  Table number.
! 13 iup               O  Index of gridpoint upstream the structure.
!  3 ngrid             I  Number of grid points in network.
! 15 sign              O  Flow direction (+/-).
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
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flppar.pf,v $
! Revision 1.3  1999/03/15  15:50:28  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:13  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:47  kuipe_j
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
   integer istru, ngrid, il, ir, itab, iup, idown
   real    strpar(dmstrpar,*)
   real    cap, capold, hstart, hstop, sign, hact
   double precision h1(ngrid)
!
!     Local variables
!
   integer condir
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Determine pump parameters: condir, itab, cap, capold, hstart/stop
!     -----------------------------------------------------------------
!
   condir = int(strpar(1,istru))
   itab   = int(strpar(2,istru))
   cap    = strpar(3,istru)
   hstart = strpar(4,istru)
   hstop  = strpar(5,istru)
   capold = strpar(6,istru)
!
   if ( (hstart .lt. hstop) .eqv. (condir .eq. cpmpdw) ) then
      sign  = 1.0
      iup   = il
      idown = ir
   else
      sign  = -1.0
      iup   = ir
      idown = il
   endif
!
   if      ( condir .eq. cpmpup ) then
      hact = sngl( h1(il) )
   else if ( condir .eq. cpmpdw ) then
      hact = sngl( h1(ir) )
   endif
!
end
