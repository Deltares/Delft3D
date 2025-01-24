subroutine FLGTAR (istru  ,strpar ,sign   ,zs     ,wstr   ,&
&w2     ,wsd    ,zb2    ,dg     ,ds1    ,&
&ds2    ,cgf    ,cgd    ,cwf    ,cwd    ,&
&mugf   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLGTAR (FLow get General sTructure ARguments)
!
! Module description: Parameters for the general structure are extracted
!                     from the packed array strpar.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 cgd               O  Correction coefficient for drowned gate flow.
! 12 cgf               O  Correction coefficient for free gate flow.
! 15 cwd               O  Correction coefficient for drowned weir flow.
! 14 cwf               O  Correction coefficient for free weir flow.
!  9 dg                O  Gate opening height.
! 10 ds1               O  Delta s1 general structure.
! 11 ds2               O  Delta s2 general structure.
!  1 istru             I  Number of structure.
! 16 mugf              O  Contraction coefficient for free gate flow.
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
!  6 w2(ngrid)         IO W2 coefficient of momentum equation
!  6 w2                IO Width at right side of structure.
!  6 w2(ngrid)         IO Total width at (n+theta2) in every grid point.
!  7 wsd               O  Width structure right or left side.
!  5 wstr              O  Width at centre of structure.
!  8 zb2               IO Bed level at right side of structure.
!  4 zs                IO Bed level at centre of structure.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flgtar.pf,v $
! Revision 1.8  1999/03/15  15:49:55  kuipe_j
! tabs removed
!
! Revision 1.7  1996/04/12  13:03:48  kuipe_j
! headers, minor changes
!
! Revision 1.6  1996/01/17  14:38:25  kuipe_j
! header update
!
! Revision 1.5  1995/10/18  08:59:20  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/09/22  10:01:22  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:55:00  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:58  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:43  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:52  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
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
   double precision  sign   ,zs     ,wstr   ,w2     ,wsd    ,zb2    ,&
   &dg     ,ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,&
   &cwd    ,mugf
!
!     Declaration of local variables:
!
   double precision  w1     ,zb1    ,wsdl   ,zbsl   ,wsdr   ,zbsr   ,&
   &help
!
!     Fetch parameters from structure info array
!
   w1   = strpar( 1,istru)
   zb1  = strpar( 2,istru)
   wsdl = strpar( 3,istru)
   zbsl = strpar( 4,istru)
   wstr = strpar( 5,istru)
   zs   = strpar( 6,istru)
   wsdr = strpar( 7,istru)
   zbsr = strpar( 8,istru)
   w2   = strpar( 9,istru)
   zb2  = strpar(10,istru)
   dg   = strpar(11,istru)
!
!     Determine cgf, cgd, cwf, cwd, mugf
!     (flow direction dependent)
!
   if ( sign .gt. 0.0D0 ) then
      cgf  = strpar(12,istru)
      cgd  = strpar(13,istru)
      cwf  = strpar(14,istru)
      cwd  = strpar(15,istru)
      mugf = strpar(16,istru)
   else
      cgf  = strpar(17,istru)
      cgd  = strpar(18,istru)
      cwf  = strpar(19,istru)
      cwd  = strpar(20,istru)
      mugf = strpar(21,istru)
   endif
!
!     Determine flow direction dependent parameters
!
   if ( sign .gt. 0.0D0 ) then
      wsd  = wsdr
      ds1  = zs - zbsr
      ds2  = zbsr - zb2
   else
      wsd  = wsdl
      ds1  = zs - zbsl
      ds2  = zbsl - zb1
      help = w1
      w1   = w2
      w2   = help
      help = zb1
      zb1  = zb2
      zb2  = help
   endif
!
end
