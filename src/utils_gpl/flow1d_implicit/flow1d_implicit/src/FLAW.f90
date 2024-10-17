subroutine FLAW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,&
&strpar ,h      ,h1     ,q      ,q2     ,af     ,&
&rho    ,strhis ,asde   ,csde   ,esde   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAW (FLow structure Advanced Weir)
!
! Module description: In subroutine FLAW the ABCDE coefficients for the
!                     stage-discharge equation are computed for an
!                     advanced weir.
!
!                     In subroutine FLQHAW the Q-H relation for the
!                     advanced weir is defined.
!
!                     The following items can be controlled:
!
!                     Wn = Width across flow section
!                     Zs = Crest level of weir
!
!                     The following input parameters are available for
!                     this type of structure:
!
!                     -   Level of crest Zs
!                     -   Total net width Wn
!                     -   Number of piers N
!                     -   Heigth of upstream face P
!                     -   Design head H0 of the weir
!                     -   Pier contraction coefficient Kp
!                     -   Abutment contraction coefficient Ka
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 af                P  -
! 14 asde              O  a-coefficient in stage-discharge equation for
!                         this specific structure.
! 15 csde              O  c-coefficient in stage-discharge equation for
!                         this specific structure.
! 16 esde              O  e-coefficient in stage-discharge equation for
!                         this specific structure.
!  1 g                 P  -
!  9 h1                P  -
!  8 h                 P  -
!  2 il                P  -
!  3 ir                P  -
!  5 istru             I  Number of structure.
!  4 ngrid             I  Number of grid points in network.
! 10 q                 P  -
! 12 rho               P  -
!  6 strclo            P  -
! 13 strhis(10,nstru)  O  For each structure the discharge and the
!                         parameters to be controlled must be saved to
!                         be able to write to the output file. This will
!                         be done in array strhis(8,nstru). This array
!                         will also be used to check the values of the
!                         controlled parameters or to determine if
!                         increase(open) or decrease(close) of these
!                         parameters occurs. This array will also be
!                         part of the restart file.
!                         (1,i) = Gate height
!                         (2,i) = Crest height
!                         (3,i) = Crest width
!                         (4,i) = Discharge through structure
!                         (5,i) = Gate height at previous time step
!                         (6,i) = Crest height at previous time step
!                         (7,i) = Crest width at previous time step
!                         (8,i) = Flow condition of general structure:
!                                 formno = 0, closed or other structure
!                                 formno = 1, free weir
!                                 formno = 2, drowned weir
!                                 formno = 3, free gate
!                                 formno = 4, drowned gate
!                         (9,i) = coefficient Q-H-realtion asde
!                         (10,i)= coefficient Q-H-realtion bsde
!                         (11,i)= coefficient Q-H-realtion csde
!                         (12,i)= coefficient Q-H-realtion dsde
!  7 strpar(21,nstru)  I  Each structure is characterized by a number of
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
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  FLow UP- and DOwnstream near structure
! kaawar
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flaw.pf,v $
! Revision 1.14  1999/03/15  15:49:30  kuipe_j
! tabs removed
!
! Revision 1.13  1998/02/13  12:12:29  kuipe_j
! Adapt to CMT
!
! Revision 1.12  1997/11/26  14:54:31  kuipe_j
! diffusion zero for free flow + width zero allowed
!
! Revision 1.11  1997/10/03  06:39:34  kuipe_j
! criterium for flow drection changed
!
! Revision 1.10  1996/04/12  13:03:39  kuipe_j
! headers, minor changes
!
! Revision 1.9  1996/04/11  08:23:09  kuipe_j
! Kalman module added
!
! Revision 1.8  1996/01/17  14:38:12  kuipe_j
! header update
!
! Revision 1.7  1995/09/22  10:00:55  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:47  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:24  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:11  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:54:44  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:40  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:27  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:29  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Declaration of function:
!
   real    FLQHAW
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid
   logical strclo(*)
   real    strpar(dmstrpar,*), strhis(dmstrh,*)
   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   real    af(ngrid)
   real    rho(ngrid)
   real    asde, csde, esde, g
!
!     Declaration of local variables:
!
   integer iup, idown
   logical strsta
   real    hunp1, hdnp1, hun, hdn, uu, ud, dh, qa, qdhu, qdhd, teken
   real    h0, ka, kp, n, p, wn, zs
   real    dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, dum9,&
   &dum10, dum11, dum12, dum13, wideff,hcrfre
   real    crest

   crest = strpar(1 ,istru)
!
   call FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,&
   &q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,&
   &hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,&
   &idown  ,teken  )
!
!     Determine crest height, total net width and number of piers
!     Determine P, H0, Kp and Ka (flow direction dependent)
!
!
!      call KAAWAR(istru  ,strpar ,teken  ,h0     ,ka     ,kp     ,
!     +            n      ,p      ,wn     ,zs     )
!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQHAW(g      ,istru  ,strsta ,strclo ,hunp1  ,&
   &hdnp1  ,uu     ,h0     ,ka     ,kp     ,&
   &n      ,p      ,wn     ,zs     ,wideff ,&
   &dum2   ,dum3   ,dum4   ,dum5   ,dum6   ,&
   &dum7   ,dum8   ,dum9   ,dum10  ,dum11  ,&
   &dum12  ,dum13  )
!
   strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1+dh,&
      &hdnp1  ,uu     ,h0     ,ka     ,kp      ,&
      &n      ,p      ,wn     ,zs     ,dum1    ,&
      &dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,&
      &dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,&
      &dum12  ,dum13  )

      qdhu = ( qdhu - qa ) / dh
   else
      qdhu = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1   ,&
      &hdnp1-dh,uu     ,h0     ,ka     ,kp      ,&
      &n      ,p      ,wn     ,zs     ,dum1    ,&
      &dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,&
      &dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,&
      &dum12  ,dum13  )
      qdhu = ( qdhu + qa ) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1   ,&
      &hdnp1-dh,uu     ,h0     ,ka     ,kp      ,&
      &n      ,p      ,wn     ,zs     ,dum1    ,&
      &dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,&
      &dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,&
      &dum12  ,dum13  )
      qdhd = ( qa - qdhd ) / dh
   else
      qdhd = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1+dh,&
      &hdnp1  ,uu     ,h0     ,ka     ,kp      ,&
      &n      ,p      ,wn     ,zs     ,dum1    ,&
      &dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,&
      &dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,&
      &dum12  ,dum13  )
      qdhd = ( -qdhd - qa)/dh
   endif
!
!     Calculate if structure is drowned or free
!
   if (wideff .le. 1.e-6) then
      strclo(istru) = .true.
   endif
   if (strclo(istru)) then
      strhis(8,istru) = 0.
   else
      hcrfre = (hunp1 + uu**2/2./g - zs) * .6666666
      if (hcrfre .lt. (hdnp1-zs)) then
!            drowned weir flow
         strhis(8,istru) = 2.
      else
!            free weir flow
         strhis(8,istru) = 1.
      endif
   endif
!
   asde = qdhu
!
   csde = qdhd
!
   if (teken .gt. 0) then
      esde = -qa + (hunp1-hun) * qdhu + (hdnp1-hdn) * qdhd
   else
      esde = -qa + (hdnp1-hdn) * qdhu + (hunp1-hun) * qdhd
   endif
!
end
