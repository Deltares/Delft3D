subroutine FLPP (il     ,ir     ,ngrid  ,istru  ,strclo ,strpar ,&
&h      ,h1     ,maxtab ,ntabm  ,ntab   ,table  ,&
&strhis ,asde   ,csde   ,esde   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLPP (FLow structure PumP)
!
! Module description: In subroutine FLPP the ABCDE coefficients are
!                     computed for a pump.
!
!                     In this subroutine the coefficients A2-E2 will be
!                     computed for the stage-discharge equation for this
!                     specific structure. In subroutine FLQHPP the Q-H
!                     relation for the pump is defined.
!
!                     The pump is controlled by the upward or downward
!                     water level. By definition the upward side is the
!                     side from the beginning of the branch, the down-
!                     ward side points to the end of the branch.
!
!                     The reduction factor of the pump capacity can be
!                     given as a function of the head.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 asde              O  a-coefficient in stage-discharge equation for
!                         this specific structure.
! 15 csde              O  c-coefficient in stage-discharge equation for
!                         this specific structure.
! 16 esde              O  e-coefficient in stage-discharge equation for
!                         this specific structure.
!  8 h1(ngrid)         I  Water level in every grid point at time t(n).
!  7 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  1 il                I  Grid point on left side of structure (lower
!                         index).
!  2 ir                I  Grid point on right side of structure (upper
!                         index).
!  4 istru             I  Number of structure.
!  9 maxtab            I  Maximum number of defined tables.
!  3 ngrid             I  Number of grid points in network.
! 11 ntab              P  -
! 10 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  5 strclo            P  -
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
!  6 strpar(21,nstru)  I  Each structure is characterized by a number of
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
! 12 table             P  -
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flpp.pf,v $
! Revision 1.9  1999/03/15  15:50:26  kuipe_j
! tabs removed
!
! Revision 1.8  1998/02/13  12:12:30  kuipe_j
! Adapt to CMT
!
! Revision 1.7  1997/11/26  14:54:33  kuipe_j
! diffusion zero for free flow + width zero allowed
!
! Revision 1.6  1996/01/17  14:38:43  kuipe_j
! header update
!
! Revision 1.5  1995/09/22  10:02:06  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:47  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:19  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:19  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:00  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:23  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of function:
!
   real    FLQHPP
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid, maxtab, ntabm, ntab(4,maxtab)
   logical strclo(*)
   real    strpar(dmstrpar,*), strhis(dmstrh,*)
   double precision h(ngrid), h1(ngrid)
   real    table(ntabm)
   real    asde, csde, esde
!
!     Declaration of local variables:
!
   logical strsta
   integer itab ,iup  ,idown
   real    hact ,dh   ,qa    ,qdhu ,qdhd  ,cap  ,capold ,hstart ,&
   &hstop, teken
   double precision hup  ,hdown
!
!     Assign value strpar to local variable:
!
   call FLPPAR(istru  ,strpar ,ngrid  ,h1     ,il     ,ir     ,&
   &itab   ,cap    ,capold ,hstart ,hstop  ,&
   &iup    ,idown  ,teken  ,hact   )
!
   dh = 0.01
!
   strsta = .true.
   qa = FLQHPP (istru  ,strsta ,strclo ,h(iup) ,h(idown) ,&
   &hact   ,itab   ,cap    ,capold ,hstart ,hstop    ,&
   &maxtab ,ntabm  ,ntab   ,table  ,teken  )
!
!
!     Save old capacity in strpar
!     Function return value is discharge Q for pump
!
   strpar(6,istru) = capold
   strhis(4,istru) = qa
   strhis(8,istru) = 1.
   strsta = .false.
!
!     Differentiation to i-left
!
   if (teken .gt. 0.) then
      hup   = h(iup)   + dh
      hdown = h(idown)
   else
      hup   = h(iup)
      hdown = h(idown) - dh
   endif
   qdhu = FLQHPP (istru   ,strsta ,strclo ,hup    ,hdown  ,&
   &hact    ,itab   ,cap    ,capold ,hstart ,hstop  ,&
   &maxtab  ,ntabm  ,ntab   ,table  ,teken)
!
   qdhu = (qdhu - qa)/dh*teken
!
!
!     Differentiation to i-right
!
   if (teken .gt. 0.) then
      hup   = h(iup)
      hdown = h(idown) - dh
   else
      hup   = h(iup)   + dh
      hdown = h(idown)
   endif
   qdhd = FLQHPP (istru   ,strsta ,strclo ,hup    ,hdown  ,&
   &hact    ,itab   ,cap    ,capold ,hstart ,hstop  ,&
   &maxtab  ,ntabm  ,ntab   ,table  ,teken  )
!
   qdhd = (qa - qdhd)/dh*teken
!
   asde = qdhu
!
   csde = qdhd
!
   esde = -qa + ( h(il)-h1(il) )*qdhu + ( h(ir)-h1(ir) )*qdhd
!
end
