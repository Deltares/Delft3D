subroutine FLGS (g      ,il     ,ir     ,iter   ,ngrid  ,istru  ,&
&lambda ,relstr ,dhstru ,strclo ,strpar ,h      ,&
&h1     ,q      ,q1     ,q2     ,af     ,wf     ,&
&lsalt  ,rho    ,strhis ,asde   ,bsde   ,csde   ,&
&dsde   ,esde   ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLGS (FLow General Structure)
!
! Module description: In subroutine FLGS the ABCDE coefficients are
!                     computed for a General Structure.
!
!                     In this subroutine the coefficients A2-E2 will be
!                     computed for the stage-discharge equation for this
!                     specific structure. In subroutine FLQHGS the Q-H
!                     relation for the general structure is defined.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 16 af                P  -
! 21 asde              IO a-coefficient in stage-discharge equation for
!                         this specific structure.
! 22 bsde              IO b-coefficient in stage-discharge equation for
!                         this specific structure.
! 23 csde              IO c-coefficient in stage-discharge equation for
!                         this specific structure.
!  9 dhstru            I  Delta h used for numerical differentation.
! 24 dsde              IO -
! 25 esde              O  e-coefficient in stage-discharge equation for
!                         this specific structure.
!  1 g                 I  Acceleration of gravity.
! 13 h1                P  -
! 12 h                 P  -
!  2 il                I  Grid point on left side of structure (lower
!                         index).
!  3 ir                P  -
!  6 istru             I  Number of structure.
!  4 iter              I  Iteration step.
!  7 lambda            I  Extra resistance in general structure.
! 18 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  5 ngrid             I  Number of grid points in network.
! 15 q1                P  -
! 14 q                 P  -
!  8 relstr            I  Under relaxation factor for structures.
! 19 rho(ngrid)        I  Density of diluted water per grid point.
! 10 strclo            P  -
! 20 strhis(10,nstru)  IO For each structure the discharge and the
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
! 11 strpar(21,nstru)  I  Each structure is characterized by a number of
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
! 17 wf                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flgtar  FLow get General sTructure ARguments
! flupdg  FLow UP/Downstream near General structure
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flgs.pf,v $
! Revision 1.14  1999/03/15  15:49:51  kuipe_j
! tabs removed
!
! Revision 1.13  1997/11/06  09:16:40  kuipe_j
! Linearization original again
!
! Revision 1.12  1997/10/03  06:38:31  kuipe_j
! linearization compounds improved, changed flow dir. crit
!
! Revision 1.11  1996/05/30  09:56:37  kuipe_j
! general structure dlim, controllers
!
! Revision 1.10  1996/04/11  08:23:18  kuipe_j
! Kalman module added
!
! Revision 1.9  1996/01/17  14:38:21  kuipe_j
! header update
!
! Revision 1.8  1995/11/21  11:07:49  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/22  10:01:17  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:53  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:33  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:17  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:54:58  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:56  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:41  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:50  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of function:
!
   double precision FLQHGS
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
   include '..\include\erradm.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid, iter, juer, ker
   logical lsalt, strclo(*)
   real    strpar(dmstrpar,*), strhis(dmstrh,*)
   double precision h(ngrid),h1(ngrid),q(ngrid),q1(ngrid),q2(ngrid)
   real    af(ngrid),wf(ngrid),  rho(ngrid)
   real    asde, bsde, csde, dsde, esde, g
   real    lambda ,relstr ,dhstru
!
!     Declaration of local variables:
!
   integer           iup, idown, formno, lstnam
   logical           strsta
   double precision  hunp1 ,hdnp1 ,hun   ,hdn   ,uu  ,ud ,qa ,qdhu ,&
   &qdhd  ,teken ,wstr  ,zs    ,w2  ,zb2,dg ,&
   &cgf   ,cgd   ,cwf   ,cwd   ,mugf,wsd,ds1,ds2  ,&
   &rhoast,crest ,as    ,bs  ,cs  ,ds,&
   &qun   ,qunp1 ,wu    ,au    ,qdqu,&
   &gdub  ,lamdub,reldub,dhsdub,dum1
   real              astr, abran, hcrit, hstr
   character*8       abrtxt, astrtxt
   character*40      strnam
!
   integer     qol
   parameter  (qol=4)

   LOGICAL uitput

   COMMON  /UITPUT/uitput

   include '..\include\errcod.i'

   gdub   = g
   lamdub = lambda
   reldub = relstr
   dhsdub = dhstru
!
   crest = strpar(6 ,istru)
!
   call FLUPDG (gdub   ,il     ,ir     ,ngrid  ,h      ,h1     ,&
   &q      ,q1     ,q2     ,af     ,wf     ,rho    ,&
   &crest  ,hunp1  ,hdnp1  ,hun    ,hdn    ,uu     ,&
   &ud     ,qun    ,qunp1  ,au     ,wu     ,iup    ,&
   &idown  ,teken  )
!
   call FLGTAR (istru  ,strpar ,teken  ,zs     ,wstr   ,&
   &w2     ,wsd    ,zb2    ,dg     ,ds1    ,&
   &ds2    ,cgf    ,cgd    ,cwf    ,cwd    ,&
   &mugf   )
!
   if (iter.eq.1) then
      if (nrerperr(neragenstr).lt.20) then
         abran = min(af(il),af(ir))
         hcrit = 0.6666667*(hunp1 + uu*uu/g*0.5 - zs)
         hstr  = min(h1(il),h1(ir))-zs
         hstr  = max(hstr,hcrit)
         astr  = min(hstr,sngl(dg))*wstr
         if (astr.gt.abran*1.01) then
            nrerperr(neragenstr) = nrerperr(neragenstr) + 1
            call getstr(istru,strnam,lstnam)
            ker = warnng
            write (abrtxt,'(f8.0)') abran
            write (astrtxt,'(f8.0)') astr
            call error (juer,'FLGS Area in gen structure @'//&
            &strnam(:lstnam)//'@(@'//astrtxt//'@ > area branch (@'&
            &//abrtxt//'@)',eflgsa, ker)
         endif
      endif
   endif
   if (istru.eq.-10) then
!JK   if ((istru.eq.3 .or. istru.eq.6).and.iter.gt.90) then
!JK   if (istru.gt.0) then
      WRITE (99,*) '==== ITER',iter
      WRITE (99,*) 'STRUC=',istru,'B=',wstr,'H=',zs,'GP=',il,&
      &'S=',teken
      WRITE (99,*) 'GATE =',dg
      uitput=.true.
   else
      uitput=.false.
   endif
!
   if ( lsalt ) then
      rhoast = rho(idown) / rho(iup)
   else
      rhoast = 1.0D0
   endif
!
   strsta = .true.
   qa = teken * FLQHGS (gdub   ,istru  ,strsta ,strclo ,hunp1  ,&
   &hdnp1  ,uu     ,zs     ,wstr   ,w2     ,&
   &wsd    ,zb2    ,dg     ,ds1    ,ds2    ,&
   &rhoast ,cgf    ,cgd    ,cwf    ,cwd    ,&
   &mugf   ,dum1   ,formno ,lamdub ,dhsdub )
   strsta = .false.
!
   qdhu = teken * FLQHGS(gdub  ,istru  ,strsta,strclo,hunp1+dhsdub ,&
   &hdnp1 ,uu     ,zs    ,wstr  ,w2   ,&
   &wsd   ,zb2    ,dg    ,ds1   ,ds2  ,rhoast ,&
   &cgf   ,cgd    ,cwf   ,cwd   ,mugf ,dum1   ,&
   &formno,lamdub,dhsdub )
   qdhu = ( qdhu - qa ) / dhsdub

   qdhd = teken * FLQHGS(gdub  ,istru  ,strsta,strclo,hunp1        ,&
   &hdnp1-dhsdub  ,uu    ,zs    ,wstr ,w2     ,&
   &wsd   ,zb2    ,dg    ,ds1   ,ds2  ,rhoast ,&
   &cgf   ,cgd    ,cwf   ,cwd   ,mugf ,dum1   ,&
   &formno,lamdub,dhsdub )
   qdhd = ( qa - qdhd ) / dhsdub

   qdqu = qdhu * uu/(au*gdub)
   qdhu = qdhu * (1.D0-uu**2*wu/(au*gdub))
!
   if (uitput) then
      WRITE (99  ,*) 'RHO-STER',istru,rhoast
      WRITE (99  ,*) 'Hu,Hd,U',hunp1,hdnp1,uu
      WRITE (99  ,*) 'IN STRUC qa,afg-qdhu,qdhd,  ',istru,qa,qdhu,qdhd
      WRITE (99  ,*) 'FORMNO= ',formno
   endif

   if (teken .gt. 0) then
      as    = qdhu
      bs    = qdqu
      cs    = qdhd
      ds    = 0.
   else
      as    = qdhd
      bs    = 0.
      cs    = qdhu
      ds    = qdqu
   endif
!
!     Underrelaxation of Q
!
   if (iter.gt.1) then
      qa = reldub*qa+(1.0D0 - reldub)*dble(strhis(qol,istru))
   endif

   asde = sngl(as)
   bsde = sngl(bs)
   csde = sngl(cs)
   dsde = sngl(ds)
   strhis(qol,istru) = sngl(qa)
!
   if (teken .gt. 0) then
      esde = sngl(-qa + (hunp1-hun) * as + (hdnp1-hdn) * cs&
      &+ (qunp1-qun) * (bs + ds))

   else
      esde = sngl(-qa + (hdnp1-hdn) * as + (hunp1-hun) * cs&
      &+ (qunp1-qun) * (bs + ds))
   endif
!
   strhis(8,istru)=formno
!
end
