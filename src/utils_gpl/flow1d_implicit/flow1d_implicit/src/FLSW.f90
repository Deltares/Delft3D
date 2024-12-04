subroutine FLSW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,&
&strpar ,h      ,h1     ,q      ,q2     ,af     ,&
&maxtab ,ntabm  ,ntab   ,table  ,rho    ,strhis ,&
&iter   ,relstr ,asde   ,csde   ,esde   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer / J.Kuipers
!
! Module:             FLSW (FLow structure Simple Weir)
!
! Module description: In subroutine FLSW the ABCDE coefficients for the
!                     stage-discharge equation are computed for a simple
!                     weir.
!
!                     In subroutine FLQHSW the Q-H relation for the
!                     simple weir is defined.
!
!                     The following items can be controlled:
!
!                     Wstr = Width across flow section
!                     Zs   = Crest level of weir
!
!                     For the calculation of the velocity the flow from
!                     the previous iteration level and the flow area on
!                     level n+1/2 are used. This because differences
!                     will be small.
!
!                     The input parameters for the simple weir are the
!                     following:
!
!                     -   Level of crest Zs
!                     -   Crest width Wstr
!                     -   Correction coefficient cw
!                     -   Submergence limit sl
!                     -   Drowned flow reduction curve (stored in tabu-
!                         lar form)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 af                P  -
! 20 asde              IO a-coefficient in stage-discharge equation for
!                         this specific structure.
! 21 csde              IO c-coefficient in stage-discharge equation for
!                         this specific structure.
! 22 esde              O  e-coefficient in stage-discharge equation for
!                         this specific structure.
!  1 g                 P  -
!  9 h1                P  -
!  8 h                 P  -
!  2 il                P  -
!  3 ir                P  -
!  5 istru             I  Number of structure.
! 18 iter              I  Iteration step.
! 12 maxtab            I  Maximum number of defined tables.
!  4 ngrid             I  Number of grid points in network.
! 14 ntab              P  -
! 13 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 10 q                 P  -
! 19 relstr            I  Under relaxation factor for structures.
! 16 rho               P  -
!  6 strclo            P  -
! 17 strhis(10,nstru)  IO For each structure the discharge and the
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
! 15 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flswar  FLow get Simple Weir ARguments
! flupdo  FLow UP- and DOwnstream near structure
!=======================================================================
!
!     Declaration of function:
!
   real    FLQHSW
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid, maxtab, ntabm ,iter
   integer ntab(4,maxtab)
   logical strclo(*)
   real    strpar(dmstrpar,*), strhis(dmstrh,*)
   real    af(ngrid)
   real    rho(ngrid)
   real    table(ntabm)
   real    asde, csde, esde, g ,relstr
   real    wideff, hcrfre

   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   integer     qol
   parameter  (qol=4)
!
!     Declaration of local variables:
!
   integer iup, idown, itab
   logical strsta, wet, red, reda, redb
   real    hunp1, hdnp1, hun, hdn, uu, ud, dh, dq, qa,  teken
   real    zs, wstr, cw, slim ,relst1, dum1 ,qdh1, qdh2, qdhu, qdhd
   real    crest, fred,fred1, fred2, hda, hdb, qdh1q, qdh2q
   real    qq, dh2, theta, hua, hub, dh1, afactor, qtot
!
   crest = strpar(1 ,istru)
!
   call FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,&
   &q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,&
   &hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,&
   &idown  ,teken  )
!
   call FLSWAR(istru  ,strpar ,teken  ,zs     ,wstr   ,cw     ,&
   &slim   ,itab   )
!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQHSW (g      ,istru  ,strsta ,strclo ,hunp1  ,&
   &hdnp1  ,uu     ,ud     ,zs     ,wstr   ,&
   &cw     ,slim   ,itab   ,maxtab ,ntabm  ,&
   &ntab   ,table  ,fred   )
!
   strsta = .false.
!
!     Numerical differentiation by incrementing in waterlevels.
!
   qdhu = FLQHSW (g       ,istru  ,strsta ,strclo ,hunp1+dh,&
   &hdnp1   ,uu     ,ud     ,zs     ,wstr    ,cw    ,&
   &slim    ,itab   ,maxtab ,ntabm  ,ntab    ,table ,&
   &dum1    )
!
   qdhd = FLQHSW (g       ,istru  ,strsta ,strclo ,hunp1   ,&
   &hdnp1-dh,uu     ,ud     ,zs     ,wstr    ,cw    ,&
   &slim    ,itab   ,maxtab ,ntabm  ,ntab    ,table ,&
   &dum1    )
!
   if (teken .gt. 0) then
      qdh1 = ( qdhu - qa ) / dh
      qdh2 = ( qa - qdhd ) / dh
   else
      qdh1 = (  qdhd + qa ) / dh
      qdh2 = ( -qdhu - qa ) / dh
   endif
!
   fred1 = .10
   fred2 = .05
   if (fred.lt.fred1) then
!
!        Degeneration ahead (reduction factor --> 0), so
!        numerical differentiation will be done by incrementing in
!        discharge.
!
      dq = .1
!        ARS 7170 / Use discharge of this structure only.
!                   Change is made for compound structures.
      qtot = abs((q(il)+q(ir))) *.5
!
!        Discharge thru all simple weirs
      qq = max(qtot - abs(strhis(10,istru)),0.)
!
!        Ratio between wet area of this structure to wet area of
!        all simple weirs
      if (strhis(11,istru).gt.1.e-6) then
         afactor = strpar(10,istru)/strhis(11,istru)
      else
         afactor = 1.
      endif
!
!        Discharge thru this weir
      qq = qq * afactor
!
      dq = max(1.e-4*qq,0.1)
      hua = hunp1
      hub = hunp1
!
!        Numerical differentiation to downwards waterlevel
!
      call FLQHSWQ(g      ,hua    ,hda    ,qq     ,uu     ,&
      &ud     ,zs     ,wstr   ,cw     ,wet    ,&
      &itab   ,maxtab ,ntabm  ,ntab   ,table  ,&
      &reda   )
!
      if (wet) then
10       continue

         call FLQHSWQ(g      ,hub    ,hdb    ,qq+dq  ,uu     ,&
         &ud     ,zs     ,wstr   ,cw     ,wet    ,&
         &itab   ,maxtab ,ntabm  ,ntab   ,table  ,&
         &redb   )
         dh2 = hda - hdb
         if (redb) then
            if (dh2 .lt. 1.e-4) then
               dq = dq * 10.
               goto 10
            endif
         endif
      endif
!
      if (wet) then
         red = redb
         if (.not.redb.and.reda) then
!
            hua = hub
            hda = hdb
!
!              Reduction factor in one point < 1, so force in both
!              points the reduction factor = 1 by increasing the
!              discharge.
!
            call FLQHSWQ(g      ,hub    ,hdb    ,qq+dq*2,uu     ,&
            &ud     ,zs     ,wstr   ,cw     ,wet    ,&
            &itab   ,maxtab ,ntabm  ,ntab   ,table  ,&
            &red    )
         endif

         if (red) then
!
!               Reduction factor < 1, so numerical differentiation
!               to upwards waterlevel
!
            qdhu = FLQHSW (g     ,istru ,strsta ,strclo ,hua+dh2 ,&
            &hda   ,uu    ,ud     ,zs     ,wstr    ,&
            &cw    ,slim  ,itab   ,maxtab ,ntabm   ,&
            &ntab  ,table , dum1  )
            if (teken.gt.0.) then
               qdh1q = ( qdhu - qq ) / dh2
               qdh2q = -dq / dh2
            else
               qdh1q = dq / dh2
               qdh2q = ( qq - qdhu ) / dh2
            endif
         else
!
!              Reduction factor = 1, So the downwards differentiation
!              coefficients are zero.
!              This will very seldom occur.
!
            dh1 = hub - hua
            if (teken.gt.0.) then
               qdh1q = dq / dh1
               qdh2q = 0.
            else
               qdh1q = 0.
               qdh2q = -dq / dh1
            endif
         endif
         if (fred.lt.fred2) then
!
!              Only differentiation by incrementing in discharge.
!
            qdh1  = qdh1q
            qdh2  = qdh2q
            qa    = qq * teken
            hunp1 = hua
            hdnp1 = hda
         else
!
!              Transition zone
!
            theta = (fred -fred1) / (fred2 - fred1)
            qdh1  = (1.-theta) * qdh1  + theta * qdh1q
            qdh2  = (1.-theta) * qdh2  + theta * qdh2q
            qa    = (1.-theta) * qa    + theta * qq * teken
            hunp1 = (1.-theta) * hunp1 + theta * hua
            hdnp1 = (1.-theta) * hdnp1 + theta * hda
         endif
      endif
   endif
!
!     Calculate if structure is drowned or free
!
   wideff = wstr * cw
   if (wideff .le. 1.e-6) then
      strclo(istru) = .true.
   endif
   if (strclo(istru)) then
      strhis(8,istru) = 0.
   else
      hcrfre = (hunp1 +uu**2/2./g - zs) * .6666666
      if (hcrfre .lt. (hdnp1-zs)) then
!            drowned weir flow
         strhis(8,istru) = 2.
      else
!            free weir flow
         strhis(8,istru) = 1.
      endif
   endif
!
   asde = qdh1
   csde = qdh2
!
!     Underrelaxation of the A, C and E-coefficient
!
   if (iter.ne.1) then
      relst1 = 1.0 - relstr
      qa   = relstr*qa  +relst1*strhis(qol,istru)
   endif
   strhis(qol,istru) = qa
!
   if (teken .gt. 0) then
      esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde
   else
      esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde
   endif
!
end

subroutine flswarea (g      ,il     ,ir     ,ngrid  ,istru  ,&
&strpar ,h      ,h1     ,q      ,q2     ,&
&af     ,rho    )
!
!     Calculate wetted area in simple weir above crest
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid
   real    strpar(dmstrpar,*)
   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   real    af(ngrid)
   real    rho(ngrid)
   real    g
!
!     Declaration of local variables:
!
   integer iup,   idown, itab
   real    hunp1, hdnp1, hun, hdn,  uu, ud, teken
   real    crest, wstr,  cw,  slim, a
!
   crest = strpar(1 ,istru)
!
   call FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,&
   &q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,&
   &hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,&
   &idown  ,teken  )
!
   call FLSWAR(istru  ,strpar ,teken  ,crest  ,wstr   ,cw     ,&
   &slim   ,itab   )

   a                = (max(hunp1,hdnp1)-crest)*wstr*cw

   strpar(10,istru) = max(a,0.)

end
