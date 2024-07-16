      function FLQH02 (g     , istru , nstru , strsta, strclo,
     &                 hup   , hdn   , uu    , width , le    ,
     &                 z     , length, alpha , lw    , ksa   ,
     &                 ks    , rloc  , wloc  , theta , cc    ,
     &                 beta  , bot   , qin   , teken , lbvin ,
     &                 lbvout)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow QH relation type 02 (open flume)
c
c Module description: Calculate discharge through structure. This 
c                     routine is taken from WENDY and altered to suit
c                     SOBEK. Functionality is unchanged. WENDY history
c                     is:
c                        Project: Construction-Module
c                        Programmer: G. van Driel / H. van Zanten
c                        Function: Discharge through structure type 2
c                        Updates: None
c                     Note: due to array length limitations, part of
c                     the functionality has been left out. This is to
c                     included later with the introduction of dynamic
c                     array allocation. See documentation for details.
cc
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flqh02.pf,v $
c Revision 1.3  1999/03/15  15:50:30  kuipe_j
c tabs removed
c
c Revision 1.2  1998/06/08  12:35:36  kuipe_j
c log added
c
c
c
c***********************************************************************
c
c Parameters:
c name     I/O     what
c g         P      acceleration due to gravity
c istru     I      structure number
c nstru     I      number of structures
c strsta    I      structure status
c strclo    I      structure status
c width     I      width of flume (W)
c le        I      bottom level at the entrance (Le)
c z         I      difference between inlet and outlet levels (z)
c length    I      length of flume (L)
c alpha     I      ratio width of approach section vs. W (alfa)
c lw        I      length of approach section (Lw)
c ksa       I      roughness of approach section (ksa)
c ks        I      roughness of flume section (ks)
c rloc      I      inlet/outlet rounding (r)
c wloc      I      inlet/outlet beveling (w)
c theta     I      bevel angle (theta)
c cc(1)     I      coefficient C1 (if = 0 then calculate)
c cc(2)     I      coefficient C2 (if = 0 then calculate)
c cc(3)     I      coefficient C3 (if = 0 then calculate)
c nflume    I      number of flumes (Nf) (=1)
c beta      I      correction factor (beta)
c bot       I      average bed level
c lbvin     I      inlet bevelled (T), round (F)
c lbvout    I      outlet bevelled (T), round (F)
c teken     I      flow direction
c qin       I      discharge
c
c***********************************************************************

c
c declare arguments
      integer teken , istru , nstru
      logical lbvin , lbvout, strsta, strclo(nstru)
      real    g     , hup   , uu    , width , le, hdn   ,
     &        z     , length, alpha , lw    , ksa   , ks    ,
     &        rloc  , wloc  , theta , cc(3) , beta  , bot   ,
     &        qin

c
c declare local variables
      real    slope , elu   , rle   , rlemax, q     , delhz ,
     &        delh1 , deltah, h1le  , codef , x     , x2    ,
     &        c0    , ckr   , w     , w2    , ckw   , y     ,
     &        y2    , r1    , yci   , ycrtm , ycrit , acrit ,
     &        rcrit , rr    , hulp  , fcroot, rnc   , scrit ,
     &        hf1i  , arg1  , qstr  , ski   , ai    , hfio  ,
     &        yco   , ro    , roi   , hf    , aco   , arg2  ,
     &        ao    , aomu  , arg3  , h2yci , ri    
      integer nflume, kflow , i     , kco1  

c
c declare functions
      real    FLQH02, FLFR1I, FLFRIO
      logical  EQUAL
      external EQUAL

c
c to avoid an error here in the code set nflume to 1
      nflume = 1

      if (teken .gt. 0) then
        rle = le
      else
        rle = le - z
      endif

      slope = z / length
      elu = hup + uu*uu / (2. * g)
      rlemax = MAX(le, (le - z))

      if (elu .le. z) then
        if (strsta) strclo(istru) = .true. 
      else
        if (strsta) strclo(istru) = .false.
      endif

      if (elu .lt. rlemax) then
        if (strsta) strclo(istru) = .true. 
        q = 0.
        qstr = 0.
        goto 10
      else
        q = qin
        if (strsta) strclo(istru) = .false.
      endif

      delhz  = elu - rle + z
      delh1  = elu - rle
      deltah = hdn - rle + z
      h1le   = hup - rle

c
c calculate defaults for c1, c2 and c3
      kco1 = 1
      do 20 i = 1, 3
        if (EQUAL(cc(i),0.)) then

c
c has cc(1) already been calculated?
          if (kco1 .eq. 0) then
            cc(i) = codef
          else
            x  = delh1 / width
            x2 = x * x

            if (x .le. 0.4) then
              c0 = 0.93
            else
              if (x .gt. 2.) then
                c0 = 0.7054
              else
                c0  = -.0694*x2 + .0265*x + .93
              endif
            endif

c
c calculate ckw and ckr
            if ((teken .gt. 0. .and. lbvin) .or.
     &         (teken .le. 0. .and. lbvout)) then
              ckr = 1.
              w   = wloc / width
              if (w .gt. 0.1192) w = .1192
              w2  = w * w
              ckw  = -6.25*w2 + 1.49*w
              ckw  = ckw * (theta / 30.)**1.7 + 1.
            else
              y    = rloc / width
              if (y .gt. 0.1645) y = .1645
              y2   = y * y
              ckr  = -7.813*y2 + 2.57*y + 1.
              ckw  = 1.
            endif

            cc(i) = c0 * ckr * ckw
            codef = cc(i)
            kco1  = 0
          endif
        endif
   20 continue

c
c the following if statement is no longer used.
c---  De eerste keer per iteratie worden SCRIT en het flowtype bepaald:
c      if (k .eq. 0) then

c
c calculate rcrit and acrit for scrit
        r1 = beta * (hup - bot)
        ri = h1le * width / (2.*h1le + width)
        hf1i = FLFR1I (width, q, lw, r1, ksa, alpha, ri, ks, h1le, ski)

        yci =  (elu - rle - hf1i) * 2./3.

c
c set criterium
        if (z .lt. 0.) then
          ycrtm = yci + (2./3.)*z
        else
          ycrtm = yci
        endif

        if (ycrtm .le. 0.) ycrtm = .0001

        if (yci .le. 0.) then
          yci = 0.0001
          q   = 0.
          qstr = 0.
          goto 10
        endif

        ycrit = yci
        acrit  = width * ycrit
        rcrit  = acrit / (width + 2.*ycrit)
        rr     = rcrit
        if (rr .lt. 0.1) rr = 0.1
        if (rr .lt. (10. * ks)) rr = 10. * ks

        if (rr .gt. rcrit) then
          rcrit = rr
          acrit = width * width * rcrit / (width - 2.*rcrit)
        endif

c
c calculate critical slope
        hulp  =  12 * rcrit / ks
        fcroot= 1. / (2 * ALOG10(hulp) )
        rnc    = rcrit**(1./6.) * fcroot / 8.86
        scrit  = (rnc * q / (acrit * rcrit**(2./3)))**2

c
c determine the flowtype
        if (z .lt. 0.) then
          h2yci = deltah / ycrtm
        else
          h2yci = deltah / (ycrtm + z)
        endif

        if (h2yci .ge. 1.) then
          kflow = 3
        else
          if (slope .gt. scrit) then
            kflow = 1
          else
            kflow = 2
          endif
        endif
c      endif

      goto (1,2,3,4) kflow

c
c flowtype 1: critical depth at inlet
 1    r1 = beta * (hup - bot)
      ri = h1le * width / (2. * h1le + width)
      hf1i = FLFR1I (width, q, lw, r1, ksa, alpha, ri, ks, h1le, ski)
      ycrit = yci
      acrit  = width * ycrit

c
c eqn II-10.7
      arg1 = 2. * g * (delh1 - ycrit - hf1i)

      if (arg1 .le. 0. ) then
        hf1i = 0
        arg1 = 2. * g * (delh1 - ycrit - hf1i)
      endif

      qstr  = nflume * cc(1) * acrit * SQRT(arg1)
      goto 10

c
c flowtype 2: critical depth at outlet
 2    r1 = beta * (hup - bot)
      ai = h1le * width
      ri = ai / (2.*h1le + width)
      hf1i = FLFR1I (width, q, lw, r1, ksa, alpha, ri, ks, h1le, ski)

c
c use initial value for yco, where hfiostart = l*q*q/ki/ki

      hfio = length * q * q / (ski * ski)
      yco  = 2./3. * (delhz - hf1i - hfio)

c
c calculate ro by iteration
      do 11 i = 1, 10
        ro = width * yco / (width + 2.*yco)
        ri = ai / (2*h1le + width)
        hfio = FLFRIO (width, q, length, ro, ks, yco, ri, ks, h1le, ski)
        yco  = 2./3. * (delhz - hf1i - hfio)
        roi  = width * yco / (width + 2.*yco)
        if (ABS(ro - roi) .lt. 0.0005 * roi) goto 12
   11 continue

   12 continue

      hf = hf1i + hfio
      yco  = (delhz - hf) * 2 / 3
      aco  = width * yco

c
c eqn II-11.12
      arg2 = 2. * g * (delhz - yco - hf)

      if (arg2 .le. 0.) then 
        hf = 0.
        arg2 = 2. * g * (delhz - yco - hf)
      endif

      q = nflume * cc(2) * aco * SQRT(arg2)

      goto  10

c
c flowtype 3: subcritical flow in flume
 3    r1 = beta * (hup - bot)
      ai = h1le * width
      ri = ai / (2.*h1le + width)

      ao = deltah * width
      ro = ao / (2.*deltah + width)
      hf1i = FLFR1I (width, q, lw, r1, ksa, alpha, ri, ks, h1le, ski)

      ri = ai / (2 * h1le + width)
      hfio = FLFRIO (width, q, length, ro, ks, deltah, ri, ks,
     &               h1le, ski)
      hf = hf1i + hfio
      aomu = cc(3) * ao

c
c eqn II-12.12
      arg3 = 2. * g * (hup + uu*uu / (2. * g) - hdn - hf)

      if (arg3 .le. 0.) then
        hf = 0.
        arg3 = 1.0e-6
      endif

      qstr = nflume * aomu * SQRT(arg3)
      goto 10

 4    continue
      qstr = 0.

 10   FLQH02 = qstr

      return
      end
