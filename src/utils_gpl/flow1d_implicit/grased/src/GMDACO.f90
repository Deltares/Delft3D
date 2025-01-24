subroutine GMDACO ( start  ,igpm1  ,igp    ,igpp1  ,deltaa,&
&daacor ,x      ,ws     ,zbave  ,zbfl  ,&
&zbeps  ,nvast  ,ngrid  ,nfrac  ,ncfl  ,&
&nonngp ,wf     ,deltar ,jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         C. Flokstra
!
! Module:             GMDACO
!
! Module description: Calculate correction on delta A to prevent
!                     negative frequencies or erosion below fixed layer.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 29 ws                P  -
! 21 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmarea.F,v $
! Revision 1.4  1996/01/08  13:29:33  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.3  1996/01/05  15:43:18  kuipe_j
! Lateral sediment and structures
!
! Revision 1.2  1995/09/27  10:11:25  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!
!     Parameters
!

   integer   nfrac  ,igpm1  ,igp    ,igpp1  ,nonngp  ,nvast  ,&
   &ngrid  ,ncfl   ,jugralg
   real      zbeps
   real      x      (ngrid),&
   &ws     (ngrid),&
   &deltar (ngrid,nfrac) ,&
   &zbave  (*),&
   &zbfl   (*),&
   &daacor (*),&
   &wf     (ngrid)
   double precision deltaa (ngrid,nfrac+1)
   logical   start
!
!     Local variables
!
   integer   icf    ,nmet1

   real      dx     ,wsact  ,dzt  ,zbavmi  ,dztma  ,dzfac  ,&
   &daacrh ,dzta ,dztb
   double precision sum, sumd
!
!     Adapt daa to prevent negative frequencies
!
!
   if (start) then
      do 10 icf=1,nfrac+1
         daacor(icf) = 0.0
10    continue
      ncfl= 0
   endif

   if ((nonngp.ne.1).and.(nvast.ne.1)) return
!
!     Adapt cross section adaptation area due to upstream correction
!
   dx = 0.5 * (x(igpp1) - x(igpm1))
   if (ncfl.eq.1) then
      sum = 0.0
      do 20 icf=1,nfrac
         deltaa(igp,icf) = deltaa(igp,icf) + daacor(icf)/dx
         sum = sum + deltaa(igp,icf)
         daacor(icf) = 0.0
20    continue
      deltaa(igp,nfrac+1) = sum
      ncfl= 0
   endif
!
   do 25 icf=1,nfrac
      if ( deltaa(igp,icf) .gt.0.0 ) goto 28
25 continue
!
!  no erosion
!
   return
!
28 continue
!
   if (nvast.eq.1) then
!
! correctie voor door vaste laag schieten
!
      if (wf(igp).lt.ws(igp)) then
         wsact = wf(igp)
      else
         wsact = ws(igp)
      endif
      dzt = -deltaa(igp,nfrac+1) / wsact
      zbavmi = zbfl(igp)+zbeps
      if (zbave(igp)+dzt .lt. zbavmi) then
         write(jugralg,*) ' below fix. l. ',igp,zbave(igp),dzt,zbavmi
         nmet1= 2
!
         if (nmet1.eq.1) then
! nmet1 = 1
            dztma = zbave(igp) - zbavmi
            if (dztma.gt.0.0) then
               dzfac = 1.0 + dztma / dzt
               do 30 icf=1,nfrac+1
                  daacor(icf) = dzfac * deltaa(igp,icf)
                  deltaa(igp,icf)= deltaa(igp,icf) - daacor(icf)
30             continue
            else
               do 35 icf=1,nfrac+1
                  daacor(icf) = deltaa(igp,icf)
                  deltaa(igp,icf)= 0.0
35             continue
            endif
!
         else
! nmet1=2
            dzta= 0.0
            dztb= 0.0
            do 29 icf=1,nfrac
               if (deltaa(igp,icf).gt.0.0) then
                  dzta = dzta + deltaa(igp,icf)
               else
                  dztb = dztb + deltaa(igp,icf)
               endif
29          continue
            dzta  = -dzta / wsact
            dztb  = -dztb / wsact
            dztma = zbave(igp) + dztb - zbavmi
            if (dztma.ge.0.0) then
               dzfac = 1.0 + dztma / dzta
            else
               dzfac = 1.0
               write(jugralg,*) 'gmdaco : below zbeps '
            endif
            do 31 icf=1,nfrac
               if (deltaa(igp,icf).gt.0.0) then
                  daacor(icf) = dzfac * deltaa(igp,icf)
                  deltaa(igp,icf)= deltaa(igp,icf) - daacor(icf)
               endif
31          continue
            sum  = 0D0
            sumd = 0D0
            do 32 icf=1,nfrac
               sum  = sum  + daacor(icf)
               sumd = sumd + deltaa(igp,icf)
32          continue
            daacor(nfrac+1)    = sum
            deltaa(igp,nfrac+1)= sumd
!           else
!              do 36 icf=1,nfrac+1
!                 daacor(icf) = deltaa(igp,icf)
!                 deltaa(igp,icf)= 0.0
! 36           continue
!           endif
! einde nmet1
         endif
         ncfl= 1
      endif
   endif
!
! correctie voor negatieve fracties
!
   if (nonngp.eq.1) then
      do 40 icf=1,nfrac
         if (deltaa(igp,icf) .gt. deltar(igp,icf))then
            daacrh      = deltaa(igp,icf) - deltar(igp,icf)
            daacor(icf) = daacor(icf) + daacrh
            deltaa(igp,icf) = deltar(igp,icf)
            deltaa(igp,nfrac+1) = deltaa(igp,nfrac+1) - daacrh
            ncfl= 1
         endif
40    continue
   endif
!
   if (ncfl.eq.1) then
      do 50 icf=1,nfrac
         daacor(icf) = daacor(icf) * dx
50    continue
   endif
!
   return
end
