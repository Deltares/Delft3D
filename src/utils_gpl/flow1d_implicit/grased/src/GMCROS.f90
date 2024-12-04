subroutine GMCROS (ibr    ,ngrid  ,nbran  ,nboun  ,nnode  ,nfrac ,&
&time   ,moropt ,nonngp ,nvast  ,zbeps ,&
&grid   ,branch ,node   ,maxtab ,ntabm  ,ntab  ,&
&table  ,mbdpar ,x      ,maxlev ,nlev   ,hlev  ,&
&wf     ,wft    ,ws     ,flwdir ,deltaa ,deltar,&
&daacor ,zbave  ,zbfl   ,jugralg,sumda  )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         J.Kuipers
!
! Module:             GMCROS (Graded Morphology adapt CROSs-sections)
!
! Module description: Routine GMAREA has calculated the changes in the
!                     areas in a branch. These calculated changes are
!                     used to adapt the cross sectional tables in routi-
!                     ne MOADCS. But prior to adaptation the change in
!                     area will be corrected for negative frequencies.
!                     Corrections in areas will be tranferred to the
!                     next downstream cross-section.
!
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log:$
!
!***********************************************************************
!
!
!     Parameters
!
   integer   ibr    ,maxlev ,maxtab ,moropt ,ntabm ,nboun ,&
   &ngrid  ,nnode  ,nbran  ,nfrac  ,nonngp,nvast ,&
   &jugralg
   integer   branch (4,nbran)       ,mbdpar (5,nboun)     ,&
   &nlev   (ngrid)         ,ntab   (4,maxtab)    ,&
   &node   (4,nnode)       ,&
   &flwdir (ngrid)         ,grid(ngrid)
   real      zbeps
   real      deltar (ngrid,nfrac) ,&
   &daacor (nfrac)         ,&
   &zbave  (ngrid)         ,zbfl   (ngrid)       ,&
   &sumda  (ngrid)         ,&
   &table  (ntabm)         ,&
   &wft    (ngrid,maxlev)  ,&
   &wf     (ngrid)         ,ws(ngrid)            ,&
   &x      (ngrid)
   double precision  time, hlev (ngrid,maxlev),&
   &deltaa (ngrid,nfrac+1)
!
!     Local variables
!
   integer   i1     ,i2     ,igp    ,igpm1  ,igpp1  ,ncfl  ,&
   &plus   ,mint   ,ia     ,ib     ,ic     ,jf    ,&
   &il     ,ir     ,ibou   ,ibout
   real      sum
   logical   prevst ,first  ,flwpos
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Read first and last grid point of branch
!
   i1 = branch(3,ibr)
   i2 = branch(4,ibr)
!
!
!     Determine direction of flow
!     Processing will be done in positive flow direction.
!
   plus = 0
   mint = 0
   do 10 igp=i1,i2
      plus = plus + max(flwdir(igp),0)
      mint = mint + min(flwdir(igp),0)
10 continue
   flwpos = plus .ge. mint

   if (flwpos) then
      ia = i1
      ib = i2
      ic = 1
   else
      ia = i2
      ib = i1
      ic = -1
   endif
!
!     Determine if left end of branch is a boundary with
!     condition z=f(t)
!
   il    = 0
   ibout = node(1,branch(1,ibr))
   if (ibout .gt. 1) then
      ibou = node(4,branch(1,ibr))
      if (mbdpar(1,ibou) .eq. cmbzft) then
         if (flwdir(i1) .ge. 0) then
            il = i1
         endif
      endif
   endif
!
!     Determine if right end of branch is a boundary with
!     condition z=f(t)
!
   ir    = 0
   ibout = node(1,branch(2,ibr))
   if (ibout .gt. 1) then
      ibou = node(4,branch(2,ibr))
      if (mbdpar(1,ibou) .eq. cmbzft) then
         if (flwdir(i2) .le. 0) then
            ir = i2
         endif
      endif
   endif
!
   prevst = .false.
   first  = .true.
!
   do 20 igp = ia,ib,ic
      if (.not.(grid(igp).eq.2.and.prevst)) then
!
!           Correction of areas for negative frequencies
!           except for boundary points with z=f(t) condition.
!
!
         if (igp .ne. il .and. igp .ne. ir) then
            igpp1 = min (igp+1,i2)
            igpm1 = max (igp-1,i1)
            call GMDACO(first ,igpm1 ,igp   ,igpp1  ,deltaa ,daacor,&
            &x     ,ws    ,zbave ,zbfl   ,zbeps  ,nvast ,&
            &ngrid ,nfrac ,ncfl  ,nonngp ,wf    ,&
            &deltar,jugralg      )
            first = .false.
         endif
!
!           Adapt cross section
!
!
         call GMADCS (igp    ,deltaa (igp,nfrac+1)  ,time  ,moropt  ,&
         &nboun  ,ngrid  ,nnode ,branch(1,ibr ) ,&
         &node   ,mbdpar ,maxtab ,ntabm ,ntab  ,table   ,&
         &maxlev ,nlev   ,hlev   ,wft   ,ws      ,&
         &flwdir )
      endif
      prevst = grid(igp).eq.2
20 continue

   do igp=i1,i2
      sumda(igp) = sumda(igp) + deltaa(igp,nfrac+1)
   enddo
!
!     Check if at the end of the loop over the branch there is still
!     area left that must be adjusted (to another branch).
!
   sum = 0.
   do 30 jf=1,nfrac
      sum = sum + daacor(jf)
30 continue
   if (sum .gt. 0.) then
!
!        Als deze melding vaak komt moet overwogen worden om
!        bijv. het restant naar de naburige tak te verdelen.
!        (7-12-99)
!
      write (jugralg,*)&
      &' No erosion possible at downstream end of branch'
   endif

   return
end
