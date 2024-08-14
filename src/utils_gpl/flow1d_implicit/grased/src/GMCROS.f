      subroutine GMCROS (ibr    ,ngrid  ,nbran  ,nboun  ,nnode  ,nfrac ,
     +                   time   ,moropt ,nonngp ,nvast  ,zbeps ,
     +                   grid   ,branch ,node   ,maxtab ,ntabm  ,ntab  ,
     +                   table  ,mbdpar ,x      ,maxlev ,nlev   ,hlev  ,
     +                   wf     ,wft    ,ws     ,flwdir ,deltaa ,deltar,
     +                   daacor ,zbave  ,zbfl   ,jugralg,sumda  )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         J.Kuipers        
c
c Module:             GMCROS (Graded Morphology adapt CROSs-sections)
c
c Module description: Routine GMAREA has calculated the changes in the
c                     areas in a branch. These calculated changes are
c                     used to adapt the cross sectional tables in routi-
c                     ne MOADCS. But prior to adaptation the change in
c                     area will be corrected for negative frequencies.
c                     Corrections in areas will be tranferred to the
c                     next downstream cross-section.
c
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log:$
c
c***********************************************************************
c
c
c     Parameters
c
      integer   ibr    ,maxlev ,maxtab ,moropt ,ntabm ,nboun ,
     +          ngrid  ,nnode  ,nbran  ,nfrac  ,nonngp,nvast ,
     +          jugralg
      integer   branch (4,nbran)       ,mbdpar (5,nboun)     ,
     +          nlev   (ngrid)         ,ntab   (4,maxtab)    ,
     +          node   (4,nnode)       ,
     +          flwdir (ngrid)         ,grid(ngrid)
      real      zbeps
      real      deltar (ngrid,nfrac) ,
     +          daacor (nfrac)         ,
     +          zbave  (ngrid)         ,zbfl   (ngrid)       ,
     +          sumda  (ngrid)         ,
     +          table  (ntabm)         ,
     +          wft    (ngrid,maxlev)  ,
     +          wf     (ngrid)         ,ws(ngrid)            ,
     +          x      (ngrid)
      double precision  time, hlev (ngrid,maxlev), 
     +                  deltaa (ngrid,nfrac+1) 
c
c     Local variables
c
      integer   i1     ,i2     ,igp    ,igpm1  ,igpp1  ,ncfl  ,
     +          plus   ,mint   ,ia     ,ib     ,ic     ,jf    ,
     +          il     ,ir     ,ibou   ,ibout
      real      sum
      logical   prevst ,first  ,flwpos
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c      
c     Read first and last grid point of branch
c
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
c 
c
c     Determine direction of flow
c     Processing will be done in positive flow direction.
c
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
c
c     Determine if left end of branch is a boundary with
c     condition z=f(t) 
c
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
c
c     Determine if right end of branch is a boundary with
c     condition z=f(t) 
c
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
c
      prevst = .false.
      first  = .true. 
c
      do 20 igp = ia,ib,ic
         if (.not.(grid(igp).eq.2.and.prevst)) then
c
c           Correction of areas for negative frequencies
c           except for boundary points with z=f(t) condition.
c
c  
            if (igp .ne. il .and. igp .ne. ir) then
                igpp1 = min (igp+1,i2)
                igpm1 = max (igp-1,i1)
                call GMDACO(first ,igpm1 ,igp   ,igpp1  ,deltaa ,daacor,
     +                      x     ,ws    ,zbave ,zbfl   ,zbeps  ,nvast ,
     +                      ngrid ,nfrac ,ncfl  ,nonngp ,wf    ,
     +                      deltar,jugralg      )
            first = .false.
            endif
c
c           Adapt cross section
c
c
            call GMADCS (igp    ,deltaa (igp,nfrac+1)  ,time  ,moropt  ,
     +                   nboun  ,ngrid  ,nnode ,branch(1,ibr ) ,
     +                   node   ,mbdpar ,maxtab ,ntabm ,ntab  ,table   ,
     +                   maxlev ,nlev   ,hlev   ,wft   ,ws      ,
     +                   flwdir )
         endif
         prevst = grid(igp).eq.2
   20 continue

      do igp=i1,i2
         sumda(igp) = sumda(igp) + deltaa(igp,nfrac+1)
      enddo
c
c     Check if at the end of the loop over the branch there is still
c     area left that must be adjusted (to another branch).
c
      sum = 0.
      do 30 jf=1,nfrac
          sum = sum + daacor(jf)
   30 continue
      if (sum .gt. 0.) then
c
c        Als deze melding vaak komt moet overwogen worden om  
c        bijv. het restant naar de naburige tak te verdelen.
c        (7-12-99)
c
         write (jugralg,*) 
     +        ' No erosion possible at downstream end of branch'
      endif

      return
      end
