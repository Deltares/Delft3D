      subroutine GMDACO ( start  ,igpm1  ,igp    ,igpp1  ,deltaa,
     +                    daacor ,x      ,ws     ,zbave  ,zbfl  ,
     +                    zbeps  ,nvast  ,ngrid  ,nfrac  ,ncfl  ,
     +                    nonngp ,wf     ,deltar ,jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         C. Flokstra
c
c Module:             GMDACO
c
c Module description: Calculate correction on delta A to prevent 
c                     negative frequencies or erosion below fixed layer.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 29 ws                P  -
c 21 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gmarea.F,v $
c Revision 1.4  1996/01/08  13:29:33  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.3  1996/01/05  15:43:18  kuipe_j
c Lateral sediment and structures
c
c Revision 1.2  1995/09/27  10:11:25  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c
c     Parameters
c

      integer   nfrac  ,igpm1  ,igp    ,igpp1  ,nonngp  ,nvast  ,
     +          ngrid  ,ncfl   ,jugralg 
      real      zbeps        
      real      x      (ngrid),
     +          ws     (ngrid),
     +          deltar (ngrid,nfrac) ,
     +          zbave  (*),
     +          zbfl   (*),
     +          daacor (*),
     +          wf     (ngrid)
	double precision deltaa (ngrid,nfrac+1)
      logical   start
c
c     Local variables
c
      integer   icf    ,nmet1

      real      dx     ,wsact  ,dzt  ,zbavmi  ,dztma  ,dzfac  ,
     +          daacrh ,dzta ,dztb
	double precision sum, sumd
c
c     Adapt daa to prevent negative frequencies
c
c
      if (start) then
         do 10 icf=1,nfrac+1
            daacor(icf) = 0.0                             
  10     continue
         ncfl= 0
      endif

      if ((nonngp.ne.1).and.(nvast.ne.1)) return
c
c     Adapt cross section adaptation area due to upstream correction 
c
      dx = 0.5 * (x(igpp1) - x(igpm1))
      if (ncfl.eq.1) then
         sum = 0.0
         do 20 icf=1,nfrac
            deltaa(igp,icf) = deltaa(igp,icf) + daacor(icf)/dx
            sum = sum + deltaa(igp,icf) 
            daacor(icf) = 0.0
  20     continue
         deltaa(igp,nfrac+1) = sum                                    
         ncfl= 0
      endif
c
      do 25 icf=1,nfrac
         if ( deltaa(igp,icf) .gt.0.0 ) goto 28
  25  continue
c
c  no erosion
c
      return
c
  28  continue
c
      if (nvast.eq.1) then
c
c correctie voor door vaste laag schieten
c
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
c
            if (nmet1.eq.1) then
c nmet1 = 1
               dztma = zbave(igp) - zbavmi
               if (dztma.gt.0.0) then
                  dzfac = 1.0 + dztma / dzt
                  do 30 icf=1,nfrac+1
                     daacor(icf) = dzfac * deltaa(igp,icf) 
                     deltaa(igp,icf)= deltaa(igp,icf) - daacor(icf)
  30              continue
               else
                  do 35 icf=1,nfrac+1
                     daacor(icf) = deltaa(igp,icf) 
                     deltaa(igp,icf)= 0.0                            
  35              continue
               endif
c
            else
c nmet1=2
               dzta= 0.0
               dztb= 0.0
               do 29 icf=1,nfrac
                  if (deltaa(igp,icf).gt.0.0) then
                     dzta = dzta + deltaa(igp,icf)
                  else
                     dztb = dztb + deltaa(igp,icf)
                  endif
  29           continue
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
  31           continue
               sum  = 0D0
               sumd = 0D0
               do 32 icf=1,nfrac
                  sum  = sum  + daacor(icf)
                  sumd = sumd + deltaa(igp,icf)
  32           continue
               daacor(nfrac+1)    = sum
               deltaa(igp,nfrac+1)= sumd
c           else
c              do 36 icf=1,nfrac+1
c                 daacor(icf) = deltaa(igp,icf)
c                 deltaa(igp,icf)= 0.0
c 36           continue
c           endif
c einde nmet1
            endif
            ncfl= 1
         endif
      endif
c
c correctie voor negatieve fracties
c
      if (nonngp.eq.1) then
         do 40 icf=1,nfrac
            if (deltaa(igp,icf) .gt. deltar(igp,icf))then
               daacrh      = deltaa(igp,icf) - deltar(igp,icf)
               daacor(icf) = daacor(icf) + daacrh
               deltaa(igp,icf) = deltar(igp,icf)
               deltaa(igp,nfrac+1) = deltaa(igp,nfrac+1) - daacrh
               ncfl= 1
            endif
  40     continue
      endif
c
      if (ncfl.eq.1) then
         do 50 icf=1,nfrac
            daacor(icf) = daacor(icf) * dx
  50     continue
      endif
c
      return
      end
