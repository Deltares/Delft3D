subroutine gscond (nlayer ,nfrac  ,ngrid  ,j      ,jl     ,jr    ,&
&width  ,deffec ,dunele ,dmed   ,dmexla ,dmed0 ,&
&dhda   ,duda   ,ddefdh ,ddefdu ,ddefdd ,fdi   ,&
&pdiadi ,x      ,sedtr  ,source ,deposi ,delta ,&
&deltb  ,deltc  ,jugralg)
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gscond.F,v $
! Revision 1.3  1996/01/08  13:29:51  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:12:00  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!  Module:     Graded Sediment Determine CONDition (sedimentation
!              or erosion)
!
!     Declaration of parameters
!
   integer    nlayer ,nfrac  ,ngrid  ,j      ,jl     ,jr    ,jugralg
   real       width  ,deffec ,dunele ,dmed   ,dmexla ,dmed0 ,dhda  ,&
   &duda   ,ddefdh ,ddefdu ,ddefdd ,delta  ,deltb ,deltc ,&
   &fdi    ,pdiadi
   real       x      (ngrid)         ,sedtr  (ngrid,nfrac+2)       ,&
   &source(ngrid,nfrac+2)
   logical    deposi
!
!     Declaration of local variables
!
   real       radelt ,delta1 ,deltb1 ,deltc1 ,rdx    ,dstdx  ,ddm1dx,&
   &srctot ,srcdm1 ,ddefdt ,chan0  ,chan1  ,rdx1   ,rdx2  ,&
   &ddefdt0,factor
   logical    deposn
!
!     Declaration of constants
!
   real       gamma
   parameter (gamma=0.08)

!     Calculate constants

   radelt = 1. / (width * deffec)
   delta1 = - ( ddefdh * dhda + ddefdu * duda )
   deltb1 = - ddefdd * radelt

   if (nlayer .eq. 1) then
      deltc1 = 0.
   else if (nlayer .eq. 2) then
      deltc1 = gamma * radelt * sedtr(j,nfrac+1) / dunele * ddefdd *&
      &(dmexla - fdi * dmed - pdiadi)
   endif

!     Calculate derivatives

   rdx    = 1. / (x(jr)-x(jl))
   dstdx  = (sedtr(jr,nfrac+1) - sedtr(jl,nfrac+1)) * rdx
   ddm1dx = (sedtr(jr,nfrac+2) - sedtr(jl,nfrac+2)) * rdx

!     Calculate source
   if (jr-jl .eq. 2) then
!        Within branch
      rdx1 = x(jl+1)-x(jl)
      rdx2 = x(jr)-x(jr-1)
      if (rdx1 .gt. 1.e-6) rdx1 = 1. / rdx1
      if (rdx2 .gt. 1.e-6) rdx2 = 1. / rdx2
      srctot = (source(jl,nfrac+1) * rdx1&
      &+ source(jl+1,nfrac+1) * rdx2) *.5
      srcdm1 = (source(jl,nfrac+2) * rdx1&
      &+ source(jl+1,nfrac+2) * rdx2) *.5
   else
!        At first or last cell in branch
      srctot = source(jl,nfrac+1) * rdx *.5
      srcdm1 = source(jl,nfrac+2) * rdx *.5
   endif
!
!     Calculate delta's and d(deff)/dt. Whether it is
!     deposition or sedimentation is determined by the previous step.
!
   delta = delta1
   deltb = deltb1
   deltc = deltc1
   call gsdelt (deposi ,nlayer ,dmed   ,dmexla ,dmed0 ,deffec,&
   &ddefdd ,radelt ,delta  ,deltb  ,deltc )

   ddefdt = delta * (dstdx - srctot) +&
   &deltb * (ddm1dx - srcdm1) + deltc

   chan0  = - dstdx + srctot - width * ddefdt
   deposn = - dstdx + srctot - width * ddefdt .gt. 0.

   if (deposn .neqv. deposi) then
!
!        Recalculate delta's and d(deff)/dt. Because change of
!        deposition to sedimentation or the other way around.
!
      delta = delta1
      deltb = deltb1
      deltc = deltc1
      call gsdelt (deposn ,nlayer ,dmed   ,dmexla ,dmed0 ,deffec,&
      &ddefdd ,radelt ,delta  ,deltb  ,deltc )

      ddefdt = delta * (dstdx - srctot) +&
      &deltb * (ddm1dx - srcdm1) + deltc

      deposn = - dstdx + srctot - width * ddefdt .gt. 0.
      if (deposn .neqv. deposi) then
         deposi = deposn
      else
         chan1  = - dstdx + srctot - width * ddefdt
!
!           3-1-01 ARS 6025
!           It cannot be determined if the effective layer is
!           in deposition or sedimentation phase.
!           So asume it is zero sedimentation.
!
!           Recalculate if it is sedimentation
!
         deposn = .true.
         delta  = delta1
         deltb  = deltb1
         deltc  = deltc1
         call gsdelt (deposn ,nlayer ,dmed   ,dmexla ,dmed0 ,deffec,&
         &ddefdd ,radelt ,delta  ,deltb  ,deltc )
!
!           Adapt d(deff)/dt such that the amount of sedimentation
!           is zero.
!
         ddefdt0 = (- dstdx + srctot ) / width
         ddefdt  = delta * (dstdx - srctot) +&
         &deltb * (ddm1dx - srcdm1) + deltc
         if (abs(ddefdt).gt.1.e-20) then
            factor = ddefdt0 / ddefdt
            delta  = delta * factor
            deltb  = deltb * factor
            deltc  = deltc * factor
            deposi = .true.
         else
            write (jugralg,'(a,i4,a,2e13.5)')&
            &' Cannot determine deposition or erosion ('&
            &,j ,') ',chan0,chan1
         endif
      endif
   endif

end
