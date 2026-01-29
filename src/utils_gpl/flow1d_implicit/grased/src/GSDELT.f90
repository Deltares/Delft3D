subroutine gsdelt (deposi ,nlayer ,dmed   ,dmexla ,dmed0 ,deffec,&
&ddefdd ,radelt ,delta  ,deltb  ,deltc )
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdelt.F,v $
! Revision 1.2  1995/09/27  10:12:03  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
! Module:             GSDELT (Graded Sediment calculate DELTa
!                             coefficients)
!
!     Declaration of parameters
!
   integer    nlayer
   real       dmed   ,dmexla ,dmed0 ,deffec ,ddefdd ,&
   &radelt ,delta  ,deltb ,deltc
   logical    deposi
!
!     Declaration of local variables
!
   real       delte
!
   if (deposi) then
!        Deposition
      delta  = delta + dmed * radelt * ddefdd
   else
!        Erosion
      if (nlayer .eq. 1) then

         delta = delta + dmed0 * radelt * ddefdd
         delte = 1. / (1. + (dmed - dmed0) / deffec * ddefdd)
!            3-1-01 ARS 6025
!            avoid devide by zero
         if (abs(delte).gt.1000000.) then
            delte = sign(1000000.,delte)
         endif

      else if (nlayer .eq. 2) then

         delta = delta + dmexla * radelt * ddefdd
         delte = 1. / (1. + (dmed - dmexla) / deffec * ddefdd)

!            3-1-01 ARS 6025
!            avoid devide by zero
         if (abs(delte).gt.1000000.) then
            delte = sign(1000000.,delte)
         endif

      endif
      delta = delta * delte
      deltb = deltb * delte
      deltc = deltc * delte
   endif

end
