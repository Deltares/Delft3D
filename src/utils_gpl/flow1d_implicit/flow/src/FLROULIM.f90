subroutine flroulim (kode,c,juer,ker)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             FLROULIM (FLow ROUghness LIMiter)
!
! Module description: Limit Chezy value and give message
!
!-----------------------------------------------------------------------
!
!     Declaration of Parameters:
!
   integer    kode ,juer ,ker
   real       c
!
   real       chlow,chcur
   common     /roughmin/chlow(3),chcur(3)
   save       /roughmin/
!
   include '..\include\errcod.i'
!
!     Declaration of local variables:
!
   integer       isec1 ,i
   character*6   ctxt
   character*12  sectxt(3)
   data         (sectxt(i),i=1,3)/'main channel',&
   &'floodplain 1',&
   &'floodplain 2'/
!
   if (kode.gt.0) then
!        store current value
      isec1 = kode
      chcur(isec1) = min (chcur(isec1),c)
   else if (kode.eq.-1) then
!        initialze first time
      do i=1,3
         chlow(i) = 1000.
      enddo
   else if (kode.eq.-2) then
!        initialze for end of time step
      do i=1,3
         chcur(i) = 1000.
      enddo
   else if(kode.eq.-3) then
!        store values at end of time step
      do i=1,3
         chlow(i) = min (chcur(i),chlow(i))
      enddo
   else if(kode.eq.-4) then
      if (chlow(1).lt.999.9) then
         write (ctxt,'(f6.2)') chlow(1)
         call error (juer,'FLROULIM Chezy in @'//sectxt(1)//&
         &'@ limited (@'//ctxt//'@)',eflrou,ker)
      endif
      do i=2,3
         if (chlow(i).lt.999.9) then
            call error (juer,'FLROULIM Chezy in @'//sectxt(i)//&
            &'@ limited ',eflrof,ker)
         endif
      enddo
   endif
end
