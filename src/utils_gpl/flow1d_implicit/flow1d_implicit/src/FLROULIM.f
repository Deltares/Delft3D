      subroutine flroulim (kode,c,juer,ker)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             FLROULIM (FLow ROUghness LIMiter)
c
c Module description: Limit Chezy value and give message
c
c-----------------------------------------------------------------------
c
c     Declaration of Parameters:
c
      integer    kode ,juer ,ker
      real       c
c
      real       chlow,chcur
      common     /roughmin/chlow(3),chcur(3)
      save       /roughmin/
c
      include '../include/errcod.i'
c
c     Declaration of local variables:
c
      integer       isec1 ,i
      character*6   ctxt
      character*12  sectxt(3)
      data         (sectxt(i),i=1,3)/'main channel',
     +                               'floodplain 1', 
     +                               'floodplain 2'/ 
c      
      if (kode.gt.0) then
c        store current value 
         isec1 = kode
         chcur(isec1) = min (chcur(isec1),c)
      else if (kode.eq.-1) then
c        initialze first time
         do i=1,3
            chlow(i) = 1000.
         enddo   
      else if (kode.eq.-2) then
c        initialze for end of time step
         do i=1,3
            chcur(i) = 1000.
         enddo   
      else if(kode.eq.-3) then 
c        store values at end of time step  
         do i=1,3
            chlow(i) = min (chcur(i),chlow(i)) 
         enddo
      else if(kode.eq.-4) then   
         if (chlow(1).lt.999.9) then
            write (ctxt,'(f6.2)') chlow(1)
            call sre_error (juer,'FLROULIM Chezy in @'//sectxt(1)//
     +                  '@ limited (@'//ctxt//'@)',eflrou,ker)
         endif
         do i=2,3
            if (chlow(i).lt.999.9) then
               call sre_error (juer,'FLROULIM Chezy in @'//sectxt(i)//
     +                     '@ limited ',eflrof,ker)
            endif
         enddo   
      endif
      end
