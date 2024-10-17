subroutine secysb (g      ,u      ,dvelo  ,depth  ,ibrl   ,juer  ,&
&igp    ,sedtra ,sedpdu ,celeri ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SECYSB (SEdiment CeleritY Sedredge Branch)
!
! Module description: Calculate celerity for a sedredge branch
!
!                     The Froude number is calculated and tested for
!                     extreme values. The celerity is calculated using
!                     numerical differentiation. For this the transport
!                     on U+.001 is calculated.
!
! Precondition:       Froude number < 1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 celeri            O  celerity
!  4 depth             I  avarage depth
!  3 dvelo             I  Difference in velocity u+du
!  1 g                 I  Acceleration of gravity.
!  5 ibrl              IO Number of actual branch. Will be set to zero
!                         when a message is generated.
!  6 juer              P  -
! 10 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  8 sedpdu            I  sediment transport as f(u+du)
!  7 sedtra            I  calculated sediment transport
!  2 u                 I  velocity
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: secysb.pf,v $
! Revision 1.5  1999/06/01  13:42:40  kuipe_j
! names in messages substituted + message template
!
! Revision 1.4  1999/03/15  13:47:30  kuipe_j
! coordinate in message
!
! Revision 1.3  1995/05/30  09:56:27  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:15  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:17  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:39  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    juer ,ibrl ,ker,ibrd, igp, lbrnam
   real       g    ,u    ,dvelo   ,depth  ,sedtra ,sedpdu ,celeri,xc
!
!     Declaration of local parameters
!
   real               velo   ,frou2 ,dsdu
   character*40 branam
   character*5  txt
   character*11 xtxt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   integer iostat
   logical exist
   real froumx, froum
!
   velo  = abs(u)
   frou2 = velo*velo / (g*depth)
   if (frou2 .gt. .64) then
!+++++++++++++++++++++++++++++++
! start rev 1.3:
!+++++++++++++++++++++++++++++++
      froumx = .98
      inquire(file='../FROUDMAX',iostat=iostat,exist=exist)
      if (iostat.eq.0 .and.  exist) then
         open(98,file='../FROUDMAX',iostat=iostat,status='old')
         if (iostat.eq.0) then
            read(98,'(f10.0)',iostat=iostat)froum
         endif
         if (iostat.eq.0) then
            froumx = max(froumx,froum)
         endif
         close (98)
      endif
      if (frou2 .gt. froumx) then
         write(*,*)'+++SECYSB froumx**2=',froumx,&
         &' exceeded, froude**2 value=',frou2
!-orig   if (frou2 .gt. .98) then
!+++++++++++++++++++++++++++++++
! end rev 1.3:
!+++++++++++++++++++++++++++++++
         ker = fatal
         call getloc (igp,ibrd,xc)
         write (txt,'(f5.2)') sqrt(frou2)
         call getbrn (ibrd,branam,lbrnam)
         write (xtxt,'(f10.2)') xc
         call error (juer,&
         &'SECYSB Froude number too large (@'//txt//&
         &'@) in branch @'//branam(:lbrnam)//'@ at X= @'&
         &//xtxt//'@',esefro,ker)
         goto 100
      else if (ibrl .ne. 0) then
!
!           Only one message will be given in a branch.
!
         ker = warnng
         call getloc (igp,ibrd,xc)
         call getbrn (ibrd,branam,lbrnam)
         write (xtxt,'(f10.2)') xc
         call error (juer,&
         &'SECYSB Froude numbers > .8 in branch @'&
         &//branam(:lbrnam)//'@ starting at X=@'//&
         &xtxt//'@',esefrw,ker)
         ibrl = 0
      endif
   endif
!
   dsdu   = (sedpdu - abs(sedtra)) / dvelo
   celeri = sign(dsdu * velo / ((1.-frou2) * depth),sedtra)
!
100 continue
!
end
