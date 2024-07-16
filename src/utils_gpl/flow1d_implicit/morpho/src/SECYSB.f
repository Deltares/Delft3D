      subroutine secysb (g      ,u      ,dvelo  ,depth  ,ibrl   ,juer  ,
     &                   igp    ,sedtra ,sedpdu ,celeri ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SECYSB (SEdiment CeleritY Sedredge Branch)
c
c Module description: Calculate celerity for a sedredge branch
c
c                     The Froude number is calculated and tested for
c                     extreme values. The celerity is calculated using
c                     numerical differentiation. For this the transport
c                     on U+.001 is calculated.
c
c Precondition:       Froude number < 1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 celeri            O  celerity
c  4 depth             I  avarage depth
c  3 dvelo             I  Difference in velocity u+du
c  1 g                 I  Acceleration of gravity.
c  5 ibrl              IO Number of actual branch. Will be set to zero
c                         when a message is generated.
c  6 juer              P  -
c 10 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  8 sedpdu            I  sediment transport as f(u+du)
c  7 sedtra            I  calculated sediment transport
c  2 u                 I  velocity
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: secysb.pf,v $
c Revision 1.5  1999/06/01  13:42:40  kuipe_j
c names in messages substituted + message template
c
c Revision 1.4  1999/03/15  13:47:30  kuipe_j
c coordinate in message
c
c Revision 1.3  1995/05/30  09:56:27  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:15  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:17  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:39  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    juer ,ibrl ,ker,ibrd, igp, lbrnam
      real       g    ,u    ,dvelo   ,depth  ,sedtra ,sedpdu ,celeri,xc
c
c     Declaration of local parameters
c
      real               velo   ,frou2 ,dsdu
      character*40 branam
      character*5  txt
      character*11 xtxt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      integer iostat
      logical exist
      real froumx, froum
c
      velo  = abs(u)
      frou2 = velo*velo / (g*depth)
      if (frou2 .gt. .64) then
c+++++++++++++++++++++++++++++++
c start rev 1.3:
c+++++++++++++++++++++++++++++++
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
            write(*,*)'+++SECYSB froumx**2=',froumx,
     +                ' exceeded, froude**2 value=',frou2
c-orig   if (frou2 .gt. .98) then
c+++++++++++++++++++++++++++++++
c end rev 1.3:
c+++++++++++++++++++++++++++++++
            ker = fatal
            call getloc (igp,ibrd,xc)
            write (txt,'(f5.2)') sqrt(frou2)
            call getbrn (ibrd,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            call error (juer,
     +           'SECYSB Froude number too large (@'//txt//
     +           '@) in branch @'//branam(:lbrnam)//'@ at X= @'
     +           //xtxt//'@',esefro,ker)
            goto 100
         else if (ibrl .ne. 0) then
c
c           Only one message will be given in a branch.
c
            ker = warnng
            call getloc (igp,ibrd,xc)
            call getbrn (ibrd,branam,lbrnam)
            write (xtxt,'(f10.2)') xc
            call error (juer,
     +            'SECYSB Froude numbers > .8 in branch @'
     +            //branam(:lbrnam)//'@ starting at X=@'//
     +            xtxt//'@',esefrw,ker)
            ibrl = 0
         endif
      endif
c
      dsdu   = (sedpdu - abs(sedtra)) / dvelo
      celeri = sign(dsdu * velo / ((1.-frou2) * depth),sedtra)
c
  100 continue
c
      end
