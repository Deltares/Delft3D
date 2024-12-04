subroutine fltspa(strpar ,istru  ,nstru  ,stdbq ,nstdb1 ,s     ,&
&xnod   ,ynod   ,n      ,m     ,deltah ,inttyp,&
&zs     ,zero   ,juer   ,ker   )
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
! Module:             FLTSPA (FLow Tabulated Structure get PAramaters)
!
! Module description: Parameters for the data base are extracted
!                     from the packed array strpar.
!
!                     Also at the first call the data in data base
!                     STDBQ containing the Q-h relation is reordered.
!
!-----------------------------------------------------------------------
! Parameters:
! NAME             IO DESCRIPTION
! deltah            O switch for type of arguments ynod:
!                     true:  ynod = h1-h2
!                     false: ynod = h2
! inttyp            O 1 = linear inerpolation
!                     2 = spline
! istru             I structure number
! m                 O number of columns (h2-values) in the Q-h table-1
! n                 O number of rows (h1-values) in the Q-h table-1
! nstdb1            I number of elements of data base for this structure
! nstru               number of structures
! strpar(21,nstru) IO Parameters for data base structure:
!                     (1,i) interpolation:
!                           1 = linear
!                           2 = spline
!                     (2,i) sequence number of data base structure
!                           (1,2,…), i.e. data base structure number
!                     (3,i) number of dimensions of data base (2 or 3)
!                     (4,i) number of H1-values nh1
!                     (5,i) number of H2-values nh2
!                     (6,i) number of values of gate height, crest level
!                           or width
!                           (extension in future; 1 in Sobek 2.51)
!                     (7,i) dummy
!                     (8,i) dummy
!                     (9,i) index of first element in array with
!                           discharges stdbq for this structure
!                     (10,i) type of values of first row in stdbq for
!                           this structure:
!                           1 : H2-values
!                           2 : dH-values (dH=H2-H1)
!                     (11,i) number of structure where the data base
!                           of this structure is defined. This is
!                           the structure with the first occurrence
!                           of a link to this data base table.
!                     (12,i) indicator that identifies if this data
!                           base table is initialised:
!                           0 : no
!                           1 : yes
!                     (13,i) level of sill of structure or reference
!                           level
!                     (14,i) indicator that defines if discharge is zero
!                           if sill is dry (0), otherwise 1.
!                     (15,i) minimum H1 (wrt sill) in case of underflow
!                           (otherwise 1E20)
!                     (16,i) minimum H2/Dh in case of underflow
!                           (otherwise 1E20)
!                     (17,i) maximum H1 (wrt sill) in case of overflow
!                           (otherwise -1E20)
!                     (18,i) maximum H2/DH in case of overflow
!                           (otherwise -1E20)
!                     (19,i) 4-digit indicater for under or overflow in
!                           an iteration step.
!                           From right to left a digit corresponds for:
!                           H1-underflow, H2/DH-underflow,
!                           H1-overflow, H2/DH-overflow.
!                           A digit can be 0 (inside) or 1 (outside
!                           data base).
! stdbq            IO Table containing discrete Q-h relation, h1 values
!                     and h2 (or h1-h2) values at initial input:
!                     stdbq(-1,-1)= dummy / gate value
!                     stdbq(i,-1) = h1(i)                   for i=0(1)n
!                     stdbq(-1,j) = (h1-h2)(j)  or
!                                   h2(j)                   for j=0(1)m
!                     stdbq(i,j)  = Q(i,j)      for i=0(1)n and j=0(1)m
!                     At output and following calls:
!                     stdbq(1)      dummy / gate value
!                     stdbq(2..)    h1(i)                   for i=0(1)n
!                     stdbq(3+n..)  (h1-h2)(j) or
!                                   h2(j)                   for j=0(1)m
!                     stdbq(4+m+n..)Q(i,j)      for i=0(1)n and j=0(1)m
! s                 O pointer to array s (contains Q)
! xnod              O pointer to array xnod (contains h1)
! ynod              O pointer to array ynod (contains h2 or h1-h2)
! zs                O level of sill
!-----------------------------------------------------------------------
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer    istru ,nstru     ,nstdb1 ,s     ,xnod  ,ynod ,&
   &n     ,m         ,inttyp ,juer  ,ker
   real       zs
   real       strpar(dmstrpar,nstru) ,stdbq  (nstdb1)
   logical    deltah,zero
!
!     Declaration of external
!
   external   wlcrest0
   logical    wlcrest0
!
!     Declaration of local variables:
!
   integer    nh1   ,nh2       ,sdbdef ,j    ,ijget  ,ijput ,&
   &ind   ,ij1       ,ij2    ,m2
   real       h2
   logical    inidb ,initial
!
!     Include sobek constants and error codes
!
   include '../include/errcod.i'
!
   nh1     = nint(strpar(4,istru))
   nh2     = nint(strpar(5,istru))
   n       = nh1 - 1
   m       = nh2 - 1
   deltah  = nint(strpar(10,istru)).eq.2
   inttyp  = nint(strpar(1 ,istru))
   sdbdef  = nint(strpar(11,istru))
   initial = nint(strpar(12,istru)).eq.0
   inidb   = nint(strpar(12,sdbdef)).eq.0
   zs      =      strpar(13,istru)
   zero    = nint(strpar(14,sdbdef)).eq.0
!
   xnod    = 2
   ynod    = 2+nh1
   s       = 2+nh1+nh2
!
!     Initialize limits
!
   if (initial) then
!        for current structure
      strpar(15,istru) = 1.e20
      strpar(16,istru) = 1.e20
      strpar(17,istru) = -1.e20
      strpar(18,istru) = -1.e20
      strpar(12,istru) = 1.
   endif
   if (inidb) then
!        for structure that contains data base definition
      strpar(15,sdbdef) = 1.e20
      strpar(16,sdbdef) = 1.e20
      strpar(17,sdbdef) = -1.e20
      strpar(18,sdbdef) = -1.e20
   endif
!
   if (inidb) then
!
!       Reorder structure data base
!       j is index in s from 0..m
!       First shift h2-values from columns to second row
!
      do j=1,m
         ijget = (m+2-j)*(n+2)+j
         ijput = n+3+j
         h2    = stdbq(ijget)
         do ind = ijget-1,ijput,-1
            stdbq(ind+1) = stdbq(ind)
         enddo
         stdbq(ijput) = h2
      enddo
!
!       h2-values in second row are in reverse order,
!       so they are put in increasing order.
!
      m2=m/2
      do j=1,m2
         ij1 = n+3+j
         ij2 = n+m+4-j
         h2  = stdbq(ij1)
         stdbq(ij1) = stdbq(ij2)
         stdbq(ij2) = h2
      enddo
!
!       Set indicator of structure where data base was defined
!       (first occurence at input) at 'initialized'.

      strpar(12,sdbdef) = 1.0

      zero = wlcrest0 (stdbq(s) ,stdbq(xnod) ,stdbq(ynod) ,n ,m ,&
      &deltah   )
      if (zero) then
         strpar(14,sdbdef)= 0
      else
         strpar(14,sdbdef)= 1
      endif

   endif
!
!     Warning if discharge is not zero at sill
!
   if (initial .and. .not.zero) then
!         call getstr(istru,strnam,lstnam)
!         ker = warnng
!         call sre_error (juer,'FLTSPA Q not zero in structure @'
!     +               //strnam(:lstnam)//'@ if sill dry', eflcr0, ker)
   endif
   if (inidb .and. .not.zero) then
!         call getstr(sdbdef,strnam,lstnam)
      if (sdbdef.ne.istru) then
!            call getstr(sdbdef,strnam,lstnam)
!            ker = warnng
!            call sre_error (juer,'FLTSPA Q not zero in structure @'
!     +                  //strnam(:lstnam)//'@ if sill dry', eflcr0, ker)
      endif
   endif
!
   return
!
end
function wlcrest0 (s ,xnod, ynod ,n ,m ,deltah)
!
!     Function determines if discharge is zero when crest becomes
!     dry.
!
   integer  n         ,m
   real     s(0:n,0:m), xnod(0:n) ,ynod(0:m)
   logical  wlcrest0  ,deltah
!
   logical  nul
   integer  j    ,jj
!
!     Declaration of external
!
   external   epsequ
   logical    epsequ
!
   if (deltah) then
      nul = .true.
      j = 0
      do while (.not.EPSEQU (ynod(j),0.,1.e-8))
         j = j+1
      enddo
      do jj=j,m
         nul = nul .and. (EPSEQU (s(0,jj),0.,1.e-6))
      enddo
   else
      if (EPSEQU (xnod(0),0.,1.e-8) .and. EPSEQU (ynod(0),0.,1.e-8)&
      &.and. EPSEQU (s(0,0),0.,1.e-6)) then
         nul = .true.
      else
         nul = .false.
      endif
   endif
   wlcrest0 = nul
!
end
