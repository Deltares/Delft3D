module gadata
!
!     arrays with length nreeksf
   real         ,allocatable, save :: tijdenf(:)   ,reeksf(:)
!     arrays with length nreeks / nreeksi
   real         ,allocatable, save :: tijden(:)    ,reeks(:) ,&
   &coreeks(:)
   double precision ,allocatable, save ::  y2(:)   ,u(:)
!     arrays with length lrenbuf
   real         ,allocatable, save :: reeksen(:)
!     arrays with length nvar*nloc
   real         ,allocatable, save :: buffer(:)
!     arrays with length nvar
   character*20 ,allocatable, save :: parnam(:)
!     arrays with length nloc
   character*20 ,allocatable, save :: locnam(:)
   integer, parameter :: hreeks=1 ,qreeks=2 ,vreeks=3 ,creeks=4 ,&
   &sreeks=5 ,greeks=6

!     array with unknown length garesult
   real         ,allocatable, save :: garesult(:)
   integer     ::  avail=0
!
   integer, parameter :: type    = 1  ,grooth  = 2  ,locat   = 3  ,&
   &ngetij  = 4  ,&
   &igetij  = 1  ,begin   = 2  ,eind    = 3  ,&
   &duur    = 4  ,gemidd  = 5  ,slag    = 6  ,&
   &mintijd = 7  ,minw    = 8  ,maxtijd = 9  ,&
   &maxw    =10  ,vloed   =11  ,eb      =12  ,&
   &nummer  =13
!
!                   type    type van de groorheid
! 1                 grooth  grootheidsnummer
! 2                 locat   locatie nummer
! 3                 ngetij  aantal getijden
! 3+(igetij-1) +1   igetij  getijnummer = 1
! 3+(igetij-1) +2   begin   begintijd getij
! 3+(igetij-1) +3   eind    eindtijd getij
! 3+(igetij-1) +4   duur    duur getij
! 3+(igetij-1) +5   gemidd  middenstand / reststroom / gemiddelde
! 3+(igetij-1) +6   slag    getijslag / delta
! 3+(igetij-1) +7   mintijd tijd minimum
! 3+(igetij-1) +8   minw    minimum waarde
! 3+(igetij-1) +9   maxtijd tijd maximum
! 3+(igetij-1) +10  maxw    maximum waarde
! 3+(igetij-1) +11  vloed   vloedvolume / gemiddelde (positief)
! 3+(igetij-1) +12  eb      ebvolume / gemiddelde (negatief)
! 3+(igetij-1) +1   nummer  getijnummer = 2
end module
