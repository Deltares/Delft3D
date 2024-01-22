subroutine cor2world (x_cor  , y_cor , z_cor , s_cor  , h_cor   , b_cor   ,&
                                                            x_jet  , y_jet , z_jet , s_jet  , h_jet   , b_jet   ,&
                                                            xz     , yz    , dps  ,&
                                                            no_val , taua  , idis, gdp     )
    
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none 
    
     type(globdat),target :: gdp
        !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    
    integer ,dimension(:)          , pointer :: m_diff
    integer ,dimension(:)          , pointer :: n_diff
!
! Global variables
!
    integer                                                    , intent(in)    :: idis
    integer                                                    , intent(in)   :: no_val
    real(fp)                                                   , intent(inout) :: taua
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: xz
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: yz
    real(fp)   , dimension(no_val)                               , intent(in)    :: x_cor
    real(fp)   , dimension(no_val)                               , intent(in)    :: y_cor
    real(fp)   , dimension(no_val)                               , intent(in)    :: z_cor
    real(fp)   , dimension(no_val)                               , intent(in)    :: s_cor
    real(fp)   , dimension(no_val)                               , intent(in)    :: h_cor
    real(fp)   , dimension(no_val)                               , intent(in)    :: b_cor
    real(fp)   , dimension(no_val)                               , intent(out) :: x_jet
    real(fp)   , dimension(no_val)                               , intent(out) :: y_jet
    real(fp)   , dimension(no_val)                               , intent(out) :: z_jet
    real(fp)   , dimension(no_val)                               , intent(out) :: h_jet
    real(fp)   , dimension(no_val)                               , intent(out) :: b_jet
    real(fp)   , dimension(no_val)                               , intent(out) :: s_jet
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)    :: dps
!
! Local variables
!
    integer                                                                    :: i
    real(fp)                                                                   :: deg2rad
    integer                                                                    :: nm_diff
!
!! executable statements -------------------------------------------------------
!

    deg2rad = acos(-1.0_fp)/180.0_fp
    m_diff         => gdp%gdnfl%m_diff
    n_diff         => gdp%gdnfl%n_diff

    call n_and_m_to_nm(n_diff(idis), m_diff(idis), nm_diff, gdp)  
    
    do i = 1,no_val
                
                x_jet(i) = xz(nm_diff) + x_cor(i)*cos(taua*deg2rad) - y_cor(i)*sin(taua*deg2rad)
                y_jet(i) = yz(nm_diff) + x_cor(i)*sin(taua*deg2rad) + y_cor(i)*cos(taua*deg2rad)
                z_jet(i) = real(dps(nm_diff),fp) - z_cor(i)
                s_jet(i) = s_cor(i)
                h_jet(i) = h_cor(i)
                b_jet(i) = b_cor(i)
    enddo
    end subroutine cor2world