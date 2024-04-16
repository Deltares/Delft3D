module test_coordinate_reference_system
    use ftnunit
    use coordinate_reference_system, only: transform_coordinates, WGS84_PROJ_STRING, RIJKSDRIEHOEK_PROJ_STRING

    implicit none

    character(len=*), parameter :: UTM32N_PROJ_STRING = "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"

    public:: tests_coordinate_reference_system
    
    ! Reference coordinates: WGS84 
    real(kind=kind(1.0d0)), dimension(16) :: REF_WGS84_LAT = [ &
        4.3824929d0, 4.3823802d0, 4.3823293d0, 4.3822032d0, &
        4.3819672d0, 4.3818331d0, 4.3819216d0, 4.3822622d0, &
        4.3824527d0, 4.3827611d0, 4.3829274d0, 4.3829247d0, &
        4.3827879d0, 4.3826511d0, 4.3826002d0, 4.3825814d0 ]
    real(kind=kind(1.0d0)), dimension(16) :: REF_WGS84_LON = [ &
        51.98688d0, 51.9868155d0, 51.986685d0, 51.9865397d0, &
        51.9864356d0, 51.9863629d0, 51.9862836d0, 51.9863167d0, &
        51.9863117d0, 51.9862341d0, 51.9862291d0, 51.9863183d0, &
        51.9864141d0, 51.9865314d0, 51.9866636d0, 51.9868205d0 ]

    ! Reference coordinates: UTM Zone 32N
    real(kind=kind(1.0d0)), dimension(16) :: REF_UTM32N_X = [ &
        182995.863d0, 182987.674d0, 182983.259d0, 182973.578d0, &
        182956.649d0, 182946.933d0, 182952.445d0, 182976.050d0, &
        182989.086d0, 183009.699d0, 183021.075d0, 183021.520d0, &
        183012.811d0, 183004.253d0, 183001.696d0, 183001.515d0]
    real(kind=kind(1.0d0)), dimension(16) :: REF_UTM32N_Y = [ &
        5769652.680d0, 5769646.003d0, 5769631.723d0, 5769616.125d0, &
        5769605.587d0, 5769598.093d0, 5769588.893d0, 5769591.085d0, &
        5769589.697d0, 5769579.727d0, 5769578.445d0, 5769588.370d0, &
        5769599.614d0, 5769613.247d0, 5769628.161d0, 5769645.681d0]

    ! Reference coordinates: Rijksdriehoek 
    real(kind=kind(1.0d0)), dimension(16) :: REF_RD_X = [ &
        85985.882d0, 85978.042d0, 85974.345d0, 85965.459d0, &
        85949.089d0, 85939.766d0, 85945.723d0, 85969.169d0, &
        85982.246d0, 86003.310d0, 86014.726d0, 86014.677d0, &
        86005.428d0, 85996.212d0, 85992.919d0, 85991.869d0]
    real(kind=kind(1.0d0)), dimension(16) :: REF_RD_Y = [ &
        444753.139d0, 444746.071d0, 444731.601d0, 444715.556d0, &
        444704.199d0, 444696.239d0, 444687.333d0, 444690.691d0, &
        444689.954d0, 444681.028d0, 444680.314d0, 444690.240d0, &
        444701.028d0, 444714.207d0, 444728.963d0, 444746.436d0]

    real(kind=kind(1d0)) :: MARGIN = 1d-8

contains
subroutine tests_coordinate_reference_system
    call test(test_rd_to_wgs84, "Verify converted RD coordinates match the reference WGS84 coordinates")
    call test(test_wgs84_to_rd, "Verify converted WGS84 coordinates match the reference RD coordinates")
    call test(test_utm_zone_32n_to_wgs84, "Verify converted UTM zone 32N coordinates match the reference WGS84 coordinates")
    call test(test_wgs84_to_utm_zone_32n, "Verify converted WGS84 coordinates match the reference UTM zone 32N coordinates")
end subroutine tests_coordinate_reference_system

subroutine test_rd_to_wgs84
    ! Arrange
    real(kind=kind(1d0)), dimension(size(REF_WGS84_LAT)) :: dst_x = 0d0, dst_y = 0d0

    ! Act
    call transform_coordinates(WGS84_PROJ_STRING, RIJKSDRIEHOEK_PROJ_STRING, REF_WGS84_LAT, REF_WGS84_LON, dst_x, dst_y)

    ! Assert
    call assert_comparable(dst_x, REF_RD_X, MARGIN, "Converted x-coordinates should match reference values")
    call assert_comparable(dst_y, REF_RD_Y, MARGIN, "Converted y-coordinates should match reference values")
end subroutine test_rd_to_wgs84

subroutine test_wgs84_to_rd
    ! Arrange
    real(kind=kind(1d0)), dimension(size(REF_RD_X)) :: dst_x = 0d0, dst_y = 0d0

    ! Act
    call transform_coordinates(RIJKSDRIEHOEK_PROJ_STRING, WGS84_PROJ_STRING, REF_RD_X, REF_RD_Y, dst_x, dst_y)

    ! Assert
    call assert_comparable(dst_x, REF_WGS84_LAT, MARGIN, "Converted latitude should match reference values")
    call assert_comparable(dst_y, REF_WGS84_LON, MARGIN, "Converted longitude should match reference values")
end subroutine

subroutine test_wgs84_to_utm_zone_32n
    ! Arrange
    real(kind=kind(1d0)), dimension(size(REF_WGS84_LAT)) :: dst_x = 0d0, dst_y = 0d0

    ! Act
    call transform_coordinates(WGS84_PROJ_STRING, UTM32N_PROJ_STRING, REF_WGS84_LAT, REF_WGS84_LON, dst_x, dst_y)

    ! Assert
    call assert_comparable(dst_x, REF_UTM32N_X, MARGIN, "Converted x-coordinates should match reference values")
    call assert_comparable(dst_y, REF_UTM32N_Y, MARGIN, "Converted y-coordinates should match reference values")
end subroutine

subroutine test_utm_zone_32n_to_wgs84
    ! Arrange
    real(kind=kind(1d0)), dimension(size(REF_UTM32N_X)) :: dst_x = 0d0, dst_y = 0d0

    ! Act
    call transform_coordinates(UTM32N_PROJ_STRING, WGS84_PROJ_STRING, REF_UTM32N_X, REF_UTM32N_Y, dst_x, dst_y)

    ! Assert
    call assert_comparable(dst_x, REF_WGS84_LAT, MARGIN, "Converted latitude  should match reference values")
    call assert_comparable(dst_y, REF_WGS84_LON, MARGIN, "Converted longitude should match reference values")
end subroutine

end module test_coordinate_reference_system
