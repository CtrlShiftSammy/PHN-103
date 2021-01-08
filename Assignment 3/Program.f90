program Program
    implicit none
        integer theta
        real gain
        open(unit=2 , file="Output.txt")
            write (2, *) "  Antenna Gain vs Angle (deg)"
            write (2, *)
            write (2, *) "  Antenna Gain        Angle (deg)"
            write (2, *)
            do theta = 0, 90
                call Gx( gain, theta)
                write (2, 3) gain, theta
                3 format (f13.6, i13)
            end do
        close ( unit = 2 ) !Closing the file after printing in it
end program Program

subroutine Gx(g, d)
    implicit none
        real :: g
        integer :: d
        real :: pi = 4 * atan(1.0)
        if ( d == 0 ) then
            g = 0.0
        else
            g = abs( (sin(6.0  * d * pi / 180.0 )) / (6.0  * d * pi / 180.0) )
        end if
end subroutine Gx