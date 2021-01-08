program Program
    implicit none
        integer R
        real S
        open(unit = 1 , file="Output.txt")
        write (1, *) "R Value    Safe Value"
        do R = 25, 250, 25
            if ( R < 120 ) then
                S = 17000.0 - 0.485 * R ** 2.0
            else
                S = 18000.0 / ( 1 + (( R ** 2.0) / 18000.0))
            end if
            write (1, 2) R, S
            2 format (i5, f16.4)
        end do
        close ( unit = 1 ) !Closing the file after printing in it
end program Program