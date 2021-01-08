program Program
    implicit none
    real wavelength
    integer m, n
    open(unit = 1 , file="Output.txt")
        write (1, *) "Value of m    Value of n  Wavelength in Å"
        do m = 2, 50
            do n = 1, m-1
                wavelength = 911.8 / (( 1.0 / n**2.0 ) - ( 1.0 / m**2.0 ))
                write (1, 2) m, n, wavelength
                2 format (i10, i10, f15.3)
            end do
        end do
    close ( unit = 1 ) !Closing the file after printing in it
end program Program