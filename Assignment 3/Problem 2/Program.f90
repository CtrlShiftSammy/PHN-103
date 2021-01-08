program Program
    
    implicit none

    real :: eccentricity, eccentricity_a, eccentricity_b, eccentricity_c, theta, p, r, pi, perihelion, aphelion
    integer :: i = 1
    p = 1200
    pi = 4 * atan(1.0) !tan (pi/4) = 1
    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10)  eccentricity_a, eccentricity_b, eccentricity_c, theta
    10 close (unit = 1)
    !CLosing Input file after reading from it

    open(unit=2 , file="Output.txt")
        !Opening Output file to print distance of the satellite from the centre of the Earth
        do while ( i <= 3 )
            if ( i == 1) then
                eccentricity = eccentricity_a
            else if ( i == 2) then
                eccentricity = eccentricity_b
            else
                eccentricity = eccentricity_c
            end if
            r = p / ( 1 - eccentricity * cos( theta * 2 * pi / 360 ))
            perihelion = p / ( 1 + eccentricity)
            aphelion = p / ( 1 - eccentricity)
            write (2, 3) "Distance of satellite at θ =", theta, "° and ∈ of", eccentricity, "is", r, " kilometres."
            write (2, 4) "Closest distance of satellite at ∈ of", eccentricity, "is", perihelion, " kilometres."
            write (2, 4) "Furthest distance of satellite at ∈ of", eccentricity, "is", aphelion, " kilometres."
            3 format ( a30, f5.1, a13, f5.2, a3, f10.4, a12)
            4 format ( a30, f5.2, a3, f10.3, a12)
            write (2, *)
            i = i + 1
        end do
        write (2, *) "Try using a different value of θ in Input.txt"
    close ( unit = 2 ) !Closing the file after printing in it

end program Program