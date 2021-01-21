program Program
    
    implicit none

    real :: eccentricity, eccentricity_a, eccentricity_b, eccentricity_c, p, r, pi
    real, dimension(3,360) :: Dist
    real, dimension(3) ::  perihelion, aphelion
    integer, dimension(360) :: t
    integer :: i, j, theta
    i = 1
    j = 1
    p = 1200.0
    pi = 4 * atan(1.0) !tan (pi/4) = 1
    perihelion = (/huge(0.0), huge(0.0), huge(0.0)/)
    aphelion = (/0.0, 0.0, 0.0/)
    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10)  eccentricity_a, eccentricity_b, eccentricity_c
    10 close (unit = 1)
    !CLosing Input file after reading from it

    open(unit=2 , file="Output1.txt")
    open(unit=3 , file="Output2.txt")
    open(unit=4 , file="Output3.txt")
    do while ( i <= 3 )
        write ((i + 1), *) "Theta   r"
        if ( i == 1) then
            eccentricity = eccentricity_a
        else if ( i == 2) then
            eccentricity = eccentricity_b
        else
            eccentricity = eccentricity_c
        end if
        do theta = 1, 360
            r = p / ( 1 - eccentricity * cos( real(theta) * 2.0 * pi / 360.0 ))
            write ((i + 1), 5) theta,"  ", r
        end do
        i = i + 1
        5 format (i3, a, f9.3)
    end do
    close ( unit = 2 ) !Closing the file after printing in it
    close ( unit = 3 ) !Closing the file after printing in it
    close ( unit = 4 ) !Closing the file after printing in it

    open (unit = 5, file = "Output1.txt", status = 'old' )
    read (5,*,end = 11)
        do i = 1, 360
            read (5,*,end = 11) t(i), Dist(1, i)
        end do
    11 close (unit = 5)
    open (unit = 6, file = "Output2.txt", status = 'old' )
    read (6,*,end = 12)
        do i = 1, 360
            read (6,*,end = 12) t(i), Dist(2, i)
        end do
    12 close (unit = 6)
    open (unit = 7, file = "Output3.txt", status = 'old' )
    read (7,*,end = 13)
        do i = 1, 360
            read (7,*,end = 13) t(i), Dist(3, i)
        end do
    13 close (unit = 7)
    do i = 1, 360
        do j = 1, 3
            if ( Dist(j, i) <= perihelion(j)) then
                perihelion(j) = Dist(j, i)
            end if
            if ( Dist(j, i) >= aphelion(j)) then
                aphelion(j) = Dist(j, i)
            end if
        end do
    end do
    open(unit=8 , file="Output.txt")
        do i = 1, 3
            if ( i == 1) then
                eccentricity = eccentricity_a
            else if ( i == 2) then
                eccentricity = eccentricity_b
            else
                eccentricity = eccentricity_c
            end if    
            write (8, 9) "The closest distance is", perihelion(i), " km for E = ", eccentricity
            write (8, 9) "The furthest distance is", aphelion(i), " km for E = ", eccentricity
            write (8, *)
            9 format (a, f9.3, a, f5.2)
        end do
    close ( unit = 8 ) !Closing the file after printing in it
end program Program