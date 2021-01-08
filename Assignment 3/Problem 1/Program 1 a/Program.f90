program Program
    
    implicit none

    real :: x1, y1, x2, y2, d

    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10) x1, y1, x2, y2
    10 close (unit = 1)
    !CLosing Input file after reading from it

    d = sqrt( ( x1 - x2) ** 2 + ( y1 - y2) ** 2 ) !Using standard formula to calculate distance between points (x1, y1) and (x2, y2)

    open(unit=2 , file="Output.txt")
        !Opening Output file to print distance
        write (2, *) "Distance between the points is ", d, "units."
    close ( unit = 2 ) !Closing the file after printing in it

end program Program