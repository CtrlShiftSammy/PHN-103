program Program
    
    implicit none

    real :: x1, y1, z1, x2, y2, z2, d, x_vector, y_vector, z_vector, x_cosine, y_cosine, z_cosine

    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10) x1, y1, z1, x2, y2, z2
    10 close (unit = 1)
    !CLosing Input file after reading from it

    x_vector = x2 - x1
    y_vector = y2 - y1
    z_vector = z2 - z1

    d = sqrt( x_vector ** 2 + y_vector ** 2 + z_vector ** 2 ) !Using standard formula to calculate distance between points (x1, y1, z1) and (x2, y2, z2)
    x_cosine = x_vector / d !Using standard formula to calculate direction cosine along X-axis
    y_cosine = y_vector / d !Using standard formula to calculate direction cosine along Y-axis
    z_cosine = z_vector / d !Using standard formula to calculate direction cosine along Z-axis

    open(unit=2 , file="Output.txt")
        !Opening Output file to print disctance and direction cosines
        write (2, *) "Distance between the points is ", d, "units."
        write (2, *) "Direction cosines of the vector are ", x_cosine, ",", y_cosine, "and", z_cosine
    close ( unit = 2 ) !Closing the file after printing in it

end program Program