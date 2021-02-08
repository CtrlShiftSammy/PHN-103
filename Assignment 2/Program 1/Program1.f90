program Program1
    
    implicit none

    real :: a, b, c, root1, root2
    !a, b and c are the coefficients of ax2 + bx + c = 0

    open (unit = 1, file = "Input.txt", status = 'old' )
        read (1,*,end = 10) a, b, c
    10 close (unit = 1)
    !CLosing Input file after reading from it

    open(unit=2 , file="Output.txt")
    !Opening Output file to print roots
    if ( (b**2 - 4*a*c) < 0 ) then
        write (2, *) "INVALID INPUT" !in case determinant is less than 0, taking a square root will give a complex value, which can't be used as root1 and root2 are declared as real
    else
        !Using Sridharacharya Formula
        root1 = (-b + sqrt((b*b) - (4*a*c)))/(2*a)
        root2 = (-b - sqrt((b*b) - (4*a*c)))/(2*a)
        write (2, "(E13.2)") root1, root2 !Printing the final value
    end if    
    
    close ( unit = 2 ) !Closing the file after printing in it

end program Program1


