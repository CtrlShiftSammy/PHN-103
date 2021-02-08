program Program2

    implicit none
    
    real :: J_Sum, x, term, fact ! J_Sum is the function J(x)
    integer :: i, t
    
    open (unit = 1, file = "Input.txt", status = 'old' )
    !Opening Input file to read Input
        read (1,*,end = 10) x
    10 close ( unit = 1 ) !Closing Input file after reading from it

    term = 1.0
    J_Sum = 0.0
    i = 1 !i is the number of the term
    
    do while ( abs(term) > 1.0e-6)
        J_Sum = J_Sum + term
        t = 1
        fact = 1.0
        do while ( t <= i) !Nested loop
            fact = fact * t
            t = t + 1
        end do
        term = (((-1) ** (i)) * (x ** (2 * i))) / (( 2 ** (2 * i)) * (fact ** 2)) !Formula for term
        i = i + 1 !Increment
    end do
    print *, J_Sum !Displaying the sum in terminal
    open (unit = 2, file = 'Output.txt', status = 'old')
    !Opening Output file to write value
        write(2, "(E13.2)") J_Sum !Writing value in the Output file

end program Program2
