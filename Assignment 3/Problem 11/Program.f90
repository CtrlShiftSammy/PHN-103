program Program
    implicit none
    real, dimension (6, 6) :: Matrix1, Matrix2, Sum
    integer i, j
    real trace1, trace2
    trace1 = 0.0
    trace2 = 0.0
    open (unit = 1, file = "Input1.txt", status = 'old' )
    do i = 1, 6
        read (1,*,end = 10) (Matrix1(i, j),  j = 1, 6) 
    end do
    10 close (unit = 1)
    open (unit = 1, file = "Input2.txt", status = 'old' )
    do i = 1, 6
        read (1,*,end = 20) (Matrix2(i, j),  j = 1, 6) 
    end do
    20 close (unit = 1)
        open(unit = 3 , file="Output.txt")
        write (3, *) "Sum of the given matrices is:"
        write (3, *)
        do i = 1, 6
            do j = 1, 6
                Sum(i, j) = Matrix1(i, j) + Matrix2(i, j)
                if ( i == j ) then
                    trace1 = trace1 + Matrix1(i, j)
                    trace2 = trace2 + Matrix2(i, j)
                end if
                if ( j == 6 ) then
                    write (3, 4, advance = 'yes') Sum(i, j)
                else
                    write (3, 4, advance = 'no') Sum(i, j)
                end if
                4 format (f10.1)
            end do
        end do
        write (3, *)
        write (3, 5) "Trace of Matrix 1 is ", trace1
        write (3, *)
        write (3, 5) "Trace of Matrix 2 is ", trace2
        5 format (a22, e10.3)
    close ( unit = 3 ) !Closing the file after printing in it

end program Program