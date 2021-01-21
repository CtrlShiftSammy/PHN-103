program Program
    implicit none
    integer, dimension(:), allocatable :: array
    integer n, i
    logical :: negative = .false.
    real sum, product, arithmetic_mean, geometric_mean
    write (*,*) "Enter the value of n."
    read (*,*) n
    allocate(array(n))
    open(unit = 1, file = "Input.txt", status = 'old' )
    do i = 1, n
        read (1,*,end = 10) array (i)
        if ( array(i) < 0) then
            negative = .true.
        end if
    end do
    10 close (unit = 1)

    open(unit=2 , file="Output.txt")
    if ( negative ) then
        write (2, *) "The program cannot calculate geometric mean of a negative input."
    else
        i = 1
        sum = 0.0
        product = 1.0
        do while (i < n + 1)
            sum = sum + array(i)
            product = product * array(i)
            i = i + 1
        end do
        arithmetic_mean = sum / n
        geometric_mean = (product) ** (1.0 / n)
        write (2, *) "The arithmetic mean is", arithmetic_mean
        write (2, *) "The geometric mean is", geometric_mean
    end if
    close ( unit = 2 ) !Closing the file after printing in it
    deallocate(array)
end program Program