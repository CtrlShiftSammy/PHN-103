program Program

    implicit none
        integer, dimension(:), allocatable :: raw_array, array
        real sum, product, arithmetic_mean, geometric_mean
        integer i_max, i, j, n
        sum = 0.0
        product = 1.0
        i_max = 100
        n = 1
        allocate(raw_array(i))
        open(unit = 1, file = "Input.txt", status = 'old' )
            i = 1
            do while (i < i_max)
                read (1,*,end = 10) raw_array (i)
                if ( raw_array(i) < 0) then
                    i = i - 1
                    n = n - 1
                end if
                if ( raw_array(i) /= 0) then
                    n = i
                end if
                i = i + 1
            end do
        10 close (unit = 1)
        allocate(array(n))
         do i = 1, n
             array(i) = raw_array(i)
         end do
        deallocate(raw_array)
        do i = 1, n
            sum = sum + array(i)
            product = product * array(i)
        end do
        arithmetic_mean = sum / n
        geometric_mean = (product) ** (1.0 / n)
        open(unit=2 , file="Output.txt")
        write (2, *) "The arithmetic mean is", arithmetic_mean
        write (2, *) "The geometric mean is", geometric_mean
        close ( unit = 2 ) !Closing the file after printing in it

        deallocate(array)
end program Program

