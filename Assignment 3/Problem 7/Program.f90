program Program
    implicit none
        integer, dimension (3) :: N_terms
        real, dimension (3) :: Sums
        integer i, j
        open(unit = 1, file = "Input.txt", status = 'old' )
        do i = 1, 3
            read (1,*,end = 10) N_terms(i)
            Sums(i) = 0.0
        end do
        10 close (unit = 1)
        do i = 1, 3
            do j = 1, N_terms(i)
                Sums(i) = Sums(i) + ((-1.0) ** (j + 1.0)) / ( 2.0 * j - 1.0 )
            end do
        end do
        open(unit=2 , file="Output.txt")
            do i = 1, 3
                write (2, 3) "Sum of series up to", N_terms(i), "terms is", Sums(i)
                3 format (a20, i4, a10, f13.9)
            end do
         
        close ( unit = 2 ) !Closing the file after printing in it
end program Program