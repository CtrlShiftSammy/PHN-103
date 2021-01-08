program Program
    implicit none
        integer :: r1, c1, r2, c2, i, j
        real, dimension(:,:), allocatable :: Matrix1, Matrix2, Product
        open (unit = 1, file = "Input1.txt", status = 'old' )
            read (1,*,end = 10) r1
            read (1,*,end = 10) c1
            allocate(Matrix1(r1, c1))
            do i = 1, r1
                do j = 1, c1
                    read (1,*,end = 10) Matrix1(i, j) 
                end do
            end do
        10 close (unit = 1)
        !CLosing Input file after reading from it
        open (unit = 2, file = "Input2.txt", status = 'old' )
            read (2,*,end = 10) r2
            read (2,*,end = 10) c2
            allocate(Matrix2(r2, c2))
            do i = 1, r2
                do j = 1, c2
                    read (2,*,end = 20) Matrix2(i, j) 
                end do
            end do
        20 close (unit = 2)
        !CLosing Input file after reading from it
        open(unit = 3 , file="Output.txt")
            if ( c1 /= r2 ) then
                write (3, *) "INVALID INPUT. MATRICES ARE OF INCOMPATIBLE SIZES."
            else
                write (3, *) "Product of the given matrices is:"
                allocate(Product(r1, c2))
                Product = matmul (Matrix1, Matrix2)
                do i = 1, r1
                    do j = 1, c2
                        if ( j == c2 ) then
                            write (3, 4, advance = 'yes') product(i, j)
                        else
                            write (3, 4, advance = 'no') product(i, j)
                        end if
                        4 format (f10.1)
                    end do
                end do
            end if
        close ( unit = 3 ) !Closing the file after printing in it
        deallocate(Matrix1)
        deallocate(Matrix2)
        deallocate(Product)
end program Program