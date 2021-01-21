module MatrixMultiplicationModule
    implicit none
    
    contains
        subroutine MultiplyMatrices(a,b,r1,c1,r2,c2,t1) 
            implicit none
                integer :: i, j, k, r1, c1, r2, c2
                real :: sum
                real, dimension(:,:) :: a, b
                real, dimension(:,:) :: t1
            
                sum = 0
                do i =1,r1
                    do j = 1,c2
                        do k = 1,c1
                            sum = sum + (a(i,k) * b(k,j))
                        end do
                        t1(i,j) = sum 
                        sum = 0
                    end do
                end do
            end subroutine MultiplyMatrices
    end module MatrixMultiplicationModule

program Program
    use MatrixMultiplicationModule
    implicit none
        integer :: r1, c1, r2, c2, i, j
        real, dimension(:,:), allocatable :: Matrix1, Matrix2, Product
        open (unit = 1, file = "Input1.txt", status = 'old' )
            read (1,*,end = 10) r1, c1
            allocate(Matrix1(r1, c1))
            do i = 1, r1
                read (1,*,end = 10) (Matrix1(i, j),  j = 1, c1)
            end do
        10 close (unit = 1)
        !CLosing Input file after reading from it
        open (unit = 2, file = "Input2.txt", status = 'old' )
            read (2,*,end = 10) r2, c2
            allocate(Matrix2(r2, c2))
            do i = 1, r2
                read (2,*,end = 20) (Matrix2(i, j),  j = 1, c2)
            end do
        20 close (unit = 2)
        !CLosing Input file after reading from it
        allocate(Product(r1, c2))
        open(unit = 3 , file="Output.txt")
            if ( c1 /= r2 ) then
                write (3, *) "INVALID INPUT. MATRICES ARE OF INCOMPATIBLE SIZES."
            else
                write (3, *) "Product of the given matrices is:"
                call MultiplyMatrices (Matrix1, Matrix2, r1, c1, r2, c2, Product)
                do i = 1, r1
                    do j = 1, c2
                        if ( j == c2 ) then
                            write (3, 4, advance = 'yes') Product(i, j)
                        else
                            write (3, 4, advance = 'no') Product(i, j)
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