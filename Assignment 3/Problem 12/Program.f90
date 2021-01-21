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
        integer :: r1, c1, r2, c2, r3, c3, i, j
        real, dimension(:,:), allocatable :: Matrix1, Matrix2, Matrix3, Product1, Product2
        open (unit = 1, file = "Input1.txt", status = 'old' )
            read (1,*,end = 10) r1, c1
            allocate(Matrix1(r1, c1))
            do i = 1, r1
                read (1,*,end = 10) (Matrix1(i, j) ,  j = 1, c1)
            end do
        10 close (unit = 1)
        !CLosing Input file after reading from it
        open (unit = 2, file = "Input2.txt", status = 'old' )
            read (2,*,end = 10) r2, c2
            allocate(Matrix2(r2, c2))
            do i = 1, r2
                read (2,*,end = 20) (Matrix2(i, j) ,  j = 1, c2)
            end do
        20 close (unit = 2)
        !CLosing Input file after reading from it
        open (unit = 3, file = "Input3.txt", status = 'old' )
            read (3,*,end = 30) r3, c3
            allocate(Matrix3(r3, c3))
            do i = 1, r3
                read (3,*,end = 30) (Matrix3(i, j),  j = 1, c3)
            end do
        30 close (unit = 3)
        !CLosing Input file after reading from it
        open(unit = 4 , file="Output.txt")
            if ( c1 /= r2 .or. c2 /= r3) then
                write (4, *) "INVALID INPUT. MATRICES ARE OF INCOMPATIBLE SIZES."
            else
                write (4, *) "Product of the given matrices is:"
                write (4, *)
                allocate(Product1(r1, c2))
                allocate(Product2(r1, c3))
                call MultiplyMatrices (Matrix1, Matrix2, r1, c1, r2, c2, Product1)
                call MultiplyMatrices (Product1, Matrix3, r1, c2, r3, c3, Product2)
                do i = 1, r1
                    do j = 1, c3
                        if ( j == c3 ) then
                            write (4, 4, advance = 'yes') Product2(i, j)
                        else
                            write (4, 4, advance = 'no') Product2(i, j)
                        end if
                        4 format (f18.5)
                    end do
                end do
            end if
        close ( unit = 3 ) !Closing the file after printing in it
        deallocate(Matrix1)
        deallocate(Matrix2)
        deallocate(Matrix3)
        deallocate(Product1)
        deallocate(Product2)
end program Program