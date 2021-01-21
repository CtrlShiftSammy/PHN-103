program Program
    implicit none
        integer :: a, b, i, j
        real, dimension(3, 3) :: Matrix
        real, dimension(2, 2) :: SubMatrix
        real :: det2, det3
        det3 = 0.0
        open (unit = 1, file = "Input.txt", status = 'old' )
            do i = 1, 3
                read (1,*,end = 10) (Matrix(i, j),  j = 1, 3)
            end do
        10 close (unit = 1)
        !CLosing Input file after reading from it
        do i = 1, 3
            a = i + 1
            b = i + 2
            if ( a > 3) then
                a = a - 3
            end if
            if ( b > 3 ) then
                b = b - 3
            end if
            SubMatrix(1,1) = Matrix(2, a)
            SubMatrix(1,2) = Matrix(2, b)
            SubMatrix(2,1) = Matrix(3, a)
            SubMatrix(2,2) = Matrix(3, b)
            det2 = determ(SubMatrix)
            det3 = det3 + det2 * Matrix(1, i)
        end do
        open(unit=2 , file="Output.txt")
            write (2, *) "The determinant of given matrix is", det3
        close ( unit = 2 ) !Closing the file after printing in it
contains
function determ(mat)
    implicit none
    real, dimension(2, 2) :: mat
    real :: determ
    determ = mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1)
end function determ
end program Program
