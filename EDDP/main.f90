program principal
    use EDDP
    use arreglos
    use SELs

    implicit none
    
    real(8) x0, x1, y0, y1
    integer(4) n, m, orden
    real(8), dimension(:, :), allocatable :: mat, distribucion
    real(8), dimension(:), allocatable :: superior, inferior, izquierda, derecha
    type(condicion), dimension(:), allocatable :: superior2, inferior2, izquierda2, derecha2
    real(8), dimension(:, :), allocatable :: term_ind, xini, res

    x0 = 0.
    x1 = 20.
    y0 = 0.
    y1 = 10.
    n = 8
    m = 4
    orden = (n - 1) * (m - 1)

    allocate(mat(1:orden, 1:orden), distribucion(1:m+1, 1:n+1))
    allocate(term_ind(1:orden, 1), xini(1:orden, 1), res(1:orden, 1))
    allocate(superior(1:n-1), inferior(1:n-1))
    allocate(izquierda(1:m-1), derecha(1:m-1))
    allocate(superior2(1:n-1), inferior2(1:n-1))
    allocate(izquierda2(1:m-1), derecha2(1:m-1))

    superior = 0.
    inferior = 0.
    izquierda = 0.
    derecha = 100.
    !call generarSistema(mat, term_ind, x0, x1, y0, y1, n, m, superior, inferior, izquierda, derecha, laplace)
    ! call mostrarMatriz(mat)
    ! write (*, *)
    ! call mostrarMatriz(term_ind)
    ! write (*, *)
    
    superior2%valor = 0.
    inferior2%valor = 0.
    izquierda2%valor = 0.
    derecha2%valor = 100.
    superior2%tipo = NEUMANN
    inferior2%tipo = DIRICHLET
    izquierda2%tipo = NEUMANN
    derecha2%tipo = DIRICHLET
    call generarSistema2(mat, term_ind, x0, x1, y0, y1, n, m, superior2, inferior2, izquierda2, derecha2, laplace)

    call mostrarMatriz(mat, '(21F7.2)')
    write (*, *)
    call mostrarMatriz(term_ind, '(F7.2)')
    write (*, *)

    xini = 0.
    res = gaussSeidel(mat, term_ind, xini, 0.000001_8)
    call mostrarMatriz(res)
    write (*, *)

    ! distribucion = generarDistribucion(n, m, res(:, 1), 0._8, 100._8, 0._8, 100._8, superior, inferior, izquierda, derecha)
    ! call mostrarMatriz(distribucion)

    distribucion = generarDistribucion2(n, m, x0, x1, y0, y1, res(:, 1), 0._8, &
        100._8, 0._8, 100._8, superior2, inferior2, izquierda2, derecha2)
    call mostrarMatriz(distribucion, '(9F7.2)')
    call grabarDatos(distribucion, x0, x1, y0, y1, n, m, 'valores.dat')
    call system('gnuplot -persist matriz.p')

    deallocate(mat, distribucion, superior, inferior, izquierda, derecha, term_ind, xini, res)
    deallocate(superior2, inferior2, derecha2, izquierda2)

contains

    function f(x, y)
        real(8), intent(in) :: x, y
        real(8) f

        f = x / y
    end function

    subroutine plot(archivo)
        character(len=*), intent(in) :: archivo

        open(unit=2, file="temporal.p", access='SEQUENTIAL', status='REPLACE')
        write(2, *) 'set title "Distribución de Temperaturas en una placa rectangular"'
        write(2, *) 'set xlabel "x"'
        write(2, *) 'set ylabel "y"'
        write(2, *) 'set pm3d map'
        write(2, *) 'set nokey'
        write(2, *) 'splot "', archivo, '"'
        call system('gnuplot -persist temporal.p')
        close(2, STATUS='DELETE')
    end subroutine plot

end program principal