program principal
    use EDDP
    use arreglos
    use SELs

    implicit none
    
    real(8) x0, x1, y0, y1
    integer(4) n, m, orden
    real(8), dimension(:, :), allocatable :: distribucion
    type(frontera), dimension(:), allocatable :: superior, inferior, izquierda, derecha
    real(8), dimension(:), allocatable :: res

    x0 = 0.
    x1 = 20.
    y0 = 0.
    y1 = 10.
    n = 501
    m = 251
    orden = (n - 1) * (m - 1)

    allocate(distribucion(1:m+1, 1:n+1))
    allocate(superior(1:n-1), inferior(1:n-1))
    allocate(izquierda(1:m-1), derecha(1:m-1))
    
    superior%valor = 0.
    inferior%valor = 0.
    izquierda%valor = 0.
    derecha%valor = 100.
    superior%tipo = DIRICHLET
    inferior%tipo = DIRICHLET
    izquierda%tipo = DIRICHLET
    derecha%tipo = DIRICHLET
    call elipticas(res, x0, x1, y0, y1, n, m, superior, inferior, izquierda, derecha, laplace, 0.00001_8)
    
    ! call mostrarVector(res)
    write (*, *)

    distribucion = generarDistribucion(n, m, x0, x1, y0, y1, res, 0._8, &
        100._8, 0._8, 100._8, superior, inferior, izquierda, derecha)
    ! call mostrarMatriz(distribucion, '(9F7.2)')
    call grabarDatos(distribucion, x0, x1, y0, y1, n, m, 'valores.dat')
    call plot('valores.dat')

    deallocate(distribucion, superior, inferior, izquierda, derecha, res)

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