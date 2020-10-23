program principal
    use EDOs

    implicit none

    integer(4), parameter :: cant_ec = 1 !Modificar
    character(len=*), parameter :: archivo = "datos.dat"
    real(8) v(0:cant_ec)

    !ACA SE TOQUETEA EL VECTOR
    v(0) = -5.
    v(1) = 25.

    call menu(v, archivo)
    !call iterar(eulerSimple, fp, v, h, estrategia1, tol, xf, archivo)
    !call plot(archivo)
    !call iterar(eulerModificado, fp, v, h, estrategia1, tol, xf, archivo)
    !call plot(archivo)
    !call iterar(eulerMejorado, fp, v, h, estrategia1, tol, xf, archivo)
    !call plot(archivo)
    !call iterar(rk, fp, v, h, estrategia1, tol, xf, archivo)
    !call plot(archivo)
    !call iterar(rkf, fp, v, h, estrategia2, tol, xf, archivo)
    !call plot(archivo)
    
contains

    function f(v)
        intent (in) :: v
        real(8) v(0:cant_ec), f
        
        f = v(0)**2     ! Cambiar la funcion aca
    end function f

    function fp(v)      ! Cambiar funcion aca
        intent (in) :: v
        real(8) v(0:), fp(0:size(v) - 1)

        fp(0) = 1       ! Variable independiente
        fp(1) = 2.*v(0) 
    end function fp

    subroutine plot(archivo)
        intent(in) :: archivo
        character (LEN=*) :: archivo

        open(unit=2, file="temporal.p", access='SEQUENTIAL', status='REPLACE')
        write(2, *) "set autoscale"
        write(2, *) "unset log"
        write(2, *) "unset label"
        write(2, *) "set xtic auto"
        write(2, *) "set ytic auto"
        write(2, *) "set title 'E2'"
        write(2, *) "set xlabel 'x'"
        write(2, *) "set ylabel 'f(x)'"
        write(2, *) "plot '", archivo, "' using 1:2 title 'metodo' with linespoints"
        call system('gnuplot -persist temporal.p')
        close(2, STATUS='DELETE')
    end subroutine plot

    subroutine menu(v, archivo)
        logical :: op
        integer :: select
        procedure(metodo), pointer :: met
        real(8) v(0:)
        character (LEN=*) :: archivo
        
        op = .true. 
        do while(op)
            write(*, *) "Seleccione un Metodo:"
            write(*, *) "1 - Euler Simple"
            write(*, *) "2 - Euler Modificado"
            write(*, *) "3 - Euler Mejorado"
            write(*, *) "4 - Runge Kutta 4"
            write(*, *) "5 - Runge Kutta Felhberg"
            write(*, *) "0 - Salir"

            read(*, *) select

            select case (select)
                case (1)
                    write(*, *) "------------"
                    write(*, *) "Euler Simple"
                    write(*, *) "------------"
                    met => eulerSimple
                    call submenuMetodo(met, v, archivo)
                case (2)
                    write(*, *) "----------------"
                    write(*, *) "Euler Modificado"
                    write(*, *) "-----------------"
                    met => eulerModificado
                    call submenuMetodo(met, v, archivo)
                case (3)
                    write(*, *) "--------------"
                    write(*, *) "Euler Mejorado"
                    write(*, *) "--------------"
                    met => eulerMejorado
                    call submenuMetodo(met, v, archivo)
                case (4)
                    write(*, *) "-------------"
                    write(*, *) "Runge Kutta 4"
                    write(*, *) "-------------"
                    met => rk
                    call submenuMetodo(met, v, archivo)
                case (5)
                    write(*, *) "--------------------"
                    write(*, *) "Runge Kutta Felhberg"
                    write(*, *) "--------------------"
                    met => rkf
                    call submenuMetodo(met, v, archivo)
                case (0)
                    write(*, *) "Saliendo"
                    op = .false.
            end select

        end do
    end subroutine menu

    subroutine submenuMetodo(met, v, archivo)
        procedure(metodo) :: met
        procedure(pasoConCambio), pointer :: estrategia
        logical :: op
        integer :: select, n, strat
        real(8) h, xf, tol, v(0:)
        character (LEN=*) :: archivo

        op = .true.
        do while (op)
            write(*, *) "Seleccione una opcion:"
            write(*, *) "1 - Iterar cantidad de veces"
            write(*, *) "2 - Iterar cantidad de veces con tolerancia"
            write(*, *) "3 - Iterar hasta un valor final"
            write(*, *) "4 - Iterar hasta un valor final con tolerancia"
            write(*, *) "0 - Menu anterior (Eleccion de Metodo)"

            read(*, *) select

            select case (select)
                case (1)
                    write(*, *) "-----------------"
                    write(*, *) "Cantidad de Veces"
                    write(*, *) "-----------------"

                    write(*, *) "Ingrese paso h:"
                    read(*, *) h
                    

                    write(*, *) "Ingrese cant repeticiones:"
                    read(*, *) n

                    call iterar(met, fp, v, h, n, archivo)

                case (2)
                    write(*, *) "--------------------------------"
                    write(*, *) "Cantidad de Veces con Tolerancia"
                    write(*, *) "--------------------------------"

                    write(*, *) "Ingrese paso h:"
                    read(*, *) h

                    write(*, *) "Ingrese cant repeticiones:"
                    read(*, *) n

                    write(*, *) "Ingrese tolerancia:"
                    read(*, *) tol

                    strat = 0
                    do while(strat /= 1 .and. strat /= 2)
                        write(*,*) "Elija estrategia 1 o 2"
                        read(*, *) strat
                    end do

                    if (strat == 1) then
                        estrategia => estrategia1
                    else
                        estrategia => estrategia2
                    end if

                    call iterar(met, fp, v, h, estrategia, tol, n, archivo)

                case (3)
                    write(*, *) "-----------"
                    write(*, *) "Valor Final"
                    write(*, *) "-----------"

                    write(*, *) "Ingrese paso h:"
                    read(*, *) h
                    

                    write(*, *) "Ingrese x final:"
                    read(*, *) xf

                    call iterar(met, fp, v, h, xf, archivo)
                case (4)
                    write(*, *) "--------------------------"
                    write(*, *) "Valor Final con Tolerancia"
                    write(*, *) "--------------------------"
                    
                    write(*, *) "Ingrese paso h:"
                    read(*, *) h

                    write(*, *) "Ingrese x final:"
                    read(*, *) xf

                    write(*, *) "Ingrese tolerancia:"
                    read(*, *) tol

                    strat = 0
                    do while(strat /= 1 .and. strat /= 2)
                        write(*,*) "Elija estrategia 1 o 2"
                        read(*, *) strat
                    end do

                    if (strat == 1) then
                        estrategia => estrategia1
                    else
                        estrategia => estrategia2
                    end if

                    call iterar(met, fp, v, h, estrategia, tol, xf, archivo)

                case (0)
                    write(*, *) "Menu anterior"
                    op = .false.
            end select
        end do

    end subroutine submenuMetodo

end program principal