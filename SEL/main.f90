program principal
    use SELs
    use arreglos

    implicit none

    integer, parameter :: orden = 4 !Modificar
    real(8) matriz(orden, orden), term_ind(orden, 1), xini(orden, 1), matriz2(3, 3)

    xini = 0
    call leerMatriz(matriz, "matriz_ejemplo1.txt")
    call leerMatriz(term_ind, "independientes_ejemplo1.txt")

    call mostrarMatriz(matriz)
    write(*, *)
    call mostrarMatriz(term_ind)
    write(*, *)

    write(*, *) "Norma M ", mNormaM(matriz)
    write(*, *) "Norma L ", mNormaL(matriz)
    write(*, *) "Norma F ", mNormaF(matriz)
    write(*, *)

    call pivotear(matriz, term_ind)
    call mostrarMatriz(matriz)
    write(*, *)
    call mostrarMatriz(term_ind)
    write(*, *)

    write(*, *) "Solucion Gauss:"
    call mostrarMatriz(solucionGauss(matriz, term_ind))
    write(*, *)

    write(*, *) "Solucion Gauss iterativo:"
    call mostrarMatriz(refinamientoIter(matriz, term_ind, 0.000000000001_8, solucionGauss, mNormaM))
    write(*, *)

    write(*, *) "Jacobi:"
    call mostrarMatriz(jacobi(matriz, term_ind, xini, 0.000000000001_8))
    write(*, *)

    write(*, *) "Gauss Seidel:"
    call mostrarMatriz(gaussSeidel(matriz, term_ind, xini, 0.000000000001_8))
    write(*, *)

    write(*, *) "Relajacion:"
    call mostrarMatriz(relajacion(matriz, term_ind, xini, 0.000000000001_8))
    write(*, *)

    matriz2(1, 1) = 1.
    matriz2(1, 2) = 0.
    matriz2(1, 3) = 0.
    matriz2(2, 1) = 2.
    matriz2(2, 2) = 1.
    matriz2(2, 3) = 0.
    matriz2(3, 1) = -1.
    matriz2(3, 2) = -2.
    matriz2(3, 3) = -1.
    write(*, *) "Matriz original:"
    call mostrarMatriz(matriz2)
    write(*, *)
    write(*, *) "Matriz inversa:"
    call mostrarMatriz(matrizInversa(matriz2))
    write(*, *)
    write(*, *) "Chequeo matriz inversa:"
    call mostrarMatriz(matmul(matriz2, matrizInversa(matriz2)))

contains

    subroutine Menu(matriz, term_ind, xini)
        real(8) matriz(orden, orden), term_ind(orden, 1), xini(orden, 1)
        logical :: op
        integer select

        op = .true.

        do while(op)
            write(*, *) "Seleccione una Opcion:"
            write(*, *) "1 - Gauss"
            write(*, *) "2 - Gauss con Refinamiento Iterativo"
            write(*, *) "3 - Jacobi"
            write(*, *) "4 - Gauss Seidel"
            write(*, *) "5 - Matriz inversa"
            write(*, *) "0 - Salir"

            read (*, *) select

            select case (select)
                case (1)
                    write(*,*) "----"
                    write(*,*) "Gaus"
                    write(*,*) "----"
                    call mostrarMatriz(solucionGauss(matriz, term_ind))
                case (2)
                    write(*,*) "-------------------------------"
                    write(*,*) "Gaus con Refinamiento Iterativo"
                    write(*,*) "-------------------------------"
                    call mostrarMatriz(refinamientoIter(matriz, term_ind, 0.000000000001_8, solucionGauss, mNormaM))
                case (3)
                    write(*,*) "------"
                    write(*,*) "Jacobi"
                    write(*,*) "------"
                    call mostrarMatriz(jacobi(matriz, term_ind, xini, 0.000000000001_8))
                case (4)
                    write(*,*) "------------"
                    write(*,*) "Gauss Seidel"
                    write(*,*) "------------"
                    call mostrarMatriz(gaussSeidel(matriz, term_ind, xini, 0.000000000001_8))
                case (5)
                    write(*, *) "--------------"
                    write(*, *) "Matriz Inversa"
                    write(*, *) "--------------"
                    call mostrarMatriz(matrizInversa(matriz))
                case (0)
                    write(*, *) "Saliendo"
                    op = .false.
            end select

        end do

    end subroutine Menu


end program principal
