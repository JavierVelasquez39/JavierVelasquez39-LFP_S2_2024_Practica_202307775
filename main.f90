module Automodule
    
    type :: Equipo
        character(len=256) :: nombre
        integer :: cantidad
        real :: precio
        character(len=256) :: ubicacion
        contains
            procedure :: inicializar
            procedure :: agregarStock
            procedure :: quitarStock
    end type Equipo

    contains

    ! Subrutina para inicializar el tipo de dato Equipo
    subroutine inicializar(this, nombre, cantidad, precio, ubicacion)
        class(Equipo), intent(inout) :: this
        character(len=256), intent(in) :: nombre
        integer, intent(in) :: cantidad
        real, intent(in) :: precio
        character(len=256), intent(in) :: ubicacion

        this%nombre = nombre
        this%cantidad = cantidad
        this%precio = precio
        this%ubicacion = ubicacion
    end subroutine inicializar

    ! Subrutina para agregar stock al tipo de dato Equipo
    subroutine agregarStock(this, cantidad)
        class(Equipo), intent(inout) :: this
        integer, intent(in) :: cantidad
        this%cantidad = this%cantidad + cantidad
    end subroutine agregarStock

    ! Subrutina para quitar stock al tipo de dato Equipo
    subroutine quitarStock(this, cantidad)
        class(Equipo), intent(inout) :: this
        integer, intent(in) :: cantidad

        integer :: resultante
        resultante = this%cantidad - cantidad

        if(resultante>=0)then 
            this%cantidad = this%cantidad - cantidad
        else
            print *, "No se puede quitar esa cantidad de stock, stock negativo"
        endif

    end subroutine quitarStock

end module Automodule

module global_vars
    use Automodule
    integer :: n=1
    integer :: i
    type(Equipo), dimension(150) :: inventario
end module global_vars

program main
    use Automodule
    use global_vars
    implicit none
    integer :: op ! Variable para leer la opción

    print *, "Practica 1 - Lenguajes Formales y de Programacion"
    do
        print *, "-------------------------"
        print *, "# Sistema de inventario: "
        print *, "-------------------------"
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar Instrucciones de Movimiento"
        print *, "3. Crear informe de Inventario"
        print *, "4. Salir"
        print *, "5. Eliminar elementos en pantalla"
        print *, "Ingrese una opcion"
        read *, op ! Leer la opcion del usuario

        select case (op)
            case (1)
                call vaciarInventario() ! Limpia el inventario antes de cargarlo
                call cargarInventarioArchivo()
                print *, "Presione cualquier tecla para regresar al menú principal..."
                read *
            case (2)
                call accionesArchivo()
                print *, "Presione cualquier tecla para regresar al menú principal..."
                read *
            case (3)
                call crearInformeInventario()
            case (4)
                print *, "Saliendo del programa..."
                do i=1, n-1
                    print *, "Equipo: ", trim(inventario(i)%nombre)
                    print *, "Cantidad: ", inventario(i)%cantidad
                    print *, "Precio: ", inventario(i)%precio
                    print *, "Ubicacion: ", trim(inventario(i)%ubicacion)
                end do
                exit
            case (5)
                call system("cls") 
            case default
                print *, "Opcion no valida, presiona cualquier tecla para continuar"
                read *
        end select
    end do
end program main

subroutine cargarInventarioArchivo()
    use Automodule
    use global_vars
    implicit none

    integer :: iunit, ios, pos, cantidad_int
    real :: precio_real
    character(len=256) :: nombre, cantidad, precio, ubicacion, linea, comando
    character(len=256) :: archivoEntrada

    print *, "Ingrese el nombre del archivo de entrada: "
    read *, archivoEntrada

    ! Asignar unidad al archivo
    iunit = 10

    open(unit=iunit, file=trim(archivoEntrada), status="old", action="read", iostat=ios)

    if (ios /= 0) then
        print *, "Error al abrir el archivo: ", trim(archivoEntrada)
        stop
    endif

    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)
        
        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, " ")
        if (pos > 0) then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))

            ! Extraer nombre
            pos = index(linea, ";")
            if (pos > 0) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))
                print *, "Nombre: ", nombre  ! <--- Depuración

                ! Extraer cantidad
                pos = index(linea, ";")
                if (pos > 0) then
                    cantidad = linea(1:pos-1)
                    linea = trim(linea(pos+1:))
                    read(cantidad, '(I10)', iostat=ios) cantidad_int
                    print *, "Cantidad: ", cantidad_int  ! <--- Depuración

                    ! Extraer precio
                    pos = index(linea, ";")
                    if (pos > 0) then
                        precio = linea(1:pos-1)
                        linea = trim(linea(pos+1:))
                        read(precio, '(F10.2)', iostat=ios) precio_real
                        print *, "Precio: ", precio_real  ! <--- Depuración

                        ! Extraer ubicacion
                        ubicacion = linea
                        print *, "Ubicacion: ", ubicacion  ! <--- Depuración

                        if (comando == "crear_equipo") then
                            call crearInventario(nombre, cantidad_int, precio_real, ubicacion)
                        endif
                    endif
                endif
            endif
        endif
    end do
    close(unit=iunit)
end subroutine cargarInventarioArchivo

subroutine crearInventario(nombre, cantidad, precio, ubicacion)
    use Automodule
    use global_vars
    implicit none
    ! dummy arguments
    character(len=256), intent(in) :: nombre
    integer, intent(in) :: cantidad
    real, intent(in) :: precio
    character(len=256), intent(in) :: ubicacion

    type(Equipo) :: equipoNuevo
    call equipoNuevo%inicializar(nombre, cantidad, precio, ubicacion)
    inventario(n) = equipoNuevo
    n = n + 1
    print *, "Inventario actualizado. n = ", n  ! <--- Depuración
end subroutine crearInventario

subroutine AccionesArchivo()
    use Automodule
    use global_vars

    integer :: iunit, ios, pos, cantidad_int
    character(len=256) :: nombre, ubicacion, linea, comando
    character(len=256) :: archivoAcciones

    print *, "Ingrese la ruta del archivo de instrucciones de movimientos:"
    read *, archivoAcciones

    ! Archivo en modo lectura
    iunit = 11
    open(unit=iunit, file=trim(archivoAcciones), status="old", action="read", iostat=ios)

    ! Verificar si hay error al abrir el archivo
    if (ios /= 0) then
        print *, "Error al abrir el archivo: ", archivoAcciones
        stop
    end if

    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)
        print *, "Leyendo línea: ", linea

        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, ' ')
        if (pos > 0) then
            comando = trim(linea(1:pos-1))
            linea = trim(linea(pos+1:))
            ! Separar por ;
            pos = index(linea, ';')
            if (pos > 0) then
                nombre = trim(linea(1:pos-1))
                linea = trim(linea(pos+1:))
                ! Siguiente atributo
                pos = index(linea, ';')
                if (pos > 0) then
                    read(linea(1:pos-1), '(I10)', iostat=ios) cantidad_int
                    ubicacion = trim(linea(pos+1:))

                    if (comando == "agregar_stock") then
                        call agregar_stock(nombre, cantidad_int, ubicacion)
                    else if (comando == "eliminar_equipo") then
                        call eliminar_equipo(nombre, cantidad_int, ubicacion)
                    end if
                end if
            end if
        end if
    end do

    close(unit=iunit)
end subroutine AccionesArchivo

subroutine agregar_stock(nombre, cantidad, ubicacion)
    use Automodule
    use global_vars
    ! use dummy arguments
    character(len=256), intent(in) :: nombre
    integer, intent(in) :: cantidad
    character(len=256), intent(in) :: ubicacion
    logical :: encontrado = .false.

    do i=1, n-1 
        if(inventario(i)%nombre == nombre .and. inventario(i)%ubicacion == ubicacion) then
            call inventario(i)%agregarStock(cantidad)
            encontrado = .true.
            print *, "Agregado stock: ", cantidad, " de ", trim(nombre), " en ", trim(ubicacion)
        endif
    end do

    if (.not. encontrado) then
        print *, "No se encontro el equipo ", trim(nombre), " en la ubicacion ", trim(ubicacion)
    endif
end subroutine agregar_stock

subroutine eliminar_equipo(nombre, cantidad, ubicacion)
    use Automodule
    use global_vars
    !dummy
    character(len=256), intent (in) :: nombre
    integer, intent (in) :: cantidad
    character(len=256), intent (in) :: ubicacion
    logical :: encontrado = .false.

    do i=1, n-1
        if(trim(inventario(i)%nombre) == trim(nombre) .and. trim(inventario(i)%ubicacion) == trim(ubicacion)) then
            call inventario(i)%quitarStock(cantidad)
            encontrado = .true.
            print *, "Stock reducido de ", nombre, " en ", ubicacion
            print *, "Nueva cantidad: ", inventario(i)%cantidad
        end if
    end do

    if (.not. encontrado) then
        print *, "No se encontró el producto ", nombre, " en la ubicación ", ubicacion
    end if
end subroutine eliminar_equipo

subroutine crearInformeInventario()
    use Automodule
    use global_vars

    integer :: iunit, ios
    real :: valor_total

    iunit = 20

    ! Abre el archivo en modo escritura, reemplazando el contenido si ya existe
    open(unit=iunit, file="informe.txt", status="replace", action="write", iostat=ios)

    ! Verifica si el archivo se abrió correctamente
    if (ios /= 0) then
        print *, "Error al abrir el archivo de informe.txt"
        stop
    endif 

    ! Encabezado del informe 
    write(iunit, '(A30, A20, A20, A20, A20)') "Nombre", "Cantidad", "Precio Unitario", "Valor total", "Ubicacion"

    ! Escribe el contenido del inventario en el archivo
    do i=1, n-1
        valor_total = inventario(i)%cantidad * inventario(i)%precio
        write(iunit, '(A30, I10, F10.2, F10.2, A20)') trim(inventario(i)%nombre), inventario(i)%cantidad, inventario(i)%precio, valor_total, trim(inventario(i)%ubicacion)
    end do 

    ! Cierra el archivo
    close(unit=iunit)
    print *, "Informe de inventario creado en informe.txt"
    print *, "--------------------------------------------"
    print *, "Presione una tecla para continuar: "
    read * 
end subroutine crearInformeInventario

subroutine vaciarInventario()
    use global_vars
    n = 1 
end subroutine vaciarInventario