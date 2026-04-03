! Fortran JSON AST
module fjson
    use fjson_tokenizer
    implicit none

    type, public :: json_node
        integer :: value_int = 0
        real :: value_float = 0.0
        logical :: value_bool = .false.
        character(len=:), allocatable :: value_string
        character(len=:), allocatable :: node_type  ! INT|FLOAT|BOOL|STRING|NULL|OBJECT|ARRAY|ERROR
        character(len=:), allocatable :: name  ! Object property name
        integer :: child_nodes_count
        type(json_node), dimension(:), allocatable :: child_nodes
        contains
            procedure append_child_node
            procedure create_number_node
            procedure create_string_node
            procedure create_bool_node
            procedure create_array_node
            procedure create_object_node
    end type json_node

    contains
        function parse_json(s) result(res)
            character(len=*), intent(in) :: s
            type(json_node) :: res
            integer :: pos
            type(token_t) :: tok

            pos = 1
            res = parse_value(s, pos)

            call next_token(s, pos, tok)
            if (tok%kind /= token_type_eof) then
                res%node_type = "ERROR"
                res%value_string = "Trailing characters after valid JSON"
                res%child_nodes_count = 0
                if (allocated(res%child_nodes)) deallocate(res%child_nodes)
            end if
        end function parse_json

        recursive function parse_value(s, pos) result(node)
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos
            type(json_node) :: node
            type(token_t) :: tok

            call next_token(s, pos, tok)

            select case (tok%kind)
                case (token_type_string)
                    call node%create_string_node(tok%text)

                case (token_type_number)
                    call node%create_number_node(tok%text)

                case (token_type_bool)
                    call node%create_bool_node(tok%text)

                case (token_type_null)
                    call node%create_bool_node(tok%text)

                case (token_type_lbracket)
                    call parse_array_after_open(node, s, pos)

                case (token_type_lbrace)
                    call parse_object_after_open(node, s, pos)

                case default
                    node%node_type = "ERROR"
                    node%value_string = tok%text
                    node%child_nodes_count = 0
            end select
        end function parse_value

        recursive subroutine parse_array_after_open(this, s, pos)
            class(json_node), intent(inout) :: this
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos

            type(token_t) :: tok
            type(json_node) :: child
            integer :: save_pos

            this%node_type = "ARRAY"
            this%value_string = ""
            this%child_nodes_count = 0
            if (allocated(this%child_nodes)) deallocate(this%child_nodes)

            ! Check for empty array: []
            save_pos = pos
            call next_token(s, pos, tok)
            if (tok%kind == token_type_rbracket) return

            ! Not empty
            pos = save_pos

            do
                child = parse_value(s, pos)
                call this%append_child_node(child)

                call next_token(s, pos, tok)
                select case (tok%kind)
                case (token_type_comma)
                    cycle
                case (token_type_rbracket)
                    exit
                case default
                    this%node_type = "ERROR"
                    this%value_string = "Expected ',' or ']' in array"
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end select
            end do
        end subroutine parse_array_after_open

        recursive subroutine parse_object_after_open(this, s, pos)
            class(json_node), intent(inout) :: this
            character(len=*), intent(in) :: s
            integer, intent(inout) :: pos

            type(token_t) :: tok
            type(json_node) :: child
            character(len=:), allocatable :: key
            integer :: save_pos

            this%node_type = "OBJECT"
            this%value_string = ""
            this%child_nodes_count = 0
            if (allocated(this%child_nodes)) deallocate(this%child_nodes)

            ! Check for empty object: {}
            save_pos = pos
            call next_token(s, pos, tok)
            if (tok%kind == token_type_rbrace) return

            ! Not empty
            pos = save_pos

            do
                ! Key must be a string
                call next_token(s, pos, tok)
                if (tok%kind /= token_type_string) then
                    this%node_type = "ERROR"
                    this%value_string = "Expected string key in object"
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end if

                key = tok%text(2:len(tok%text)-1)

                ! Must have colon
                call next_token(s, pos, tok)
                if (tok%kind /= token_type_colon) then
                    this%node_type = "ERROR"
                    this%value_string = "Expected ':' after object key"
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end if

                child = parse_value(s, pos)
                child%name = key
                call this%append_child_node(child)

                call next_token(s, pos, tok)
                select case (tok%kind)
                case (token_type_comma)
                    cycle
                case (token_type_rbrace)
                    exit
                case default
                    this%node_type = "ERROR"
                    this%value_string = "Expected ',' or '}' in object"
                    if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                    this%child_nodes_count = 0
                    return
                end select
            end do
        end subroutine parse_object_after_open

        subroutine append_child_node(this, child)
            class(json_node) :: this, child
            type(json_node), dimension(:), allocatable :: temp
            if (.not. allocated(this%child_nodes)) then
                allocate(this%child_nodes(256))
            elseif(size(this%child_nodes) - this%child_nodes_count == 0) then
                allocate(temp(this%child_nodes_count * 2))
                temp(1:size(this%child_nodes)) = this%child_nodes(1:size(this%child_nodes))
                call move_alloc(from=temp,to=this%child_nodes)
            end if
            this%child_nodes_count = this%child_nodes_count + 1
            this%child_nodes(this%child_nodes_count) = child
        end subroutine append_child_node

        subroutine create_number_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: err
            this%child_nodes_count = 0
            if(index(s,".") /= 0) then
                this%node_type = "FLOAT"
                this%value_string = s
                read(s,*,iostat=err) this%value_float
                if (err /= 0) this%node_type = "ERROR"
            else
                this%node_type = "INT"
                this%value_string = s
                read(s,*,iostat=err) this%value_int
                if (err /= 0) this%node_type = "ERROR"
            end if
        end subroutine create_number_node

        subroutine create_string_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            character :: first, last
            this%child_nodes_count = 0
            first = s(1:1)
            last = s(len(s):len(s))
            if(first == '"' .and. last == '"') then
                this%node_type = "STRING"
                this%value_string = s(2:len(s)-1)
            else
                this%node_type = "ERROR"
                this%value_string = s
            end if
        end subroutine create_string_node

        subroutine create_bool_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            this%child_nodes_count = 0
            if(s == "true" .or. s == "false") then
                this%node_type = "BOOL"
                this%value_bool = (s == "true")
                this%value_string = s
            elseif(s == "null") then
                this%node_type = "NULL"
                this%value_string = s
            else
                this%node_type = "ERROR"
                this%value_string = s
            end if
        end subroutine create_bool_node

        subroutine create_array_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: pos
            type(token_t) :: tok

            pos = 1
            call next_token(s, pos, tok)
            if (tok%kind /= token_type_lbracket) then
                this%node_type = "ERROR"
                this%value_string = "Array must start with '['"
                this%child_nodes_count = 0
                if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                return
            end if

            call parse_array_after_open(this, s, pos)
        end subroutine create_array_node

        subroutine create_object_node(this,s)
            class(json_node) :: this
            character(len=*) :: s
            integer :: pos
            type(token_t) :: tok

            pos = 1
            call next_token(s, pos, tok)
            if (tok%kind /= token_type_lbrace) then
                this%node_type = "ERROR"
                this%value_string = "Object must start with '{'"
                this%child_nodes_count = 0
                if (allocated(this%child_nodes)) deallocate(this%child_nodes)
                return
            end if

            call parse_object_after_open(this, s, pos)
        end subroutine create_object_node
end module fjson