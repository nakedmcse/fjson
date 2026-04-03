module fjson_tokenizer
    implicit none
    private
    public :: next_token, token_type_string, token_type_number, token_type_bool, token_type_null, token_type_lbrace, &
            token_type_rbrace, token_type_lbracket, token_type_rbracket, token_type_colon, token_type_comma, &
            token_type_error, token_type_eof, token_t

    integer, parameter :: token_type_error    = -1
    integer, parameter :: token_type_eof      = 0
    integer, parameter :: token_type_string   = 1
    integer, parameter :: token_type_number   = 2
    integer, parameter :: token_type_bool     = 3
    integer, parameter :: token_type_null     = 4
    integer, parameter :: token_type_lbrace   = 5
    integer, parameter :: token_type_rbrace   = 6
    integer, parameter :: token_type_lbracket = 7
    integer, parameter :: token_type_rbracket = 8
    integer, parameter :: token_type_colon    = 9
    integer, parameter :: token_type_comma    = 10

    type :: token_t
        integer :: kind = token_type_error
        character(len=:), allocatable :: text
    end type token_t

contains

    logical function is_whitespace(ch)
        character(len=1), intent(in) :: ch
        is_whitespace = (ch == ' ' .or. ch == char(9) .or. ch == char(10) .or. ch == char(13))
    end function is_whitespace

    logical function is_digit(ch)
        character(len=1), intent(in) :: ch
        is_digit = (ch >= '0' .and. ch <= '9')
    end function is_digit

    subroutine skip_whitespace(s, pos)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: pos

        do while (pos <= len(s) .and. is_whitespace(s(pos:pos)))
            pos = pos + 1
        end do
    end subroutine skip_whitespace

    subroutine next_token(s, pos, tok)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: pos
        type(token_t), intent(out) :: tok

        integer :: start
        character(len=1) :: ch

        call skip_whitespace(s, pos)

        if (pos > len(s)) then
            tok%kind = token_type_eof
            tok%text = ""
            return
        end if

        ch = s(pos:pos)

        select case (ch)
        case ('{')
            tok%kind = token_type_lbrace
            tok%text = "{"
            pos = pos + 1
            return
        case ('}')
            tok%kind = token_type_rbrace
            tok%text = "}"
            pos = pos + 1
            return
        case ('[')
            tok%kind = token_type_lbracket
            tok%text = "["
            pos = pos + 1
            return
        case (']')
            tok%kind = token_type_rbracket
            tok%text = "]"
            pos = pos + 1
            return
        case (':')
            tok%kind = token_type_colon
            tok%text = ":"
            pos = pos + 1
            return
        case (',')
            tok%kind = token_type_comma
            tok%text = ","
            pos = pos + 1
            return
        case ('"')
            call scan_string(s, pos, tok)
            return
        case ('t')
            call scan_literal(s, pos, "true", token_type_bool, tok)
            return
        case ('f')
            call scan_literal(s, pos, "false", token_type_bool, tok)
            return
        case ('n')
            call scan_literal(s, pos, "null", token_type_null, tok)
            return
        case default
            if (ch == '-' .or. is_digit(ch)) then
                call scan_number(s, pos, tok)
            else
                tok%kind = token_type_error
                tok%text = s(pos:pos)
                pos = pos + 1
            end if
            return
        end select
    end subroutine next_token

    subroutine scan_literal(s, pos, literal, kind, tok)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: literal
        integer, intent(in) :: kind
        type(token_t), intent(out) :: tok

        integer :: n

        n = len(literal)

        if (pos + n - 1 <= len(s) .and. s(pos:pos+n-1) == literal) then
            tok%kind = kind
            tok%text = literal
            pos = pos + n
        else
            tok%kind = token_type_error
            tok%text = s(pos:min(len(s), pos+n-1))
            pos = min(len(s)+1, pos+n)
        end if
    end subroutine scan_literal

    subroutine scan_string(s, pos, tok)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: pos
        type(token_t), intent(out) :: tok

        integer :: start
        logical :: escaped

        start = pos
        pos = pos + 1
        escaped = .false.

        do while (pos <= len(s))
            if (escaped) then
                escaped = .false.
            else
                select case (s(pos:pos))
                case ('\')
                    escaped = .true.
                case ('"')
                    pos = pos + 1
                    tok%kind = token_type_string
                    tok%text = s(start:pos-1)
                    return
                end select
            end if
            pos = pos + 1
        end do

        tok%kind = token_type_error
        tok%text = s(start:len(s))
    end subroutine scan_string

    subroutine scan_number(s, pos, tok)
        character(len=*), intent(in) :: s
        integer, intent(inout) :: pos
        type(token_t), intent(out) :: tok

        integer :: start

        start = pos

        if (s(pos:pos) == '-') pos = pos + 1

        if (pos > len(s) .or. .not. is_digit(s(pos:pos))) then
            tok%kind = token_type_error
            tok%text = s(start:min(len(s), pos))
            return
        end if

        do while (pos <= len(s) .and. is_digit(s(pos:pos)))
            pos = pos + 1
        end do

        if (pos <= len(s) .and. s(pos:pos) == '.') then
            pos = pos + 1

            if (pos > len(s) .or. .not. is_digit(s(pos:pos))) then
                tok%kind = token_type_error
                tok%text = s(start:min(len(s), pos))
                return
            end if

            do while (pos <= len(s) .and. is_digit(s(pos:pos)))
                pos = pos + 1
            end do
        end if

        tok%kind = token_type_number
        tok%text = s(start:pos-1)
    end subroutine scan_number

end module fjson_tokenizer