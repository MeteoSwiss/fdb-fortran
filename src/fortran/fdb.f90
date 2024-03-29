MODULE fdb
   use, intrinsic :: iso_c_binding
   use eccodes
   implicit none

   integer :: MAX_CHAR = 128

   interface
      integer(kind=c_int) function fdb_key_add(key, param, value) bind(C, name='fdb_key_add')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
         type(c_ptr), intent(in), value :: key
         character(kind=c_char, len=1), dimension(*), INTENT(in) :: param
         character(kind=c_char, len=1), dimension(*), INTENT(in) :: value
      end function fdb_key_add
   end interface

   interface
      integer(kind=c_int) function fdb_new_handle(fdb_handle) bind(C, name='fdb_new_handle')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(inout) :: fdb_handle
      end function fdb_new_handle
   end interface

   interface
      integer(kind=c_int) function fdb_new_key(key) bind(C,name='fdb_new_key')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(inout) :: key
      end function fdb_new_key
   end interface

   interface
      integer(kind=c_int) function fdb_archive(fdb_handle, key, data, length) bind(C, name='fdb_archive')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in), value :: fdb_handle
         type(c_ptr), intent(in), value :: key
         type(c_ptr), intent(in), value :: data
         integer(kind=c_int), intent(in), value :: length
      end function fdb_archive
   end interface

   interface
      integer(kind=c_int) function fdb_initialise() bind(C, name='fdb_initialise')
         use, intrinsic :: iso_c_binding, only : c_int
      end function fdb_initialise
   end interface

   interface
      integer(kind=c_int) function fdb_new_request(req) bind(C,name='fdb_new_request')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(inout) :: req
      end function fdb_new_request
   end interface

   interface
      integer(kind=c_int) function fdb_request_add(req, param, values, numValues) bind(C, name='fdb_request_add')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
         type(c_ptr), intent(in), value :: req
         character(kind=c_char, len=1), dimension(*), INTENT(in) :: param
         type(c_ptr), intent(in) :: values(*)
         integer(kind=c_int), INTENT(in), value :: numValues
      end function 
   end interface

   interface
      integer(kind=c_int) function fdb_retrieve(fdb_handle, req, dr) bind(C, name='fdb_retrieve')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in), value :: fdb_handle
         type(c_ptr), intent(in), value :: req         
         type(c_ptr), intent(in), value :: dr
      end function fdb_retrieve
   end interface

   interface
      integer(kind=c_int) function fdb_flush(fdb_handle) bind(C, name='fdb_flush')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in), value :: fdb_handle
      end function fdb_flush
   end interface

   interface
      integer(kind=c_int) function fdb_new_datareader(dr) bind(C,name='fdb_new_datareader')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(inout) :: dr
      end function fdb_new_datareader
   end interface

   interface
      integer(kind=c_int) function fdb_datareader_open(dr, size) bind(C,name='fdb_datareader_open')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_long
         type(c_ptr), intent(in), value :: dr
         integer(kind=c_long), intent(inout) :: size
      end function fdb_datareader_open
   end interface

   interface
      integer(kind=c_int) function fdb_datareader_close(dr) bind(C,name='fdb_datareader_close')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in), value      :: dr
      end function fdb_datareader_close
   end interface

   interface
      integer(kind=c_int) function fdb_datareader_tell(dr, pos) bind(C,name='fdb_datareader_tell')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_long
         type(c_ptr), intent(in), value      :: dr
         integer(kind=c_long), intent(inout) :: pos
      end function fdb_datareader_tell
   end interface
 
   interface
      integer(kind=c_int) function fdb_datareader_seek(dr, pos) bind(C,name='fdb_datareader_seek')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_long
         type(c_ptr), intent(in), value :: dr
         integer(kind=c_long), intent(in), value  :: pos
      end function fdb_datareader_seek
   end interface
   
   interface
      integer(kind=c_int) function fdb_datareader_skip(dr, count) bind(C,name='fdb_datareader_skip')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_long
         type(c_ptr), intent(in), value :: dr
         integer(kind=c_long), intent(in), value  :: count
      end function fdb_datareader_skip
   end interface
   
   interface
      integer(kind=c_int) function fdb_datareader_read(dr, buf, count, read) bind(C,name='fdb_datareader_read')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, c_long
         type(c_ptr), intent(in), value :: dr
         character(kind=c_char, len=1), dimension(*), intent(inout) :: buf
         integer(kind=c_long), intent(in), value  :: count
         integer(kind=c_long), intent(inout)      :: read
      end function fdb_datareader_read
   end interface

   interface
      integer(kind=c_int) function fdb_delete_datareader(dr) bind(C,name='fdb_delete_datareader')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in), value      :: dr
      end function fdb_delete_datareader
   end interface


   interface
      integer(kind=c_int) function fdb_list(fdb_handle, req, it, duplicates) bind(C,name='fdb_list')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_bool, c_int
         type(c_ptr), value   :: fdb_handle
         type(c_ptr), value   :: req 
         type(c_ptr)       :: it
         logical(kind=c_bool) :: duplicates
      end function fdb_list
   end interface

   interface
      integer(kind=c_int) function fdb_new_listiterator(it) bind(C,name='fdb_new_listiterator')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in)      :: it
      end function fdb_new_listiterator
   end interface

   interface
      integer(kind=c_int) function fdb_listiterator_next(it) bind(C,name='fdb_listiterator_next')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char, c_bool
         type(c_ptr), intent(in), value               :: it
      end function fdb_listiterator_next
   end interface

   interface
      integer(kind=c_int) function fdb_delete_listiterator(it) bind(C,name='fdb_delete_listiterator')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr
         type(c_ptr), intent(in), value      :: it
      end function fdb_delete_listiterator
   end interface

   interface
      integer(kind=c_int) function fdb_listiterator_attrs(it, uri, off, len) bind(C,name='fdb_listiterator_attrs')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_int, c_char
         type(c_ptr), intent(in), value      :: it
         character(kind=c_char, len=1), dimension(*), INTENT(inout) :: uri
         integer(kind=c_int), intent(inout) :: off
         integer(kind=c_int), intent(inout) :: len
      end function fdb_listiterator_attrs
   end interface

   interface
      integer(kind=c_int) function fdb_inspect(fdb_handle, req, it) bind(C,name='fdb_inspect')
         use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_bool, c_int
         type(c_ptr), value   :: fdb_handle
         type(c_ptr), value   :: req 
         type(c_ptr)       :: it
      end function fdb_inspect
   end interface

   CONTAINS
   SUBROUTINE get_value_of_key(igrib, keyname, keyvalue_str, type)
      character(len=*), INTENT(IN)         :: keyname
      character(len=MAX_CHAR), INTENT(OUT) :: keyvalue_str
      character(len=*), INTENT(IN), OPTIONAL  :: type
      integer, INTENT(IN)                  :: igrib
      integer                              :: keyvalue_int
      integer                              :: res
      integer                              :: is_missing
      call codes_is_missing(igrib, trim(keyname), is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get values
         if (type == 'integer') then
            call codes_get_int(igrib, keyname, keyvalue_int)
            write (keyvalue_str, "(I4)") keyvalue_int
            keyvalue_str=trim(adjustl(keyvalue_str))
         else
            call codes_get(igrib, keyname, keyvalue_str)
         end if
         write (*, *) keyname, '=', keyvalue_str
      else
         write (*, *) keyname, ' is missing from data.'
      end if
   END SUBROUTINE

   SUBROUTINE copy_s2a(a,s,status)   ! copy s(1:Clen(s)) to char array
      CHARACTER(*),INTENT(IN) :: s
      CHARACTER, INTENT(OUT) :: a(:)
      INTEGER, INTENT(OUT) :: status
      INTEGER :: i
      status=0
      IF (LEN(s) > SIZE(a)) THEN
         status = 1
      END IF
      DO i = 1,LEN(s)
         a(i) = s(i:i)
      END DO
      a(LEN(s)+1) = char(0)
   END SUBROUTINE copy_s2a

   SUBROUTINE copy_s2a_exit(a,s,caller)
      CHARACTER(*),INTENT(IN) :: s, caller
      CHARACTER, INTENT(IN) :: a(:)
      WRITE(*,*) caller,': String ', s, ' of length', LEN(s), &
      & ' is greater than the size of character &
      & array it is being assigned to:', SIZE(a)
      call EXIT(1)
   END SUBROUTINE copy_s2a_exit

   SUBROUTINE add_key_to_fdb_from_file(key, igrib, keyname_str, type)  
      use, intrinsic :: iso_c_binding, only : c_int, c_ptr, c_char
      type(c_ptr), INTENT(INOUT)                  :: key
      character(len=*), INTENT(IN)                :: keyname_str
      character(len=*), INTENT(IN), OPTIONAL      :: type
      ! Local vars
      character(kind=c_char), dimension(MAX_CHAR) :: keyname
      character(kind=c_char), dimension(MAX_CHAR) :: keyvalue
      character(len=MAX_CHAR)                     :: keyvalue_str
      integer                                     :: keyvalue_int
      integer, INTENT(IN)                         :: igrib
      integer                                     :: res, status
      call get_value_of_key(igrib, trim(keyname_str), keyvalue_str, type) 
      call copy_s2a(keyvalue, trim(keyvalue_str),status)
      if (status /= 0) then
         call copy_s2a_exit(keyvalue, trim(keyvalue_str),'add_key_to_fdb_from_file')
      endif
      call copy_s2a(keyname, trim(keyname_str),status)
      if (status /= 0) then
         call copy_s2a_exit(keyname, trim(keyname_str),'add_key_to_fdb_from_file')
      endif
      res = fdb_key_add(key, keyname, keyvalue)
   END SUBROUTINE add_key_to_fdb_from_file

   ! convertValues creates a list of pointers to characer array values used in requests to fdb. 
   ! First converts array of strings to array of character arrays, then creates pointers.
   ! This is required since the interface fdb_request_add expects this data format. 
   SUBROUTINE convertValues(numStrings, values_str_array, values_array, values_ptr)
      use, intrinsic :: iso_c_binding 
      integer(kind=c_int) , INTENT(IN)                :: numStrings
      CHARACTER(len=*), INTENT(IN)                    :: values_str_array(numStrings)
      CHARACTER(kind=c_char), TARGET , INTENT(INOUT)  :: values_array(MAX_CHAR,numStrings)
      TYPE(C_PTR), INTENT(OUT)                        :: values_ptr(numStrings)
      integer                                         :: ns, i, status
      character(len=MAX_CHAR)                         :: value_str
      character(kind=c_char), dimension(MAX_CHAR)     :: value
      character(len=5)                                :: ns_str
      DO ns = 1, numStrings
         call copy_s2a(values_array(:,ns), trim(adjustl(values_str_array(ns))),status)
         if (status /= 0) then
            call copy_s2a_exit(values_array(:,ns), trim(adjustl(values_str_array(ns))),'convertValues')
         endif
         values_ptr(ns) = C_LOC(values_array(:,ns))
      END DO
   END SUBROUTINE


   SUBROUTINE fdb_request_add_values(req, keyname_str, values_str_array)
      use, intrinsic :: iso_c_binding 
      type(c_ptr), INTENT(IN)                        :: req
      CHARACTER(len=*), INTENT(IN)                   :: keyname_str
      CHARACTER(len=*), INTENT(IN)                   :: values_str_array(:)
      CHARACTER(kind=c_char), TARGET, ALLOCATABLE    :: values_array(:,:)
      TYPE(C_PTR)                                    :: values_ptr(SIZE(values_str_array))
      integer(kind=c_int)                            :: numStrings
      character(kind=c_char), dimension(MAX_CHAR)    :: keyname
      integer(kind=c_int)                            :: res, status

      numStrings=SIZE(values_str_array)
      ALLOCATE(values_array(MAX_CHAR,numStrings))
      call convertValues(numStrings, values_str_array, values_array, values_ptr)
      call copy_s2a(keyname, trim(keyname_str),status)
      if (status /= 0) then
         call copy_s2a_exit(keyname, trim(keyname_str),'fdb_request_add_values')
      endif
      res = fdb_request_add(req, keyname, values_ptr, numStrings);
      DEALLOCATE(values_array)
   END SUBROUTINE

   function str_replace(str,pos,new) result(strout)
      implicit none
      character(len=*), intent(in) :: str
      character(len=:), allocatable :: strout
      character(len=1), intent(in) :: new
      integer :: i, pos
      strout=""
      do i=1,LEN(str)
         if(i.eq.pos)then
            strout=strout//new
         else
            strout=strout//str(i:i)
         endif
      end do
   end function str_replace

end module fdb
