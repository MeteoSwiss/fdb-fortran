program test_fdb_retrieve
   use fdb
   use eccodes

   integer(c_int)                            :: res
   type(c_ptr)                               :: fdb_handle
   type(c_ptr)                               :: req
   type(c_ptr)                               :: dr
   integer(c_long)                           :: size

   integer                                   :: i, ifile, igrib, iret
   character(len=10)                         :: open_mode = 'r'

   character(kind=c_char, len=1), dimension(:), allocatable  :: buf
   character(kind=c_char, len=1), dimension(:), allocatable  :: message
   integer(c_long)                                :: read

   CHARACTER(len=32)                         :: date_values(1)
   CHARACTER(len=32)                         :: parameterNumber_values(3)
   CHARACTER(len=32)                         :: single_value(1)

   integer                                   :: msgid, status
   character(len=128)                        :: keyvalue_str

   integer                                   :: numberOfValues
   real, dimension(:), target, allocatable   :: values

   integer :: idx_a, idx_b, compute_next_index, next_grib

   res = fdb_initialise()
   res = fdb_new_handle(fdb_handle)
   res = fdb_new_request(req)


   date_values(1)="202012040900"
   ! date_values(2)="202012041200"
   call fdb_request_add_fortran(req, "dateTime", date_values)
   call fdb_request_add_fortran(req, "productionStatusOfProcessedData", ["2"])
   call fdb_request_add_fortran(req, "productDefinitionTemplateNumber", ["1"])
   parameterNumber_values(1)='18'
   parameterNumber_values(2)='20'
   parameterNumber_values(3)='30'
   call fdb_request_add_fortran(req, "parameterNumber", parameterNumber_values)
   call fdb_request_add_fortran(req, "generatingProcessIdentifier", ["121"])
   call fdb_request_add_fortran(req, "typeOfLevel", ["surface"])
   call fdb_request_add_fortran(req, "level", ["0"])

   res = fdb_new_datareader(dr);
   res = fdb_retrieve(fdb_handle, req, dr)
   res = fdb_datareader_open(dr, size)
   write (*, *) 'size of data =', size
   
   allocate(buf(size))
   res = fdb_datareader_read(dr, buf, size, read)

   idx_a= index_chararray(buf,'GRIB') 
   idx_b= index_chararray(buf(5:), 'GRIB')+4

   do while(compute_next_index .ne. 0)
      message = buf(idx_a:idx_b-1)
      WRITE (*, *) 'message=', message(:10)

         call codes_new_from_message(msgid, message, status)
         write (*, *) 'status=', status, ', msgid=', msgid
      
         call codes_get(msgid, "parameterNumber", keyvalue_str)
         write (*, *) 'parameterNumber=', keyvalue_str

         ! get the size of the values array
         call codes_get_size(msgid, 'values', numberOfValues)
         write (*, *) 'numberOfValues=', numberOfValues

         allocate (values(numberOfValues), stat=status)
         ! get data values
         call codes_get(msgid, 'values', values)
         write (*, *) 'values=', values(0:20)

      buf(idx_a:idx_b-1)=' '

      compute_next_index= index_chararray(buf(idx_b:), 'GRIB')
      idx_a=compute_next_index+idx_b-1

      next_grib=index_chararray(buf(idx_a+5:), 'GRIB')
      if (next_grib .eq. 0) THEN
         idx_b=size+1
      ELSE 
         idx_b = index_chararray(buf(idx_a+5:), 'GRIB')+4+idx_a
      END IF
   end do

   deallocate(buf)
   ! res = fdb_datareader_tell(dr, read);
   ! write (*, *) 'read=', read

   ! ! count=4
   ! ! res = fdb_datareader_read_2(dr, buf, count, read)
   ! ! write (*, *) 'buf=', buf

   ! call codes_get_message_size_int(msgid, nbytes, status)
   ! write (*, *) 'nbytes=', nbytes
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! ! ! res = fdb_flush(fdb_handle) ! WHAT DOES THIS DO?
   ! ! res = fdb_new_listiterator(it)
   ! ! res = fdb_list(fdb_handle, req, it);

   ! ! res = fdb_listiterator_next(it, exist, item);
   ! ! call c_f_pointer(item, itemf)
   ! ! write (*, *) 'itemf=', itemf, 'enditemf'
   ! ! write (*, *) 'length itemf=', LEN(itemf)

   ! ! res = fdb_listiterator_next(it, exist, item);
   ! ! call c_f_pointer(item, itemf)
   ! ! write (*, *) 'itemf=', itemf, 'enditemf'

   ! ! res = fdb_delete_listiterator(it)


   ! res = fdb_datareader_seek(dr, read);
   ! write (*, *) 'read after seek=', read


   res = fdb_delete_datareader(dr);
   write(*,*) 'end of test_fdb_retrieve'
end program
