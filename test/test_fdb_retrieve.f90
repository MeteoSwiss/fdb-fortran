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

   character(kind=c_char, len=1), dimension(100)  :: string_buf
   type(c_ptr)                               :: buf
   integer(c_long)                               :: count
   integer(c_long)                               :: read

   CHARACTER(len=32)                         :: date_values(1)
   CHARACTER(len=32)                         :: single_value(1)


   res = fdb_initialise()
   res = fdb_new_handle(fdb_handle)
   res = fdb_new_request(req)


   date_values(1)="202012040900"
   ! date_values(2)="202012041200"
   call fdb_request_add_fortran(req, "dateTime", date_values)
   call fdb_request_add_fortran(req, "productionStatusOfProcessedData", ["2"])
   call fdb_request_add_fortran(req, "productDefinitionTemplateNumber", ["1"])
   call fdb_request_add_fortran(req, "parameterNumber", ["20"])
   call fdb_request_add_fortran(req, "generatingProcessIdentifier", ["121"])
   call fdb_request_add_fortran(req, "typeOfLevel", ["surface"])
   call fdb_request_add_fortran(req, "level", ["0"])

   res = fdb_new_datareader(dr);
   res = fdb_retrieve(fdb_handle, req, dr)
   res = fdb_datareader_open(dr, size)
   write (*, *) 'size of data =', size

   count=4
   res = fdb_datareader_read(dr, string_buf, count, read)

   if (ALL(string_buf(:4) .eq. ['G','R','I','B'])) then 
      write (*, *) 'PASS, string_buf=', string_buf(1:count)
   ELSE 
      WRITE (*, *) 'FAIL, string_buf=', string_buf(1:count)
   END IF

   res = fdb_datareader_tell(dr, read);
   write (*, *) 'read=', read

   res = fdb_delete_datareader(dr);

   ! count=4
   ! res = fdb_datareader_read_2(dr, buf, count, read)
   ! write (*, *) 'buf=', buf

   write(*,*) 'end of test_fdb_retrieve'
end program
