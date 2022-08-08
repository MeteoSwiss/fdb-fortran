program test_fdb_retrieve
   use fdb
   use eccodes

   integer(c_int)                            :: res
   type(c_ptr)                               :: fdb_handle
   type(c_ptr)                               :: req
   type(c_ptr)                               :: dr
   type(c_ptr)                               :: size
   integer                                   :: i, ifile, igrib, iret
   character(len=10)                         :: open_mode = 'r'

   ! character(len=4)                :: grib
   ! character(len=1000)              :: buf
   type(c_ptr)                               :: grib
   type(c_ptr)                               :: buf
   integer(c_long)                           :: count
   type(c_ptr)                               :: read

   CHARACTER(len=32)                         :: date_values(2)
   CHARACTER(len=32)                         :: single_value(1)


   res = fdb_initialise()
   res = fdb_new_handle(fdb_handle)
   res = fdb_new_request(req)


   date_values(1)="202012040900"
   date_values(2)="202012041200"
   call fdb_request_add_fortran(req, "datetime", date_values)
   call fdb_request_add_fortran(req, "generatingProcessIdentifier", ["121"])
   call fdb_request_add_fortran(req, "level", ["17"])
   call fdb_request_add_fortran(req, "parameterNumber", ["6"])
   call fdb_request_add_fortran(req, "productDefinitionTemplateNumber", ["1"])
   call fdb_request_add_fortran(req, "productionStatusOfProcessedData", ["2"])
   call fdb_request_add_fortran(req, "typeOfLevel", ["generalVertical"])

   res = fdb_new_datareader(dr);
   res = fdb_retrieve(fdb_handle, req, dr)
   res = fdb_datareader_open(dr, size)

   count=10000000

   res = fdb_datareader_read(dr, grib, count, read)

   res = fdb_datareader_tell(dr, read);

   ! write (*, *) 'grib=', grib
   ! write (*, *) 'read=', read


   write(*,*) 'end of test_fdb_retrieve'
end program
