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
   integer(c_int)                            :: numStrings

   ! character(kind=c_char), dimension(32), target, allocatable   :: values(:)
   ! type(c_ptr)    :: values_ptr

   integer ::  ns
   CHARACTER(LEN=100), DIMENSION(1), TARGET            :: values_str_array(0:2)
   TYPE(C_PTR), DIMENSION(1)                           :: values_ptr(0:2)

   character(kind=c_char), dimension(32)                        :: value
   character(len=32)                                            :: value_str


   res = fdb_initialise()

   res = fdb_new_handle(fdb_handle)

   res = fdb_new_request(req)

   value_str= "202012040900"
   numStrings=2
   DO ns = 0, numStrings-1
      values_str_array(ns) = trim(value_str)//char(0)
      values_ptr(ns) = C_LOC(values_str_array(ns))
   END DO

   res = fdb_request_add(req, "datetime", values_ptr, numStrings);


   ! value_str= "121"
   ! numStrings=1
   ! values_str_array(:)='empty'
   ! DO ns = 0, numStrings-1
   !    values_str_array(ns) = trim(value_str)//char(0)
   !    values_ptr(ns) = C_LOC(values_str_array(ns))
   ! END DO

   ! res = fdb_request_add(req, "generatingProcessIdentifier", values_ptr, numStrings);

   ! res = fdb_request_add(req, "level", "31", 1);
   ! res = fdb_request_add(req, "parameterNumber", "6", 1);
   ! res = fdb_request_add(req, "productDefinitionTemplateNumber", "1", 1);
   ! res = fdb_request_add(req, "productionStatusOfProcessedData", "2", 1);
   ! res = fdb_request_add(req, "typeOfLevel", "generalVertical", 1);
   
   write (*, *) 'req=', req

   ! res = fdb_new_datareader(dr);

   ! res = fdb_retrieve(fdb_handle, req, dr)

   ! res = fdb_datareader_open(dr, size)

   ! count=10000000

   ! res = fdb_datareader_read(dr, grib, count, read)
   ! res = fdb_datareader_tell(dr, read);

   ! write (*, *) 'grib=', grib
   ! write (*, *) 'read=', read


   write(*,*) 'end'
end program
