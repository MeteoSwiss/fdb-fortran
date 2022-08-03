SUBROUTINE copy_s2a(a,s)   ! copy s(1:Clen(s)) to char array
   CHARACTER(*),INTENT(IN) :: s
   CHARACTER :: a(LEN(s))
   INTEGER :: i
   DO i = 1,LEN(s)
      a(i) = s(i:i)
   END DO
   a(LEN(s)+1) = char(0)
END SUBROUTINE copy_s2a

program test_fdb
   use fdb
   use eccodes

   integer(c_int) :: res
   type(c_ptr) :: fdb_handle
   type(c_ptr) :: key
   integer :: i, ifile, igrib, iret
   character(len=10)                  ::  open_mode = 'r'
   character(kind=c_char), dimension(128) :: generatingProcessIdentifier, productionStatusOfProcessedData, dateTime, typeOfLevel, &
   &                    parameterNumber, level, productDefinitionTemplateNumber
   character(len=128) :: generatingProcessIdentifier_str, productionStatusOfProcessedData_str, dateTime_str, typeOfLevel_str, &
   &                    parameterNumber_str, level_str, productDefinitionTemplateNumber_str

   integer:: level_int
   integer :: numberOfValues
   real, dimension(:), target, allocatable ::  values
   type(c_ptr) :: values_ptr
   character(len=128) :: keyname_str
   character(kind=c_char), dimension(128) :: keyname

   res = fdb_initialise()

   res = fdb_new_handle(fdb_handle)

   res = fdb_new_key(key)

   call codes_open_file(ifile, '/home/vcherkas/fdb-poc/build/fdb/fdb-fortran/lfff00000000c', open_mode)

   call codes_grib_new_from_file(ifile, igrib, iret)

   LOOP: DO WHILE (iret /= CODES_END_OF_FILE)

      is_missing = 0;

      call get_value_of_key(igrib, 'generatingProcessIdentifier', generatingProcessIdentifier_str) 
      call get_value_of_key(igrib, 'productionStatusOfProcessedData', productionStatusOfProcessedData_str) 
      call get_value_of_key(igrib, 'dateTime', dateTime_str) 
      call get_value_of_key(igrib, 'typeOfLevel', typeOfLevel_str) 
      call get_value_of_key(igrib, 'parameterNumber', parameterNumber_str) 
      call get_value_of_key(igrib, 'productDefinitionTemplateNumber', productDefinitionTemplateNumber_str) 
      call get_value_of_key(igrib, 'level', level_str, level_int) 

      ! get the size of the values array
      call codes_get_size(igrib, 'values', numberOfValues)
      write (*, *) 'numberOfValues=', numberOfValues

      call copy_s2a(generatingProcessIdentifier, trim(generatingProcessIdentifier_str))
      call copy_s2a(productionStatusOfProcessedData, trim(productionStatusOfProcessedData_str))
      call copy_s2a(dateTime, trim(dateTime_str))
      call copy_s2a(typeOfLevel, trim(typeOfLevel_str))
      call copy_s2a(level, trim(level_str))
      call copy_s2a(parameterNumber, trim(parameterNumber_str))
      call copy_s2a(productDefinitionTemplateNumber, trim(productDefinitionTemplateNumber_str))

      keyname_str = "generatingProcessIdentifier"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, generatingProcessIdentifier)

      keyname_str = "productionStatusOfProcessedData"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, productionStatusOfProcessedData)

      keyname_str = "dateTime"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, dateTime)

      keyname_str = "typeOfLevel"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, typeOfLevel)

      keyname_str = "level"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, level)

      keyname_str = "parameterNumber"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, parameterNumber)

      keyname_str = "productDefinitionTemplateNumber"
      call copy_s2a(keyname, trim(keyname_str))
      res = fdb_key_add(key, keyname, productDefinitionTemplateNumber)

      allocate (values(numberOfValues), stat=iret)
      ! get data values
      call codes_get(igrib, 'values', values)
      values_ptr = transfer(c_loc(values), values_ptr)

      ret = fdb_archive(fdb_handle, key, values_ptr, numberOfValues);

      call codes_release(igrib)
      call codes_grib_new_from_file(ifile, igrib, iret)
   end do LOOP

   call codes_close_file(ifile)

   write(*,*) "end"
end program
