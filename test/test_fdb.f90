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
      call codes_is_missing(igrib, 'generatingProcessIdentifier', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'generatingProcessIdentifier', generatingProcessIdentifier_str)
         write (*, *) 'generatingProcessIdentifier=', generatingProcessIdentifier_str
      else
         write (*, *) 'generatingProcessIdentifier is missing'
      end if

      call codes_is_missing(igrib, 'productionStatusOfProcessedData', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'productionStatusOfProcessedData', productionStatusOfProcessedData_str)
         write (*, *) 'productionStatusOfProcessedData=', productionStatusOfProcessedData_str
      else
         write (*, *) 'productionStatusOfProcessedData is missing'
      end if

      call codes_is_missing(igrib, 'dateTime', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'dateTime', dateTime_str)
         write (*, *) 'dateTime=', dateTime_str
      else
         write (*, *) 'dateTime is missing'
      end if

      call codes_is_missing(igrib, 'typeOfLevel', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'typeOfLevel', typeOfLevel_str)
         write (*, *) 'typeOfLevel=', typeOfLevel_str
      else
         write (*, *) 'typeOfLevel is missing'
      end if

      call codes_is_missing(igrib, 'level', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'level', level_int)
         ! convert int to string
         write (level_str, "(I4)") level_int
         level_str=trim(adjustl(level_str))
         write (*, *) 'level=', level_str
      else
         write (*, *) 'level is missing'
      end if

      call codes_is_missing(igrib, 'parameterNumber', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'parameterNumber', parameterNumber_str)
         write (*, *) 'parameterNumber=', parameterNumber_str
      else
         write (*, *) 'parameterNumber is missing'
      end if

      call codes_is_missing(igrib, 'productDefinitionTemplateNumber', is_missing);
      if (is_missing /= 1) then
         ! key value is not missing so get as an integer
         call codes_get(igrib, 'productDefinitionTemplateNumber', productDefinitionTemplateNumber_str)
         write (*, *) 'productDefinitionTemplateNumber=', productDefinitionTemplateNumber_str
      else
         write (*, *) 'productDefinitionTemplateNumber is missing'
      end if

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
