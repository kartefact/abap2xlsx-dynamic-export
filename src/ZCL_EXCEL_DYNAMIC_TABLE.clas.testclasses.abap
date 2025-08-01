*"* use this source file for your ABAP unit test classes
CLASS ltcl_dynamic_table DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_excel_dynamic_table.

    METHODS setup.
    METHODS test_constructor_default    FOR TESTING.
    METHODS test_constructor_injection  FOR TESTING.
    METHODS test_export_simple_data     FOR TESTING.
    METHODS test_invalid_input_handling FOR TESTING.
ENDCLASS.


CLASS ltcl_performance DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION LONG.

  PRIVATE SECTION.
    METHODS test_large_dataset_performance FOR TESTING.
    METHODS test_cache_performance         FOR TESTING.
ENDCLASS.


CLASS ltcl_performance IMPLEMENTATION.
  METHOD test_large_dataset_performance.
    TYPES: BEGIN OF ty_large,
             field1 TYPE string,
             field2 TYPE i,
             field3 TYPE p LENGTH 10 DECIMALS 2,
           END OF ty_large,
           tt_large TYPE STANDARD TABLE OF ty_large.

    DATA lt_large        TYPE tt_large.
    DATA ls_row          TYPE ty_large.
    DATA lo_exporter     TYPE REF TO zcl_excel_dynamic_table.
    DATA lo_data         TYPE REF TO data.
    DATA lv_start        TYPE i.
    DATA lv_end          TYPE i.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_result       TYPE string.
    DATA lv_counter      TYPE i.
    DATA ls_options      TYPE zif_excel_dynamic_table=>ty_export_options.
    DATA lv_counter_char TYPE c LENGTH 10.

    " Generate large dataset (reduced for 7.31 compatibility)
    lv_counter = 1.
    DO 1000 TIMES.
      CLEAR ls_row.
      " Convert integer to character before concatenation
      lv_counter_char = lv_counter.
      CONCATENATE 'Record' lv_counter_char INTO ls_row-field1.
      ls_row-field2 = lv_counter.
      ls_row-field3 = lv_counter * '1.5'.
      APPEND ls_row TO lt_large.
      lv_counter = lv_counter + 1.
    ENDDO.

    GET RUN TIME FIELD lv_start.

    lo_exporter = NEW #( ).
    GET REFERENCE OF lt_large INTO lo_data.

    TRY.
        lv_result = lo_exporter->zif_excel_dynamic_table~export_to_xlsx( io_data    = lo_data
                                                                         iv_title   = 'Performance Test'
                                                                         is_options = ls_options ).

        GET RUN TIME FIELD lv_end.

        " Should complete within 10 seconds (reduced for smaller dataset)
        cl_abap_unit_assert=>assert_true(
            act = COND #( WHEN ( lv_end - lv_start ) < 10000000 THEN abap_true ELSE abap_false )
            msg = 'Large dataset export should complete within 10 seconds' ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Performance test failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_cache_performance.
    TYPES: BEGIN OF ty_cache_test,
             field1 TYPE string,
             field2 TYPE i,
           END OF ty_cache_test.

    DATA ls_data     TYPE ty_cache_test.
    DATA lo_data     TYPE REF TO data.
    DATA lo_type     TYPE REF TO cl_abap_typedescr.
    DATA lo_analyzer TYPE REF TO zcl_excel_type_analyzer.
    DATA lt_fields1  TYPE zif_excel_type_analyzer=>ty_field_catalog.
    DATA lt_fields2  TYPE zif_excel_type_analyzer=>ty_field_catalog.
    DATA lv_start1   TYPE i.
    DATA lv_end1     TYPE i.
    DATA lv_start2   TYPE i.
    DATA lv_end2     TYPE i.

    ls_data-field1 = 'Test'.
    ls_data-field2 = 100.

    GET REFERENCE OF ls_data INTO lo_data.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lo_data ).

    lo_analyzer = NEW #( ).

    TRY.
        " First call (cache miss)
        GET RUN TIME FIELD lv_start1.
        lt_fields1 = lo_analyzer->zif_excel_type_analyzer~analyze_structure( lo_type ).
        GET RUN TIME FIELD lv_end1.

        " Second call (cache hit)
        GET RUN TIME FIELD lv_start2.
        lt_fields2 = lo_analyzer->zif_excel_type_analyzer~analyze_structure( lo_type ).
        GET RUN TIME FIELD lv_end2.

        " Cache hit should be faster or equal
        cl_abap_unit_assert=>assert_true(
            act = COND #( WHEN ( lv_end2 - lv_start2 ) <= ( lv_end1 - lv_start1 ) THEN abap_true ELSE abap_false )
            msg = 'Cached call should be faster or equal to first call' ).

        " Results should be identical
        cl_abap_unit_assert=>assert_equals( exp = lt_fields2
                                            act = lt_fields1
                                            msg = 'Cache should return identical results' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Cache performance test failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_interfaces DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_dynamic_table_interface FOR TESTING.
    METHODS test_type_analyzer_interface FOR TESTING.
    METHODS test_flattener_interface     FOR TESTING.
ENDCLASS.


CLASS ltcl_interfaces IMPLEMENTATION.
  METHOD test_dynamic_table_interface.
    DATA lo_table TYPE REF TO zif_excel_dynamic_table.

    lo_table = NEW zcl_excel_dynamic_table( ).

    cl_abap_unit_assert=>assert_bound( act = lo_table
                                       msg = 'Interface should be implemented' ).
  ENDMETHOD.

  METHOD test_type_analyzer_interface.
    DATA lo_analyzer TYPE REF TO zif_excel_type_analyzer.

    lo_analyzer = NEW zcl_excel_type_analyzer( ).

    cl_abap_unit_assert=>assert_bound( act = lo_analyzer
                                       msg = 'Interface should be implemented' ).
  ENDMETHOD.

  METHOD test_flattener_interface.
    DATA lo_flattener TYPE REF TO zif_excel_table_flattener.
    DATA lo_analyzer  TYPE REF TO zcl_excel_type_analyzer.

    lo_analyzer = NEW #( ).
    lo_flattener = NEW zcl_excel_table_flattener( io_type_analyzer = lo_analyzer ).

    cl_abap_unit_assert=>assert_bound( act = lo_flattener
                                       msg = 'Interface should be implemented' ).
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_dynamic_table IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_constructor_default.
    DATA lo_table TYPE REF TO zcl_excel_dynamic_table.

    lo_table = NEW #( ).

    " Test passes if no exception is raised
    cl_abap_unit_assert=>assert_bound( act = lo_table
                                       msg = 'Constructor should create object' ).
  ENDMETHOD.

  METHOD test_constructor_injection.
    DATA lo_analyzer  TYPE REF TO zcl_excel_type_analyzer.
    DATA lo_flattener TYPE REF TO zcl_excel_table_flattener.
    DATA lo_table     TYPE REF TO zcl_excel_dynamic_table.

    lo_analyzer = NEW #( ).
    lo_flattener = NEW #( io_type_analyzer = lo_analyzer ).

    lo_table = NEW #( io_analyzer  = lo_analyzer
                      io_flattener = lo_flattener ).

    cl_abap_unit_assert=>assert_bound( act = lo_table
                                       msg = 'Constructor with injection should work' ).
  ENDMETHOD.

  METHOD test_export_simple_data.
    TYPES: BEGIN OF ty_test,
             field1 TYPE string,
             field2 TYPE i,
           END OF ty_test,
           tt_test TYPE STANDARD TABLE OF ty_test.

    DATA lt_data    TYPE tt_test.
    DATA ls_row     TYPE ty_test.
    DATA lo_data    TYPE REF TO data.
    DATA lv_result  TYPE string.
    DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

    " Build test data without VALUE constructor
    ls_row-field1 = 'Test1'.
    ls_row-field2 = 100.
    APPEND ls_row TO lt_data.

    ls_row-field1 = 'Test2'.
    ls_row-field2 = 200.
    APPEND ls_row TO lt_data.

    GET REFERENCE OF lt_data INTO lo_data.

    TRY.
        lv_result = mo_cut->zif_excel_dynamic_table~export_to_xlsx( io_data    = lo_data
                                                                    iv_title   = 'Test Export'
                                                                    is_options = ls_options ).

        cl_abap_unit_assert=>assert_not_initial( act = lv_result
                                                 msg = 'Should return base64 string' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Export failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_input_handling.
    DATA lo_data    TYPE REF TO data.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_result  TYPE string.
    DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

    " Test with unbound data reference
    TRY.
        lv_result = mo_cut->zif_excel_dynamic_table~export_to_xlsx( io_data    = lo_data
                                                                    iv_title   = 'Test Export'
                                                                    is_options = ls_options ).
        cl_abap_unit_assert=>fail( 'Should raise exception for unbound data' ).
      CATCH zcx_excel_dynamic_table.
        " Expected exception
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
