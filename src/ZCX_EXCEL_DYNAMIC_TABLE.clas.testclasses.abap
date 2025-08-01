*"* use this source file for your ABAP unit test classes
CLASS ltcl_exception DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_exception_creation FOR TESTING.
    METHODS test_error_codes        FOR TESTING.
    METHODS test_exception_chaining FOR TESTING.
ENDCLASS.


CLASS ltcl_exception IMPLEMENTATION.
  METHOD test_exception_creation.
    DATA lx_exception TYPE REF TO zcx_excel_dynamic_table.

    TRY.
        lx_exception = NEW #( iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                              iv_message    = 'Test message' ).

        cl_abap_unit_assert=>assert_bound( act = lx_exception
                                           msg = 'Exception should be created' ).
      CATCH cx_root.
        cl_abap_unit_assert=>fail( 'Exception creation should not fail' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_error_codes.
    cl_abap_unit_assert=>assert_equals( exp = 'INVALID_INPUT'
                                        act = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                                        msg = 'Error code should match' ).

    cl_abap_unit_assert=>assert_equals( exp = 'TYPE_ANALYSIS_FAILED'
                                        act = zcx_excel_dynamic_table=>gc_error_codes-type_analysis_failed
                                        msg = 'Error code should match' ).
  ENDMETHOD.

  METHOD test_exception_chaining.
    DATA lx_root    TYPE REF TO cx_root.
    DATA lx_dynamic TYPE REF TO zcx_excel_dynamic_table.

    TRY.
        lx_root = NEW cx_sy_zerodivide( ).

        " Chain it with our custom exception
        lx_dynamic = NEW #( iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                            iv_message    = 'Chained exception test'
                            ix_previous   = lx_root ).

        cl_abap_unit_assert=>assert_bound( act = lx_dynamic->previous
                                           msg = 'Previous exception should be set' ).
      CATCH cx_root.
        cl_abap_unit_assert=>fail( 'Exception chaining should work' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
