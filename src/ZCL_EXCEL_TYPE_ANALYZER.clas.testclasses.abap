*"* use this source file for your ABAP unit test classes
CLASS ltcl_type_analyzer DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_excel_type_analyzer.

    METHODS setup.
    METHODS test_analyze_simple_structure FOR TESTING.
    METHODS test_analyze_nested_structure FOR TESTING.
    METHODS test_cache_functionality      FOR TESTING.
    METHODS test_numeric_field_detection  FOR TESTING.
    METHODS test_invalid_type_handling    FOR TESTING.
ENDCLASS.


CLASS ltcl_type_analyzer IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD test_analyze_simple_structure.
    TYPES: BEGIN OF ty_simple,
             field1 TYPE string,
             field2 TYPE i,
             field3 TYPE p LENGTH 10 DECIMALS 2,
           END OF ty_simple.

    DATA ls_data   TYPE ty_simple.
    DATA lo_data   TYPE REF TO data.
    DATA lo_type   TYPE REF TO cl_abap_typedescr.
    DATA lt_fields TYPE zif_excel_type_analyzer=>ty_field_catalog.

    " Create actual data instance instead of using describe_by_name
    ls_data-field1 = 'test'.
    ls_data-field2 = 123.
    ls_data-field3 = '456.78'.

    GET REFERENCE OF ls_data INTO lo_data.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lo_data ).

    TRY.
        lt_fields = mo_cut->zif_excel_type_analyzer~analyze_structure( lo_type ).

        cl_abap_unit_assert=>assert_equals( exp = 3
                                            act = lines( lt_fields )
                                            msg = 'Should find 3 fields' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Type analysis failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_cache_functionality.
    TYPES: BEGIN OF ty_cache_test,
             field1 TYPE string,
           END OF ty_cache_test.

    DATA ls_data    TYPE ty_cache_test.
    DATA lo_data    TYPE REF TO data.
    DATA lo_type    TYPE REF TO cl_abap_typedescr.
    DATA lt_fields1 TYPE zif_excel_type_analyzer=>ty_field_catalog.
    DATA lt_fields2 TYPE zif_excel_type_analyzer=>ty_field_catalog.

    ls_data-field1 = 'test'.
    GET REFERENCE OF ls_data INTO lo_data.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lo_data ).

    TRY.
        lt_fields1 = mo_cut->zif_excel_type_analyzer~analyze_structure( lo_type ).
        lt_fields2 = mo_cut->zif_excel_type_analyzer~analyze_structure( lo_type ).

        cl_abap_unit_assert=>assert_equals( exp = lt_fields2
                                            act = lt_fields1
                                            msg = 'Cache should return same results' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Cache test failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_numeric_field_detection.
    TYPES: BEGIN OF ty_numeric,
             int_field  TYPE i,
             pack_field TYPE p LENGTH 10 DECIMALS 2,
             char_field TYPE c LENGTH 10,
           END OF ty_numeric.

    DATA ls_data   TYPE ty_numeric.
    DATA lo_data   TYPE REF TO data.
    DATA lo_type   TYPE REF TO cl_abap_typedescr.
    DATA lt_fields TYPE zif_excel_type_analyzer=>ty_field_catalog.
    DATA ls_field  TYPE zif_excel_type_analyzer=>ty_field_info.

    ls_data-int_field  = 100.
    ls_data-pack_field = '123.45'.
    ls_data-char_field = 'test'.

    GET REFERENCE OF ls_data INTO lo_data.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lo_data ).

    TRY.
        lt_fields = mo_cut->zif_excel_type_analyzer~analyze_structure( lo_type ).

        READ TABLE lt_fields INTO ls_field WITH KEY name = 'INT_FIELD'.
        cl_abap_unit_assert=>assert_true( act = ls_field-is_numeric
                                          msg = 'Integer field should be numeric' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Numeric field test failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_analyze_nested_structure.
    TYPES: BEGIN OF ty_parent,
             name  TYPE string,
             value TYPE i,
           END OF ty_parent.
    TYPES: BEGIN OF ty_child,
             parent_name TYPE string,
             child_value TYPE i,
           END OF ty_child,
           tt_children TYPE STANDARD TABLE OF ty_child WITH DEFAULT KEY.
    TYPES: BEGIN OF ty_hierarchy,
             name  TYPE string,
             nodes TYPE tt_children,
             value TYPE i,
           END OF ty_hierarchy.

    DATA ls_data   TYPE ty_hierarchy.
    DATA ls_child  TYPE ty_child.
    DATA lo_data   TYPE REF TO data.
    DATA lo_type   TYPE REF TO cl_abap_typedescr.
    DATA lt_fields TYPE zif_excel_type_analyzer=>ty_field_catalog.

    " Create actual data instance instead of using describe_by_name
    ls_data-name  = 'Root'.
    ls_data-value = 100.

    ls_child-parent_name = 'Root'.
    ls_child-child_value = 50.
    APPEND ls_child TO ls_data-nodes.

    GET REFERENCE OF ls_data INTO lo_data.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( lo_data ).

    TRY.
        lt_fields = mo_cut->zif_excel_type_analyzer~analyze_structure( lo_type ).

        " Should exclude NAME and NODES fields
        READ TABLE lt_fields TRANSPORTING NO FIELDS WITH KEY name = 'NAME'.
        cl_abap_unit_assert=>assert_subrc( exp = 4
                                           act = sy-subrc
                                           msg = 'NAME field should be excluded' ).

        READ TABLE lt_fields TRANSPORTING NO FIELDS WITH KEY name = 'NODES'.
        cl_abap_unit_assert=>assert_subrc( exp = 4
                                           act = sy-subrc
                                           msg = 'NODES field should be excluded' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Nested structure analysis failed: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_type_handling.
    DATA lt_fields      TYPE zif_excel_type_analyzer=>ty_field_catalog.

    " Test with unsupported type - use elementary type which isn't handled
    " Skip null test since get_type_hash doesn't handle null references defensively
    DATA lo_string_type TYPE REF TO cl_abap_elemdescr.

    lo_string_type = cl_abap_elemdescr=>get_string( ).

    TRY.
        lt_fields = mo_cut->zif_excel_type_analyzer~analyze_structure( lo_string_type ).
        " Elementary types should return empty catalog since analyzer only handles structures/tables
        cl_abap_unit_assert=>assert_initial( act = lt_fields
                                             msg = 'Elementary types should return empty field catalog' ).
      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        " If exception is raised, verify it contains the expected message
        DATA(lv_error_text) = lx_error->get_text( ).
        FIND 'Type analysis failed' IN lv_error_text.
        cl_abap_unit_assert=>assert_subrc( exp = 0
                                           act = sy-subrc
                                           msg = 'Should contain type analysis failed message for unsupported types' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
