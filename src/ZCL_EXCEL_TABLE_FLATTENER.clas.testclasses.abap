*"* use this source file for your ABAP unit test classes
CLASS ltcl_excel_table_flattener DEFINITION
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mo_cut           TYPE REF TO zcl_excel_table_flattener.
    DATA mo_type_analyzer TYPE REF TO zif_excel_type_analyzer.

    METHODS setup                          FOR TESTING.
    METHODS test_constructor               FOR TESTING.
    METHODS test_flatten_simple_table      FOR TESTING.
    METHODS test_flatten_hierarchical_data FOR TESTING.
    METHODS test_flatten_with_field_maps   FOR TESTING.
    METHODS test_flatten_empty_table       FOR TESTING.
    METHODS test_flatten_invalid_data      FOR TESTING.
    METHODS test_process_leaf_nodes        FOR TESTING.
    METHODS test_process_nested_structures FOR TESTING.
    METHODS test_node_indentation          FOR TESTING.

    " Helper methods
    METHODS create_test_data
      RETURNING VALUE(ro_data) TYPE REF TO data.

    METHODS create_hierarchical_data
      RETURNING VALUE(ro_data) TYPE REF TO data.

    METHODS create_export_options
      RETURNING VALUE(rs_options) TYPE zif_excel_dynamic_table=>ty_export_options.

ENDCLASS.


CLASS ltcl_excel_table_flattener IMPLEMENTATION.
  METHOD setup.
    mo_type_analyzer = NEW zcl_excel_type_analyzer( ).
    mo_cut = NEW zcl_excel_table_flattener( mo_type_analyzer ).
  ENDMETHOD.

  METHOD test_constructor.
    " Test constructor with valid type analyzer
    DATA lo_flattener TYPE REF TO zcl_excel_table_flattener.

    lo_flattener = NEW zcl_excel_table_flattener( mo_type_analyzer ).
    cl_abap_unit_assert=>assert_bound( lo_flattener ).
  ENDMETHOD.

  METHOD test_flatten_simple_table.
    " Test flattening of simple table structure
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    FIELD-SYMBOLS <lt_result> TYPE STANDARD TABLE.

    lo_data = create_test_data( ).
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
    ls_options = create_export_options( ).

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 0 ).

        ASSIGN lo_result->* TO <lt_result>.
        cl_abap_unit_assert=>assert_bound( act = REF #( <lt_result> ) ).
        cl_abap_unit_assert=>assert_not_initial( <lt_result> ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_flatten_hierarchical_data.
    " Test flattening of hierarchical data with NODES
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    FIELD-SYMBOLS <lt_result> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_line>   TYPE any.
    FIELD-SYMBOLS <lv_node>   TYPE any.

    lo_data = create_hierarchical_data( ).
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
    ls_options = create_export_options( ).

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 0 ).

        ASSIGN lo_result->* TO <lt_result>.
        cl_abap_unit_assert=>assert_bound( act = REF #( <lt_result> ) ).

        " Check that NODE column exists and has proper indentation
        ASSIGN <lt_result>[ 1 ] TO <ls_line>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NODE' OF STRUCTURE <ls_line> TO <lv_node>.
          cl_abap_unit_assert=>assert_bound( act = REF #( <lv_node> ) ).
        ENDIF.

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_flatten_with_field_maps.
    " Test flattening with custom field mappings
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.
    DATA ls_mapping    TYPE zif_excel_dynamic_table=>ty_field_mapping.

    lo_data = create_test_data( ).
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).

    " Create field mapping
    ls_mapping-abap_field       = 'FIELD1'.
    ls_mapping-excel_field_name = 'Custom Field'.
    APPEND ls_mapping TO ls_options-field_mappings.

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 0 ).

        cl_abap_unit_assert=>assert_bound( lo_result ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_flatten_empty_table.
    " Test flattening of empty table
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    " TODO: variable is assigned but never used (ABAP cleaner)
    FIELD-SYMBOLS <lt_empty>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <lt_result> TYPE STANDARD TABLE.

    " Create empty table
    CREATE DATA lo_data TYPE STANDARD TABLE OF string.
    ASSIGN lo_data->* TO <lt_empty>.

    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
    ls_options = create_export_options( ).

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 0 ).

        ASSIGN lo_result->* TO <lt_result>.
        cl_abap_unit_assert=>assert_bound( act = REF #( <lt_result> ) ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_flatten_invalid_data.
    " Test error handling for invalid data
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    " Create elementary type (not table or structure)
    lo_type_descr = cl_abap_typedescr=>describe_by_name( 'STRING' ).
    ls_options = create_export_options( ).

    TRY.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(lo_result) = mo_cut->zif_excel_table_flattener~flatten( io_data           = REF #( 'test' )
                                                                     io_type_descr     = lo_type_descr
                                                                     is_export_options = ls_options
                                                                     iv_level          = 0 ).

        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        " Check for the actual error message from flattener, not type analyzer
        DATA(lv_error_text) = lx_error->get_text( ).
        FIND 'Unsupported data type' IN lv_error_text.
        cl_abap_unit_assert=>assert_subrc(
            exp = 0
            act = sy-subrc
            msg = |Expected 'Unsupported data type' error message not found: { lv_error_text }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_process_leaf_nodes.
    " Test processing of leaf nodes (nodes without children)
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    lo_data = create_hierarchical_data( ).
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
    ls_options = create_export_options( ).

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 0 ).

        cl_abap_unit_assert=>assert_bound( lo_result ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_process_nested_structures.
    " Test processing of nested structures
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    lo_data = create_hierarchical_data( ).
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
    ls_options = create_export_options( ).

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 1 ).

        cl_abap_unit_assert=>assert_bound( lo_result ).

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_node_indentation.
    " Test NODE column indentation based on hierarchy level
    DATA lo_data       TYPE REF TO data.
    DATA lo_type_descr TYPE REF TO cl_abap_typedescr.
    DATA lo_result     TYPE REF TO data.
    DATA ls_options    TYPE zif_excel_dynamic_table=>ty_export_options.

    FIELD-SYMBOLS <lt_result> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_line>   TYPE any.
    FIELD-SYMBOLS <lv_node>   TYPE any.

    lo_data = create_hierarchical_data( ).
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( lo_data ).
    ls_options = create_export_options( ).

    TRY.
        lo_result = mo_cut->zif_excel_table_flattener~flatten( io_data           = lo_data
                                                               io_type_descr     = lo_type_descr
                                                               is_export_options = ls_options
                                                               iv_level          = 2 ).

        ASSIGN lo_result->* TO <lt_result>.
        ASSIGN <lt_result>[ 1 ] TO <ls_line>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NODE' OF STRUCTURE <ls_line> TO <lv_node>.
          " Check for proper indentation (4 spaces for level 2)
          cl_abap_unit_assert=>assert_char_cp( exp = '    *'
                                               act = CONV string( <lv_node> ) ).
        ENDIF.

      CATCH zcx_excel_dynamic_table INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( |Unexpected exception: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_test_data.
    " Create simple test data structure
    TYPES: BEGIN OF ty_test_structure,
             field1 TYPE string,
             field2 TYPE i,
             field3 TYPE p LENGTH 10 DECIMALS 2,
           END OF ty_test_structure,
           ty_test_table TYPE STANDARD TABLE OF ty_test_structure.

    DATA lt_test_data TYPE ty_test_table.
    DATA ls_test_line TYPE ty_test_structure.

    ls_test_line-field1 = 'Test Value 1'.
    ls_test_line-field2 = 100.
    ls_test_line-field3 = '123.45'.
    APPEND ls_test_line TO lt_test_data.

    ls_test_line-field1 = 'Test Value 2'.
    ls_test_line-field2 = 200.
    ls_test_line-field3 = '678.90'.
    APPEND ls_test_line TO lt_test_data.

    CREATE DATA ro_data LIKE lt_test_data.
    ASSIGN ro_data->* TO FIELD-SYMBOL(<lt_data>).
    <lt_data> = lt_test_data.
  ENDMETHOD.

  METHOD create_hierarchical_data.
    " Create hierarchical test data with NAME and NODES - compatible with ABAP 7.31
    " Use separate types to avoid self-referencing structures

    " Define child node structure (leaf nodes)
    TYPES: BEGIN OF ty_child_node,
             name  TYPE string,
             value TYPE i,
           END OF ty_child_node,
           ty_child_table TYPE STANDARD TABLE OF ty_child_node WITH DEFAULT KEY.

    " Define parent node structure with direct table field
    TYPES: BEGIN OF ty_parent_node,
             name  TYPE string,
             value TYPE i,
             nodes TYPE ty_child_table, " Direct table, not reference
           END OF ty_parent_node,
           ty_parent_table TYPE STANDARD TABLE OF ty_parent_node.

    DATA lt_root_data  TYPE ty_parent_table.
    DATA ls_root_node  TYPE ty_parent_node.
    DATA lt_child_data TYPE ty_child_table.
    DATA ls_child_node TYPE ty_child_node.

    " Create child nodes (leaf nodes)
    ls_child_node-name  = 'Child 1'.
    ls_child_node-value = 10.
    APPEND ls_child_node TO lt_child_data.

    ls_child_node-name  = 'Child 2'.
    ls_child_node-value = 20.
    APPEND ls_child_node TO lt_child_data.

    " Create root node with children
    ls_root_node-name  = 'Root Node'.
    ls_root_node-value = 0.  " Parent nodes have zero values
    ls_root_node-nodes = lt_child_data.  " Direct assignment, not reference
    APPEND ls_root_node TO lt_root_data.

    " Create another root node without children (leaf)
    CLEAR: ls_root_node,
           lt_child_data.
    ls_root_node-name  = 'Leaf Node'.
    ls_root_node-value = 30.
    " nodes table remains empty for leaf nodes
    APPEND ls_root_node TO lt_root_data.

    CREATE DATA ro_data LIKE lt_root_data.
    ASSIGN ro_data->* TO FIELD-SYMBOL(<lt_data>).
    <lt_data> = lt_root_data.
  ENDMETHOD.

  METHOD create_export_options.
    " Create default export options
    CLEAR rs_options.
    " Leave field_mappings empty for default behavior
  ENDMETHOD.
ENDCLASS.
