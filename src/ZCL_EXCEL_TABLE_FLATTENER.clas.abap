"! Table flattener for Dynamic Table Export in ABAP2XLSX, implementing hierarchical data flattening.
"! Flattens nested internal tables into a flat structure with an indented NODE column for Excel output.
"! Integrates with zif_excel_table_flattener and zif_excel_type_analyzer for modular processing.
CLASS zcl_excel_table_flattener DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_excel_table_flattener.

    "! Constructor to initialize the flattener with a type analyzer.
    "! @parameter io_type_analyzer | Reference to the type analyzer interface for structure analysis.
    METHODS constructor
      IMPORTING io_type_analyzer TYPE REF TO zif_excel_type_analyzer.

  PRIVATE SECTION.
    DATA mo_type_analyzer TYPE REF TO zif_excel_type_analyzer.
    DATA mt_field_catalog TYPE zif_excel_type_analyzer=>ty_field_catalog.

    METHODS create_flat_structure
      IMPORTING it_field_catalog  TYPE zif_excel_type_analyzer=>ty_field_catalog
                is_export_options TYPE zif_excel_dynamic_table=>ty_export_options
      RETURNING VALUE(ro_table)   TYPE REF TO data
      RAISING   zcx_excel_dynamic_table.

    METHODS process_hierarchical_data
      IMPORTING io_source         TYPE REF TO data
                io_source_type    TYPE REF TO cl_abap_typedescr
                iv_level          TYPE i
                iv_path           TYPE string
                is_export_options TYPE zif_excel_dynamic_table=>ty_export_options
      CHANGING  co_target_table   TYPE REF TO data
      RAISING   zcx_excel_dynamic_table.
ENDCLASS.


CLASS zcl_excel_table_flattener IMPLEMENTATION.
  METHOD constructor.
    mo_type_analyzer = io_type_analyzer.
  ENDMETHOD.

  METHOD zif_excel_table_flattener~flatten.
    TRY.
        mt_field_catalog = mo_type_analyzer->analyze_structure( io_type_descr ).
        ro_flat_table = create_flat_structure( it_field_catalog  = mt_field_catalog
                                               is_export_options = is_export_options ).
        process_hierarchical_data( EXPORTING io_source         = io_data
                                             io_source_type    = io_type_descr
                                             iv_level          = iv_level
                                             iv_path           = ''
                                             is_export_options = is_export_options
                                   CHANGING  co_target_table   = ro_flat_table ).

      CATCH zcx_excel_dynamic_table
            cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                    iv_message    = |Flattening failed: { lx_error->get_text( ) }|
                    ix_previous   = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD create_flat_structure.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.
    DATA ls_component  TYPE cl_abap_structdescr=>component.

    ls_component-name = 'NODE'.
    ls_component-type = cl_abap_elemdescr=>get_string( ).
    APPEND ls_component TO lt_components.

    IF is_export_options-field_mappings IS NOT INITIAL.
      LOOP AT is_export_options-field_mappings INTO DATA(ls_mapping).
        READ TABLE it_field_catalog INTO DATA(ls_field) WITH KEY name = ls_mapping-abap_field.
        IF sy-subrc = 0.
          ls_component-name = ls_mapping-abap_field.
          ls_component-type = ls_field-type.
          APPEND ls_component TO lt_components.
        ELSE.
          RAISE EXCEPTION TYPE zcx_excel_dynamic_table
            EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                      iv_message    = |Field { ls_mapping-abap_field } not found in input structure|.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT it_field_catalog INTO ls_field.
        ls_component-name = ls_field-name.
        ls_component-type = ls_field-type.
        APPEND ls_component TO lt_components.
      ENDLOOP.
    ENDIF.

    DATA(lo_struct) = cl_abap_structdescr=>create( lt_components ).
    DATA(lo_table) = cl_abap_tabledescr=>create( p_line_type  = lo_struct
                                                 p_table_kind = cl_abap_tabledescr=>tablekind_std ).

    CREATE DATA ro_table TYPE HANDLE lo_table.
  ENDMETHOD.

  METHOD process_hierarchical_data.
    FIELD-SYMBOLS <lt_source>       TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_source>       TYPE any.
    FIELD-SYMBOLS <lt_target>       TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_target>       TYPE any.
    FIELD-SYMBOLS <lv_name>         TYPE any.
    FIELD-SYMBOLS <lv_nodes>        TYPE ANY TABLE.
    FIELD-SYMBOLS <lv_source_field> TYPE any.
    FIELD-SYMBOLS <lv_target_field> TYPE any.

    DATA lr_target_line TYPE REF TO data.
    DATA lv_node_path   TYPE string.

    CASE io_source_type->kind.
      WHEN cl_abap_typedescr=>kind_table.
        ASSIGN io_source->* TO <lt_source>.
        ASSIGN co_target_table->* TO <lt_target>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_excel_dynamic_table
            EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                      iv_message    = |Failed to assign target table at path: { iv_path }|.
        ENDIF.

        DATA(lo_line_type) = CAST cl_abap_tabledescr( io_source_type )->get_table_line_type( ).
        LOOP AT <lt_source> ASSIGNING <ls_source>.
          process_hierarchical_data( EXPORTING io_source         = REF #( <ls_source> )
                                               io_source_type    = lo_line_type
                                               iv_level          = iv_level
                                               iv_path           = iv_path
                                               is_export_options = is_export_options
                                     CHANGING  co_target_table   = co_target_table ).
        ENDLOOP.

      WHEN cl_abap_typedescr=>kind_struct.
        ASSIGN io_source->* TO <ls_source>.
        ASSIGN co_target_table->* TO <lt_target>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_excel_dynamic_table
            EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                      iv_message    = |Failed to assign target table at path: { iv_path }|.
        ENDIF.

        CREATE DATA lr_target_line LIKE LINE OF <lt_target>.
        ASSIGN lr_target_line->* TO <ls_target>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_excel_dynamic_table
            EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                      iv_message    = |Failed to create target line at path: { iv_path }|.
        ENDIF.

        ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_source> TO <lv_name>.
        ASSIGN COMPONENT 'NODE' OF STRUCTURE <ls_target> TO <lv_target_field>.
        IF <lv_name> IS ASSIGNED AND <lv_name> IS NOT INITIAL.
          <lv_target_field> = |{ repeat( val = `  `
                                         occ = iv_level ) }{ <lv_name> }|.
          lv_node_path = |{ iv_path }->{ <lv_name> }|.
        ELSE.
          <lv_target_field> = |{ repeat( val = `  `
                                         occ = iv_level ) }Unnamed|.
          lv_node_path = |{ iv_path }->Unnamed|.
        ENDIF.

        DATA(lv_is_leaf) = abap_false.
        ASSIGN COMPONENT 'NODES' OF STRUCTURE <ls_source> TO <lv_nodes>.
        IF sy-subrc = 0.
          IF <lv_nodes> IS INITIAL.
            lv_is_leaf = abap_true.
          ELSE.
            LOOP AT mt_field_catalog INTO DATA(ls_field).
              ASSIGN COMPONENT ls_field-name OF STRUCTURE <ls_source> TO <lv_source_field>.
              IF sy-subrc = 0 AND <lv_source_field> IS NOT INITIAL.
                TRY.
                    DATA(lv_num) = CONV decfloat34( <lv_source_field> ).
                    IF lv_num <> 0.
                      lv_is_leaf = abap_true.
                      EXIT.
                    ENDIF.
                  CATCH cx_sy_conversion_error.
                    CONTINUE.
                ENDTRY.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ELSE.
          lv_is_leaf = abap_true.
        ENDIF.

        DATA lt_mappings TYPE zif_excel_dynamic_table=>ty_field_mappings.
        lt_mappings = is_export_options-field_mappings.
        IF lt_mappings IS INITIAL.
          lt_mappings = VALUE #( FOR field IN mt_field_catalog
                                 ( abap_field = field-name ) ).
        ENDIF.

        LOOP AT lt_mappings INTO DATA(ls_mapping).
          READ TABLE mt_field_catalog INTO ls_field WITH KEY name = ls_mapping-abap_field.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT ls_mapping-abap_field OF STRUCTURE <ls_source> TO <lv_source_field>.
          ASSIGN COMPONENT ls_mapping-abap_field OF STRUCTURE <ls_target> TO <lv_target_field>.
          IF NOT ( sy-subrc = 0 AND <lv_source_field> IS ASSIGNED AND <lv_target_field> IS ASSIGNED ).
            CONTINUE.
          ENDIF.

          IF lv_is_leaf = abap_true.
            TRY.
                <lv_target_field> = <lv_source_field>.
              CATCH cx_sy_conversion_error INTO DATA(lx_error).
                RAISE EXCEPTION TYPE zcx_excel_dynamic_table
                  EXPORTING
                    iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                    iv_message    = |Conversion error for field { ls_field-name } at path { lv_node_path }: { lx_error->get_text( ) }|.
            ENDTRY.
          ELSE.
            IF ls_field-is_numeric = abap_true.
              <lv_target_field> = 0.
            ELSE.
              CLEAR <lv_target_field>.
            ENDIF.
          ENDIF.
        ENDLOOP.

        APPEND <ls_target> TO <lt_target>.

        IF <lv_nodes> IS ASSIGNED AND <lv_nodes> IS NOT INITIAL.
          DATA(lo_nodes_type) = cl_abap_typedescr=>describe_by_data( <lv_nodes> ).
          process_hierarchical_data( EXPORTING io_source         = REF #( <lv_nodes> )
                                               io_source_type    = lo_nodes_type
                                               iv_level          = iv_level + 1
                                               iv_path           = lv_node_path
                                               is_export_options = is_export_options
                                     CHANGING  co_target_table   = co_target_table ).
        ENDIF.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-flattening_failed
                    iv_message    = |Unsupported data type at path: { iv_path }|.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
