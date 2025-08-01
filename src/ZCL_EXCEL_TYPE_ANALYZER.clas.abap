"! Type analyzer for dynamic Excel table export, implementing RTTS-based structure analysis.
"! This class analyzes the structure of ABAP data types (e.g., tables, structures) to identify
"! fields for export, excluding 'NAME' to avoid duplicate columns. It caches results to optimize
"! performance for repeated analyses. Implements zif_excel_type_analyzer for modular integration
"! with the ABAP2XLSX dynamic table export framework.
CLASS zcl_excel_type_analyzer DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_excel_type_analyzer.

    "! Analyzes the structure of a given ABAP type and returns a catalog of fields.
    METHODS constructor.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_cache_entry,
        type_hash TYPE string,
        fields    TYPE zif_excel_type_analyzer=>ty_field_catalog,
      END OF ty_cache_entry,
      ty_cache TYPE HASHED TABLE OF ty_cache_entry WITH UNIQUE KEY type_hash.

    DATA mt_cache TYPE ty_cache.

    METHODS get_type_hash
      IMPORTING io_type_descr  TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(rv_hash) TYPE string.

    METHODS analyze_structure_recursive
      IMPORTING io_type_descr TYPE REF TO cl_abap_typedescr
                iv_prefix     TYPE string DEFAULT ''
      CHANGING  ct_fields     TYPE zif_excel_type_analyzer=>ty_field_catalog
      RAISING   zcx_excel_dynamic_table.
ENDCLASS.

CLASS zcl_excel_type_analyzer IMPLEMENTATION.
  METHOD constructor.
    CLEAR mt_cache.
  ENDMETHOD.

  METHOD zif_excel_type_analyzer~analyze_structure.
    DATA lv_hash TYPE string.
    DATA ls_cache_entry TYPE ty_cache_entry.

    TRY.
        lv_hash = get_type_hash( io_type_descr ).
        READ TABLE mt_cache INTO ls_cache_entry WITH KEY type_hash = lv_hash.
        IF sy-subrc = 0.
          rt_fields = ls_cache_entry-fields.
          RETURN.
        ENDIF.

        analyze_structure_recursive( EXPORTING io_type_descr = io_type_descr
                                     CHANGING  ct_fields     = rt_fields ).

        ls_cache_entry-type_hash = lv_hash.
        ls_cache_entry-fields    = rt_fields.
        INSERT ls_cache_entry INTO TABLE mt_cache.

        DATA lx_error TYPE REF TO cx_root.
      CATCH cx_root INTO lx_error.
        DATA temp1 TYPE REF TO zcx_excel_dynamic_table.
        CREATE OBJECT temp1 TYPE zcx_excel_dynamic_table EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-type_analysis_failed iv_message = |Type analysis failed: { lx_error->get_text( ) }| ix_previous = lx_error.
        RAISE EXCEPTION temp1.
    ENDTRY.
  ENDMETHOD.

  METHOD get_type_hash.
    DATA lv_type_info TYPE string.
    lv_type_info = |{ io_type_descr->kind }{ io_type_descr->type_kind }{ io_type_descr->length }|.
    IF io_type_descr->kind = cl_abap_typedescr=>kind_struct.
      DATA temp2 TYPE REF TO cl_abap_structdescr.
      temp2 ?= io_type_descr.
      DATA lo_struct LIKE temp2.
      lo_struct = temp2.
      DATA lt_components TYPE abap_component_tab.
      lt_components = lo_struct->get_components( ).
      DATA ls_comp LIKE LINE OF lt_components.
      LOOP AT lt_components INTO ls_comp.
        lv_type_info = |{ lv_type_info }{ ls_comp-name }{ ls_comp-type->type_kind }|.
      ENDLOOP.
    ENDIF.
    rv_hash = lv_type_info.
  ENDMETHOD.

  METHOD analyze_structure_recursive.
    DATA ls_field TYPE zif_excel_type_analyzer=>ty_field_info.
    DATA lt_components TYPE cl_abap_structdescr=>component_table.

    CASE io_type_descr->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        DATA temp3 TYPE REF TO cl_abap_structdescr.
        temp3 ?= io_type_descr.
        DATA lo_struct LIKE temp3.
        lo_struct = temp3.
        lt_components = lo_struct->get_components( ).

        DATA lv_has_nested LIKE abap_false.
        lv_has_nested = abap_false.
        DATA ls_comp LIKE LINE OF lt_components.
        LOOP AT lt_components INTO ls_comp.
          IF ls_comp-name = 'NAME' OR ls_comp-name = 'NODES'.
            CONTINUE.
          ENDIF.
          IF ls_comp-type->kind = cl_abap_typedescr=>kind_struct OR ls_comp-type->kind = cl_abap_typedescr=>kind_table.
            lv_has_nested = abap_true.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_components INTO ls_comp.
          IF ls_comp-name = 'NAME' OR ls_comp-name = 'NODES'.
            CONTINUE.
          ENDIF.
          CASE ls_comp-type->kind.
            WHEN cl_abap_typedescr=>kind_elem.
              IF lv_has_nested = abap_false.
                ls_field-name = ls_comp-name.
                DATA temp4 TYPE REF TO cl_abap_datadescr.
                temp4 ?= ls_comp-type.
                ls_field-type = temp4.
                CASE ls_comp-type->type_kind.
                  WHEN cl_abap_typedescr=>typekind_packed OR
                       cl_abap_typedescr=>typekind_int OR
                       cl_abap_typedescr=>typekind_int1 OR
                       cl_abap_typedescr=>typekind_int2 OR
                       cl_abap_typedescr=>typekind_float OR
                       cl_abap_typedescr=>typekind_decfloat OR
                       cl_abap_typedescr=>typekind_decfloat16 OR
                       cl_abap_typedescr=>typekind_decfloat34.
                    ls_field-is_numeric = abap_true.
                  WHEN OTHERS.
                    ls_field-is_numeric = abap_false.
                ENDCASE.
                APPEND ls_field TO ct_fields.
              ENDIF.

            WHEN cl_abap_typedescr=>kind_struct.
              analyze_structure_recursive( EXPORTING io_type_descr = ls_comp-type
                                                     iv_prefix     = ''
                                           CHANGING  ct_fields     = ct_fields ).

            WHEN cl_abap_typedescr=>kind_table.
              DATA temp5 TYPE REF TO cl_abap_tabledescr.
              temp5 ?= ls_comp-type.
              DATA lo_table LIKE temp5.
              lo_table = temp5.
              analyze_structure_recursive( EXPORTING io_type_descr = lo_table->get_table_line_type( )
                                                     iv_prefix     = ''
                                           CHANGING  ct_fields     = ct_fields ).
          ENDCASE.
        ENDLOOP.

      WHEN cl_abap_typedescr=>kind_table.
        DATA temp6 TYPE REF TO cl_abap_tabledescr.
        temp6 ?= io_type_descr.
        DATA lo_table_descr LIKE temp6.
        lo_table_descr = temp6.
        analyze_structure_recursive( EXPORTING io_type_descr = lo_table_descr->get_table_line_type( )
                                               iv_prefix     = ''
                                     CHANGING  ct_fields     = ct_fields ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
