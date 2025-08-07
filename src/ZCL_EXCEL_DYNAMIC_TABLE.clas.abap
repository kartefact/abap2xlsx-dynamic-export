"! Main dynamic table class with dependency injection
CLASS zcl_excel_dynamic_table DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_excel_dynamic_table.

    "! Constructor for dependency injection
    "! @parameter io_flattener | Dependency injection for table flattener
    "! @parameter io_analyzer  | Dependency injection for type analyzer
    METHODS constructor
      IMPORTING io_flattener TYPE REF TO zif_excel_table_flattener OPTIONAL
                io_analyzer  TYPE REF TO zif_excel_type_analyzer   OPTIONAL.

  PRIVATE SECTION.
    DATA mo_flattener TYPE REF TO zif_excel_table_flattener.
    DATA mo_analyzer  TYPE REF TO zif_excel_type_analyzer.

    " Format constants for export types
    CONSTANTS: BEGIN OF gc_export_format,
                 xlsx TYPE c LENGTH 1 VALUE 'X',
                 xls  TYPE c LENGTH 1 VALUE 'L',
                 csv  TYPE c LENGTH 1 VALUE 'C',
               END OF gc_export_format.

    " CSV option defaults
    CONSTANTS: BEGIN OF gc_csv_defaults,
                 delimiter   TYPE c LENGTH 1 VALUE ',',
                 enclosure   TYPE c LENGTH 1 VALUE '"',
                 indentation TYPE c LENGTH 1 VALUE 'S',
               END OF gc_csv_defaults.

    "! Validates the input data reference
    "! @parameter io_data                 |
    "! @raising   zcx_excel_dynamic_table |
    METHODS validate_input
      IMPORTING io_data TYPE REF TO data
      RAISING   zcx_excel_dynamic_table.

    "! Creates an Excel file from flattened data
    "! @parameter io_flat_table           |
    "! @parameter i_table_title           |
    "! @parameter is_export_options       |
    "! @parameter iv_format               |
    "! @parameter rv_base64               |
    "! @raising   zcx_excel_dynamic_table |
    METHODS create_excel_file
      IMPORTING io_flat_table     TYPE REF TO data
                i_table_title     TYPE string
                is_export_options TYPE zif_excel_dynamic_table=>ty_export_options
                iv_format         TYPE c DEFAULT 'X'
      RETURNING VALUE(rv_base64)  TYPE string
      RAISING   zcx_excel_dynamic_table.

    "! Creates a CSV file from flattened data
    "! @parameter io_flat_table           |
    "! @parameter i_table_title           |
    "! @parameter is_export_options       |
    "! @parameter rv_base64               |
    "! @raising   zcx_excel_dynamic_table |
    METHODS create_csv_file
      IMPORTING io_flat_table     TYPE REF TO data
                i_table_title     TYPE string
                is_export_options TYPE zif_excel_dynamic_table=>ty_export_options
      RETURNING VALUE(rv_base64)  TYPE string
      RAISING   zcx_excel_dynamic_table.

    "! Formats the field catalog for display
    "! @parameter is_export_options |
    "! @parameter ct_field_catalog  |
    METHODS format_field_catalog
      IMPORTING is_export_options TYPE zif_excel_dynamic_table=>ty_export_options
      CHANGING  ct_field_catalog  TYPE zexcel_t_fieldcatalog.

    "! Sets default CSV options if not provided
    "! @parameter cs_options |
    METHODS set_csv_defaults
      CHANGING cs_options TYPE zif_excel_dynamic_table=>ty_export_options.
ENDCLASS.


CLASS zcl_excel_dynamic_table IMPLEMENTATION.
  METHOD constructor.
    IF io_analyzer IS BOUND.
      mo_analyzer = io_analyzer.
    ELSE.
      mo_analyzer = NEW zcl_excel_type_analyzer( ).
    ENDIF.

    IF io_flattener IS BOUND.
      mo_flattener = io_flattener.
    ELSE.
      mo_flattener = NEW zcl_excel_table_flattener( mo_analyzer ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_excel_dynamic_table~export_to_xlsx.
    DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

    ls_options = is_options.
    ls_options-export_format = gc_export_format-xlsx.

    TRY.
        validate_input( io_data ).

        DATA(lo_type_descr) = cl_abap_typedescr=>describe_by_data_ref( io_data ).

        DATA(lo_flat_table) = mo_flattener->flatten( io_data           = io_data
                                                     io_type_descr     = lo_type_descr
                                                     is_export_options = ls_options ).

        rv_base64 = create_excel_file( io_flat_table     = lo_flat_table
                                       i_table_title     = iv_title
                                       is_export_options = ls_options
                                       iv_format         = gc_export_format-xlsx ).

      CATCH zcx_excel_dynamic_table
            cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-excel_creation_failed
                    iv_message    = |Failed to create XLSX file: { lx_error->get_text( ) }|
                    ix_previous   = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_excel_dynamic_table~export_to_xls.
    DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

    ls_options = is_options.
    ls_options-export_format = gc_export_format-xls.

    TRY.
        validate_input( io_data ).

        DATA(lo_type_descr) = cl_abap_typedescr=>describe_by_data_ref( io_data ).

        DATA(lo_flat_table) = mo_flattener->flatten( io_data           = io_data
                                                     io_type_descr     = lo_type_descr
                                                     is_export_options = ls_options ).

        rv_base64 = create_excel_file( io_flat_table     = lo_flat_table
                                       i_table_title     = iv_title
                                       is_export_options = ls_options
                                       iv_format         = gc_export_format-xls ).

      CATCH zcx_excel_dynamic_table
            cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-excel_creation_failed
                    iv_message    = |Failed to create XLS file: { lx_error->get_text( ) }|
                    ix_previous   = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_excel_dynamic_table~export_to_csv.
    DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

    ls_options = is_options.
    ls_options-export_format = gc_export_format-csv.
    set_csv_defaults( CHANGING cs_options = ls_options ).

    TRY.
        validate_input( io_data ).

        DATA(lo_type_descr) = cl_abap_typedescr=>describe_by_data_ref( io_data ).

        DATA(lo_flat_table) = mo_flattener->flatten( io_data           = io_data
                                                     io_type_descr     = lo_type_descr
                                                     is_export_options = ls_options ).

        rv_base64 = create_csv_file( io_flat_table     = lo_flat_table
                                     i_table_title     = iv_title
                                     is_export_options = ls_options ).

      CATCH zcx_excel_dynamic_table
            cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-excel_creation_failed
                    iv_message    = |Failed to create CSV file: { lx_error->get_text( ) }|
                    ix_previous   = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_excel_dynamic_table~export_data.
    " Set default format if not specified
    DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

    ls_options = is_options.
    IF ls_options-export_format IS INITIAL.
      ls_options-export_format = gc_export_format-xlsx.
    ENDIF.

    CASE ls_options-export_format.
      WHEN gc_export_format-xlsx.
        rv_base64 = zif_excel_dynamic_table~export_to_xlsx( io_data    = io_data
                                                            is_options = ls_options
                                                            iv_title   = iv_title ).
      WHEN gc_export_format-xls.
        rv_base64 = zif_excel_dynamic_table~export_to_xls( io_data    = io_data
                                                           is_options = ls_options
                                                           iv_title   = iv_title ).
      WHEN gc_export_format-csv.
        rv_base64 = zif_excel_dynamic_table~export_to_csv( io_data    = io_data
                                                           is_options = ls_options
                                                           iv_title   = iv_title ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                    iv_message    = |Unsupported export format: { ls_options-export_format }|.
    ENDCASE.
  ENDMETHOD.

  METHOD validate_input.
    IF io_data IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_excel_dynamic_table
        EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                  iv_message    = 'Input data reference is not bound'.
    ENDIF.

    DATA(lo_type_descr) = cl_abap_typedescr=>describe_by_data_ref( io_data ).
    IF     lo_type_descr->kind <> cl_abap_typedescr=>kind_table
       AND lo_type_descr->kind <> cl_abap_typedescr=>kind_struct.
      RAISE EXCEPTION TYPE zcx_excel_dynamic_table
        EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                  iv_message    = 'Input data must be a table or structure'.
    ENDIF.
  ENDMETHOD.

  METHOD create_excel_file.
    CONSTANTS lc_default_column_width TYPE i VALUE 15.

    DATA lo_excel          TYPE REF TO zcl_excel.
    DATA lo_worksheet      TYPE REF TO zcl_excel_worksheet.
    DATA lo_writer         TYPE REF TO zif_excel_writer.
    DATA lt_field_catalog  TYPE zexcel_t_fieldcatalog.
    DATA ls_table_settings TYPE zexcel_s_table_settings.

    FIELD-SYMBOLS <lt_flat> TYPE STANDARD TABLE.

    TRY.
        ASSIGN io_flat_table->* TO <lt_flat>.

        lo_excel = NEW #( ).
        lo_worksheet = lo_excel->get_active_worksheet( ).
        lo_worksheet->set_title( CONV zexcel_sheet_title( i_table_title ) ).

        " Map relevant fields from is_export_options to ls_table_settings
        MOVE-CORRESPONDING is_export_options TO ls_table_settings.
        IF ls_table_settings-table_style IS INITIAL.
          ls_table_settings-table_style = zcl_excel_table=>builtinstyle_medium5.
        ENDIF.
        ls_table_settings-show_row_stripes = abap_true.

        lt_field_catalog = zcl_excel_common=>get_fieldcatalog( <lt_flat> ).
        format_field_catalog( EXPORTING is_export_options = is_export_options
                              CHANGING  ct_field_catalog  = lt_field_catalog ).

        lo_worksheet->bind_table( ip_table          = <lt_flat>
                                  is_table_settings = ls_table_settings
                                  it_field_catalog  = lt_field_catalog ).

        lo_worksheet->freeze_panes( ip_num_rows = 1 ).

        DATA(l_col_count) = lines( lt_field_catalog ).
        DO l_col_count TIMES.
          lo_worksheet->set_column_width( ip_column    = sy-index
                                          ip_width_fix = lc_default_column_width ).
        ENDDO.

        " Select appropriate writer based on format
        CASE iv_format.
          WHEN gc_export_format-xlsx.
            lo_writer = NEW zcl_excel_writer_2007( ).
            DATA(l_binary) = lo_writer->write_file( lo_excel ).
            rv_base64 = cl_http_utility=>encode_x_base64( l_binary ).
            rv_base64 = |data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,{ rv_base64 }|.
          WHEN gc_export_format-xls.
            lo_writer = NEW zcl_excel_writer_huge_file( ).
            l_binary = lo_writer->write_file( lo_excel ).
            rv_base64 = cl_http_utility=>encode_x_base64( l_binary ).
            rv_base64 = |data:application/vnd.ms-excel;base64,{ rv_base64 }|.
          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_excel_dynamic_table
              EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-invalid_input
                        iv_message    = |Invalid format for Excel export: { iv_format }|.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-excel_creation_failed
                    iv_message    = |Failed to bind table to worksheet: { lx_error->get_text( ) }|
                    ix_previous   = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD create_csv_file.
    DATA lo_excel          TYPE REF TO zcl_excel.
    DATA lo_worksheet      TYPE REF TO zcl_excel_worksheet.
    DATA lo_writer         TYPE REF TO zcl_excel_writer_csv.
    DATA lt_field_catalog  TYPE zexcel_t_fieldcatalog.
    DATA ls_table_settings TYPE zexcel_s_table_settings.

    FIELD-SYMBOLS <lt_flat> TYPE STANDARD TABLE.

    TRY.
        ASSIGN io_flat_table->* TO <lt_flat>.

        lo_excel = NEW #( ).
        lo_worksheet = lo_excel->get_active_worksheet( ).
        lo_worksheet->set_title( CONV zexcel_sheet_title( i_table_title ) ).

        " Map relevant fields from is_export_options to ls_table_settings
        MOVE-CORRESPONDING is_export_options TO ls_table_settings.
        ls_table_settings-show_row_stripes = abap_false. " Not applicable for CSV

        lt_field_catalog = zcl_excel_common=>get_fieldcatalog( <lt_flat> ).
        format_field_catalog( EXPORTING is_export_options = is_export_options
                              CHANGING  ct_field_catalog  = lt_field_catalog ).

        lo_worksheet->bind_table( ip_table          = <lt_flat>
                                  is_table_settings = ls_table_settings
                                  it_field_catalog  = lt_field_catalog ).

        " Create CSV writer and configure options using class methods
        zcl_excel_writer_csv=>set_delimiter( is_export_options-csv_options-delimiter ).
        zcl_excel_writer_csv=>set_enclosure( is_export_options-csv_options-enclosure ).
        IF is_export_options-csv_options-line_ending IS NOT INITIAL.
          zcl_excel_writer_csv=>set_endofline( is_export_options-csv_options-line_ending ).
        ENDIF.

        lo_writer = NEW zcl_excel_writer_csv( ).
        DATA(l_csv_binary) = lo_writer->zif_excel_writer~write_file( lo_excel ).
        rv_base64 = cl_http_utility=>encode_x_base64( l_csv_binary ).
        rv_base64 = |data:text/csv;base64,{ rv_base64 }|.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_excel_dynamic_table
          EXPORTING iv_error_code = zcx_excel_dynamic_table=>gc_error_codes-excel_creation_failed
                    iv_message    = |Failed to create CSV file: { lx_error->get_text( ) }|
                    ix_previous   = lx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD format_field_catalog.
    LOOP AT ct_field_catalog ASSIGNING FIELD-SYMBOL(<ls_field>).
      DATA(lv_excel_field_name) = <ls_field>-fieldname.
      READ TABLE is_export_options-field_mappings INTO DATA(ls_mapping) WITH KEY abap_field = <ls_field>-fieldname.
      IF sy-subrc = 0 AND ls_mapping-excel_field_name IS NOT INITIAL.
        lv_excel_field_name = ls_mapping-excel_field_name.
      ELSEIF <ls_field>-fieldname = 'NODE'.
        lv_excel_field_name = 'Hierarchy'.
      ELSEIF <ls_field>-fieldname CP 'LEVEL_*'.
        " Handle level columns for CSV column-based indentation
        DATA(lv_level_num) = <ls_field>-fieldname+6.
        lv_excel_field_name = |Level { lv_level_num }|.
      ELSE.
        lv_excel_field_name = to_mixed( val  = <ls_field>-fieldname
                                        case = abap_true ).
      ENDIF.

      <ls_field>-scrtext_s = lv_excel_field_name.
      <ls_field>-scrtext_m = lv_excel_field_name.
      <ls_field>-scrtext_l = lv_excel_field_name.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_csv_defaults.
    " Set default CSV options if not provided
    IF cs_options-csv_options-delimiter IS INITIAL.
      cs_options-csv_options-delimiter = gc_csv_defaults-delimiter.
    ENDIF.
    IF cs_options-csv_options-enclosure IS INITIAL.
      cs_options-csv_options-enclosure = gc_csv_defaults-enclosure.
    ENDIF.
    IF cs_options-csv_options-line_ending IS INITIAL.
      cs_options-csv_options-line_ending = cl_abap_char_utilities=>cr_lf.
    ENDIF.
    IF cs_options-csv_options-indentation IS INITIAL.
      cs_options-csv_options-indentation = gc_csv_defaults-indentation.
    ENDIF.
  ENDMETHOD.
ENDCLASS.