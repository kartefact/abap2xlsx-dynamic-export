"! Custom exception for dynamic table operations
CLASS zcx_excel_dynamic_table DEFINITION
  PUBLIC
  INHERITING FROM zcx_excel FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_error_codes,
        invalid_input         TYPE string VALUE 'INVALID_INPUT',
        type_analysis_failed  TYPE string VALUE 'TYPE_ANALYSIS_FAILED',
        flattening_failed     TYPE string VALUE 'FLATTENING_FAILED',
        excel_creation_failed TYPE string VALUE 'EXCEL_CREATION_FAILED',
        memory_exceeded       TYPE string VALUE 'MEMORY_EXCEEDED',
      END OF gc_error_codes.


    "! Constructor for the exception class
    "! @parameter iv_error_code | Error code for the exception
    "! @parameter iv_message | Error message for the exception
    "! @parameter ix_previous | Optional previous exception
    METHODS constructor
      IMPORTING iv_error_code TYPE string
                iv_message    TYPE string
                ix_previous   TYPE REF TO cx_root OPTIONAL.

  PRIVATE SECTION.
    DATA mv_error_code TYPE string.
ENDCLASS.


CLASS zcx_excel_dynamic_table IMPLEMENTATION.
  METHOD constructor.
    super->constructor( error = iv_message ).
    mv_error_code = iv_error_code.
    IF ix_previous IS BOUND.
      previous = ix_previous.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
