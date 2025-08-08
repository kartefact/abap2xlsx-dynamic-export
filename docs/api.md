# API Reference

## Interface: ZIF_EXCEL_DYNAMIC_TABLE

Main interface for dynamic table export functionality.

### Methods

#### export_to_xlsx()

Exports data to XLSX format.

**Parameters:**

- `io_data` (REF TO data) - Reference to the data to be exported
- `is_options` (ty_export_options, optional) - Export options including field mappings
- `iv_title` (string, default: 'Title') - Title for the Excel file

**Returns:** Base64 encoded string of the XLSX file

**Raises:** zcx_excel_dynamic_table

#### export_to_xls()

Exports data to legacy XLS format.

**Parameters:**

- `io_data` (REF TO data) - Reference to the data to be exported
- `is_options` (ty_export_options, optional) - Export options including field mappings
- `iv_title` (string, default: 'Title') - Title for the Excel file

**Returns:** Base64 encoded string of the XLS file

**Raises:** zcx_excel_dynamic_table

#### export_to_csv()

Exports data to CSV format with configurable options.

**Parameters:**

- `io_data` (REF TO data) - Reference to the data to be exported
- `is_options` (ty_export_options, optional) - Export options including CSV settings
- `iv_title` (string, default: 'Title') - Title for the CSV file

**Returns:** Base64 encoded string of the CSV file

**Raises:** zcx_excel_dynamic_table

#### export_data()

Generic export method that supports all formats based on options.

**Parameters:**

- `io_data` (REF TO data) - Reference to the data to be exported
- `is_options` (ty_export_options, optional) - Export options including format selection
- `iv_title` (string, default: 'Title') - Title for the file

**Returns:** Base64 encoded string of the file

**Raises:** zcx_excel_dynamic_table

### Data Types

#### ty_export_options

Main configuration structure for export operations.

**Fields:**

- Includes all fields from `zexcel_s_table_settings`
- `field_mappings` (ty_field_mappings) - Custom field mappings
- `export_format` (c LENGTH 1) - Export format ('X'=XLSX, 'L'=XLS, 'C'=CSV)
- `csv_options` (ty_csv_options) - CSV-specific configuration

#### ty_csv_options

CSV-specific configuration options.

**Fields:**

- `delimiter` (c LENGTH 1) - Field separator character (default: ',')
- `enclosure` (c LENGTH 1) - Text qualifier character (default: '"')
- `line_ending` (string) - Line terminator sequence
- `indentation` (c LENGTH 1) - Indentation type ('S'=spaces, 'C'=columns)

#### ty_field_mapping

Maps ABAP fields to Excel columns with custom names.

**Fields:**

- `abap_field` (string) - ABAP field name
- `excel_column` (zexcel_cell_column_alpha) - Excel column identifier
- `excel_field_name` (string) - Custom field name for Excel header

## Export Format Codes

| Code | Format | Description |
|------|--------|-------------|
| 'X' | XLSX | Modern Excel format (default) |
| 'L' | XLS | Legacy Excel format |
| 'C' | CSV | Comma-separated values |

## CSV Indentation Types

| Code | Type | Description |
|------|------|-------------|
| 'S' | Spaces | Traditional space-based indentation in NODE column |
| 'C' | Columns | Column-based hierarchy using LEVEL_X columns |

## Error Handling

All methods raise `zcx_excel_dynamic_table` exceptions with specific error codes:

- `INVALID_INPUT` - Invalid or missing input data
- `FLATTENING_FAILED` - Error during data structure flattening
- `EXCEL_CREATION_FAILED` - Error during Excel file generation
- `CSV_CREATION_FAILED` - Error during CSV file generation

## Usage Patterns

### Basic Export

```abap
DATA(lv_result) = lo_exporter->export_to_xlsx( io_data = lo_data_ref ).
```

### Format-Specific Export

```abap
DATA(lv_csv) = lo_exporter->export_to_csv( 
  io_data = lo_data_ref
  iv_title = 'My CSV Export'
  is_options = ls_csv_options
).
```

### Generic Export with Format Selection

```abap
ls_options-export_format = 'C'.
DATA(lv_result) = lo_exporter->export_data(
  io_data = lo_data_ref
  is_options = ls_options
).
```
