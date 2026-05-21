# API Reference

## Interface: ZIF_EXCEL_DYNAMIC_TABLE

Main interface for dynamic table export functionality.

**Version:** `1.001.0`

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

CSV-specific configuration options. All fields are optional — unset fields use their documented defaults.

> **⚠️ Concurrency note:** `zcl_excel_writer_csv` stores these settings as `CLASS-DATA` (process-global static variables). If two work processes or parallel RFC calls execute a CSV export simultaneously, they will overwrite each other's settings. Serialise CSV exports or add an external mutex when parallel execution is required.

**Fields:**

| Field | Type | Default | Description |
|---|---|---|---|
| `delimiter` | c LENGTH 1 | `,` | Field separator character |
| `enclosure` | c LENGTH 1 | `"` | Text qualifier / quoting character |
| `line_ending` | string | CR+LF | Line terminator sequence |
| `indentation` | c LENGTH 1 | `S` | Hierarchy style: `S`=spaces in NODE column, `C`=separate LEVEL_X columns |
| `skip_hidden_rows` | abap_bool | `abap_false` | Skip rows hidden by AutoFilter in the source worksheet |
| `skip_hidden_cols` | abap_bool | `abap_false` | Skip columns marked as hidden in the source worksheet |
| `initial_ext_date` | char10 | *(user format)* | Replacement value written for empty/initial date cells. Leave blank to use the SAP user's date format. |

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

### CSV Export — Skip Hidden Rows/Columns

```abap
DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

ls_options-csv_options = VALUE #(
  delimiter        = ','
  enclosure        = '"'
  skip_hidden_rows = abap_true   " omit AutoFilter-hidden rows
  skip_hidden_cols = abap_true   " omit hidden columns
).

DATA(lv_csv) = lo_exporter->export_to_csv(
  io_data    = lo_data_ref
  iv_title   = 'Filtered Export'
  is_options = ls_options
).
```

### CSV Export — Fixed Date Format for Empty Dates

```abap
DATA ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

ls_options-csv_options = VALUE #(
  delimiter        = ','
  initial_ext_date = 'N/A'   " written for initial/empty date cells
).

DATA(lv_csv) = lo_exporter->export_to_csv(
  io_data    = lo_data_ref
  iv_title   = 'Date Export'
  is_options = ls_options
).
```
