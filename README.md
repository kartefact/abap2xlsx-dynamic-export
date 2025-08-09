# ABAP2XLSX Dynamic Export Add-on

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/kartefact/abap2xlsx-dynamic-export)

A powerful extension for the abap2xlsx framework that enables dynamic export of hierarchical ABAP data structures with automatic flattening to multiple formats.

## Features

- **Multi-Format Export**: Support for XLSX, XLS, and CSV formats
- **Hierarchical Data Flattening**: Automatically converts nested structures into flat tables
- **CSV Column-Based Indentation**: Advanced hierarchy visualization using separate level columns
- **Dynamic Type Analysis**: RTTS-based analysis with performance caching
- **Custom Field Mappings**: Rename columns and control export fields
- **Memory Management**: Built-in memory monitoring and limits
- **Dependency Injection**: Testable architecture with interface-based design

## Quick Start

### XLSX Export (Default)
```abap
DATA: lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      lv_excel TYPE string.

lo_exporter = NEW #( ).
GET REFERENCE OF your_data INTO DATA(lo_data).
lv_excel = lo_exporter->export_to_xlsx( 
  io_data = lo_data
  iv_title = 'My Export'
).
```

### CSV Export with Column Hierarchy

```abap
DATA: lo_exporter TYPE REF TO zcl_excel_dynamic_table,
      ls_options TYPE zif_excel_dynamic_table=>ty_export_options,
      lv_csv TYPE string.

" Configure CSV with column-based indentation
ls_options-csv_options = VALUE #(
  delimiter = ','
  enclosure = '"'
  indentation = 'C'  " Column-based hierarchy
).

lo_exporter = NEW #( ).
GET REFERENCE OF your_hierarchical_data INTO DATA(lo_data).
lv_csv = lo_exporter->export_to_csv(
  io_data = lo_data
  iv_title = 'Hierarchical Export'
  is_options = ls_options
).
```

### Generic Export (Auto-format Detection)

```abap
DATA: ls_options TYPE zif_excel_dynamic_table=>ty_export_options.

ls_options-export_format = 'C'.  " 'X'=XLSX, 'L'=XLS, 'C'=CSV
lv_result = lo_exporter->export_data(
  io_data = lo_data
  iv_title = 'Multi-Format Export'
  is_options = ls_options
).
```

## Export Formats

| Format | Method | Description |
|--------|--------|-------------|
| XLSX | `export_to_xlsx()` | Modern Excel format with full formatting |
| XLS | `export_to_xls()` | Legacy Excel format for compatibility |
| CSV | `export_to_csv()` | Comma-separated values with hierarchy options |
| Auto | `export_data()` | Generic method with format selection |

## CSV Hierarchy Options

- **Space-based** (`indentation = 'S'`): Traditional indented NODE column
- **Column-based** (`indentation = 'C'`): Separate LEVEL_X columns for clear hierarchy

## Documentation

- [Installation Guide](docs/installation.md)
- [Implementation Guide](docs/implementation.md)
- [Usage Examples](docs/examples.md)
- [API Reference](docs/api.md)
