# ABAP2XLSX Dynamic Export Add-on

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/kartefact/abap2xlsx-dynamic-export)

A powerful extension for the abap2xlsx framework that enables dynamic Excel export of hierarchical ABAP data structures with automatic flattening.

## Features

- **Hierarchical Data Flattening**: Automatically converts nested structures into flat Excel tables
- **Dynamic Type Analysis**: RTTS-based analysis with performance caching
- **Custom Field Mappings**: Rename columns and control export fields
- **Memory Management**: Built-in memory monitoring and limits
- **Dependency Injection**: Testable architecture with interface-based design

## Quick Start

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

## Documentation

- [Installation Guide](docs/INSTALLATION.md)
- [Implementation Guide](docs/IMPLEMENTATION.md)
- [Usage Examples](docs/EXAMPLES.md)
