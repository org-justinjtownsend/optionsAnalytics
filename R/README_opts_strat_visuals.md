# Options Strategy Table Function

## Overview

The `opts_strat_tbl()` function creates interactive, extensible tables for displaying options strategies data using the `reactable` package. This function is designed to help options analysts determine the best strategies to use by providing a user-friendly interface for exploring strategy data.

## Function Signature

```r
opts_strat_tbl(data, columns = NULL, searchable = TRUE, 
               sortable = TRUE, striped = TRUE, compact = TRUE, 
               showDetail = FALSE)
```

## Parameters

- **`data`** (required): A data frame containing options strategies data (e.g., output of `create_vertical_spreads()$vsData`)
- **`columns`** (optional): A character vector specifying which columns to display. If not provided, uses all columns from the data
- **`searchable`** (optional): Logical. Whether to enable search functionality (default: `TRUE`)
- **`sortable`** (optional): Logical. Whether to enable sorting (default: `TRUE`, uses reactable default behavior)
- **`striped`** (optional): Logical. Whether to use striped rows for better visibility (default: `TRUE`)
- **`compact`** (optional): Logical. Whether to use compact display for better visibility with many rows (default: `TRUE`)
- **`showDetail`** (optional): Logical. Whether to show expandable detail rows (default: `FALSE`)

## Return Value

Returns a `reactable` widget that can be:
- Displayed in RStudio
- Embedded in R Markdown documents
- Exported to HTML
- Customized with additional styling

## Usage Examples

### Basic Usage

```r
# Create table with default settings
library(optionsAnalytics)
vs_result <- create_vertical_spreads(...)
table <- opts_strat_tbl(data = vs_result$vsData)
```

### Custom Columns

```r
# Display only specific columns
custom_cols <- c("dte", "vsType", "strikeL", "strikeH", "vsValue", "p_max")
table <- opts_strat_tbl(
  data = vs_result$vsData,
  columns = custom_cols
)
```

### Parameter Customization

```r
# Customize table appearance
table <- opts_strat_tbl(
  data = vs_result$vsData,
  searchable = FALSE,    # Disable search
  striped = FALSE,       # Disable stripes
  compact = FALSE        # Disable compact mode
)
```

### Data Filtering

```r
# Filter data before creating table
lcs_data <- vs_result$vsData[vs_result$vsData$vsType == "lcs", ]
table <- opts_strat_tbl(data = lcs_data)
```

## Expected Data Structure

The function expects data with columns typically found in vertical spreads output:

- **`dte`**: Days to expiration
- **`dist`**: Distance between strike prices
- **`vsType`**: Vertical spread type (lcs, scs, lps, sps)
- **`strikeL`**: Lower strike price
- **`strikeH`**: Higher strike price
- **`vsValue`**: Strategy value
- **`p_max`**: Maximum profit
- **`bep`**: Breakeven point
- **`instrType`**: Instrument type (c for calls, p for puts)
- **`posType`**: Position type (dr_s for debit, cr_s for credit)

## Features

### Interactive Capabilities
- **Search**: Find specific strategies quickly
- **Sort**: Sort by any column (ascending/descending)
- **Filter**: Built-in filtering capabilities
- **Pagination**: Handle large datasets efficiently

### Visual Enhancements
- **Striped rows**: Alternating row colors for better readability
- **Compact mode**: Optimized spacing for many rows
- **Professional styling**: Clean, modern appearance
- **Responsive design**: Adapts to different screen sizes

### Data Handling
- **Input validation**: Ensures data is properly formatted
- **Error handling**: Graceful handling of invalid inputs
- **Column selection**: Flexible column display options
- **Performance**: Efficient processing of large datasets

## Integration with Options Analytics

This function is designed to work seamlessly with the options analytics pipeline:

1. **Data Source**: Accepts output from `create_vertical_spreads()`
2. **Format Compatibility**: Works with standard vertical spreads data structure
3. **Workflow Integration**: Fits into the options analysis workflow
4. **Extensibility**: Ready for future enhancements (sparklines, metrics, etc.)

## Future Enhancements

The function is designed to be extensible for future features:

- **Historical sparklines**: Visual representation of strategy performance over time
- **Risk metrics**: Additional risk/reward calculations
- **Performance indicators**: Win rates, Sharpe ratios, drawdown analysis
- **Interactive filtering**: Advanced filtering and sorting options
- **Export capabilities**: CSV, Excel, PDF export options

## Dependencies

- **`reactable`**: Core table functionality
- **`dplyr`**: Data manipulation (if needed for filtering)
- **Base R**: Core functionality

## Testing

The function includes comprehensive testing:

- **Unit tests**: Basic functionality validation
- **Integration tests**: Real data testing
- **Performance tests**: Large dataset handling
- **Error handling tests**: Invalid input validation

## Troubleshooting

### Common Issues

1. **Package not found**: Ensure `reactable` is installed
2. **Data format errors**: Verify data structure matches expected format
3. **Performance issues**: Use `compact = TRUE` for large datasets
4. **Display issues**: Check RStudio/R Markdown compatibility

### Error Messages

- **"data must be a data frame"**: Input is not a data frame
- **"Missing columns"**: Required columns not found in data
- **"No data available"**: Empty or invalid data

## Support

For issues or questions:
1. Check the test files for usage examples
2. Verify data format matches expected structure
3. Review error messages for specific issues
4. Test with sample data first

## Version History

- **v1.0.0**: Initial implementation with basic functionality
- Basic table creation with reactable
- Parameter customization options
- Input validation and error handling
- Comprehensive testing suite
