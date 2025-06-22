#!/bin/bash

# Comprehensive MD Table Ecosystem Test
# Tests all md_ scripts in a realistic business intelligence workflow

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test directories
TEST_DIR="/var/home/zach/.dotfiles/tests/scripts/md_"
INPUT_DIR="${TEST_DIR}/input"
OUTPUT_DIR="${TEST_DIR}/output"
TEMP_DIR="${TEST_DIR}/temp"

# Ensure clean output directory
rm -rf "${OUTPUT_DIR}"
mkdir -p "${OUTPUT_DIR}" "${TEMP_DIR}"

echo -e "${BLUE}=== Comprehensive MD Table Ecosystem Test ===${NC}"
echo "Testing all md_ scripts in a business intelligence workflow"
echo

# Function to print test steps
print_step() {
    echo -e "${YELLOW}Step $1: $2${NC}"
}

# Function to validate output
validate_output() {
    local file="$1"
    local description="$2"
    
    if [[ -f "$file" && -s "$file" ]]; then
        echo -e "${GREEN}✓ $description - File created successfully${NC}"
        echo "  Size: $(wc -l < "$file") lines, $(wc -c < "$file") bytes"
    else
        echo -e "${RED}✗ $description - File missing or empty${NC}"
        return 1
    fi
}

# Test counter
step=1

print_step $((step++)) "Generate synthetic customer data using md_table_generate"
echo "Generating 50 synthetic customers with realistic relationships..."
md_table_generate \
    --columns "customer_id:uuid,name:name,email:email,company:company,age:int(25-65),annual_revenue:int(100000-1000000),industry:choice(Technology|Healthcare|Finance|Retail|Manufacturing)" \
    --rows 50 \
    --seed 42 \
    > "${OUTPUT_DIR}/customers.md"

validate_output "${OUTPUT_DIR}/customers.md" "Synthetic customer data generation"

print_step $((step++)) "Backup original sales data using md_table_backup"
echo "Creating encrypted backup of sales data..."
cd "${INPUT_DIR}"
cat sales_data.md | md_table_backup \
    --backup \
    --compress gzip \
    --encrypt \
    --password "test123" \
    --backup-dir "${OUTPUT_DIR}" \
    > "${OUTPUT_DIR}/backup_log.txt"

validate_output "${OUTPUT_DIR}/backup_log.txt" "Backup operation log"

print_step $((step++)) "Normalize and clean sales data using md_table_normalize"
echo "Cleaning data and handling inconsistencies..."
cat "${INPUT_DIR}/sales_data.md" | md_table_normalize \
    --full-pipeline \
    --strategy mean \
    --report "${OUTPUT_DIR}/normalization_report.json" \
    > "${OUTPUT_DIR}/sales_normalized.md"

validate_output "${OUTPUT_DIR}/sales_normalized.md" "Normalized sales data"
validate_output "${OUTPUT_DIR}/normalization_report.json" "Normalization report"

print_step $((step++)) "Query sales data using md_table_query"
echo "Executing SQL queries on sales data..."
cat "${OUTPUT_DIR}/sales_normalized.md" | md_table_query \
    --query "SELECT Region, Product, SUM(Total) as Total_Sales, COUNT(*) as Transaction_Count FROM data GROUP BY Region, Product ORDER BY Total_Sales DESC" \
    > "${OUTPUT_DIR}/sales_by_region_product.md"

validate_output "${OUTPUT_DIR}/sales_by_region_product.md" "SQL query results"

print_step $((step++)) "Reshape data using md_table_reshape"
echo "Pivoting sales data by region and month..."
cat "${OUTPUT_DIR}/sales_normalized.md" | md_table_reshape \
    --pivot "index=Product,values=Total,columns=Region" \
    --aggfunc sum \
    > "${OUTPUT_DIR}/sales_pivot.md"

validate_output "${OUTPUT_DIR}/sales_pivot.md" "Pivoted sales data"

print_step $((step++)) "Aggregate statistics using md_table_aggregate"
echo "Computing comprehensive sales statistics..."
cat "${OUTPUT_DIR}/sales_normalized.md" | md_table_aggregate \
    --groupby "Region,Category" \
    --agg "Total:sum+mean+std,Quantity:sum+mean" \
    > "${OUTPUT_DIR}/sales_stats.md"

validate_output "${OUTPUT_DIR}/sales_stats.md" "Aggregated statistics"

print_step $((step++)) "Calculate additional metrics using md_table_calc"
echo "Adding calculated columns for analysis..."
cat "${INPUT_DIR}/sales_data.md" | md_table_calc \
    --formula "Commission = \$_Total * 0.05" \
    --formula "Profit_Margin = (G - (G * 0.6)) / G" \
    > "${OUTPUT_DIR}/sales_with_calculations.md"

validate_output "${OUTPUT_DIR}/sales_with_calculations.md" "Sales data with calculations"

print_step $((step++)) "Analyze data patterns using md_table_analyze"
echo "Performing statistical analysis and pattern detection..."
cat "${OUTPUT_DIR}/sales_with_calculations.md" | md_table_analyze \
    --all \
    --generate-insights \
    --output "${OUTPUT_DIR}/analysis_report.md"

validate_output "${OUTPUT_DIR}/analysis_report.md" "Statistical analysis report"

print_step $((step++)) "Profile data quality using md_table_profile"
echo "Generating comprehensive data profile..."
cat "${OUTPUT_DIR}/sales_with_calculations.md" | md_table_profile \
    --output "${OUTPUT_DIR}/data_profile.md"

validate_output "${OUTPUT_DIR}/data_profile.md" "Data quality profile"

print_step $((step++)) "Train ML model using md_table_ml"
echo "Training regression model to predict sales..."
cat "${OUTPUT_DIR}/sales_with_calculations.md" | md_table_ml \
    --regress \
    --target "Total" \
    --features "Quantity,Unit_Price" \
    --algorithm "rf" \
    --cross-validate \
    --save-model "${OUTPUT_DIR}/sales_model.pkl" \
    > "${OUTPUT_DIR}/ml_results.md"

validate_output "${OUTPUT_DIR}/ml_results.md" "ML model results"

print_step $((step++)) "Forecast future sales using md_table_forecast"
echo "Generating sales forecasts..."
cat "${OUTPUT_DIR}/sales_normalized.md" | md_table_forecast \
    --date-column "Date" \
    --value-column "Total" \
    --periods 6 \
    --method "ensemble" \
    --evaluate \
    > "${OUTPUT_DIR}/sales_forecast.md"

validate_output "${OUTPUT_DIR}/sales_forecast.md" "Sales forecast"

print_step $((step++)) "Create visualizations using md_table_plot"
echo "Generating data visualizations..."
cat "${OUTPUT_DIR}/sales_stats.md" | md_table_plot \
    --type bar \
    --columns "Region,Total_sum" \
    --output "${OUTPUT_DIR}/sales_by_region.png" \
    --title "Sales by Region"

validate_output "${OUTPUT_DIR}/sales_by_region.png" "Sales visualization"

print_step $((step++)) "Generate summary report using md_table_summarize"
echo "Creating executive summary..."
cat "${OUTPUT_DIR}/sales_with_calculations.md" | md_table_summarize \
    --comprehensive \
    --insights \
    > "${OUTPUT_DIR}/executive_summary.md"

validate_output "${OUTPUT_DIR}/executive_summary.md" "Executive summary"

print_step $((step++)) "Create templates using md_table_template"
echo "Saving sales template for future use..."
cat "${OUTPUT_DIR}/sales_normalized.md" | md_table_template \
    --save-template "sales_template" \
    --description "Normalized sales data template"

echo "Applying template with parameters..."
md_table_template \
    --apply-template "sales_template" \
    --params "date=$(date +%Y-%m-%d),company=TestCorp" \
    --customize-fields "rows=5" \
    > "${OUTPUT_DIR}/template_applied.md"

validate_output "${OUTPUT_DIR}/template_applied.md" "Template application"

print_step $((step++)) "Export to multiple formats using md_table_export_enhanced"
echo "Exporting final results to various formats..."
cat "${OUTPUT_DIR}/executive_summary.md" | md_table_export_enhanced \
    --format excel \
    --style professional \
    --sheet-name "Executive Summary" \
    --output "${OUTPUT_DIR}/final_report.xlsx"

cat "${OUTPUT_DIR}/sales_stats.md" | md_table_export_enhanced \
    --format json \
    --output "${OUTPUT_DIR}/sales_stats.json"

validate_output "${OUTPUT_DIR}/final_report.xlsx" "Excel export"
validate_output "${OUTPUT_DIR}/sales_stats.json" "JSON export"

print_step $((step++)) "Create interactive dashboard using md_table_dashboard"
echo "Building interactive dashboard..."
cat "${OUTPUT_DIR}/sales_with_calculations.md" | md_table_dashboard \
    --title "Sales Analytics Dashboard" \
    --output "${OUTPUT_DIR}/dashboard.html" \
    --no-serve

validate_output "${OUTPUT_DIR}/dashboard.html" "Interactive dashboard"

print_step $((step++)) "Convert between formats using md_from_csv and md_to_csv"
echo "Testing CSV conversion utilities..."
cat "${OUTPUT_DIR}/sales_stats.md" | md_to_csv > "${OUTPUT_DIR}/sales_stats.csv"
cat "${OUTPUT_DIR}/sales_stats.csv" | md_from_csv > "${OUTPUT_DIR}/sales_stats_converted.md"

validate_output "${OUTPUT_DIR}/sales_stats.csv" "CSV conversion"
validate_output "${OUTPUT_DIR}/sales_stats_converted.md" "CSV to markdown conversion"

print_step $((step++)) "Sync data using md_table_sync (dry run)"
echo "Testing database sync capabilities..."
cat "${OUTPUT_DIR}/sales_normalized.md" | md_table_sync \
    --source database \
    --connection-string "sqlite:///test.db" \
    --table sales \
    --push \
    --create-table \
    --dry-run \
    > "${OUTPUT_DIR}/sync_log.txt"

validate_output "${OUTPUT_DIR}/sync_log.txt" "Database sync log"

echo
echo -e "${BLUE}=== Workflow Validation ===${NC}"

# Count successful outputs
total_files=$(find "${OUTPUT_DIR}" -type f | wc -l)
echo "Total output files created: $total_files"

# Validate key workflow files
echo
echo "Key workflow validation:"

# Check that we have data flowing through the pipeline
original_rows=$(tail -n +3 "${INPUT_DIR}/sales_data.md" | wc -l)
normalized_rows=$(tail -n +3 "${OUTPUT_DIR}/sales_normalized.md" | wc -l)
echo "Original sales rows: $original_rows, Normalized rows: $normalized_rows"

# Check aggregation worked
stats_content=$(cat "${OUTPUT_DIR}/sales_stats.md" 2>/dev/null || echo "")
if echo "$stats_content" | grep -q "Total_sum"; then
    echo -e "${GREEN}✓ Aggregation pipeline working${NC}"
else
    echo -e "${RED}✗ Aggregation pipeline failed${NC}"
fi

# Check ML model was created
if [[ -f "${OUTPUT_DIR}/sales_model.pkl" ]]; then
    echo -e "${GREEN}✓ ML model trained and saved${NC}"
else
    echo -e "${RED}✗ ML model training failed${NC}"
fi

# Check dashboard was created
if [[ -f "${OUTPUT_DIR}/dashboard.html" ]] && grep -q "plotly" "${OUTPUT_DIR}/dashboard.html" 2>/dev/null; then
    echo -e "${GREEN}✓ Interactive dashboard created${NC}"
else
    echo -e "${RED}✗ Dashboard creation failed${NC}"
fi

# Check exports
if [[ -f "${OUTPUT_DIR}/final_report.xlsx" ]] && [[ -f "${OUTPUT_DIR}/sales_stats.json" ]]; then
    echo -e "${GREEN}✓ Multi-format exports successful${NC}"
else
    echo -e "${RED}✗ Export operations failed${NC}"
fi

echo
echo -e "${BLUE}=== Pipeline Integrity Test ===${NC}"

# Test a complex pipeline combining multiple tools
echo "Testing complete pipeline integration..."
cat "${INPUT_DIR}/sales_data.md" | \
    md_table_normalize --full-pipeline | \
    md_table_query --query "SELECT Region, AVG(Total) as Avg_Sales FROM data GROUP BY Region" | \
    md_table_calc --formula "Scaled_Sales = Avg_Sales / 1000" | \
    md_table_export_enhanced --format json \
    > "${OUTPUT_DIR}/pipeline_test.json"

if [[ -f "${OUTPUT_DIR}/pipeline_test.json" ]] && jq empty "${OUTPUT_DIR}/pipeline_test.json" 2>/dev/null; then
    echo -e "${GREEN}✓ Complex pipeline integration successful${NC}"
else
    echo -e "${RED}✗ Pipeline integration failed${NC}"
fi

echo
echo -e "${BLUE}=== Test Summary ===${NC}"
echo "Test completed. Output directory: ${OUTPUT_DIR}"
echo "Files generated: $total_files"
echo
echo "Key outputs:"
ls -la "${OUTPUT_DIR}" | grep -E "\.(md|json|xlsx|html|png|csv|pkl)$" | awk '{print "  " $9 " (" $5 " bytes)"}'

echo
echo -e "${GREEN}Comprehensive test workflow completed successfully!${NC}"
echo "All md_ scripts have been tested in a realistic business scenario."