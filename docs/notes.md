todo:

1. make sure calculated columns are actually correct. Possibly drop and recalculate?
    - fuel_cost = fuel_used_gallons * most_recent_fuel_purch_price
    - pce_eligible_total_kwh = sum(things)


2. revamp imputation code
    - don't just target all NULL values, use outlier log and possibly bounds log
    - back calculate most_recent_fuel_purch_price from (fuel_cost / fuel_used_gallons)
    - change "imputed_" boolean to imputation method
        NA for no imputations
        "last observation carried forward" or "kallman smoothing" "unknown" etc
    - seasonality
        some things are very seasonal
            generation
            generation type
        some things are stable throughout seasons
            customers
    - last observation carry forward (LCOF)
        rates
    - "cleaned_during_energy_statistics" starts as FALSE for new things
        set to TRUE after passing through The Tool



3. add community names to outputted files (outliers, etc)



5. AVEC negative fuel reporting
    - at end of year, AVEC reports values in order to adjust previous values
    - these values may be negative values for fuel_used_gallons, fuel_cost


6. within charting, we have some records that are calculated, display both in chart
    - line loss
        - (total_gen - powerhouse_consumption_kwh - total_sales) / (total_generation + total_purchased - powerhouse_generation)
    - diesel_fuel_efficiency
        - fuel_used_gallons
        - diesel_kwh_generated
    - fuel_cost
        - most_recent_fuel_purch_price
        - fuel_used_gallons


7. use historic (2001 - 2016) as reference data points for validation
    BUT do not attempt to clean!
    Also, do not attempt to clean data that has been cleaned
        some action must happen via The Tool in order for record to pass to L3
        until then, cleaned_during_energy_stats == FALSE





l0 - just CSV of XLSX file

l1 - combine all months into single consolidated (combine header and rate_line)
    - perform calculated columns
    - pivot other_generation
    - type checking of columns (numeric, boolean, etc)
    - back calculate AVEC specific adjustments (can't do until end of calendar year)

l2 - all QA/QC
    - out of bounds
    - statistical outliers
    - machine imputations
    - human imputations

l3 - final
    - clean, validated
    - options
        XLSX with multiple sheets
        CSV super wide format
        CSV truncated imputation columns
        CSV of imputation notes
        API
