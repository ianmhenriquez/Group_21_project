source("helper.r") # Ensure helper functions are available

# This script contains functions for merging and preparing the Olist dataset

# Main merging function that combines all data frames
# Inside merging.r

create_enriched_orders <- function(orders_raw, order_items_raw, payments_raw, 
                                   customers_raw, products_raw, sellers_raw, 
                                   translation_raw) {
  
  # Step 1: Aggregate payments to order level
  payments_agg <- aggregate(
    cbind(payment_value, payment_installments) ~ order_id,
    data = payments_raw,
    FUN = function(x) c(total = sum(x), max = max(x))
  )
  payments_agg <- data.frame(
    order_id                 = payments_agg$order_id,
    payment_value_total      = payments_agg$payment_value[, "total"],
    payment_installments_max = payments_agg$payment_installments[, "max"]
  )
  
  # Step 2: Translate product categories
  products <- merge(products_raw, translation_raw, 
                    by = "product_category_name", all.x = TRUE)
  
  # Replace NA categories with "Miscellaneous"
  products$product_category_name_english[is.na(products$product_category_name_english)] <- "Miscellaneous"
  
  # Drop columns not needed downstream
  products <- products[, !(names(products) %in% c("product_category_name", 
                                                  "product_name_lenght", 
                                                  "product_description_lenght", 
                                                  "product_photos_qty"))]
  
  # Step 3: Enrich order items with products and sellers
  order_items_enriched <- merge(order_items_raw, products, by = "product_id", all.x = TRUE)
  order_items_enriched <- merge(order_items_enriched, sellers_raw, by = "seller_id", all.x = TRUE)
  
  # Step 4: Prepare the orders spine
  orders <- orders_raw
  
  # Robust Date Cleaning: Replace empty strings with NA to prevent parsing errors
  date_cols <- c("order_purchase_timestamp", "order_approved_at",
                 "order_delivered_carrier_date", "order_delivered_customer_date",
                 "order_estimated_delivery_date")
  for (col in date_cols) {
    orders[[col]][orders[[col]] == ""] <- NA
  }
  
  # Parse dates
  orders$order_purchase_timestamp      <- as.POSIXct(orders$order_purchase_timestamp)
  orders$order_approved_at             <- as.POSIXct(orders$order_approved_at)
  orders$order_delivered_carrier_date  <- as.Date(orders$order_delivered_carrier_date)
  orders$order_delivered_customer_date <- as.Date(orders$order_delivered_customer_date)
  orders$order_estimated_delivery_date <- as.Date(orders$order_estimated_delivery_date)
  
  # Join aggregated payments and customers
  orders <- merge(orders, payments_agg, by = "order_id", all.x = TRUE)
  orders <- merge(orders, customers_raw, by = "customer_id", all.x = TRUE)
  
  # Filter to delivered orders with both delivery dates present
  orders <- orders[!is.na(orders$order_delivered_customer_date) & 
                   !is.na(orders$order_estimated_delivery_date), ]
  
  # Construct target variables
  orders$is_late <- factor(
    ifelse(orders$order_delivered_customer_date > orders$order_estimated_delivery_date, 
           "Late", "On_Time"),
    levels = c("On_Time", "Late")
  )
  
  orders$delivery_delay_days <- as.numeric(difftime(
    orders$order_delivered_customer_date,
    orders$order_estimated_delivery_date,
    units = "days"
  ))
  
  # Step 5: Final Join
  orders_enriched <- merge(orders, order_items_enriched, by = "order_id", all.x = TRUE)
  
  return(orders_enriched)
}
