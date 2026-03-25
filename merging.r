# Data Merging and Preparation
# This script contains functions for merging and preparing the Olist dataset

# Main merging function that combines all data frames
merge_olist_data <- function(products_translated, order_items_frame, orders_frame, payment_frame, customer_frame) {

  # Merge order items with translated product information
  order_items_enriched <- merge(
    order_items_frame,
    products_translated,
    by = "product_id",
    all.x = TRUE
  )
  
  # Show order status distribution
  cat("Order Status Distribution:\n")
  print(table(orders_frame$order_status))
  cat("\n")
  
  # Merge orders with order items enriched with product information
  orders_enriched <- merge(
    orders_frame,
    order_items_enriched,
    by = "order_id",
    all.x = TRUE
  )
  
  # Merge with customer information
  orders_enriched <- merge(
    orders_enriched,
    customer_frame,
    by = "customer_id",
    all.x = TRUE
  )

  #Merge payment data
  # NOTE: payment_frame is merged raw intentionally to demonstrate row inflation.
  # We will address this in the next step by aggregating payment information to one row per order.
  orders_enriched <- merge(
    orders_enriched,
    payment_frame,
    by = "order_id",
    all.x = TRUE
  )
  return(orders_enriched)
}

# Category merging function
# Merges sparse categories into broader groupings
merge_categories <- function(orders_enriched) {
  
  # Show category distribution before merging
  cat("Category Distribution (before merging):\n")
  print(sort(table(orders_enriched$product_category_name_english), decreasing = TRUE))
  cat("\nMedian category count: ", median(table(orders_enriched$product_category_name_english)), "\n\n")
  
  # Merge fashion related categories
  orders_enriched$product_category_name_english[grepl("fashion", orders_enriched$product_category_name_english)] <- "fashion"
  
  # Merge home furniture related categories
  home_furniture_categories <- c("furniture_bedroom", "furniture_mattress_and_upholstery", "kitchen_dining_laundry_garden_furniture", "la_cuisine", "home_comfort_2")
  orders_enriched$product_category_name_english[orders_enriched$product_category_name_english %in% home_furniture_categories] <- "home_furniture"
  
  # Merge construction tool related categories
  construction_tool_categories <- c("construction_tools_safety", "costruction_tools_garden", "costruction_tools_tools", "signaling_and_security", "security_and_services")
  orders_enriched$product_category_name_english[orders_enriched$product_category_name_english %in% construction_tool_categories] <- "construction_tools"
  
  # Merge appliance related categories
  appliance_categories <- c("home_appliances_2", "small_appliances_home_oven_and_coffee")
  orders_enriched$product_category_name_english[orders_enriched$product_category_name_english %in% appliance_categories] <- "appliances"
  
  # Merge industry related categories
  industry_commerce_categories <- c("agro_industry_and_commerce", "industry_commerce_and_business")
  orders_enriched$product_category_name_english[orders_enriched$product_category_name_english %in% industry_commerce_categories] <- "industry_commerce"
  
  return(orders_enriched)
}
