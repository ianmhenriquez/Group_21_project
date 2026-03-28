source("helper.r") # Ensure helper functions are available

# This script contains functions for merging and preparing the Olist dataset

# Main merging function that combines all data frames
merge_olist_data <- function( orders_frame, translation_frame, order_items_frame, payment_frame, customer_frame, sellers_frame = NULL, product_frame) {

  payments_agg <- aggregate(
    cbind(payment_value, payment_installments) ~ order_id,
    data = payment_frame,
    FUN = function(x) c(total = sum(x), max = max(x))
  )

  payments_agg <- data.frame(
    order_id = payments_agg$order_id,
    payment_value_total = payments_agg$payment_value[, "total"],
    payment_installments_max = payments_agg$payment_installments[, "max"]
  )

  # standardize date formats in orders_frame
  orders_frame$order_delivered_customer_date <- as.Date(
    orders_frame$order_delivered_customer_date, format = "%Y-%m-%d")

  orders_frame$order_estimated_delivery_date <- as.Date(
    orders_frame$order_estimated_delivery_date, format = "%Y-%m-%d")

  orders_frame$order_purchase_timestamp <- as.POSIXct(
    orders_frame$order_purchase_timestamp, format = "%Y-%m-%d %H:%M:%S")

  orders_frame$order_delivered_carrier_date <- as.Date(
    orders_frame$order_delivered_carrier_date, format = "%Y-%m-%d")

  # merge products with category translations
  translated_products <- merge(
    product_frame,
    translation_frame,
    by = "product_category_name",
    all.x = TRUE
  )

  removed_translation_columns <- c("product_category_name", "product_name_lenght", "product_description_lenght", "product_photos_qty")
  translated_products <- remove_column(translated_products, removed_translation_columns)
  # add empty categoires to misc category
  translated_products$product_category_name_english[is.na(translated_products$product_category_name_english)] <- "Miscellaneous"

  # merge translated product into order_items
  order_items_enriched <- merge(
    order_items_frame,
    translated_products,
    by = "product_id",
    all.x = TRUE
  )

  #merge order_items_enriched product with seller info
  order_items_enriched <- merge(
    order_items_enriched,
    sellers_frame,
    by = "seller_id",
    all.x = TRUE
  )

  # merge aggregated payment with orders
  orders_enriched <- merge(
    orders_frame,
    payments_agg,
    by = "order_id",
    all.x = TRUE
  )

  #merge customer info into order items + payments
  orders_enriched <- merge(
    orders_enriched,
    customer_frame,
    by = "customer_id",
    all.x = TRUE
  )

  # create new features for delivery performance analysis
  orders_enriched <- orders_enriched[
  !is.na(orders_enriched$order_delivered_customer_date) & 
  !is.na(orders_enriched$order_estimated_delivery_date), ]

  orders_enriched$is_late <- as.factor(ifelse(
    orders_enriched$order_delivered_customer_date > 
    orders_enriched$order_estimated_delivery_date, "Late", "On_Time"))

  orders_enriched$delivery_delay_days <- as.numeric(difftime(
    orders_enriched$order_delivered_customer_date,
    orders_enriched$order_estimated_delivery_date,
    units = "days"))

  # merge enriched orders with enriched order items since orders are the main unit of analysis for our project
  order_items_enriched <- merge(
    order_items_enriched,
    orders_enriched,
    by = "order_id",
    all.x = TRUE
  )

  return(order_items_enriched)
}
