SELECT MIN(ws_bill_customer_sk)
FROM   web_sales, 
       customer, 
       customer_address,
       catalog_sales,
       warehouse
WHERE  ws_bill_customer_sk = c_customer_sk 
       AND ca_address_sk =  c_current_addr_sk 
       AND c_current_addr_sk = cs_bill_addr_sk
       AND cs_warehouse_sk = w_warehouse_sk
       AND  w_warehouse_sq_ft = ws_quantity
