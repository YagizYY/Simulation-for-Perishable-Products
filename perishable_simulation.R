library(data.table)

perishable <- function(days,
                       life_vector,
                       demand_FIFO,
                       demand_LIFO,
                       reorder_point,
                       order_qty) {
  if (all(days == length(demand_FIFO), days == length(demand_LIFO)) == FALSE
  )
  {
    cat(
      "There is a mismatch among days, demand_FIFO & demand_LIFO.
              Make sure that their horizons are equal."
    )
  } else{
    
    my_table <- data.table(
      days = 1:days,
      start_stock = sum(life_vector),
      end_stock = 0,
      demand_FIFO,
      demand_LIFO,
      given_order = 0,
      incoming_order = 0,
      total_sales = 0,
      waste = 0,
      carry_incoming_order = 0
    )
    
    # define total demand by adding FIFO and LIFO demands
    my_table[, total_demand := demand_FIFO + demand_LIFO]
    
    stock_cols <- paste0("beginning_stock", 1:length(life_vector))
    end_cols <- paste0("ending_stock", 1:length(life_vector))
    shifted_end <- paste0("carry_ending_stock", 1:length(life_vector))
    
    my_table[, c(stock_cols) := as.list(life_vector)]
    my_table[, c(shifted_end) := as.list(life_vector)]
    my_table[, c(end_cols) := 0]
    
    for (i in 1:days) {
      # assign incoming_order
      my_table[i, incoming_order := carry_incoming_order]
      
      # assign shifted_end to stock_cols
      my_table[i, c(stock_cols) := mget(shifted_end)]
      
      # add incoming_order to beginning_stock_max
      my_table[i, stock_cols[length(stock_cols)] :=
                 get(stock_cols[length(stock_cols)]) + incoming_order]
      
      # define start_stock
      my_table[i, start_stock := rowSums(.SD), .SDcols = stock_cols]
      
      # calculate ending_stock's
      my_stock <- as.numeric(my_table[i, mget(stock_cols)])
      
      fifo_sum <- cumsum(my_stock[-length(my_stock)])
      
      lifo_sum <- my_stock[-c(1, 2)]
      lifo_sum <- rev(cumsum(rev(lifo_sum)))
      lifo_sum <- c(lifo_sum, 0)
      
      for (j in 1:(length(life_vector) - 1)) {
        my_table[i, end_cols[j] := max(
          0,
          get(stock_cols[j + 1]) -
            max(0, demand_FIFO - fifo_sum[j]) -
            max(0, demand_LIFO - lifo_sum[j])
        )]
      }
      
      # Shift ending_stock_i to shifted_end_i+1
      my_table[, c(shifted_end) := shift(.SD, fill = 0), .SDcols = end_cols]
      
      # Calculate end_stock
      my_table[i, end_stock := rowSums(.SD), .SDcols = end_cols]
      
      # RQ Policy
      my_table[i, given_order := ifelse(end_stock <= reorder_point,
                                        order_qty, 0)]
      
      my_table[, carry_incoming_order := shift(given_order)]
      
      # define waste
      waster <- my_stock[-c(1)]
      waster <- rev(cumsum(rev(waster)))
      
      my_table[i, waste := max(0,
                               beginning_stock1 - demand_FIFO -
                                 max(0,
                                     demand_LIFO - sum(waster)))]
      
      # define total sales
      my_table[i, total_sales := start_stock - end_stock - waste]
      
      # define lost sales
      my_table[i, lost_sales := total_demand - total_sales]
      
    }
    
    
    return(my_table[,
                    c(
                      "start_stock",
                      "demand_FIFO",
                      "demand_LIFO",
                      "total_demand",
                      stock_cols,
                      end_cols,
                      "total_sales",
                      "end_stock",
                      "lost_sales",
                      "given_order",
                      "incoming_order",
                      "waste"
                    ),
                    with = FALSE])
    
  }
}


################################################################################
my_output <- perishable(
  days = 50,
  life_vector = c(35,40,20),
  demand_FIFO = rpois(50, 4),
  demand_LIFO = rpois(50, 7),
  reorder_point = 30,
  order_qty = 20
)

# -> life_vector : c(number of SKUs that have a remaining life-time of 1,
#                    number of SKUs that have a remaining life-time of 2,
#                       .....
#                   )
#####
# days = 500
# life_vector = c(35, 40, 26)
# demand_FIFO = rpois(500 , 7)
# demand_LIFO = rpois(500, 9)
# reorder_point = 3
# order_qty = 10
################################################################################
