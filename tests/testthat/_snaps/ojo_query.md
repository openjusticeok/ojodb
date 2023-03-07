# ojo_query succeeds correctly in non-interactive mode

    Code
      ojo_query("SELECT 'a' as test;", .con = db)
    Output
      # A tibble: 1 x 1
        test 
        <chr>
      1 a    

# ojo_query succeeds with global flag in non-interactive mode

    Code
      ojo_query("SELECT 'a' as test;", .global = TRUE)
    Output
      # A tibble: 1 x 1
        test 
        <chr>
      1 a    

# ojo_query succeeds correctly in interactive mode

    Code
      ojo_query("SELECT 'a' as test;")
    Output
      # A tibble: 1 x 1
        test 
        <chr>
      1 a    

