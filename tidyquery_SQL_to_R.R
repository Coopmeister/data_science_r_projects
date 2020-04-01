library(tidyquery)

data()

iris


query(
      
      " select * 
        from iris 
        where Species == 'setosa'
      "
)  

show_dplyr("select * 
            from iris 
            where Species == 'setosa'")





