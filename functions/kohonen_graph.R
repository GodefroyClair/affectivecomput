
##create a function to plot a variable on each experience
plot_var_exp_ho <- function(df, var_select) {
  df %>% select_("date", "nom.experience", var_select, "tps.ecoule", "quart.temps")
  function(nom.exp, print = T) { 
    df_exp <- df %>% filter(nom.experience == nom.exp)
    gg <- ggplot(df_exp, aes_string(x = "date", y = var_select)) + 
      geom_path() +
      ggtitle(paste("transpi", nom.exp))
    if(print == T) print(gg)
    else gg
  }
}

