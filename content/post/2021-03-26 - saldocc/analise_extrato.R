
# pacotes -----------------------------------------------------------------
library(tidyverse)
library(lubridate)

# Wrangle -----------------------------------------------------------------

df <- list.files(pattern = "*.csv") %>%
  map_df(~read_delim(.,
                   delim = ";", 
                   escape_double = FALSE, 
                   col_names = c("data","ocasião","valor","saldo"), 
                   col_types = cols(
                            data = col_date(format = "%d/%m/%Y")), 
                   locale = locale(
                            time_format = "%d/%m/%y", 
                            decimal_mark = ",", 
                            grouping_mark = ".", 
                            encoding = "ISO-8859-1"),
                   trim_ws = TRUE, 
                   skip = 8)) %>%
  separate(ocasião, into = c("trans","detalhe"), sep = "-") %>%
  mutate(valor = parse_number(str_remove(valor,pattern = "-"),
                              locale = locale(grouping_mark = ".")),
         dif = (saldo - lag(saldo)),
         trans = str_to_title(trans)) %>%
  filter(valor != 0, !is.na(valor) == TRUE)

# explorando ------------------------------------------------------------
last_date <- str_c("Última transação: ",last(df$data))
y_label <- seq(2000,5000, by =100)  

plot_saldo <- ggplot(df, aes(data,saldo))+
  geom_line(size = 1)+
   labs(
     title = "Saldo em conta corrente no banco inter",
     subtitle = last_date)+
    scale_x_date(date_breaks = "5 days", date_labels = "%d/%b")+
    scale_y_continuous(breaks =y_label, labels = str_c("R$",y_label))+
  theme_minimal()
    



big_drop <- df %>% group_by(month(data)) %>% filter(row_number(-desc(dif))==1)
  plot_todoper <- ggplot(df, aes(x = data, y = saldo))+
    geom_line(size = 1, 
              shape = 1,
              data = big_drop)+
    ggrepel::geom_label_repel(aes(label = trans),
                              data = big_drop)+
    scale_x_date(date_breaks = "30 days",
                 date_labels = "%d/%b")+
    scale_y_continuous(breaks = y_label,
                       labels = str_c("R$ ", y_label))
  
  
# 2021
y_label <- seq(1000,5000, by =500) 
big_drop21 <- df21 %>% group_by(month(data)) %>% filter(row_number(-desc(dif))==1)
df21 <-filter(df, data >="2021-01-01")
  
plot21 <- ggplot(df21, aes(x = data, y = saldo))+
    geom_line(size = 1, data = big_drop21)+
    ggrepel::geom_label_repel(aes(label = trans),data = big_drop21)+
    scale_x_date(date_breaks = "10 days",date_labels = "%d/%b")+
    scale_y_continuous(breaks = y_label,labels = str_c("R$ ", y_label))
  
#comments
#qual dos plots tem o melhor layout de código?  


  