library(tidyverse)
library(purrr)
library(furrr)

graphics.off()

my.d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.d)

library(tidyverse)
library(purrr)

sapply(list.files('fcts/', full.names = TRUE), source) 

# simple example for TD
my.TD <- 'Tesouro IPCA+ 2024'
df.TD <- invest_TD(TD.to.invest = my.TD,
                       date.buy =  as.Date('2004-01-01'),
                       date.sell = Sys.Date(),
                       value.first.buy = 10000, 
                       value.monthly.buy = 1000)

p <- ggplot(df.TD, aes(ref.month, port.net.value)) + 
  geom_line() + 
  labs(title = paste0('Investimento de R$ 1000 em ', my.TD), 
       x = 'Datas', y = 'Valor Líquido de Resgate')

x11() ; p
