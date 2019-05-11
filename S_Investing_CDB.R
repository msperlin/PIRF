library(tidyverse)
library(purrr)
library(furrr)

graphics.off()

my.d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.d)

library(tidyverse)
library(purrr)

sapply(list.files('fcts/', full.names = TRUE), source) 

# simple example for CDB
df.0 <- invest_cdb_lca(type.invest = 'CDB', 
                       type.indexing = 'CDI', 
                       percent.index = 1, # 100% of CDI 
                       date.buy =  as.Date('2010-01-01'),
                       date.sell = Sys.Date(),
                       contract.duration = '1 year', 
                       value.first.buy = 1000, 
                       value.monthly.buy = 0)

p <- ggplot(df.0, aes(ref.month, port.net.value)) + 
  geom_line() + 
  labs(title = 'Investimento de R$ 1000 em CDB 100% CDI', 
       x = 'Datas', y = 'Valor Líquido de Resgate')

x11() ; p

# second example for comparing CDBs
df.1 <- invest_cdb_lca(contract.duration = '10 year')
df.2 <- invest_cdb_lca(contract.duration = '3 year')
df.3 <- invest_cdb_lca(contract.duration = '3 months')

df.to.plot <- bind_rows(df.1, df.2, df.3)


p <- ggplot(df.to.plot, #, df.5), 
            aes(x=ref.month, y = port.net.value,
                                       color = contract.duration )) +
  geom_line() + 
  labs(title = 'Comparação do efeito de diferimento do imposto', 
       x = 'Datas', y = 'Valor Líquido de Resgate')

x11(); print(p)
