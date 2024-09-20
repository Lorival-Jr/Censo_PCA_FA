# Sobre os bancos, tem os dados do pib desde 2010 até 2021
# Censo com características referentes à 01/09/2022

###########################################################################

# Trabalhando o banco -----------------------------------------------------


dados_brutos <- read.csv('ibge.csv', encoding = 'UTF-8')[,-1]
dados21 <- dados_brutos[dados_brutos$Ano == '2021',]
 
table(dados21$Ano)
quant <- dados21[dados21$Ano == '2021',
                      
                      c('vlr_agro', 'vlr_ind', 'vlr_ser', 
                         'vlr_pub' , 'vlr_tot', 'vlr_imp',
                         'pib',      'pib_capta', 'area' ,
                         'tot_pop', 'tot_dom', 'tot_dom_par', 'tot_dom_col', 'cres')]

# Tem várias variáveis redundantes, que explicam a mesma coisa
quant2 <-  na.omit(dados21[dados21$Ano == '2021',
                      
                      c('vlr_agro', 'vlr_ind', 'vlr_ser', 
                         'vlr_pub', 'vlr_imp',
                         'pib',      'pib_capta', 'area' ,
                         'tot_pop', 'tot_dom_par', 'tot_dom_col', 'cres')])
quant3 <- quant2
names(quant3) <- c('Valor agro', 'Valor indústria', 'Valor Serviços', 'Valor Serv. Público', 'Valor impostos', 'PIB', 'PIB/capta', 'Área', 'Total População', 'Domicílios particulares', 'Domicílios Coletivos', 'Crescimento do PIB' )

classe <- na.omit(dados21[dados21$Ano == '2021',
                  
                  c('vlr_agro', 'vlr_ind', 'vlr_ser', 
                    'vlr_pub' , 'vlr_tot', 'vlr_imp',
                    'pib',      'pib_capta', 'area' ,
                    'tot_pop', 'tot_dom', 'tot_dom_par', 'tot_dom_col', 'cres', 'Nom_reg')])

# Descritiva --------------------------------------------------------------

library(corrplot)

corrplot(cor(quant3), method = 'color',
         type = 'lower'  ,
         addCoef.col ='black',tl.col="black",
         number.cex = 0.8, diag=FALSE, tl.srt = 45)


# PCA ---------------------------------------------------------------------
library("MVar.pt")


pc <- PCA(data = quant2, type = 1) # executa o PCA
tit = c("Scree-plot","Grafico das observacoes","Circulo de correlacoes")
cls <- as.character(dados21$Nom_reg)

Plot.PCA(pc, titles = tit, xlabel = NA, ylabel = NA, size = 1.3, 
         grid = TRUE, color = TRUE, linlab = NA, axes = TRUE, class = cls, 
         classcolor = NA, posleg = 2, boxleg = F, savptc = FALSE,
         width = 3236, height = 2000, res = 300, casc = F)

print("Variâncias dos Componentes Principais:"); round(pc$mtxAutvlr,2)
print("Correlação dos Componentes Principais:"); round(t(pc$mtxCCP),2)

Plot.PCA(pc, titles = tit, class = cls,)


# Análise Fatorial --------------------------------------------------------

fa <- FA(data = quant2, method = "PC", type = 2, nfactor = 3)
tit <- c("Scree-plot","Scores das observacoes","Cargas Fatoriais","Biplot")

cls <- as.character(classe$Nom_reg)

Plot.FA(FA = fa, titles = tit, xlabel = NA, ylabel = NA,
        color = TRUE, linlab = NA, savptc = FALSE, size = 1.1,
        posleg = 1, boxleg = FALSE, class = cls, axes = TRUE,
        classcolor = NA, width = 3236, height = 2000, res = 300, casc = FALSE)

print("Matriz com todos os resultados associados:"); round(fa$mtxresult,3)
print("Matriz com todos os resultados associados:"); round(fa$mtxvar,3)
# A terceira componente explica apenas pib_capta

