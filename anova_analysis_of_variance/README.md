---
title: "PAC4 - Anàlisi de variànçia i repàs del curs"
author: "Albert Costas Gutierrez"
date: '18 de junio 2018'
output:
  html_document:
    df_print: kable
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_depth: 2
  word_document:
    toc: yes
    toc_depth: '2'
  pdf_document:
    toc: yes
editor_options:
  chunk_output_type: inline
---





****
# Preprocessat
****

## Carrega de les dades

**Carregar el fitxer de dades “Fumadores.csv”.**


```r
# Carregar el fitxer de dades en R
mydata <- read.csv2(file = "Fumadores.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, encoding="UTF-8")
n.var <- names(mydata)
# breu resumen dels atributs
head(mydata, n=5)
```

<div class="kable-table">

     AE  Tipo 
-------  -----
 4.0100  NF   
 4.8772  NF   
 4.2927  NF   
 4.9475  NF   
 4.1266  NF   

</div>

## Tipus de dades

**Consultar els tipus de dades de les variables i si és necessari, aplicar les conversions adequades. Esbrinar possibles inconsistències en els valors de Tipus o AE. En el cas que existan inconsistències, corregir-les.**


```r
summary(mydata)
```

```
##        AE             Tipo   
##  Min.   :1.038   FI     :20  
##  1st Qu.:2.519   FM     :20  
##  Median :2.954   FP     :20  
##  Mean   :3.074   NF     :19  
##  3rd Qu.:3.627   FL     :16  
##  Max.   :5.284   NI     :16  
##                  (Other): 8
```

```r
res <- sapply(mydata,class)

kable(data.frame(
    variables = names(res),
    clase = as.vector(res)
))
```



variables   clase   
----------  --------
AE          numeric 
Tipo        factor  

```r
summary(mydata$Tipo)
```

```
## FI fL FL FM FP NF ni NI 
## 20  3 16 20 20 19  5 16
```


```r
mydata <- as.data.frame(sapply(mydata, toupper))
summary(mydata$Tipo)
```

```
## FI FL FM FP NF NI 
## 20 19 20 20 19 21
```


```r
mydata$AE <- as.numeric(as.character(mydata$AE))
summary(mydata)
```

```
##        AE        Tipo   
##  Min.   :1.038   FI:20  
##  1st Qu.:2.519   FL:19  
##  Median :2.954   FM:20  
##  Mean   :3.074   FP:20  
##  3rd Qu.:3.627   NF:19  
##  Max.   :5.284   NI:21
```

## Realitzar un anàlisi descriptiu de la mostra en la seva totalitat en relació a la variable AE


```r
mean(mydata$AE)
```

```
## [1] 3.07446
```

```r
sd(mydata$AE)
```

```
## [1] 0.8704699
```

```r
barplot(mydata$AE, col=heat.colors(length(unique(mydata$AE))))
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Analitzar les dades segons el tipus de fumador

**Mostrar el número de persones en cada tipus, la mitjana d’AE cada grup i un boxplot on es mostra la distribució d’AE per cada tipus, etc. Es recomana ordenar els gràfics de menys a més AE. Per calcular la mitjana o altres variables per cada tipus de fumador, podeu usar les funcions summarize y group_by de la llibreria dplyr que us seràn de gran utilitat. Per realitzar la visualització de les dades, podeu usar la funció ggplot de la llibreria ggplot2.**


```r
by_Tipo <- mydata %>%
  group_by(Tipo) %>%
  summarise(mean = mean(AE), sd = sd(AE))

by_Tipo
```

<div class="kable-table">

Tipo        mean          sd
-----  ---------  ----------
FI      2.456500   0.5515356
FL      2.773679   0.7182298
FM      2.687775   0.6575152
FP      3.118690   0.7497423
NF      4.240605   0.8100596
NI      3.206190   0.5025607

</div>

```r
by_Tipo.m <- melt(mydata, id.var = "Tipo")

p <- ggplot(data = by_Tipo.m, aes(x = reorder(by_Tipo.m$Tipo, value, median), y=value)) + geom_boxplot(aes(fill=Tipo))
p + facet_wrap( ~ variable, scales="free")
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
ggplot(data = mydata, aes(x = AE, y = Tipo, colour = Tipo, group = Tipo)) + 
    stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, 
    geom = "line") + labs(y = "mean (result)") + theme_bw()
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

# Interval de confiança

**Calcular l’interval de confiança de la capacitat pulmonar de tota la mostra. El nivell de confiança és 95%. Realitzar el càlcul manualment sense usar les funcions t.test o equivalents. Podeu usar qnorm, qt, pnorm, pt, . . . En quant a l’elecció del mètode pel càlcul de l’interval de confiança, s’ha de justificar la vostra elecció.**

$\overline{x} \pm Z_{\propto / 2} * \frac{\delta}{\sqrt{n}}$

Seleciona la mitjana i la $\pm$ per obtenir el marge d'error, utlitzem les funciones anteriors per calcular la mitjana, el valor critic i l'estadictic de contrast:


```r
mitajana_function <- function(dataList) {
  iterator <- 0
  sum <- 0
  for (item in dataList) {
    sum <- sum + item
    iterator <- iterator + 1
  }

  return (sum / iterator)
}

## funció per calcular la desviació tipica
desviacion_tipica_function <- function(dataset, mitjana_mostral, n)
{
  #calculem la variància i posteriorment la desviació típica
  vtemp = 0
  for(v in dataset) {
    vtemp = vtemp + ((v - mitjana_mostral)^2)
  }
  variancia = vtemp/n
  
  #calculem la desviació típica
  return  = sqrt(variancia)
}

#funcio per calcular el valor critic
valor_critic_funtion <- function(percentatge, graus) {
  return (qt(percentatge, graus))
}

## funció per calcular l'interval de confiança
intertal_confiança_funtion <- function(dataset, percentage_conf, mitjana_poblacional) {
  
    #obtenim el nombre de mostres
    n <- length(dataset)

   mitjana_mostral = mitajana_function(dataset)
  
    valCritic <- valor_critic_funtion(
         percentage_conf/100,
        (n - 1)
    )
    
    
    #primer calculem la variància i posteriorment la desviació típica
  o = desviacion_tipica_function(dataset, mitjana_mostral, n)
    
  errorStd <- (o/ (sqrt(length(dataset)) - 1))
    
    return (
    list(
       (mitjana_mostral - (valCritic * errorStd)),
       (mitjana_mostral + (valCritic * errorStd))
     )
    )
}


list[interval_de_conf_inf, interval_de_conf_sup] <- intertal_confiança_funtion(
    mydata$AE,
    95,
    5
)

c(interval_de_conf_inf, interval_de_conf_sup)
```

```
## [1] 2.929431 3.219489
```
He utilitzat función per reutilitzar el codi, les funcions calculen la funció $\overline{x} \pm Z_{\propto / 2} * \frac{\delta}{\sqrt{n}}$ podem veure que el rang de confiça és:
2.9294308, 3.2194885
 

****
# Comparació de dues mostres: Fumadors vs No Fumadors
****

**Podem afirmar que la capacitat pulmonar del fumadors és inferior a la de no fumadors? Inclou dintre de la categoria de no fumadors els fumadors pasius. Realitzar el càlcul manualment sense usar les funcions t.test o equivalents. Podeu usar qnorm, qt, pnorm, pt, . . . Seguir els passos que s’indican a continuació.**

## Escriure la hipótesi nul·la i alternativa

#### Hipòtesi nul·la
$H_0: \mu_1 - \mu_2 = 0$

#### Hipòtesi alternativa
$H_1: \mu_1 - \mu_2 < 0$

Assignem a $\mu_1$ a la capacitat pulmonar dels no fumadors és igual a la dels fumadors a $\mu_2$ a la capacitat dels no fumadors és superior a la dels fumadors.

En aquest cas, la nostra hipòtesi nul·la és que no hi ha variació entre la capacitat pulmonar dels furmadors i no fumadors, pel que fa la hipòtesi alternativa ens diu que la capitat pulmonar dels furmadors és superior a la capacitat dels no fumadors.

## Preparar les dades per realitzar el contrast


```r
noFumadors <- mydata[(mydata$Tipo == "NF") | (mydata$Tipo == "FP"),]
head(noFumadors, n=5)
```

<div class="kable-table">

     AE  Tipo 
-------  -----
 4.0100  NF   
 4.8772  NF   
 4.2927  NF   
 4.9475  NF   
 4.1266  NF   

</div>

```r
length(noFumadors$AE)
```

```
## [1] 39
```

```r
fumadors <- mydata[!(mydata$Tipo == "NF") & !(mydata$Tipo == "FP"),]
head(fumadors, n=5)
```

<div class="kable-table">

          AE  Tipo 
---  -------  -----
40    2.9084  NI   
41    2.5423  NI   
42    3.8249  NI   
43    3.7012  NI   
44    2.7578  NI   

</div>

```r
length(fumadors$AE)
```

```
## [1] 80
```


```r
barplot(noFumadors$AE, col=heat.colors(length(unique(noFumadors$AE))))
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
barplot(fumadors$AE, col=heat.colors(length(unique(fumadors$AE))))
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## Especificar quin tipus de contrast aplicareu

Es tracta de mosters independets perque son persones diferentes les que estan al grup de fumadors y els de no fumadors, sera contrat de hipótesis en mostres indepents. D'altra banda les dos mostres son del mateix camp, però estan disciminades pel camp de tipus,... llavors la variància, tot i ser deconeguda, es la igual, en aquest sentit aplicarem constras sobre la diferència de mitjanes per variàncies poblacionals desconegues però iguals.

## Realitzar els càlculs del valor p

$T=\frac{\begin{pmatrix}\textstyle\overline{X_1}-\overline X_2\end{pmatrix}-\left(\mu_1-\mu_2\right)}{SE}\;=\;t\left(df\right)$

$SE=\sqrt{\frac{\displaystyle\widehat{S_1^2}}{n_1}+\frac{\widehat{S_2^2}}{n_2}}$

El nivell de significació o alta es $\alpha$ = 0.05, ja que el nivell de confiança és 95%


```r
variancia_function <- function(dataList) {
  dataMean <- mean(dataList)
  totalItems <- length(dataList)
  suma <- 0
  for(item in dataList) {
    suma <- suma + ((item - dataMean)^2)
  }
  
  return (suma / (totalItems - 1))
}

calcul_estadictic_mitjana <- function(dataList1, dataList2, mu1, mu2) {
  totalMostres1 <- length(dataList1)
  totalMostres2 <- length(dataList2)

  mean1 <- mitajana_function(dataList1)
  mean2 <- mitajana_function(dataList2)

    return (
    (mean1 - mean2) / (SE_function(dataList1, dataList2, mean1, mean2))
  )
}

SE_function <- function(dataList1, dataList2,mean1,mean2) {
  
  totalMostres1 <- length(dataList1)
  totalMostres2 <- length(dataList2)
  
  sd1 <- variancia_function(dataList1)
  sd2 <- variancia_function(dataList2)
  
  return (sqrt((sd1^2 / totalMostres1) + (sd2^2 / totalMostres2)))
}

tValue2mean <- calcul_estadictic_mitjana(
  fumadors$AE,
  noFumadors$AE,
  mitajana_function(fumadors$AE),
  mitajana_function(noFumadors$AE)
)

tValue2mean
```

```
## [1] -5.694068
```

### El valor critic: n+m-2



```r
grados_de_libertat = ((length(fumadors$AE)+length(noFumadors$AE))-2)
v_critic_2 = valor_critic_funtion(
  0.95,
  grados_de_libertat
  )

v_critic_2
```

```
## [1] 1.657982
```

$pvalue\;=\;P(\vert T_{\;calculada}\vert\;>=t_{df},_{1-\alpha/2})$



```r
p_valor_2_function <- function(tValue, itemsList1, itemsList2) {
  df1 = length(itemsList1) - 1
  df2 = length(itemsList2) - 1
  return (pt(-abs(tValue), df1) + (1 - pt(abs(tValue), df2)))
}

p_value_2 = p_valor_2_function(
  tValue2mean,
  fumadors$AE,
  noFumadors$AE
)
p_value_2
```

```
## [1] 8.499471e-07
```
El valor p és 8.4994709\times 10^{-7}

## Interpretar el resultat del contrast

Rebutjar Hipòstasi nul·la $H_o\;$ perque el p-value $\leq \alpha$ en aquest cas $\alpha$ = 0.05. Donat aquest resultat descartem la hipòtesi nul·la i per tant podem dir a la capacitat dels no fumadors és superior a la dels fumadors.

El p-valor ens proporciona el grau de credibilitat de la hipòtesi nul·la: com el valor de p és "molt petit" (inferior a 0,005), significa que la hipòtesi nul·la és del tot increïble (sobre la base de les observacions obtingudes), i per tant la descartem.

****
# ANOVA
****

**A continuació es realitzarà una anàlisi de variància, on es vol comparar la capacitat pulmonar entre els sis tipus de fumadors no fumadors classificats prèviament. L’anàlisi de variància consisteix a avaluar si la variabilitat d’una variable dependent pot explicar-se a partir d’una o diverses variables independents, denominades factors. En el cas que ens ocupa, ens interessa avaluar si la variabilitat de la variable AE pot explicar-se pel factor tipus de fumador. Hi ha dues preguntes bàsiques a respondre:**

• Hi ha diferències entre la capacitat pulmonar (AE) entre els diferents tipus de fumadors/no fumadors?

• Si hi ha diferències, entre quins grups estan aquestes diferències? 

**Per a la resolució d’aquesta secció, se seguiran els apunts de López-Roldán i Fachelli (2015).**

## Verificar l’assumpció de normalitat

**Atenent al material, gràfic III.8.6 i pàgina 25, avaluar si el conjunt de dades compleix les condicions d’aplicació d’ANOVA. Seguiu els passos que s’indiquen a continuació.**

### Representar gràficament la normalitat de la mostra de dades AE amb la funció qqnorm
Dibuixem la línia de la normal

```r
qqnorm(mydata$AE)
qqline(mydata$AE, col = 3)
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

### Escriure la hipòtesi nul·la i la hipòtesi alternativa

$H_0:$ La distribució de dades segueix la normal

$H_1:$ La distribució de dades no segueix la normal

### Aplicar un test de normalitat

**Aplicar un test de normalitat, seguint les recomanacions del material esmentat López-Roldán i Fachelli (2015). Justificar l’elecció.**


```r
test.shapiro <- shapiro.test(mydata$AE)
test.shapiro
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  mydata$AE
## W = 0.97845, p-value = 0.05293
```

```r
plot(density(mydata$AE))
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
### Interpretar els resultats a partir del gràfic qqnorm i dels valors que retorna el test.

En aquest cas el valor p-value (0.0529277) és més gran que 0.05 per tant podem acceptar la hipòtesi nul·la, afirmen que es tracta una distribució de dades normal.

## Homoscedasticitat: Homogeneïtat de variàncies

**Una altra de les condicions d’aplicació d’ANOVA és la igualtat de variàncies (homoscedasticitat). Aplicar un test per validar si els grups presenten igual variància. Seguiu les indicacions dels apunts de López-Roldán i Fachelli (2015).**

### Escriure la hipòtesi nul·la i la hipòtesi alternativa

#### hipotesi nul·la
$H_0: \sigma_0 =  \sigma_1$

#### hipotesi alternativa
$H_1: \sigma_0 \neq  \sigma_1$

### Realitzar els càlculs


```r
test.bart <- bartlett.test(mydata$AE~mydata$Tipo)
test.bart
```

```
## 
## 	Bartlett test of homogeneity of variances
## 
## data:  mydata$AE by mydata$Tipo
## Bartlett's K-squared = 6.0904, df = 5, p-value = 0.2975
```

```r
hovPlot(AE~Tipo, data=mydata)
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

Podem veure que el valor p-value (0.2975217) és major que 0.05, per tant acceptem la hipòtesis nul·la, acceptem que no existeixen diferències entre les variàncies dels dos atributs.

Per altra banda, podem veure la desviació estàndard dels grups envers la capacitat pulmonar, podem veure que no es massa diferent.



```r
tapply(mydata$AE, mydata$Tipo, sd)
```

```
##        FI        FL        FM        FP        NF        NI 
## 0.5515356 0.7182298 0.6575152 0.7497423 0.8100596 0.5025607
```
## ANOVA unifactorial (One Way ANOVA)

**Calcular ANOVA d’un factor (one-way ANOVA o independent samples ANOVA) per investigar si hi ha diferències en el nivell d’aire expulsat (AE) entre els diferents tipus de fumadors.**

### Escriure la hipòtesi nul·la i la hipòtesi alternativa

##### Hipotesis nul·la
$H_0:\mu_0 = \mu_1$

##### Hipotesis alternativa
$H_1:\mu_0 \neq \mu_1$

### Realitzar els càlculs

**Podeu utilitzar la funció aov.**


```r
anova_4 <- aov(AE~Tipo, data=mydata)
summary(anova_4)
```

```
##              Df Sum Sq Mean Sq F value   Pr(>F)    
## Tipo          5  38.59   7.718   17.16 1.33e-12 ***
## Residuals   113  50.82   0.450                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Interpretar els resultats de la prova ANOVA i relacionar-los amb el resultat gràfic del boxplot

Podem veure que el valor p-value 1.3324566\times 10^{-12} és menor que 0,05, rebutgem la hipòtesi nul·la, afirmem que les mitjanes són diferents, per tant com a conclució la variable AE és pot explicar a partir de la variable independent de tipus de fumador.

Per altra banda observem que la mitjana dels grups envers la AE (capacitat pulmonar) són molt diferents entre elles.



```r
tapply(mydata$AE, mydata$Tipo, mean)
```

```
##       FI       FL       FM       FP       NF       NI 
## 2.456500 2.773679 2.687775 3.118690 4.240605 3.206190
```

## Càlculs ANOVA

### Identificar les variables

**Identificar les variables SST (Total Sum of Squares), SSW (Within Sum of Squares), SSB (Between Sum of Squares) i els graus de llibertat. A partir d’aquests valors, calcular manualment el valor F, el valor crític (a un nivell de confiança del 95%), i el valor p. Interpretar els resultats.**

Utilitzarem la seguent formula per trobar el valor F:

$F = MSB/MSW$

Les funciones que s'utilitezen per al càlculs són les seguents:

#### Mean Square between "factor" 
$MSB = SST / df_{between}$ 

#### Mean Square residual "Within"
$MSW = SSW / df_{residual}$ 

Realitzem els valors df, sst i ssw dels calculs anteriors:

```r
df.between <- summary(anova_4)[[1]][["Df"]][1]
df.residual <- summary(anova_4)[[1]][["Df"]][2]
sst <- summary(anova_4)[[1]][["Sum Sq"]][1]
ssw <- summary(anova_4)[[1]][["Sum Sq"]][2]
```

##### MSB

```r
msb <- sst / df.between
msb
```

```
## [1] 7.71769
```

#####  MSW

```r
msw <- ssw / df.residual
msw
```

```
## [1] 0.4497544
```

#####  F

```r
f_estadistic <- msb/msw
f_estadistic
```

```
## [1] 17.15979
```

Observem que el valor de F és 17.1597863. Per realitzar la comprovació la funció de anova també hi ha el mateix  resultat 17.1597863

### Calcular manualment les variables

**Calcular manualment SSB, SSW i SST a partir de les dades de la mostra. Comproveu que el càlcul coincideix amb els valors que retorna la funció ANOVA. Les funcions summarize i group_by us poden ser útils per a simplificar els càlculs.**

#### SSB

Com abans primer observem les formules per calcular el valor SSB:

$SSB = \sum_{i} n_i (\bar Y_i - \bar Y ...)^2$

Formula SSW:

$SSW = \sum_{i} \sum_{j} (\bar Y_{ij} - \bar Y)^2$

Formula SST:

$SST = SSB + SSW$

Realitzem els calculs SSB:
* Valors FI

```r
valors.FI <- nrow(mydata[mydata$Tipo == "FI", ])
```

* Valors FL

```r
valors.FL <- nrow(mydata[mydata$Tipo == "FL", ])
```

* Valors FM

```r
valors.FM <- nrow(mydata[mydata$Tipo == "FM", ])
```

* Valors FP

```r
valors.FP <- nrow(mydata[mydata$Tipo == "FP", ])
```

* Valors NF

```r
valors.NF <- nrow(mydata[mydata$Tipo == "NF", ])
```

* Valors NI

```r
valors.NI <- nrow(mydata[mydata$Tipo == "NI", ])
```

* Mitjanes

```r
mean.FI <- mean(mydata[mydata$Tipo == "FI", "AE"])
mean.FL <- mean(mydata[mydata$Tipo == "FL", "AE"])
mean.FM <- mean(mydata[mydata$Tipo == "FM", "AE"])
mean.FP <- mean(mydata[mydata$Tipo == "FP", "AE"])
mean.NF <- mean(mydata[mydata$Tipo == "NF", "AE"])
mean.NI <- mean(mydata[mydata$Tipo == "NI", "AE"])
```

* Sumes

```r
overallMean <- mean(mydata$AE)
ssb_calculat <- sum(valors.FI * (mean.FI - overallMean)^2 +
                    valors.FL * (mean.FL - overallMean)^2 +
                    valors.FM * (mean.FM - overallMean)^2 +
                    valors.FP * (mean.FP - overallMean)^2 +
                    valors.NF * (mean.NF - overallMean)^2 +
                    valors.NI * (mean.NI - overallMean)^2)
ssb_calculat
```

```
## [1] 38.58845
```
El SSB és  38.5884505, comparat amb la funció de annova per veure si es correcte: 38.5884505, podem veure que esta correcte.

#### SSW


```r
ssw_calculat <- sum(c((mydata[mydata$Tipo == "FI", "AE"] - mean.FI)^2,
                      (mydata[mydata$Tipo == "FL", "AE"] - mean.FL)^2,
                      (mydata[mydata$Tipo == "FM", "AE"] - mean.FM)^2,
                      (mydata[mydata$Tipo == "FP", "AE"] - mean.FP)^2,
                      (mydata[mydata$Tipo == "NF", "AE"] - mean.NF)^2,
                      (mydata[mydata$Tipo == "NI", "AE"] - mean.NI)^2))
ssw_calculat
```

```
## [1] 50.82225
```

El valor de SSW es 50.822252 el qual el comparem amb el de la funció de anova  50.822252

#### SST


```r
sst_calculat <- ssb_calculat + ssw_calculat
sst_calculat
```

```
## [1] 89.4107
```
En aquest cas el valor de SST és 89.4107024 comprovem que es correctea mb la funció de anova 89.4107024

## Calcular la força de la relació i interpretar el resultat

Per calcular la força de la relació SSB/SST:

```r
eta_quadrat_calculat <- ssw_calculat/sst_calculat
eta_quadrat_calculat
```

```
## [1] 0.5684135
```

Podem veure als apunta de López-Roldán y Fachelli (pàg. 37), podem calcular la força de relació ho podem calcular dividint la variabilitat explicada entre la variabilitat total, la qual ens defeneixla part explicada per la variable independent (0<= η2η2 <= 1).

Per tant la nostra variable independent  del tipus de fumador sòls pot explicar el `reta_quadrat_calculat` de la desigualtat de la variable explicada (AE)

L’estadístic F dóna un valor elevat F=17.1597863. Donat que no és compleix p<0.05, no podem rebutjar la hipòtesi nul.la per tant la capacitat pulmonar si hi ha diferències entre la capacitat pulmonar (AE) entre els diferents tipus de fumadors/no fumadors, almenys un dels tipos és diferent a la resta. El boxplot mostra visualment diferències entre els tipus, tant en els valors centrals de AE per tipus com en la dispersió. La següent gràfica mostra la distribució de AE en les diferents regions de manera anàloga al boxplot que s’ha mostrat anteriorment.


```r
ggline(mydata, x = "Tipo", y = "AE",
add = c("mean_se", "jitter"),
ylab = "AE", xlab = "Tipo")
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

****
# Comparacions múltiples
****

**Independentment del resultat obtingut en l’apartat anterior, vam realitzar un test de comparació múltiple entre els grups. Aquest test s’aplica quan el test ANOVA retorna rebutjar la hipòtesi nul·la d’igualtat de mitjanes. Per tant, procedirem com si el test ANOVA hagués donat com a resultat el rebuig de la hipòtesi nul·la.**

## Sense correcció

**Calcular les comparacions entre grups sense cap tipus de correcció. Podeu utilitzar la funció pairwise.t.test.**

Apliqueu la prova amb dues cues. Identificar els parells que són significativament diferents amb un valor alfa=0.05.


```r
pw.noadjust <- pairwise.t.test(mydata$AE, mydata$Tipo, p.adjust.method ="none", alternative="two.side")
pw.noadjust
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  mydata$AE and mydata$Tipo 
## 
##    FI      FL      FM      FP      NF     
## FL 0.14264 -       -       -       -      
## FM 0.27780 0.69003 -       -       -      
## FP 0.00228 0.11110 0.04451 -       -      
## NF 2.5e-13 6.9e-10 6.2e-11 8.2e-07 -      
## NI 0.00051 0.04400 0.01484 0.67704 3.6e-06
## 
## P value adjustment method: none
```

## Correcció de Bonferroni

**Aplicar la correcció de Bonferroni en la comparació múltiple. Interpretar el resultat i contrastar el resultat amb l’obtingut sense correcció.**

L’ajust de Bonferroni, que consisteix en dividir l’error de tipus I (α) pel nombre
de comparacions. Així $\alpha^* = \frac{\alpha}{ k \choose n}$ 

Cada comparació entre un parell de mostres es fa amb el valor $α^∗$. El test de Bonferroni és excessivament conservador i pot tenir un elevat error de tipus II, és a dir, no detectar diferències quan aquestes hi són.


```r
pw.bonf <- pairwise.t.test(mydata$AE, mydata$Tipo, p.adjust.method ="bonferroni", alternative="two.side")
pw.bonf
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  mydata$AE and mydata$Tipo 
## 
##    FI      FL      FM      FP      NF     
## FL 1.0000  -       -       -       -      
## FM 1.0000  1.0000  -       -       -      
## FP 0.0342  1.0000  0.6676  -       -      
## NF 3.7e-12 1.0e-08 9.3e-10 1.2e-05 -      
## NI 0.0077  0.6600  0.2226  1.0000  5.5e-05
## 
## P value adjustment method: bonferroni
```

La correció de Bonferroni, els valors p són majors amb la relació amb les comparacions dels mateixos valors sense correció, podem veure per exemple que els valos FI i FL sense correció és 
5.1140026\times 10^{-4} i 0.0439987 i els mateixos valors p amb correció 0.007671 i 0.65998 aixo fa que el test sigui més consevador, per tant més complicat de rebuthar la hipòtesi nul·la.

## Prova de Scheffé

**La prova de Scheffé és una de les més usades quan es compleixen condicions d’igualtat de variància. Segons López, està basada en la distribució F. Permet la comparació binària entre parells de mitges i també la comparació múltiple. En les comparacions de parells és conservadora. Es pot aplicar amb mostres de mida desigual i és força robusta davant l’incompliment de la condició d’homoscedasticitat.**
**Apliqueu la prova de Scheffé i interpretar el resultat.**


En general, el mètode de Scheffe explica la taxa d'error de la família mitjançant la ponderació de l'estadística de prova (CC) mitjançant l'error quadrat mitjà (MSEMSE), les mostres entre DF (k-1,k-1) i les mides de grup (si són desiguals):

$C = \sqrt{(k - 1)~F~MSE~(\frac{1}{n_i} + \frac{1}{n_j})}$


```r
pw.scheffe <- ScheffeTest(x=anova_4, which="Tipo", conf.level = 0.95)
pw.scheffe
```

```
## 
##   Posthoc multiple comparisons of means : Scheffe Test 
##     95% family-wise confidence level
## 
## $Tipo
##              diff      lwr.ci     upr.ci    pval    
## FL-FI  0.31717895 -0.41054995  1.0449078 0.82268    
## FM-FI  0.23127500 -0.48706345  0.9496134 0.94500    
## FP-FI  0.66219000 -0.05614845  1.3805284 0.09157 .  
## NF-FI  1.78410526  1.05637637  2.5118342 1.6e-10 ***
## NI-FI  0.74969048  0.03995519  1.4594258 0.03108 *  
## FM-FL -0.08590395 -0.81363284  0.6418249 0.99947    
## FP-FL  0.34501105 -0.38271784  1.0727399 0.76389    
## NF-FL  1.46692632  0.72992662  2.2039260 2.7e-07 ***
## NI-FL  0.43251153 -0.28672653  1.1517496 0.53111    
## FP-FM  0.43091500 -0.28742345  1.1492534 0.53392    
## NF-FM  1.55283026  0.82510137  2.2805592 2.9e-08 ***
## NI-FM  0.51841548 -0.19131981  1.2281508 0.30238    
## NF-FP  1.12191526  0.39418637  1.8496442 0.00016 ***
## NI-FP  0.08750048 -0.62223481  0.7972358 0.99935    
## NI-NF -1.03441479 -1.75365285 -0.3151767 0.00057 ***
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

La taula dels resultats proporciona la comparació de les mitjanes, per a tota combinació possible de grups. La comparació es fa amnb el valor "Crític de F", i els valors p es generen en funció d'això.

Hem de mirar si el p-value es inferior a 0.05 per indicar si la mitjana dels dos grups comparants és diferents, en aquest cas NF-FI, NF-FL, NI-FI, NF-FL, NF-FM, NF-FP i NI-NF té un valor p de inferiora  0.05, que diu que la mitjana dels grups són significativament diferents.

Veiem que exactament 6 contrastos són significatius mentre que 9 no ho són.

****
# ANOVA multifactorial
****

**En una segona fase de la investigació es va avaluar l’efecte del sexe com a variable independent. Amb aquest objectiu, es va recol·lectar un segon conjunt de dades amb les variables independents sexe i nivell de fumador i amb la variable dependent capacitat pulmonar mida segons l’aire expulsat, igual que amb el primer conjunt de dades. Aquest conjunt de dades es troba en el fitxer Fumadores2.csv**

## Estudiar els efectes principals i possibles interaccions

**Examinar les característiques del conjunt de dades i realitzar un estudi visual de les dades. A continuació, aplicar ANOVA. Seguir els passos que s’indiquen a continuació.**

### Anàlisi visual

**Es realitzarà un primer estudi visual per determinar si només existeix efectes principals o hi ha efectes d’interacció entre sexe i tipus de fumador. Per a això, seguir els passos que s’indiquen a continuació:**

**1. Llegir el conjunt de dades**


```r
mydata2 <- read.csv2(file = "Fumadores2.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, encoding="UTF-8")
names(mydata2)
```

```
## [1] "AE"   "Tipo" "Sex"
```

```r
# breu resumen dels atributs
head(mydata2, n=5)
```

<div class="kable-table">

       AE  Tipo   Sex 
---------  -----  ----
 3.607107  NF     M   
 3.708264  NF     M   
 3.824298  NF     M   
 3.355295  NF     M   
 3.921486  NF     M   

</div>

**2. Agrupar el conjunt de dades per tipus de fumador i sexe i calcular la mitjana d’AE en cada grup. Podeu fer servir les instruccions group_by i summarise de la llibreria dplyr per realitzar aquest procés. Mostra el conjunt de dades en forma de taula, on es mostri la mitjana de cada grup segons el sexe i tipus de fumador.**


```r
by_Tipo_vrs_Sex <- mydata2 %>%
  group_by(Tipo, Sex ) %>%
  summarise(mean = mean(AE), sd = sd(AE))

by_Tipo_vrs_Sex
```

<div class="kable-table">

Tipo   Sex        mean          sd
-----  ----  ---------  ----------
FI     F      2.153394   0.8045994
FI     M      2.486166   0.8875409
FL     F      2.743544   0.7966098
FL     M      3.037663   0.7124353
FM     F      2.282694   0.8058870
FM     M      2.574741   0.7630455
FP     F      3.301332   0.3866788
FP     M      3.406332   0.4123249
NF     F      3.641762   0.3967712
NF     M      3.748391   0.5643445
NI     F      3.163780   0.9818005
NI     M      3.292063   0.9255948

</div>

**3. Mostra en un plot d’AE per a cada tipus de fumador i sexe. Podeu inspirar-en els gràfics de López-Roldán i Fachelli (2015), pàg.38. Podeu realitzar aquest tipus de gràfic utilitzant la funció ggplot de la llibreria ggplot2.**


```r
ggplot(data = by_Tipo_vrs_Sex, 
       aes(x = mean, y = Tipo, colour = Tipo, group = Tipo)
    ) +
    stat_summary(fun.y = mean, geom = "point") + 
    stat_summary(fun.y = mean, geom = "line") + 
    theme_bw()
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

```r
ggplot(data = mydata2, aes(x = AE, y = Tipo, colour = Tipo, group = Tipo)) + 
    stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, 
    geom = "line") + labs(y = "mean (result)") + theme_bw()
```

![](A4_AnalisiEstadisticaAvançada_2018_acostas_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

**4. Interpretar el resultat sobre si hi ha només efectes principals o hi ha interacció. Si hi ha interacció, explicar com s’observa i quins efectes produeix aquesta interacció.**

Interpretem els resultats observant que hi ha relació directa entre el sexe dels fumadors que són dels tipus: FI, FN i FL i la capacitat pulmonar envers el sexe masculí.

Per contra els tipus de NI, NF i FP de fumadors no hi ha diferència significativa.

### ANOVA multifactorial

**Calcular ANOVA multifactorial per avaluar si la variable dependent AE es pot explicar a partir de les variables independents sexe i tipus de fumador. Incloeu l’efecte de la interacció només si s’ha observat aquesta interacció en l’anàlisi visual de l’apartat anterior. Interpreteu el resultat.**

Utilitzarem l'anàlisi d'abans amb l'Anova amb 2 variables explicatives.

#### Hipotesi nul·la
$H_0:\mu_0 = \mu_1$

#### Hipotesi alternativa
$H_1:\mu_0 \neq \mu_1$

Fem servir la funció AOV per concloure si hi ha diferencies entre les mitjanes.


```r
anova_2_variables <- aov(AE ~ Tipo+Sex, mydata2)
summary(anova_2_variables)
```

```
##              Df Sum Sq Mean Sq F value  Pr(>F)    
## Tipo          5 100.05  20.009  37.456 < 2e-16 ***
## Sex           1   3.92   3.922   7.343 0.00697 ** 
## Residuals   495 264.43   0.534                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

El resultat del p-value del Tipo és 1.3923733\times 10^{-32} i el del Sex és 0.0069676. 

Els 2 resultat són menor que 0.05, per tant rebutjem la hipòtesis nul·la $H_o\;$, podem afirmar que les mitjanes son diferents.

# Bibliografia

* https://rpubs.com/crazyhottommy/reorder-boxplot
* https://forcats.tidyverse.org/reference/fct_reorder.html
* https://rstudio-pubs-static.s3.amazonaws.com/326337_2155b78b37f74027869879598fb0d614.html#scheffes-test
* https://rpubs.com/Joaquin_AR/219148
* https://ca.wikibooks.org/wiki/Manual_de_R_(Estad%C3%ADstica)
* http://halweb.uc3m.es/esp/Personal/personas/amalonso/esp/p-valor.pdf
* https://es.wikipedia.org/wiki/Test_de_Shapiro–Wilk
* https://ca.wikipedia.org/wiki/Distribuci%C3%B3_t_de_Student
* https://rpubs.com/alvarohdez/101424

