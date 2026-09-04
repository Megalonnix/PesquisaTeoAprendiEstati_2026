# Relatório do Pipeline de Análise – IPDM (Regressão Logística)

- **Link código fonte:** https://github.com/Megalonnix/PesquisaTeoAprendiEstati_2026/blob/analise_exploratoria/scripts/pesquisa_IPDM/notebook_03__.ipynb

## 1. Introdução

Este relatório documenta o processo de preparação dos dados do IPDM e a aplicação de um modelo de regressão logística. O objetivo é prever se a média das quatro dimensões do estado é superior à média das quatro dimensões do município, com base nos indicadores municipais.

Os dados brutos estavam em formato longo (uma linha por município/ano/indicador) e foram transformados para formato largo, com cada indicador como uma coluna. A variável resposta binária `media` foi criada a partir da comparação das médias.

## 2. Transformação dos Dados (Formato Largo)

### 2.1. Leitura e padronização

O arquivo bruto é lido, os nomes das colunas são padronizados e os valores numéricos são convertidos corretamente (tratando vírgula como decimal e removendo pontos de milhar).

```r
parse_numeric <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\.", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  as.numeric(x)
}

caminho_raw <- file.path("..", "..", "data", "raw", "Datasets_IPDM", "dados_ipdm.csv")
caminho_processed <- file.path("..", "..", "data", "processed", "Datasets_IPDM", "dados_ipdm_largura.csv")

base <- read.csv2(
  caminho_raw,
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "latin1"
)

if ("Municipio" %in% names(base)) names(base)[names(base) == "Municipio"] <- "municipio"
if ("Ano" %in% names(base)) names(base)[names(base) == "Ano"] <- "ano"
if ("Tipo" %in% names(base)) names(base)[names(base) == "Tipo"] <- "tipo"

base$municipio <- trimws(base$municipio)
base$tipo <- trimws(base$tipo)
base$ano <- as.integer(base$ano)
base$Valor <- parse_numeric(base$Valor)
base$`Valor Estado` <- parse_numeric(base$`Valor Estado`)
```

### 2.2. Normalização dos indicadores

Os nomes dos indicadores são padronizados para facilitar a manipulação.

```r
base$tipo <- ifelse(base$tipo == "Riqueza", "riqueza",
  ifelse(base$tipo == "Longevidade", "longevidade",
    ifelse(base$tipo == "Escolaridade", "escolaridade",
      ifelse(base$tipo == "IPDM", "IPDM", base$tipo))))

base <- base[!duplicated(base[, c("cod_ibge", "municipio", "ano", "tipo")]), ]
```

### 2.3. Transformação para formato largo

Os dados são separados em dois conjuntos: valores do município e valores do estado. Cada um é transformado para formato largo usando `reshape` e depois unidos.

```r
cidade <- split(base[, c("cod_ibge", "municipio", "ano", "tipo", "Valor")], base$tipo)
estado <- split(base[, c("cod_ibge", "municipio", "ano", "tipo", "Valor Estado")], base$tipo)

to_wide <- function(df, value_col) {
  out <- reshape(
    df,
    idvar = c("cod_ibge", "municipio", "ano"),
    timevar = "tipo",
    direction = "wide"
  )
  names(out) <- gsub(paste0(value_col, "."), "", names(out), fixed = TRUE)
  out
}

cidade_wide <- Reduce(function(x, y) merge(x, y, by = c("cod_ibge", "municipio", "ano"), all = TRUE),
  lapply(cidade, function(df) {
    names(df)[names(df) == "Valor"] <- unique(df$tipo)
    df <- df[, c("cod_ibge", "municipio", "ano", unique(df$tipo))]
    df
  }))

estado_wide <- Reduce(function(x, y) merge(x, y, by = c("cod_ibge", "municipio", "ano"), all = TRUE),
  lapply(estado, function(df) {
    names(df)[names(df) == "Valor Estado"] <- paste0(unique(df$tipo), "_estado")
    df <- df[, c("cod_ibge", "municipio", "ano", paste0(unique(df$tipo), "_estado"))]
    df
  }))

for (nm in c("riqueza_estado", "longevidade_estado", "escolaridade_estado", "IPDM_estado")) {
  if (!(nm %in% names(estado_wide))) {
    estado_wide[[nm]] <- NA_real_
  }
}

resultado <- merge(cidade_wide, estado_wide, by = c("cod_ibge", "municipio", "ano"), all = TRUE)
```

### 2.4. Cálculo da variável resposta `media`

A variável `media` é binária: **1** se a média do estado for maior que a média do município, **0** caso contrário. Linhas com valores faltantes são descartadas.

```r
resultado <- resultado[, c(
  "cod_ibge",
  "municipio",
  "ano",
  "riqueza",
  "riqueza_estado",
  "longevidade",
  "longevidade_estado",
  "escolaridade",
  "escolaridade_estado",
  "IPDM",
  "IPDM_estado"
)]

resultado$media <- ifelse(
  !is.na(resultado$riqueza) & !is.na(resultado$longevidade) &
    !is.na(resultado$escolaridade) & !is.na(resultado$IPDM) &
    !is.na(resultado$riqueza_estado) & !is.na(resultado$longevidade_estado) &
    !is.na(resultado$escolaridade_estado) & !is.na(resultado$IPDM_estado),
  as.integer(
    ((resultado$riqueza_estado + resultado$longevidade_estado +
         resultado$escolaridade_estado + resultado$IPDM_estado) / 4) >
    ((resultado$riqueza + resultado$longevidade +
         resultado$escolaridade + resultado$IPDM) / 4)
  ),
  NA_integer_
)

dir.create(dirname(caminho_processed), recursive = TRUE, showWarnings = FALSE)
write.csv2(resultado, caminho_processed, row.names = FALSE, fileEncoding = "latin1")
```

### 2.5. Visualização do resultado

```r
print(head(resultado))
#   cod_ibge  municipio  ano riqueza riqueza_estado longevidade
# 1  3500105 Adamantina 2014   0.365          0.457       0.770
# 2  3500105 Adamantina 2016   0.365          0.438       0.710
# 3  3500105 Adamantina 2018   0.382          0.451       0.686
# 4  3500105 Adamantina 2020   0.378          0.439       0.779
# 5  3500105 Adamantina 2022   0.359          0.441       0.742
# 6  3500105 Adamantina 2024   0.381          0.464       0.784
#   longevidade_estado escolaridade escolaridade_estado  IPDM IPDM_estado media
# 1              0.698        0.510               0.449 0.548       0.535     0
# 2              0.717        0.587               0.511 0.554       0.555     1
# 3              0.721        0.612               0.563 0.560       0.578     1
# 4              0.722        0.625               0.594 0.594       0.585     0
# 5              0.697        0.594               0.556 0.565       0.565     0
# 6              0.716        0.658               0.569 0.608       0.583     0

print(paste("Arquivo salvo em:", caminho_processed))
# [1] "Arquivo salvo em: ../../data/processed/Datasets_IPDM/dados_ipdm_largura.csv"
```

## 3. Regressão Logística

### 3.1. Carregamento dos dados processados

O arquivo largo gerado na etapa anterior é carregado, e as colunas de interesse são selecionadas.

```r
caminho_dados <- file.path("..", "..", "data", "processed", "Datasets_IPDM", "dados_ipdm_largura.csv")
dados <- read.csv2(caminho_dados, fileEncoding = "latin1")

dados_modelo <- dados[, c("riqueza", "longevidade", "escolaridade", "IPDM", "media")]
dados_modelo <- na.omit(dados_modelo)

cat("Total de observacoes usadas:", nrow(dados_modelo), "\n")
# Total de observacoes usadas: 3870

cat("Proporcao de media = 1:", mean(dados_modelo$media), "\n\n")
# Proporcao de media = 1: 0.6860465
```

### 3.2. Ajuste do modelo logístico

Utiliza-se a função `glm()` com família binomial para estimar os coeficientes.

```r
modelo <- glm(media ~ riqueza + longevidade + escolaridade + IPDM,
              family = binomial, data = dados_modelo)

summary(modelo)
```

```
Call:
glm(formula = media ~ riqueza + longevidade + escolaridade + IPDM, 
    family = binomial, data = dados_modelo)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)    57.108      2.158  26.457   <2e-16 ***
riqueza      -159.211     75.899  -2.098   0.0359 *  
longevidade  -162.642     75.859  -2.144   0.0320 *  
escolaridade -153.536     75.930  -2.022   0.0432 *  
IPDM          375.943    227.412   1.653   0.0983 .  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4816.0  on 3869  degrees of freedom
Residual deviance: 1661.5  on 3865  degrees of freedom
AIC: 1671.5

Number of Fisher Scoring iterations: 7
```

### 3.3. Interpretação dos coeficientes e Odds Ratio

Os coeficientes estão na escala do logito. Para facilitar a interpretação, calcula-se a Razão de Chances (Odds Ratio = exp(coeficiente)).

```r
cat("\nCoeficientes (logit):\n")
print(round(coef(modelo), 4))
#  (Intercept)      riqueza  longevidade escolaridade         IPDM 
#     57.1082    -159.2112    -162.6423    -153.5359     375.9433 

cat("\nOdds Ratio (exp(coef)):\n")
print(round(exp(coef(modelo)), 4))
#   (Intercept)       riqueza   longevidade  escolaridade          IPDM 
#  6.335654e+24  0.000000e+00  0.000000e+00  0.000000e+00 1.862436e+163

# Observação: Os Odds Ratio extremos (próximos de 0 ou infinito) ocorrem devido à escala dos dados e à alta significância dos coeficientes.
```

## 4. Avaliação do Modelo

### 4.1. Matriz de confusão (limiar = 0,5)

Prevê-se a probabilidade de `media = 1` para cada observação e classifica-se como 1 se a probabilidade for maior que 0,5.

```r
p <- predict(modelo, type = "response")
limiar <- 0.5
yhat <- as.integer(p > limiar)

matriz <- table(Real = dados_modelo$media, Previsto = yhat)
print(matriz)
```

```
    Previsto
Real    0    1
   0  995  220
   1  180 2475
```

### 4.2. Métricas de desempenho

```r
VP <- matriz["1", "1"]
VN <- matriz["0", "0"]
FP <- matriz["0", "1"]
FN <- matriz["1", "0"]

acuracia       <- (VP + VN) / sum(matriz)
precisao       <- VP / (VP + FP)
recall         <- VP / (VP + FN)
especificidade <- VN / (VN + FP)

cat("\n--- Metricas (limiar = 0.5) ---\n")
cat("Acuracia:       ", round(acuracia, 3), "\n")
cat("Precisao:       ", round(precisao, 3), "\n")
cat("Recall/Sensib:  ", round(recall, 3), "\n")
cat("Especificidade: ", round(especificidade, 3), "\n")
```

```
--- Metricas (limiar = 0.5) ---
Acuracia:        0.897 
Precisao:        0.918 
Recall/Sensib:   0.932 
Especificidade:  0.819
```

### 4.3. Teste com diferentes limiares

```r
testar_limiar <- function(p, y_real, limiar) {
  yhat <- as.integer(p > limiar)
  m <- table(Real = y_real, Previsto = factor(yhat, levels = c(0,1)))
  cat("\n--- Limiar =", limiar, "---\n")
  print(m)
}

testar_limiar(p, dados_modelo$media, 0.3)
testar_limiar(p, dados_modelo$media, 0.5)
testar_limiar(p, dados_modelo$media, 0.7)
```

```
--- Limiar = 0.3 ---
    Previsto
Real    0    1
   0  896  319
   1   53 2602

--- Limiar = 0.5 ---
    Previsto
Real    0    1
   0  995  220
   1  180 2475

--- Limiar = 0.7 ---
    Previsto
Real    0    1
   0 1076  139
   1  329 2326
```

### 4.4. Curva ROC e AUC

```r
ord <- order(p, decreasing = TRUE)
tpr <- cumsum(dados_modelo$media[ord]) / sum(dados_modelo$media)
fpr <- cumsum(1 - dados_modelo$media[ord]) / sum(1 - dados_modelo$media)

plot(c(0, fpr), c(0, tpr), type = "l", lwd = 3, col = "darkorange",
     xlab = "FPR (falso positivo)", ylab = "TPR (recall)",
     main = "Curva ROC - Modelo IPDM")
abline(0, 1, lty = 2, col = "gray")

auc <- mean(outer(p[dados_modelo$media == 1], p[dados_modelo$media == 0], ">"))
cat("\nAUC (Area sob a curva):", round(auc, 3), "\n")
```

```
AUC (Area sob a curva): 0.965
```

A imagem da curva ROC é gerada no gráfico do R.

## 5. Conclusão

O pipeline de transformação e modelagem mostrou-se eficaz. O modelo logístico consegue prever com alta acurácia (89,7%) se a média do estado é superior à do município, com AUC de 0,965, indicando excelente poder discriminativo.

- Os coeficientes negativos para `riqueza`, `longevidade` e `escolaridade` indicam que municípios com melhores indicadores nessas dimensões têm **menor** chance de serem superados pelo estado (ou seja, tendem a ter `media = 0`).
- O `IPDM` apresentou coeficiente positivo, mas com significância marginal (p = 0,098).
- O modelo é estatisticamente significativo (residual deviance = 1661,5 contra null deviance = 4816,0).

Todo o processo é reproduzível a partir dos scripts disponíveis no repositório.

