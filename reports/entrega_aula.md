# Relatório do Pipeline de Análise – IPDM (Regressão Logística)

## 1. Introdução

Este relatório documenta o processo de preparação dos dados do IPDM e a aplicação de um modelo de regressão logística. O objetivo é prever se a média das quatro dimensões do estado é superior à média das quatro dimensões do município, com base nos indicadores municipais.

Os dados brutos estavam em formato longo (uma linha por município/ano/indicador) e foram transformados para formato largo, com cada indicador como uma coluna. A variável resposta binária `media` foi criada a partir da comparação das médias.

---

## 2. Transformação dos Dados (Formato Largo)

### 2.1. Leitura e padronização

O arquivo bruto é lido, os nomes das colunas são padronizados e os valores numéricos são convertidos corretamente (tratando vírgula como decimal e removendo pontos de milhar).

```r
# Script R para transformar o banco IPDM e calcular a coluna media
# - Leitura: data/raw/Datasets_IPDM/dados_ipdm.csv
# - Escrita: data/processed/Datasets_IPDM/dados_ipdm_largura.csv

parse_numeric <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\.", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  as.numeric(x)
}

# Caminhos relativos (a partir da pasta scripts/pesquisa_IPDM/)
caminho_raw <- file.path("..", "..", "data", "raw", "Datasets_IPDM", "dados_ipdm.csv")
caminho_processed <- file.path("..", "..", "data", "processed", "Datasets_IPDM", "dados_ipdm_largura.csv")

# Leitura do CSV
base <- read.csv2(
  caminho_raw,
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "latin1"
)

# Padroniza colunas (verificando se existem para evitar erro)
if ("Municipio" %in% names(base)) names(base)[names(base) == "Municipio"] <- "municipio"
if ("Ano" %in% names(base)) names(base)[names(base) == "Ano"] <- "ano"
if ("Tipo" %in% names(base)) names(base)[names(base) == "Tipo"] <- "tipo"

base$municipio <- trimws(base$municipio)
base$tipo <- trimws(base$tipo)
base$ano <- as.integer(base$ano)
base$Valor <- parse_numeric(base$Valor)
base$`Valor Estado` <- parse_numeric(base$`Valor Estado`)
```

### 2.2 Normalização dos Indicadores

Os nomes dos indicadores são padronizados para facilitar a manipulação.

```r
base$tipo <- ifelse(base$tipo == "Riqueza", "riqueza",
  ifelse(base$tipo == "Longevidade", "longevidade",
    ifelse(base$tipo == "Escolaridade", "escolaridade",
      ifelse(base$tipo == "IPDM", "IPDM", base$tipo))))

# Remove duplicatas por municipio + ano + tipo
base <- base[!duplicated(base[, c("cod_ibge", "municipio", "ano", "tipo")]), ]
```

### 2.3 Transformação para formato largo

Os dados são separados em dois conjuntos: valores do município e valores do estado. Cada um é transformado para formato largo usando reshape e depois unidos.

```r
# Cria tabelas separadas para cidade e estado
cidade <- split(base[, c("cod_ibge", "municipio", "ano", "tipo", "Valor")], base$tipo)
estado <- split(base[, c("cod_ibge", "municipio", "ano", "tipo", "Valor Estado")], base$tipo)

# Função auxiliar para wide
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

# Garante colunas do estado (caso algum indicador falte)
for (nm in c("riqueza_estado", "longevidade_estado", "escolaridade_estado", "IPDM_estado")) {
  if (!(nm %in% names(estado_wide))) {
    estado_wide[[nm]] <- NA_real_
  }
}

# Une as tabelas
resultado <- merge(cidade_wide, estado_wide, by = c("cod_ibge", "municipio", "ano"), all = TRUE)
```