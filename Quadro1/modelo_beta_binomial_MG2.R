# ============================================================
# ESCORPIONISMO E ACESSIBILIDADE AOS PESA
# Regressao beta-binomial com desfecho MG2
# ============================================================
#
# Desfecho:
#   MG2 = classificacao operacional ampliada de casos moderados/graves
#
# Analises:
#   1. Modelo principal: todos os municipios, acesso categorizado
#      (Local, <=30 min, 31-60 min, >60 min)
#   2. Modelo secundario: municipios sem acesso local, tempo continuo
#      expresso a cada 10 minutos
#
# ATENCAO:
# O arquivo original df_preliminar(1).csv ainda nao contem a coluna MG2.
# Antes de executar os modelos, acrescente ao banco municipal uma coluna
# chamada MG2, contendo o numero de casos MG2 de cada municipio.
#
# O script interrompe a execucao com uma mensagem clara caso MG2 nao exista.
# ============================================================


# ------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------

pacotes <- c(
  "glmmTMB",
  "dplyr",
  "broom.mixed",
  "DHARMa",
  "readr"
)

# Instala apenas os pacotes ausentes
pacotes_ausentes <- pacotes[!pacotes %in% rownames(installed.packages())]

if (length(pacotes_ausentes) > 0) {
  install.packages(pacotes_ausentes)
}

library(glmmTMB)
library(dplyr)
library(broom.mixed)
library(DHARMa)
library(readr)

install.packages('DHARMa')
install.packages("qgam")

# 1. Força a atualização do pacote mgcv na sua biblioteca pessoal
install.packages("mgcv", lib = "/home/usuario/R/x86_64-pc-linux-gnu-library/4.2")

# 2. Tente instalar o qgam novamente
install.packages("qgam")


# ------------------------------------------------------------
# 2. IMPORTACAO DO BANCO
# ------------------------------------------------------------

# Coloque o arquivo CSV na mesma pasta deste script.
arquivo <- "df_preliminar(1).csv"

if (!file.exists(arquivo)) {
  stop(
    paste0(
      "Arquivo nao encontrado: ", arquivo,
      "\nColoque o CSV na mesma pasta do script ou altere o objeto 'arquivo'."
    )
  )
}

df <- read.csv(
  arquivo,
  sep = ";",
  dec = ".",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

cat("\nDimensoes do banco original:\n")
print(dim(df))

cat("\nPrimeiros nomes de variaveis:\n")
print(names(df))


# ------------------------------------------------------------
# 3. LIMPEZA DAS COLUNAS DE INDICE
# ------------------------------------------------------------

df <- df %>%
  select(-any_of(c("Unnamed: 0.1", "Unnamed: 0")))


# ------------------------------------------------------------
# 4. VERIFICACAO DA EXISTENCIA DO MG2
# ------------------------------------------------------------

if (!"MG2" %in% names(df)) {
  stop(
    paste0(
      "\nA coluna MG2 nao foi encontrada no banco.\n\n",
      "O arquivo original contem MG, mas ainda nao contem MG2.\n",
      "Acrescente ao banco municipal uma coluna denominada MG2, ",
      "com a contagem de casos moderados/graves segundo a definicao ",
      "operacional ampliada, e execute novamente o script.\n"
    )
  )
}


# ------------------------------------------------------------
# 5. VERIFICACAO DAS VARIAVEIS NECESSARIAS
# ------------------------------------------------------------

variaveis_necessarias <- c(
  "MG2",
  "TOTAL_CASOS",
  "ACESSO_LOCAL",
  "TEMPO",
  "CAT_TEMPO",
  "INCID_MEDIA_GERAL",
  "PROP_POP_0A10",
  "PROP_POP_60MAIS",
  "LOG_POP",
  "PROP_CLASS_IGN"
)

variaveis_ausentes <- setdiff(variaveis_necessarias, names(df))

if (length(variaveis_ausentes) > 0) {
  stop(
    paste(
      "Variaveis ausentes no banco:",
      paste(variaveis_ausentes, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# 6. PREPARACAO DO DESFECHO E DO BANCO ANALITICO
# ------------------------------------------------------------

df <- df %>%
  mutate(
    MG2 = as.numeric(MG2),
    TOTAL_CASOS = as.numeric(TOTAL_CASOS),
    LEVE_MG2 = TOTAL_CASOS - MG2
  )

# Verificacoes de consistencia
resumo_consistencia <- df %>%
  summarise(
    municipios = n(),
    sem_casos = sum(TOTAL_CASOS == 0, na.rm = TRUE),
    mg2_ausente = sum(is.na(MG2)),
    total_ausente = sum(is.na(TOTAL_CASOS)),
    mg2_negativo = sum(MG2 < 0, na.rm = TRUE),
    leve_mg2_negativo = sum(LEVE_MG2 < 0, na.rm = TRUE),
    mg2_maior_que_total = sum(MG2 > TOTAL_CASOS, na.rm = TRUE)
  )

cat("\nResumo de consistencia:\n")
print(resumo_consistencia)

if (
  resumo_consistencia$mg2_negativo > 0 ||
  resumo_consistencia$leve_mg2_negativo > 0 ||
  resumo_consistencia$mg2_maior_que_total > 0
) {
  stop(
    "Foram encontradas inconsistencias: MG2 negativo ou superior ao TOTAL_CASOS."
  )
}

df_modelo <- df %>%
  filter(
    TOTAL_CASOS > 0,
    !is.na(MG2),
    !is.na(LEVE_MG2),
    !is.na(ACESSO_LOCAL),
    !is.na(CAT_TEMPO),
    !is.na(INCID_MEDIA_GERAL),
    !is.na(PROP_POP_0A10),
    !is.na(PROP_POP_60MAIS),
    !is.na(LOG_POP),
    !is.na(PROP_CLASS_IGN)
  ) %>%
  mutate(
    CAT_TEMPO = factor(
      CAT_TEMPO,
      levels = c(
        "Local",
        "≤30 min",
        "31–60 min",
        ">60 min"
      )
    )
  )

cat("\nNumero de municipios incluidos no modelo principal:\n")
print(nrow(df_modelo))

cat("\nDistribuicao das categorias de acesso:\n")
print(table(df_modelo$CAT_TEMPO, useNA = "ifany"))

if (any(is.na(df_modelo$CAT_TEMPO))) {
  warning(
    "Ha valores de CAT_TEMPO que nao coincidem com os niveis definidos."
  )
}


# ------------------------------------------------------------
# 7. DESCRICAO DO DESFECHO MG2
# ------------------------------------------------------------

resumo_mg2 <- df_modelo %>%
  summarise(
    municipios = n(),
    total_casos = sum(TOTAL_CASOS),
    total_mg2 = sum(MG2),
    proporcao_mg2 = sum(MG2) / sum(TOTAL_CASOS)
  )

cat("\nResumo global do desfecho MG2:\n")
print(resumo_mg2)

resumo_por_acesso <- df_modelo %>%
  group_by(CAT_TEMPO) %>%
  summarise(
    municipios = n(),
    total_casos = sum(TOTAL_CASOS),
    total_mg2 = sum(MG2),
    proporcao_mg2 = sum(MG2) / sum(TOTAL_CASOS),
    .groups = "drop"
  )

cat("\nResumo de MG2 por categoria de acesso:\n")
print(resumo_por_acesso)

write.csv(
  resumo_por_acesso,
  "resumo_mg2_por_categoria_acesso.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ============================================================
# MODELO 1 — PRINCIPAL
# Todos os municipios; acesso categorizado
# Referencia: acesso local
# ============================================================

modelo_bb_mg2 <- glmmTMB(
  cbind(MG2, LEVE_MG2) ~
    CAT_TEMPO +
    INCID_MEDIA_GERAL +
    PROP_POP_0A10 +
    PROP_POP_60MAIS +
    LOG_POP +
    PROP_CLASS_IGN,
  family = betabinomial(link = "logit"),
  data = df_modelo
)

cat("\n============================================================\n")
cat("MODELO PRINCIPAL — TODOS OS MUNICIPIOS\n")
cat("============================================================\n")

print(summary(modelo_bb_mg2))

cat("\nHessiana positiva definida:\n")
print(modelo_bb_mg2$sdr$pdHess)

cat("\nDiagnostico de convergencia:\n")
diagnose(modelo_bb_mg2)

cat("\nParametro de dispersao do modelo:\n")
print(sigma(modelo_bb_mg2))


# ------------------------------------------------------------
# 8. ODDS RATIOS DO MODELO PRINCIPAL
# ------------------------------------------------------------

resultado_principal <- tidy(
  modelo_bb_mg2,
  effects = "fixed",
  component = "cond",
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    variavel = term,
    OR = estimate,
    IC95_inferior = conf.low,
    IC95_superior = conf.high,
    valor_p = p.value
  )

cat("\nOdds ratios — modelo principal:\n")
print(resultado_principal)

write.csv(
  resultado_principal,
  "resultado_beta_binomial_mg2_principal.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------
# 9. DIAGNOSTICO DOS RESIDUOS — MODELO PRINCIPAL
# ------------------------------------------------------------

set.seed(123)

residuos_principal <- simulateResiduals(
  fittedModel = modelo_bb_mg2,
  n = 1000
)

png(
  filename = "diagnostico_residuos_mg2_principal.png",
  width = 1600,
  height = 1200,
  res = 180
)

plot(residuos_principal)

dev.off()

cat("\nTeste de uniformidade — modelo principal:\n")
print(testUniformity(residuos_principal))

cat("\nTeste de dispersao — modelo principal:\n")
print(testDispersion(residuos_principal))

cat("\nTeste de outliers — modelo principal:\n")
print(testOutliers(residuos_principal))

cat("\nTeste de inflacao de zeros — modelo principal:\n")
print(testZeroInflation(residuos_principal))


# ============================================================
# MODELO 2 — SECUNDARIO
# Apenas municipios sem acesso local
# Tempo continuo a cada 10 minutos
# ============================================================

df_regional <- df_modelo %>%
  filter(
    ACESSO_LOCAL == 0,
    !is.na(TEMPO)
  ) %>%
  mutate(
    TEMPO_10MIN = TEMPO / 10
  )

cat("\nNumero de municipios sem acesso local:\n")
print(nrow(df_regional))

cat("\nResumo do tempo de deslocamento:\n")
print(summary(df_regional$TEMPO))

modelo_bb_regional_mg2 <- glmmTMB(
  cbind(MG2, LEVE_MG2) ~
    TEMPO_10MIN +
    INCID_MEDIA_GERAL +
    PROP_POP_0A10 +
    PROP_POP_60MAIS +
    LOG_POP +
    PROP_CLASS_IGN,
  family = betabinomial(link = "logit"),
  data = df_regional
)

cat("\n============================================================\n")
cat("MODELO SECUNDARIO — MUNICIPIOS SEM ACESSO LOCAL\n")
cat("============================================================\n")

print(summary(modelo_bb_regional_mg2))

cat("\nHessiana positiva definida:\n")
print(modelo_bb_regional_mg2$sdr$pdHess)

cat("\nDiagnostico de convergencia:\n")
diagnose(modelo_bb_regional_mg2)

cat("\nParametro de dispersao do modelo:\n")
print(sigma(modelo_bb_regional_mg2))


# ------------------------------------------------------------
# 10. ODDS RATIOS DO MODELO REGIONAL
# ------------------------------------------------------------

resultado_regional <- tidy(
  modelo_bb_regional_mg2,
  effects = "fixed",
  component = "cond",
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    variavel = term,
    OR = estimate,
    IC95_inferior = conf.low,
    IC95_superior = conf.high,
    valor_p = p.value
  )

cat("\nOdds ratios — modelo regional:\n")
print(resultado_regional)

write.csv(
  resultado_regional,
  "resultado_beta_binomial_mg2_regional.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------
# 11. DIAGNOSTICO DOS RESIDUOS — MODELO REGIONAL
# ------------------------------------------------------------

set.seed(123)

residuos_regional <- simulateResiduals(
  fittedModel = modelo_bb_regional_mg2,
  n = 1000
)

png(
  filename = "diagnostico_residuos_mg2_regional.png",
  width = 1600,
  height = 1200,
  res = 180
)

plot(residuos_regional)

dev.off()

cat("\nTeste de uniformidade — modelo regional:\n")
print(testUniformity(residuos_regional))

cat("\nTeste de dispersao — modelo regional:\n")
print(testDispersion(residuos_regional))

cat("\nTeste de outliers — modelo regional:\n")
print(testOutliers(residuos_regional))


# ------------------------------------------------------------
# 12. MODELO COMPLEMENTAR REGIONAL COM TEMPO CATEGORIZADO
# ------------------------------------------------------------

df_regional <- df_regional %>%
  mutate(
    CAT_TEMPO_REGIONAL = factor(
      as.character(CAT_TEMPO),
      levels = c(
        "≤30 min",
        "31–60 min",
        ">60 min"
      )
    )
  )

modelo_bb_regional_cat_mg2 <- glmmTMB(
  cbind(MG2, LEVE_MG2) ~
    CAT_TEMPO_REGIONAL +
    INCID_MEDIA_GERAL +
    PROP_POP_0A10 +
    PROP_POP_60MAIS +
    LOG_POP +
    PROP_CLASS_IGN,
  family = betabinomial(link = "logit"),
  data = df_regional
)

resultado_regional_cat <- tidy(
  modelo_bb_regional_cat_mg2,
  effects = "fixed",
  component = "cond",
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  transmute(
    variavel = term,
    OR = estimate,
    IC95_inferior = conf.low,
    IC95_superior = conf.high,
    valor_p = p.value
  )

cat("\nOdds ratios — modelo regional com tempo categorizado:\n")
print(resultado_regional_cat)

write.csv(
  resultado_regional_cat,
  "resultado_beta_binomial_mg2_regional_categorico.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------
# 13. INFORMACOES FINAIS DA EXECUCAO
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ANALISE CONCLUIDA\n")
cat("============================================================\n")

cat(
  "\nArquivos gerados:\n",
  "- resumo_mg2_por_categoria_acesso.csv\n",
  "- resultado_beta_binomial_mg2_principal.csv\n",
  "- resultado_beta_binomial_mg2_regional.csv\n",
  "- resultado_beta_binomial_mg2_regional_categorico.csv\n",
  "- diagnostico_residuos_mg2_principal.png\n",
  "- diagnostico_residuos_mg2_regional.png\n"
)

cat("\nInformacoes da sessao do R:\n")
print(sessionInfo())
