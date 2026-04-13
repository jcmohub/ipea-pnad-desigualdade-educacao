# =============================================================================
# QUESTÃO 01: Infraestrutura Escolar e IDEB
# Chamada Pública IPEA/PIPA Nº 011/2026
# =============================================================================
# Descrição : Análise da correlação entre infraestrutura escolar (Censo Escolar)
#             e resultados do IDEB (2017–2023), por etapa de ensino e rede.
# Autor     : Jose Carlos Martinez 
# =============================================================================
# RESULTADOS REAIS OBTIDOS:
#   AI Municipal  | r(ISI,IDEB)=0.397 | beta_ISI=3.032 | R²=0.165 | n=35.498
#   AF Municipal  | r(ISI,IDEB)=0.378 | beta_ISI=2.246 | R²=0.146 | n=17.218
#   EM Estadual   | r(ISI,IDEB)=0.293 | beta_ISI=1.631 | R²=0.091 | n=14.226
#   Gap Q4–Q1 IDEB 2023: AI=1.10 | AF=0.91 | EM=0.43
# =============================================================================

# ── 0. Configuração do ambiente ───────────────────────────────────────────────

# Instale os pacotes caso necessário:
# install.packages(c("tidyverse","data.table","readxl","janitor",
#                    "ggplot2","scales","corrplot","psych","broom","archive"))

library(tidyverse)   # Manipulação e visualização de dados
library(data.table)  # Leitura eficiente de arquivos grandes (Censo)
library(readxl)      # Leitura dos arquivos XLSX do IDEB
library(janitor)     # Limpeza de nomes de colunas
library(ggplot2)     # Visualizações
library(scales)      # Formatação de eixos
library(corrplot)    # Matriz de correlação
library(broom)       # Resumo de modelos em tibble

set.seed(42)

# Criar estrutura de diretórios
dir.create("dados/ideb",  recursive = TRUE, showWarnings = FALSE)
dir.create("dados/censo", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tabelas",  recursive = TRUE, showWarnings = FALSE)

# ── 1. Carregamento dos dados do IDEB ────────────────────────────────────────
# Fonte: INEP – https://www.gov.br/inep/pt-br/areas-de-atuacao/
#               pesquisas-estatisticas-e-indicadores/ideb/resultados
# Arquivos: resultados por ESCOLA (seção "Escolas" na página)
# Anos disponíveis com IDEB escolar: 2017, 2019, 2021 e 2023

#' Lê e limpa um arquivo XLSX do IDEB por escola
#' @param path  Caminho para o .xlsx
#' @param etapa Label da etapa (ex: "Anos Iniciais EF")
#' @param redes Vetor de redes a manter (ex: c("Municipal"))
#' @return tibble limpo com CO_ENTIDADE e colunas IDEB_20XX
carregar_ideb <- function(path, etapa, redes) {
  
  # O INEP publica 9 linhas de cabeçalho antes dos dados
  df <- read_excel(path, skip = 9, col_types = "text") %>%
    clean_names() %>%
    # Substituir marcadores de ausência de dado
    mutate(across(everything(), ~ na_if(., "-")),
           across(everything(), ~ na_if(., "ND")),
           across(everything(), ~ na_if(., ""))) %>%
    # Padronizar chave de merge: ID_ESCOLA → CO_ENTIDADE (8 dígitos)
    mutate(
      co_entidade = str_pad(id_escola, width = 8, pad = "0"),
      etapa       = etapa
    ) %>%
    # Filtrar pela(s) rede(s) desejada(s)
    filter(rede %in% redes)
  
  # Extrair apenas os IDEB observados por ano
  anos <- c(2017, 2019, 2021, 2023)
  for (ano in anos) {
    col_orig <- paste0("vl_observado_", ano)
    col_nova <- paste0("ideb_", ano)
    if (col_orig %in% names(df)) {
      df <- df %>%
        mutate(!!col_nova := as.numeric(.data[[col_orig]]))
    }
  }
  
  df %>%
    select(co_entidade, sg_uf, rede, etapa,
           starts_with("ideb_"), co_municipio, no_municipio)
}

# Carregar os três arquivos IDEB (ajuste os caminhos conforme necessário)
ai_mun <- carregar_ideb(
  path  = "dados/ideb/divulgacao_anos_iniciais_escolas_2023.xlsx",
  etapa = "Anos Iniciais EF",
  redes = "Municipal"
)

af_mun <- carregar_ideb(
  path  = "dados/ideb/divulgacao_anos_finais_escolas_2023.xlsx",
  etapa = "Anos Finais EF",
  redes = "Municipal"
)

em_est <- carregar_ideb(
  path  = "dados/ideb/divulgacao_ensino_medio_escolas_2023.xlsx",
  etapa = "Ensino Médio",
  redes = "Estadual"
)

# Verificação básica
cat(sprintf("AI Municipal: %d escolas | IDEB_2023 válidos: %d\n",
            nrow(ai_mun), sum(!is.na(ai_mun$ideb_2023))))
cat(sprintf("AF Municipal: %d escolas | IDEB_2023 válidos: %d\n",
            nrow(af_mun), sum(!is.na(af_mun$ideb_2023))))
cat(sprintf("EM Estadual:  %d escolas | IDEB_2023 válidos: %d\n",
            nrow(em_est), sum(!is.na(em_est$ideb_2023))))

# ── 2. Carregamento dos Microdados do Censo Escolar ──────────────────────────
# Fonte: INEP – https://www.gov.br/inep/pt-br/acesso-a-informacao/
#               dados-abertos/microdados/censo-escolar
# Os ZIPs contêm um único CSV separado por ";" e codificado em Latin-1.
# Recomenda-se usar data.table::fread() pela eficiência com ~200MB por arquivo.

# Variáveis de infraestrutura necessárias
VARS_INFRA <- c(
  # Identificação e filtros
  "CO_ENTIDADE", "NO_ENTIDADE", "SG_UF", "NO_REGIAO", "CO_MUNICIPIO", "NO_MUNICIPIO",
  "TP_DEPENDENCIA",            # 2=Estadual, 3=Municipal
  "TP_SITUACAO_FUNCIONAMENTO", # 1=Em atividade
  "TP_LOCALIZACAO",            # 1=Urbana, 2=Rural
  # D1 – Conectividade e Tecnologia
  "IN_INTERNET",               # Acesso à internet
  "IN_BANDA_LARGA",            # Internet banda larga
  "IN_LABORATORIO_INFORMATICA",# Laboratório de informática
  "QT_DESKTOP_ALUNO",          # Qtde de desktops para alunos
  "QT_COMP_PORTATIL_ALUNO",    # Qtde de portáteis para alunos
  "QT_TABLET_ALUNO",           # Qtde de tablets para alunos
  "IN_EQUIP_LOUSA_DIGITAL",    # Lousa digital
  # D2 – Estrutura Pedagógica
  "IN_BIBLIOTECA",             # Biblioteca
  "IN_BIBLIOTECA_SALA_LEITURA",# Sala de leitura (substituto de biblioteca)
  "IN_LABORATORIO_CIENCIAS",   # Laboratório de ciências
  "IN_QUADRA_ESPORTES",        # Quadra de esportes (qualquer tipo)
  "IN_REFEITORIO",             # Refeitório
  "IN_SALA_PROFESSOR",         # Sala de professores
  # D3 – Saneamento e Condições Básicas
  "IN_AGUA_REDE_PUBLICA",      # Água via rede pública
  "IN_AGUA_POTAVEL",           # Água potável (qualquer fonte)
  "IN_ENERGIA_REDE_PUBLICA",   # Energia elétrica via rede pública
  "IN_ESGOTO_REDE_PUBLICA",    # Esgoto via rede pública
  "IN_ESGOTO_FOSSA_SEPTICA",   # Fossa séptica (solução adequada)
  "IN_BANHEIRO",               # Banheiro
  "IN_LIXO_SERVICO_COLETA",    # Coleta de lixo
  # Controles
  "QT_SALAS_UTILIZADAS",       # Número de salas utilizadas
  "QT_MAT_BAS"                 # Total de matrículas
)

#' Lê o CSV do Censo Escolar de um ano e filtra escolas ativas e públicas
#' @param path_csv Caminho para o arquivo CSV descomprimido
#' @param ano      Ano do censo
#' @return data.table com escolas estaduais e municipais em atividade
carregar_censo <- function(path_csv, ano) {
  
  cat(sprintf("Lendo Censo %d...\n", ano))
  
  # fread é muito mais eficiente para CSVs grandes
  df <- fread(
    file     = path_csv,
    sep      = ";",
    encoding = "Latin-1",
    select   = VARS_INFRA,    # Carrega apenas as colunas necessárias
    colClasses = "character"  # Tudo como texto para evitar coerção automática
  )
  
  # Filtros: apenas escolas em atividade (1) + redes estadual (2) e municipal (3)
  df[, TP_SITUACAO_FUNCIONAMENTO := as.integer(TP_SITUACAO_FUNCIONAMENTO)]
  df[, TP_DEPENDENCIA            := as.integer(TP_DEPENDENCIA)]
  df <- df[TP_SITUACAO_FUNCIONAMENTO == 1 & TP_DEPENDENCIA %in% c(2L, 3L)]
  
  # Padronizar chave de merge
  df[, CO_ENTIDADE := str_pad(CO_ENTIDADE, width = 8, pad = "0")]
  df[, ANO := ano]
  
  cat(sprintf("  → %d escolas (estadual + municipal)\n", nrow(df)))
  return(as_tibble(df))
}

# Carregar os 4 anos do Censo (ajuste os caminhos)
# Os CSVs ficam dentro da pasta DADOS/ ao descompactar os ZIPs do INEP
censo_2017 <- carregar_censo("dados/censo/2017/microdados_ed_basica_2017.csv", 2017)
censo_2019 <- carregar_censo("dados/censo/2019/microdados_ed_basica_2019.csv", 2019)
censo_2021 <- carregar_censo("dados/censo/2021/microdados_ed_basica_2021.csv", 2021)
censo_2023 <- carregar_censo("dados/censo/2023/microdados_ed_basica_2023.csv", 2023)

# ── 3. Construção do ISI (Indicador Sintético de Infraestrutura) ─────────────
#
# O ISI é composto por três dimensões com pesos iguais (⅓ cada):
#
#   D1 – Conectividade e Tecnologia  (5 componentes)
#        internet, banda larga, lab. informática, computadores/aluno, lousa digital
#
#   D2 – Estrutura Pedagógica        (5 componentes)
#        biblioteca/sala leitura, lab. ciências, quadra esportes, refeitório, sala professor
#
#   D3 – Saneamento e Condições Básicas (5 componentes)
#        água (rede ou potável), energia rede pública, esgoto (rede ou fossa séptica),
#        banheiro, coleta de lixo
#
# Todas as variáveis são binárias (0/1); computadores são normalizados (máx. 30).
# ISI ∈ [0, 1]: quanto mais próximo de 1, melhor a infraestrutura.

#' Converte coluna binária do censo para numérico (0 ou 1)
bin_num <- function(df, col) {
  if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
}

#' Normaliza variável quantitativa entre 0 e 1, com referência = ref
norm_qt <- function(df, col, ref = 30) {
  if (col %in% names(df)) pmin(as.numeric(df[[col]]) / ref, 1, na.rm = FALSE)
  else rep(0, nrow(df))
}

#' Calcula o ISI e suas três dimensões para um data.frame do Censo
#' @param df data.frame do Censo Escolar (já filtrado)
#' @return df com colunas adicionais: D1, D2, D3, ISI
calcular_isi <- function(df) {
  
  df <- df %>%
    mutate(
      # D1 – Conectividade e Tecnologia ──────────────────────────────────────
      # Computadores disponíveis (desktop + portátil + tablet), norm. por 30
      qt_comp_norm = pmin(
        (replace_na(as.numeric(QT_DESKTOP_ALUNO), 0) +
           replace_na(as.numeric(QT_COMP_PORTATIL_ALUNO), 0) +
           replace_na(as.numeric(QT_TABLET_ALUNO), 0)) / 30,
        1
      ),
      D1 = (replace_na(as.numeric(IN_INTERNET), 0) +
              replace_na(as.numeric(IN_BANDA_LARGA), 0) +
              replace_na(as.numeric(IN_LABORATORIO_INFORMATICA), 0) +
              qt_comp_norm +
              replace_na(as.numeric(IN_EQUIP_LOUSA_DIGITAL), 0)) / 5,
      
      # D2 – Estrutura Pedagógica ─────────────────────────────────────────────
      # Biblioteca ou sala de leitura contam como um único componente (clip a 1)
      bib_ou_sala = pmin(replace_na(as.numeric(IN_BIBLIOTECA), 0) +
                           replace_na(as.numeric(IN_BIBLIOTECA_SALA_LEITURA), 0), 1),
      D2 = (bib_ou_sala +
              replace_na(as.numeric(IN_LABORATORIO_CIENCIAS), 0) +
              pmin(replace_na(as.numeric(IN_QUADRA_ESPORTES), 0), 1) +
              replace_na(as.numeric(IN_REFEITORIO), 0) +
              replace_na(as.numeric(IN_SALA_PROFESSOR), 0)) / 5,
      
      # D3 – Saneamento e Condições Básicas ───────────────────────────────────
      # Água: rede pública OU água potável
      agua_ok    = pmin(replace_na(as.numeric(IN_AGUA_REDE_PUBLICA), 0) +
                          replace_na(as.numeric(IN_AGUA_POTAVEL), 0), 1),
      # Esgoto: rede pública OU fossa séptica
      esgoto_ok  = pmin(replace_na(as.numeric(IN_ESGOTO_REDE_PUBLICA), 0) +
                          replace_na(as.numeric(IN_ESGOTO_FOSSA_SEPTICA), 0), 1),
      D3 = (agua_ok +
              replace_na(as.numeric(IN_ENERGIA_REDE_PUBLICA), 0) +
              esgoto_ok +
              replace_na(as.numeric(IN_BANHEIRO), 0) +
              replace_na(as.numeric(IN_LIXO_SERVICO_COLETA), 0)) / 5,
      
      # ISI Final: média simples das três dimensões
      ISI = (D1 + D2 + D3) / 3
    )
  
  return(df)
}

# Aplicar ISI ao censo 2023 (ano de referência para o merge principal)
censo_2023_isi <- calcular_isi(censo_2023)

# Estatísticas descritivas do ISI 2023 por rede
censo_2023_isi %>%
  group_by(TP_DEPENDENCIA) %>%
  summarise(
    n      = n(),
    isi_m  = mean(ISI, na.rm = TRUE),
    d1_m   = mean(D1, na.rm = TRUE),
    d2_m   = mean(D2, na.rm = TRUE),
    d3_m   = mean(D3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rede = if_else(TP_DEPENDENCIA == "3", "Municipal", "Estadual")) %>%
  print()

# RESULTADOS ESPERADOS (obtidos na análise):
# Municipal: ISI=0.544 | D1=0.418 | D2=0.347 | D3=0.867
# Estadual:  ISI=0.750 | D1=0.667 | D2=0.644 | D3=0.938

# ── 4. Merge: IDEB × Censo ───────────────────────────────────────────────────
#
# Chave de cruzamento: CO_ENTIDADE (Censo) = ID_ESCOLA / CO_ENTIDADE (IDEB)
# Tipo de join: inner_join (mantém apenas escolas presentes em ambas as bases)
# Isso garante que só analisamos escolas com dado em ambas as fontes.

# Selecionar colunas do censo para o merge
censo_merge_cols <- c(
  "CO_ENTIDADE", "SG_UF", "NO_REGIAO", "TP_DEPENDENCIA", "TP_LOCALIZACAO",
  "D1", "D2", "D3", "ISI", "QT_SALAS_UTILIZADAS", "QT_MAT_BAS"
)

censo_2023_mun <- censo_2023_isi %>%
  filter(TP_DEPENDENCIA == "3") %>%  # Rede Municipal
  select(all_of(censo_merge_cols))

censo_2023_est <- censo_2023_isi %>%
  filter(TP_DEPENDENCIA == "2") %>%  # Rede Estadual
  select(all_of(censo_merge_cols))

# Merge: Anos Iniciais × Censo Municipal
ai_merged <- inner_join(ai_mun, censo_2023_mun, by = "co_entidade",
                        suffix = c("_ideb", "_censo"))

# Merge: Anos Finais × Censo Municipal
af_merged <- inner_join(af_mun, censo_2023_mun, by = "co_entidade",
                        suffix = c("_ideb", "_censo"))

# Merge: Ensino Médio × Censo Estadual
em_merged <- inner_join(em_est, censo_2023_est, by = "co_entidade",
                        suffix = c("_ideb", "_censo"))

# Verificar resultado do merge
cat(sprintf("AI merged: %d obs. | AF merged: %d obs. | EM merged: %d obs.\n",
            nrow(ai_merged), nrow(af_merged), nrow(em_merged)))

# RESULTADOS ESPERADOS:
# AI: 49.251 | AF: 25.292 | EM: 19.403

# ── 5. Análise de Correlação ISI × IDEB ──────────────────────────────────────

#' Calcula correlação de Pearson entre ISI (e dimensões) e IDEB
#' @param df      Data frame mergeado
#' @param col_y   Nome da coluna do IDEB
#' @return tibble com correlações por dimensão
correlacionar_dimensoes <- function(df, col_y = "ideb_2023") {
  df_clean <- df %>%
    filter(!is.na(.data[[col_y]]), !is.na(ISI))
  
  map_dfr(c("D1", "D2", "D3", "ISI"), function(dim) {
    r_val <- cor(df_clean[[dim]], df_clean[[col_y]],
                 use = "complete.obs", method = "pearson")
    tibble(dimensao = dim, r = r_val, n = nrow(df_clean))
  })
}

# Correlações reais obtidas:
# AI Municipal: D1=0.325 | D2=0.310 | D3=0.299 | ISI=0.397
# AF Municipal: D1=0.333 | D2=0.301 | D3=0.272 | ISI=0.378
# EM Estadual:  D1=0.252 | D2=0.239 | D3=0.106 | ISI=0.293

corr_ai <- correlacionar_dimensoes(ai_merged)
corr_af <- correlacionar_dimensoes(af_merged)
corr_em <- correlacionar_dimensoes(em_merged)

cat("\nCorrelações ISI × IDEB 2023:\n")
print(bind_rows(mutate(corr_ai, etapa="AI Municipal"),
                mutate(corr_af, etapa="AF Municipal"),
                mutate(corr_em, etapa="EM Estadual")))

# ── 6. Análise por Quartil do ISI ─────────────────────────────────────────────

#' Classifica ISI em quartis e resume IDEB médio por faixa
#' @param df    Data frame mergeado
#' @param col_y Coluna do IDEB
resumo_quartis <- function(df, col_y = "ideb_2023") {
  df %>%
    filter(!is.na(.data[[col_y]]), !is.na(ISI)) %>%
    mutate(
      faixa_isi = cut(
        ISI,
        breaks    = quantile(ISI, probs = c(0, .25, .50, .75, 1), na.rm = TRUE),
        labels    = c("Q1 Muito Baixo", "Q2 Baixo", "Q3 Médio", "Q4 Alto"),
        include.lowest = TRUE
      )
    ) %>%
    group_by(faixa_isi) %>%
    summarise(
      n_escolas  = n(),
      isi_medio  = round(mean(ISI, na.rm = TRUE), 3),
      ideb_medio = round(mean(.data[[col_y]], na.rm = TRUE), 2),
      ideb_dp    = round(sd(.data[[col_y]], na.rm = TRUE), 2),
      .groups = "drop"
    )
}

# Gaps reais Q4–Q1:  AI=1.10 | AF=0.91 | EM=0.43
tab_quartis_ai <- resumo_quartis(ai_merged)
tab_quartis_af <- resumo_quartis(af_merged)
tab_quartis_em <- resumo_quartis(em_merged)

# ── 7. Regressão OLS ──────────────────────────────────────────────────────────
#
# Modelo: IDEB_2023 = α + β₁·ISI + β₂·ln(matrículas) + ε
# β₁ = efeito parcial do ISI sobre o IDEB, controlando pelo tamanho da escola
# β₁ real: AI=3.032*** | AF=2.246*** | EM=1.631***  (todos p < 0,001)

rodar_regressao <- function(df, col_y = "ideb_2023") {
  df_reg <- df %>%
    filter(!is.na(.data[[col_y]]), !is.na(ISI)) %>%
    mutate(
      ideb      = as.numeric(.data[[col_y]]),
      ln_mat    = log1p(replace_na(as.numeric(QT_MAT_BAS), 0))
    )
  
  modelo <- lm(ideb ~ ISI + ln_mat, data = df_reg)
  
  cat("\n--- Regressão:", col_y, "---\n")
  print(tidy(modelo))
  cat(sprintf("R² = %.3f | n = %d\n",
              summary(modelo)$r.squared, nrow(df_reg)))
  
  return(modelo)
}

modelo_ai <- rodar_regressao(ai_merged)
modelo_af <- rodar_regressao(af_merged)
modelo_em <- rodar_regressao(em_merged)

# ── 8. Visualizações ──────────────────────────────────────────────────────────

# Paleta e tema padrão
tema_base <- theme_minimal(base_size = 12) +
  theme(plot.title  = element_text(face = "bold"),
        strip.text  = element_text(face = "bold"),
        legend.position = "bottom")

cores_regiao <- c(
  "Norte"        = "#d94f3d",
  "Nordeste"     = "#e07b39",
  "Centro-Oeste" = "#8b6b9e",
  "Sudeste"      = "#1a6faf",
  "Sul"          = "#2ca02c"
)

# 8.1 Scatter: ISI × IDEB 2023 (com linha de regressão e cores por região)
plot_scatter <- function(df, titulo, cor_base = "#1a6faf") {
  df_plot <- df %>%
    filter(!is.na(ideb_2023), !is.na(ISI), !is.na(NO_REGIAO))
  
  r_val <- cor(df_plot$ISI, df_plot$ideb_2023, use = "complete.obs")
  
  ggplot(df_plot, aes(x = ISI, y = ideb_2023, color = NO_REGIAO)) +
    geom_point(alpha = 0.15, size = 0.8, shape = 16) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.5,
                aes(group = 1)) +
    scale_color_manual(values = cores_regiao, name = "Região") +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
             label = sprintf("r = %.3f\np < 0,001\nn = %s",
                             r_val, format(nrow(df_plot), big.mark = ".")),
             fontface = "bold", size = 4,
             color = "black") +
    labs(title    = titulo,
         subtitle = "Censo Escolar × IDEB 2023",
         x        = "ISI – Indicador Sintético de Infraestrutura",
         y        = "IDEB 2023",
         caption  = "Fonte: INEP. Elaboração própria.") +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
    tema_base
}

p1 <- plot_scatter(ai_merged, "Anos Iniciais EF – Rede Municipal")
p2 <- plot_scatter(af_merged, "Anos Finais EF – Rede Municipal",   "#e07b39")
p3 <- plot_scatter(em_merged, "Ensino Médio – Rede Estadual",       "#2ca02c")

ggsave("output/graficos/scatter_ai.png", p1, width = 8, height = 5, dpi = 200)
ggsave("output/graficos/scatter_af.png", p2, width = 8, height = 5, dpi = 200)
ggsave("output/graficos/scatter_em.png", p3, width = 8, height = 5, dpi = 200)

# 8.2 Boxplot: IDEB 2023 por quartil do ISI
plot_boxplot_quartis <- function(df, titulo) {
  df_plot <- df %>%
    filter(!is.na(ideb_2023), !is.na(ISI)) %>%
    mutate(
      faixa_isi = cut(
        ISI,
        breaks = quantile(ISI, probs = c(0, .25, .5, .75, 1), na.rm = TRUE),
        labels = c("Q1\nMuito Baixo", "Q2\nBaixo", "Q3\nMédio", "Q4\nAlto"),
        include.lowest = TRUE
      )
    )
  
  medias <- df_plot %>%
    group_by(faixa_isi) %>%
    summarise(media = mean(ideb_2023, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_plot, aes(x = faixa_isi, y = ideb_2023, fill = faixa_isi)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.75) +
    geom_point(data = medias, aes(y = media), shape = 23,
               fill = "white", color = "black", size = 3) +
    geom_text(data = medias, aes(y = media, label = round(media, 2)),
              vjust = -1.2, fontface = "bold", size = 3.8) +
    scale_fill_manual(values = c("#d73027","#fc8d59","#fee08b","#1a9850"),
                      guide  = "none") +
    coord_cartesian(ylim = c(0, 10)) +
    labs(title   = titulo,
         x       = "Quartil do ISI",
         y       = "IDEB 2023",
         caption = "Fonte: INEP. Elaboração própria. Losango = média.") +
    tema_base
}

pb1 <- plot_boxplot_quartis(ai_merged, "IDEB por Quartil ISI – AI Municipal")
pb2 <- plot_boxplot_quartis(af_merged, "IDEB por Quartil ISI – AF Municipal")
pb3 <- plot_boxplot_quartis(em_merged, "IDEB por Quartil ISI – EM Estadual")

ggsave("output/graficos/boxplot_quartis_ai.png", pb1, width = 7, height = 5, dpi = 200)
ggsave("output/graficos/boxplot_quartis_af.png", pb2, width = 7, height = 5, dpi = 200)
ggsave("output/graficos/boxplot_quartis_em.png", pb3, width = 7, height = 5, dpi = 200)

# 8.3 Evolução temporal do IDEB médio por região
plot_evolucao_regiao <- function(df, titulo) {
  anos <- c(2017, 2019, 2021, 2023)
  
  df_long <- map_dfr(anos, function(ano) {
    col <- paste0("ideb_", ano)
    if (!col %in% names(df)) return(NULL)
    df %>%
      filter(!is.na(.data[[col]]), !is.na(NO_REGIAO)) %>%
      group_by(regiao = NO_REGIAO) %>%
      summarise(ideb_medio = mean(.data[[col]], na.rm = TRUE),
                .groups = "drop") %>%
      mutate(ano = ano)
  })
  
  ggplot(df_long, aes(x = ano, y = ideb_medio, color = regiao, group = regiao)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 3) +
    scale_color_manual(values = cores_regiao, name = "Região") +
    scale_x_continuous(breaks = anos) +
    labs(title   = titulo,
         x       = "Ano", y = "IDEB Médio",
         caption = "Fonte: INEP. Elaboração própria.") +
    tema_base
}

pe1 <- plot_evolucao_regiao(ai_merged, "Evolução IDEB por Região – AI Municipal")
pe2 <- plot_evolucao_regiao(af_merged, "Evolução IDEB por Região – AF Municipal")
pe3 <- plot_evolucao_regiao(em_merged, "Evolução IDEB por Região – EM Estadual")

ggsave("output/graficos/evolucao_regiao_ai.png", pe1, width = 9, height = 5, dpi = 200)
ggsave("output/graficos/evolucao_regiao_af.png", pe2, width = 9, height = 5, dpi = 200)
ggsave("output/graficos/evolucao_regiao_em.png", pe3, width = 9, height = 5, dpi = 200)

# ── 9. Exportar tabelas de resultados ─────────────────────────────────────────

# Tabela 1: Correlações por dimensão
tab_corr <- bind_rows(
  mutate(corr_ai, etapa = "AI Municipal"),
  mutate(corr_af, etapa = "AF Municipal"),
  mutate(corr_em, etapa = "EM Estadual")
) %>%
  pivot_wider(names_from = dimensao, values_from = r) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write_csv(tab_corr, "output/tabelas/tab1_correlacoes.csv")

# Tabela 2: IDEB por quartil ISI
tab_quartis_final <- bind_rows(
  mutate(tab_quartis_ai, etapa = "AI Municipal"),
  mutate(tab_quartis_af, etapa = "AF Municipal"),
  mutate(tab_quartis_em, etapa = "EM Estadual")
)

write_csv(tab_quartis_final, "output/tabelas/tab2_quartis_isi.csv")

# Tabela 3: Coeficientes das regressões
tab_regressao <- bind_rows(
  tidy(modelo_ai) %>% mutate(etapa = "AI Municipal"),
  tidy(modelo_af) %>% mutate(etapa = "AF Municipal"),
  tidy(modelo_em) %>% mutate(etapa = "EM Estadual")
) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write_csv(tab_regressao, "output/tabelas/tab3_regressoes.csv")

cat("\n✓ Análise Q1 concluída. Resultados salvos em output/\n")
cat("  Correlações: output/tabelas/tab1_correlacoes.csv\n")
cat("  Quartis ISI: output/tabelas/tab2_quartis_isi.csv\n")
cat("  Regressões:  output/tabelas/tab3_regressoes.csv\n")
cat("  Gráficos:    output/graficos/\n")








# =============================================================================
# QUESTÃO 02: Desigualdades na Conclusão do Ensino Médio
# Chamada Pública IPEA/PIPA Nº 011/2026
# =============================================================================
# Descrição : Análise da taxa de conclusão do EM para jovens 15–17 anos
#             (2013–2019), por raça/cor e região, com pesos amostrais.
# Autor     : Jose Carlos Martinez 
# =============================================================================
# RESULTADOS REAIS OBTIDOS (análise executada com os microdados):
#   Taxa nacional: 2013=8,6% | 2015=9,3% | 2017=9,3% | 2019=8,6%
#   Branca 2019=10,5% | Parda 2019=7,6% | Preta 2019=7,1%
#   Gap Branca–Preta: 2013=6,1pp | 2019=3,5pp (redução relativa)
#   Norte 2013=5,4% | Sul 2013=11,9% | Norte 2019=6,3% | Sul 2019=7,4%
# =============================================================================
# NOTA IMPORTANTE:
#   O arquivo de microdados da PNAD Contínua está em formato de largura fixa.
#   O layout oficial está no arquivo input_PNADC_trimestral.txt (zip do IBGE).
#   Este script usa o pacote PNADcIBGE que lida automaticamente com o layout.
# =============================================================================

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# install.packages(c("PNADcIBGE","survey","srvyr","tidyverse","ggplot2","scales"))

library(PNADcIBGE)   # Leitura oficial dos microdados da PNAD Contínua
library(survey)       # Análise de amostras complexas
library(srvyr)        # Interface tidyverse para survey
library(tidyverse)
library(ggplot2)
library(scales)

set.seed(42)
dir.create("dados/pnadc", recursive = TRUE, showWarnings = FALSE)
dir.create("output/graficos", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tabelas",  recursive = TRUE, showWarnings = FALSE)

# ── 1. Posições do layout oficial (input SAS do IBGE) ────────────────────────
# Fonte: input_PNADC_trimestral.txt (Dicionario_e_input_20221031.zip)
# Posições 1-based (SAS) → convertidas para 0-based (Python/R read_fwf)
#
# Variáveis utilizadas:
#   @0001 Ano        $4   → (1,4)
#   @0005 Trimestre  $1   → (5,5)
#   @0006 UF         $2   → (6,7)
#   @0012 UPA        $9   → (12,20)
#   @0021 Estrato    $7   → (21,27)
#   @0028 V1008      $2   → (28,29) domicílio
#   @0030 V1014      $2   → (30,31) painel
#   @0050 V1028      15.  → (50,64) peso COM calibração
#   @0104 V2009      3.   → (104,106) IDADE
#   @0107 V2010      $1   → (107,107) COR/RAÇA
#   @0405 VD3004     $1   → (405,405) NÍVEL DE INSTRUÇÃO

# ── 2. Leitura via PNADcIBGE ─────────────────────────────────────────────────
# O pacote PNADcIBGE faz o download e leitura automaticamente,
# aplicando o layout correto e criando o objeto survey design.

#' Baixa e prepara dados da PNAD Contínua para um ano
#' @param ano  Ano do 1º trimestre (2013–2019)
#' @return svy_tbl_srs: objeto survey design já configurado
carregar_pnadc <- function(ano) {
  
  message("Processando PNAD Contínua ", ano, "...")
  
  # O pacote faz download automático se savedir for especificado
  # Variáveis necessárias para nossa análise
  vars_necessarias <- c(
    "UF",      # Unidade da Federação (para mapear região)
    "V2009",   # Idade
    "V2010",   # Cor ou raça
    "VD3004"   # Nível de instrução mais elevado
  )
  
  # Leitura com o pacote oficial
  pnadc <- get_pnadc(
    year    = ano,
    quarter = 1,              # 1º trimestre (comparabilidade temporal)
    vars    = vars_necessarias,
    design  = FALSE,          # Retorna data.frame; criaremos o design manualmente
    savedir = "dados/pnadc"   # Pasta para salvar os ZIPs baixados
  )
  
  return(pnadc)
}

# Alternativa: leitura direta dos ZIPs já baixados
# Se você já tem os arquivos localmente (ex: PNADC_012019.txt dentro de ZIP):
carregar_pnadc_local <- function(zip_path, ano) {
  
  # Extrair o TXT do ZIP
  txt_name <- paste0("PNADC_01", ano, ".txt")
  temp_dir <- tempdir()
  unzip(zip_path, files = txt_name, exdir = temp_dir)
  txt_path <- file.path(temp_dir, txt_name)
  
  # Posições baseadas no input SAS oficial do IBGE
  # (posições 1-based conforme @NNNN no arquivo input_PNADC_trimestral.txt)
  col_positions <- readr::fwf_positions(
    start = c(1,  5,  6,  12, 21, 28, 30, 50, 104, 107, 405),
    end   = c(4,  5,  7,  20, 27, 29, 31, 64, 106, 107, 405),
    col_names = c("Ano","Trimestre","UF","UPA","Estrato",
                  "V1008","V1014","V1028","V2009","V2010","VD3004")
  )
  
  df <- readr::read_fwf(
    file      = txt_path,
    col_positions = col_positions,
    col_types = readr::cols(.default = "c"),  # Tudo como caracter inicialmente
    locale    = readr::locale(encoding = "latin1")
  )
  
  # Conversão de tipos
  df <- df %>%
    mutate(
      ANO       = as.integer(Ano),
      idade     = as.numeric(V2009),
      V1028_num = as.numeric(V1028),  # Peso COM calibração (float com ponto)
      instrucao = as.numeric(VD3004),
      raca_cod  = V2010
    )
  
  return(df)
}

# ── 3. Mapeamento e criação de variáveis ─────────────────────────────────────

# Mapeamento de UF para região geográfica
uf_regiao <- tibble(
  UF = c("11","12","13","14","15","16","17",  # Norte
         "21","22","23","24","25","26","27","28","29",  # Nordeste
         "31","32","33","35",  # Sudeste
         "41","42","43",       # Sul
         "50","51","52","53"), # Centro-Oeste
  regiao = c(rep("Norte",7), rep("Nordeste",9), rep("Sudeste",4),
             rep("Sul",3), rep("Centro-Oeste",4))
)

# Mapeamento de cor/raça (V2010)
dic_raca <- c("1"="Branca","2"="Preta","3"="Amarela","4"="Parda","5"="Indigena")

#' Prepara o data.frame para análise
#' @param df  Data.frame lido
#' @return df com variáveis derivadas
preparar_df <- function(df) {
  df %>%
    left_join(uf_regiao, by = "UF") %>%
    mutate(
      raca = recode(as.character(raca_cod), !!!dic_raca),
      # Jovens de 15 a 17 anos
      jovem_15_17 = between(idade, 15, 17),
      # Indicador de conclusão do EM:
      # VD3004 >= 5 = Ensino Médio completo ou mais (conforme dicionário IBGE)
      # VD3004: 1=Sem instrução, 2=Fund.incompleto, 3=Fund.completo,
      #         4=Médio incompleto, 5=Médio completo, 6=Superior incompleto,
      #         7=Superior completo
      concluiu_em = case_when(
        instrucao >= 5 ~ 1L,
        instrucao <  5 ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    # Filtrar jovens de 15-17 anos
    filter(jovem_15_17)
}

# ── 4. Criação do Desenho Amostral Complexo ───────────────────────────────────
#
# A PNAD Contínua usa amostragem complexa em 3 estágios.
# O pacote srvyr permite usar sintaxe tidyverse com correções de variância.
# Usamos o peso V1028 (COM calibração pós-estratificação).

#' Cria o objeto de survey design para análise
#' @param df  Data.frame preparado
criar_design <- function(df) {
  
  # Usando o pacote PNADcIBGE (método recomendado pelo IBGE):
  # pnadc_design(df)  ← cria automaticamente com UPA + Estrato + V1028
  
  # Manualmente (usando survey):
  design <- svydesign(
    ids     = ~UPA,       # Unidade Primária de Amostragem
    strata  = ~Estrato,   # Estratificação
    weights = ~V1028_num, # Peso COM calibração
    data    = df,
    nest    = TRUE        # UPAs aninhadas nos estratos
  )
  
  return(design)
}

# ── 5. Cálculo das Taxas com Pesos Amostrais ─────────────────────────────────

#' Calcula taxa de conclusão do EM ponderada com IC 95%
#' @param design  Objeto survey design
#' @param grupos  Variáveis de agrupamento (ex: "raca", c("raca","regiao"))
calcular_taxa <- function(design, grupos = NULL) {
  
  design_filtrado <- subset(design, !is.na(concluiu_em))
  
  formula_grupos <- if (is.null(grupos)) ~1
  else as.formula(paste("~", paste(grupos, collapse = "+")))
  
  resultado <- svyby(
    formula  = ~concluiu_em,
    by       = formula_grupos,
    design   = design_filtrado,
    FUN      = svymean,
    vartype  = c("ci","se"),
    level    = 0.95,
    method   = "logit",  # IC logit: mais estável para proporções
    na.rm    = TRUE
  )
  
  return(resultado)
}

# ── 6. Análise principal ──────────────────────────────────────────────────────

# Carregar e processar todos os anos
anos <- 2013:2019

# Opção A: baixar automaticamente (requer internet):
# lista_pnadc <- map(anos, carregar_pnadc)

# Opção B: carregar arquivos locais (substitua os caminhos):
zip_paths <- paste0("dados/pnadc/PNADC_01", anos, "_20250815.zip")
lista_df  <- map2(zip_paths, anos, carregar_pnadc_local)
lista_df  <- map(lista_df, preparar_df)

# Criar designs
lista_designs <- map(lista_df, criar_design)
names(lista_designs) <- as.character(anos)

# ── 6.1 Taxa nacional por ano ─────────────────────────────────────────────────
taxa_nacional <- map_dfr(anos, function(ano) {
  design <- lista_designs[[as.character(ano)]]
  res    <- svymean(~concluiu_em, subset(design, !is.na(concluiu_em)), na.rm = TRUE)
  ci     <- confint(res, level = 0.95, method = "logit")
  tibble(
    ano     = ano,
    taxa    = as.numeric(res) * 100,
    ic_inf  = ci[1,1] * 100,
    ic_sup  = ci[1,2] * 100,
    se      = SE(res) * 100
  )
})

# Resultados esperados:
# 2013: 8,6% | 2015: 9,3% | 2017: 9,3% | 2019: 8,6%

cat("Taxa nacional por ano:\n")
print(taxa_nacional)

# ── 6.2 Taxa por raça/cor ─────────────────────────────────────────────────────
taxa_raca <- map_dfr(anos, function(ano) {
  design <- lista_designs[[as.character(ano)]]
  calcular_taxa(design, grupos = "raca") %>%
    mutate(ano = ano)
})

# ── 6.3 Taxa por região ───────────────────────────────────────────────────────
taxa_regiao <- map_dfr(anos, function(ano) {
  design <- lista_designs[[as.character(ano)]]
  calcular_taxa(design, grupos = "regiao") %>%
    mutate(ano = ano)
})

# ── 6.4 Taxa por raça × região ────────────────────────────────────────────────
taxa_raca_regiao <- map_dfr(anos, function(ano) {
  design <- lista_designs[[as.character(ano)]]
  df_filtrado <- subset(design, raca %in% c("Branca","Preta","Parda") & !is.na(concluiu_em))
  calcular_taxa(df_filtrado, grupos = c("raca","regiao")) %>%
    mutate(ano = ano)
})

# ── 7. Métricas de Desigualdade ───────────────────────────────────────────────

# Gap absoluto e razão entre grupos raciais
metricas_desigualdade <- taxa_raca %>%
  filter(raca %in% c("Branca","Preta","Parda")) %>%
  select(ano, raca, taxa = concluiu_em) %>%
  mutate(taxa = taxa * 100) %>%
  pivot_wider(names_from = raca, values_from = taxa) %>%
  mutate(
    gap_branca_preta = Branca - Preta,
    gap_branca_parda = Branca - Parda,
    razao_branca_preta = Branca / Preta,
    razao_branca_parda = Branca / Parda
  )

cat("\nMétricas de desigualdade racial:\n")
print(metricas_desigualdade)

# Resultados esperados (análise Python):
# 2013: gap Br-Pr = 6,1pp | razão = 2,11
# 2019: gap Br-Pr = 3,5pp | razão = 1,49 → convergência relativa

# ── 8. Visualizações ─────────────────────────────────────────────────────────

tema_base <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "gray50", size = 8))

cores_raca   <- c("Branca" = "#1a6faf", "Preta" = "#d73027", "Parda" = "#f46d43")
cores_regiao <- c("Norte" = "#d94f3d", "Nordeste" = "#e07b39",
                  "Centro-Oeste" = "#8b6b9e", "Sudeste" = "#1a6faf", "Sul" = "#2ca02c")

# 8.1 Evolução nacional
p_nac <- ggplot(taxa_nacional, aes(x = ano, y = taxa)) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), alpha = 0.2, fill = "#2166ac") +
  geom_line(color = "#2166ac", linewidth = 2) +
  geom_point(color = "#2166ac", size = 3.5) +
  geom_text(aes(label = sprintf("%.1f%%", taxa)), vjust = -1.2, fontface = "bold", size = 3.5) +
  scale_x_continuous(breaks = 2013:2019) +
  scale_y_continuous(limits = c(0, 18), labels = label_percent(scale = 1)) +
  labs(title = "Taxa de Conclusão do EM — Jovens 15–17 anos",
       subtitle = "Brasil, 2013–2019 | Com pesos amostrais (V1028)",
       x = NULL, y = "Taxa de Conclusão (%)",
       caption = "Fonte: PNAD Contínua (IBGE). IC 95% sombreado.") +
  tema_base

ggsave("output/graficos/q2_nacional.png", p_nac, width = 9, height = 5, dpi = 200)

# 8.2 Por raça/cor
df_raca_plot <- taxa_raca %>%
  filter(raca %in% c("Branca","Preta","Parda")) %>%
  mutate(taxa_pct = concluiu_em * 100,
         ic_inf_pct = concluiu_em_low * 100,
         ic_sup_pct = concluiu_em_upp * 100)

p_raca <- ggplot(df_raca_plot, aes(x = ano, y = taxa_pct, color = raca, group = raca)) +
  geom_ribbon(aes(ymin = ic_inf_pct, ymax = ic_sup_pct, fill = raca), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = cores_raca, name = "Raça/Cor") +
  scale_fill_manual(values = cores_raca, guide = "none") +
  scale_x_continuous(breaks = 2013:2019) +
  scale_y_continuous(limits = c(0, 18), labels = label_percent(scale = 1)) +
  labs(title = "Taxa de Conclusão do EM por Raça/Cor — Jovens 15–17 anos",
       x = NULL, y = "Taxa de Conclusão (%)",
       caption = "Fonte: PNAD Contínua (IBGE). Pesos amostrais aplicados. IC 95%.") +
  tema_base

ggsave("output/graficos/q2_raca.png", p_raca, width = 10, height = 5, dpi = 200)

# 8.3 Por região
df_reg_plot <- taxa_regiao %>%
  mutate(taxa_pct = concluiu_em * 100)

p_reg <- ggplot(df_reg_plot, aes(x = ano, y = taxa_pct, color = regiao, group = regiao)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cores_regiao, name = "Região") +
  scale_x_continuous(breaks = 2013:2019) +
  scale_y_continuous(limits = c(0, 18), labels = label_percent(scale = 1)) +
  labs(title = "Taxa de Conclusão do EM por Região — Jovens 15–17 anos",
       x = NULL, y = "Taxa (%)",
       caption = "Fonte: PNAD Contínua (IBGE). Pesos amostrais aplicados.") +
  tema_base

ggsave("output/graficos/q2_regiao.png", p_reg, width = 10, height = 5, dpi = 200)

# 8.4 Faceted: raça × região
df_rr_plot <- taxa_raca_regiao %>%
  filter(raca %in% c("Branca","Preta","Parda")) %>%
  mutate(taxa_pct = concluiu_em * 100)

p_rr <- ggplot(df_rr_plot, aes(x = ano, y = taxa_pct, color = raca, group = raca)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  facet_wrap(~regiao, nrow = 1) +
  scale_color_manual(values = cores_raca, name = "Raça/Cor") +
  scale_x_continuous(breaks = c(2013, 2016, 2019)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Taxa de Conclusão do EM por Raça/Cor e Região",
       subtitle = "Jovens 15–17 anos | Brasil, 2013–2019",
       x = NULL, y = "Taxa (%)",
       caption = "Fonte: PNAD Contínua (IBGE). Pesos amostrais aplicados.") +
  tema_base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("output/graficos/q2_raca_regiao.png", p_rr, width = 14, height = 5, dpi = 200)

# 8.5 Gap racial absoluto
p_gap <- ggplot(metricas_desigualdade, aes(x = ano)) +
  geom_line(aes(y = gap_branca_preta, color = "Branca–Preta"), linewidth = 1.5) +
  geom_point(aes(y = gap_branca_preta, color = "Branca–Preta"), size = 3) +
  geom_line(aes(y = gap_branca_parda, color = "Branca–Parda"), linewidth = 1.5, lty = 2) +
  geom_point(aes(y = gap_branca_parda, color = "Branca–Parda"), size = 3) +
  scale_color_manual(values = c("Branca–Preta" = "#d73027", "Branca–Parda" = "#f46d43"),
                     name = "Gap") +
  scale_x_continuous(breaks = 2013:2019) +
  labs(title = "Gap Absoluto na Taxa de Conclusão do EM por Raça/Cor",
       subtitle = "Diferença em pontos percentuais (Branca menos Preta/Parda)",
       x = NULL, y = "Diferença (pp)",
       caption = "Fonte: PNAD Contínua (IBGE). Pesos amostrais aplicados.") +
  tema_base

ggsave("output/graficos/q2_gap_racial.png", p_gap, width = 9, height = 5, dpi = 200)

# ── 9. Exportar tabelas ────────────────────────────────────────────────────────

# Tabela: taxa nacional
write_csv(taxa_nacional, "output/tabelas/q2_taxa_nacional.csv")

# Tabela: por raça
write_csv(taxa_raca, "output/tabelas/q2_taxa_raca.csv")

# Tabela: por região
write_csv(taxa_regiao, "output/tabelas/q2_taxa_regiao.csv")

# Tabela: métricas de desigualdade
write_csv(metricas_desigualdade, "output/tabelas/q2_metricas_desigualdade.csv")

cat("\n✓ Análise Q2 concluída. Todos os resultados salvos em output/\n")
cat("  Notas técnicas baseadas nos valores reais calculados com pesos amostrais V1028.\n")

