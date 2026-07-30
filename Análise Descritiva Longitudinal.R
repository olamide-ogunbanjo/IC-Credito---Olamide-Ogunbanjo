#ANÁLISE DESCRITIVA LONGITUDINAL - MERCADO DE CRÉDITO (2015-2025)
# Script separado dos modelos de regressão (Modelo 1 e 2), para organização.
# Mistura duas fontes: (a) séries do SGS via API (GetBCBData) e (b) dados
# extraídos manualmente de PDFs sem API (REF, RCF, Dashboard Open Finance)

#install.packages(c("GetBCBData","dplyr","lubridate","ggplot2","tidyr","scales","readxl"))
library("GetBCBData")
library("dplyr")
library("lubridate")
library("ggplot2")
library("tidyr")
library("scales")

# --- Paleta e tema padrão para manter consistência visual entre os gráficos ---
tema_padrao <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Marcos temporais de referência (usados como linhas verticais nos gráficos)
marco_cadastro_positivo <- as.Date("2019-01-01")
marco_open_finance <- as.Date("2021-02-01")
covid_inicio <- as.Date("2020-03-01")
covid_fim <- as.Date("2021-12-01")

# Subperíodos de comparação (usados nas tabelas-síntese) - versão para dados MENSAIS (com data)
definir_subperiodo <- function(data) {
  case_when(
    data < marco_cadastro_positivo ~ "2015-2018 (pré-Cadastro Positivo)",
    data < marco_open_finance      ~ "2019-2021 (transição/pandemia)",
    TRUE                            ~ "2022-2025 (pós-Open Finance)"
  )
}

# Versão equivalente para dados ANUAIS (séries extraídas de PDF, sem ref.date mensal)
definir_subperiodo_ano <- function(ano) {
  case_when(
    ano <= 2018 ~ "2015-2018 (pré-Cadastro Positivo)",
    ano <= 2021 ~ "2019-2021 (transição/pandemia)",
    TRUE        ~ "2022-2025 (pós-Open Finance)"
  )
}


# ============================================================
# 2) VOLUME E COMPOSIÇÃO DO CRÉDITO
# ============================================================

#2.1) Saldo total da carteira e concessões mensais (já usados nos modelos)
saldo_credito <- gbcbd_get_series(20539, first.date = '2015-01-01', last.date = '2025-12-01') # saldo carteira total
concessoes    <- gbcbd_get_series(24439, first.date = '2015-01-01', last.date = '2025-12-01') # concessões dessaz.

#2.2) Crédito/PIB — Painel de Estatísticas Monetárias e de Crédito
credito_pib <- gbcbd_get_series(20622, first.date = '2015-01-01', last.date = '2025-12-01')

#2.3) Saldo por modalidade (pessoa física)
modalidades_ids <- c(
  consignado      = 20671,
  cartao_rotativo = 20679,
  cheque_especial = 20665,
  pessoal_nao_consig = 20666,
  imobiliario     = 20704
)
modalidades <- gbcbd_get_series(modalidades_ids, first.date = '2015-01-01', last.date = '2025-12-01')

# Gráfico 2.1 - Evolução do saldo total + concessões, com marcos temporais
grafico_volume <- ggplot(saldo_credito, aes(x = ref.date, y = value)) +
  geom_line(color = "#1F3864", linewidth = 0.8) +
  geom_vline(xintercept = marco_cadastro_positivo, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = marco_open_finance, linetype = "dashed", color = "grey40") +
  annotate("rect", xmin = covid_inicio, xmax = covid_fim, ymin = -Inf, ymax = Inf,
           alpha = 0.1, fill = "red") +
  labs(title = "Saldo Total da Carteira de Crédito (2015-2025)",
       x = NULL, y = "R$ milhões") +
  tema_padrao
# ggsave("grafico_2_1_volume_credito.png", grafico_volume, width = 9, height = 5)

# Gráfico 2.2 - Composição por modalidade (área empilhada)
modalidades_nomeadas <- modalidades %>%
  mutate(modalidade = names(modalidades_ids)[match(id.num, modalidades_ids)],
         modalidade = recode(modalidade,
                             consignado         = "Consignado",
                             cartao_rotativo     = "Cartão Rotativo",
                             cheque_especial     = "Cheque Especial",
                             pessoal_nao_consig  = "Pessoal (não consignado)",
                             imobiliario         = "Imobiliário"
         ))

grafico_modalidades <- ggplot(modalidades_nomeadas, aes(x = ref.date, y = value, fill = modalidade)) +
  geom_area(alpha = 0.85, position = "stack") +
  geom_vline(xintercept = marco_cadastro_positivo, linetype = "dashed", color = "grey20") +
  geom_vline(xintercept = marco_open_finance, linetype = "dashed", color = "grey20") +
  scale_fill_manual(values = c("#1F3864", "#8FAADC", "#C00000", "#BFBFBF", "#2E7D32")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Composição do Saldo de Crédito por Modalidade (Pessoa Física)",
       x = NULL, y = "R$ milhões", fill = NULL) +
  tema_padrao

grafico_modalidades
# ggsave("grafico_2_2_composicao_modalidades.png", grafico_modalidades, width = 9, height = 5)

# Versão complementar em % (composição relativa, independente do crescimento do total)
grafico_modalidades_pct <- modalidades_nomeadas %>%
  group_by(ref.date) %>%
  mutate(participacao_pct = value / sum(value, na.rm = TRUE) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = ref.date, y = participacao_pct, fill = modalidade)) +
  geom_area(alpha = 0.85, position = "stack") +
  geom_vline(xintercept = marco_cadastro_positivo, linetype = "dashed", color = "grey20") +
  geom_vline(xintercept = marco_open_finance, linetype = "dashed", color = "grey20") +
  scale_fill_manual(values = c("#1F3864", "#8FAADC", "#C00000", "#BFBFBF", "#2E7D32")) +
  labs(title = "Participação Relativa das Modalidades no Crédito PF (%)",
       x = NULL, y = "% do saldo total (das 5 modalidades)", fill = NULL) +
  tema_padrao

grafico_modalidades_pct
# ggsave("grafico_2_2b_composicao_modalidades_pct.png", grafico_modalidades_pct, width = 9, height = 5)


# ============================================================
# 3) CUSTO DO CRÉDITO
# ============================================================

#3.1) Spread bancário médio — SGS
spread_medio <- gbcbd_get_series(20783, first.date = '2015-01-01', last.date = '2025-12-01') # confirmado: "Spread médio das operações de crédito - Total" (p.p., mensal)

#3.2) Decomposição do ICC (Indicador de Custo do Crédito) e do spread - dado manual, extraído dos PDFs
# Para cada ano, usado o valor da edição MAIS RECENTE que o cobre (cada edição revisa os
# anos anteriores) - ver tabela de correspondência ano->edição no comentário abaixo.
# 2015: REB2017 | 2016: REB2018 | 2017: REB2019 | 2018: REB2020 | 2019: REB2021 |
# 2020: REB2022 | 2021: REB2023 | 2022: REB2024 | 2023-2025: REF2026 (1ª ed., mais recente,
# revisa os valores de 2023-2024 que constavam no REB2024)
# Fonte REB (2015-2024): bcb.gov.br/publicacoes/relatorioeconomiabancaria
# Fonte REF (a partir de 2025): bcb.gov.br/publicacoes/ref (1ª edição de cada ano)
custo_credito_decomposicao <- data.frame(
  ano = 2015:2025,
  custo_captacao_pp  = c(7.72, 8.10, 7.61, 6.82, 6.28, 5.20, 5.15, 6.76, 7.54, 7.53, 8.32),
  inadimplencia_pp    = c(3.94, 4.85, 4.92, 4.19, 3.99, 3.62, 2.97, 3.68, 4.51, 4.16, 4.61),
  despesas_admin_pp   = c(2.81, 3.28, 3.46, 3.69, 3.78, 3.43, 3.31, 3.07, 2.93, 2.90, 2.89),
  tributos_fgc_pp      = c(2.61, 2.51, 2.26, 2.42, 2.51, 2.43, 2.53, 2.57, 2.61, 2.66, 2.59),
  margem_financeira_pp = c(1.82, 1.72, 1.90, 2.05, 2.62, 2.38, 2.26, 2.61, 2.51, 2.59, 2.52),
  icc_total_pp          = c(18.91, 20.46, 20.15, 19.17, 19.18, 17.06, 16.22, 18.69, 20.10, 19.84, 20.93)
)

# Checagem de consistência: soma dos componentes deve bater com icc_total_pp
# (NÃO com 100 - os valores são em p.p., não % do total)
custo_credito_decomposicao <- custo_credito_decomposicao %>%
  mutate(
    soma_componentes = custo_captacao_pp + inadimplencia_pp + despesas_admin_pp +
      tributos_fgc_pp + margem_financeira_pp,
    diferenca_check   = round(icc_total_pp - soma_componentes, 2),
    spread_icc_pp      = icc_total_pp - custo_captacao_pp  # spread = ICC - custo de captação
  )
# print(custo_credito_decomposicao)
# diferenca_check deve ficar próximo de 0; se não, há erro de transcrição

# Gráfico 3.2 - Decomposição do ICC em área empilhada (componentes em p.p., não %)
grafico_decomposicao <- custo_credito_decomposicao %>%
  select(ano, custo_captacao_pp, inadimplencia_pp, despesas_admin_pp,
         tributos_fgc_pp, margem_financeira_pp) %>%
  pivot_longer(-ano, names_to = "componente", values_to = "pontos_percentuais") %>%
  mutate(componente = recode(componente,
                             custo_captacao_pp    = "Custo de Captação",
                             inadimplencia_pp      = "Inadimplência",
                             despesas_admin_pp     = "Despesas Administrativas",
                             tributos_fgc_pp        = "Tributos e FGC",
                             margem_financeira_pp   = "Margem Financeira"
  )) %>%
  ggplot(aes(x = ano, y = pontos_percentuais, fill = componente)) +
  geom_area(alpha = 0.85, position = "stack") +
  geom_vline(xintercept = as.numeric(format(marco_open_finance, "%Y")),
             linetype = "dashed", color = "grey20") +
  scale_fill_manual(values = c("#1F3864", "#8FAADC", "#C00000", "#BFBFBF", "#2E7D32")) +
  labs(title = "Decomposição do Indicador de Custo do Crédito (ICC)",
       x = NULL, y = "Pontos percentuais (p.p.)", fill = NULL) +
  tema_padrao

grafico_decomposicao
# ggsave("grafico_3_2_decomposicao_icc.png", grafico_decomposicao, width = 9, height = 5)

#3.3) Decomposição do ICC em % do ICC médio ajustado (Tabela 2.1.2/3.3 dos relatórios)
# 2015-2022: valores publicados diretamente na tabela em % de cada edição (mesma lógica
#   de "edição mais recente" da seção 3.2)
# 2023-2025: RECALCULADOS a partir da tabela em p.p. (icc_total_pp), e não transcritos
#   diretamente da tabela publicada em % - ao reconferir a imagem da Tabela 2.1.2 (REF 2026),
#   o valor de "Inadimplência" não bateu no teste de soma (~100,5 em vez de 100), sinal de
#   possível erro de transcrição/OCR daquele número específico. Recalcular a partir do p.p.
#   (que já passou na checagem de soma) resolve a inconsistência - mas VALE CONFERIR
#   diretamente no PDF do REF 2026 antes de finalizar, caso quiser usar o valor oficial
#   publicado em vez do recalculado.
custo_credito_decomposicao_pct <- data.frame(
  ano = 2015:2025,
  custo_captacao_pct    = c(40.84, 39.59, 37.77, 35.58, 32.74, 30.48, 31.75, 36.17, 37.51, 37.95, 39.75),
  inadimplencia_pct      = c(20.85, 23.70, 24.42, 21.86, 20.80, 21.22, 18.31, 19.69, 22.44, 20.97, 22.02),
  despesas_admin_pct     = c(14.88, 16.03, 17.17, 19.25, 19.71, 20.11, 20.41, 16.43, 14.58, 14.62, 13.81),
  tributos_fgc_pct        = c(13.81, 12.27, 11.22, 12.62, 13.09, 14.24, 15.60, 13.75, 12.99, 13.41, 12.37),
  margem_financeira_pct   = c(9.61, 8.41, 9.43, 10.69, 13.66, 13.95, 13.93, 13.96, 12.49, 13.05, 12.04)
)

# Checagem: aqui sim a soma deve ficar em ~100 (diferente da tabela em p.p.)
custo_credito_decomposicao_pct <- custo_credito_decomposicao_pct %>%
  mutate(soma_check_pct = round(custo_captacao_pct + inadimplencia_pct +
                                  despesas_admin_pct + tributos_fgc_pct +
                                  margem_financeira_pct, 1))
# print(custo_credito_decomposicao_pct)  # soma_check_pct deve ficar ~100

# Gráfico 3.3 - Composição relativa do ICC (% - mostra mudança de estrutura,
# independente do nível total ter subido ou descido)
grafico_decomposicao_pct <- custo_credito_decomposicao_pct %>%
  select(-soma_check_pct) %>%
  pivot_longer(-ano, names_to = "componente", values_to = "percentual") %>%
  mutate(componente = recode(componente,
                             custo_captacao_pct    = "Custo de Captação",
                             inadimplencia_pct      = "Inadimplência",
                             despesas_admin_pct     = "Despesas Administrativas",
                             tributos_fgc_pct        = "Tributos e FGC",
                             margem_financeira_pct   = "Margem Financeira"
  )) %>%
  ggplot(aes(x = ano, y = percentual, fill = componente)) +
  geom_area(alpha = 0.85, position = "stack") +
  geom_vline(xintercept = as.numeric(format(marco_open_finance, "%Y")),
             linetype = "dashed", color = "grey20") +
  scale_fill_manual(values = c("#1F3864", "#8FAADC", "#C00000", "#BFBFBF", "#2E7D32")) +
  labs(title = "Composição Relativa do ICC (% do ICC médio ajustado)",
       x = NULL, y = "% do ICC", fill = NULL) +
  tema_padrao

grafico_decomposicao_pct
# ggsave("grafico_3_3_decomposicao_icc_pct.png", grafico_decomposicao_pct, width = 9, height = 5)

# Sugestão de leitura conjunta para o texto: usar o gráfico em p.p. (3.2) para discutir
# se o crédito ficou mais caro/barato em nível, e o gráfico em % (3.3) para discutir
# se a estrutura de custos mudou (ex: se inadimplência ganhou peso relativo mesmo que
# o ICC total tenha caído) - as duas leituras juntas contam uma história mais completa
# do que qualquer uma isolada


# ============================================================
# 4) INADIMPLÊNCIA
# ============================================================

#4.1) Série total (já usada nos modelos, aqui em versão puramente descritiva)
inadimplencia_total <- gbcbd_get_series(21082, first.date = '2015-01-01', last.date = '2025-12-01')

#4.2) Inadimplência por modalidade — códigos SGS (série "recursos livres", % da carteira)
inadimplencia_modalidades_ids <- c(
  consignado      = 21119,
  cartao_rotativo = 21127,
  cheque_especial = 21113
)
inadimplencia_modalidades <- gbcbd_get_series(inadimplencia_modalidades_ids,
                                              first.date = '2015-01-01', last.date = '2025-12-01')


#============================================================
# 5) ACESSO AO CRÉDITO E À BANCARIZAÇÃO POR PERFIL DO TOMADOR
#============================================================
# Fonte: Relatório de Cidadania Financeira (RCF) - edições 2018, 2021 e 2025
# (CORREÇÃO: não existe edição de 2023 - o RCF 2021 se descreve textualmente como
# "o segundo Relatório de Cidadania Financeira", confirmando que 2021 é a 2ª edição
# e 2025 é a 3ª; não há publicação entre 2021 e 2025)
# bcb.gov.br/nor/relcidfin — dados extraídos manualmente das tabelas/gráficos do PDF
#
# ESTRUTURA (do mais robusto ao mais frágil, decisão tomada em conjunto):
#   5.1 Relacionamento bancário (CCS) - EIXO CENTRAL, presente nas 3 edições
#   5.2 Global Findex - validação externa do eixo central (padronizado, Banco Mundial)
#   5.3 Penetração de crédito por faixa de renda - camada complementar (cada edição
#       com sua própria definição, NÃO comparável ano a ano de forma direta)
#   5.4 ICF/IIF por região - nota histórica 2018-2021 (índice DESCONTINUADO em 2025)
#   5.5 Gênero e raça - retrato transversal único da edição 2025 (dado não existia
#       nas edições anteriores)

#5.1) Relacionamento bancário (CCS) - % de adultos com relacionamento com o SFN
# Fontes:
#   2018 (RCF): Gráfico 1.10 (nacional, série 2015-2017) e Gráfico 1.11 (regional, 2017)
#   2021 (RCF): metodologia mista no próprio relatório - CCS mostra crescimento em
#     nível absoluto (149 para 163 milhões de pessoas em 2020, não convertido a %
#     diretamente no texto); pesquisa BCB "O brasileiro e os hábitos de uso de meios
#     de pagamento" (2019) aponta 77% - ATENÇÃO: metodologia de pesquisa amostral,
#     não comparável diretamente ao dado administrativo do CCS de 2018/2025
#   2025 (RCF): 92,6% relacionamento bancário geral; 88,4% "usuários ativos do SFN"
#     (dez/2024) - definição ligeiramente mais restrita (exige uso, não só posse)
relacionamento_bancario_nacional <- data.frame(
  ano = c(2015, 2016, 2017, 2019, 2020, 2024),
  fonte           = c("RCF2018 (CCS)", "RCF2018 (CCS)", "RCF2018 (CCS)",
                      "RCF2021 (pesquisa BCB)", "RCF2021 (CCS, nível absoluto)", "RCF2025 (CCS)"),
  percentual      = c(0.86, 0.85, 0.865, 0.77, NA, 0.926),  # 2020: só nível absoluto disponível (ver nota)
  metodologia     = c("administrativo", "administrativo", "administrativo",
                      "pesquisa amostral", "administrativo (nível, não %)", "administrativo")
)
# ATENÇÃO: a coluna "metodologia" mostra que a série NÃO é homogênea - documentar
# isso explicitamente no texto ao apresentar o gráfico, para não sugerir uma
# comparabilidade que os dados não têm

#5.2) Global Findex (Banco Mundial) - % de adultos com conta em instituição financeira
# Citado nas 3 edições do RCF como validação cruzada do dado administrativo do BC
# Fonte: World Bank Global Findex Database (globalfindex.worldbank.org)
# (2011 desconsiderado - fora do escopo do projeto, que cobre 2015-2025)
global_findex_brasil <- data.frame(
  ano = c(2014, 2017, 2021, 2024),
  percentual_com_conta = c(0.68, 0.70, 0.84, 0.86)
)

#5.3) Relacionamento bancário por região
# ATENÇÃO: as duas edições usam métricas DIFERENTES, não diretamente comparáveis:
#   2018: % de PENETRAÇÃO regional (adultos com relacionamento / adultos da região)
#         - Gráfico 1.11, ano-base 2017
#   2021: não encontrada nenhuma tabela/gráfico equivalente nesta edição - o RCF 2021
#         substituiu esse recorte pelo ranking regional do ICF (índice composto,
#         desconsiderado por decisão de escopo)
#   2025: % de COMPOSIÇÃO regional (distribuição dos clientes bancarizados entre as
#         regiões, não a penetração dentro de cada região) - Tabela 1.1.2, dez/2024
#         Ex: dos 175,2 milhões de clientes com relacionamento no SFN, 43% estão no
#         Sudeste - isso reflete o peso populacional da região, não necessariamente
#         uma penetração maior
relacionamento_bancario_regional <- data.frame(
  edicao_rcf = c(rep(2018, 5), rep(2021, 5), rep(2025, 5)),
  regiao = rep(c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"), times = 3),
  metrica = c(rep("penetração (% adultos da região)", 5),
              rep("não disponível nesta edição", 5),
              rep("composição (% dos clientes bancarizados)", 5)),
  percentual = c(
    0.723, NA, NA, 0.909, NA,        # 2018: só Norte e Sudeste citados no texto corrido
    NA, NA, NA, NA, NA,               # 2021: indisponível
    0.08, 0.26, 0.08, 0.43, 0.15      # 2025: Tabela 1.1.2, coluna SFN total
  )
)
# Para tornar 2018 e 2025 comparáveis na mesma métrica (penetração), seria necessário
# dividir os clientes de cada região (2025) pela população adulta regional (ex: IBGE/
# PNAD) - não feito aqui por estar fora do escopo definido para esta seção complementar

#5.4) Gênero e raça - retrato transversal, exclusivo da edição 2025
# Fonte: Tabela 2.2.2 (RCF 2025) - dado não existe nas edições anteriores (2018 afirma
# textualmente que as bases do BC "nem sempre permitem a desagregação por raça"; 2021
# tem só recorte de gênero, sem raça, fora do contexto de qualidade de crédito)
# Duas bases populacionais diferentes, cada uma com seu %-uso de crédito em 2024
# (crédito para consumo, exclui financiamento habitacional e crédito rural):
#   CadÚnico = população de baixa renda/beneficiários de programas sociais
#   RAIS = população com vínculo empregatício formal
inclusao_genero_raca_2025 <- data.frame(
  categoria = c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra",
                "Brancos (total)", "Negros (total)", "Homens (total)", "Mulheres (total)"),
  cadunico_populacao_mil   = c(4125, 9167, 7477, 16104, 11602, 25271, 13292, 23582),
  cadunico_pct_uso_credito = c(0.59, 0.54, 0.65, 0.62, 0.63, 0.59, 0.56, 0.63),
  rais_populacao_mil       = c(8755, 9649, 8035, 7204, 16789, 16853, 18404, 15239),
  rais_pct_uso_credito     = c(0.76, 0.76, 0.78, 0.80, 0.77, 0.78, 0.76, 0.79)
)
# Leitura-chave já indicada no próprio relatório: em ambas as bases, mulheres usam
# mais crédito proporcionalmente que homens; a população RAIS (emprego formal) usa
# crédito de forma mais uniforme entre grupos que a população CadÚnico (baixa renda)

# Gráfico 5.1 - Relacionamento bancário nacional (eixo central), com nota de
# metodologia mista via cor/forma do ponto
grafico_bancarizacao <- ggplot(relacionamento_bancario_nacional,
                               aes(x = ano, y = percentual, color = metodologia)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), color = "grey60", linetype = "dashed") +
  geom_point(data = global_findex_brasil %>% mutate(metodologia = "Global Findex (validação externa)"),
             aes(x = ano, y = percentual_com_conta, color = metodologia), size = 3, shape = 17) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.6, 1)) +
  scale_color_manual(values = c("administrativo" = "#1F3864",
                                "pesquisa amostral" = "#C00000",
                                "administrativo (nível, não %)" = "grey50",
                                "Global Findex (validação externa)" = "#2E7D32")) +
  labs(title = "Evolução da Bancarização no Brasil (% adultos)",
       subtitle = "Fontes mistas - ver metodologia de cada ponto",
       x = NULL, y = "% da população adulta", color = NULL) +
  tema_padrao

grafico_bancarizacao
# ggsave("grafico_5_1_bancarizacao.png", grafico_bancarizacao, width = 9, height = 5)

# Gráfico 5.2 - Uso de crédito por gênero e raça, comparando as duas bases (2025)
grafico_genero_raca <- inclusao_genero_raca_2025 %>%
  filter(categoria %in% c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")) %>%
  select(categoria, cadunico_pct_uso_credito, rais_pct_uso_credito) %>%
  pivot_longer(-categoria, names_to = "base", values_to = "pct_uso") %>%
  mutate(base = recode(base, cadunico_pct_uso_credito = "CadÚnico (baixa renda)",
                       rais_pct_uso_credito = "RAIS (emprego formal)")) %>%
  ggplot(aes(x = categoria, y = pct_uso, fill = base)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("CadÚnico (baixa renda)" = "#C00000", "RAIS (emprego formal)" = "#1F3864")) +
  labs(title = "Uso de Crédito por Gênero e Raça (2024)",
       x = NULL, y = "% que usou crédito", fill = NULL) +
  tema_padrao

grafico_genero_raca
# ggsave("grafico_5_2_genero_raca.png", grafico_genero_raca, width = 9, height = 5)


# ============================================================
# 6) ESTRUTURA CONCORRENCIAL — BANCOS x FINTECHS
# ============================================================
# Fonte: REB (edicoes 2015-2023) + REF, tema selecionado 2.4
# (RELESTAB202504-refPub.pdf, abril/2025).
#
# ATENCAO - TRANSICAO DE FONTE:
#   O REB publicou "Indicadores de concentracao" ate a edicao de
#   2023 (dado-base dez/2023). Nao ha edicao do REB para 2024; a
#   partir dai o BC passou a reportar o tema no REF (Capitulo II -
#   Temas Selecionados, secao 2.4), cuja 1a edicao com esse
#   conteudo e a de abril/2025 (dado-base dez/2024). A transicao
#   efetiva de fonte ocorre no ano-base 2024, nao em 2025 — nao
#   ha dado de fechamento de 2025 nos documentos fornecidos.
#
# ATENCAO - MUDANCA METODOLOGICA (RC5 -> RC4):
#   Ate o REB com dado-base 2020, o BC usava a Razao de
#   Concentracao dos CINCO maiores (RC5), com base em operacoes
#   DOMESTICAS. A partir do dado-base 2021, passou a RC4. O REF
#   historicamente usa RC4 incluindo tambem operacoes de
#   instituicoes brasileiras no exterior (nota de rodape 105,
#   REB/2017, p.86). RC5 e RC4 NAO sao diretamente comparaveis.
#
# Agregado: "Operacoes de credito", segmento amplo bancario +
# nao bancario (b1+b2+b3+b4+n1[+n2+n4]) — unico agregado com
# serie continua 2015-2024 nos documentos disponibilizados.

concentracao_sfn <- data.frame(
  ano = 2015:2024,
  fonte = c(rep("REB", 9), "REF"),
  metodologia_rc = c(rep("RC5", 6), rep("RC4", 4)),
  ihhn_credito = c(
    0.1242,  # 2015 - REB_2017.pdf, Tabela 5.1 (p.86)
    0.1291,  # 2016 - REB_2017.pdf, Tabela 5.1
    0.1276,  # 2017 - REB_2019.pdf, Tabela 6.1 (revisado; original REB_2017: 0,1280)
    0.1205,  # 2018 - reb_2018.pdf / REB_2019.pdf
    0.1124,  # 2019 - reb_2020.pdf, Tabela 6.1
    0.1069,  # 2020 - reb_2020.pdf, Tabela 6.1
    0.1022,  # 2021 - reb2023p.pdf, Tabela 5.1 (revisado; original reb_2021: 0,1034)
    0.1003,  # 2022 - reb2023p.pdf / RELESTAB202504, Tabela 2.4.1 (convergem)
    0.0990,  # 2023 - reb2023p.pdf / RELESTAB202504, Tabela 2.4.1 (convergem)
    0.0986   # 2024 - RELESTAB202504-refPub.pdf, Tabela 2.4.1 (p.75)
  ),
  rc4_credito = c(  # RC5 (%) ate 2020; RC4 (%) de 2021 em diante — ver metodologia_rc
    73.2,  # 2015
    74.3,  # 2016
    73.0,  # 2017
    70.9,  # 2018
    69.8,  # 2019
    68.5,  # 2020
    58.9,  # 2021 (RC4; reb2023p revisado; original reb_2021: 59,3)
    58.6,  # 2022 (RC4)
    57.8,  # 2023 (RC4)
    57.9   # 2024 (RC4)
  ),
  equivalente_numero_ihhn = c(
    8.1, 7.7, 7.8, 8.3, 8.9, 9.4, 9.8, 10.0, 10.1, 10.1
  )
)
# NAO ha, nos documentos fornecidos, dado de fechamento de 2025
# (o REF abril/2025 traz dado-base dez/2024). A linha ano=2025 foi
# omitida em vez de preenchida com NA silencioso — inclua-a quando
# a proxima edicao do REF for disponibilizada.


# ------------------------------------------------------------
# 6.2) Participacao de mercado por segmento institucional
# ------------------------------------------------------------
# IMPORTANTE: os relatorios NAO reportam uma particao unica em
# "Bancos tradicionais / Cooperativas / Nao bancarias-fintechs /
# Bancos publicos". Existem DOIS recortes distintos, calculados
# sobre universos diferentes e NAO somaveis entre si:
#   (a) por segmento institucional: b1+b2 (bancos comerciais e
#       multiplos), b3 (cooperativas de credito), b4 (bancos de
#       desenvolvimento), n1+n2 (instituicoes nao bancarias,
#       inclui fintechs de credito/pagamento) — soma ~100%
#   (b) por tipo de controle: Publico vs. Privado — atravessa
#       TODOS os segmentos de (a) (ex.: BB e CEF estao em b1+b2
#       E sao publicos; BNDES esta em b4 E e publico) — soma 100%
# Somar as duas tabelas para chegar a 4 categorias mutuamente
# exclusivas ("bancos publicos" + "bancos privados" + "cooperativas"
# + "fintechs" = 100%) produziria numeros incorretos, pois
# cooperativas e nao bancarias tambem se dividem em publico/privado.
# Por isso, os dois recortes ficam em data.frames separados.
#
# Disponivel apenas a partir de 2019 — a tabela "Participacao por
# segmento (%)" so passou a ser publicada a partir da edicao
# reb_2021.pdf, cobrindo 2019 em diante. 2015-2018: sem dado nos
# documentos fornecidos. Agregado: Operacoes de credito (mesmo
# escopo da secao 6.1). Nao ha dado de 2025 (ver nota acima).

participacao_segmento <- data.frame(
  ano = rep(2019:2024, each = 4),
  segmento = rep(
    c("Bancos comerciais/multiplos (b1+b2)",
      "Cooperativas de credito (b3)",
      "Instituicoes nao bancarias/fintechs (n1+n2)",
      "Bancos de desenvolvimento (b4)"),
    times = 6
  ),
  participacao_pct = c(
    # 2019 - reb_2021.pdf, Tabela 6.1
    86.5, 4.3, 1.1, 8.1,
    # 2020 - reb_2021.pdf / reb2022p.pdf (convergem)
    86.4, 5.1, 1.1, 7.4,
    # 2021 - reb2023p.pdf (revisado; originais reb_2021/reb2022p: 86,2/6,1/1,4/6,3)
    86.2, 6.0, 1.5, 6.2,
    # 2022 - reb2023p.pdf / RELESTAB202504, Tabela 2.4.1 (convergem)
    86.2, 6.3, 1.7, 5.7,
    # 2023 - reb2023p.pdf / RELESTAB202504, Tabela 2.4.1 (convergem)
    85.8, 6.8, 2.0, 5.3,
    # 2024 - RELESTAB202504-refPub.pdf, Tabela 2.4.1 (p.75)
    85.0, 7.2, 2.6, 5.2
  ),
  fonte = rep(c("REB", "REB", "REB", "REB", "REB", "REF"), each = 4)
  # nota: fonte listada é a mais recente/revisada usada para cada
  # ano; ver comentarios acima linha a linha para a origem exata
)

# Recorte complementar: participacao por tipo de controle (Publico x
# Privado), TODOS os segmentos, mesmo agregado (Operacoes de credito).
# NAO somar com participacao_segmento acima (ver nota).
participacao_controle <- data.frame(
  ano = 2019:2024,
  publico_pct = c(47.6, 45.6, 43.5, 43.5, 44.1, 44.0),
  # 2019-2020: reb_2021.pdf | 2021-2023: reb2023p.pdf (revisado) | 2024: RELESTAB202504
  privado_pct = c(52.4, 54.4, 56.5, 56.5, 55.9, 56.0)
)


# ============================================================
# 7) INDICADORES DE TECNOLOGIA
# ============================================================

#7.1) Open Finance — Consentimentos ativos (dashboard.openfinancebrasil.org.br)
# Dado real, semanal, extraído diretamente do dashboard (2023-01 em diante - não há
# granularidade semanal/mensal publicamente disponível antes disso, como você já notou)
# Arquivo: Consentimentos_ativos.xlsx, aba "Consentimentos ativos", dados a partir da linha 11
library(readxl)
consentimentos_raw <- read_excel("Consentimentos_ativos.xlsx",
                                 sheet = "Consentimentos ativos",
                                 skip = 10)  # pula os metadados/filtros do início do export
consentimentos_raw <- consentimentos_raw %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Agregação mensal (média das semanas de cada mês) para consistência com o restante do dataset
open_finance_consentimentos_mensal <- consentimentos_raw %>%
  mutate(ano_mes = floor_date(date, "month")) %>%
  group_by(ano_mes) %>%
  summarise(consentimentos_ativos_media = mean(value, na.rm = TRUE), .groups = "drop")

# Gráfico 7.1 - Evolução semanal (mais granular, mostra melhor os saltos de fase do Open Finance)
grafico_consentimentos <- ggplot(consentimentos_raw, aes(x = date, y = value)) +
  geom_line(color = "#1F3864", linewidth = 0.7) +
  labs(title = "Consentimentos Ativos no Open Finance (semanal)",
       x = NULL, y = "Consentimentos ativos") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  tema_padrao

grafico_consentimentos
# ggsave("grafico_7_1_consentimentos.png", grafico_consentimentos, width = 9, height = 5)

#7.1b) PENDENTE — instituições participantes por tipo (bancos x fintechs)
# O dashboard do Open Finance (dashboard.openfinancebrasil.org.br) NÃO tem uma página com
# série histórica de "instituições participantes por tipo" - conferido diretamente na
# estrutura do site. O que existe lá são apenas RANKINGS de instituições individuais por
# volume de chamadas de API (ex: dashboard.openfinancebrasil.org.br/payment-initiation/
# ranking/holders - "maiores detentores de conta"), não uma contagem agregada por categoria
# ao longo do tempo.
# O cadastro completo de instituições participantes (por categoria: transmissora, receptora,
# iniciadora, detentora de conta) fica no Diretório de Participantes, mantido pela Associação
# Open Finance Brasil (openfinancebrasil.org.br) - mas é um registro "ao vivo" (foto do
# momento), não uma série histórica com data de cada instituição que entrou.
# Alternativas caso queira esse dado:
#   (a) usar o número total atual como um único ponto (ex: releases de imprensa/associação
#       citam >800 instituições cadastradas em 2025) - serve como nota pontual, não série;
#   (b) tentar reconstruir uma série aproximada via capturas do Diretório em datas diferentes
#       no Wayback Machine - trabalhoso e não confiável para fins acadêmicos;
#   (c) documentar a ausência dessa série como limitação, e usar consentimentos ativos +
#       requisições de API (já disponíveis) como proxies de adoção, que cobrem bem o
#       objetivo de mostrar a evolução do uso do Open Finance mesmo sem o dado de
#       participantes por tipo.
# Recomendação: seguir com (c), já que essa é uma seção complementar.

#7.2) Cadastro Positivo — evolução do estoque de cadastrados (pessoas naturais)
# CORREÇÃO: a fonte não é secundária (Boa Vista/Serasa) como eu tinha sugerido antes -
# você encontrou a fonte correta e melhor: o próprio relatório oficial do BCB
# "Análise dos Efeitos do Cadastro Positivo" (2021, em atendimento ao art. 5º da LC 166/2019)
# ATENÇÃO - limitações importantes desta fonte, únicas para os dados que ela fornece:
#   1. O relatório só cobre até dezembro/2020 - não há dado oficial de 2021-2025 nesta fonte
#      (precisaria de fonte adicional para estender a série, se quiser cobrir até 2025)
#   2. O Gráfico 1 do relatório (evolução do estoque de PF) é um ÍNDICE base 100 = abril/2019
#      (soma das 4 GBDs), não um número absoluto de CPFs - e o próprio relatório alerta que
#      esse número é SUPERESTIMADO por duplicidade de registro do mesmo cadastrado nas
#      diferentes GBDs. Portanto, não tratar como contagem exata de pessoas.
#   3. Os dois pontos abaixo são os únicos valores ABSOLUTOS (não indexados) citados no
#      texto do relatório, e por isso são os mais seguros para uso comparativo:
cadastro_positivo <- data.frame(
  periodo = c("dez/2016 (regime opt-in)", "dez/2020 (regime opt-out, pós-LC 166/2019)"),
  pessoas_naturais_milhoes = c(5.5, 100),   # "pelo menos 100 milhões" - relatório não dá número exato
  pct_populacao_adulta = c(NA, 66),          # 66% da população >19 anos (projeção IBGE), só citado para 2020
  observacao = c("citado no parecer do Senador Armando Monteiro (relator da LC 166/2019)",
                 "'pelo menos' 100 milhões - relatório não fornece número exato; véspera da mudança de regime")
)
# Salto de ~18x entre os dois pontos, refletindo a mudança de regime opt-in -> opt-out
# (o relatório também menciona que o próprio regime opt-in, sozinho, gerou apenas 5,5 milhões
# em quase 6 anos, "menos de 5% do potencial de mercado", segundo o parecer legislativo)

# PENDENTE, caso queira estender a série além de 2020: buscar fonte adicional (Boa Vista,
# Serasa, ou nova consulta ao BCB) para os anos 2021-2025 - este relatório não cobre esse
# período. Documentar essa lacuna temporal explicitamente no texto se optar por não estender.


# ============================================================
# 8) SÍNTESE COMPARATIVA ENTRE SUBPERÍODOS
# ============================================================
# ESCOPO: só entram aqui séries que têm cobertura contínua (ou quase) ao longo dos três
# subperíodos. Ficam DE FORA, por decisão de escopo, já explicada:
#   - Seção 5 (RCF): só 3 pontos esparsos por edição (2018/2021/2025) - não é uma média
#     de subperíodo, é uma comparação direta entre edições. Tratar no texto como
#     "evolução entre edições do RCF", não como linha desta tabela.
#   - Seção 7 (Open Finance): consentimentos só começam em 2023 - não tem cobertura no
#     subperíodo 2015-2018 nem 2019-2021, então uma "média do subperíodo" seria enganosa
#     (compararia um subperíodo com 3 anos de dado real a outro com zero). Tratar como
#     ponto isolado pós-2022 no texto.
#   - Seção 7.2 (Cadastro Positivo): só 2 pontos (dez/2016 e dez/2020) - mesma lógica.

#8.1) Bloco A - séries MENSAIS do SGS (usa definir_subperiodo, por data)

sintese_mensal <- saldo_credito %>%
  transmute(subperiodo = definir_subperiodo(ref.date), saldo_credito = value) %>%
  group_by(subperiodo) %>%
  summarise(saldo_credito_medio = mean(saldo_credito, na.rm = TRUE), .groups = "drop") %>%
  
  left_join(
    concessoes %>%
      transmute(subperiodo = definir_subperiodo(ref.date), concessoes = value) %>%
      group_by(subperiodo) %>%
      summarise(concessoes_media = mean(concessoes, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  ) %>%
  
  left_join(
    credito_pib %>%
      transmute(subperiodo = definir_subperiodo(ref.date), credito_pib = value) %>%
      group_by(subperiodo) %>%
      summarise(credito_pib_medio = mean(credito_pib, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  ) %>%
  
  left_join(
    spread_medio %>%
      transmute(subperiodo = definir_subperiodo(ref.date), spread = value) %>%
      group_by(subperiodo) %>%
      summarise(spread_medio_pp = mean(spread, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  ) %>%
  
  left_join(
    inadimplencia_total %>%
      transmute(subperiodo = definir_subperiodo(ref.date), inadimplencia = value) %>%
      group_by(subperiodo) %>%
      summarise(inadimplencia_media_pct = mean(inadimplencia, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  )

# Reordenar as linhas na ordem cronológica correta (group_by/summarise não garante isso)
ordem_subperiodos <- c("2015-2018 (pré-Cadastro Positivo)",
                       "2019-2021 (transição/pandemia)",
                       "2022-2025 (pós-Open Finance)")
sintese_mensal <- sintese_mensal %>%
  mutate(subperiodo = factor(subperiodo, levels = ordem_subperiodos)) %>%
  arrange(subperiodo)

#8.2) Bloco A-complementar - inadimplência por modalidade e composição por modalidade,
# calculados à parte por terem formato "longo" (uma linha por modalidade x data)

sintese_inadimplencia_modalidade <- inadimplencia_modalidades %>%
  mutate(modalidade = names(inadimplencia_modalidades_ids)[match(id.num, inadimplencia_modalidades_ids)],
         subperiodo = definir_subperiodo(ref.date)) %>%
  mutate(subperiodo = factor(subperiodo, levels = ordem_subperiodos)) %>%
  group_by(subperiodo, modalidade) %>%
  summarise(inadimplencia_media_pct = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(subperiodo, modalidade)

sintese_composicao_modalidade <- modalidades_nomeadas %>%
  mutate(subperiodo = definir_subperiodo(ref.date)) %>%
  mutate(subperiodo = factor(subperiodo, levels = ordem_subperiodos)) %>%
  group_by(subperiodo, modalidade) %>%
  summarise(saldo_medio = mean(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(subperiodo) %>%
  mutate(participacao_pct = saldo_medio / sum(saldo_medio) * 100) %>%
  ungroup() %>%
  arrange(subperiodo, modalidade)

#8.3) Bloco B - séries ANUAIS extraídas de PDF (usa definir_subperiodo_ano, por ano)
# ATENÇÃO: custo_credito_decomposicao e concentracao_sfn cobrem 2015-2024/2025 (quase
# completo); participacao_segmento e participacao_controle só cobrem 2019-2024 - portanto
# o subperíodo "2015-2018" ficará com NA nessas duas últimas. Isso é esperado e deve ser
# indicado como tal na tabela final (não é erro de código, é limitação de fonte já
# documentada nos comentários da Seção 6).

sintese_anual <- custo_credito_decomposicao %>%
  transmute(subperiodo = definir_subperiodo_ano(ano), icc_total_pp = icc_total_pp) %>%
  group_by(subperiodo) %>%
  summarise(icc_medio_pp = mean(icc_total_pp, na.rm = TRUE), .groups = "drop") %>%
  
  left_join(
    concentracao_sfn %>%
      transmute(subperiodo = definir_subperiodo_ano(ano), ihhn = ihhn_credito, rc4 = rc4_credito) %>%
      group_by(subperiodo) %>%
      summarise(ihhn_medio = mean(ihhn, na.rm = TRUE),
                rc4_medio_pct = mean(rc4, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  ) %>%
  
  left_join(
    participacao_segmento %>%
      filter(segmento == "Instituicoes nao bancarias/fintechs (n1+n2)") %>%
      transmute(subperiodo = definir_subperiodo_ano(ano), fintechs_pct = participacao_pct) %>%
      group_by(subperiodo) %>%
      summarise(fintechs_participacao_media_pct = mean(fintechs_pct, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  ) %>%
  
  left_join(
    participacao_controle %>%
      transmute(subperiodo = definir_subperiodo_ano(ano), publico = publico_pct) %>%
      group_by(subperiodo) %>%
      summarise(publico_participacao_media_pct = mean(publico, na.rm = TRUE), .groups = "drop"),
    by = "subperiodo"
  ) %>%
  mutate(subperiodo = factor(subperiodo, levels = ordem_subperiodos)) %>%
  arrange(subperiodo)
# NOTA: como concentracao_sfn tem RC5 até 2020 e RC4 de 2021 em diante (ver Seção 6),
# "rc4_medio_pct" do subperíodo 2019-2021 mistura as duas metodologias (2019-2020 em RC5,
# 2021 em RC4) - documentar essa quebra de série explicitamente ao apresentar esse valor
# no texto, não só reportar o número.

#8.4) Tabela mestra final - junta os dois blocos por subperíodo
sintese_subperiodos <- sintese_mensal %>%
  left_join(sintese_anual, by = "subperiodo")

# print(sintese_subperiodos)  # conferir antes de formatar a tabela final para o Word
# view(sintese_composicao_modalidade)   # tabela auxiliar - composição por modalidade
# view(sintese_inadimplencia_modalidade) # tabela auxiliar - inadimplência por modalidade

#8.5) Pontos narrativos complementares (NÃO entram na tabela mestra - ver justificativa
# no início da Seção 8; usar como texto corrido na discussão, citando a fonte e o
# ano/edição específico de cada ponto):
#   - Bancarização (RCF): 86% (2017) -> 92,6% (2024)
#   - Global Findex: 70% (2017) -> 86% (2024)
#   - Consentimentos ativos Open Finance: evolução 2023-2025 (ver grafico_consentimentos)
#   - Cadastro Positivo: 5,5 milhões (2016, opt-in) -> "pelo menos" 100 milhões
#     (2020, opt-out) — salto de ~18x na mudança de regime