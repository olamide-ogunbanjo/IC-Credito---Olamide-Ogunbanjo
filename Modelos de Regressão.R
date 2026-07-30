#MODELO 1 - INADIMPLÊNCIA
#install.packages(c("GetBCBData","dplyr", "lubridate", "tseries", "urca", "lmtest","sandwich", "car", "vars"))
library("GetBCBData")
library("dplyr")
library("lubridate")
library("tseries")
library("urca")
library("lmtest")
library("sandwich")
library("car")
library("vars")

#1) Baixar as Séries
tot_conc <- gbcbd_get_series(24439, first.date = '2015-01-01', last.date = '2025-12-01')  # Concessões dessaz
selic_meta <- gbcbd_get_series(432, first.date = '2015-01-01', last.date = '2025-12-01')
ibc_br <- gbcbd_get_series(24364, first.date = '2015-01-01', last.date = '2025-12-01')    # IBC-Br dessaz
inadimplencia <- gbcbd_get_series(21082, first.date = '2015-01-01', last.date = '2025-12-01')  # sem versão dessaz

#2) Padronizar os períodos e filtrar colunas
tot_conc <- tot_conc %>%
  mutate(ano_mes = floor_date(ref.date, "month")) %>%
  dplyr::select(ano_mes, credito = value)
ibc_br <- ibc_br %>%
  mutate(ano_mes = floor_date(ref.date, "month")) %>%
  dplyr::select(ano_mes, atividade = value)
inadimplencia <- inadimplencia %>%
  mutate(ano_mes = floor_date(ref.date, "month")) %>%
  dplyr::select(ano_mes, inadimplencia = value)
selic_mensal <- selic_meta %>%
  mutate(ano_mes = floor_date(ref.date, "month")) %>%
  group_by(ano_mes) %>%
  summarise(juros = mean(value, na.rm = TRUE))

#3) Juntar tudo em um dataframe
df <- tot_conc %>%
  left_join(selic_mensal,    by = "ano_mes") %>%
  left_join(ibc_br,          by = "ano_mes") %>%
  left_join(inadimplencia,   by = "ano_mes") %>%
  arrange(ano_mes)

#4) Montar as Dummies
# Dummy de tecnologia: 0 antes de fev/2021 (início do Open Finance), 1 a partir daí
# Dummy de pandemia, separada da de tecnologia para não confundir os dois choques
# Log do crédito, já que o Modelo 1 usa ln(Crédito) como variável dependente
# Dummy sazonal de mês: Inadimplência não tem versão dessazonalizada, então o
# padrão sazonal anual precisa ser controlado diretamente no modelo
df <- df %>%
  mutate(
    tecnologia = if_else(ano_mes >= as.Date("2021-02-01"), 1, 0),
    covid = if_else(ano_mes >= as.Date("2020-03-01") & ano_mes <= as.Date("2021-12-01"), 1, 0),
    ln_credito = log(credito),
    mes = factor(month(ano_mes))
  )

# Cria 11 dummies binárias de mês (jan é a categoria de referência, omitida)
dummies_sazonais <- model.matrix(~ mes, data = df)[, -1]

#5) Testar Estacionaridade
# --- Função auxiliar para organizar os resultados dos dois testes lado a lado ---
testar_estacionariedade <- function(serie, nome) {
  adf <- adf.test(na.omit(serie))
  kpss <- ur.kpss(na.omit(serie), type = "tau") # "tau" inclui tendência, que é visível nas séries
  cat("\n===", nome, "===\n")
  cat("ADF  - Estatística:", round(adf$statistic, 3),
      "| p-valor:", round(adf$p.value, 4), "\n")
  cat("KPSS - Estatística:", round(kpss@teststat, 3),
      "| Valor crítico (5%):", round(kpss@cval[2], 3), "\n")
}

# --- Rodar para as quatro séries do Modelo 1 ---
testar_estacionariedade(df$ln_credito,    "Ln(Crédito)")
testar_estacionariedade(df$juros,         "Juros (Selic mensal)")
testar_estacionariedade(df$atividade,     "Atividade (IBC-Br dessaz)")
testar_estacionariedade(df$inadimplencia, "Inadimplência")

# Inspeção visual da Atividade: ADF e KPSS divergiram em nível; o gráfico confirma
# tendência de crescimento clara (queda 2020 + trajetória ascendente até 2025),
# validando a leitura do KPSS (não-estacionária) sobre a do ADF
plot(df$ano_mes, df$atividade, type = "l", main = "IBC-Br (dessaz)")

# Testar o nível da diferença I(1) ou I(2)
testar_estacionariedade(diff(df$ln_credito),    "Δ Ln(Crédito)")
testar_estacionariedade(diff(df$juros),         "Δ Juros")
testar_estacionariedade(diff(df$atividade),     "Δ Atividade")
testar_estacionariedade(diff(df$inadimplencia), "Δ Inadimplência")

# Diagnóstico adicional: Juros e Inadimplência ficaram ambíguos na diferença
# Reforçando o ADF com seleção ótima de defasagens via AIC (o teste padrão usa
# truncamento fixo, que pode não capturar a persistência real da série)
ur.df(diff(df$juros), type = "drift", selectlags = "AIC") %>% summary()
ur.df(diff(df$inadimplencia), type = "drift", selectlags = "AIC") %>% summary()
# Inadimplência: com defasagem ótima, ADF rejeita raiz unitária com folga (tau2 = -7.52,
# defasagem extra não significativa) -> falso alarme do teste padrão, série é I(1)

# Juros seguiu ambíguo mesmo com AIC (tau2 = -2.35, não rejeita a 10%) ->
# testando quebra estrutural via Zivot-Andrews (mais apropriado que ADF/KPSS
# quando há mudança de regime e não tendência suave)
ur.za(na.omit(df$juros), model = "both", lag = 4) %>% summary()
ur.za(na.omit(df$juros), model = "both", lag = 1) %>% summary()
# Resultado limítrofe (rejeita a 5%, não a 1%) e quebra estimada em fev/2021 -
# mesma data da dummy "tecnologia". Optou-se por NÃO corrigir por quebra e tratar
# Juros como I(1) genuína, para não confundir o efeito da Selic com o do Open Finance
# (literatura de macroeconometria brasileira trata Selic como I(1) sem controvérsia)

# CONCLUSÃO: as quatro séries (ln_credito, juros, atividade, inadimplencia) são I(1)
# -> próxima etapa: seleção de defasagens (VARselect) e teste de cointegração de Johansen

#6) Selecionar o número de defasagens do VAR (base para o Johansen)
sistema <- df %>%
  dplyr::select(ln_credito, juros, atividade, inadimplencia) %>%
  na.omit()

VARselect(sistema, lag.max = 12, type = "const")$selection
# SC(BIC) mais parcimonioso, mais adequado ao tamanho da amostra -> ponto de partida

#7) Checar autocorrelação residual do VAR antes do Johansen
# Séries de Concessões e Inadimplência tinham problema de sazonalidade não tratada
# (Concessões e IBC-Br já corrigidos com as versões dessaz; Inadimplência não tem
# versão dessaz -> controlada via dummies de mês, junto com tecnologia e covid)
exogenas <- cbind(
  df %>% dplyr::select(tecnologia, covid) %>% slice(1:nrow(sistema)),
  dummies_sazonais[1:nrow(sistema), ]
)

var_teste_completo <- VAR(sistema, p = 3, type = "const", exogen = exogenas)
serial.test(var_teste_completo, lags.pt = 12, type = "PT.asymptotic")
# Se p > 0.05: sazonalidade era a causa raiz da autocorrelação -> segue com K=3
# Se p < 0.05: testar K=7 novamente, agora já com sazonalidade controlada

# K=3 não bastou -> testado K=7 (const e trend, resultado praticamente igual) ->
# investigado efeito ARCH (heterocedasticidade condicional), confirmado mas moderado
# (p=0.037) -> aceito como limitação documentada, não perseguido além disso
var_teste7_trend <- VAR(sistema, p = 7, type = "both", exogen = exogenas)
arch.test(var_teste7_trend, lags.multi = 5, multivariate.only = TRUE)

#8) Teste de cointegração de Johansen (K=7, exógenas incluídas via dumvar)
johansen <- ca.jo(sistema, type = "trace", ecdet = "const", K = 7, dumvar = exogenas)
summary(johansen)
# Rank de cointegração = 2 (r=0 e r<=1 rejeitados com folga; r<=2 não rejeitado)
# -> as variáveis são cointegradas, o que VALIDA o MQO em nível originalmente
# proposto (Stock, 1987: coeficientes superconsistentes sob cointegração),
# afastando o risco de regressão espúria sem precisar migrar para VECM
# (decisão alinhada com a orientadora, dado o escopo de uma IC)

#9) Causalidade de Granger entre Crédito e Inadimplência
# (ressalva: sob I(1)/cointegração, o teste padrão pode não ter distribuição
# assintótica convencional - o ideal seria Toda-Yamamoto, fora do escopo aqui;
# mantém-se a defasagem por justificativa teórica independente do resultado)
grangertest(ln_credito ~ inadimplencia, order = 7, data = sistema)
grangertest(inadimplencia ~ ln_credito, order = 7, data = sistema)
# Nenhuma direção significativa (p=0.2278 e p=0.5652) - sem evidência empírica
# forte de causalidade de Granger em qualquer sentido, com a ressalva acima

#10) MODELO 1 - Concessão de Crédito: MQO em nível, com defasagem na variável
# de simultaneidade (inadimplencia) e todas as dummies validadas
df_modelo <- df %>%
  mutate(inadimplencia_lag = lag(inadimplencia, 1)) %>%
  na.omit()

modelo1 <- lm(ln_credito ~ tecnologia + juros + atividade + inadimplencia_lag +
                covid + mes,
              data = df_modelo)
summary(modelo1)

# Diagnóstico dos resíduos
bgtest(modelo1, order = 7)   # autocorrelação (esperado rejeitar, dado o histórico do VAR)
bptest(modelo1)              # heterocedasticidade
vif(modelo1)                 # multicolinearidade (checar tecnologia/juros/atividade)

# Erros-padrão robustos (HAC/Newey-West, lag=7 para manter consistência com o K validado)
coeftest(modelo1, vcov = NeweyWest(modelo1, lag = 7, prewhite = FALSE))

# Tabela final para o capítulo de resultados
library(modelsummary)
modelsummary(modelo1,
             vcov = function(x) NeweyWest(x, lag = 7, prewhite = FALSE),
             stars = TRUE,
             title = "Modelo 1 - Concessão de Crédito (MQO com erros-padrão HAC, lag=7)")


#MODELO 2 - INADIMPLÊNCIA
# Reaproveita todo o diagnóstico já feito no sistema (estacionariedade, quebra
# estrutural em Juros, sazonalidade, cointegração rank=2) - só espelha a estrutura
# do Modelo 1, invertendo a variável dependente e a defasagem de simultaneidade

#11) MODELO 2 - MQO em nível, com Crédito defasado (mesma lógica de simultaneidade)
df_modelo2 <- df %>%
  mutate(ln_credito_lag = lag(ln_credito, 1)) %>%
  na.omit()

modelo2 <- lm(inadimplencia ~ tecnologia + juros + atividade + ln_credito_lag +
                covid + mes,
              data = df_modelo2)
summary(modelo2)

# Diagnóstico dos resíduos (mesmo roteiro do Modelo 1)
bgtest(modelo2, order = 7)
bptest(modelo2)
vif(modelo2)

# Erros-padrão robustos (HAC/Newey-West, lag=7)
coeftest(modelo2, vcov = NeweyWest(modelo2, lag = 7, prewhite = FALSE))

# Tabela final para o capítulo de resultados
modelsummary(modelo2,
             vcov = function(x) NeweyWest(x, lag = 7, prewhite = FALSE),
             stars = TRUE,
             title = "Modelo 2 - Inadimplência (MQO com erros-padrão HAC, lag=7)")