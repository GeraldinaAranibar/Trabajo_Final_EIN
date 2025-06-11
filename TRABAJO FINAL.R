
# 1. Cargar librerías ---------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
library(ggfortify)
library(reshape2)
library(ggpubr)

# 2. Parámetros de archivo ----------------------------------------------------
ruta    <- "/Users/geraldinaaranibargonzales/Downloads"
archivo <- "Bolivia - Exportaciones segun Actividad Economica y Producto por Año y Mes, 1992 - 2024.xlsx"
hoja    <- "ExpActProdMes 92-24 Valor"

# 3. Construir vector de períodos (YYYY-MM) -----------------------------------
hdr       <- read_excel(file.path(ruta, archivo), sheet = hoja,
                        range = "C4:OG5", col_names = FALSE)
year_row  <- na.locf(as.character(hdr[1, ]))
month_row <- as.character(hdr[2, ])
meses_map <- c(Enero=1,Febrero=2,Marzo=3,Abril=4,Mayo=5,Junio=6,
               Julio=7,Agosto=8,Septiembre=9,Octubre=10,
               Noviembre=11,Diciembre=12)
periods   <- sprintf("%s-%02d", year_row, meses_map[month_row])

# 4. Leer y limpiar datos completos ------------------------------------------
bd_all <- read_excel(file.path(ruta, archivo), sheet = hoja,
                     range = "B6:OG101",
                     col_names = c("detalle", periods))
df_all <- bd_all %>%
  pivot_longer(cols      = all_of(periods),
               names_to  = "periodo",
               values_to = "valor_chr") %>%
  mutate(
    valor   = as.numeric(gsub(",", ".", valor_chr)),
    periodo = as.yearmon(periodo, "%Y-%m")
  ) %>%
  filter(!is.na(periodo))

# 5. Serie total de exportaciones --------------------------------------------
serie_tot <- df_all %>%
  filter(toupper(detalle) %in% c("TOTAL", "EXPORTACIONES")) %>%
  arrange(periodo)
start_ts  <- c(year(min(serie_tot$periodo)), month(min(serie_tot$periodo)))
export_ts <- ts(serie_tot$valor, frequency = 12,
                start = start_ts, end = c(2024, 12))

# 6. Estadísticos descriptivos ------------------------------------------------
summary_vals <- df_all %>%
  filter(toupper(detalle) == "EXPORTACIONES") %>%
  pull(valor) %>%
  summary()
sd_val <- df_all %>%
  filter(toupper(detalle) == "EXPORTACIONES") %>%
  pull(valor) %>%
  sd(na.rm = TRUE)
print(summary_vals)
print(paste("SD =", round(sd_val,2)))

# 7. EDA y visualizaciones básicas -------------------------------------------
# 7.1 Serie de tiempo
autoplot(export_ts) + ggtitle("Exportaciones Totales 1992–2024")
# 7.2 Primera y segunda diferencias
d1 <- diff(export_ts)
d2 <- diff(export_ts, differences = 2)
ggarrange(
  autoplot(d1) + ggtitle("1ª Diferencia"),
  autoplot(d2) + ggtitle("2ª Diferencia"),
  ncol = 1, nrow = 2
)
# 7.3 Diferencia estacional (lag=12)
ds <- diff(export_ts, lag = 12)
autoplot(ds) + ggtitle("Diferencia Estacional (12)")

# 7.4 Tasas de crecimiento
growth_Q <- 100 * (export_ts[-1] - export_ts[-length(export_ts)]) / export_ts[-length(export_ts)]
growth_G <- 100 * (export_ts[-(1:12)] - export_ts[-((length(export_ts)-11):length(export_ts))]) / export_ts[-((length(export_ts)-11):length(export_ts))]
# Construimos data frame de igual longitud a export_ts
n <- length(export_ts)
growth_df <- data.frame(
  periodo = as.yearmon(time(export_ts)),
  Qt      = c(NA, growth_Q),    
  Gt      = c(rep(NA, 12), growth_G)
)
# Graficamos tasas de crecimiento
ggplot(growth_df, aes(x = periodo)) +
  geom_line(aes(y = Qt, color = "Qt")) +
  geom_line(aes(y = Gt, color = "Gt")) +
  labs(title = "Tasas de Crecimiento Qt y Gt")

# 7.5 Boxplot mensual
df_box <- df_all %>%
  filter(toupper(detalle) == "EXPORTACIONES") %>%
  mutate(mes = factor(month(periodo), levels=1:12, labels=names(meses_map)))
ggplot(df_box, aes(x = mes, y = valor)) +
  geom_boxplot() +
  ggtitle("Boxplot Mensual de Exportaciones") +
  xlab("Mes") + ylab("Valor (USD millones)")

# 7.6 Heatmap anual-mes
heat <- df_all %>%
  filter(toupper(detalle) == "EXPORTACIONES") %>%
  mutate(year = year(periodo), month = month(periodo)) %>%
  group_by(year, month) %>%
  summarise(mean_val = mean(valor, na.rm=TRUE), .groups="drop")
ggplot(heat, aes(x = month, y = year, fill = mean_val)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap Exportaciones Promedio Mensual", x="Mes", y="Año")

# 8. Pruebas de Estacionariedad ----------------------------------------------
adf_tot <- adf.test(export_ts)
adf_d1  <- adf.test(d1)
adf_ds  <- adf.test(ds)
print(adf_tot)
print(adf_d1)
print(adf_ds)

# 9. Descomposición -----------------------------------------------------------
decomp <- decompose(export_ts)
plot(decomp)

# 10. Modelo ETS com Model Flowchart -----------------------------------------
fit_ets    <- ets(export_ts)
summary(fit_ets)
checkresiduals(fit_ets)
fc_ets     <- forecast(fit_ets, h = 24)
autoplot(fc_ets) + ggtitle("Pronóstico ETS 2025–2026")

# 11. Modelos ARIMA manual vs automático --------------------------------------
fit_man    <- Arima(export_ts, order=c(2,1,2), seasonal=list(order=c(1,1,1), period=12))
summary(fit_man)
checkresiduals(fit_man)
fc_man     <- forecast(fit_man, h = 24)
autoplot(fc_man) + ggtitle("Pronóstico ARIMA Manual")



fit_auto   <- auto.arima(export_ts, seasonal = TRUE)
summary(fit_auto)
checkresiduals(fit_auto)
fc_auto    <- forecast(fit_auto, h = 24)
autoplot(fc_auto) + ggtitle("Pronóstico ARIMA Automático")

# 12. Comparación de modelos -------------------------------------------------
metrics_ts  <- rbind(
  ETS            = accuracy(fc_ets)[1, ],
  ARIMA_Manual   = accuracy(fc_man)[1, ],
  ARIMA_Automático = accuracy(fc_auto)[1, ]
)
print(metrics_ts)

# Combinamos observado y pronósticos en un solo objeto ts
h <- length(fc_ets$mean)
obs_ts <- export_ts
ets_fc_ts <- ts(c(rep(NA, length(obs_ts)), fc_ets$mean),
                start = start_ts, frequency = 12)
man_fc_ts <- ts(c(rep(NA, length(obs_ts)), fc_man$mean),
                start = start_ts, frequency = 12)
auto_fc_ts<- ts(c(rep(NA, length(obs_ts)), fc_auto$mean),
                start = start_ts, frequency = 12)

# Unimos objetos para graficar
plt_ts <- cbind(
  Observado   = c(obs_ts, rep(NA, h)),
  ETS         = ets_fc_ts,
  ARIMA_Manual = man_fc_ts,
  ARIMA_Automático = auto_fc_ts
)
autoplot(plt_ts) +
  ggtitle("Comparativo Pronósticos 2025–2026") +
  xlab("Año") + ylab("Exportaciones (USD millones)")

# 13. Selección y pronóstico de top 2 productos -----------------------------

prod_sum <- df_all %>%
  filter(!detalle %in% c("TOTAL","EXPORTACIONES")) %>%
  group_by(detalle) %>%
  summarise(total_export = sum(valor,na.rm=TRUE), .groups="drop") %>%
  arrange(desc(total_export))

top2 <- prod_sum$detalle[1:2]
print("Top 2 productos por volumen total:")
print(prod_sum[1:2, ])

# ========================================================
# Producto 1: EXTRACCIÓN DE HIDROCARBUROS
# ========================================================

# Subconjunto y ts

df1   <- df_all %>% filter(detalle == "EXTRACCIÓN DE HIDROCARBUROS") %>% arrange(periodo)
ts1   <- ts(df1$valor, frequency = 12,
            start = c(year(min(df1$periodo)), month(min(df1$periodo))),
            end = c(2024,12))

# Serie Mensual

autoplot(ts1) + ggtitle("Extracción de Hidrocarburos — Serie Mensual")

# Diferencias

d1_1  <- diff(ts1)
ds1   <- diff(ts1, lag = 12)

ggarrange(
  autoplot(d1_1) + ggtitle("1ª Diferencia - Hidrocarburos"),
  autoplot(ds1)  + ggtitle("Diferencia Estacional (12) - Hidrocarburos"),
  nrow = 2
)

# ACF / PACF y ADF

ggAcf(ts1)  + ggtitle("ACF - Hidrocarburos")
ggPacf(ts1) + ggtitle("PACF - Hidrocarburos")
print(adf.test(ts1))
print(adf.test(d1_1))
print(adf.test(ds1))

# ARIMA Automático

fit1 <- auto.arima(ts1, seasonal = TRUE)
print(summary(fit1))
checkresiduals(fit1)

# Pronóstico 12 meses

fc1 <- forecast(fit1, h = 12)
autoplot(fc1) + ggtitle("Pronóstico 12 meses - Hidrocarburos")
print(accuracy(fc1)[1, ])

# ========================================================
# Producto 2:GAS NATURAL
# ======================================================== 

# Subconjunto y ts
df2   <- df_all %>% filter(detalle == "Gas Natural") %>% arrange(periodo)
ts2   <- ts(df2$valor, frequency = 12,
            start = c(year(min(df2$periodo)), month(min(df2$periodo))),
            end = c(2024,12))

# Serie Mensual
autoplot(ts2) + ggtitle("Gas Natural — Serie Mensual")

# Diferencias
d1_2  <- diff(ts2)
ds2   <- diff(ts2, lag = 12)

ggarrange(
  autoplot(d1_2) + ggtitle("1ª Diferencia - Gas Natural"),
  autoplot(ds2)  + ggtitle("Diferencia Estacional (12) - Gas Natural"),
  nrow = 2
)

# ACF / PACF y ADF
ggAcf(ts2)  + ggtitle("ACF - Gas Natural")
ggPacf(ts2) + ggtitle("PACF - Gas Natural")
print(adf.test(ts2))
print(adf.test(d1_2))
print(adf.test(ds2))

# ARIMA Automático

fit2 <- auto.arima(ts2, seasonal = TRUE)
print(summary(fit2))
checkresiduals(fit2)

# Pronóstico 12 meses

fc2 <- forecast(fit2, h = 12)
autoplot(fc2) + ggtitle("Pronóstico 12 meses - Gas Natural")
print(accuracy(fc2)[1, ])


install.packages("rticles")
install.packages("tinytex")
tinytex::reinstall_tinytex(repository = "illinois")
tinytex::tlmgr_install("textcase")
file.exists("references.bib")


