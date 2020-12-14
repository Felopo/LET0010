library(rio)
library(lubridate)
library(readr)
library(ggplot2)
library(ggthemes)
library(cowplot)

pl_19_20 = rio::import("datos-procesados/pl_19-20.csv")

pl_18_19 = rio::import("datos-procesados/pl_18-19.csv")

pl_17_18 = rio::import("datos-procesados/pl_17-18.csv")

pl_16_17 = rio::import("datos-procesados/pl_16-17.csv")

pl_15_16 = rio::import("datos-procesados/pl_15-16.csv")

pl_14_15 = rio::import("datos-procesados/pl_14-15.csv")

# 19-20
# Partidos jugados por el city 19-20
pj_19_20 = pl_19_20[which(pl_19_20$HomeTeam == "Man City" | pl_19_20$AwayTeam == "Man City"),]
# Partidos de local 19-20
pjl_19_20 = pl_19_20[which(pl_19_20$HomeTeam == "Man City"),]
# Partidos de visita 19-20
pjv_19_20 = pl_19_20[which(pl_19_20$AwayTeam == "Man City"),]
# Promedio de goles por partido
prom_goles_19_20 = (sum(pl_19_20$FTHG) + sum(pl_19_20$FTAG)) / 380
# Promedio de goles del city por partido
prom_goles_city_19_20 = (sum(pjl_19_20$FTHG) + sum(pjv_19_20$FTAG)) / 38
# Promedio goles recibidos por partido
prom_rec_city_19_20 = (sum(pjl_19_20$FTAG) + sum(pjv_19_20$FTHG)) / 38
# Cantidad victorias
v_19_20 = nrow(pjl_19_20[which(pjl_19_20$FTR == "H"),]) + nrow(pjv_19_20[which(pjv_19_20$FTR == "A"),]) 
# Cantidad derrotas
d_19_20 = nrow(pjl_19_20[which(pjl_19_20$FTR == "A"),]) + nrow(pjv_19_20[which(pjv_19_20$FTR == "H"),])
# Cantidad empates
e_19_20 = nrow(pjl_19_20[which(pjl_19_20$FTR == "D"),]) + nrow(pjv_19_20[which(pjv_19_20$FTR == "D"),])
# Cantidad puntos
puntos_19_20 = (v_19_20 * 3) + (e_19_20 * 1)
# Promedio faltas por partido
prom_faltas_19_20 = (sum(pjl_19_20$HF) + sum(pjv_19_20$AF))  / 38
# Cantidad tarjetas amarillas
am_19_20 = sum(pjl_19_20$HY) + sum(pjv_19_20$AY)
# Cantidad tarjetas rojas
roj_19_20 = sum(pjl_19_20$HR) + sum(pjv_19_20$AR)
# Contra los big-six
victorias_19_20 = 0
if (pjl_19_20[which(pjl_19_20$AwayTeam == "Man United"),]$FTR == "H") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjv_19_20[which(pjv_19_20$HomeTeam == "Man United"),]$FTR == "A") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjl_19_20[which(pjl_19_20$AwayTeam == "Chelsea"),]$FTR == "H") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjv_19_20[which(pjv_19_20$HomeTeam == "Chelsea"),]$FTR == "A") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjl_19_20[which(pjl_19_20$AwayTeam == "Arsenal"),]$FTR == "H") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjv_19_20[which(pjv_19_20$HomeTeam == "Arsenal"),]$FTR == "A") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjl_19_20[which(pjl_19_20$AwayTeam == "Liverpool"),]$FTR == "H") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjv_19_20[which(pjv_19_20$HomeTeam == "Liverpool"),]$FTR == "A") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjl_19_20[which(pjl_19_20$AwayTeam == "Tottenham"),]$FTR == "H") {
  victorias_19_20 = victorias_19_20 + 1
}
if (pjv_19_20[which(pjv_19_20$HomeTeam == "Tottenham"),]$FTR == "A") {
  victorias_19_20 = victorias_19_20 + 1
}


# 18-19
# Partidos jugados por el city 18-19
pj_18_19 = pl_18_19[which(pl_18_19$HomeTeam == "Man City" | pl_18_19$AwayTeam == "Man City"),]
# Partidos de local 18-19
pjl_18_19 = pl_18_19[which(pl_18_19$HomeTeam == "Man City"),]
# Partidos de visita 18-19
pjv_18_19 = pl_18_19[which(pl_18_19$AwayTeam == "Man City"),]
# Promedio de goles por partido
prom_goles_18_19 = (sum(pl_18_19$FTHG) + sum(pl_18_19$FTAG)) / 380
# Promedio de goles del city por partido
prom_goles_city_18_19 = (sum(pjl_18_19$FTHG) + sum(pjv_18_19$FTAG)) / 38
# Promedio goles recibidos por partido
prom_rec_city_18_19 = (sum(pjl_18_19$FTAG) + sum(pjv_18_19$FTHG)) / 38
# Cantidad victorias
v_18_19 = nrow(pjl_18_19[which(pjl_18_19$FTR == "H"),]) + nrow(pjv_18_19[which(pjv_18_19$FTR == "A"),]) 
# Cantidad derrotas
d_18_19 = nrow(pjl_18_19[which(pjl_18_19$FTR == "A"),]) + nrow(pjv_18_19[which(pjv_18_19$FTR == "H"),])
# Cantidad empates
e_18_19 = nrow(pjl_18_19[which(pjl_18_19$FTR == "D"),]) + nrow(pjv_18_19[which(pjv_18_19$FTR == "D"),])
# Cantidad puntos
puntos_18_19 = (v_18_19 * 3) + (e_18_19 * 1)
# Promedio faltas por partido
prom_faltas_18_19 = (sum(pjl_18_19$HF) + sum(pjv_18_19$AF))  / 38
# Cantidad tarjetas amarillas
am_18_19 = sum(pjl_18_19$HY) + sum(pjv_18_19$AY)
# Cantidad tarjetas rojas
roj_18_19 = sum(pjl_18_19$HR) + sum(pjv_18_19$AR)
# Contra los big-six
victorias_18_19 = 0
if (pjl_18_19[which(pjl_18_19$AwayTeam == "Man United"),]$FTR == "H") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjv_18_19[which(pjv_18_19$HomeTeam == "Man United"),]$FTR == "A") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjl_18_19[which(pjl_18_19$AwayTeam == "Chelsea"),]$FTR == "H") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjv_18_19[which(pjv_18_19$HomeTeam == "Chelsea"),]$FTR == "A") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjl_18_19[which(pjl_18_19$AwayTeam == "Arsenal"),]$FTR == "H") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjv_18_19[which(pjv_18_19$HomeTeam == "Arsenal"),]$FTR == "A") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjl_18_19[which(pjl_18_19$AwayTeam == "Liverpool"),]$FTR == "H") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjv_18_19[which(pjv_18_19$HomeTeam == "Liverpool"),]$FTR == "A") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjl_18_19[which(pjl_18_19$AwayTeam == "Tottenham"),]$FTR == "H") {
  victorias_18_19 = victorias_18_19 + 1
}
if (pjv_18_19[which(pjv_18_19$HomeTeam == "Tottenham"),]$FTR == "A") {
  victorias_18_19 = victorias_18_19 + 1
}

# 17-18
# Partidos jugados por el city 17-18
pj_17_18 = pl_17_18[which(pl_17_18$HomeTeam == "Man City" | pl_17_18$AwayTeam == "Man City"),]
# Partidos de local 17-18
pjl_17_18 = pl_17_18[which(pl_17_18$HomeTeam == "Man City"),]
# Partidos de visita 17-18
pjv_17_18 = pl_17_18[which(pl_17_18$AwayTeam == "Man City"),]
# Promedio de goles por partido
prom_goles_17_18 = (sum(pl_17_18$FTHG) + sum(pl_17_18$FTAG)) / 380
# Promedio de goles del city por partido
prom_goles_city_17_18 = (sum(pjl_17_18$FTHG) + sum(pjv_17_18$FTAG)) / 38
# Promedio goles recibidos por partido
prom_rec_city_17_18 = (sum(pjl_17_18$FTAG) + sum(pjv_17_18$FTHG)) / 38
# Cantidad victorias
v_17_18 = nrow(pjl_17_18[which(pjl_17_18$FTR == "H"),]) + nrow(pjv_17_18[which(pjv_17_18$FTR == "A"),]) 
# Cantidad derrotas
d_17_18 = nrow(pjl_17_18[which(pjl_17_18$FTR == "A"),]) + nrow(pjv_17_18[which(pjv_17_18$FTR == "H"),])
# Cantidad empates
e_17_18 = nrow(pjl_17_18[which(pjl_17_18$FTR == "D"),]) + nrow(pjv_17_18[which(pjv_17_18$FTR == "D"),])
# Cantidad puntos
puntos_17_18 = (v_17_18 * 3) + (e_17_18 * 1)
# Promedio faltas por partido
prom_faltas_17_18 = (sum(pjl_17_18$HF) + sum(pjv_17_18$AF))  / 38
# Cantidad tarjetas amarillas
am_17_18 = sum(pjl_17_18$HY) + sum(pjv_17_18$AY)
# Cantidad tarjetas rojas
roj_17_18 = sum(pjl_17_18$HR) + sum(pjv_17_18$AR)
# Contra los big-six
victorias_17_18 = 0
if (pjl_17_18[which(pjl_17_18$AwayTeam == "Man United"),]$FTR == "H") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjv_17_18[which(pjv_17_18$HomeTeam == "Man United"),]$FTR == "A") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjl_17_18[which(pjl_17_18$AwayTeam == "Chelsea"),]$FTR == "H") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjv_17_18[which(pjv_17_18$HomeTeam == "Chelsea"),]$FTR == "A") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjl_17_18[which(pjl_17_18$AwayTeam == "Arsenal"),]$FTR == "H") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjv_17_18[which(pjv_17_18$HomeTeam == "Arsenal"),]$FTR == "A") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjl_17_18[which(pjl_17_18$AwayTeam == "Liverpool"),]$FTR == "H") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjv_17_18[which(pjv_17_18$HomeTeam == "Liverpool"),]$FTR == "A") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjl_17_18[which(pjl_17_18$AwayTeam == "Tottenham"),]$FTR == "H") {
  victorias_17_18 = victorias_17_18 + 1
}
if (pjv_17_18[which(pjv_17_18$HomeTeam == "Tottenham"),]$FTR == "A") {
  victorias_17_18 = victorias_17_18 + 1
}

# 16-17
# Partidos jugados por el city 16-17
pj_16_17 = pl_16_17[which(pl_16_17$HomeTeam == "Man City" | pl_16_17$AwayTeam == "Man City"),]
# Partidos de local 16-17
pjl_16_17 = pl_16_17[which(pl_16_17$HomeTeam == "Man City"),]
# Partidos de visita 16-17
pjv_16_17 = pl_16_17[which(pl_16_17$AwayTeam == "Man City"),]
# Promedio de goles por partido
prom_goles_16_17 = (sum(pl_16_17$FTHG) + sum(pl_16_17$FTAG)) / 380
# Promedio de goles del city por partido
prom_goles_city_16_17 = (sum(pjl_16_17$FTHG) + sum(pjv_16_17$FTAG)) / 38
# Promedio goles recibidos por partido
prom_rec_city_16_17 = (sum(pjl_16_17$FTAG) + sum(pjv_16_17$FTHG)) / 38
# Cantidad victorias
v_16_17 = nrow(pjl_16_17[which(pjl_16_17$FTR == "H"),]) + nrow(pjv_16_17[which(pjv_16_17$FTR == "A"),]) 
# Cantidad derrotas
d_16_17 = nrow(pjl_16_17[which(pjl_16_17$FTR == "A"),]) + nrow(pjv_16_17[which(pjv_16_17$FTR == "H"),])
# Cantidad empates
e_16_17 = nrow(pjl_16_17[which(pjl_16_17$FTR == "D"),]) + nrow(pjv_16_17[which(pjv_16_17$FTR == "D"),])
# Cantidad puntos
puntos_16_17 = (v_16_17 * 3) + (e_16_17 * 1)
# Promedio faltas por partido
prom_faltas_16_17 = (sum(pjl_16_17$HF) + sum(pjv_16_17$AF))  / 38
# Cantidad tarjetas amarillas
am_16_17 = sum(pjl_16_17$HY) + sum(pjv_16_17$AY)
# Cantidad tarjetas rojas
roj_16_17 = sum(pjl_16_17$HR) + sum(pjv_16_17$AR)
# Contra los big-six
victorias_16_17 = 0
if (pjl_16_17[which(pjl_16_17$AwayTeam == "Man United"),]$FTR == "H") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjv_16_17[which(pjv_16_17$HomeTeam == "Man United"),]$FTR == "A") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjl_16_17[which(pjl_16_17$AwayTeam == "Chelsea"),]$FTR == "H") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjv_16_17[which(pjv_16_17$HomeTeam == "Chelsea"),]$FTR == "A") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjl_16_17[which(pjl_16_17$AwayTeam == "Arsenal"),]$FTR == "H") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjv_16_17[which(pjv_16_17$HomeTeam == "Arsenal"),]$FTR == "A") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjl_16_17[which(pjl_16_17$AwayTeam == "Liverpool"),]$FTR == "H") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjv_16_17[which(pjv_16_17$HomeTeam == "Liverpool"),]$FTR == "A") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjl_16_17[which(pjl_16_17$AwayTeam == "Tottenham"),]$FTR == "H") {
  victorias_16_17 = victorias_16_17 + 1
}
if (pjv_16_17[which(pjv_16_17$HomeTeam == "Tottenham"),]$FTR == "A") {
  victorias_16_17 = victorias_16_17 + 1
}

# 15-16
# Partidos jugados por el city 15-16
pj_15_16 = pl_15_16[which(pl_15_16$HomeTeam == "Man City" | pl_15_16$AwayTeam == "Man City"),]
# Partidos de local 15-16
pjl_15_16 = pl_15_16[which(pl_15_16$HomeTeam == "Man City"),]
# Partidos de visita 15-16
pjv_15_16 = pl_15_16[which(pl_15_16$AwayTeam == "Man City"),]
# Promedio de goles por partido
prom_goles_15_16 = (sum(pl_15_16$FTHG) + sum(pl_15_16$FTAG)) / 380
# Promedio de goles del city por partido
prom_goles_city_15_16 = (sum(pjl_15_16$FTHG) + sum(pjv_15_16$FTAG)) / 38
# Promedio goles recibidos por partido
prom_rec_city_15_16 = (sum(pjl_15_16$FTAG) + sum(pjv_15_16$FTHG)) / 38
# Cantidad victorias
v_15_16 = nrow(pjl_15_16[which(pjl_15_16$FTR == "H"),]) + nrow(pjv_15_16[which(pjv_15_16$FTR == "A"),]) 
# Cantidad derrotas
d_15_16 = nrow(pjl_15_16[which(pjl_15_16$FTR == "A"),]) + nrow(pjv_15_16[which(pjv_15_16$FTR == "H"),])
# Cantidad empates
e_15_16 = nrow(pjl_15_16[which(pjl_15_16$FTR == "D"),]) + nrow(pjv_15_16[which(pjv_15_16$FTR == "D"),])
# Cantidad puntos
puntos_15_16 = (v_15_16 * 3) + (e_15_16 * 1)
# Promedio faltas por partido
prom_faltas_15_16 = (sum(pjl_15_16$HF) + sum(pjv_15_16$AF))  / 38
# Cantidad tarjetas amarillas
am_15_16 = sum(pjl_15_16$HY) + sum(pjv_15_16$AY)
# Cantidad tarjetas rojas
roj_15_16 = sum(pjl_15_16$HR) + sum(pjv_15_16$AR)
# Contra los big-six
victorias_15_16 = 0
if (pjl_15_16[which(pjl_15_16$AwayTeam == "Man United"),]$FTR == "H") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjv_15_16[which(pjv_15_16$HomeTeam == "Man United"),]$FTR == "A") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjl_15_16[which(pjl_15_16$AwayTeam == "Chelsea"),]$FTR == "H") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjv_15_16[which(pjv_15_16$HomeTeam == "Chelsea"),]$FTR == "A") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjl_15_16[which(pjl_15_16$AwayTeam == "Arsenal"),]$FTR == "H") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjv_15_16[which(pjv_15_16$HomeTeam == "Arsenal"),]$FTR == "A") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjl_15_16[which(pjl_15_16$AwayTeam == "Liverpool"),]$FTR == "H") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjv_15_16[which(pjv_15_16$HomeTeam == "Liverpool"),]$FTR == "A") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjl_15_16[which(pjl_15_16$AwayTeam == "Tottenham"),]$FTR == "H") {
  victorias_15_16 = victorias_15_16 + 1
}
if (pjv_15_16[which(pjv_15_16$HomeTeam == "Tottenham"),]$FTR == "A") {
  victorias_15_16 = victorias_15_16 + 1
}

# 14-15
# Partidos jugados por el city 14-15
pj_14_15 = pl_14_15[which(pl_14_15$HomeTeam == "Man City" | pl_14_15$AwayTeam == "Man City"),]
# Partidos de local 14-15
pjl_14_15 = pl_14_15[which(pl_14_15$HomeTeam == "Man City"),]
# Partidos de visita 14-15
pjv_14_15 = pl_14_15[which(pl_14_15$AwayTeam == "Man City"),]
# Promedio de goles por partido
prom_goles_14_15 = (sum(pl_14_15$FTHG) + sum(pl_14_15$FTAG)) / 380
# Promedio de goles del city por partido
prom_goles_city_14_15 = (sum(pjl_14_15$FTHG) + sum(pjv_14_15$FTAG)) / 38
# Promedio goles recibidos por partido
prom_rec_city_14_15 = (sum(pjl_14_15$FTAG) + sum(pjv_14_15$FTHG)) / 38
# Cantidad victorias
v_14_15 = nrow(pjl_14_15[which(pjl_14_15$FTR == "H"),]) + nrow(pjv_14_15[which(pjv_14_15$FTR == "A"),]) 
# Cantidad derrotas
d_14_15 = nrow(pjl_14_15[which(pjl_14_15$FTR == "A"),]) + nrow(pjv_14_15[which(pjv_14_15$FTR == "H"),])
# Cantidad empates
e_14_15 = nrow(pjl_14_15[which(pjl_14_15$FTR == "D"),]) + nrow(pjv_14_15[which(pjv_14_15$FTR == "D"),])
# Cantidad puntos
puntos_14_15 = (v_14_15 * 3) + (e_14_15 * 1)
# Promedio faltas por partido
prom_faltas_14_15 = (sum(pjl_14_15$HF) + sum(pjv_14_15$AF))  / 38
# Cantidad tarjetas amarillas
am_14_15 = sum(pjl_14_15$HY) + sum(pjv_14_15$AY)
# Cantidad tarjetas rojas
roj_14_15 = sum(pjl_14_15$HR) + sum(pjv_14_15$AR)
# Contra los big-six
victorias_14_15 = 0
if (pjl_14_15[which(pjl_14_15$AwayTeam == "Man United"),]$FTR == "H") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjv_14_15[which(pjv_14_15$HomeTeam == "Man United"),]$FTR == "A") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjl_14_15[which(pjl_14_15$AwayTeam == "Chelsea"),]$FTR == "H") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjv_14_15[which(pjv_14_15$HomeTeam == "Chelsea"),]$FTR == "A") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjl_14_15[which(pjl_14_15$AwayTeam == "Arsenal"),]$FTR == "H") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjv_14_15[which(pjv_14_15$HomeTeam == "Arsenal"),]$FTR == "A") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjl_14_15[which(pjl_14_15$AwayTeam == "Liverpool"),]$FTR == "H") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjv_14_15[which(pjv_14_15$HomeTeam == "Liverpool"),]$FTR == "A") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjl_14_15[which(pjl_14_15$AwayTeam == "Tottenham"),]$FTR == "H") {
  victorias_14_15 = victorias_14_15 + 1
}
if (pjv_14_15[which(pjv_14_15$HomeTeam == "Tottenham"),]$FTR == "A") {
  victorias_14_15 = victorias_14_15 + 1
}


# INFORME

# Promedio de goles por temporada
datos_prom_goles = data.frame(temporadas = c("14-15", "15-16", "16-17", "17-18", "18-19", "19-20"), 
                    promedios = c(p_14_15 = prom_goles_city_14_15, p_15_16 = prom_goles_city_15_16, 
                                  p_16_17 = prom_goles_city_16_17, p_17_18 = prom_goles_city_17_18, 
                                  p_18_19 = prom_goles_city_18_19, p_19_20 = prom_goles_city_19_20))

# Gráfico de promedio de goles por temporada
ggplot2::ggplot(data = datos_prom_goles, mapping = aes(x = temporadas, y = promedios)) +
  labs(x = "Temporada", y = "Promedio de goles por partido",
       title = "Promedio de goles del Manchester City por temporada", 
       caption = "Figura 1.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") + 
  scale_color_colorblind() + 
  geom_point(size = 5, mapping = aes(color = temporadas)) + 
  geom_line(aes(group = 1))

ggsave(here::here("figuras", "promedio-goles-city.png"))

# Promedio de goles recibidos por temporada
temporadas = c("14-15", "15-16", "16-17", "17-18", "18-19", "19-20")
datos_prom_rec = data.frame(temporadas = c("14-15", "15-16", "16-17", "17-18", "18-19", "19-20"), 
                    promedios = c(p_14_15 = prom_rec_city_14_15, p_15_16 = prom_rec_city_15_16, 
                                  p_16_17 = prom_rec_city_16_17, p_17_18 = prom_rec_city_17_18, 
                                  p_18_19 = prom_rec_city_18_19, p_19_20 = prom_rec_city_19_20))

# Gráfico de promedio de goles recibidos  por temporada
ggplot2::ggplot(data = datos_prom_rec, mapping = aes(x = temporadas, y = promedios)) +
  labs(x = "Temporada", y = "Promedio de goles recibidos por partido",
       title = "Promedio de goles recibidos por el Manchester City por temporada", 
       caption = "Figura 2.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right") + 
  scale_color_colorblind() + 
  geom_point(size = 5, mapping = aes(color = temporadas)) + 
  geom_line(aes(group = 1))
  
ggsave(here::here("figuras", "promedio-recibidos-city.png"))

# Diferencia de goles por temporada

dif_goles_14_15 = sum(pjl_14_15$FTHG) + sum(pjv_14_15$FTAG) - sum(pjl_14_15$FTAG) - sum(pjv_14_15$FTHG)
dif_goles_15_16 = sum(pjl_15_16$FTHG) + sum(pjv_15_16$FTAG) - sum(pjl_15_16$FTAG) - sum(pjv_15_16$FTHG)
dif_goles_16_17 = sum(pjl_16_17$FTHG) + sum(pjv_16_17$FTAG) - sum(pjl_16_17$FTAG) - sum(pjv_16_17$FTHG)
dif_goles_17_18 = sum(pjl_17_18$FTHG) + sum(pjv_17_18$FTAG) - sum(pjl_17_18$FTAG) - sum(pjv_17_18$FTHG)
dif_goles_18_19 = sum(pjl_18_19$FTHG) + sum(pjv_18_19$FTAG) - sum(pjl_18_19$FTAG) - sum(pjv_18_19$FTHG)
dif_goles_19_20 = sum(pjl_19_20$FTHG) + sum(pjv_19_20$FTAG) - sum(pjl_19_20$FTAG) - sum(pjv_19_20$FTHG)

# Contra los big six

datos_vic = data.frame(temporadas = temporadas, 
                       victorias = c(victorias_14_15, victorias_15_16, victorias_16_17, 
                                     victorias_17_18, victorias_18_19, victorias_19_20))

# Gráfico de cantidad de victorias contra los big six
ggplot2::ggplot(data = datos_vic) + 
  geom_bar(aes(x = temporadas, y = victorias), stat = "identity", fill = temporadas) +
  labs(x = "Temporada", y = "Cantidad partidos ganados a los Big Six",
       title = "Partidos ganados a los Big Six por temporada", caption = "Figura 3.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")

ggsave(here::here("figuras", "ganados-a-big-six.png"))

# Faltas y tarjetas por partido
datos_faltas = data.frame(temporadas,
                          prom_faltas = c(prom_faltas_14_15, prom_faltas_15_16, prom_faltas_16_17,
                                          prom_faltas_17_18, prom_faltas_18_19, prom_faltas_19_20))
grafico_prom_faltas = 
  ggplot2::ggplot(data = datos_faltas) + 
            geom_bar(aes(x = temporadas, y  = prom_faltas), stat = "identity", fill = temporadas) +
            labs(x = "Temporada", y = "Promedio de faltas por partido",
                 title = "Promedio de faltas por temporada", caption = "Figura 4.") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
datos_tarjetas = data.frame(temporadas, 
                            amarillas = c(am_14_15, am_15_16, am_16_17, am_17_18, am_18_19, 
                                          am_19_20),
                            rojas = c(roj_14_15, roj_15_16, roj_16_17, roj_17_18, roj_18_19, 
                                      roj_19_20))
grafico_tarjetas = 
  ggplot2::ggplot(data = datos_tarjetas) +
            geom_bar(stat = "identity", fill = temporadas, aes(x = temporadas, y = amarillas)) + 
            geom_text(aes(x = temporadas, y = amarillas, label=amarillas), 
                      position=position_dodge(width=0.9), vjust=-0.25) + 
            labs(x = "Temporada", y = "Cantidad tarjetas amarillas por temporada",
                 title = "Total de tarjetas amarillas", caption = "Figura 5.") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
plot_grid(grafico_prom_faltas, grafico_tarjetas)

ggsave(here::here("figuras", "prom-faltas-y-amarillas.png"))

# Info extra 19-20
# Primavera y Verano  21/03 hasta 23/09
# Otoño y Invierno 23/09 hasta 21/03
# Puntos ganados
puntos_p_v = 0
puntos_o_i = 0
dias_p_v = 0
dias_o_i = 0
# Para los partidos de local
for (i in 1:nrow(pjl_17_18)) {
  if (dmy("21-03-2019") < dmy(pjl_17_18$Date[i]) & dmy("23-09-2019") > dmy(pjl_17_18$Date[i]) | 
      dmy("21-03-2020") < dmy(pjl_17_18$Date[i]) & dmy("23-09-2020") > dmy(pjl_17_18$Date[i])) {
    dias_p_v = dias_p_v + 1
    if (pjl_17_18$FTR[i] == "H"){
      puntos_p_v = puntos_p_v + 3
    } else if (pjl_17_18$FTR[i] == "D"){
      puntos_p_v = puntos_p_v + 1
    }
  }
  if (dmy("23-09-2019") < dmy(pjl_17_18$Date[i]) & dmy("21-03-2020") > dmy(pjl_17_18$Date[i])) {
    dias_o_i = dias_o_i + 1
    if (pjl_17_18$FTR[i] == "H"){
      puntos_o_i = puntos_o_i + 3
    } else if (pjl_17_18$FTR[i] == "D"){
      puntos_o_i = puntos_o_i + 1
    }
  }
}
# Para los partidos de visita
for (i in 1:nrow(pjv_17_18)) {
  if (dmy("21-03-2019") < dmy(pjv_17_18$Date[i]) & dmy("23-09-2019") > dmy(pjv_17_18$Date[i]) | 
      dmy("21-03-2020") < dmy(pjv_17_18$Date[i]) & dmy("23-09-2020") > dmy(pjv_17_18$Date[i])) {
    dias_p_v = dias_p_v + 1
    if (pjv_17_18$FTR[i] == "A"){
      puntos_p_v = puntos_p_v + 3
    } else if (pjv_17_18$FTR[i] == "D"){
      puntos_p_v = puntos_p_v + 1
    }
  }
  if (dmy("23-09-2019") < dmy(pjv_17_18$Date[i]) & dmy("21-03-2020") > dmy(pjv_17_18$Date[i])) {
    dias_o_i = dias_o_i + 1
    if (pjv_17_18$FTR[i] == "A"){
      puntos_o_i = puntos_o_i + 3
    } else if (pjv_17_18$FTR[i] == "D"){
      puntos_o_i = puntos_o_i + 1
    }
  }
}
porcentaje_p_v = (dias_p_v / 38) * 100
porcentaje_o_i = (dias_o_i / 38) * 100

# Info extra 18-19
# Primavera y Verano  21/03 hasta 23/09
# Otoño y Invierno 23/09 hasta 21/03
# Puntos ganados
puntos_p_v = 0
puntos_o_i = 0
dias_p_v = 0
dias_o_i = 0
# Para los partidos de local
for (i in 1:nrow(pjl_18_19)) {
  if (dmy("21-03-2018") < dmy(pjl_18_19$Date[i]) & dmy("23-09-2018") > dmy(pjl_18_19$Date[i]) | 
      dmy("21-03-2019") < dmy(pjl_18_19$Date[i]) & dmy("23-09-2019") > dmy(pjl_18_19$Date[i])) {
    dias_p_v = dias_p_v + 1
    if (pjl_18_19$FTR[i] == "H"){
      puntos_p_v = puntos_p_v + 3
    } else if (pjl_18_19$FTR[i] == "D"){
      puntos_p_v = puntos_p_v + 1
    }
  }
  if (dmy("23-09-2018") < dmy(pjl_18_19$Date[i]) & dmy("21-03-2019") > dmy(pjl_18_19$Date[i])) {
    dias_o_i = dias_o_i + 1
    if (pjl_18_19$FTR[i] == "H"){
      puntos_o_i = puntos_o_i + 3
    } else if (pjl_18_19$FTR[i] == "D"){
      puntos_o_i = puntos_o_i + 1
    }
  }
}
# Para los partidos de visita
for (i in 1:nrow(pjv_18_19)) {
  if (dmy("21-03-2018") < dmy(pjv_18_19$Date[i]) & dmy("23-09-2018") > dmy(pjv_18_19$Date[i]) | 
      dmy("21-03-2019") < dmy(pjv_18_19$Date[i]) & dmy("23-09-2019") > dmy(pjv_18_19$Date[i])) {
    dias_p_v = dias_p_v + 1
    if (pjv_18_19$FTR[i] == "A"){
      puntos_p_v = puntos_p_v + 3
    } else if (pjv_18_19$FTR[i] == "D"){
      puntos_p_v = puntos_p_v + 1
    }
  }
  if (dmy("23-09-2018") < dmy(pjv_18_19$Date[i]) & dmy("21-03-2019") > dmy(pjv_18_19$Date[i])) {
    dias_o_i = dias_o_i + 1
    if (pjv_18_19$FTR[i] == "A"){
      puntos_o_i = puntos_o_i + 3
    } else if (pjv_18_19$FTR[i] == "D"){
      puntos_o_i = puntos_o_i + 1
    }
  }
}
porcentaje_p_v = (dias_p_v / 38) * 100
porcentaje_o_i = (dias_o_i / 38) * 100
