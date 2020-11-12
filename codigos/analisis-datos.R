library(rio)
library(ggplot2)
library(lubridate)

pl_19_20 = rio::import("pl 2019-2020.csv")
pl_19_20 = pl_19_20[1:24]

pl_18_19 = rio::import("pl 2018-2019.csv")
pl_18_19 = pl_18_19[1:23]

pl_17_18 = rio::import("pl 2017-2018.csv")
pl_17_18 = pl_17_18[1:23]

pl_16_17 = rio::import("pl 2016-2017.csv")
pl_16_17 = pl_16_17[1:23]

pl_15_16 = rio::import("pl 2015-2016.csv")
pl_15_16 = pl_15_16[1:23]

pl_14_15 = rio::import("pl 2014-2015.csv")
pl_14_15 = pl_14_15[1:23]

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
