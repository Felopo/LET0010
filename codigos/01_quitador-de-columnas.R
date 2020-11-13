# Quitamos los datos sobre apuestas
pl_19_20 = rio::import("datos-sin-procesar/pl 2019-2020.csv")
pl_19_20 = pl_19_20[1:24]
pl_19_20 = pl_19_20[, !(names(pl_19_20) == "Time")]

pl_18_19 = rio::import("datos-sin-procesar/pl 2018-2019.csv")
pl_18_19 = pl_18_19[1:23]

pl_17_18 = rio::import("datos-sin-procesar/pl 2017-2018.csv")
pl_17_18 = pl_17_18[1:23]

pl_16_17 = rio::import("datos-sin-procesar/pl 2016-2017.csv")
pl_16_17 = pl_16_17[1:23]

pl_15_16 = rio::import("datos-sin-procesar/pl 2015-2016.csv")
pl_15_16 = pl_15_16[1:23]

pl_14_15 = rio::import("datos-sin-procesar/pl 2014-2015.csv")
pl_14_15 = pl_14_15[1:23]

# Escrbimos los datos de cada temporada, sin los datos sobre apuestas
readr::write_csv(pl_14_15, here::here("datos-procesados", "pl_14-15.csv"))
readr::write_csv(pl_15_16, here::here("datos-procesados", "pl_15-16.csv"))
readr::write_csv(pl_16_17, here::here("datos-procesados", "pl_16-17.csv"))
readr::write_csv(pl_17_18, here::here("datos-procesados", "pl_17-18.csv"))
readr::write_csv(pl_18_19, here::here("datos-procesados", "pl_18-19.csv"))
readr::write_csv(pl_19_20, here::here("datos-procesados", "pl_19-20.csv"))
