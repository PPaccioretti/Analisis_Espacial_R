
# Manejo de datos
library(dplyr) 
# Manejo de datos espaciales
library(sf) 

# Graficos
library(ggplot2) 
# Mapas y graficos de datos espaciales
library(tmap) 

# Ajuste de modelos Mixtos
library(nlme) 
# Comparacion de medias
library(emmeans) 
# library(multcomp)


datos_ensayo_p <- read_sf('Datos/Dia 1/Ensayo_P/Ensayo_P.shp')

datos_ensayo_p <-
datos_ensayo_p |>
  mutate(BLOQUE = as.character(BLOQUE))

datos_ensayo_p <- 
  cbind(datos_ensayo_p, 
        st_coordinates(st_centroid(datos_ensayo_p)))

head(datos_ensayo_p)

tm_shape(datos_ensayo_p) +
  tm_polygons(fill = 'Rto')

tm_shape(datos_ensayo_p) +
  tm_polygons(fill = 'ZM',
  fill.scale = tm_scale_categorical())

tm_shape(datos_ensayo_p) +
  tm_polygons(fill = 'BLOQUE',
  fill.scale = tm_scale_categorical())

tm_shape(datos_ensayo_p) +
  tm_polygons(fill = 'TRATA',
  fill.scale = tm_scale_categorical())

ggplot(datos_ensayo_p, aes(TRATA, Rto, color = ZM, group = ZM)) +
  stat_summary()


mdl_errores_indep <- lme(Rto ~ ZM + TRATA + ZM*TRATA, 
list(BLOQUE=pdIdent(~1)),
data = datos_ensayo_p)


mdl_errores_corr_exp <- lme(Rto ~ ZM + TRATA + ZM*TRATA, 
random = list(BLOQUE=pdIdent(~1)),
correlation = corExp(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_p)

mdl_errores_corr_gau <- lme(Rto ~ ZM + TRATA + ZM*TRATA, 
random = list(BLOQUE=pdIdent(~1)),
correlation = corGaus(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_p)

mdl_errores_corr_lin <- lme(Rto ~ ZM + TRATA + ZM*TRATA, 
random = list(BLOQUE=pdIdent(~1)),
correlation = corLin(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_p)

mdl_errores_corr_spher <- lme(Rto ~ ZM + TRATA + ZM*TRATA, 
random = list(BLOQUE=pdIdent(~1)),
correlation = corSpher(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_p)

mdl_errores_corr_rationalquad<- lme(Rto ~ ZM + TRATA + ZM*TRATA, 
random = list(BLOQUE=pdIdent(~1)),
correlation = corRatio(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_p)


data.frame(
  Modelo = c('Errores Independientes',
             'Correlacion Espacial (Exp)',
             'Correlacion Espacial (Gaussian)',
             'Correlacion Espacial (Linear)',
             'Correlacion Espacial (Spheric)',
             'Correlacion Espacial (Rational Quadratic)'),
  AIC = c(AIC(mdl_errores_indep), 
          AIC(mdl_errores_corr_exp),
          AIC(mdl_errores_corr_gau),
          AIC(mdl_errores_corr_lin),
          AIC(mdl_errores_corr_spher),
          AIC(mdl_errores_corr_rationalquad)),
  BIC = c(BIC(mdl_errores_indep), 
          BIC(mdl_errores_corr_exp),
          BIC(mdl_errores_corr_gau),
          BIC(mdl_errores_corr_lin),
          BIC(mdl_errores_corr_spher),
          BIC(mdl_errores_corr_rationalquad))
) |> 
  arrange(AIC, BIC)

datos_ensayo_p$resid_corr_rationalquad <- 
  residuals(mdl_errores_corr_rationalquad, type = 'normalized')
datos_ensayo_p$pred_corr_rationalquad <- 
  predict(mdl_errores_corr_rationalquad)

ggplot(datos_ensayo_p, aes(sample = resid_corr_rationalquad)) +
        geom_qq(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        geom_qq_line() +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Theoretical")

ggplot(data = datos_ensayo_p,
        aes(x = pred_corr_rationalquad, y = resid_corr_rationalquad)
        ) +
        geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Fitted Value")

ggplot(data = datos_ensayo_p,
        aes(x = BLOQUE, y = resid_corr_rationalquad)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Bloque")

ggplot(data = datos_ensayo_p,
        aes(x = TRATA, y = resid_corr_rationalquad)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Tratamiento")


joint_tests(mdl_errores_corr_rationalquad)


modelb <- emmeans(mdl_errores_corr_rationalquad, c("ZM", "TRATA"), type = "response")
comparaciones <- 
  multcomp::cld(modelb, Letters = LETTERS, alpha = 0.05)  |>
  as.data.frame(row.names = NULL) |>
  dplyr::rename(mean = emmean) 

comparaciones

ggplot(comparaciones, aes(x = TRATA, y = mean, fill = ZM)) +
  geom_col(
    position = position_dodge(width = 0.5),
    width = 0.5,
    color = "black") +
  geom_text(aes(label = .group), 
  position = position_dodge(width = 0.5),
  vjust = -0.5, hjust = 0.5, size = 5) +
  labs(
    x = "Tratamiento",
    y = "Rendimiento promedio ajustado",
    fill = "Zona"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
