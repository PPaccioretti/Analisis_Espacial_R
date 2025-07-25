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


set.seed(2025)
datos_ensayo_n <- read_sf('Datos/Dia 1/Ensayo/EnsayoCurso.shp')

datos_ensayo_n <-
datos_ensayo_n |>
  mutate(Z = as.character(Z),
        N2 = N * N) |>
  group_by(N, Z) |>
  slice_sample(prop = 0.15)

datos_ensayo_n <- 
  cbind(datos_ensayo_n, 
        st_coordinates(datos_ensayo_n))

head(datos_ensayo_n)

tm_shape(datos_ensayo_n) +
  tm_dots(fill = 'Rto')

tm_shape(datos_ensayo_n) +
  tm_dots(fill = 'Z',
  fill.scale = tm_scale_categorical())

tm_shape(datos_ensayo_n) +
  tm_dots(fill = 'N',
  fill.scale = tm_scale_categorical())

ggplot(datos_ensayo_n, aes(N, Rto, color = Z, group = Z)) +
  stat_summary()


mdl_errores_indep <- gls(Rto ~ N + N2 + Z + N*Z + N2*Z + N*N2*Z,  
data = datos_ensayo_n)


mdl_errores_corr_exp <- gls(Rto ~ N + N2 + Z + N*Z + N2*Z + N*N2*Z,  
correlation = corExp(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_n)

mdl_errores_corr_gau <- gls(Rto ~ N + N2 + Z + N*Z + N2*Z + N*N2*Z,  
correlation = corGaus(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_n)

mdl_errores_corr_lin <- gls(Rto ~ N + N2 + Z + N*Z + N2*Z + N*N2*Z,  
correlation = corLin(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_n)

mdl_errores_corr_spher <- gls(Rto ~ N + N2 + Z + N*Z + N2*Z + N*N2*Z,  
correlation = corSpher(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_n)

mdl_errores_corr_rationalquad<- gls(Rto ~ N + N2 + Z + N*Z + N2*Z + N*N2*Z,  
correlation = corRatio(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_n)


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

datos_ensayo_n_noout <- 
  datos_ensayo_n[abs(residuals(mdl_errores_corr_exp, type = 'normalized')) < 3,]

mdl_errores_corr_exp <- gls(Rto ~ N + N2 + Z + N*Z + N2*Z,  
correlation = corExp(form=~ X + Y,
nugget=FALSE,
metric='euclidean'),
data = datos_ensayo_n_noout)

datos_ensayo_n_noout$resid_corr_exp <- 
  residuals(mdl_errores_corr_exp, type = 'normalized')
datos_ensayo_n_noout$pred_corr_exp <- 
  predict(mdl_errores_corr_exp)

ggplot2::ggplot(datos_ensayo_n_noout, ggplot2::aes(sample = resid_corr_exp)) +
        ggplot2::geom_qq(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::geom_qq_line() +
        ggplot2::theme_bw() +
        ggplot2::labs(y = "Standardised Residual", x = "Theoretical")

ggplot2::ggplot(
        data = datos_ensayo_n_noout,
        mapping = ggplot2::aes(x = pred_corr_exp, y = resid_corr_exp)
        ) +
        ggplot2::geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::theme_bw() +
        ggplot2::labs(y = "Standardised Residual", x = "Fitted Value")

ggplot2::ggplot(
        data = datos_ensayo_n_noout,
        mapping = ggplot2::aes(x = Z, y = resid_corr_exp)
        ) +
        ggplot2::geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::theme_bw() +
        ggplot2::labs(y = "Standardised Residual", x = "Zona")

ggplot2::ggplot(
        data = datos_ensayo_n_noout,
        mapping = ggplot2::aes(x = N, y = resid_corr_exp)
        ) +
        ggplot2::geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        ggplot2::theme_bw() +
        ggplot2::labs(y = "Standardised Residual", x = "Nitrogeno")


joint_tests(mdl_errores_corr_exp)


modelb <- emmeans(mdl_errores_corr_exp, c("Z", "N"), type = "response")
multcomp::cld(modelb, Letters = LETTERS, alpha = 0.05)  |>
  as.data.frame(row.names = NULL) |>
  dplyr::rename(mean = emmean) 


coefficients(mdl_errores_corr_exp) 


range_n_by_zone <- datos_ensayo_n_noout |>
  st_drop_geometry() |>
  group_by(Z) |>
  summarise(min_N = min(N), max_N = max(N))

grid_pred <- range_n_by_zone |>
  rowwise() |>
  do({
    data.frame(
      Z = .$Z,
      N = seq(.$min_N, .$max_N, length.out = 100)
    )
  }) |>
  ungroup() |>
  mutate(
    N2 = N^2
  )



grid_pred$Rto_pred <- predict(mdl_errores_corr_exp, newdata = grid_pred)

opt_n_by_zone <- grid_pred %>%
  group_by(Z) %>%
  slice_max(Rto_pred, n = 1, with_ties = FALSE) |>
  select(Z, N, Rto_pred)

opt_n_by_zone



ggplot(grid_pred, aes(x = N, y = Rto_pred, color = Z)) +
  geom_line() +
  geom_point(data = opt_n_by_zone, aes(x = N, y = Rto_pred), size = 3) +
  labs(x = "Dosis de N", y = "Rendimiento predicho (Kg/ha)") +
  theme_minimal()

