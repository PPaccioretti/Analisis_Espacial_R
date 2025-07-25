library(dplyr)
library(ggplot2)

library(nlme)
library(emmeans)

datos_trigo <- read.table('Datos/Dia 1/Trigo.txt',
sep = '\t',
header = TRUE,
dec = ',')

datos_trigo <- 
  datos_trigo |>
  mutate(Bloque = as.character(Bloque))


head(datos_trigo)

ggplot(datos_trigo, aes(Bloque, Fila)) +
  geom_raster(aes(fill = Genotipo))

ggplot(datos_trigo, aes(Bloque, Fila)) +
  geom_raster(aes(fill = Rendimiento))


ggplot(datos_trigo, aes(Genotipo, Rendimiento)) +
  stat_summary(fun.data = mean_se)

mdl_errores_indep <- lme(Rendimiento ~ Genotipo, 
list(Bloque=pdIdent(~1)),
data = datos_trigo)

datos_trigo$resid_indep <- 
  residuals(mdl_errores_indep, type = 'normalized')
datos_trigo$pred_indep <- 
  predict(mdl_errores_indep)

ggplot(datos_trigo, aes(sample = resid_indep)) +
        geom_qq(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        geom_qq_line() +
        theme_bw() +
        labs(y = "Standardised Residual", 
        x = "Theoretical")

ggplot(
        data = datos_trigo,
        mapping = aes(x = pred_indep, y = resid_indep)
        ) +
        geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual",
         x = "Fitted Value")



ggplot(
        data = datos_trigo,
        mapping = aes(x = Bloque, y = resid_indep)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Bloque")



ggplot(
        data = datos_trigo,
        mapping = aes(x = Genotipo, y = resid_indep)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Genotipo")


joint_tests(mdl_errores_indep)


emmeans(mdl_errores_indep, 'Genotipo') |>
  as.data.frame() |>
  arrange(emmean)


mdl_errores_corr_exp <- lme(Rendimiento ~ Genotipo, 
random = list(Bloque=pdIdent(~1)),
correlation = corExp(form=~Bloque_vis + Fila,
nugget=FALSE,
metric='euclidean'),
data = datos_trigo)


mdl_errores_corr_gau <- lme(Rendimiento ~ Genotipo, 
random = list(Bloque=pdIdent(~1)),
correlation = corGaus(form=~Bloque_vis + Fila,
nugget=FALSE,
metric='euclidean'),
data = datos_trigo)

mdl_errores_corr_lin <- lme(Rendimiento ~ Genotipo, 
random = list(Bloque=pdIdent(~1)),
correlation = corLin(form=~Bloque_vis + Fila,
nugget=FALSE,
metric='euclidean'),
data = datos_trigo)

mdl_errores_corr_spher <- lme(Rendimiento ~ Genotipo, 
random = list(Bloque=pdIdent(~1)),
correlation = corSpher(form=~Bloque_vis + Fila,
nugget=FALSE,
metric='euclidean'),
data = datos_trigo)

mdl_errores_corr_rationalquad<- lme(Rendimiento ~ Genotipo, 
random = list(Bloque=pdIdent(~1)),
correlation = corRatio(form=~Bloque_vis + Fila,
nugget=FALSE,
metric='euclidean'),
data = datos_trigo)




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
          AIC(mdl_errores_corr_rationalquad)
          ),
  BIC = c(BIC(mdl_errores_indep), 
          BIC(mdl_errores_corr_exp),
          BIC(mdl_errores_corr_gau),
          BIC(mdl_errores_corr_lin),
          BIC(mdl_errores_corr_spher),
          BIC(mdl_errores_corr_rationalquad))
) |> 
  arrange(AIC, BIC)

datos_trigo$resid_corr_lin <- 
  residuals(mdl_errores_corr_lin, type = 'normalized')
datos_trigo$pred_corr_lin <- 
  predict(mdl_errores_corr_lin)

ggplot(datos_trigo, aes(sample = resid_corr_lin)) +
        geom_qq(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        geom_qq_line() +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Theoretical")

ggplot(
        data = datos_trigo,
        mapping = aes(x = pred_corr_lin, y = resid_corr_lin)
        ) +
        geom_point(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Fitted Value")



ggplot(
        data = datos_trigo,
        mapping = aes(x = Bloque, y = resid_corr_lin)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Bloque")



ggplot(
        data = datos_trigo,
        mapping = aes(x = Genotipo, y = resid_corr_lin)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Genotipo")


joint_tests(mdl_errores_corr_lin)


emmeans(mdl_errores_corr_lin, 'Genotipo') |>
  as.data.frame() |>
  arrange(emmean)

