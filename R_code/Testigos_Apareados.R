library(dplyr)
library(ggplot2)

library(nlme)
library(emmeans)

datos_testigosap <- read.table('Datos/Dia 1/Testigos_apareados.txt',
sep = '\t',
header = TRUE)


head(datos_testigosap)

ggplot(datos_testigosap, aes(Posicion, Rendimiento, color = Hibrido == 'Testigo')) +
  geom_point()


mdl_errores_indep <- aov(Rendimiento ~ Hibrido, 
data = datos_testigosap)

mdl_errores_corr_exp <- gls(Rendimiento ~ Hibrido, 
correlation = corExp(form=~Posicion,
nugget=FALSE,
metric='euclidean'),
data = datos_testigosap)


mdl_errores_corr_gau <- gls(Rendimiento ~ Hibrido, 
correlation = corGaus(form=~Posicion,
nugget=FALSE,
metric='euclidean'),
data = datos_testigosap)

mdl_errores_corr_lin <- gls(Rendimiento ~ Hibrido, 
correlation = corLin(form=~Posicion,
nugget=FALSE,
metric='euclidean'),
data = datos_testigosap)

mdl_errores_corr_spher <- gls(Rendimiento ~ Hibrido, 
correlation = corSpher(form=~Posicion,
nugget=FALSE,
metric='euclidean'),
data = datos_testigosap)

mdl_errores_corr_rationalquad<- gls(Rendimiento ~ Hibrido, 
correlation = corRatio(form=~Posicion,
nugget=FALSE,
metric='euclidean'),
data = datos_testigosap)




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

datos_testigosap$resid_corr_exp <- 
  residuals(mdl_errores_corr_exp, type = 'normalized')
datos_testigosap$pred_corr_exp <-
  predict(mdl_errores_corr_exp)

ggplot(datos_testigosap, aes(sample = resid_corr_exp)) +
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
        data = datos_testigosap,
        mapping = aes(x = pred_corr_exp, y = resid_corr_exp)
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
        data = datos_testigosap,
        mapping = aes(x = Hibrido, y = resid_corr_exp)
        ) +
        geom_boxplot(
            colour = "black",
            fill = "aquamarine3",
            size = 2,
            shape = 21
        ) +
        theme_bw() +
        labs(y = "Standardised Residual", x = "Hibrido")


joint_tests(mdl_errores_corr_exp)


emmeans(mdl_errores_corr_exp, 'Hibrido') |>
  as.data.frame() |>
  arrange(emmean)

