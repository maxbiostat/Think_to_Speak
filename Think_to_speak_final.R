## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----message=FALSE, cache=TRUE------------------------------------------------
library("cmdstanr")
library("rstan")
library("ggplot2")
library("reshape2")
library("tidyverse")
library("kableExtra")
library("posterior")
library("scales")


## ----cache=TRUE---------------------------------------------------------------
summy <- function(x, alpha = 0.95){
  nn <- colnames(x)
  mm <- colMeans(x)
  mn <- apply(x, 2, median)
  ll <- apply(x, 2, quantile, probs = (1 - alpha)/2)
  uu <- apply(x, 2, quantile, probs = (1 + alpha)/2) 
  out <- data.frame(variable = nn, 
                    Mean =  mm,
                    Median = mn,
                    Lower = ll,
                    Upper = uu)
  return(out)
}


## ----cache=TRUE---------------------------------------------------------------
portuguese.manner.data <- data.frame(
  speaker = paste0("2018-", 1:14),
  total_types = c(23, 20, 11, 10, 10,
                  12, 9, 13, 13, 12,
                  18, 13, 14, 20),
  types_manner = c(2, 1, 1, 2, 1,
                   1, 1, 2, 1, 2,
                   1, 1, 1, 1),
  occurrences_manner = c(3, 4, 1, 3, 1,
                         1, 1, 4, 1, 3,
                         1, 2, 2, 4)
)

english.manner.data <- data.frame(
  speaker = c("20-a", "20-b", "20-c", "20-d",
              "20-e", "20-f", "20-g", "20-h",
              "20-i", "20-j", "20-k", "20-l"
  ),
  total_types = c(14, 17, 28, 13,
                  18, 21, 18, 22,
                  15, 15, 19, 18),
  types_manner = c(2, 3, 6, 4,
                   6, 4, 2, 3,
                   2, 6, 4, 2),
  occurrences_manner = c(2, 5, 10, 7,
                         8, 6, 3, 6,
                         3, 8, 4, 2)
)

tableI <- data.frame(
  language = c("Spanish", "Italian", "French", "German",
               "BP", "English"),
  total_types = c(39, 60, 53, 67,
                  21, 41),
  types_manner = c(7, 10, 3, 23,
                   5, 20),
  nspeakers = c(12, 12, 12, 12,
                nrow(portuguese.manner.data),
                nrow(english.manner.data)),
  occurrence_manner = c(11, 16, 14, 62,
                        sum(portuguese.manner.data$occurrences_manner),
                        sum(english.manner.data$occurrences_manner))
)
table_I <- rename(tableI,
                  'Language' = language,
                  'Total types' = total_types,
                  'Manner types' = types_manner,
                  'No. speakers'= nspeakers,
                  'Occurrences (manner)' = occurrence_manner)


## ----tabdata, echo = FALSE, results='markup', cache=TRUE----------------------
knitr::kable(table_I, booktabs = TRUE, caption = "Aggregated on manner verb usage in six languages." ) %>%  kable_styling(position = "center")


## ----figdata,  echo=FALSE, results='asis', fig.align = 'center', fig.cap = "Boxplots of disaggregated counts for total types of movement verbs, types manner verbs and occurrences of manner verbs.", cache=TRUE----
disagg.data <- data.frame(
  Language = c(rep("Portuguese", 14), rep("English", 12)),
  Total = c(portuguese.manner.data$total_types, english.manner.data$total_types),
  Types = c(portuguese.manner.data$types_manner, english.manner.data$types_manner),
  Occurrences = c(portuguese.manner.data$occurrences_manner, english.manner.data$occurrences_manner)
)
suppressMessages(
  disagg.forPlot <- melt(disagg.data,
                         variable.name = "Variable",
                         value.name = "count")
)

fig0 <- ggplot(data = disagg.forPlot,
               aes(x = Language, y = count,
                   colour = Language, fill = Language)) +
  geom_boxplot(alpha = .4) + 
  geom_jitter(colour = "black", size = 0.4, alpha = 0.9) +
  scale_y_continuous("Counts",
                     breaks = trans_breaks(identity, identity, n = 4),
                     expand = c(0, 0)) + 
  facet_grid(.~Variable, scales = "free_y") +
  theme_bw(base_size = 16) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
fig0


## ----warning=FALSE, message=FALSE, error=FALSE--------------------------------
data.proportions <- list(
  K = nrow(tableI),
  N = tableI$total_types,
  y = tableI$types_manner
)

prop_model <- cmdstanr::cmdstan_model("stan/proportions.stan")

output <- capture.output(
  fit.proportions <- 
    prop_model$sample(data = data.proportions, refresh = 0, chains = 4,
                      parallel_chains = 4, show_messages = FALSE) 
) 

# fit.proportions
p.samples <- fit.proportions$draws("p", format = "df")
variables(p.samples) <- tableI$language
p.samples <- p.samples %>% select(tableI$language)


## ----proptab, echo = FALSE, results='markup', cache=TRUE----------------------
raw.res.prop <- summy(p.samples)
rownames(raw.res.prop) <- NULL
results.prop <- rename(raw.res.prop, Language = variable)
knitr::kable(results.prop, digits = 2, booktabs = TRUE, caption = "Posterior mean and 95\\% BCIs for the proportion of manner verbs.") %>%  kable_styling(position = "center")


## ----propfig, echo=FALSE, results='asis', fig.align = 'center',  fig.cap = "Posterior density of the proportion of manner verbs for each language."----
suppressMessages(
  p.for.figure <- melt(p.samples, variable.name = "Language", value.name = "p") 
)

fig1 <- ggplot(data = p.for.figure, aes(x = p, colour = Language, fill = Language)) +
  geom_density(alpha = .4) +
  scale_x_continuous("Proportion of manner verbs", expand = c(0, 0)) +
  scale_y_continuous("Probability density", expand = c(0, 0)) +
  theme_bw(base_size = 16)

fig1


## ----warning=FALSE, message=FALSE, error=FALSE--------------------------------
data.occurrence <- list(
  K = nrow(tableI),
  P = tableI$nspeakers,
  x = tableI$occurrence_manner
)

occurrence_model <- cmdstanr::cmdstan_model("stan/occurrence.stan")

out2 <- capture.output(
  fit.occurrence <- 
    occurrence_model$sample(data = data.occurrence,
                            refresh = 0,
                            chains = 4,
                            parallel_chains = 4,
                            show_messages = FALSE)
)

lambda.samples <- fit.occurrence$draws("lambda", format = "df")
variables(lambda.samples) <- tableI$language
lambda.samples <- lambda.samples %>% select(tableI$language)


## ----occtab,  echo = FALSE, results='markup', cache=TRUE----------------------
raw.res.occurrence <- summy(lambda.samples)
rownames(raw.res.occurrence) <- NULL
results.occurrence <- rename(raw.res.occurrence, Language = variable)
knitr::kable(results.occurrence, digits = 2, booktabs = TRUE,
             caption = "Posterior mean and 95\\% BCIs for the rate of occurrence of manner verbs estimated from aggregated data.") %>%  kable_styling(position = "center")


## ----occfig,  echo=FALSE, results='asis', fig.align = 'center', fig.cap = "Posterior density for the rate of occurence of manner verbs estimated from aggregated data.", cache=TRUE----
suppressMessages(
  lambda.for.figure <- melt(lambda.samples,
                            variable.name = "Language",
                            value.name = "lambda")
)

fig2 <- ggplot(data = lambda.for.figure,
               aes(x = lambda, colour = Language, fill = Language)) +
  geom_density(alpha = .4) +
  scale_x_continuous("Mean rate of occurrence of manner verbs", expand = c(0, 0)) +
  scale_y_continuous("Probability density", expand = c(0, 0)) +
  theme_bw(base_size = 16)
fig2


## ----warning=FALSE, message=FALSE, error=FALSE--------------------------------
data.occurrence.disaggregated <- list(
  K = nrow(tableI) - 2,
  P = tableI$nspeakers[-c(5, 6)],
  x = tableI$occurrence_manner[-c(5, 6)],
  N1 = nrow(portuguese.manner.data),
  y1 = portuguese.manner.data$occurrences_manner,
  N2 = nrow(english.manner.data),
  y2 = english.manner.data$occurrences_manner
)

occurrence_model_disaggregated <-
  cmdstanr::cmdstan_model("stan/occurrence_disaggregated.stan")

out3 <- capture.output(
  fit.occurrence.disaggregated <- 
    occurrence_model_disaggregated$sample(data = data.occurrence.disaggregated,
                                          refresh = 0, chains = 1)
)

lambda.samples.disaggregated <- fit.occurrence.disaggregated$draws("lambda", format = "df")
variables(lambda.samples.disaggregated) <- tableI$language
lambda.samples.disaggregated <- lambda.samples.disaggregated %>% select(tableI$language)


## ----occdisaggtab,  echo = FALSE, results='markup', cache=TRUE----------------
raw.res.occurrence.disagg <- summy(lambda.samples.disaggregated)
rownames(raw.res.occurrence.disagg) <- NULL
results.occurrence.disagg <- rename(raw.res.occurrence.disagg, Language = variable)
knitr::kable(results.occurrence.disagg, digits = 2, booktabs = TRUE,
             caption = "Posterior mean and 95\\% BCIs for the rate of occurrence of manner verbs estimated from disaggregated data.") %>%  kable_styling(position = "center")


## ----occdisaggfig,  echo=FALSE, results='asis', fig.align = 'center', fig.cap = "Posterior density for the rate of occurence of manner verbs estimated from disaggregated data.", cache=TRUE----
suppressMessages(
  lambda.disaggregated.for.figure <- melt(lambda.samples.disaggregated,
                                          variable.name = "Language",
                                          value.name = "lambda")
)
fig3 <- ggplot(data = lambda.disaggregated.for.figure,
               aes(x = lambda, colour = Language, fill = Language)) +
  geom_density(alpha = .4) +
  scale_x_continuous("Mean occurrence rate of manner verbs",
                     expand = c(0, 0)) +
  scale_y_continuous("Probability density", expand = c(0, 0)) +
  theme_bw(base_size = 16)
fig3 ## should be somewhat similar to fig2


## ----indreffig,  echo=FALSE, results='asis', fig.align = 'center', fig.cap = "Posterior median and BCI for the rate of occurence of manner verbs  by speaker. Horizontal lines mark the language-level median.", cache=TRUE----

gamma.draws <- cbind(fit.occurrence.disaggregated$draws(c("gamma_1"), format = "df"),
                     fit.occurrence.disaggregated$draws(c("gamma_2"), format = "df"))

gamma.draws <- gamma.draws[, grep("gamma_", names(gamma.draws))]

gamma.summaries <- summy(gamma.draws)
colnames(gamma.summaries) <- c("par", "mean", "median", "lower", "upper")

gamma.summaries <- data.frame(gamma.summaries,
                              language = c(rep("BP",
                                               data.occurrence.disaggregated$N1),
                                           rep("English", data.occurrence.disaggregated$N2)))

gamma.summaries$speaker <- as.factor(c(1:data.occurrence.disaggregated$N1,
                                       1:data.occurrence.disaggregated$N2))


overall.medians <- data.frame(median =
                        apply(lambda.samples.disaggregated, 2, median), 
                      language = tableI$language)

fig3 <- ggplot(gamma.summaries, aes(x = speaker, y = median,
                                    ymin = lower, ymax = upper)) +
  geom_pointrange() +
  scale_y_continuous("Posterior median", expand = c(0, 0)) + 
  facet_grid(language~., scales = "free_y", drop = TRUE) + 
  geom_hline(data = overall.medians[5:6, ],
             aes(yintercept = median), linetype = "longdash") +
  theme_bw(base_size = 16)
fig3


## ----figheight, echo=FALSE, out.width="80%", out.height="80%", fig.align='center',fig.cap = "Expected height of boys/girls as a function of age. Taken from Our World in Data (https://ourworldindata.org/human-height). The solid lines show the median height and the ribbons show the median plus or minus two standard deviations.", cache=TRUE----
knitr::include_graphics("healthy_height_growth_curves.png")


## ----echo = TRUE--------------------------------------------------------------
sessionInfo()

