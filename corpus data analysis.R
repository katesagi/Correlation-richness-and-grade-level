setwd("~/Desktop/CU Boulder Grad/Thesis/Raw Data")
library(Rling)
library(tidyr)
library(tidyverse)
library(car)

read_csv("thesis_corpus_data.csv")
corpus_data <- read_csv("thesis_corpus_data.csv")

head(corpus_data)

##distribution of the data, all response metrics below:
hist(corpus_data$DESC_div_total)
hist(corpus_data$ADP_div_total)
hist(corpus_data$SCONJ_div_total)
hist(corpus_data$UWD_div_total)
hist(corpus_data$avg_SL)
hist(corpus_data$avg_arrow_length)
hist(corpus_data$total_words)

corpus_data$grade_level <- as.factor(corpus_data$grade_level)

#single variable models for all metrics
#then split into ALB versus CCS tables 
mod1 <- lm(DESC_div_total ~ grade_level, data = corpus_data)
summary(mod1)

mod2 <- lm(ADP_div_total ~ grade_level, data = corpus_data)
summary(mod2)

mod3 <- lm(SCONJ_div_total ~ grade_level, data = corpus_data)
summary(mod3)

mod4 <- lm(UWD_div_total ~ grade_level, data = corpus_data)
summary(mod4)

mod5 <- lm(avg_SL ~ grade_level, data = corpus_data)
summary(mod5)

mod6 <- lm(avg_arrow_length ~ grade_level, data = corpus_data)
summary(mod6)

mod7 <- lm(total_words ~ grade_level, data = corpus_data)
summary(mod7)

##to save summary results from models as a text file:
capture.output(summary(mod1), file = "desc_grade_model.txt")

capture.output(summary(mod2), file = "adp_grade_model.txt")

capture.output(summary(mod3), file = "sconj_grade_model.txt")

capture.output(summary(mod4), file = "uwd_grade_model.txt")

capture.output(summary(mod5), file = "SL_grade_model.txt")

capture.output(summary(mod6), file = "arrow_grade_model.txt")

capture.output(summary(mod7), file = "totalwords_grade_model.txt")

##visual representation of trend, separating by grade level
m1p <- ggplot(corpus_data, aes(x=grade_level, y=DESC_div_total, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m1p + ggtitle("Descriptor concentration in texts across grade level") +
  xlab("Grade Level") + ylab("Descriptor concentration")

m2p <- ggplot(corpus_data, aes(x=grade_level, y=ADP_div_total, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m2p + ggtitle("Adposition concentration in texts across grade level") +
  xlab("Grade Level") + ylab("Adposition concentration")

m3p <- ggplot(corpus_data, aes(x=grade_level, y=SCONJ_div_total, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m3p + ggtitle("Subordinating conjunction concentration in texts across grade level") +
  xlab("Grade Level") + ylab("Subordinating conjunction concentration")

m4p <- ggplot(corpus_data, aes(x=grade_level, y=UWD_div_total, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m4p + ggtitle("Unique word diversity in texts across grade level") +
  xlab("Grade Level") + ylab("Proportion unique words")

m5p <- ggplot(corpus_data, aes(x=grade_level, y=avg_SL, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m5p + ggtitle("Average sentence lengths in texts across grade level") +
  xlab("Grade Level") + ylab("Average sentence length")

m6p <- ggplot(corpus_data, aes(x=grade_level, y=avg_arrow_length, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m6p + ggtitle("AVG Dependency arrow length in texts across grade level") +
  xlab("Grade Level") + ylab("AVG Dependency arrow length")

m7p <- ggplot(corpus_data, aes(x=grade_level, y=total_words, fill=grade_level))+geom_boxplot(show.legend=FALSE)
m7p + ggtitle("Total word count in texts across grade level") +
  xlab("Grade Level") + ylab("Word count")


## multicollinearity
##all comp metric from big model
multi_co_data <- corpus_data %>% dplyr::select(avg_SL, ADP_div_total, UWD_div_total, DESC_div_total, SCONJ_div_total, avg_arrow_length)
pairs(multi_co_data)

str(corpus_data)

##ordered logistic regression model, all metrics together
##significance comes from difference between standard error and value
library(MASS)
log_mod1 <- polr(grade_level ~ avg_SL + DESC_div_total, data = corpus_data, Hess = TRUE)
summary(log_mod1)

##if confidence intervals bound zero, it's likely it's not significant
library(broom)
tidy(log_mod1, conf.int = TRUE)

##ordinal model, shows confidence interval ranges
log_mod2 <- polr(grade_level ~ avg_SL + ADP_div_total + UWD_div_total + DESC_div_total + SCONJ_div_total +avg_arrow_length, data = corpus_data, Hess = TRUE)
summary(log_mod2)
tidy(log_mod2, conf.int = TRUE)
capture.output(summary(log_mod2), file = "full_model_summary.txt")
capture.output(tidy(log_mod2, conf.int = TRUE), file = "conf_int_full.txt")

##model with total words, didn't really work
log_mod3 <- polr(grade_level ~ avg_SL + UWD_div_total + DESC_div_total + SCONJ_div_total +avg_arrow_length + total_words, data = corpus_data, Hess = TRUE)
summary(log_mod3)
tidy(log_mod3, conf.int = TRUE)


##Separating the corpus into chunks, poetry v prose, ALB vs CCS
poetry_corpus <- filter(corpus_data, text_type == "poetry")

prose_corpus <- filter(corpus_data, text_type == "prose")

alb_corpus <- filter(corpus_data, standards == "ALB")

ccs_corpus <- filter(corpus_data, standards == "CCS")

##Specific corpus models:
##POETRY
poetry_mod1 <- lm(DESC_div_total ~ grade_level, data = poetry_corpus)
summary(poetry_mod1)

poetry_mod2 <- lm(ADP_div_total ~ grade_level, data = poetry_corpus)
summary(poetry_mod2)

poetry_mod3 <- lm(SCONJ_div_total ~ grade_level, data = poetry_corpus)
summary(poetry_mod3)

poetry_mod4 <- lm(UWD_div_total ~ grade_level, data = poetry_corpus)
summary(poetry_mod4)

poetry_mod5 <- lm(avg_SL ~ grade_level, data = poetry_corpus)
summary(poetry_mod5)

poetry_mod6 <- lm(avg_arrow_length ~ grade_level, data = poetry_corpus)
summary(poetry_mod6)

poetry_mod7 <- lm(total_words ~ grade_level, data = poetry_corpus)
summary(poetry_mod7)

##All variables for POETRY
log_poetry <- polr(grade_level ~ avg_SL + ADP_div_total + UWD_div_total + DESC_div_total + SCONJ_div_total +avg_arrow_length, data = poetry_corpus, Hess = TRUE)
summary(log_poetry)
tidy(log_poetry, conf.int = TRUE)
capture.output(tidy(log_poetry, conf.int = TRUE), file = "conf_int_poetry.txt")

##PROSE
prose_mod1 <- lm(DESC_div_total ~ grade_level, data = prose_corpus)
summary(prose_mod1)

prose_mod2 <- lm(ADP_div_total ~ grade_level, data = prose_corpus)
summary(poetry_mod2)

prose_mod3 <- lm(SCONJ_div_total ~ grade_level, data = prose_corpus)
summary(prose_mod3)

prose_mod4 <- lm(UWD_div_total ~ grade_level, data = prose_corpus)
summary(prose_mod4)

prose_mod5 <- lm(avg_SL ~ grade_level, data = prose_corpus)
summary(prose_mod5)

prose_mod6 <- lm(avg_arrow_length ~ grade_level, data = prose_corpus)
summary(prose_mod6)

prose_mod7 <- lm(total_words ~ grade_level, data = prose_corpus)
summary(prose_mod7)

##All variables for PROSE
log_prose <- polr(grade_level ~ avg_SL + ADP_div_total + UWD_div_total + DESC_div_total + SCONJ_div_total +avg_arrow_length, data = prose_corpus, Hess = TRUE)
summary(log_prose)
confint.default(log_prose)

capture.output(confint.default(log_prose), file = "conf_int_prose.txt")

##ALBERTA
alb_mod1 <- lm(DESC_div_total ~ grade_level, data = alb_corpus)
summary(alb_mod1)

alb_mod2 <- lm(ADP_div_total ~ grade_level, data = alb_corpus)
summary(alb_mod2)

alb_mod3 <- lm(SCONJ_div_total ~ grade_level, data = alb_corpus)
summary(alb_mod3)

alb_mod4 <- lm(UWD_div_total ~ grade_level, data = alb_corpus)
summary(alb_mod4)

alb_mod5 <- lm(avg_SL ~ grade_level, data = alb_corpus)
summary(alb_mod5)

alb_mod6 <- lm(avg_arrow_length ~ grade_level, data = alb_corpus)
summary(alb_mod6)

alb_mod7 <- lm(total_words ~ grade_level, data = alb_corpus)
summary(alb_mod7)

##All variables for ALB
log_alb <- polr(grade_level ~ avg_SL + ADP_div_total + UWD_div_total + DESC_div_total + SCONJ_div_total +avg_arrow_length, data = alb_corpus, Hess = TRUE)
summary(log_alb)
tidy(log_alb, conf.int = TRUE)
capture.output(tidy(log_alb, conf.int = TRUE), file = "conf_int_alb.txt")

##CCS
ccs_mod1 <- lm(DESC_div_total ~ grade_level, data = ccs_corpus)
summary(ccs_mod1)

ccs_mod2 <- lm(ADP_div_total ~ grade_level, data = ccs_corpus)
summary(ccs_mod2)

ccs_mod3 <- lm(SCONJ_div_total ~ grade_level, data = ccs_corpus)
summary(ccs_mod3)

ccs_mod4 <- lm(UWD_div_total ~ grade_level, data = ccs_corpus)
summary(ccs_mod4)

ccs_mod5 <- lm(avg_SL ~ grade_level, data = ccs_corpus)
summary(ccs_mod5)

ccs_mod6 <- lm(avg_arrow_length ~ grade_level, data = ccs_corpus)
summary(ccs_mod6)

ccs_mod7 <- lm(total_words ~ grade_level, data = ccs_corpus)
summary(ccs_mod7)

##All variables for CCS
log_ccs <- polr(grade_level ~ avg_SL + ADP_div_total + UWD_div_total + DESC_div_total + SCONJ_div_total +avg_arrow_length, data = ccs_corpus, Hess = TRUE)
summary(log_ccs)
tidy(log_ccs, conf.int = TRUE)
capture.output(tidy(log_ccs, conf.int = TRUE), file = "conf_int_ccs.txt")



