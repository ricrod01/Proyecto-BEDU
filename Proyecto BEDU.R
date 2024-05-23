library(dplyr)
library(ggplot2)
library(readxl)
library(agricolae)


setwd("C:\\Users\\ricro\\OneDrive\\Desktop\\R")

df <- read_excel("Proyecto.xlsx")

df <- select(df, Temperatura, Bandeja, Región, 'Proporción masa perdida')
df <- rename(df, T = Temperatura, B = Bandeja, R = Región, P = 'Proporción masa perdida') 

df <- mutate(df, R = factor(R))
df <- mutate(df, T = factor(T))

str(df)

B1_T60 <- df[which(df$B == 1 & df$T == 60),]
B1_T65 <- df[which(df$B == 1 & df$T == 65),]
B1_T70 <- df[which(df$B == 1 & df$T == 70),]

B2_T60 <- df[which(df$B == 2 & df$T == 60),]
B2_T65 <- df[which(df$B == 2 & df$T == 65),]
B2_T70 <- df[which(df$B == 2 & df$T == 70),]

B3_T60 <- df[which(df$B == 3 & df$T == 60),]
B3_T65 <- df[which(df$B == 3 & df$T == 65),]
B3_T70 <- df[which(df$B == 3 & df$T == 70),]

B4_T60 <- df[which(df$B == 4 & df$T == 60),]
B4_T65 <- df[which(df$B == 4 & df$T == 65),]
B4_T70 <- df[which(df$B == 4 & df$T == 70),]


aB1_T60 <- pairwise.t.test(B1_T60$P, B1_T60$R, p.adjust.method = "bonferroni")
aB1_T60

aB2_T60 <- pairwise.t.test(B2_T60$P, B2_T60$R, p.adjust.method = "bonferroni")
aB2_T60

aB3_T60 <- pairwise.t.test(B3_T60$P, B3_T60$R, p.adjust.method = "bonferroni")
aB3_T60

aB4_T60 <- pairwise.t.test(B4_T60$P, B4_T60$R, p.adjust.method = "bonferroni")
aB4_T60

aB1_T65 <- pairwise.t.test(B1_T65$P, B1_T65$R, p.adjust.method = "bonferroni")
aB1_T65

aB2_T65 <- pairwise.t.test(B2_T65$P, B2_T65$R, p.adjust.method = "bonferroni")
aB2_T65

aB3_T65 <- pairwise.t.test(B3_T65$P, B3_T65$R, p.adjust.method = "bonferroni")
aB3_T65

aB4_T65 <- pairwise.t.test(B4_T65$P, B4_T65$R, p.adjust.method = "bonferroni")
aB4_T65

aB1_T70 <- pairwise.t.test(B1_T70$P, B1_T70$R, p.adjust.method = "bonferroni")
aB1_T70

aB2_T70 <- pairwise.t.test(B2_T70$P, B2_T70$R, p.adjust.method = "bonferroni")
aB2_T70

aB3_T70 <- pairwise.t.test(B3_T70$P, B3_T70$R, p.adjust.method = "bonferroni")
aB3_T70

aB4_T70 <- pairwise.t.test(B4_T70$P, B4_T70$R, p.adjust.method = "bonferroni")
aB4_T70


ggplot(df[which(df$B == 1),], aes(x = T, y = P, fill = R)) +
  stat_summary(fun = mean, geom = "bar", position = "stack", color = "black") +
  labs(title = "Media de Proporción de masa perdida por Temperatura y Región, B1",
       x = "Temperatura",
       y = "Media de Proporción de masa perdida") +
  facet_wrap(~R) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange", "green"))

ggplot(df[which(df$B == 2),], aes(x = T, y = P, fill = R)) +
  stat_summary(fun = mean, geom = "bar", position = "stack", color = "black") +
  labs(title = "Media de Proporción de masa perdida por Temperatura y Región, B2",
       x = "Temperatura",
       y = "Media de Proporción de masa perdida") +
  facet_wrap(~R) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange", "green"))

ggplot(df[which(df$B == 3),], aes(x = T, y = P, fill = R)) +
  stat_summary(fun = mean, geom = "bar", position = "stack", color = "black") +
  labs(title = "Media de Proporción de masa perdida por Temperatura y Región, B3",
       x = "Temperatura",
       y = "Media de Proporción de masa perdida") +
  facet_wrap(~R) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange", "green"))

ggplot(df[which(df$B == 4),], aes(x = T, y = P, fill = R)) +
  stat_summary(fun = mean, geom = "bar", position = "stack", color = "black") +
  labs(title = "Media de Proporción de masa perdida por Temperatura y Región, B4",
       x = "Temperatura",
       y = "Media de Proporción de masa perdida") +
  facet_wrap(~R) +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange", "green"))


df2 <- read_excel("Proyecto2.xlsx")

lmB1_T60_RI <- lm(PROMEDIO ~ 0 + TIEMPO, data = df2[which(df2$TEMPERATURA == 60 & 
                                                        df2$BANDEJA == 1 & 
                                                        df2$REGION == 'I'),])
summary(lmB1_T60_RI)

lmB1_T60_RC <- lm(PROMEDIO ~ 0 + TIEMPO, data = df2[which(df2$TEMPERATURA == 60 & 
                                                            df2$BANDEJA == 1 & 
                                                            df2$REGION == 'C'),])
summary(lmB1_T60_RC)

lmB1_T60_RD <- lm(PROMEDIO ~ 0 + TIEMPO, data = df2[which(df2$TEMPERATURA == 60 & 
                                                            df2$BANDEJA == 1 & 
                                                            df2$REGION == 'D'),])
summary(lmB1_T60_RD)