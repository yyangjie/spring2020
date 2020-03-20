wuhan <- rbind(
    control = c(1188, 920, 336, 1250),
    jinyintan = c(670, 469, 178, 458),
    jytdeath = c(85, 50, 19, 52),
    renmin = c(45, 25, 15, 28)
)

colnames(wuhan) <- c("A", "B", "AB", "O")

## function to construct subtables
subtab <- function(wuhan, type, case) {
    rows <- c("control", case)
    x <- wuhan[rows, type]
    y <- (rowSums(wuhan) - wuhan[, type])[rows]
    tab <- cbind(x, y)
    colnames(tab) <- c(type, "other")
    tab
}
## for example
subtab(wuhan, "A", "jinyintan")
subtab(wuhan, "O", "jytdeath")

#### significant association: Jinyintan
chisq.test(wuhan[c("control", "jinyintan"), ])
## which blood type is causing the significance
chisq.test(subtab(wuhan, "A", "jinyintan"))
chisq.test(subtab(wuhan, "B", "jinyintan"))
chisq.test(subtab(wuhan, "AB", "jinyintan"))
chisq.test(subtab(wuhan, "O", "jinyintan"))

## regression analyses on expanded data
exptab <- function(tab) {
    y <- rep(c(0, 1, 0, 1), c(tab))
    tp <- dimnames(tab)[[2]][1]
    type <- rep(c(1, 1, 0, 0), c(tab))
    data.frame(y = y, type = type)
}

jinyintan.A <- subtab(wuhan, "A", "jinyintan")
## may add other covariates such as gender and age, and interactions
summary(glm(y ~ type, data = exptab(jinyintan.A), family = "binomial"))
anova(lm(y ~ type, data = exptab(jinyintan.A)))

jinyintan.O <- subtab(wuhan, "O", "jinyintan")
summary(glm(y ~ type, data = exptab(jinyintan.O), family = "binomial"))
anova(lm(y ~ type, data = exptab(jinyintan.O)))


#### signifiant association: death from Jinyintan
chisq.test(wuhan[c("control", "jytdeath"), ])
## which specific type caused the significance
chisq.test(subtab(wuhan, "A", "jytdeath"))
chisq.test(subtab(wuhan, "B", "jytdeath"))
chisq.test(subtab(wuhan, "AB", "jytdeath"))
chisq.test(subtab(wuhan, "O", "jytdeath"))


#### insignificant association: Renmin
chisq.test(wuhan[c("control", "renmin"), ])
